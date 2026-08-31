{-# LANGUAGE OverloadedStrings #-}

-- | A very small GitHub GraphQL client: one POST, retried, decoded to 'Value'.
module ProfileCard.GitHub
  ( Client
  , newClient
  , runQuery
  ) where

import           Control.Concurrent      (threadDelay)
import           Control.Exception       (SomeException, try)
import           Control.Monad           (when)
import           Data.Aeson              (Value (..), eitherDecode, encode, object, (.=))
import qualified Data.Aeson.Key          as K
import qualified Data.Aeson.KeyMap       as KM
import qualified Data.ByteString.Char8   as BS
import           Data.Text               (Text)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (newTlsManager)
import           Network.HTTP.Types      (statusCode)
import           System.Environment      (lookupEnv)
import           System.Exit             (die)
import           System.IO               (hPutStrLn, stderr)

data Client = Client
  { clManager :: Manager
  , clToken   :: BS.ByteString
  }

-- | Reads @ACCESS_TOKEN@ first (the name the workflow gives a PAT that can also
-- see private contributions), then the tokens the runner provides by default.
newClient :: IO Client
newClient = do
  mtok <- firstJustM lookupEnv ["ACCESS_TOKEN", "GITHUB_TOKEN", "GH_TOKEN"]
  tok  <- maybe (die "No token: set ACCESS_TOKEN or GITHUB_TOKEN.") pure mtok
  mgr  <- newTlsManager
  pure (Client mgr (BS.pack tok))
  where
    firstJustM _ []       = pure Nothing
    firstJustM f (x : xs) = f x >>= maybe (firstJustM f xs) (pure . Just)

-- | Run a query with variables, returning the @data@ object. Retries on
-- transient failures: a burst of LOC queries reliably draws the occasional 502.
runQuery :: Client -> Text -> [(Text, Value)] -> IO Value
runQuery client query vars = go 1
  where
    maxAttempts = 5 :: Int

    go attempt = do
      outcome <- try (attemptOnce client query vars)
      case outcome of
        Right (Right v)  -> pure v
        Right (Left err) -> next attempt err
        Left e           -> next attempt (show (e :: SomeException))

    next attempt err
      | attempt >= maxAttempts = die ("GitHub API failed: " <> err)
      | otherwise = do
          hPutStrLn stderr ("  retry " <> show attempt <> "/" <> show maxAttempts <> ": " <> err)
          threadDelay (attempt * attempt * 1000000) -- 1s, 4s, 9s, 16s
          go (attempt + 1)

attemptOnce :: Client -> Text -> [(Text, Value)] -> IO (Either String Value)
attemptOnce client query vars = do
  initReq <- parseRequest "https://api.github.com/graphql"
  let body = encode (object ["query" .= query, "variables" .= object (map toPair vars)])
      req  = initReq
        { method          = "POST"
        , requestBody     = RequestBodyLBS body
        , requestHeaders  =
            [ ("Authorization", "Bearer " <> clToken client)
            , ("User-Agent",    "profile-card-haskell")
            , ("Content-Type",  "application/json")
            ]
        , responseTimeout = responseTimeoutMicro 60000000
        }
  resp <- httpLbs req (clManager client)
  let code = statusCode (responseStatus resp)
  when (code == 401 || code == 403) $
    die ("GitHub returned " <> show code <> ": token is missing, expired, or lacks `repo` scope.")
  pure $ case eitherDecode (responseBody resp) of
    Left err  -> Left ("bad JSON (HTTP " <> show code <> "): " <> err)
    Right val -> extractData code val
  where
    toPair (k, v) = K.fromText k .= v

-- | GraphQL reports failures with HTTP 200 and an @errors@ array, so the status
-- code alone cannot tell us whether the call worked.
extractData :: Int -> Value -> Either String Value
extractData code (Object o) =
  case KM.lookup "errors" o of
    Just errs -> Left ("HTTP " <> show code <> ": " <> take 400 (show (encode errs)))
    Nothing   -> maybe (Left ("HTTP " <> show code <> ": response had no `data`")) Right
                       (KM.lookup "data" o)
extractData _ _ = Left "response was not a JSON object"
