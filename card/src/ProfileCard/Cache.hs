{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Per-repository line counts, persisted between runs.
--
-- Walking every authored commit of every repository is by far the most
-- expensive part of a run, so results are cached and only the commits pushed
-- since the previous run are fetched.
module ProfileCard.Cache
  ( Cache (..)
  , RepoLoc (..)
  , repoKey
  , emptyCache
  , loadCache
  , saveCache
  ) where

import           Crypto.Hash              (SHA512 (..), hashWith)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty', defConfig, Config (..), Indent (..), keyOrder)
import qualified Data.ByteString.Lazy     as BL
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           GHC.Generics             (Generic)
import           System.Directory         (createDirectoryIfMissing, doesFileExist)
import           System.FilePath          (takeDirectory)
import           System.IO                (hPutStrLn, stderr)

-- | What we know about one repository's authored history.
data RepoLoc = RepoLoc
  { rlHead     :: Text  -- ^ Default-branch head when this entry was written; an
                        --   unchanged head means nothing needs re-fetching.
  , rlLastSeen :: Text  -- ^ Newest /authored/ commit counted so far.
  , rlAdded    :: Int
  , rlDeleted  :: Int
  , rlCommits  :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON RepoLoc where
  toJSON (RepoLoc h l a d c) = object
    [ "head" .= h, "lastSeen" .= l, "added" .= a, "deleted" .= d, "commits" .= c ]

instance FromJSON RepoLoc where
  parseJSON = withObject "RepoLoc" $ \o -> RepoLoc
    <$> o .:  "head"
    <*> o .:  "lastSeen"
    <*> o .:? "added"   .!= 0
    <*> o .:? "deleted" .!= 0
    <*> o .:? "commits" .!= 0

newtype Cache = Cache { cacheRepos :: Map Text RepoLoc }
  deriving (Show, Eq)

instance ToJSON Cache where
  toJSON (Cache repos) = object ["version" .= (1 :: Int), "repos" .= repos]

instance FromJSON Cache where
  parseJSON = withObject "Cache" $ \o -> Cache <$> o .:? "repos" .!= M.empty

-- | The cache key for a repository: SHA-512 of @owner\/name@, hex.
--
-- This file is committed to a public repository, and keying it by name listed
-- every private repository the account owns. The card only ever needs to ask
-- \"have I seen this repository before, and at which commit\", which a digest
-- answers just as well as the name does.
--
-- It raises the bar rather than sealing the door: an unsalted digest of a
-- guessable name can be confirmed by hashing the guess. It stops the cache
-- being a readable inventory, which is the actual problem.
repoKey :: Text -> Text
repoKey = T.pack . show . hashWith SHA512 . TE.encodeUtf8

emptyCache :: Cache
emptyCache = Cache M.empty

-- | A missing or corrupt cache is not fatal: it just means a cold, slow run.
loadCache :: FilePath -> IO Cache
loadCache path = do
  exists <- doesFileExist path
  if not exists
    then pure emptyCache
    else do
      raw <- BL.readFile path
      case eitherDecode raw of
        Right c  -> pure c
        Left err -> do
          hPutStrLn stderr ("warning: ignoring unreadable cache " <> path <> ": " <> err)
          pure emptyCache

-- | Written pretty-printed with sorted keys so the daily commit diff stays
-- readable and only shows repositories that actually changed.
saveCache :: FilePath -> Cache -> IO ()
saveCache path cache = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (encodePretty' conf cache <> "\n")
  where
    conf = defConfig
      { confIndent  = Spaces 2
      , confCompare = keyOrder ["version", "repos"] <> compare
      }
