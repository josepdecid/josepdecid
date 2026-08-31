{-# LANGUAGE OverloadedStrings #-}

-- | Turns GitHub's GraphQL API into the handful of numbers the card shows.
module ProfileCard.Stats
  ( Profile (..)
  , Repo (..)
  , LangStat (..)
  , Stats (..)
  , gather
  ) where

import           Control.Monad          (foldM)
import           Data.Aeson
import           Data.Aeson.Types       (Parser, parseEither)
import qualified Data.Aeson.Key         as K
import qualified Data.Aeson.KeyMap      as KM
import           Data.List              (sortOn)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (mapMaybe)
import           Data.Ord               (Down (..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (UTCTime, getCurrentTime, utctDay, toGregorian)
import           System.Exit            (die)
import           System.IO              (hPutStr, hPutStrLn, hFlush, stderr)
import           Text.Read              (readMaybe)

import           ProfileCard.Cache
import           ProfileCard.Config     (Config (..))
import           ProfileCard.Format     (tshow)
import           ProfileCard.GitHub

-- | The account itself. Only what the card actually prints is requested:
-- @followers@, @following@, @name@ and @login@ were being fetched and thrown
-- away.
data Profile = Profile
  { pfId          :: Text
  , pfCreatedAt   :: UTCTime
  , pfContribTo   :: Int   -- ^ Repositories contributed to but not owned.
  , pfYears       :: [Int] -- ^ Years with any contribution activity.
  } deriving (Show)

data Repo = Repo
  { rpNameWithOwner :: Text
  , rpOwner         :: Text
  , rpName          :: Text
  , rpIsFork        :: Bool
  , rpIsPrivate     :: Bool
  , rpStars         :: Int
  , rpLanguages     :: [(Text, Text, Int)] -- ^ (name, colour, bytes)
  , rpHead          :: Maybe Text          -- ^ Default-branch head commit.
  } deriving (Show)

rpIsOwned :: Text -> Repo -> Bool
rpIsOwned login r = T.toLower (rpOwner r) == T.toLower login

data LangStat = LangStat
  { lsName  :: Text
  , lsColor :: Text
  , lsBytes :: Int
  } deriving (Show)

-- | Everything the renderer needs, and nothing it does not.
data Stats = Stats
  { stProfile     :: Profile
  , stRepos       :: Int
  , stPrivate     :: Int
  , stStars       :: Int
  , stCommits     :: Int
  , stCommitsYear :: Int
  , stLocAdded    :: Int
  , stLocDeleted  :: Int
  , stLangs       :: [LangStat]
  , stLangTotal   :: Int   -- ^ Bytes across every language, not just the top ones.
  , stGeneratedAt :: UTCTime
  }

-- | Fetch everything, updating the on-disk LOC cache as a side effect.
gather :: Config -> IO Stats
gather cfg = do
  client <- newClient
  now    <- getCurrentTime
  let login = cfgLogin cfg

  progress "profile"
  profile <- fetchProfile client login

  progress "repositories"
  repos <- fetchRepos client login

  progress "commit totals"
  (allCommits, yearCommits) <- fetchCommits client login (pfYears profile) (currentYear now)

  cache <- loadCache (cfgCachePath cfg)
  (cache', added, deleted) <- fetchLoc client cfg profile repos cache
  saveCache (cfgCachePath cfg) cache'

  let owned       = filter (rpIsOwned login) repos
      sourceRepos = filter (not . rpIsFork) owned
      ignored     = map T.toLower (cfgIgnoreLangs cfg)
      langBytes   = [ l | r <- sourceRepos
                        , l@(name, _, _) <- rpLanguages r
                        , T.toLower name `notElem` ignored ]
  pure Stats
    { stProfile     = profile
    , stRepos       = length sourceRepos
    , stPrivate     = length (filter rpIsPrivate sourceRepos)
    , stStars       = sum (map rpStars owned)
    , stCommits     = allCommits
    , stCommitsYear = yearCommits
    , stLocAdded    = added
    , stLocDeleted  = deleted
    , stLangs       = topLanguages (cfgTopLangs cfg) langBytes
    , stLangTotal   = sum [ b | (_, _, b) <- langBytes ]
    , stGeneratedAt = now
    }

currentYear :: UTCTime -> Int
currentYear t = let (y, _, _) = toGregorian (utctDay t) in fromIntegral y

progress :: String -> IO ()
progress msg = hPutStrLn stderr ("-> " <> msg)

-- | 'parseEither' with the failure turned into a fatal, quotable error.
decodeOrDie :: String -> (Value -> Parser a) -> Value -> IO a
decodeOrDie what p v =
  either (\e -> die ("could not read " <> what <> ": " <> e <> "\n  in: " <> take 500 (show v)))
         pure
         (parseEither p v)

-- Profile ---------------------------------------------------------------

profileQuery :: Text
profileQuery = T.unlines
  [ "query($login: String!) {"
  , "  user(login: $login) {"
  , "    id createdAt"
  , "    repositoriesContributedTo(contributionTypes: [COMMIT, PULL_REQUEST, REPOSITORY], includeUserRepositories: false) { totalCount }"
  , "    contributionsCollection { contributionYears }"
  , "  }"
  , "}"
  ]

fetchProfile :: Client -> Text -> IO Profile
fetchProfile client login = do
  raw <- runQuery client profileQuery [("login", String login)]
  decodeOrDie "profile" parser raw
  where
    parser = withObject "data" $ \d -> do
      u <- d .: "user"
      Profile
        <$> u .: "id"
        <*> u .: "createdAt"
        <*> (u .: "repositoriesContributedTo" >>= (.: "totalCount"))
        <*> (u .: "contributionsCollection" >>= (.: "contributionYears"))

-- Commits ---------------------------------------------------------------

-- | @contributionsCollection@ only ever covers a one-year window, so one
-- aliased field is requested per year the account has been active.
commitsQuery :: [Int] -> Text
commitsQuery years =
  "query($login: String!) { user(login: $login) {" <> T.concat (map field years) <> "} }"
  where
    field y = T.concat
      [ " y", tshow y, ": contributionsCollection("
      , "from: \"", tshow y, "-01-01T00:00:00Z\", "
      , "to: \"",   tshow y, "-12-31T23:59:59Z\") "
      , "{ totalCommitContributions restrictedContributionsCount }"
      ]

-- | Returns (commits across every year, commits in the current year). Private
-- contributions are included only when the token can see them.
fetchCommits :: Client -> Text -> [Int] -> Int -> IO (Int, Int)
fetchCommits _ _ [] _ = pure (0, 0)
fetchCommits client login years thisYear = do
  raw <- runQuery client (commitsQuery years) [("login", String login)]
  perYear <- decodeOrDie "commit totals" parser raw
  pure (sum (M.elems perYear), M.findWithDefault 0 thisYear perYear)
  where
    parser = withObject "data" $ \d -> do
      u <- d .: "user"
      fmap M.fromList (mapM one (KM.toList u))

    -- Fields come back under the aliases the query invented: @y2024@ and so on.
    one (key, val) = do
      let name = K.toText key
      year <- maybe (fail ("unexpected field " <> T.unpack name)) pure
                    (readMaybe (T.unpack (T.drop 1 name)))
      n <- withObject "contributionsCollection" perYearTotal val
      pure (year :: Int, n)

    perYearTotal o = (+) <$> o .: "totalCommitContributions"
                         <*> o .:? "restrictedContributionsCount" .!= 0

-- Repositories ----------------------------------------------------------

reposQuery :: Text
reposQuery = T.unlines
  [ "query($login: String!, $cursor: String) {"
  , "  user(login: $login) {"
  , "    repositories(first: 50, after: $cursor,"
  , "      ownerAffiliations: [OWNER, COLLABORATOR, ORGANIZATION_MEMBER]) {"
  , "      pageInfo { hasNextPage endCursor }"
  , "      nodes {"
  , "        nameWithOwner name isFork isPrivate stargazerCount"
  , "        owner { login }"
  , "        languages(first: 10, orderBy: {field: SIZE, direction: DESC}) {"
  , "          edges { size node { name color } }"
  , "        }"
  , "        defaultBranchRef { target { ... on Commit { oid } } }"
  , "      }"
  , "    }"
  , "  }"
  , "}"
  ]

fetchRepos :: Client -> Text -> IO [Repo]
fetchRepos client login = go Nothing []
  where
    go cursor acc = do
      raw <- runQuery client reposQuery
               (("login", String login) : [("cursor", String c) | Just c <- [cursor]])
      (nodes, next) <- decodeOrDie "repositories" parser raw
      let acc' = acc <> nodes
      case next of
        Just c  -> go (Just c) acc'
        Nothing -> pure acc'

    parser = withObject "data" $ \d -> do
      repos <- d .: "user" >>= (.: "repositories")
      nodes <- repos .: "nodes" >>= mapM parseRepo
      info  <- repos .: "pageInfo"
      more  <- info .: "hasNextPage"
      end   <- info .:? "endCursor"
      pure (nodes, if more then end else Nothing)

parseRepo :: Value -> Parser Repo
parseRepo = withObject "repository" $ \o -> Repo
  <$> o .: "nameWithOwner"
  <*> (o .: "owner" >>= (.: "login"))
  <*> o .: "name"
  <*> o .: "isFork"
  <*> o .: "isPrivate"
  <*> o .: "stargazerCount"
  <*> (o .: "languages" >>= (.: "edges") >>= mapM parseLangEdge)
  <*> (o .:? "defaultBranchRef" >>= traverse (.: "target") >>= traverse (.: "oid"))

parseLangEdge :: Value -> Parser (Text, Text, Int)
parseLangEdge = withObject "language edge" $ \e -> do
  size <- e .: "size"
  node <- e .: "node"
  name <- node .: "name"
  colr <- node .:? "color" .!= "#8b949e"
  pure (name, colr, size)

-- | The @n@ largest languages by bytes written.
topLanguages :: Int -> [(Text, Text, Int)] -> [LangStat]
topLanguages n edges =
  take n
    . sortOn (Down . lsBytes)
    . map (\(name, (colr, bytes)) -> LangStat name colr bytes)
    . M.toList
    $ foldr add M.empty edges
  where
    add (name, colr, bytes) = M.insertWith merge name (colr, bytes)
    merge (newColor, newBytes) (oldColor, oldBytes) =
      (if T.null oldColor then newColor else oldColor, oldBytes + newBytes)

-- Lines of code ---------------------------------------------------------

historyQuery :: Text
historyQuery = T.unlines
  [ "query($owner: String!, $name: String!, $authorId: ID!, $cursor: String) {"
  , "  repository(owner: $owner, name: $name) {"
  , "    defaultBranchRef { target { ... on Commit {"
  , "      history(first: 100, after: $cursor, author: {id: $authorId}) {"
  , "        pageInfo { hasNextPage endCursor }"
  , "        nodes { oid additions deletions }"
  , "      }"
  , "    } } }"
  , "  }"
  , "}"
  ]

-- | Sum additions and deletions over every commit authored by the user, using
-- the cache to skip untouched repositories and to fetch only new commits
-- elsewhere.
fetchLoc :: Client -> Config -> Profile -> [Repo] -> Cache -> IO (Cache, Int, Int)
fetchLoc client _cfg profile repos cache0 = do
  hPutStrLn stderr ("-> lines of code across " <> show (length repos) <> " repositories")
  (m, hits) <- foldM step (cacheRepos cache0, 0 :: Int) repos
  hPutStrLn stderr ("\n   " <> show hits <> " unchanged (served from cache)")
  let entries = mapMaybe (`M.lookup` m) (map (repoKey . rpNameWithOwner) repos)
  pure (Cache m, sum (map rlAdded entries), sum (map rlDeleted entries))
  where
    step (m, hits) repo = case rpHead repo of
      Nothing -> pure (m, hits) -- empty repository, nothing to count
      Just headOid ->
        case M.lookup (repoKey (rpNameWithOwner repo)) m of
          Just cached | rlHead cached == headOid -> do
            tick '.'
            pure (m, hits + 1)
          cached -> do
            tick '+'
            entry <- countRepo client profile repo headOid cached
            pure (M.insert (repoKey (rpNameWithOwner repo)) entry m, hits)

    tick c = hPutStr stderr [c] >> hFlush stderr

-- | Page backwards through authored commits, stopping at the newest commit we
-- had already counted. If that commit never turns up (a rebase or force-push
-- rewrote it), the repository is recounted from scratch rather than
-- double-counted.
countRepo :: Client -> Profile -> Repo -> Text -> Maybe RepoLoc -> IO RepoLoc
countRepo client profile repo headOid cached = do
    (fresh, foundSentinel) <- walk Nothing [] 
    let (added, deleted, commits) = totals fresh
    pure $ case (cached, foundSentinel) of
      (Just old, True) -> RepoLoc
        { rlHead     = headOid
        , rlLastSeen = newestOid fresh (rlLastSeen old)
        , rlAdded    = rlAdded   old + added
        , rlDeleted  = rlDeleted old + deleted
        , rlCommits  = rlCommits old + commits
        }
      _ -> RepoLoc
        { rlHead     = headOid
        , rlLastSeen = newestOid fresh ""
        , rlAdded    = added
        , rlDeleted  = deleted
        , rlCommits  = commits
        }
  where
    sentinel = rlLastSeen <$> cached

    newestOid []           fallback = fallback
    newestOid ((oid, _, _) : _) _   = oid

    totals = foldr (\(_, a, d) (as, ds, n) -> (as + a, ds + d, n + 1)) (0, 0, 0)

    walk cursor acc = do
      raw <- runQuery client historyQuery
        ([ ("owner",    String (rpOwner repo))
         , ("name",     String (rpName repo))
         , ("authorId", String (pfId profile))
         ] <> [("cursor", String c) | Just c <- [cursor]])
      (nodes, next) <- decodeOrDie ("history of " <> T.unpack (rpNameWithOwner repo)) parser raw
      let (before, rest) = break (\(oid, _, _) -> Just oid == sentinel) nodes
          acc'           = acc <> before
      if not (null rest)
        then pure (acc', True)
        else case next of
               Just c  -> walk (Just c) acc'
               Nothing -> pure (acc', False)

    parser = withObject "data" $ \d -> do
      mref <- d .:? "repository" >>= traverse (.:? "defaultBranchRef")
      case mref of
        Just (Just ref) -> do
          hist  <- ref .: "target" >>= (.: "history")
          nodes <- hist .: "nodes" >>= mapM parseCommit
          info  <- hist .: "pageInfo"
          more  <- info .: "hasNextPage"
          end   <- info .:? "endCursor"
          pure (nodes, if more then end else Nothing)
        _ -> pure ([], Nothing)

    parseCommit = withObject "commit" $ \c ->
      (,,) <$> c .: "oid" <*> c .: "additions" <*> c .: "deletions"
