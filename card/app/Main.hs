{-# LANGUAGE OverloadedStrings #-}

-- | Fetches the stats and writes @assets\/light_mode.svg@ and
-- @assets\/dark_mode.svg@. Run from the repository root.
--
-- All file I/O goes through 'BS.ByteString' and an explicit UTF-8 codec: the
-- card and the ASCII panel both contain non-ASCII characters, and a CI runner
-- with a POSIX locale would otherwise fail on them at runtime.
module Main (main) where

import           Data.Char          (GeneralCategory (Format), generalCategory)
import           Data.List          (dropWhileEnd)
import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           System.Directory   (createDirectoryIfMissing, doesFileExist)
import           System.Environment (getArgs)
import           System.Exit        (die)
import           System.FilePath    (takeDirectory)
import           System.IO          (hPutStrLn, stderr)

import           ProfileCard.Config
import           ProfileCard.Render (render)
import           ProfileCard.Stats  (gather)

main :: IO ()
main = do
  cfg   <- configFromArgs =<< getArgs
  ascii <- readAscii (cfgAsciiPath cfg)
  stats <- gather cfg

  mapM_ (write cfg stats ascii)
    [ (lightTheme, cfgLightOut cfg)
    , (darkTheme,  cfgDarkOut  cfg)
    ]
  where
    write cfg stats ascii (theme, path) = do
      createDirectoryIfMissing True (takeDirectory path)
      BS.writeFile path (TE.encodeUtf8 (render cfg theme stats ascii))
      hPutStrLn stderr ("wrote " <> path)

-- | The only argument is an optional login, so a fork can be tried without
-- editing 'defaultConfig' first.
configFromArgs :: [String] -> IO Config
configFromArgs []      = pure defaultConfig
configFromArgs [login] = pure defaultConfig
  { cfgLogin = T.pack login
  , cfgTitle = T.pack login <> "@github"
  }
configFromArgs _ = die "usage: profile-card [github-login]"

-- | Optional. With no file the card draws the Haskell logo instead, which is
-- the normal case; text art is an opt-in override.
readAscii :: FilePath -> IO T.Text
readAscii path = do
  ok <- doesFileExist path
  if ok
    then dedent . sanitise . T.stripEnd . TE.decodeUtf8 <$> BS.readFile path
    else pure ""

-- | Replace Unicode format characters with spaces.
--
-- The panel is a fixed grid: every glyph has to occupy exactly one cell or the
-- rows after it slide left and the picture shears. Format characters (a soft
-- hyphen, a zero-width joiner) are invisible /and/ zero-width, and brightness
-- ramps built by walking a codepoint range pick them up without meaning to. A
-- space is the faithful substitute — it renders as nothing either way, it just
-- also takes up its cell.
sanitise :: T.Text -> T.Text
sanitise = T.map (\c -> if generalCategory c == Format then ' ' else c)

-- | Drop blank rows at the top and bottom, and the indent common to every row.
--
-- Generators pad art out to a rectangle, so the picture usually sits some
-- distance inside its own bounding box. Left as-is that padding reads as a
-- wonky left margin on the card, because the panel positions the text box and
-- not the drawing inside it. Runs after 'sanitise' so a leading format
-- character counts as the space it has become.
dedent :: T.Text -> T.Text
dedent txt = case rows of
  [] -> ""
  _  -> T.intercalate "\n" (map (T.drop indent) rows)
  where
    blank   = T.null . T.strip
    rows    = dropWhileEnd blank (dropWhile blank (T.lines txt))
    indent  = minimum (map leading (filter (not . blank) rows))
    leading = T.length . T.takeWhile (== ' ')
