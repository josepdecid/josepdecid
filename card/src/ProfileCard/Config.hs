{-# LANGUAGE OverloadedStrings #-}

-- | Everything a human is expected to tweak lives here.
module ProfileCard.Config
  ( Config (..)
  , Theme (..)
  , defaultConfig
  , lightTheme
  , darkTheme
  ) where

import Data.Text (Text)

data Config = Config
  { cfgLogin      :: Text    -- ^ GitHub handle the card is about.
  , cfgTitle      :: Text    -- ^ Prompt line, neofetch style: @user\@host@.
  , cfgOS         :: Text    -- ^ Rendered as the "OS" row.
  , cfgAsciiPath  :: FilePath -- ^ Optional text art for the left panel.
                              --   Absent means draw the Haskell logo.
  , cfgCachePath  :: FilePath
  , cfgLightOut   :: FilePath
  , cfgDarkOut    :: FilePath
  , cfgTopLangs   :: Int     -- ^ How many languages to list and swatch.
  , cfgIgnoreLangs :: [Text] -- ^ Languages to leave out of the bar entirely.
                             --   Notebooks and generated files otherwise bury
                             --   everything actually written by hand.
  } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { cfgLogin     = "josepdecid"
  , cfgTitle     = "josepdecid@github"
  , cfgOS        = "macOS & CachyOS"
  , cfgAsciiPath = "card/assets/ascii.txt"
  , cfgCachePath = "cache/loc.json"
  , cfgLightOut  = "assets/light_mode.svg"
  , cfgDarkOut   = "assets/dark_mode.svg"
  , cfgTopLangs  = 6
  , cfgIgnoreLangs = ["Jupyter Notebook", "Makefile", "ShaderLab", "HLSL"]
  }

-- | Colours for one rendering of the card. Two instances are produced per run
-- so the README can swap them with @prefers-color-scheme@.
data Theme = Theme
  { thName       :: Text
  , thBackground :: Text
  , thBorder     :: Text
  , thPrompt     :: Text  -- ^ The @user\@host@ line.
  , thKey        :: Text  -- ^ Left-hand labels.
  , thValue      :: Text  -- ^ Right-hand values.
  , thAccent     :: Text  -- ^ Numbers worth noticing.
  , thAdded      :: Text  -- ^ Insertions, in the green a diff would use.
  , thDeleted    :: Text  -- ^ Deletions, likewise.
  , thMuted      :: Text  -- ^ Rules, footnotes.
  , thAscii      :: Text
  , thCursor     :: Text
  } deriving (Show)

lightTheme :: Theme
lightTheme = Theme
  { thName       = "light"
  , thBackground = "#fffefe"
  , thBorder     = "#d0d7de"
  , thPrompt     = "#1f6feb"
  , thKey        = "#8250df"
  , thValue      = "#24292f"
  , thAccent     = "#bc4c00"
  , thAdded      = "#1a7f37"
  , thDeleted    = "#cf222e"
  , thMuted      = "#6e7781"
  , thAscii      = "#5e2ca5"
  , thCursor     = "#1f6feb"
  }

darkTheme :: Theme
darkTheme = Theme
  { thName       = "dark"
  , thBackground = "#0d1117"
  , thBorder     = "#30363d"
  , thPrompt     = "#58a6ff"
  , thKey        = "#d2a8ff"
  , thValue      = "#c9d1d9"
  , thAccent     = "#ffa657"
  , thAdded      = "#3fb950"
  , thDeleted    = "#f85149"
  , thMuted      = "#8b949e"
  , thAscii      = "#bc8cff"
  , thCursor     = "#58a6ff"
  }
