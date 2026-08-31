{-# LANGUAGE OverloadedStrings #-}

-- | Presentation helpers shared by the renderer.
module ProfileCard.Format
  ( commas
  , plural
  , uptimeSince
  , escapeXml
  , tshow
  ) where

import           Data.List       (intercalate)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time       (UTCTime (..), toGregorian)

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | @1234567@ becomes @1,234,567@.
commas :: Int -> Text
commas n
  | n < 0     = "-" <> commas (negate n)
  | otherwise = T.intercalate "," . reverse . map T.reverse . T.chunksOf 3 . T.reverse $ tshow n

plural :: Int -> Text -> Text
plural 1 word = "1 " <> word
plural n word = tshow n <> " " <> word <> "s"

-- | Calendar-aware elapsed time, e.g. @11 years, 4 months, 30 days@. Borrowing
-- days from the previous month is what makes this awkward enough to want a
-- function of its own.
uptimeSince :: UTCTime -> UTCTime -> Text
uptimeSince start now = T.pack (intercalate ", " (map T.unpack parts))
  where
    (y0, m0, d0) = toGregorian (utctDay start)
    (y1, m1, d1) = toGregorian (utctDay now)

    (dayCount, borrowMonth)
      | d1 >= d0  = (d1 - d0, 0)
      | otherwise = (d1 - d0 + daysInMonth y1 (m1 - 1), 1)

    (monthCount, borrowYear)
      | m1 - borrowMonth >= m0 = (m1 - borrowMonth - m0, 0)
      | otherwise              = (m1 - borrowMonth - m0 + 12, 1)

    yearCount = fromIntegral (y1 - y0) - borrowYear :: Int

    parts = [ plural yearCount  "year"
            , plural monthCount "month"
            , plural dayCount   "day"
            ]

-- | Days in the month before @m@ of year @y@, wrapping to December of @y-1@.
daysInMonth :: Integer -> Int -> Int
daysInMonth y m
  | m < 1     = daysInMonth (y - 1) 12
  | otherwise = case m of
      2 | leap      -> 29
        | otherwise -> 28
      n | n `elem` [4, 6, 9, 11] -> 30
        | otherwise              -> 31
  where
    leap = (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0

-- | The card carries user-controlled strings (repo and language names), so
-- everything written into the SVG goes through here.
escapeXml :: Text -> Text
escapeXml = T.concatMap replace
  where
    replace '&'  = "&amp;"
    replace '<'  = "&lt;"
    replace '>'  = "&gt;"
    replace '"'  = "&quot;"
    replace '\'' = "&apos;"
    replace c    = T.singleton c
