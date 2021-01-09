{-# LANGUAGE OverloadedStrings #-}

module Text.Slugify where

import Data.Char(GeneralCategory(LowercaseLetter, ModifierLetter, OtherLetter, UppercaseLetter, TitlecaseLetter, DecimalNumber, LetterNumber, OtherNumber, ConnectorPunctuation, LineSeparator, ParagraphSeparator, Space), isAscii, generalCategory)
import Data.Text(Text, dropAround, intercalate, split, toLower)
import qualified Data.Text as T
import Data.Text.Normalize(NormalizationMode(NFKC, NFKD), normalize)

isWordSeparator :: Char -> Bool
isWordSeparator '-' = True
isWordSeparator '\x0b' = True
isWordSeparator '\x0c' = True
isWordSeparator '\x1c' = True
isWordSeparator '\x85' = True
isWordSeparator '\t' = True
isWordSeparator '\r' = True
isWordSeparator '\x1d' = True
isWordSeparator '\x1f' = True
isWordSeparator '\x1e' = True
isWordSeparator '\n' = True
isWordSeparator c = case generalCategory c of
    LineSeparator -> True         -- Zl
    ParagraphSeparator -> True    -- Zp
    Space -> True                 -- Zs
    _ -> False

isWordChar :: Char -> Bool
isWordChar '_' = True
isWordChar '\125259' = True
isWordChar '\72162' = False
isWordChar '\123215' = False
isWordChar c = case generalCategory c of
    LowercaseLetter -> True       -- Ll
    ModifierLetter -> True        -- Lm
    OtherLetter -> True           -- Lo
    UppercaseLetter -> True       -- Lu
    TitlecaseLetter -> True       -- Lt
    DecimalNumber -> True         -- Nd
    LetterNumber -> True          -- Nl
    OtherNumber -> True           -- No
    _ -> False

postDrop :: Char -> Bool
postDrop '_' = True
postDrop '-' = True
postDrop _ = False

isRetainChar :: Char -> Bool
isRetainChar c = isWordChar c || isWordSeparator c

slugify' :: Text -> Text
slugify' = dropAround postDrop . intercalate "-" . filter (not . T.null) . split isWordSeparator . T.filter isRetainChar . toLower

slugifyUnicode :: Text -> Text
slugifyUnicode = slugify' . normalize NFKC

slugify :: Text -> Text
slugify = slugify' . T.filter isAscii . normalize NFKD
