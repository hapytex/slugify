{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Text.Slugify
Description : The module to convert text into /slug/s. A slug is a hyphen separated string of words.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

The module to convert text into /slug/s. A slug is a hyphen separated string of words. Slugs are often used
to make visually pleasant URLs by transforming for example the title of an article into a slug, and use this
in the URL, for more information see the <https://en.wikipedia.org/wiki/Clean_URL#Slug Wikipedia section>.
-}

module Text.Slugify (
  -- * Slug modes
    SlugMode(SlugAscii, SlugUnicode)
  -- * Character tests
  , isWordSeparator, isWordChar, isRetainChar
  -- * Slug algorithms
  , slugify, slugifyUnicode, slugifyWith
  ) where

import Data.Char(GeneralCategory(LowercaseLetter, ModifierLetter, OtherLetter, UppercaseLetter, TitlecaseLetter, DecimalNumber, LetterNumber, OtherNumber, LineSeparator, ParagraphSeparator, Space), isAscii, generalCategory)
import Data.Text(Text, dropAround, intercalate, split, toLower)
import qualified Data.Text as T
import Data.Text.Normalize(NormalizationMode(NFKC, NFKD), normalize)

-- | The given mode to slugify a 'Text' object.
data SlugMode
  = SlugAscii  -- ^ Slugify by removing diacritics and only retain ASCII characters.
  | SlugUnicode  -- ^ Slugify by allowing unicode characters.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Check if the given 'Char'acter is a word separator, for the given slugify
-- algorithm.
isWordSeparator
  :: Char  -- ^ The given 'Char'acter to check.
  -> Bool  -- ^ 'True' if the given character is a separator; 'False' otherwise.
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
isWordSeparator '_' = True
isWordSeparator c = case generalCategory c of
    LineSeparator -> True         -- Zl
    ParagraphSeparator -> True    -- Zp
    Space -> True                 -- Zs
    _ -> False

-- | Check if the given 'Char'acter is considered a word character for the
-- slugify algorithm.
isWordChar
  :: Char  -- ^ The given 'Char'acter to check.
  -> Bool  -- ^ 'True' if it is a word character; 'False' otherwise.
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

_postDrop :: Char -> Bool
_postDrop '_' = True
_postDrop '-' = True
_postDrop _ = False

-- | Check if the given character is retained by the slugify algorithm.
isRetainChar
  :: Char  -- ^ The given 'Char'acter to check.
  -> Bool  -- ^ True if the given 'Char'acter will be retained; 'False' otherwise. Some of these characters will however be converted to a hyphen (@-@), and thus eventually will only produce a different character in the slug.
isRetainChar c = isWordChar c || isWordSeparator c

_slugify' :: Text -> Text
_slugify' = dropAround _postDrop . intercalate "-" . filter (not . T.null) . split isWordSeparator . T.filter isRetainChar . toLower

-- | Slugify the given 'Text' object and retain Unicode characters.
slugifyUnicode
  :: Text  -- ^ The given text to convert to a slug.
  -> Text  -- ^ The corresponding /slug/.
slugifyUnicode = _slugify' . normalize NFKC

-- | Slugify the given 'Text' object and remove diacritics and convert
-- characters to the corresponding ASCII equivalent.
slugify
  :: Text  -- ^ The given text to convert to a slug.
  -> Text  -- ^ The corresponding /slug/.
slugify = _slugify' . T.filter isAscii . normalize NFKD

-- | Slugify the given 'Text' with the given 'SlugMode'.
slugifyWith
  :: SlugMode  -- ^ The given mode to slugify.
  -> Text  -- ^ The given text to convert to a slug.
  -> Text  -- ^ The corresponding /slug/.
slugifyWith SlugAscii = slugify
slugifyWith SlugUnicode = slugifyUnicode
