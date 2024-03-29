{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ICal.UnfoldedLine
  ( UnfoldedLine (..),
    UnfoldingError (..),
    UnfoldingFixableError (..),
    parseUnfoldedLines,
    renderUnfoldedLines,
  )
where

import Conformance
import Control.Exception
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder as Text
import Data.Validity
import Data.Validity.Text ()
import Data.Void
import GHC.Generics (Generic)

-- | An unfolded line of text, the required newlines are already stripped.
--
-- @
-- Lines of text SHOULD NOT be longer than 75 octets, excluding the line
-- break.  Long content lines SHOULD be split into a multiple line
-- representations using a line "folding" technique.  That is, a long
-- line can be split between any two characters by inserting a CRLF
-- immediately followed by a single linear white-space character (i.e.,
-- SPACE or HTAB).  Any sequence of CRLF followed immediately by a
-- single linear white-space character is ignored (i.e., removed) when
-- processing the content type.
--
-- For example, the line:
--
--   DESCRIPTION:This is a long description that exists on a long line.
--
-- Can be represented as:
--
--   DESCRIPTION:This is a lo
--    ng description
--     that exists on a long line.
--
-- The process of moving from this folded multiple-line representation
-- to its single-line representation is called "unfolding".  Unfolding
-- is accomplished by removing the CRLF and the linear white-space
-- character that immediately follows.
--
-- When parsing a content line, folded lines MUST first be unfolded
-- according to the unfolding procedure described above.
--
--     Note: It is possible for very simple implementations to generate
--     improperly folded lines in the middle of a UTF-8 multi-octet
--     sequence.  For this reason, implementations need to unfold lines
--     in such a way to properly restore the original sequence.
--
--
-- The content information associated with an iCalendar object is
-- formatted using a syntax similar to that defined by [RFC2425].  That
-- is, the content information consists of CRLF-separated content lines.
-- @
newtype UnfoldedLine = UnfoldedLine {unUnfoldedLine :: Text}
  deriving stock (Eq, Generic)
  deriving newtype (Show, Read, IsString)

instance Validity UnfoldedLine -- TODO: requirement that it's a single line?

data UnfoldingError = NoCRLFAtEndError
  deriving (Show, Eq)

instance Exception UnfoldingError where
  displayException = \case
    NoCRLFAtEndError -> "Document did not end in a crlf."

data UnfoldingFixableError = NoCRLFAtEndFixableError
  deriving (Show, Eq)

instance Exception UnfoldingFixableError where
  displayException = \case
    NoCRLFAtEndFixableError -> "Document did not end in a crlf, but it ended in END:VCALENDAR so tried to parse it anyway."

data UnfoldingWarning = LineTooLong
  deriving (Show, Eq)

instance Exception UnfoldingWarning where
  displayException = \case
    LineTooLong -> "Line was too long, so not folded according to spec: \"Lines of text SHOULD NOT be longer than 75 octets, excluding the line break.\""

-- TODO we can probably do something more efficient here with megaparsec.
parseUnfoldedLines :: Text -> Conform UnfoldingError UnfoldingFixableError Void [UnfoldedLine]
parseUnfoldedLines t
  | T.null t = pure []
  | otherwise = do
      let tryToParse =
            pure
              . map UnfoldedLine
              . init -- Ignore the last, empty, line
              -- [Section 3.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.1)
              -- @
              -- Content lines are delimited by a line break,
              -- which is a CRLF sequence (CR character followed by LF character).
              -- @
              . T.splitOn "\r\n"
              -- [Section 3.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.1)
              -- @
              -- Any sequence of CRLF followed immediately by a
              -- single linear white-space character is ignored (i.e., removed) when
              -- processing the content type.
              -- @
              -- Replace a newline + tab character.
              . T.replace "\r\n\t" ""
              -- Replace a newline + space character.
              --
              -- [Section 3.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.1)
              -- @
              -- The process of moving from this folded multiple-line representation
              -- to its single-line representation is called "unfolding".  Unfolding
              -- is accomplished by removing the CRLF and the linear white-space
              -- character that immediately follows.
              -- @
              . T.replace "\r\n " ""
      if T.takeEnd 2 t == "\r\n"
        then tryToParse t
        else do
          let t' = T.stripEnd t
          -- If the stream ends with END:VCALENDAR then we don't have to be
          -- scared that it got cut off, so we can try to fix this error.
          if T.takeEnd (T.length "END:VCALENDAR") t' == "END:VCALENDAR"
            then do
              emitFixableError NoCRLFAtEndFixableError
              tryToParse (t' <> "\r\n")
            else unfixableError NoCRLFAtEndError

renderUnfoldedLines :: [UnfoldedLine] -> Text
renderUnfoldedLines = LT.toStrict . LTB.toLazyText . unfoldedLinesB

unfoldedLinesB :: [UnfoldedLine] -> Text.Builder
unfoldedLinesB = foldMap unfoldedLineB

unfoldedLineB :: UnfoldedLine -> Text.Builder
unfoldedLineB = go . unUnfoldedLine
  where
    -- [Section 3.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.1)
    -- @
    -- Lines of text SHOULD NOT be longer than 75 octets, excluding the line
    -- break.  Long content lines SHOULD be split into a multiple line
    -- representations using a line "folding" technique.  That is, a long
    -- line can be split between any two characters by inserting a CRLF
    -- immediately followed by a single linear white-space character (i.e.,
    -- SPACE or HTAB).
    -- @
    go t =
      if T.length t < maxLineLen
        then LTB.fromText t <> LTB.fromString "\r\n"
        else LTB.fromText (T.take maxLineLen t) <> LTB.fromString "\r\n " <> go (T.drop maxLineLen t)
    maxLineLen = 73
