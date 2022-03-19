{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module ICal.UnfoldedLine where

import Control.Arrow (left)
import Control.Monad
import Data.ByteString (ByteString)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder as Text
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)

-- | An unfolded line of text, the required newlines are already stripped.
newtype UnfoldedLine = UnfoldedLine {unUnfoldedLine :: Text}
  deriving stock (Eq, Generic)
  deriving newtype (Show, Read, IsString)

instance Validity UnfoldedLine -- TODO: requirement that it's a single line?

parseUnfoldedLinesByteString :: ByteString -> Either String [UnfoldedLine]
parseUnfoldedLinesByteString = (left show . TE.decodeUtf8') >=> parseUnfoldedLinesText

-- TODO we can probably do something more efficient here with megaparsec.
parseUnfoldedLinesText :: Text -> Either String [UnfoldedLine]
parseUnfoldedLinesText t =
  if T.null t
    then Right []
    else
      if T.takeEnd 2 t == "\r\n"
        then
          Right
            . map UnfoldedLine
            . init -- Ignore the last, empty, line
            . T.splitOn "\r\n"
            -- Replace a newline + tab character.
            . T.replace "\r\n\t" ""
            -- Replace a newline + space character.
            . T.replace "\r\n " ""
            $ t
        else Left "Document did not end in a crlf."

renderUnfoldedLinesText :: [UnfoldedLine] -> Text
renderUnfoldedLinesText = LT.toStrict . LTB.toLazyText . unfoldedLinesB

renderUnfoldedLinesByteString :: [UnfoldedLine] -> ByteString
renderUnfoldedLinesByteString = TE.encodeUtf8 . renderUnfoldedLinesText

unfoldedLinesB :: [UnfoldedLine] -> Text.Builder
unfoldedLinesB = foldMap unfoldedLineB

unfoldedLineB :: UnfoldedLine -> Text.Builder
unfoldedLineB = go . unUnfoldedLine
  where
    go t =
      if T.length t < maxLineLen
        then LTB.fromText t <> LTB.fromString "\r\n"
        else LTB.fromText (T.take maxLineLen t) <> LTB.fromString "\r\n " <> go (T.drop maxLineLen t)
    -- [Section 3.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.1)
    -- "Lines of text SHOULD NOT be longer than 75 octets, excluding the line breaks."
    maxLineLen = 73 -- Just to be sure
