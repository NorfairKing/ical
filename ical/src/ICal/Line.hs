{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ICal.Line where

import Data.ByteString (ByteString)
import Data.String
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)

newtype UnfoldedLine = UnfoldedLine {unUnfoldedLine :: Text}
  deriving stock (Eq, Generic)
  deriving newtype (Show, Read, IsString)

instance Validity UnfoldedLine

parseUnfoldedLinesByteString :: ByteString -> Either String [UnfoldedLine]
parseUnfoldedLinesByteString = undefined

parseUnfoldedLinesText :: Text -> Either String [UnfoldedLine]
parseUnfoldedLinesText = undefined

renderUnfoldedLinesText :: [UnfoldedLine] -> Text
renderUnfoldedLinesText = undefined

renderUnfoldedLinesByteString :: [UnfoldedLine] -> ByteString
renderUnfoldedLinesByteString = TE.encodeUtf8 . renderUnfoldedLinesText
