{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ICal.Parameter.ValueDataType where

import Control.DeepSeq
import Data.Proxy
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.Parameter.Class

-- | Value Data Type
--
-- [section 3.2.20](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.20)
--
-- @
-- Parameter Name:  VALUE
--
-- Purpose:  To explicitly specify the value type format for a property
--    value.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     valuetypeparam = "VALUE" "=" valuetype
--
--     valuetype  = ("BINARY"
--                / "BOOLEAN"
--                / "CAL-ADDRESS"
--                / "DATE"
--                / "DATE-TIME"
--                / "DURATION"
--                / "FLOAT"
--                / "INTEGER"
--                / "PERIOD"
--                / "RECUR"
--                / "TEXT"
--                / "TIME"
--                / "URI"
--                / "UTC-OFFSET"
--                / x-name
--                ; Some experimental iCalendar value type.
--                / iana-token)
--                ; Some other IANA-registered iCalendar value type.
--
-- Description:  This parameter specifies the value type and format of
--    the property value.  The property values MUST be of a single value
--    type.  For example, a "RDATE" property cannot have a combination
--    of DATE-TIME and TIME value types.
--
--    If the property's value is the default value type, then this
--    parameter need not be specified.  However, if the property's
--    default value type is overridden by some other allowable value
--    type, then this parameter MUST be specified.
--
--    Applications MUST preserve the value data for x-name and iana-
--    token values that they don't recognize without attempting to
--    interpret or parse the value data.
-- @
data ValueDataType
  = TypeBinary
  | TypeBoolean
  | TypeCalendarAddress
  | TypeDate
  | TypeDateTime
  | TypeDuration
  | TypeFloat
  | TypeInteger
  | TypePeriod
  | TypeRecur
  | TypeText
  | TypeTime
  | TypeURI
  | TypeUTCOffset
  | -- | Other value type
    --
    -- @
    -- Applications MUST preserve the value data for x-name and iana-
    -- token values that they don't recognize without attempting to
    -- interpret or parse the value data.
    -- @
    TypeOther !ParamValue
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ValueDataType

instance NFData ValueDataType

instance IsParameter ValueDataType where
  parameterName Proxy = "VALUE"
  parameterP =
    singleParamP $
      pure
        . ( \pv -> case paramValueCI pv of
              "BINARY" -> TypeBinary
              "BOOLEAN" -> TypeBoolean
              "CAL-ADDRESS" -> TypeCalendarAddress
              "DATE" -> TypeDate
              "DATE-TIME" -> TypeDateTime
              "DURATION" -> TypeDuration
              "FLOAT" -> TypeFloat
              "INTEGER" -> TypeInteger
              "PERIOD" -> TypePeriod
              "RECUR" -> TypeRecur
              "TEXT" -> TypeText
              "TIME" -> TypeTime
              "URI" -> TypeURI
              "UTC-OFFSET" -> TypeUTCOffset
              _ -> TypeOther pv
          )
  parameterB = singleParamB $ \case
    TypeBinary -> "BINARY"
    TypeBoolean -> "BOOLEAN"
    TypeCalendarAddress -> "CAL-ADDRESS"
    TypeDate -> "DATE"
    TypeDateTime -> "DATE-TIME"
    TypeDuration -> "DURATION"
    TypeFloat -> "FLOAT"
    TypeInteger -> "INTEGER"
    TypePeriod -> "PERIOD"
    TypeRecur -> "RECUR"
    TypeText -> "TEXT"
    TypeTime -> "TIME"
    TypeURI -> "URI"
    TypeUTCOffset -> "UTC-OFFSET"
    TypeOther pv -> pv
