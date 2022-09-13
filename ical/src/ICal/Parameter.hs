{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Parameter where

import Control.DeepSeq
import Control.Monad
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.ContentLine

-- | Parameters
--
-- === Laws
--
-- * The 'NonEmpty ParamValue' that is built is valid:
--
-- >>> forAllValid $ \parameter -> isValid (parameterB parameter)
--
-- * Anything parsed is valid:
--
-- >>> forAllValid $ \paramValues -> isValid (parameterP paramValues)
--
-- * The parameter roundtrips through 'ContentLineValue'.
--
-- >>> forAllValid $ \parameter -> parameterP (parameterB parameter) == Right parameter
class IsParameter param where
  -- Name of the parameter
  parameterName :: Proxy param -> ParamName

  -- | Parser for the parameter
  parameterP :: NonEmpty ParamValue -> Either String param

  -- | Builder for the parameter
  parameterB :: param -> NonEmpty ParamValue

lookupParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Maybe (Either String param)
lookupParam m = do
  let name = parameterName (Proxy :: Proxy param)
  pvs <- M.lookup name m
  pure $ parameterP pvs

optionalParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Either String (Maybe param)
optionalParam m =
  let name = parameterName (Proxy :: Proxy param)
   in mapM parameterP (M.lookup name m)

optionalParamSet ::
  forall param.
  (Ord param, IsParameter param) =>
  Map ParamName (NonEmpty ParamValue) ->
  Either String (Maybe (Set param))
optionalParamSet m =
  let name = parameterName (Proxy :: Proxy param)
   in mapM
        (fmap S.fromList . mapM (parameterP . (:| [])) . NE.toList)
        (M.lookup name m)

requireParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Either String param
requireParam m = case lookupParam m of
  Just errOrResult -> errOrResult
  Nothing ->
    Left $
      unlines
        [ "Parameter not found: " <> show (parameterName (Proxy :: Proxy param)),
          "while looking through these parameters:",
          show m
        ]

paramMap :: forall param. IsParameter param => param -> Map ParamName (NonEmpty ParamValue)
paramMap param = M.singleton (parameterName (Proxy :: Proxy param)) (parameterB param)

setParamMap :: forall param. IsParameter param => Set param -> Map ParamName (NonEmpty ParamValue)
setParamMap params = case NE.nonEmpty (map parameterB (S.toList params)) of
  Nothing -> M.empty
  Just ne -> M.singleton (parameterName (Proxy :: Proxy param)) (sconcat ne)

singleParamP :: (ParamValue -> Either String a) -> NonEmpty ParamValue -> Either String a
singleParamP func = \case
  value :| [] -> func value
  _ -> Left "Expected one parameter value, but got multiple."

-- TODO figure out if this text should be case-insensitive
anySingleParamP :: (CI Text -> Either String a) -> NonEmpty ParamValue -> Either String a
anySingleParamP func = singleParamP $ \case
  UnquotedParam c -> func c
  QuotedParam t -> func (CI.mk t)

singleParamB :: (a -> ParamValue) -> a -> NonEmpty ParamValue
singleParamB func = (:| []) . func

-- TODO figure out if this text should be case-insensitive
anySingleParamB :: (a -> CI Text) -> a -> NonEmpty ParamValue
anySingleParamB func = singleParamB $ \a ->
  let ci = func a
      o = CI.original ci
   in if haveToQuoteText o
        then QuotedParam o
        else UnquotedParam ci

-- | Time Zone Identifier
--
-- [section 3.2.19](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.19)
--
-- @
-- Parameter Name:  TZID
--
-- Purpose:  To specify the identifier for the time zone definition for
--    a time component in the property value.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     tzidparam  = "TZID" "=" [tzidprefix] paramtext
--
--     tzidprefix = "/"
--
-- Description:  This parameter MUST be specified on the "DTSTART",
--    "DTEND", "DUE", "EXDATE", and "RDATE" properties when either a
--    DATE-TIME or TIME value type is specified and when the value is
--    neither a UTC or a "floating" time.  Refer to the DATE-TIME or
--    TIME value type definition for a description of UTC and "floating
--    time" formats.  This property parameter specifies a text value
--
--    that uniquely identifies the "VTIMEZONE" calendar component to be
--    used when evaluating the time portion of the property.  The value
--    of the "TZID" property parameter will be equal to the value of the
--    "TZID" property for the matching time zone definition.  An
--    individual "VTIMEZONE" calendar component MUST be specified for
--    each unique "TZID" parameter value specified in the iCalendar
--    object.
--
--    The parameter MUST be specified on properties with a DATE-TIME
--    value if the DATE-TIME is not either a UTC or a "floating" time.
--    Failure to include and follow VTIMEZONE definitions in iCalendar
--    objects may lead to inconsistent understanding of the local time
--    at any given location.
--
--    The presence of the SOLIDUS character as a prefix, indicates that
--    this "TZID" represents a unique ID in a globally defined time zone
--    registry (when such registry is defined).
--
--       Note: This document does not define a naming convention for
--       time zone identifiers.  Implementers may want to use the naming
--       conventions defined in existing time zone specifications such
--       as the public-domain TZ database [TZDB].  The specification of
--       globally unique time zone identifiers is not addressed by this
--       document and is left for future study.
--
--    The following are examples of this property parameter:
--
--     DTSTART;TZID=America/New_York:19980119T020000
--
--     DTEND;TZID=America/New_York:19980119T030000
--
--    The "TZID" property parameter MUST NOT be applied to DATE
--    properties and DATE-TIME or TIME properties whose time values are
--    specified in UTC.
--
--    The use of local time in a DATE-TIME or TIME value without the
--    "TZID" property parameter is to be interpreted as floating time,
--    regardless of the existence of "VTIMEZONE" calendar components in
--    the iCalendar object.
--
--    For more information, see the sections on the value types DATE-
--    TIME and TIME.
-- @
newtype TZIDParam = TZIDParam
  { unTZIDParam ::
      -- We assume that TZID parameters are case-insensitive because the examples in the spec are not quoted and the spec says:
      -- @
      -- Property parameter values that are not in quoted-strings are case-
      -- insensitive.
      -- @
      CI Text
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, IsString, Read)

instance Validity TZIDParam where
  validate p@(TZIDParam ci) =
    mconcat
      [ genericValidate p,
        decorateText (CI.original ci) validateSafeChar
      ]

instance NFData TZIDParam

instance IsParameter TZIDParam where
  parameterName Proxy = "TZID"
  parameterP = anySingleParamP $ Right . TZIDParam
  parameterB = anySingleParamB unTZIDParam

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
    OtherType !ParamValue
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ValueDataType

instance NFData ValueDataType

instance IsParameter ValueDataType where
  parameterName Proxy = "VALUE"
  parameterP =
    singleParamP $
      Right
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
              _ -> OtherType pv
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
    OtherType pv -> pv
