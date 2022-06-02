{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Parameter where

import Data.CaseInsensitive (CI)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.ContentLine

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

requireParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Either String param
requireParam m = case lookupParam m of
  Nothing -> Left $ "Parameter not found: " <> show (parameterName (Proxy :: Proxy param))
  Just errOrResult -> errOrResult

paramMap :: forall param. IsParameter param => param -> Map ParamName (NonEmpty ParamValue)
paramMap param = M.singleton (parameterName (Proxy :: Proxy param)) (parameterB param)

singleParamP :: (ParamValue -> Either String a) -> NonEmpty ParamValue -> Either String a
singleParamP func = \case
  value :| [] -> func value
  _ -> Left "Expected one parameter value, but got multiple."

-- [section 3.2.19](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.19)
newtype TZIDParam = TZIDParam {unTZIDParam :: CI Text}
  deriving stock (Eq, Generic)
  deriving newtype (Show, IsString, Read)

instance Validity TZIDParam

instance IsParameter TZIDParam where
  parameterName Proxy = "TZID"
  parameterP = tzIDParamP
  parameterB = tzIDParamB

tzIDParamP :: NonEmpty ParamValue -> Either String TZIDParam
tzIDParamP = singleParamP $ \case
  UnquotedParam c -> Right $ TZIDParam {unTZIDParam = c}
  p -> Left $ "Expected TZIDParam to be unquoted, but was quoted: " <> show p

tzIDParamB :: TZIDParam -> NonEmpty ParamValue
tzIDParamB = (:| []) . UnquotedParam . unTZIDParam

-- | Frequency
--
-- Part of [section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
--
--
-- @
--     freq        = "SECONDLY" / "MINUTELY" / "HOURLY" / "DAILY"
--                 / "WEEKLY" / "MONTHLY" / "YEARLY"
-- @
--
-- @
--     The FREQ rule part identifies the type of recurrence rule.  This
--     rule part MUST be specified in the recurrence rule.  Valid values
--     include SECONDLY, to specify repeating events based on an interval
--     of a second or more; MINUTELY, to specify repeating events based
--     on an interval of a minute or more; HOURLY, to specify repeating
--     events based on an interval of an hour or more; DAILY, to specify
--     repeating events based on an interval of a day or more; WEEKLY, to
--     specify repeating events based on an interval of a week or more;
--     MONTHLY, to specify repeating events based on an interval of a
--     month or more; and YEARLY, to specify repeating events based on an
--     interval of a year or more.
-- @
data Frequency
  = Secondly
  | Minutely
  | Hourly
  | Daily
  | Weekly
  | Monthly
  | Yearly
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)

instance Validity Frequency

instance IsParameter Frequency where
  parameterName Proxy = "FREQ"
  parameterP = frequencyP
  parameterB = frequencyB

frequencyP :: NonEmpty ParamValue -> Either String Frequency
frequencyP = singleParamP $ \case
  UnquotedParam c -> case c of
    "SECONDLY" -> Right Secondly
    "MINUTELY" -> Right Minutely
    "HOURLY" -> Right Hourly
    "DAILY" -> Right Daily
    "WEEKLY" -> Right Weekly
    "MONTHLY" -> Right Monthly
    "YEARLY" -> Right Yearly
    _ -> Left $ "Unknown Frequency value: " <> show c
  p -> Left $ "Expected Frequency to be unquoted, but was quoted: " <> show p

frequencyB :: Frequency -> NonEmpty ParamValue
frequencyB =
  (:| []) . UnquotedParam . \case
    Secondly -> "SECONDLY"
    Minutely -> "MINUTELY"
    Hourly -> "HOURLY"
    Daily -> "DAILY"
    Weekly -> "WEEKLY"
    Monthly -> "MONTHLY"
    Yearly -> "YEARLY"
