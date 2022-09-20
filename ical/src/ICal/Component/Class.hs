{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.Class where

import Control.Monad
import Control.Monad.Trans
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import ICal.Conformance
import ICal.ContentLine
import ICal.Property
import ICal.PropertyType.RecurrenceRule
import ICal.UnfoldedLine
import Text.Megaparsec

deriving instance Ord s => Ord (PosState s)

deriving instance (Ord s, Ord (Token s), Ord e) => Ord (ParseError s e)

deriving instance (Ord s, Ord (Token s), Ord e) => Ord (ParseErrorBundle s e)

data CalendarParseError
  = SubcomponentError !(ParseErrorBundle [ContentLine] CalendarParseError)
  | OtherError String
  deriving (Show, Eq, Ord)

parseComponentFromContentLines ::
  (Validity component, IsComponent component) =>
  [ContentLine] ->
  Conform (ParseErrorBundle [ContentLine] CalendarParseError) Void Void component
parseComponentFromContentLines = runCP componentSectionP

runCP ::
  CP a ->
  [ContentLine] ->
  Conform (ParseErrorBundle [ContentLine] CalendarParseError) Void Void a
runCP func cls = do
  errOrComponent <- runParserT func "" cls
  case errOrComponent of
    Left err -> unfixableError err
    Right component -> pure component

liftCP :: CP a -> [ContentLine] -> CP a
liftCP parserFunc cls = lift $ runCP parserFunc cls

type CP a = ParsecT CalendarParseError [ContentLine] (Conform (ParseErrorBundle [ContentLine] CalendarParseError) Void Void) a

instance VisualStream [ContentLine] where
  showTokens :: Proxy [ContentLine] -> NonEmpty ContentLine -> String
  showTokens Proxy =
    T.unpack
      . renderUnfoldedLines
      . map renderContentLineToUnfoldedLine
      . NE.toList

instance TraversableStream [ContentLine] where
  reachOffset ::
    Int ->
    PosState [ContentLine] ->
    (Maybe String, PosState [ContentLine])
  reachOffset offset posState =
    let newInput = drop offset $ pstateInput posState
        newState =
          posState
            { pstateInput = newInput,
              pstateOffset = offset,
              pstateSourcePos =
                (pstateSourcePos posState)
                  { sourceLine = mkPos (offset + 1)
                  }
            }
     in case newInput of
          [] -> (Nothing, newState)
          (cl : _) -> (Just $ T.unpack $ renderUnfoldedLines [renderContentLineToUnfoldedLine cl], newState)

-- |
--
-- === Laws
--
-- * The '[ContentLine]' that is built is valid:
--
-- >>> forAllValid $ \component -> isValid (componentB component)
--
-- * Anything parsed is valid:
--
-- >>> forAllValid $ \contentLines -> isValid (parse componentP "" contentLines)
--
-- * The property roundtrips through '[ContentLine]'.
--
-- >>> forAllValid $ \component -> parse componentP "" (DList.toList (componentB component)) == Right component
class IsComponent component where
  -- | Name for this component
  componentName :: Proxy component -> Text

  -- | Parser for this component
  componentP :: CP component

  -- | Builder for this component
  componentB :: component -> DList ContentLine

componentSectionP :: forall component. (Validity component, IsComponent component) => CP component
componentSectionP = do
  c <- sectionP (componentName (Proxy :: Proxy component)) componentP
  case prettyValidate c of
    Left err -> fail err
    Right c' -> pure c'

sectionP :: Text -> CP a -> CP a
sectionP name parser = do
  parseGivenProperty $ Begin name
  result <- parser
  parseGivenProperty $ End name
  pure result

parseGivenProperty :: IsProperty property => property -> CP ()
parseGivenProperty givenProperty = void $ single $ propertyContentLineB givenProperty

parseProperty :: IsProperty property => CP property
parseProperty = do
  contentLine <- anySingle
  case propertyContentLineP contentLine of
    Left err -> fail err
    Right p -> pure p

componentSectionB :: forall component. IsComponent component => component -> DList ContentLine
componentSectionB = sectionB (componentName (Proxy :: Proxy component)) componentB

sectionB :: Text -> (a -> DList ContentLine) -> (a -> DList ContentLine)
sectionB name func =
  (DList.singleton (propertyContentLineB (Begin name)) <>)
    . (<> DList.singleton (propertyContentLineB (End name)))
    . func

propertyListB :: IsProperty property => property -> DList ContentLine
propertyListB = DList.singleton . propertyContentLineB

propertyMListB :: IsProperty property => Maybe property -> DList ContentLine
propertyMListB = maybe DList.empty (DList.singleton . propertyContentLineB)

propertyDListB :: (Eq property, IsProperty property) => property -> property -> DList ContentLine
propertyDListB defaultValue value =
  if value == defaultValue
    then mempty
    else propertyListB value

propertySetB :: IsProperty property => Set property -> DList ContentLine
propertySetB = DList.fromList . map propertyContentLineB . S.toList

parseFirst :: forall a. IsProperty a => [ContentLine] -> CP a
parseFirst = go
  where
    name = propertyName (Proxy :: Proxy a)
    go :: [ContentLine] -> CP a
    go = \case
      [] -> fail $ "Did not find required " <> show name
      (cl : cls) ->
        if contentLineName cl == name
          then case propertyContentLineP cl of
            Right result -> pure result
            Left err -> fail err
          else go cls

parseFirstMaybe :: forall a. IsProperty a => [ContentLine] -> CP (Maybe a)
parseFirstMaybe = go
  where
    name = propertyName (Proxy :: Proxy a)
    go :: [ContentLine] -> CP (Maybe a)
    go = \case
      [] -> pure Nothing
      -- TODO do better than a linear search?
      (cl : cls) ->
        if contentLineName cl == name
          then case propertyContentLineP cl of
            Right result -> pure (Just result)
            Left err -> fail err
          else go cls

parseSet ::
  forall a.
  (Ord a, IsProperty a) =>
  [ContentLine] ->
  CP (Set a)
parseSet cls =
  fmap S.fromList $
    mapM (either fail pure . propertyContentLineP) $
      filter ((== name) . contentLineName) cls
  where
    name = propertyName (Proxy :: Proxy a)

parseSubcomponent :: forall a. (IsComponent a) => [ContentLine] -> CP a
parseSubcomponent = go1
  where
    go1 :: [ContentLine] -> CP a
    go1 = \case
      [] -> fail "No subcomponent found."
      (cl : rest) ->
        let isBegin ContentLine {..} = contentLineName == "BEGIN" && (contentLineValueRaw contentLineValue == name)
            isEnd ContentLine {..} = contentLineName == "END" && (contentLineValueRaw contentLineValue == name)
         in if isBegin cl
              then go2 $ takeWhile (not . isEnd) rest
              else go1 rest
    go2 :: [ContentLine] -> CP a
    go2 = liftCP componentP

    name = componentName (Proxy :: Proxy a)

parseManySubcomponents :: forall a. (IsComponent a) => [ContentLine] -> CP [a]
parseManySubcomponents = go1
  where
    go1 :: [ContentLine] -> CP [a]
    go1 = \case
      [] -> pure []
      (cl : rest) ->
        let isBegin ContentLine {..} = contentLineName == "BEGIN" && (contentLineValueRaw contentLineValue == name)
            isEnd ContentLine {..} = contentLineName == "END" && (contentLineValueRaw contentLineValue == name)
         in if isBegin cl
              then
                let (subComponentLines, restAfterSubcomponent) = break isEnd rest
                 in (:) <$> go2 subComponentLines <*> go1 restAfterSubcomponent
              else go1 rest
    go2 :: [ContentLine] -> CP a
    go2 = liftCP componentP

    name = componentName (Proxy :: Proxy a)

parseManySubcomponents2 :: forall a b. (IsComponent a, IsComponent b) => [ContentLine] -> CP [Either a b]
parseManySubcomponents2 = go1
  where
    go1 :: [ContentLine] -> CP [Either a b]
    go1 = \case
      [] -> pure []
      (cl : rest) ->
        let isBegin ContentLine {..} =
              contentLineName == "BEGIN"
                && ( contentLineValueRaw contentLineValue == nameA
                       || contentLineValueRaw contentLineValue == nameB
                   )
            isEnd :: forall c. IsComponent c => ContentLine -> Bool
            isEnd ContentLine {..} = contentLineName == "END" && (contentLineValueRaw contentLineValue == componentName (Proxy :: Proxy c))
         in if isBegin cl
              then
                if contentLineValueRaw (contentLineValue cl) == nameA
                  then
                    let (subComponentLines, restAfterSubcomponent) = break (isEnd @a) rest
                     in (:) <$> (Left <$> go2 subComponentLines) <*> go1 restAfterSubcomponent
                  else
                    let (subComponentLines, restAfterSubcomponent) = break (isEnd @b) rest
                     in (:) <$> (Right <$> go2 subComponentLines) <*> go1 restAfterSubcomponent
              else go1 rest
    go2 :: forall c. IsComponent c => [ContentLine] -> CP c
    go2 = liftCP componentP

    nameA = componentName (Proxy :: Proxy a)
    nameB = componentName (Proxy :: Proxy b)

parseSomeSubcomponents :: forall a. (IsComponent a) => [ContentLine] -> CP (NonEmpty a)
parseSomeSubcomponents cls = do
  cs <- parseManySubcomponents cls
  case NE.nonEmpty cs of
    Nothing -> fail "expected at least one subcompent"
    Just ne -> pure ne

validateMDateTimeStartRRule :: Maybe DateTimeStart -> Set RecurrenceRule -> Validation
validateMDateTimeStartRRule mDateTimeStart recurrenceRules =
  case mDateTimeStart of
    Nothing ->
      -- [section 3.8.2.4. Date-Time Start](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.4)
      --
      -- @
      -- This property is
      -- REQUIRED in all types of recurring calendar components that
      -- specify the "RRULE" property.
      -- @
      declare "If there is no DTSTART, then there are no recurrence rules" $
        S.null recurrenceRules
    Just dateTimeStart -> validateDateTimeStartRRule dateTimeStart recurrenceRules

validateDateTimeStartRRule :: DateTimeStart -> Set RecurrenceRule -> Validation
validateDateTimeStartRRule dateTimeStart recurrenceRules =
  decorateList (S.toList recurrenceRules) $ \recurrenceRule ->
    case recurrenceRuleUntilCount recurrenceRule of
      Just (Left u) ->
        -- [section 3.3.10.  Recurrence Rule](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
        -- @
        -- The value of the UNTIL rule part MUST have the same
        -- value type as the "DTSTART" property.
        -- Furthermore, if the
        -- "DTSTART" property is specified as a date with local time, then
        -- the UNTIL rule part MUST also be specified as a date with local
        -- time.
        -- If the "DTSTART" property is specified as a date with UTC
        -- time or a date with local time and time zone reference, then the
        -- UNTIL rule part MUST be specified as a date with UTC time.
        -- @
        let msg =
              unlines
                [ "The value type of the UNTIL rule part has the same value type as the DTSTART property.",
                  show dateTimeStart,
                  show u
                ]
         in declare msg $
              case (dateTimeStart, u) of
                (DateTimeStartDate _, UntilDate _) -> True
                (DateTimeStartDateTime _, UntilDateTime _) -> True
                _ -> False
      _ -> mempty
