{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.Class where

import Control.Applicative.Permutations
import Control.Arrow (left)
import Control.DeepSeq
import Control.Monad
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.Property
import ICal.PropertyType.Class
import ICal.PropertyType.Date
import ICal.PropertyType.DateTime
import ICal.PropertyType.Duration
import ICal.PropertyType.RecurrenceRule
import ICal.UnfoldedLine
import Text.Megaparsec

parseComponentFromContentLines :: (Validity component, IsComponent component) => [ContentLine] -> Either String component
parseComponentFromContentLines = left errorBundlePretty . parse componentSectionP ""

type CP = Parsec Void [ContentLine]

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
    go2 cls = case parse componentP "subcomponent" cls of
      Left err -> fail $ show err
      Right a -> pure a

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
    go2 cls = case parse componentP "subcomponent" cls of
      Left err -> fail $ show err
      Right a -> pure a

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
    go2 cls = case parse componentP "subcomponent" cls of
      Left err -> fail $ show err
      Right a -> pure a

    nameA = componentName (Proxy :: Proxy a)
    nameB = componentName (Proxy :: Proxy b)

parseSomeSubcomponents :: forall a. (IsComponent a) => [ContentLine] -> CP (NonEmpty a)
parseSomeSubcomponents cls = do
  cs <- parseManySubcomponents cls
  case NE.nonEmpty cs of
    Nothing -> fail "expected at least one subcompent"
    Just ne -> pure ne
