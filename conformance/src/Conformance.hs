{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Conformance where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Functor.Identity

-- | A conforming monad transformer to compute a result according to a spec.
--
-- RFC 2119 describes these terms:
--
-- 1. MUST and MUST NOT:
--    These describe absolute requirements or absolute prohibitions.
--    However, some implementations still do not adhere to these.
--    Some of those situations are fixable, and some are not.
--
--    If the situation is fixable, we error with an error of type @ue@.
--
--    If the situation is fixable, we can either error out (a strict implementation) with an error of type @fe@ or apply the fix.
--    The @fe@ parameter represents fixable errors, which can either be emitted as warnings, or errored on.
--    A predicate @(fe -> Bool)@ decides whether to fix the error. (The predicate returns True if the fixable error is to be fixed.)
-- 2. SHOULD and SHOULD NOT:
--    These describe weaker requirements or prohibitions.
--    The @w@ parameter represents warnings to represent cases where requirements or prohibitions were violated.
newtype ConformT ue fe w m a = ConformT
  { unConformT ::
      ReaderT (fe -> Bool) (WriterT (Notes fe w) (ExceptT (HaltReason ue fe) m)) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (fe -> Bool),
      MonadError (HaltReason ue fe),
      MonadWriter (Notes fe w)
    )

-- We cannot have 'Alternative' because there is no 'empty', but we don't want to depend on some dependency that provides 'Alt': https://hackage.haskell.org/package/semigroupoids-5.3.7/docs/Data-Functor-Alt.html
-- because it's a huge dependency.
altConform :: Monad m => ConformT ue fe w m a -> ConformT ue fe w m a -> ConformT ue fe w m a
altConform cf1 cf2 = do
  decider <- ask
  errOrTup1 <- lift $ runConformTFlexible decider cf1
  case errOrTup1 of
    Right (a, notes) -> do
      tell notes
      pure a
    Left _ -> do
      errOrTup2 <- lift $ runConformTFlexible decider cf2
      case errOrTup2 of
        Right (a, notes) -> do
          tell notes
          pure a
        Left err2 -> throwError err2

instance MonadTrans (ConformT ue fe w) where
  lift = ConformT . lift . lift . lift

data HaltReason ue fe
  = HaltedBecauseOfUnfixableError !ue
  | HaltedBecauseOfStrictness !fe
  deriving (Show, Eq)

instance (Exception ue, Exception fe) => Exception (HaltReason ue fe) where
  displayException = \case
    HaltedBecauseOfUnfixableError ue -> displayException ue
    HaltedBecauseOfStrictness fe -> displayException fe

data Notes fe w = Notes
  { notesFixableErrors :: ![fe],
    notesWarnings :: ![w]
  }
  deriving (Show, Eq)

instance Semigroup (Notes w fe) where
  (<>) (Notes fes1 ws1) (Notes fes2 ws2) =
    Notes
      { notesFixableErrors = fes1 ++ fes2,
        notesWarnings = ws1 ++ ws2
      }

instance Monoid (Notes w fe) where
  mempty = Notes [] []
  mappend = (<>)

nullNotes :: Notes w fe -> Bool
nullNotes Notes {..} = null notesFixableErrors && null notesWarnings

-- | Most flexible way to run a 'ConformT'
runConformTFlexible ::
  (fe -> Bool) ->
  ConformT ue fe w m a ->
  m (Either (HaltReason ue fe) (a, Notes fe w))
runConformTFlexible predicate (ConformT func) = runExceptT (runWriterT (runReaderT func predicate))

-- | Don't fix any fixable errors.
--
-- This is standard-compliant.
runConformT ::
  Monad m =>
  ConformT ue fe w m a ->
  m (Either (HaltReason ue fe) (a, [w]))
runConformT func = do
  errOrTup <- runConformTFlexible fixNone func
  pure $ do
    (a, notes) <- errOrTup
    pure (a, notesWarnings notes)

-- | Don't fix any fixable errors, and don't allow any warnings either
--
-- This is standard-compliant, but potentially more strict than necessary.
runConformTStrict ::
  Monad m =>
  ConformT ue fe w m a ->
  m (Either (Either ue (Notes fe w)) a)
runConformTStrict func = do
  errOrTup <- runConformTFlexible fixNone func
  pure $ case errOrTup of
    Left haltReason -> case haltReason of
      HaltedBecauseOfUnfixableError ue -> Left (Left ue)
      -- Cannot happen, but is fine if it does.
      HaltedBecauseOfStrictness fe -> Left (Right (Notes [fe] []))
    Right (a, notes) -> if nullNotes notes then Right a else Left (Right notes)

-- | Fix as much as possible
--
-- That this is __not__ standard-compliant.
runConformTLenient ::
  Monad m =>
  ConformT ue fe w m a ->
  m (Either ue (a, Notes fe w))
runConformTLenient func = do
  errOrTup <- runConformTFlexible fixAll func
  pure $ case errOrTup of
    Left hr -> Left $ case hr of
      HaltedBecauseOfStrictness _ -> error "cannot happen, but this cannot be proven to the compiler."
      HaltedBecauseOfUnfixableError ue -> ue
    Right r -> Right r

type Conform ue fe w = ConformT ue fe w Identity

-- | Most flexible way to run a 'Conform'
runConformFlexible ::
  -- | Predicate to select fixable errors that should be fixed and thereby
  -- become a warning instead.
  (fe -> Bool) ->
  Conform ue fe w a ->
  Either (HaltReason ue fe) (a, Notes fe w)
runConformFlexible predicate = runIdentity . runConformTFlexible predicate

-- | Don't fix any fixable errors.
--
-- This is standard-compliant
runConform ::
  Conform ue fe w a ->
  Either (HaltReason ue fe) (a, [w])
runConform = runIdentity . runConformT

-- | Don't fix any fixable errors, and don't allow any warnings either
--
-- This is standard-compliant, but potentially more strict than necessary.
runConformStrict ::
  Conform ue fe w a ->
  Either (Either ue (Notes fe w)) a
runConformStrict = runIdentity . runConformTStrict

-- | Fix as much as possible
--
-- That this is __not__ standard-compliant.
runConformLenient ::
  Conform ue fe w a ->
  Either ue (a, Notes fe w)
runConformLenient = runIdentity . runConformTLenient

-- | Try to run a conform function, return Nothing if there were unfixable
-- errors or unfixed fixable errors.
tryConform ::
  Monad m =>
  ConformT ue fe w m a ->
  ConformT ue fe w m (Maybe a)
tryConform c = ConformT $ ReaderT $ \predicate -> do
  errOrRes <- lift $ lift $ runConformTFlexible predicate c
  case errOrRes of
    Left _ -> pure Nothing
    Right (result, notes) -> do
      tell notes
      pure (Just result)

fixAll :: fe -> Bool
fixAll = const True

fixNone :: fe -> Bool
fixNone = const False

conformFromEither :: Monad m => Either ue a -> ConformT ue fe w m a
conformFromEither = \case
  Left ue -> unfixableError ue
  Right r -> pure r

conformMapAll :: Monad m => (ue1 -> ue2) -> (fe1 -> fe2) -> (w1 -> w2) -> ConformT ue1 fe1 w1 m a -> ConformT ue2 fe2 w2 m a
conformMapAll ueFunc feFunc wFunc (ConformT cFunc) =
  ConformT $
    mapReaderT
      ( mapWriterT
          ( \func -> do
              (res, notes) <- withExceptT haltReasonMapError func
              pure (res, notesMapError notes)
          )
      )
      (withReaderT (\predicate -> predicate . feFunc) cFunc)
  where
    notesMapError (Notes fes wes) =
      Notes
        (map feFunc fes)
        (map wFunc wes)
    haltReasonMapError = \case
      HaltedBecauseOfUnfixableError ue -> HaltedBecauseOfUnfixableError (ueFunc ue)
      HaltedBecauseOfStrictness fe -> HaltedBecauseOfStrictness (feFunc fe)

conformMapErrors :: Monad m => (ue1 -> ue2) -> (fe1 -> fe2) -> ConformT ue1 fe1 w m a -> ConformT ue2 fe2 w m a
conformMapErrors ueFunc feFunc = conformMapAll ueFunc feFunc id

conformMapError ::
  Monad m =>
  (ue1 -> ue2) ->
  ConformT ue1 fe w m a ->
  ConformT ue2 fe w m a
conformMapError func = conformMapErrors func id

conformMapFixableError ::
  Monad m =>
  (fe1 -> fe2) ->
  ConformT ue fe1 w m a ->
  ConformT ue fe2 w m a
conformMapFixableError = conformMapErrors id

emitWarning :: Monad m => w -> ConformT ue fe w m ()
emitWarning w = tell (Notes [] [w])

emitFixableError :: Monad m => fe -> ConformT ue fe w m ()
emitFixableError fe = do
  predicate <- ask
  if predicate fe
    then tell (Notes [fe] [])
    else throwError (HaltedBecauseOfStrictness fe)

unfixableError :: Monad m => ue -> ConformT ue fe w m a
unfixableError ue = throwError (HaltedBecauseOfUnfixableError ue)
