{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module ICal.Conformance where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Functor.Identity
import Data.Void

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

data HaltReason ue fe
  = HaltedBecauseOfUnfixableError !ue
  | HaltedBecauseOfStrictness !fe

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
-- This is standard-complient
runConformT ::
  Monad m =>
  ConformT ue fe w m a ->
  m (Either (HaltReason ue fe) (a, Notes Void w))
runConformT func = do
  errOrTup <- runConformTFlexible fixNone func
  pure $ do
    (a, notes) <- errOrTup
    pure (a, notes {notesFixableErrors = []})

-- | Don't fix any fixable errors, and don't allow any warnings either
--
-- This is standard-complient, but potentially more strict than necessary.
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
-- That this is __not__ standard-complient.
runConformTLenient ::
  Monad m =>
  ConformT ue fe w m a ->
  m (Either (HaltReason ue Void) (a, Notes fe w))
runConformTLenient func = do
  errOrTup <- runConformTFlexible fixAll func
  pure $ case errOrTup of
    Left hr -> Left $ case hr of
      HaltedBecauseOfStrictness _ ->
        HaltedBecauseOfStrictness
          (error "cannot happen, but this cannot be proven to the compiler.")
      HaltedBecauseOfUnfixableError ue -> HaltedBecauseOfUnfixableError ue
    Right r -> Right r

type Conform ue fe w a = ConformT ue fe w Identity a

-- | Most flexible way to run a 'Conform'
runConformFlexible :: (fe -> Bool) -> Conform ue fe w a -> Either (HaltReason ue fe) (a, Notes fe w)
runConformFlexible predicate = runIdentity . runConformTFlexible predicate

-- | Don't fix any fixable errors, and don't allow any warnings either
--
-- This is standard-complient, but potentially more strict than necessary.
runConformStrict ::
  Conform ue fe w a ->
  Either (Either ue (Notes fe w)) a
runConformStrict = runIdentity . runConformTStrict

fixAll :: fe -> Bool
fixAll = const True

fixNone :: fe -> Bool
fixNone = const False

conformFromEither :: Monad m => Either ue a -> ConformT ue fe w m a
conformFromEither = \case
  Left ue -> unfixableError ue
  Right r -> pure r

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
