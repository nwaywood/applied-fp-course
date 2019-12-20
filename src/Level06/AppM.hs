{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Level06.AppM
  ( AppM(..)
  , App
  , liftEither
  , runApp
  ) where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Bifunctor         (Bifunctor (..))

import           Level06.Types          (Error)

-- We're going to upgrade the capability of our AppM by generalising the type of the errors that
-- it handles. This means that we'll be able to reuse our 'AppM' in more places that maybe have an
-- overabundance of 'IO (Either e a)' types.
--
-- Our new 'AppM'' will also use the record syntax to define our 'runAppM' function. This is a more
-- common definition of this kind of newtype.
--
newtype AppM e a = AppM
  { runAppM :: IO (Either e a)
  }

-- Predominantly our application has only one error type: 'Error'. It would be tedious to have to
-- declare that on every signature. We're able to use a type _alias_ to avoid this problem. We can
-- define this type alias to make the error type variable concrete as 'Error'.
--
type App = AppM Error

-- We need to refactor the 'runAppM' function as now the name conflicts, and it needs to suit the
-- specialised 'App' type. The definition is even simpler than before. If someone near you is up to
-- the same section, try to explain to each other why this works.
--
runApp :: App a -> IO (Either Error a)
runApp = runAppM

-- You may copy your previously completed AppM instances here and then refactor them to suit the
-- more generalised type of AppM.

-- | -----------------------------------------------------------------------------------------------
-- | Copy from previous level and refactor, or reimplement to practice. The choice is yours.
-- | -----------------------------------------------------------------------------------------------

instance Functor (AppM e) where
  fmap :: (a -> b) -> AppM e a -> AppM e b
  fmap f (AppM v) = AppM $ second f <$> v

instance Applicative (AppM e) where
  pure :: a -> AppM e a
  pure x = AppM (pure (Right x))

  (<*>) :: AppM e (a -> b) -> AppM e a -> AppM e b
  -- (<*>) appF appA = appF >>= (\eEF -> eEF <$> appA)
  (<*>) appF appA = appF >>= (<$> appA)

instance Monad (AppM e) where
  (>>=) :: AppM e a -> (a -> AppM e b) -> AppM e b
  (>>=) appMa a2appMb = AppM $ runAppM appMa
                         >>= either (pure . Left) (runAppM . a2appMb)
  -- (>>=) appMa a2appMb = appMa >>= (\eEa -> )

instance MonadIO (AppM e) where
  liftIO :: IO a -> AppM e a
  liftIO ioA = AppM $ Right <$> ioA

instance MonadError e (AppM e) where
  throwError :: e -> AppM e a
  throwError err = AppM $ pure (Left err)

  catchError :: AppM e a -> (e -> AppM e a) -> AppM e a
  catchError appM f = AppM $ runAppM appM >>= (\eErrA -> case eErrA of
      Left err -> runAppM $ f err
      Right _ -> runAppM appM)

-- The 'Bifunctor' instance for 'Either' has proved useful several times
-- already. Now that our 'AppM' exposes both type variables that are used in our
-- 'Either', we can define a Bifunctor instance and reap similar benefits.
instance Bifunctor AppM where
  bimap :: (e -> d) -> (a -> b) -> AppM e a -> AppM d b
  bimap f1 f2 appM = AppM $ bimap f1 f2 <$> runAppM appM

-- This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither :: Either e a -> AppM e a
liftEither e = AppM $ pure e
