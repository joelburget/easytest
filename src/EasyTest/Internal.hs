{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving            #-}

{-# LANGUAGE OverloadedStrings            #-}

module EasyTest.Internal
  ( -- * Core
    crash
  -- , note
  , scope
  -- * Internal
  , Env(..)
  , Test(..)
  , property'
  , testProperty
  ) where

import           Control.Monad.Reader
import           Control.Monad.Trans.Writer.Strict
import           Control.Monad.Writer.Class (MonadWriter)
import           Data.List              (intercalate)
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup
#endif
#if MIN_VERSION_base(4,9,0)
import           GHC.Stack
#else
import           Data.CallStack
#endif
import Data.List.Split (splitOn)

import Hedgehog hiding (Test)


data Env = Env
  { envScope :: ![String]
  }

-- | Tests are values of type @Test a@, and 'Test' forms a monad with access to:
--
--     * I/O (via 'liftIO')
--
--     * failure (via 'crash', which yields a stack trace, or 'fail', which does not)
--
--     * logging (via 'EasyTest.note', 'EasyTest.noteScoped', or 'EasyTest.note'')
--
--     * hierarchically-named subcomputations (under 'EasyTest.scope') which can be switched on and off via 'EasyTest.runOnly'
--
--     * parallelism (via 'EasyTest.fork')
--
--     * conjunction of tests via 'MonadPlus' (the '<|>' operation runs both tests, even if the first test fails, and the tests function used above is just 'msum').
--
-- Using any or all of these capabilities, you assemble 'Test' values into a "test suite" (just another 'Test' value) using ordinary Haskell code, not framework magic. Notice that to generate a list of random values, we just 'replicateM' and 'forM' as usual.
newtype Test a = Test
  { unTest :: ReaderT Env (Writer [([String], Property)]) a }
  deriving (Functor, Applicative, Monad, MonadReader Env,
    MonadWriter [([String], Property)])

property' :: HasCallStack => PropertyT IO () -> Property
property' = withTests 1 . property

getPropertyName :: Test [String]
getPropertyName = Test $ ReaderT $ \(Env scopes) -> pure scopes

testProperty :: HasCallStack => Property -> Test ()
testProperty prop = do
  name <- getPropertyName
  Test $ ReaderT $ \_ -> tell [ (name, prop) ]

crash :: HasCallStack => String -> Test ()
crash msg = do
  name <- getPropertyName
  Test $ ReaderT $ \_ -> tell
    [ (name, property' $ do { footnote msg; failure }) ]

-- | Label a test. Can be nested. A "." is placed between nested
-- scopes, so @scope "foo" . scope "bar"@ is equivalent to @scope "foo.bar"@
scope :: String -> Test a -> Test a
scope msg = local $ \(Env scopes) -> Env (scopes <> splitOn "." msg)

-- | Prepend the current scope to a logging message
noteScoped :: String -> Test ()
noteScoped msg = do
  s <- currentScope
  note (intercalate "." s <> (if null s then "" else " ") <> msg)

-- | Log a message
note :: String -> Test ()
note msg = do
  -- TODO: figure out how to do this
  -- annotate msg
  pure ()

-- | The current scope
currentScope :: Test [String]
currentScope = asks envScope

---- | Catch all exceptions that could occur in the given `Test`
--wrap :: Test a -> Test a
--wrap (Test t) = Test $ MaybeT $ do
--  env <- ask
--  lift $ runWrap env t

--runWrap :: Env -> MaybeT (ReaderT Env IO) a -> IO (Maybe a)
--runWrap env t = do
--  result <- try $ runReaderT (runMaybeT t) env
--  case result of
--    Left e -> do
--      envNote env $
--           T.intercalate "." (envScopes env)
--        <> " EXCEPTION: "
--        <> T.pack (show (e :: SomeException))
--      runReaderT (putResult Failed) env
--      pure Nothing
--    Right a -> pure a

-- instance IsString (Test a -> Test a) where
--   fromString = scope
