{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EasyTest.Internal
  ( -- * Core
    crash
  -- , note
  , scope
  -- * Internal
  -- , Env(..)
  -- , Test(..)
  , Tree(..)
  , property'
  , testProperty
  ) where

#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup
#endif
#if MIN_VERSION_base(4,9,0)
import           GHC.Stack
#else
import           Data.CallStack
#endif
import Data.List.Split (splitOn)

import Hedgehog hiding (Test, test)


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
data Tree
  = Internal ![(String, Tree)]
  | Leaf !Property

property' :: HasCallStack => PropertyT IO () -> Property
property' = withTests 1 . property

-- getPropertyName :: Test [String]
-- getPropertyName = Test $ ReaderT $ \(Env scopes) -> pure scopes

testProperty :: HasCallStack => Property -> Tree
testProperty = Leaf
-- do
--   name <- getPropertyName
--   pure $ Leaf prop
  -- Test $ ReaderT $ \_ -> tell $ Seq.singleton (name, prop)

crash :: HasCallStack => String -> Tree
crash msg = Leaf $ property' $ do { footnote msg; failure }
-- do
--   name <- getPropertyName
--   Test $ ReaderT $ \_ -> tell $ Seq.singleton
--     (name, property' $ do { footnote msg; failure })

-- | Label a test. Can be nested. A "." is placed between nested
-- scopes, so @scope "foo" . scope "bar"@ is equivalent to @scope "foo.bar"@
scope :: String -> Tree -> Tree
scope msg tree =
  let newScopes = splitOn "." msg
  in foldr (\scope' test -> Internal [(scope', test)]) tree newScopes
  -- local $ \(Env scopes) -> Env (scopes <> splitOn "." msg)

-- -- | Prepend the current scope to a logging message
-- noteScoped :: String -> Test ()
-- noteScoped msg = do
--   s <- currentScope
--   note (intercalate "." s <> (if null s then "" else " ") <> msg)

-- -- | Log a message
-- note :: String -> Tree -> Tree
-- note msg test = do
--   modify _

  -- Test $ ReaderT $ \_ -> tell [ undefined ]
-- do
--   -- TODO: figure out how to do this
--   annotate msg
  -- pure ()

-- -- | The current scope
-- currentScope :: Test [String]
-- currentScope = asks envScope
