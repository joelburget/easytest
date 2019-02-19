{-# LANGUAGE CPP #-}

module EasyTest.Internal
  ( -- * Core
    crash
  , scope
  -- * Internal
  , Test(..)
  , unitProperty
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
import           Data.List.Split (splitOn)
import           Hedgehog
  (Property, PropertyT, failure, footnote, property, withTests)


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
data Test
  = Internal ![(String, Test)]
  | Leaf !Property

unitProperty :: HasCallStack => PropertyT IO () -> Property
unitProperty = withTests 1 . property

testProperty :: HasCallStack => Property -> Test
testProperty = Leaf

crash :: HasCallStack => String -> Test
crash msg = Leaf $ unitProperty $ do { footnote msg; failure }

-- | Label a test. Can be nested. A "." is placed between nested
-- scopes, so @scope "foo" . scope "bar"@ is equivalent to @scope "foo.bar"@
scope :: String -> Test -> Test
scope msg tree =
  let newScopes = splitOn "." msg
  in foldr (\scope' test -> Internal [(scope', test)]) tree newScopes
