{-# LANGUAGE CPP #-}
-- |
-- Module      : EasyTest.Internal
-- Copyright   : (c) Joel Burget, 2018-2019
-- License     : MIT
-- Maintainer  : joelburget@gmail.com
-- Stability   : experimental
--
-- This module defines the core internals of easytest.
module EasyTest.Internal
  ( -- * Core
    crash
  , scope
  -- * Internal
  , Test(..)
  , unitProperty
  , testProperty
  -- * Hedgehog re-exports
  , Property
  , PropertyT
  , MonadTest
  , (===)
  , (/==)
  , Seed
  , Group
  ) where

#if MIN_VERSION_base(4,9,0)
import           GHC.Stack
#else
import           Data.CallStack
#endif
import           Data.List.Split (splitOn)
import           Hedgehog
  (Property, PropertyT, failure, footnote, property, withTests, withDiscards, MonadTest, (===), (/==), Seed, Group)


data Test
  = Internal ![(String, Test)]
  | Sequence ![Test]
  | Leaf !Property

unitProperty :: HasCallStack => PropertyT IO () -> Property
unitProperty = withTests 1 . withDiscards 1 . property

testProperty :: HasCallStack => Property -> Test
testProperty = Leaf

-- | Record a failure with a given message
crash :: HasCallStack => String -> Test
crash msg = Leaf $ unitProperty $ do { footnote msg; failure }

-- | Label a test. Can be nested. A "." is placed between nested
-- scopes, so @scope "foo" . scope "bar"@ is equivalent to @scope "foo.bar"@
scope :: String -> Test -> Test
scope msg tree =
  let newScopes = splitOn "." msg
  in foldr (\scope' test -> Internal [(scope', test)]) tree newScopes
