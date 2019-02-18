{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module EasyTest.Porcelain
  ( -- * Tests
    -- Test
    expect
  , expectJust
  , expectRight
  , expectRightNoShow
  , expectLeft
  , expectLeftNoShow
  , expectEq
  , expectNeq
  , tests
  , runOnly
  , rerunOnly
  , run
  , rerun
  , scope
  , note'
  , ok
  , skip
  , crash
  , note
  , testProperty
  ) where

import           Control.Monad     (void)
import           Data.List         (intercalate)
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup
#endif
import           Data.CallStack
import           Data.List.Split   (splitOn)
import           Data.String       (fromString)

import           Hedgehog          hiding (Test)

import           EasyTest.Hedgehog
import           EasyTest.Internal


expect :: HasCallStack => Bool -> Test
expect False = crash "unexpected"
expect True  = ok

expectJust :: HasCallStack => Maybe a -> Test
expectJust Nothing  = crash "expected Just, got Nothing"
expectJust (Just _) = ok

expectRight :: (Show e, HasCallStack) => Either e a -> Test
expectRight (Left e)  = crash $ "expected Right, got (Left " ++ show e ++ ")"
expectRight (Right _) = ok

expectRightNoShow :: (HasCallStack) => Either e a -> Test
expectRightNoShow (Left _)  = crash $ "expected Right, got Left"
expectRightNoShow (Right _) = ok

expectLeft :: (Show a, HasCallStack) => Either e a -> Test
expectLeft (Right a) = crash $ "expected Left, got (Right " ++ show a ++ ")"
expectLeft (Left _)  = ok

expectLeftNoShow :: HasCallStack => Either e a -> Test
expectLeftNoShow (Right _) = crash $ "expected Left, got Right"
expectLeftNoShow (Left _)  = ok

expectEq :: (Eq a, Show a, HasCallStack) => a -> a -> Test
expectEq a b = testProperty $ property' $ a === b

expectNeq :: (Eq a, Show a, HasCallStack) => a -> a -> Test
expectNeq a b = testProperty $ property' $ a === b

-- | Run a list of tests
tests :: [Test] -> Test
tests = Internal . zip (show <$> [(1 :: Int)..])

mkGroup :: GroupName -> [([String], Property)] -> Group
mkGroup name props = Group name $ flip fmap props $ \(path, prop) ->
  case path of
    [] -> ("(unnamed)", prop)
    _  -> (fromString (intercalate "." path), prop)

runTree :: Test -> [([String], Property)]
runTree (Leaf prop)      = [([], prop)]
runTree (Internal trees) = concatMap f trees
  where f (name, tree) = addName name $ runTree tree

runTreeOnly :: [String] -> Test -> [([String], Property)]
runTreeOnly [] tree = runTree tree
runTreeOnly (_:_) tree@Leaf{} = skipTree tree
runTreeOnly (scope':scopes) (Internal nodes) = concatMap f nodes
  where f (scope'', tree) =
          if scope'' == scope'
          then runTreeOnly scopes tree
          else skipTree tree

addName :: String -> [([String], Property)] -> [([String], Property)]
addName name = fmap $ \(names, prop) -> (name:names, prop)

skipTree :: Test -> [([String], Property)]
skipTree (Leaf _prop) = [([], property' discard)]
skipTree (Internal trees) = concatMap f trees
  where f (name, tree) = addName name $ skipTree tree

-- | Run all tests whose scope starts with the given prefix
runOnly :: String -> Test -> IO ()
runOnly prefix t = do
  let props = runTreeOnly (splitOn "." prefix) t
      group = mkGroup (fromString $ "runOnly " ++ show prefix) props

  void $ checkSequential group

-- | Rerun all tests with the given seed and whose scope starts with the given
-- prefix
rerunOnly :: Seed -> String -> Test -> IO Bool
rerunOnly seed prefix t = do
  let props = runTreeOnly (splitOn "." prefix) t
      name = fromString $ "rerunOnly " ++ show prefix
  recheck' seed $ mkGroup name props

-- | Run all tests
run :: Test -> IO ()
run t =
  let props = runTree t
      group = mkGroup "run" props
  in void $ checkSequential group

-- | Rerun all tests with the given seed
rerun :: Seed -> Test -> IO Bool
rerun seed t = rerunOnly seed "" t

-- | Log a string
note :: MonadTest m => String -> m ()
note = footnote

-- | Log a showable value
note' :: (MonadTest m, Show s) => s -> m ()
note' = footnoteShow

-- | Record a successful test at the current scope
ok :: Test
ok = testProperty $ property' success

-- | Explicitly skip this test
skip :: Test
skip = testProperty $ property' discard
