{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Profunctor        (dimap, right')
import           Hedgehog               (forAll, (===))
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           System.Directory       (removeFile)
import           System.IO              (hClose, hPutStrLn)
import           System.Posix.Temp

import           EasyTest
import           EasyTest.Internal      (Test (..), Prism)

-- Normally you'd import 'prism', '_Left', and '_Right' from lens. We define
-- them here because they're simple and we can avoid the dependency.

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left . Right)
{-# INLINE _Left #-}

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (Left . Left) Right
{-# INLINE _Right #-}

suite1 :: Test
suite1 = tests
  [ scope "a" ok
  , scope "b.c" ok
  , scope "b" ok
  , scope "b" . scope "c" . scope "d" $ ok
  ]

reverseTest :: Test
reverseTest = scope "list reversal" $ property $ do
  list <- forAll $ Gen.list @_ @Int (Range.linear 0 100)
    (Gen.element [0..100])
  reverse (reverse list) === list

main :: IO ()
main = do
  _ <- run $ expect $ 1 === (1 :: Int)
  _ <- run suite1
  _ <- runOnly "a" suite1
  _ <- runOnly "b" suite1
  _ <- runOnly "b" $ tests [suite1, scope "xyz" (crash "never run")]
  _ <- runOnly "b.c" $ tests [suite1, scope "b" (crash "never run")]
  _ <- runOnly "x.go" $ tests
    [ scope "x.go to" (crash "never run")
    , scope "x.go" ok
    ]
  _ <- runOnly "x.go to" $ tests
    [ scope "x.go to" ok
    , scope "x.go" (crash "never run")
    ]
  _ <- run reverseTest

  _ <- run $ tests
    [ expect $ match _Left  (Left 1   :: Either Int ())
    , expect $ match _Right (Right () :: Either Int ())

    -- Uncomment for an example diff:
    -- , expectEq          "foo\nbar\nbaz" ("foo\nquux\nbaz" :: String)
    ]

  _ <- run $ scope "bracket" $ expect $ bracket
    (mkstemp "temp")
    (\(filepath, handle) -> hClose handle >> removeFile filepath)
    (\(_filepath, handle) -> do
      liftIO $ hPutStrLn handle "we can do IO"
      success)

  pure ()
