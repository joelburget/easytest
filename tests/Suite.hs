{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Monad.IO.Class (liftIO)
import           Hedgehog               (forAll, (===))
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           System.Directory       (removeFile)
import           System.IO              (hClose, hPutStrLn)
import           System.Posix.Temp

import           EasyTest
import           EasyTest.Prism

suite1 :: Test
suite1 = tests
  [ scope "a" $ unitTest success
  , scope "b.c" $ unitTest success
  , scope "b" $ unitTest success
  , scope "b" . scope "c" . scope "d" $ unitTest success
  ]

reverseTest :: Test
reverseTest = scope "list reversal" $ property $ do
  list <- forAll $ Gen.list @_ @Int (Range.linear 0 100)
    (Gen.element [0..100])
  reverse (reverse list) === list

main :: IO ()
main = do
  _ <- run $ example $ 1 === (1 :: Int)
  _ <- run suite1
  _ <- runOnly "a" suite1
  _ <- runOnly "b" suite1
  _ <- runOnly "b" $ tests [suite1, scope "xyz" (unitTest (crash "never run"))]
  _ <- runOnly "b.c" $ tests [suite1, scope "b" (unitTest (crash "never run"))]
  _ <- runOnly "x.go" $ tests
    [ scope "x.go to" $ unitTest $ crash "never run"
    , scope "x.go" $ unitTest success
    ]
  _ <- runOnly "x.go to" $ tests
    [ scope "x.go to" $ unitTest success
    , scope "x.go" $ unitTest $ crash "never run"
    ]
  _ <- run reverseTest

  _ <- run $ tests
    [ example $ matches      _Left  (Left 1   :: Either Int ())
    , example $ doesn'tMatch _Right (Left 1   :: Either Int ())
    , example $ matches      _Right (Right () :: Either Int ())
    , example $ doesn'tMatch _Left  (Right () :: Either Int ())

    -- Uncomment for an example diff:
    -- , expectEq          "foo\nbar\nbaz" ("foo\nquux\nbaz" :: String)
    ]

  _ <- run $ scope "bracket" $ example $ bracket
    (mkstemp "temp")
    (\(filepath, handle) -> hClose handle >> removeFile filepath)
    (\(_filepath, handle) -> do
      liftIO $ hPutStrLn handle "this temporary file will be cleaned up"
      success)

  pure ()
