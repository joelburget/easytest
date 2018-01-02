{-# Language BangPatterns #-}
{-# Language MultiParamTypeClasses #-}
{-# Language NamedFieldPuns #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

module EasyTest.Core where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List (isPrefixOf)
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import qualified System.Random as Random

show' :: Show a => a -> Text
show' = T.pack . show

data Status = Failed | Passed !Int | Skipped

combineStatus :: Status -> Status -> Status
combineStatus Skipped s = s
combineStatus s Skipped = s
combineStatus Failed _ = Failed
combineStatus _ Failed = Failed
combineStatus (Passed n) (Passed m) = Passed (n + m)

instance Semigroup Status where
  (<>) = combineStatus

instance Monoid Status where
  mempty  = Passed 0
  mappend = combineStatus

data Env =
  Env { rng :: TVar Random.StdGen
      , messages :: [Text]
      , results :: TBQueue (Maybe (TMVar ([Text], Status)))
      , note_ :: Text -> IO ()
      , allow :: [Text] }

newtype Test a = Test (ReaderT Env IO (Maybe a))

-- | Record a failure at the current scope
crash :: HasCallStack => Text -> Test a
crash msg = do
  let trace = callStack
      msg' = msg <> " " <> T.pack (prettyCallStack trace)
  Test (Just <$> putResult Failed)
  noteScoped ("FAILURE " <> msg')
  Test (pure Nothing)

putResult :: Status -> ReaderT Env IO ()
putResult passed = do
  msgs <- asks messages
  allow' <- asks allow
  r <- liftIO . atomically $ newTMVar
    (msgs, if allow' `isPrefixOf` msgs then passed else Skipped)
  q <- asks results
  lift . atomically $ writeTBQueue q (Just r)

-- | Prepend the current scope to a logging message
noteScoped :: Text -> Test ()
noteScoped msg = do
  s <- currentScope
  note (T.intercalate "." s <> (if null s then "" else " ") <> msg)

-- | Log a message
note :: Text -> Test ()
note msg = do
  note_ <- asks note_
  liftIO $ note_ msg
  pure ()

-- | The current scope
currentScope :: Test [Text]
currentScope = asks messages

atomicLogger :: IO (Text -> IO ())
atomicLogger = do
  lock <- newMVar ()
  pure $ \msg ->
    -- force msg before acquiring lock
    let dummy = T.foldl' (\_ ch -> ch == 'a') True msg
    in dummy `seq` bracket (takeMVar lock) (\_ -> putMVar lock ()) (\_ -> T.putStrLn msg)

-- | Catch all exceptions that could occur in the given `Test`
wrap :: Test a -> Test a
wrap (Test t) = Test $ do
  env <- ask
  lift $ runWrap env t

runWrap :: Env -> ReaderT Env IO (Maybe a) -> IO (Maybe a)
runWrap env t = do
  result <- try $ runReaderT t env
  case result of
    Left e -> do
      note_ env (T.intercalate "." (messages env) <> " EXCEPTION: " <> show' (e :: SomeException))
      runReaderT (putResult Failed) env
      pure Nothing
    Right a -> pure a

-- | A test with a setup and teardown
using :: IO r -> (r -> IO ()) -> (r -> Test a) -> Test a
using r cleanup use = Test $ do
  r' <- liftIO r
  env <- ask
  let Test t = use r'
  a <- liftIO (runWrap env t)
  liftIO (cleanup r')
  pure a

-- * allow' `isPrefixOf` messages': we're messaging within the allowed range
-- * messages' `isPrefixOf` allow': we're still building a prefix of the
--   allowed range but could go deeper
actionAllowed :: Env -> Bool
actionAllowed Env{messages, allow}
  = allow `isPrefixOf` messages || messages `isPrefixOf` allow

instance MonadReader Env Test where
  ask = Test $ do
    allowed <- asks actionAllowed
    if allowed
      then Just <$> ask
      else pure Nothing
  local f (Test t) = Test (local f t)
  reader f = Test (Just <$> reader f)

instance Monad Test where
  fail = crash . T.pack
  return a = Test $ do
    allowed <- asks actionAllowed
    pure $ if allowed
      then Just a
      else Nothing
  Test a >>= f = Test $ do
    a' <- a
    case a' of
      Nothing -> pure Nothing
      Just a'' -> let Test t = f a'' in t

instance Functor Test where
  fmap = liftM

instance Applicative Test where
  pure = return
  (<*>) = ap

instance MonadIO Test where
  liftIO action = do
    allowed <- asks actionAllowed
    if allowed
      then wrap $ Test (Just <$> liftIO action)
      else Test (pure Nothing)

instance Alternative Test where
  empty = Test (pure Nothing)
  Test t1 <|> Test t2 = Test $ do
    env <- ask
    (rng1, rng2) <- liftIO . atomically $ do
      currentRng <- readTVar (rng env)
      let (rng1, rng2) = Random.split currentRng
      (,) <$> newTVar rng1 <*> newTVar rng2
    lift $ do
      _ <- runWrap (env { rng = rng1 }) t1
      runWrap (env { rng = rng2 }) t2

instance MonadPlus Test where
  mzero = empty
  mplus = (<|>)
