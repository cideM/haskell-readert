module Main where

import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
-- import Control.Exception.Safe
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks, local)
import qualified Data.Aeson as Aeson
import qualified Data.Monoid as Monoid
import Data.Text (Text)
import qualified Katip as K
import qualified Lib
-- import qualified System.IO
import Test.Tasty
import Test.Tasty.HUnit

data TestEnv = TestEnv
  { testEnvDb :: Text,
    testEnvLogNamespace :: K.Namespace,
    testEnvLogContext :: K.LogContexts,
    testEnvLogEnv :: K.LogEnv
  }

instance Lib.HasDb TestEnv where
  getDb = testEnvDb

instance K.Katip (Lib.App TestEnv) where
  getLogEnv = asks testEnvLogEnv
  localLogEnv f (Lib.App m) = Lib.App (local (\s -> s {testEnvLogEnv = f (testEnvLogEnv s)}) m)

instance K.KatipContext (Lib.App TestEnv) where
  getKatipContext = asks testEnvLogContext
  localKatipContext f (Lib.App m) = Lib.App (local (\s -> s {testEnvLogContext = f (testEnvLogContext s)}) m)
  getKatipNamespace = asks testEnvLogNamespace
  localKatipNamespace f (Lib.App m) = Lib.App (local (\s -> s {testEnvLogNamespace = f (testEnvLogNamespace s)}) m)

recordingEnv :: IO (K.LogEnv, TVar [K.Item Aeson.Object])
recordingEnv = do
  items <- STM.newTVarIO Monoid.mempty
  let scribe =
        K.Scribe
          { liPush = \i -> STM.atomically (STM.modifyTVar' items (<> [K.toObject <$> i])),
            scribeFinalizer = return (),
            scribePermitItem = K.permitItem K.DebugS
          }
  le1 <- K.initLogEnv "tests" "test"
  le2 <- K.registerScribe "recorder" scribe K.defaultScribeSettings le1
  return (le2, items)

withLogging :: (TestEnv -> IO ()) -> IO ()
withLogging f = do
  (le, _) <- recordingEnv
  K.runKatipContextT le () "base" $ do
    ctx <- K.getKatipContext
    ns <- K.getKatipNamespace
    let testEnv =
          TestEnv
            { testEnvDb = "db",
              testEnvLogNamespace = ns,
              testEnvLogContext = ctx,
              testEnvLogEnv = le
            }
    liftIO $ f testEnv

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = do
  testGroup
    "unit tests"
    [ testCase "someFunction" $ do
        withLogging $ \testEnv -> do
          flip Lib.unApp testEnv $ do
            K.katipNoLogging Lib.someFunction
    ]
