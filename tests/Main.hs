module Main where

import Data.Text (Text)
import qualified Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

data TestEnv = TestEnv
  { testEnvDb :: Text,
    testEnvLog :: (Text -> IO ())
  }

instance Lib.HasLog TestEnv where
  getLog = testEnvLog

instance Lib.HasDb TestEnv where
  getDb = testEnvDb

tests :: TestTree
tests =
  testGroup
    "unit tests"
    [ testCase "someFunction" $ do
        let testEnv = TestEnv {testEnvDb = "db", testEnvLog = (\s -> print s)}
        Lib.unApp Lib.someFunction testEnv
    ]
