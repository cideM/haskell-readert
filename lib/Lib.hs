module Lib where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Reader as R
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Text (Text)
import Prelude hiding (log)

data Env = Env
  { envDatabaseConnection :: Text,
    envLog :: (Text -> IO ()),
    envWhatever :: Text
  }

newtype App env a = App {unApp :: env -> IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader env,
      MonadIO,
      MonadThrow
    )
    via ReaderT env IO

class HasLog a where
  getLog :: a -> (Text -> IO ())

instance HasLog (Text -> IO ()) where
  getLog = id

instance HasLog Env where
  getLog = envLog

class HasDb a where
  getDb :: a -> Text

instance HasDb Text where
  getDb = id

instance HasDb Env where
  getDb = envDatabaseConnection

someOtherFunction :: (MonadIO m, MonadReader env m, HasLog env) => m ()
someOtherFunction = do
  env <- R.ask
  let log = liftIO . getLog env
  log "Hello from someOtherFunction"

someFunction :: (MonadIO m, MonadReader env m, HasLog env, HasDb env) => m ()
someFunction = do
  env <- R.ask
  let log = liftIO . getLog env
  log "foo"
  log $ getDb env
  someOtherFunction
  return ()

main :: IO ()
main = do
  let env =
        Env
          { envDatabaseConnection = "db",
            envLog = (\s -> print s),
            envWhatever = "foo"
          }
  unApp someFunction env
  return ()
