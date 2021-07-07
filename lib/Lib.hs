module Lib where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Reader as R
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Text (Text)
import qualified Katip as K
import qualified System.IO
import Prelude hiding (log)

data Env = Env
  { envDatabaseConnection :: Text,
    envWhatever :: Text,
    envLogNamespace :: K.Namespace,
    envLogContext :: K.LogContexts,
    envLogEnv :: K.LogEnv
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

class HasDb a where
  getDb :: a -> Text

instance HasDb Text where
  getDb = id

instance HasDb Env where
  getDb = envDatabaseConnection

instance K.Katip (App Env) where
  getLogEnv = asks envLogEnv
  localLogEnv f (App m) = App (local (\s -> s {envLogEnv = f (envLogEnv s)}) m)

instance K.KatipContext (App Env) where
  getKatipContext = asks envLogContext
  localKatipContext f (App m) = App (local (\s -> s {envLogContext = f (envLogContext s)}) m)
  getKatipNamespace = asks envLogNamespace
  localKatipNamespace f (App m) = App (local (\s -> s {envLogNamespace = f (envLogNamespace s)}) m)

someOtherFunction ::
  ( MonadIO m,
    K.KatipContext m,
    MonadReader env m,
    HasDb env
  ) =>
  m ()
someOtherFunction = do
  env <- R.ask
  K.logLocM K.InfoS . K.ls $ getDb env

someFunction ::
  ( MonadIO m,
    MonadReader env m,
    K.KatipContext m,
    HasDb env
  ) =>
  m ()
someFunction = do
  env <- R.ask
  K.logLocM K.InfoS . K.ls $ getDb env
  K.katipAddNamespace "new_namespace" $ do
    someOtherFunction
    return ()

main :: IO ()
main = do
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal System.IO.stdout (K.permitItem K.InfoS) K.V2

  let makeLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings =<< K.initLogEnv "MyApp" "production"

  bracket makeLogEnv K.closeScribes $ \le -> do
    let initialContext = ()
    let initialNamespace = "main"

    K.runKatipContextT le initialContext initialNamespace $ do
      K.logLocM K.InfoS "Hello Katip"

      K.katipAddNamespace "additional_namespace" $
        K.katipAddContext (K.sl "some_context" True) $ do
          ns <- K.getKatipNamespace
          ctx <- K.getKatipContext

          let env =
                Env
                  { envDatabaseConnection = "db",
                    envWhatever = "foo",
                    envLogNamespace = ns,
                    envLogContext = ctx,
                    envLogEnv = le
                  }

          liftIO $ unApp someFunction env

  return ()
