{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Server where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Control.Monad.Trans.Reader  as RT
import           Control.Monad.Logger
import           Database.Persist.Sql
import           Database.Persist.MySQL
import           Network.Wai.Handler.Warp    (Port)
import           Web.Scotty.Trans
import qualified Data.Text.Lazy              as LT

------------------------------------------------------------------------------------

-- Contextはデータベースのコネクションを保持している
data Context = Context { mysqlPool :: ConnectionPool }

-- プールを保持するモナドを定義
newtype ContextM a = ContextM { runContextM :: ReaderT Context IO a }
                     deriving ( Monad, Functor, Applicative, MonadReader Context, MonadIO, MonadBase IO )

-- IO a とか強制されているところにContextMを適用できるようになる
instance MonadBaseControl IO ContextM where
  type StM ContextM a = a
  liftBaseWith f = ContextM $ liftBaseWith $ \q -> f (q . runContextM)
  restoreM = ContextM . restoreM

instance MonadLogger ContextM where
  monadLoggerLog _ _ _ _ = return ()

type CScottyM = ScottyT LT.Text ContextM
type CActionM = ActionT LT.Text ContextM

cscotty :: Port -> Context -> CScottyM () -> IO ()
cscotty port ctx = scottyT port $ \(ContextM r) -> runReaderT r ctx

-- データベース接続情報
connectInfo :: ConnectInfo
connectInfo = ConnectInfo {
    connectHost     = "localhost",
    connectPort     = 3306,
    connectUser     = "hoge",
    connectPassword = "hoge",
    connectDatabase = "hogeapp",
    connectOptions  = [],
    connectPath     = "",
    connectSSL      = Nothing
  }

-- Contextを作成する
newContext :: IO Context
newContext = do
  pool <- runStdoutLoggingT $ createMySQLPool connectInfo 4
  return Context { mysqlPool = pool }

type DatabaseM = SqlPersistT ContextM

runDB :: ConnectionPool -> DatabaseM a -> ContextM a
runDB pool sql = runSqlPool sql pool

context :: (MonadTrans t) => t ContextM Context
context = lift ask

execDB :: DatabaseM a -> CActionM a
execDB sql = do
  pool <- mysqlPool <$> context
  lift $ runDB pool sql
