{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import           Control.Monad ()
import           Control.Monad.Logger (runStderrLoggingT)
import           Control.Monad.Trans.Resource ()
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Database.Persist ()
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Yesod
import           Data.Aeson ()
import           Data.Aeson.Types ()
import           Data.Time.Clock

data App = App ConnectionPool

share [mkPersist sqlSettings] [persistLowerCase|
Owner json
    name  String
Cow json
    earNum Int
    name   String
    birth  UTCTime
    sex    String
    ownerId OwnerId
    t1     Int
    t2     Int
    t3     Int
    t4     Int
    t5     Int
    t6     Int
    t7     Int
    t8     Int
    t9     Int
    t10    Int
    t11    Int
    t12    Int
    t13    Int
    t14    Int
    t15    Int
    t16    Int
    t17    Int
    t18    Int
    t19    Int
    t20    Int
    t21    Int
    t22    Int
    t23    Int
    t24    Int
    t25    Int
    t26    Int
    t27    Int
    t28    Int
    t29    Int
    t30    Int
    t31    Int
    t32    Int
    t33    Int
    t34    Int
    t35    Int
    t36    Int
    t37    Int
    t38    Int
    t39    Int
    t40    Int
    t41    Int
    t42    Int
    t43    Int
    t44    Int
    t45    Int
    t46    Int
    t47    Int
    t48    Int
    t49    Int
    t50    Int
|]

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

instance ToJSON (E.Value UTCTime) where
  toJSON (E.Value t) = toJSON t

instance ToJSON (E.Value String) where
  toJSON (E.Value t) = toJSON t

instance ToJSON (E.Value Int) where
  toJSON (E.Value t) = toJSON t

getHomeR :: Handler Value
getHomeR = do
  cows <- runDB
           $ E.select
           $ E.from $ \(cow `E.InnerJoin` owner) -> do
                E.on $ cow ^. CowOwnerId E.==. owner ^. OwnerId
                E.where_ $ cow ^. CowOwnerId E.==. 5
                return
                    (cow ^. CowEarNum, cow ^. CowName, cow ^. CowBirth,
                     cow ^. CowSex,  owner ^. OwnerName)
             -- owners <-runDB $ selectList [OwnerName ==. "Owner1"] []
	     -- let Entity ownerId _ = head owners 
             -- cows <- runDB $ selectList [CowOwnerId ==. ownerId][]
  returnJson cows

openConnectionCount :: Int
openConnectionCount = 10

connStr = "host=localhost dbname=cow_table user=hirofumi password='hello'"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr openConnectionCount $ \pool -> liftIO $ do
    warp 3000 $ App pool