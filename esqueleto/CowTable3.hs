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
import           Database.Esqueleto
import qualified Database.Esqueleto as E
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

instance ToJSON (E.Value String) where
  toJSON (E.Value t) = toJSON t

instance ToJSON (E.Value Int) where
  toJSON (E.Value t) = toJSON t

getHomeR :: Handler Yesod.Value
getHomeR = do
  cows <- runDB
           $ select
           $ from $ \(cow `InnerJoin` owner) -> do
                on $ cow ^. CowOwnerId E.==. owner ^. OwnerId
                --where_ $ cow ^. CowEarNum E.==. val 5
                where_ $ cow ^. CowOwnerId E.==. valkey 5
                -- return
                --     (cow ^. CowEarNum, cow ^. CowName, 
                --      cow ^. CowSex,  owner ^. OwnerName)
                return $ (
                  cow ^. CowEarNum, cow ^. CowName, 
                  cow ^. CowSex, owner ^. OwnerName,
                  (cow ^. CowT1, cow ^. CowT2, cow ^. CowT3, cow ^. CowT4,
                   cow ^. CowT5, cow ^. CowT6, cow ^. CowT7, cow ^. CowT8,
                   cow ^. CowT9, cow ^. CowT10),
                  (cow ^. CowT11, cow ^. CowT12, cow ^. CowT13, cow ^. CowT14,
                   cow ^. CowT15, cow ^. CowT16, cow ^. CowT17, cow ^. CowT18,
                   cow ^. CowT19, cow ^. CowT20),
                  (cow ^. CowT21, cow ^. CowT22, cow ^. CowT23, cow ^. CowT24,
                   cow ^. CowT25, cow ^. CowT26, cow ^. CowT27, cow ^. CowT28,
                   cow ^. CowT29, cow ^. CowT30),
                  (cow ^. CowT31, cow ^. CowT32, cow ^. CowT33, cow ^. CowT34,
                   cow ^. CowT35, cow ^. CowT36, cow ^. CowT37, cow ^. CowT38,
                   cow ^. CowT39, cow ^. CowT40),
                  (cow ^. CowT41, cow ^. CowT42, cow ^. CowT43, cow ^. CowT44,
                   cow ^. CowT45, cow ^. CowT46, cow ^. CowT47, cow ^. CowT48,
                   cow ^. CowT49, cow ^. CowT50))
  let cows' = map toBuilder cows
  --liftIO $ print cows
  returnJson cows'
    where
      toBuilder (E.Value earNum, E.Value name, 
          E.Value sex, E.Value ownerName ,
          (E.Value t1, E.Value  t2, E.Value  t3, E.Value  t4, E.Value  t5,
          E.Value t6, E.Value  t7, E.Value  t8, E.Value  t9, E.Value t10),
          (E.Value t11, E.Value t12, E.Value t13, E.Value t14, E.Value t15,
          E.Value t16, E.Value t17, E.Value t18, E.Value t19, E.Value t20),
          (E.Value t21, E.Value t22, E.Value t23, E.Value t24, E.Value t25,
          E.Value t26, E.Value t27, E.Value t28, E.Value t29, E.Value t30),
          (E.Value t31, E.Value t32, E.Value t33, E.Value t34, E.Value t35,
          E.Value t36, E.Value t37, E.Value t38, E.Value t39, E.Value t40),
          (E.Value t41, E.Value t42, E.Value t43, E.Value t44, E.Value t45,
          E.Value t46, E.Value t47, E.Value t48, E.Value t49, E.Value t50))
          = object [
              "earNum".= earNum, "name" .= name, 
              "sex" .= sex,  "ownerName" .= ownerName,
              "t1"  .=  t1, "t2" .=  t2, "t3" .=  t3, "t4" .=  t4, "t5" .=  t5,
              "t6"  .=  t6, "t7" .=  t7, "t8" .=  t8, "t9" .=  t9,"t10" .= t10,
              "t11" .= t11,"t12" .= t12,"t13" .= t13,"t14" .= t14,"t15" .= t15,
              "t16" .= t16,"t17" .= t17,"t18" .= t18,"t19" .= t19,"t20" .= t20,
              "t21" .= t21,"t22" .= t22,"t23" .= t23,"t24" .= t24,"t25" .= t25,
              "t26" .= t26,"t27" .= t27,"t28" .= t28,"t29" .= t29,"t30" .= t30,
              "t31" .= t31,"t32" .= t32,"t33" .= t33,"t34" .= t34,"t35" .= t35,
              "t36" .= t36,"t37" .= t37,"t38" .= t38,"t39" .= t39,"t40" .= t40,
              "t41" .= t41,"t42" .= t42,"t43" .= t43,"t44" .= t44,"t45" .= t45,
              "t46" .= t46,"t47" .= t47,"t48" .= t48,"t49" .= t49,"t50" .= t50]

openConnectionCount :: Int
openConnectionCount = 10

connStr = "host=localhost dbname=cow_table user=hirofumi password='hello'"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr openConnectionCount $ \pool -> liftIO $ do
    warp 3000 $ App pool
