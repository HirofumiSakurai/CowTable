{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import           Control.Monad
import           Control.Monad.Logger (runStderrLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Yesod
import Data.Aeson
import Data.Aeson.Types

data App = App ConnectionPool

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Cow json
    earNum Int
    name   String
    birth  String
    sex    String
    ownerId Int
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

getHomeR :: Handler Value
getHomeR = do
             cows <-runDB (selectList [CowOwnerId ==. 1] []) :: Handler [Entity Cow]
             returnJson cows

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        forM [1..40000] $ \i -> do
            insert $ Cow i "hana" "" "female" (mod i 10)
    warp 3000 $ App pool