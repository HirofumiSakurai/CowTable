{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (forM_)
import           Control.Monad.Logger (runStderrLoggingT)
import           Control.Monad.Trans.Resource ()
import           Data.Text (Text,pack)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Aeson ()
import           Data.Aeson.Types ()
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Data.Time.Clock
import           Yesod

data App = App {connection :: Connection,
                tableColumns :: [(String, [String])]
               }

mkYesod "App" [parseRoutes|
/ HomeR GET
/cowJson CowJsonR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

data QueryFormData = QueryFormData {
  ear_num :: Bool,name :: Bool,birth :: Bool,sex :: Bool,owner_id :: Bool,
  t1 :: Bool,t2 :: Bool,t3 :: Bool,t4 :: Bool,t5 :: Bool,t6 :: Bool,t7 :: Bool,
  t8 :: Bool,t9 :: Bool,t10 :: Bool,t11 :: Bool,t12 :: Bool,t13 :: Bool,
  t14 :: Bool,t15 :: Bool,t16 :: Bool,t17 :: Bool,t18 :: Bool,t19 :: Bool,
  t20 :: Bool,t21 :: Bool,t22 :: Bool,t23 :: Bool,t24 :: Bool,t25 :: Bool,
  t26 :: Bool,t27 :: Bool,t28 :: Bool,t29 :: Bool,t30 :: Bool,t31 :: Bool,
  t32 :: Bool,t33 :: Bool,t34 :: Bool,t35 :: Bool,t36 :: Bool,t37 :: Bool,
  t38 :: Bool,t39 :: Bool,t40 :: Bool,t41 :: Bool,t42 :: Bool,t43 :: Bool,
  t44 :: Bool,t45 :: Bool,t46 :: Bool,t47 :: Bool,t48 :: Bool,t49 :: Bool,
  t50 :: Bool, ownerName :: Bool, query :: Maybe Text  }

queryForm :: Html -> MForm Handler (FormResult QueryFormData, Widget)
queryForm = renderDivs $ QueryFormData
            <$> areq checkBoxField "ear_num" (Just True)
            <*> areq checkBoxField "name" (Just True)
            <*> areq checkBoxField "birth" (Just True)
            <*> areq checkBoxField "sex" (Just True)
            <*> areq checkBoxField "owner_id" (Just True)
            <*> areq checkBoxField "t1" (Just True)
            <*> areq checkBoxField "t2" (Just True)
            <*> areq checkBoxField "t3" (Just True)
            <*> areq checkBoxField "t4" (Just True)
            <*> areq checkBoxField "t5" (Just True)
            <*> areq checkBoxField "t6" (Just True)
            <*> areq checkBoxField "t7" (Just True)
            <*> areq checkBoxField "t8" (Just True)
            <*> areq checkBoxField "t9" (Just True)
            <*> areq checkBoxField "t10" (Just True)
            <*> areq checkBoxField "t11" (Just True)
            <*> areq checkBoxField "t12" (Just True)
            <*> areq checkBoxField "t13" (Just True)
            <*> areq checkBoxField "t14" (Just True)
            <*> areq checkBoxField "t15" (Just True)
            <*> areq checkBoxField "t16" (Just True)
            <*> areq checkBoxField "t17" (Just True)
            <*> areq checkBoxField "t18" (Just True)
            <*> areq checkBoxField "t19" (Just True)
            <*> areq checkBoxField "t20" (Just True)
            <*> areq checkBoxField "t21" (Just True)
            <*> areq checkBoxField "t22" (Just True)
            <*> areq checkBoxField "t23" (Just True)
            <*> areq checkBoxField "t24" (Just True)
            <*> areq checkBoxField "t25" (Just True)
            <*> areq checkBoxField "t26" (Just True)
            <*> areq checkBoxField "t27" (Just True)
            <*> areq checkBoxField "t28" (Just True)
            <*> areq checkBoxField "t29" (Just True)
            <*> areq checkBoxField "t30" (Just True)
            <*> areq checkBoxField "t31" (Just True)
            <*> areq checkBoxField "t32" (Just True)
            <*> areq checkBoxField "t33" (Just True)
            <*> areq checkBoxField "t34" (Just True)
            <*> areq checkBoxField "t35" (Just True)
            <*> areq checkBoxField "t36" (Just True)
            <*> areq checkBoxField "t37" (Just True)
            <*> areq checkBoxField "t38" (Just True)
            <*> areq checkBoxField "t39" (Just True)
            <*> areq checkBoxField "t40" (Just True)
            <*> areq checkBoxField "t41" (Just True)
            <*> areq checkBoxField "t42" (Just True)
            <*> areq checkBoxField "t43" (Just True)
            <*> areq checkBoxField "t44" (Just True)
            <*> areq checkBoxField "t45" (Just True)
            <*> areq checkBoxField "t46" (Just True)
            <*> areq checkBoxField "t47" (Just True)
            <*> areq checkBoxField "t48" (Just True)
            <*> areq checkBoxField "t49" (Just True)
            <*> areq checkBoxField "t50" (Just True)
            <*> areq checkBoxField "owner.name" (Just True)
            <*> aopt textField "query" (Just (Just (pack "onwer.name = Owner5")))

-- see.  Database-HDBC-SqlValue.html
instance ToJSON [(String, SqlValue)] where
  toJSON row = object $ map toJSONRow row
      where
        toJSONRow (colName, SqlNull) = ((pack colName) .= Null)
        toJSONRow (colName, sqlValue) = ((pack colName) .= (toText sqlValue))
        toText (Database.HDBC.SqlString s) = pack s
        toText (SqlByteString s) = decodeUtf8 s
        toText (Database.HDBC.SqlWord32 i) = pack $ show i
        toText (Database.HDBC.SqlWord64 i) = pack $ show i
        toText (Database.HDBC.SqlInt32 i) = pack $ show i
        toText (Database.HDBC.SqlInt64 i) = pack $ show i
        toText (Database.HDBC.SqlInteger i) = pack $ show i
        toText (Database.HDBC.SqlChar c) = pack $ [c]
        toText (Database.HDBC.SqlBool b) = pack $ show b
        toText (Database.HDBC.SqlDouble a) = pack $ show a
        toText (Database.HDBC.SqlRational a) = pack $ show a
        toText (Database.HDBC.SqlLocalDate d) = pack $ show d
        toText (Database.HDBC.SqlLocalTimeOfDay d) = pack $ show d
        -- toText (Database.HDBC.SqlZonedLocalTimeOfDay d z) =
        -- SqlZonedLocalTimeOfDay TimeOfDay TimeZone -- ^ Local HH:MM:SS -HHMM.
        -- Converts to and from (TimeOfDay, TimeZone).
        toText (Database.HDBC.SqlLocalTime t) = pack $ show t
        toText (Database.HDBC.SqlZonedTime t) = pack $ show t
        toText (Database.HDBC.SqlUTCTime t) = pack $ show t
        toText (Database.HDBC.SqlDiffTime t) = pack $ show t
        toText (Database.HDBC.SqlPOSIXTime t) = pack $ show t
        toText (Database.HDBC.SqlEpochTime t) = pack $ show t
        toText (Database.HDBC.SqlTimeDiff i) = pack $ show i
        -- toText (Database.HDBC.SqlNull) = pack "NULL"
        toText _ = pack "<undefined>"

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $(widgetFile "CowTable5.hamlet")
  -- ((_, widget), enctype) <- generateFormGet queryForm
  -- defaultLayout $ do
  --   setTitle "Sample coding of Lookup cow table."
  --   [whamlet|Sample coding of Lookup cow table.
  --         <form method=GET action=@{CowJsonR} enctype=#{enctype}>
  --               ^{widget}
  --               <button>Submit
  --       |]


getCowJsonR :: Handler Value
getCowJsonR = do
  ((result, _), _) <- runFormGet queryForm
  case result of
    FormFailure msg -> returnJson $ object ["returned FormFailure" .= msg]
    FormMissing -> returnJson $ object ["returned FormMissing" .= True]
    FormSuccess query -> do
      App conn tableColumns <- getYesod
      cowsRows <- liftIO $ do
        stmt <- prepare conn "SELECT *, owner.name from cow INNER JOIN owner on cow.owner_id == owner.id where owner_id = 5"
        execute stmt []
        fetchAllRowsAL' stmt
      returnJson cowsRows

main :: IO ()
main = do
  conn <- connectSqlite3 "test2.sqlite"
  tables <-getTables conn
  tabCol <- mapM (describeList conn) tables
  print tabCol
  warp 3000 $ App {connection = conn,
                  tableColumns = tabCol}
  disconnect conn
    where
      describeList conn tableName = do
        desc <- describeTable conn tableName
        let labels = map fst desc
        return (tableName, labels)
