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
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Aeson ()
import           Data.Aeson.Types ()
import qualified Database.HDBC as DB
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

-- see.  Database-HDBC-SqlValue.html
instance ToJSON [(String, DB.SqlValue)] where
  toJSON row = object $ map toJSONRow row
      where
        toJSONRow (n, (DB.SqlNull)) = ((T.pack n) .= Null)
        toJSONRow (n, (DB.SqlWord32 w)) = ((T.pack (n++":w32")) .= w)
        toJSONRow (n, (DB.SqlWord64 w)) = ((T.pack (n++":w64")) .= w)
        toJSONRow (n, (DB.SqlInt32 i)) = ((T.pack (n++":i32")) .= i)
        toJSONRow (n, (DB.SqlInt64 i)) = ((T.pack (n++":w64")) .= i)
        toJSONRow (n, (DB.SqlInteger i)) = ((T.pack (n++":i")) .= i)
        toJSONRow (n,  (DB.SqlBool b)) = ((T.pack n) .= b)
        toJSONRow (n, (DB.SqlDouble a)) = ((T.pack (n++":d")) .= a)
        toJSONRow (n, (DB.SqlRational a)) = ((T.pack (n++":r")) .= a)
        toJSONRow (n, (DB.SqlString s)) = ((T.pack (n++":s")) .= T.pack s)
        toJSONRow (n, (DB.SqlByteString s)) = ((T.pack (n++":b")) .= decodeUtf8 s)
        toJSONRow (n, (DB.SqlChar c)) = ((T.pack (n++":c")) .= (T.pack $ [c]))
        toJSONRow (n, x ) = ((T.pack (n++":default")) .= (toText x))
        -- toText (DB.SqlLocalDate d) = T.pack $ show d
        -- toText (DB.SqlLocalTimeOfDay d) = T.pack $ show d
        -- -- toText (DB.SqlZonedLocalTimeOfDay d z) =
        -- -- SqlZonedLocalTimeOfDay TimeOfDay TimeZone -- ^ Local HH:MM:SS -HHMM.
        -- -- Converts to and from (TimeOfDay, TimeZone).
        -- toText (DB.SqlLocalTime t) = T.pack $ show t
        -- toText (DB.SqlZonedTime t) = T.pack $ show t
        -- toText (DB.SqlUTCTime t) = T.pack $ show t
        -- toText (DB.SqlDiffTime t) = T.pack $ show t
        -- toText (DB.SqlPOSIXTime t) = T.pack $ show t
        -- toText (DB.SqlEpochTime t) = T.pack $ show t
        -- toText (DB.SqlTimeDiff i) = T.pack $ show i
        toText _ = T.pack "<undefined>"

getHomeR :: Handler Html
getHomeR = do
  defaultLayout [whamlet|
<h1>表示項目の選択と、クエリの入力
<form action=@{CowJsonR} method="GET">
    耳標番号:<input name="earNum" type="checkbox" checked value="1">
    名前:<input name="name" type="checkbox" checked value="1">
    誕生日:<input name="birth" type="checkbox" checked value="1">
    性別:<input name="sex" type="checkbox" checked value="1">
    所有者名:<input name="ownerName" type="checkbox" checked value="1">
    <p>
    t1-10:<input name="t1" type="checkbox" checked value="1">
    t11-20:<input name="t2" type="checkbox" checked value="1">
    t21-30:<input name="t3" type="checkbox" checked value="1">
    t31-40:<input name="t4" type="checkbox" checked value="1">
    t41-50:<input name="t5" type="checkbox" checked value="1">
    <p>
    検索条件(where):
    <input name="where" type="text" value="owner.name = \"Owner5\"">
    <p>
    <input type="submit" value="送信">
                 |]
--  defaultLayout $(widgetFile "CowTable5.hamlet")

getCowJsonR :: Handler Value
getCowJsonR = do
  columns <- editColumns
  query <- (runInputGet $ iopt textField "where") 
  App conn tableColumns <- getYesod
  cowsRows <- liftIO $ do
    let sqlQ = T.concat ["SELECT "
                      , columns
                      , " from cow INNER JOIN owner on cow.owner_id == owner.id"
                      , (whereQ query)]
        whereQ (Just "") = ""
        whereQ (Just q) = T.concat [" where ", q]
        whereQ _ = ""
    -- let sqlQ = "SELECT * FROM cow INNER JOIN owner ON cow.owner_id == owner.id WHERE owner.id = 5"
    stmt <- DB.prepare conn $ T.unpack sqlQ
    DB.execute stmt []
    DB.fetchAllRowsAL' stmt
  returnJson cowsRows

editColumns = do
        let colTags  = ["earNum",  "name",     "birth", "sex", "ownerName"]
            colNames = ["ear_num", "cow.name", "birth", "sex", "owner.name"]
            groupTags = ["t1", "t2", "t3", "t4", "t5"]
            groupNames = ["t1, t2, t3, t4, t5, t6, t7, t8, t9, t10",
                          "t11, t12, t13, t14, t15, t16, t17, t18, t19, t20",
                          "t21, t22, t23, t24, t25, t26, t27, t28, t29, t30",
                          "t31, t32, t33, t34, t35, t36, t37, t38, t39, t40",
                          "t41, t42, t43, t44, t45, t46, t47, t48, t49, t50"]
        colList <- mapM (\t -> runInputGet $ iopt textField t)
                        (colTags ++ groupTags)
        let cols = T.intercalate ", " $ map (\(c,f)-> (c) ) $
                   filter checked $ zip (colNames ++ groupNames) colList
--             cols = (T.intercalate ", " colss) 
        liftIO $ print cols
        return cols
          where
            checked (_, (Just "1")) = True
            checked (_, _) = False

main :: IO ()
main = do
  conn <- connectSqlite3 "test2.sqlite"
  tables <-DB.getTables conn
  tabCol <- mapM (describeList conn) tables
  print tabCol
  warp 3000 $ App {connection = conn,
                  tableColumns = tabCol}
  DB.disconnect conn
    where
      describeList conn tableName = do
        desc <- DB.describeTable conn tableName
        let labels = map fst desc
        return (tableName, labels)
