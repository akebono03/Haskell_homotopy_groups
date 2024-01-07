{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Lib
-- main :: IO ()
-- main = someFunc

import Web.Spock
import Web.Spock.Config
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Text.Read (readMaybe)
-- import Database.Persist.Sqlite
import Database.SQLite.Simple (open, close, query_, field, FromRow(..), Connection)
import Data.Aeson (ToJSON, toJSON, object, (.=))

-- data Person = Person
--   { personId :: Int
--   , personName :: String
--   , personAge :: Int
--   } deriving (Show)
-- Sphere 型の定義
data Sphere = Sphere
  { sphereId :: Int
  , sphereK :: Int
  , sphereN :: Int
  -- その他のカラムに対応するフィールドを追加
  } deriving (Show)

-- -- FromRow型クラスのインスタンスを作成
-- instance FromRow Person where
--   fromRow = Person <$> field <*> field <*> field
instance FromRow Sphere where
  fromRow = Sphere <$> field <*> field <*> field
  -- 必要に応じて、他のフィールドに対応する field を追加

-- ToJSONインスタンスの定義
-- instance ToJSON Person where
--   -- toJSON (Person id name age) = object ["id" .= id, "name" .= name, "age" .= age]
--   -- toJSON (Person personId name age) = object ["id" .= personId, "name" .= name, "age" .= age]
--   toJSON p = object ["id" .= personId p, "name" .= personName p, "age" .= personAge p]
instance ToJSON Sphere where
  toJSON s = object 
    [ "id" .= sphereId s
    , "k" .= sphereK s
    , "n" .= sphereN s
    -- 他のフィールドも必要に応じて追加
    ]

-- 安全な整数変換関数
readInt :: String -> Int
readInt s = maybe 0 id (readMaybe s :: Maybe Int)

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()  -- Spockアプリケーションの設定
  -- "sphere.db" というSQLiteデータベースファイルを開く
  conn <- open "sphere.db"
  -- Spockアプリケーションを実行
  runSpock 8080 (spock spockCfg (appWithConnection conn))
  -- データベース接続を閉じる
  close conn

-- データベース接続をアプリケーション内で使用するバージョンのapp関数
appWithConnection :: Connection -> SpockM () () () ()
appWithConnection conn = do
  middleware (staticPolicy (addBase "static"))

  Web.Spock.get root $ do
    templateContent <- liftIO $ TIO.readFile "static/template.html"
    html templateContent

  post "calculate" $ do
    formData <- params
    let n = maybe 0 (readInt . T.unpack) (lookup "n" formData)
        k = maybe 0 (readInt . T.unpack) (lookup "k" formData)
        result = n + k
    html $ T.pack $ "n + k = " <> (show result)
    
  -- -- データベースからクエリを実行して結果を取得
  -- que <- liftIO $ query_ conn "SELECT * FROM sphere WHERE n = 3 and k = 8" :: ActionCtxT () (WebStateM () () ()) [Person]
  -- ここでデータベースを利用するロジックを追加
  get "people" $ do
    spheres <- liftIO $ query_ conn "SELECT id, k, n FROM sphere WHERE n = 3 and k = 8" :: ActionCtxT () (WebStateM () () ()) [Sphere]
    let htmlContent = generateHtmlForSphere spheres
    html htmlContent
    -- people <- liftIO $ query_ conn "SELECT * FROM people" :: ActionCtxT () (WebStateM () () ()) [Person]
    -- people <- liftIO $ query_ conn "SELECT * FROM sphere WHERE n = 3 and k = 8" :: ActionCtxT () (WebStateM () () ()) [Person]
    -- 取得したデータをレスポンスとして返す
    -- json people

    -- let htmlContent = generateHtml people
    -- html htmlContent

-- Sphere のリストを HTML に変換する関数
generateHtmlForSphere :: [Sphere] -> T.Text
generateHtmlForSphere spheres = T.concat
  -- HTMLを生成するコード
-- -- Personのリストを受け取り、HTMLを生成する関数
-- generateHtml :: [Person] -> T.Text
-- generateHtml people = T.concat
  [ "<html><head><title>Sphere</title></head><body>"
  , "<h1>Sphere List</h1>"
  , "<ul>"
  , T.concat $ map sphereToHtml spheres
  , "</ul>"
  , "</body></html>"
  ]

-- sphereToHtml :: Person -> T.Text
-- sphereToHtml (Person _ name age) =
--   T.concat ["<li>", T.pack name, " (", T.pack $ show age, " years old)</li>"]
sphereToHtml :: Sphere -> T.Text
sphereToHtml (Sphere ssphereId k n) =
  T.concat ["<li>", "ID: ", T.pack $ show ssphereId, ", K: ", T.pack $ show k, ", N: ", T.pack $ show n, "</li>"]

  -- -- 取得した結果を表示
  -- liftIO $ print que

-- -- メインのSpockアプリケーション
-- app :: SpockM () () () ()
-- -- app :: SpockM SqlBackend () ()
-- app = do
--   middleware (staticPolicy (addBase "static"))  -- 静的ファイルのミドルウェア

--   Web.Spock.get root $ do
--     -- テンプレートファイルを読み込み、レスポンスに埋め込む
--     templateContent <- liftIO $ TIO.readFile "static/template.html"
--     html templateContent
--   post "calculate" $ do
--     formData <- params
--     let n = maybe 0 (readInt . T.unpack) (lookup "n" formData)
--         k = maybe 0 (readInt . T.unpack) (lookup "k" formData)
--         result = n + k
--     html $ T.pack $ "n + k = " <> (show result)      

-- -- Spockアプリケーションの設定
-- spockCfg :: SpockCfg () () ()
-- spockCfg = defaultSpockCfg () PCNoDatabase ()

-- -- メインのSpockアプリケーション
-- app :: SpockM () () () ()
-- app = spock spockCfg $ do
--   middleware (staticPolicy (addBase "static"))  -- 静的ファイルのミドルウェア

--   get root $ do
--     -- テンプレートファイルを読み込み、レスポンスに埋め込む
--     -- templateContent <- liftIO $ TIO.readFile "template.html"
--     templateContent <- liftIO $ TIO.readFile "static/template.html"

--     html templateContent

-- main :: IO ()
-- main = do
--   runSpock 8080 app

