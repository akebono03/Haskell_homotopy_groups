{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Spock
import Web.Spock.Config
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Database.SQLite.Simple (open, close, query_, field, FromRow(..), Connection)
import Data.Aeson (ToJSON, toJSON, object, (.=))
-- import Data.Maybe (fromMaybe)

-- Sphere 型の定義
data Sphere = Sphere
  { sphereId :: Float
  , sphereK :: Float
  , sphereN :: Float
  , orders :: Float
  , generator :: String
  , p :: Maybe String
  , p_coe :: Maybe String
  -- その他のカラムに対応するフィールドを追加
  } deriving (Show)

-- -- FromRow型クラスのインスタンスを作成
instance FromRow Sphere where
  fromRow = Sphere <$> field <*> field <*> field <*> field <*> field <*> field <*> field
  -- 必要に応じて、他のフィールドに対応する field を追加

-- ToJSONインスタンスの定義
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
    spheres <- liftIO $ query_ conn "SELECT id, k, n, orders, generator, P, P_coe FROM sphere WHERE n = 13 and k = 0" :: ActionCtxT () (WebStateM () () ()) [Sphere]
    let n = maybe 0 (readInt . T.unpack) (lookup "n" formData)
        k = maybe 0 (readInt . T.unpack) (lookup "k" formData)
        result = n + k
        htmlContent = generateHtmlForSphere spheres
        htmlString = T.unpack htmlContent
        -- ss = if htmlString == "" then "aaaa" else "bbbb"
    -- html $ T.pack $ "n + k = " <> (show result) <> htmlString
    -- 'template2.html' を読み込む
    templateContent <- liftIO $ TIO.readFile "static/template2.html"
    -- プレースホルダーを実際の値で置換
    let finalHtml = T.replace "{result}" (T.pack $ show result) $
                    T.replace "{sphereList}" (T.pack $ htmlString) templateContent
    html finalHtml
    
--   -- ここでデータベースを利用するロジックを追加
--   get "calculate" $ do
--     spheres <- liftIO $ query_ conn "SELECT id, k, n, orders, generator FROM sphere WHERE n = 3 and k = 8" :: ActionCtxT () (WebStateM () () ()) [Sphere]
--     let htmlContent = generateHtmlForSphere spheres
--     html htmlContent
--     -- 取得したデータをレスポンスとして返す
--     -- json people


-- Sphere のリストを HTML に変換する関数
generateHtmlForSphere :: [Sphere] -> T.Text
generateHtmlForSphere spheres = T.concat
  -- HTMLを生成するコード
  [ "<html><head><title>Sphere</title></head><body>"
  , "<h1>Sphere List</h1>"
  , "<ul>"
  , T.concat $ map sphereToHtml spheres
  , "</ul>"
  , "</body></html>"
  ]

sphereToHtml :: Sphere -> T.Text
sphereToHtml (Sphere sId k n ord gene pp pp_coe) =
  T.concat ["<li>", "ID: ", T.pack $ show sId, ", K: ", T.pack $ show k, ", N: ", T.pack $ show n, ", order: ", T.pack $ show ord, ", generator: ", T.pack $ show gene, ", P: ", T.pack $ stripQuotes (fmap show pp), ", P_coe: ", T.pack $ show pp_coe, "</li>"]

stripQuotes :: Maybe String -> String
stripQuotes (Just str) = "\\(" ++ stripQuotesHelper str ++ "\\)"
stripQuotes Nothing    = ""

stripQuotesHelper :: String -> String
stripQuotesHelper str =
  case str of
    ('"':xs) -> case reverse xs of
                  ('"':ys) -> reverse ys
                  _        -> str
    _        -> str






