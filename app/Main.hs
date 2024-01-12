{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Web.Spock
import Web.Spock.Config
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Database.SQLite.Simple (open, close, query, query_, field, Only(..), FromRow(..), Connection)
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Text.Printf (printf)

-- Sphere 型の定義
data Sphere = Sphere
  { sphereK :: Int
  , sphereN :: Int
  , sphereId :: Int
  , orders :: String
  , generator :: Maybe String
  , p :: Maybe String
  , p_coe :: Maybe String
  , e :: Maybe String
  , e_coe :: Maybe String
  , h :: Maybe String
  , h_coe :: Maybe String
  , element :: Maybe String
  , gen_coe :: Maybe String
  , hyouji :: Maybe String
  , orders2 :: String
  -- その他のカラムに対応するフィールドを追加
  } deriving (Show)

-- -- FromRow型クラスのインスタンスを作成
instance FromRow Sphere where
  fromRow = Sphere <$> field <*> field <*> field <*> field <*> field 
                   <*> field <*> field <*> field <*> field <*> field
                   <*> field <*> field <*> field <*> field <*> field
  -- 必要に応じて、他のフィールドに対応する field を追加

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
        queryStr = "SELECT k, n, id, orders, generator, \
        \P, P_coe, E, E_coe, H, H_coe, \
        \Element, gen_coe, hyouji, orders2 \
        \FROM sphere WHERE n = ? and k = ?"
    spheres <- liftIO $ query conn queryStr (n, k) 
            :: ActionCtxT () (WebStateM () () ()) [Sphere]
    let htmlContent = generateHtmlForSphere spheres
        htmlString = T.unpack htmlContent

    -- 'template2.html' を読み込む
    templateContent <- liftIO $ TIO.readFile "static/template.html"
    -- プレースホルダーを実際の値で置換
    let finalHtml = 
          T.replace "{k}" (T.pack $ show k) $
          T.replace "{n}" (T.pack $ show n) $
          T.replace "{result}" (T.pack $ show result) $
          T.replace "{sphereList}" (T.pack $ htmlString) templateContent
    html finalHtml
    
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
sphereToHtml (Sphere k n sId ord gene pp pp_coe ee ee_coe hh hh_coe
              element gen_coe hyouji ord2) = T.concat 
  ["<li>"
  , "ID: ", T.pack $ show sId
  , ", K: ", T.pack $ show k
  , ", N: ", T.pack $ show n
  , ", order: ", T.pack $ show ord
  , ", generator: ", T.pack $ stripQuotes (fmap show gene)
  , ", P: ", T.pack $ stripQuotes (fmap show pp)
  , ", P_coe: ", T.pack $ show pp_coe
  , ", E: ", T.pack $ stripQuotes (fmap show ee)
  , ", E_coe: ", T.pack $ show ee_coe
  , ", H: ", T.pack $ stripQuotes (fmap show hh)
  , ", H_coe: ", T.pack $ show hh_coe
  , ", Element: ", T.pack $ show element
  , ", gen_coe: ", T.pack $ show gen_coe
  , ", hyouji: ", T.pack $ stripQuotes (fmap show hyouji)
  , ", order2: ", T.pack $ show ord2
  , "</li>"]

stripQuotes :: Maybe String -> String
stripQuotes (Just str) = "\\(" 
                      ++ (doubleBackslash . stripQuotesHelper) str 
                      ++ "\\)"
stripQuotes Nothing    = ""

stripQuotesHelper :: String -> String
stripQuotesHelper str =
  case str of
    ('"':xs) -> case reverse xs of
                  ('"':ys) -> reverse ys
                  _        -> str
    _        -> str

-- バックスラッシュを書き換える関数
doubleBackslash :: String -> String
doubleBackslash [] = []
doubleBackslash (x:xs)
  | x == '\\' = " \\" ++ doubleBackslash xs
  | otherwise = [x] ++ doubleBackslash xs
-- doubleBackslash = concatMap (\c -> if c == '\\' then " \\" else [c])


-- ToJSONインスタンスの定義
instance ToJSON Sphere where
  toJSON s = object 
    [ "k" .= sphereK s
    , "n" .= sphereN s
    , "id" .= sphereId s
    -- 他のフィールドも必要に応じて追加
    ]


