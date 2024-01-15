{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Control.Applicative ((<|>))

-- Sphere 型の定義
data Sphere = Sphere
  { sphereK :: Int
  , sphereN :: Int
  , sphereId :: Int
  , orders :: Either String Int  -- 整数または文字列を格納する Either 型
  , generator :: Maybe String
  , p :: Maybe String
  , p_coe :: Maybe String
  , e :: Maybe String
  , e_coe :: Maybe String
  , h :: Maybe String
  , h_coe :: Maybe String
  , element :: String
  , gen_coe :: Maybe String
  , hyouji :: Maybe String
  , orders2 :: Either String Int  -- 整数または文字列を格納する Either 型
  } deriving (Show)

-- -- FromRow型クラスのインスタンスを作成
instance FromRow Sphere where
  fromRow = Sphere
    <$> field -- sphereK
    <*> field -- sphereN
    <*> field -- sphereId
    <*> (Left <$> field <|> Right <$> field) -- orders
    <*> field -- generator
    <*> field -- p
    <*> field -- p_coe
    <*> field -- e
    <*> field -- e_coe
    <*> field -- h
    <*> field -- h_coe
    <*> field -- element
    <*> field -- gen_coe
    <*> field -- hyouji
    <*> (Left <$> field <|> Right <$> field) -- orders2

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
    -- let htmlContent = generateHtmlForSphere conn spheres
    --     htmlString = T.unpack htmlContent
    htmlContent <- liftIO $ generateHtmlForSphere conn spheres
    let htmlString = T.unpack htmlContent
    -- 'template2.html' を読み込む
    templateContent <- liftIO $ TIO.readFile "static/template.html"
    -- プレースホルダーを実際の値で置換
    let finalHtml = 
          T.replace "{k}" (T.pack $ show k) $
          T.replace "{n}" (T.pack $ show n) $
          T.replace "{nk}" (T.pack $ show (n+k)) $
          T.replace "{result}" (T.pack $ show result) $
          T.replace "{sphereList}" (T.pack $ htmlString) templateContent
    html finalHtml
    
-- Sphere のリストを HTML に変換する関数
generateHtmlForSphere :: Connection -> [Sphere] -> IO T.Text
generateHtmlForSphere conn spheres = do
  sphereHtml <- spheresToHtml conn spheres
  return $ T.concat
    -- HTMLを生成するコード
    [ "<html><head><title>Sphere</title></head><body>"
    , "<ul>"
    , sphereHtml
    -- , spheresToHtml conn spheres
    , "</ul>"
    , "</body></html>"
    ]

-- Spheres を HTML リストアイテムに変換する関数
spheresToHtml :: Connection -> [Sphere] -> IO T.Text
spheresToHtml conn spheres = do
  -- rows <- mapM (sphereRowToHtml conn) spheres
  rows <- mapM (sphereRowToHtml conn) spheres
  return $ T.concat
    [ "<table border=\"1\">"
    , "<tr>"
    , "<th> id </th>"
    , "<th> order </th>"
    , "<th> generator </th>"
    , "<th> P </th>"
    , "<th> E </th>"
    , "<th> H </th>"
    , "<th> element </th>"
    , "</tr>"
    , T.concat rows
    -- , T.concat [sphereRowToHtml conn sphere | sphere <- spheres]
    , "</table>"
    ]

sphereRowToHtml :: Connection -> Sphere -> IO T.Text
sphereRowToHtml conn (Sphere _ _ sId ord gene pp _ ee _ hh _ element _ hyouji ord2) = do
  latexList <- liftIO $ getLatexList conn $ stringToList element
  let latexText = T.concat $ map T.pack latexList
  return $ T.concat
    [ "<tr>"
    , "<th>", T.pack $ show sId, "</th>"
    , "<th>", T.pack $ showEither ord, "</th>"
    , "<th>", T.pack $ stripQuotes (fmap show gene), "</th>"
    , "<td>", T.pack $ stripQuotes (fmap show pp), "</td>"
    , "<td>", T.pack $ stripQuotes (fmap show ee), "</td>"
    , "<td>", T.pack $ stripQuotes (fmap show hh), "</td>"
    -- , "<td>", T.pack $ show $ stringToList element, "</td>"
    -- , "<td>", do
    --     latexList <- liftIO $ getLatexList conn $ stringToList element
    --     return $ T.concat $ map T.pack latexList
    -- , "</td>"
    -- , "<td>", latexText, "</td>"
    -- , "<td>", T.pack $ stripQuotes (fmap show latexText), "</td>"
        , "<td>", T.concat ["\\(", latexText, "\\)"] , "</td>"
    , "</tr>"
    ]
  -- where
  --   latexText = do :: T.Text
  --       latexList <- liftIO $ getLatexList conn $ stringToList element
  --       return $ T.concat $ map T.pack latexList

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

showEither :: Show a => Show b => Either a b -> String
showEither (Left a)  = show a  -- Leftの場合は値を返す
showEither (Right b) = show b  -- Rightの場合は値を表示

elId :: Connection -> Int -> IO String
elId conn id = do
  [Only res] <- query conn "SELECT latex FROM gen WHERE id = ?" (Only id)
  return res

-- 空白で分割してリストに変換する関数
stringToList :: String -> [Int]
stringToList input = map read (words input)

-- [Int] から [String] へ変換する関数
getLatexList :: Connection -> [Int] -> IO [String]
getLatexList conn ids = do
  let queryStr = "SELECT latex FROM gen WHERE id = ?"
  results <- mapM (query conn queryStr . Only) ids
  -- results は [Only String] のリストになります
  -- Only コンストラクタから値を取り出して [String] に変換
  return [res | Only res <- concat results]




-- sphereToHtml :: Sphere -> T.Text
-- sphereToHtml (Sphere k n sId ord gene pp pp_coe ee ee_coe hh hh_coe
--               element gen_coe hyouji ord2) = T.concat 
--   ["<li>"
--   , "k: ", T.pack $ show k
--   , ", n: ", T.pack $ show n
--   , ", id: ", T.pack $ show sId
--   , ", order: ", T.pack $ show ord
--   , ", generator: ", T.pack $ stripQuotes (fmap show gene)
--   , ", P: ", T.pack $ stripQuotes (fmap show pp)
--   -- , ", P_coe: ", T.pack $ show pp_coe
--   , ", E: ", T.pack $ stripQuotes (fmap show ee)
--   -- , ", E_coe: ", T.pack $ show ee_coe
--   , ", H: ", T.pack $ stripQuotes (fmap show hh)
--   -- , ", H_coe: ", T.pack $ show hh_coe
--   , ", Element: ", T.pack $ show element
--   -- , ", gen_coe: ", T.pack $ show gen_coe
--   , ", hyouji: ", T.pack $ stripQuotes (fmap show hyouji)
--   , ", order2: ", T.pack $ show ord2
--   , "</li>"]

