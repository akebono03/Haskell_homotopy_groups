{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where
-- Web.Spockを使ってWebアプリケーションを構築するためのインポート
import Web.Spock
import Web.Spock.Config
-- 静的ファイルのサービスに必要なミドルウェアをインポート
import Network.Wai.Middleware.Static (staticPolicy, addBase)
-- テキストデータの操作に関するインポート
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
-- ユーザー入力の解析や変換に関するユーティリティ
import Text.Read (readMaybe)
-- SQLiteデータベースの操作に関するインポート
import Database.SQLite.Simple (open, close, query, query_, field, Only(..), FromRow(..), Connection, Query(..))
-- JSONデータの操作に関するインポート
import Data.Aeson (ToJSON, toJSON, object, (.=))
-- テキストのフォーマットに関するユーティリティ
import Text.Printf (printf)
-- その他の一般的なユーティリティ
import Control.Applicative ((<|>))
import Control.Monad
import Control.Exception (SomeException, catch)
-- リスト操作に関連するユーティリティ
import Data.List (intercalate, zip4, zip5, group, isPrefixOf)

-- Sphere 型の定義（ホモトピー群のデータを表す）
data Sphere = Sphere
  { sphereK :: Int
  , sphereN :: Int
  , sphereId :: Int
  , orders :: Either Int String  -- 整数または文字列を格納する Either 型
  , generator :: Maybe String
  , p :: Maybe String
  , p_coe :: Maybe String
  , e :: Maybe String
  , e_coe :: Maybe String
  , h :: Maybe String
  , h_coe :: Maybe String
  , element :: String
  , gen_coe :: String
  , hyouji :: Maybe String
  , orders2 :: Either Int String  -- 整数または文字列を格納する Either 型
  } deriving (Show)

-- -- FromRow型クラスのインスタンスを作成（データベースの行からSphere型のデータを生成するため）
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

-- Gen 型の定義（球面のホモトピー群の生成元に関するデータを表す）
data Gen = Gen
  { genId :: Int
  , genDisplay :: Maybe String
  , genKinds :: Int
  , genK :: Int
  , genN :: Int
  , genOrders :: Maybe Int
  , genLatex :: String
  , genStable :: Maybe String
  , genStableOrders :: Maybe Int
  } deriving (Show)

-- FromRow型クラスのインスタンスを作成（データベースの行からGen型のデータを生成するため）
instance FromRow Gen where
  fromRow = Gen
    <$> field -- genId
    -- <*> (Left <$> field <|> Right <$> field) -- genDisplay
    <*> field -- genDisplay
    <*> field -- genKinds
    <*> field -- genK
    <*> field -- genN
    <*> field -- genOrders
    -- <*> (Left <$> field <|> Right <$> field) -- genLatex
    <*> field -- genLatex
    <*> field -- genStable
    <*> field -- genStableOrders

-- ホモトピー群の定義
data HomotopyGroupOfShpere = HomotopyGroupOfShpere { k :: Int, n :: Int }

data Order = Finite Int | Infinite
  deriving (Show, Eq)

type Tex = String
type ElTex = String

-- データベース接続をアプリケーション内で使用するバージョンのapp関数
appWithConnection :: Connection -> SpockM () () () ()
appWithConnection conn = do
  -- 静的ファイルを提供するためのミドルウェア設定
  middleware (staticPolicy (addBase "static"))
  -- ルートパス (/) にアクセスした時の処理
  Web.Spock.get root $ do
    templateContent <- liftIO $ TIO.readFile "static/template.html"
    html templateContent
  -- "/calculate" エンドポイントに対するPOSTリクエストを処理
  post "calculate" $ do
    formData <- params
    let n = maybe 0 (readInt . T.unpack) (lookup "n" formData)
        k = maybe 0 (readInt . T.unpack) (lookup "k" formData)
        result = n + k
    -- データベースから指定されたnとkに対するデータを取得
        queryStr = "SELECT k, n, id, orders, generator, \
        \P, P_coe, E, E_coe, H, H_coe, \
        \Element, gen_coe, hyouji, orders2 \
        \FROM sphere WHERE n = ? and k = ?"
    spheres <- liftIO $ query conn queryStr (n, k) 
            :: ActionCtxT () (WebStateM () () ()) [Sphere]


    -- 'template2.html' を読み込む
    templateContent <- liftIO $ TIO.readFile "static/template.html"
    -- プレースホルダーを実際の値で置換
    let queryRs = queryRows k n
    ords <- liftIO $ getOrders conn queryRs
    let ordsString = texToString $ ordersToGroupTex ords
    let groupList = ordersToGroupList ords
    elems <- liftIO $ getGenerators conn queryRs n

    genCoes <- liftIO $ getGenCoes conn queryRs
    let genCoeLists = map stringToList genCoes
        gens = map (linearCombination elems) genCoeLists

    let groupGeneratorTex = intercalate "\\oplus " [gr ++ "\\{" ++ gen ++ "\\}" | (gr, gen) <- zip groupList gens]

    -- SphereのリストをHTMLに変換
    htmlContent <- liftIO $ generateHtmlForSphere conn elems spheres
    let htmlString = T.unpack htmlContent
    let finalHtml = 
          T.replace "{k}" (T.pack $ show k) $
          T.replace "{n}" (T.pack $ show n) $
          T.replace "{nk}" (T.pack $ show (n+k)) $
          T.replace "{result}" (T.pack $ show result) $
          T.replace "{sphereList}" (T.pack $ htmlString) $
          T.replace "{group}" (T.pack $ "\\(" ++ groupGeneratorTex ++ "\\)") templateContent

    -- 最終的なHTMLをレスポンスとして返す
    html finalHtml
    
-- Sphere のリストを HTML に変換する関数
generateHtmlForSphere :: Connection -> [String] -> [Sphere] -> IO T.Text
generateHtmlForSphere conn elems spheres = do
  sphereHtml <- spheresToHtml conn elems spheres
  return $ T.concat
    -- HTMLを生成するコード
    [ "<html><head><title>Sphere</title></head><body>"
    , "<ul>"
    , sphereHtml
    , "</ul>"
    , "</body></html>"
    ]

-- Spheres を HTML リストアイテムに変換する関数
spheresToHtml :: Connection -> [String] -> [Sphere] -> IO T.Text
spheresToHtml conn elems spheres = do
  rows <- mapM (sphereRowToHtml conn elems) spheres
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
    , "</table>"
    ]

-- SphereオブジェクトをHTMLテーブルの行に変換する関数
sphereRowToHtml :: Connection -> [String] -> Sphere -> IO T.Text
sphereRowToHtml conn elems (Sphere kk nn sId ord gene pp _ ee _ hh _ element genCoe hyouji ord2) = do
  -- Sphereオブジェクトから個々のデータを取得し、HTML形式に変換
  let elementList = stringToList element
  latexList <- liftIO $ getLatexList conn elementList
  elemDimList <- liftIO $ elDimList conn nn elementList
  elemSusList <- liftIO $ elSusList conn nn elementList
  let latexText = T.concat $ map T.pack latexList
      elemDimText = T.concat $ map (T.pack . show) elemDimList
      elemSusText = T.intercalate (T.pack ",") $ map (T.pack . show) elemSusList
      -- elemSusText = T.concat $ map (T.pack . show) elemSusList
      -- elemDimText = T.pack elemDimList
      -- pi_k_n = HomotopyGroupOfShpere { k=kk, n=nn }
      -- piTex_k_n = T.pack $ piTex pi_k_n
      queryCnt = queryCount kk nn
      queryRs = queryRows kk nn
      genCoeList = stringToList genCoe
      genTex = linearCombination elems genCoeList
  dSum <- liftIO $ directSum conn queryCnt
  ords <- liftIO $ getOrders conn queryRs
  elTex <- liftIO $ elToTex conn nn elementList
  let -- この関数を使用して、ords を変換
    ordsString = texToString $ ordersToGroupTex ords
  return $ T.concat
    [ "<tr>"
    , "<th>", T.pack $ show sId, "</th>"
    , "<th>", T.pack $ showEither ord, "</th>"
    , "<th>", T.pack $ "\\(" ++ genTex ++ "\\)" , "</th>"
    , "<td>", T.pack $ stripQuotes (fmap show pp), "</td>"
    , "<td>", T.pack $ stripQuotes (fmap show ee), "</td>"
    , "<td>", T.pack $ stripQuotes (fmap show hh), "</td>"
    , "<th>", T.pack $ "\\(" ++ elTex ++ "\\)" , "</th>"
    -- , "<td>", T.concat ["\\(", latexText, "\\)", T.pack (show dSum), T.pack elTex] , "</td>"
    -- , "<td>", elemDimText , "</td>"
    , "</tr>"
    ]

-- 安全な整数変換関数
readInt :: String -> Int
readInt s = maybe 0 id (readMaybe s :: Maybe Int)

-- 文字列を安全に引用符から解放する関数
stripQuotes :: Maybe String -> String
stripQuotes (Just str) = "\\(" 
                      ++ (doubleBackslash . stripQuotesHelper) str 
                      ++ "\\)"
stripQuotes Nothing    = ""

-- 引用符を取り除くヘルパー関数
stripQuotesHelper :: String -> String
stripQuotesHelper str =
  case str of
    ('"':xs) -> case reverse xs of
                  ('"':ys) -> reverse ys
                  _        -> str
    _        -> str

-- リストを文字列に変換する関数
listToString :: [Int] -> String
listToString xs = intercalate " " $ map show xs

-- Order型の値を文字列に変換する関数
orderToString :: Order -> String
orderToString (Finite x) = show x
orderToString Infinite   = "Inf"

-- Order型の値を文字列に変換する関数（Either型を使用）
ordersToString :: Either String [Order] -> String
ordersToString (Left str)   = str
ordersToString (Right ords) = concat $ map orderToString ords

ordersToGroupList :: Either String [Order] -> [Tex]
ordersToGroupList (Left str) = [str]
ordersToGroupList (Right ords) = [if ord == Infinite then "Z" else "Z_{" ++ orderToString ord ++ "}" | ord <- ords]

ordersToGroupTex :: Either String [Order] -> Tex
ordersToGroupTex (Left str)   = str
ordersToGroupTex (Right ords) = intercalate "\\oplus " ["Z_{" ++ orderToString ord ++ "}" | ord <- ords]

texToString :: Tex -> String
texToString tex = "\\(" ++ tex ++ "\\)"

-- Either String String を Maybe String に変換
eitherToMaybe :: Either String [Order] -> Maybe [Order]
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right s) = Just s

-- テキスト中のバックスラッシュをエスケープする関数
doubleBackslash :: String -> String
doubleBackslash [] = []
doubleBackslash (x:xs)
  | x == '\\' = " \\" ++ doubleBackslash xs
  | otherwise = [x] ++ doubleBackslash xs

-- Either型の値を文字列に変換する関数
showEither :: Show a => Show b => Either a b -> String
showEither (Left a)  = show a  -- Leftの場合は値を返す
showEither (Right b) = show b  -- Rightの場合は値を表示

-- 特定のIDに対応する要素のlatex表現を取得する関数
elId :: Connection -> Int -> IO String
elId conn id = do
  [Only res] <- query conn "SELECT latex FROM gen WHERE id = ?" (Only id)
  return res

-- 空白で分割してリストに変換する関数
stringToList :: String -> [Int]
stringToList input = map read (words input)

-- [Int]型のリストに対して、それぞれの要素のlatex表現を取得する関数
getLatexList :: Connection -> [Int] -> IO [String]
getLatexList conn ids = do
  let queryStr = "SELECT latex FROM gen WHERE id = ?"
  results <- mapM (query conn queryStr . Only) ids
  -- results は [Only String] のリストになります
  -- Only コンストラクタから値を取り出して [String] に変換
  return [res | Only res <- concat results]

-- 指定したnと要素リストに基づいて、各要素の次元をリストで返す関数
elDimList :: Connection -> Int -> [Int] -> IO [Int]
elDimList conn n elList = foldM accumulate [n] elList
  where
    accumulate acc elem = do
      kRows <- query conn "SELECT k FROM gen WHERE id = ?" (Only elem) :: IO [Only Int]
      let newSum = case kRows of
            (Only kRow:_) -> last acc + kRow
            []            -> last acc
      return (acc ++ [newSum])

-- 指定したnと要素リストに基づいて、各要素の懸垂数をリストで返す関数
elSusList :: Connection -> Int -> [Int] -> IO [Int]
elSusList conn nn elList = do
  elDims <- liftIO $ elDimList conn nn elList
  res <- forM (zip elList elDims) $ \(elem, elDim) -> 
    (do
      nRows <- query conn "SELECT n FROM gen WHERE id = ?" (Only elem) :: IO [Only Int]
      let n = case nRows of
                (Only nRow:_) -> nRow
                []            -> error "No data found"
      return (elDim - n))
  return res
  -- return (res ++ [last elDims])

-- 特定の要素リストに基づいて、各要素の種類をリストで返す関数
elKindList :: Connection -> [Int] -> IO [Int]
elKindList conn elList = do
  res <- forM elList $ \elem ->
    (do
      nRows <- query conn "SELECT kinds FROM gen WHERE id = ?" (Only elem) :: IO [Only Int]
      let kind = case nRows of
                   (Only nRow:_) -> nRow
                   []            -> error "No data found"
      return kind)
  return res

-- 要素の種類に基づいて特定の処理を行う関数（例えば、出力の整形など）
elSqList :: [Int] -> [Int]
elSqList elList =
  concat [[length groupLi] ++ replicate ((length groupLi)-1) 0 | groupLi <- groupList]
  where
    groupList = group elList

-- LaTeX形式での表現を生成する関数
-- 数学的なデータをTeX形式の文字列に変換するために使用されます。
elToTex :: Connection -> Int -> [Int] -> IO ElTex
elToTex conn nn elementList = do
  -- 各要素のLaTeX表現をデータベースから取得
  latexList    <- liftIO $ getLatexList conn elementList
  elemDimList  <- liftIO $ elDimList conn nn elementList
  elemSusList  <- liftIO $ elSusList conn nn elementList
  elemKindList <- liftIO $ elKindList conn elementList
  let elemSqList = elSqList elementList
  -- 上記の情報を組み合わせて、TeX表現のリストを生成
  let texList = zip5 latexList elemDimList elemSusList elemKindList elemSqList
  return $ intercalate "" (map makeTex texList)

-- 各要素をTeX形式の文字列に変換するためのヘルパー関数
makeTex :: (String, Int, Int, Int, Int) -> String
makeTex (latex, dim, sus, kind, sq) = case kind of
  0 -> texKindZero latex dim sq
  1 -> texKindOne latex sus
  _ -> "Not defined"

-- 種類0の要素をTeX形式に変換する関数
texKindZero :: String -> Int -> Int -> String
texKindZero latex dim sq
  | sq == 0   = ""
  | otherwise = latexFormat latex dim sq

-- LaTeX形式に整形するための関数
latexFormat :: String -> Int -> Int -> String
latexFormat latex dim sq
  | sq == 1 && last latex == ',' = latex ++ show dim ++ "}"
  | sq == 1 && last latex /= ',' = latex ++ "_{" ++ show dim ++ "}"
  | last latex == ','            = latex ++ show dim ++ "}^{" ++ show sq ++ "}"
  | otherwise                    = latex ++ "_{" ++ show dim ++ "}^{" ++ show sq ++ "}"

-- 種類1の要素をTeX形式に変換する関数
texKindOne :: String -> Int -> String
texKindOne latex sus
  | sus == 0  = latex
  | sus == 1  = "E " ++ latex
  | otherwise = "E^{" ++ show sus ++ "}" ++ latex

-- 生成元のリストと係数のリストから線形結合の文字列を生成する関数
linearCombination :: [String] -> [Int] -> String
linearCombination generators coefficients
  | length generators == length cutCoefficients = result
  | otherwise = "Invalid input: Generators and coefficients must have the same length."
  where
    cutCoefficients = take (length generators) coefficients
    -- 線形結合の生成
    result = formatCombination $ zipWith formatTerm generators cutCoefficients
    -- 1つの項を整形する関数
    formatTerm generator coefficient
      | coefficient == 0 = ""  -- 係数が0の場合は項を無視
      | coefficient == 1 = generator
      | coefficient == -1 = "-" ++ generator
      | otherwise = show coefficient ++ generator  -- その他の場合は "+" を付ける
    -- 線形結合全体を整形する関数
    formatCombination terms = replaceString "+-" "-" $ intercalate "+" $ filter (not . null) terms

-- 文字列を置き換える関数
replaceString :: String -> String -> String -> String
replaceString _ _ [] = []  -- 空文字列の場合は何もしない
replaceString target replacement inputStr@(x:xs)
    | target `isPrefixOf` inputStr = replacement ++ replaceString target replacement (drop (length target) inputStr)
    | otherwise = x : replaceString target replacement xs

-- 特定のクエリに基づいてデータベースから生成元の情報を取得する関数
getGenerators :: Connection -> Query -> Int -> IO [ElTex]
getGenerators conn queryRs nn = do
  rows <- query_ conn queryRs :: IO [Sphere]
  mapM (convertToTex conn nn) rows

-- Sphereオブジェクトから特定のテキスト表現（ElTex）を生成する関数
convertToTex :: Connection -> Int -> Sphere -> IO ElTex
convertToTex conn nn sphere = elToTex conn nn $ stringToList (element sphere)

-- ホモトピー群のTeX表現を生成する関数
piTex :: HomotopyGroupOfShpere -> String
piTex mt = "\\pi_{" ++ show (n mt + k mt) ++ "}^{" ++ show (n mt) ++ "}"

-- データベースから特定のクエリに基づいて集計する関数
directSum :: Connection -> Query -> IO Int
directSum conn queryCount = do
  rows <- query_ conn queryCount :: IO [Only Int]
  case rows of
    (Only count:_) -> return count
    []             -> return 0  -- もし結果がない場合は0を返す

-- 特定の条件に基づいてデータベースから情報を取得するためのクエリを生成する関数
queryRows :: Int -> Int -> Query
queryRows k n
  | k <= -1   = stringToQuery   "SELECT * FROM sphere WHERE n = 0 AND k = -1"
  | k+2 >= n  = stringToQuery $ "SELECT * FROM sphere WHERE n = " ++ show n ++ " AND k = " ++ show k
  | otherwise = stringToQuery $ "SELECT * FROM sphere WHERE n = " ++ show (k+2) ++ " AND k = " ++ show k

-- 特定の条件に基づいてデータベースから情報を集計するためのクエリを生成する関数
queryCount :: Int -> Int -> Query
queryCount k n
  | k <= -1   = stringToQuery   "SELECT COUNT(*) FROM sphere WHERE n = 0 AND k = -1"
  | k+2 >= n  = stringToQuery $ "SELECT COUNT(*) FROM sphere WHERE n = " ++ show n ++ " AND k = " ++ show k
  | otherwise = stringToQuery $ "SELECT COUNT(*) FROM sphere WHERE n = " ++ show (k+2) ++ " AND k = " ++ show k

-- 文字列をQuery型に変換するユーティリティ関数
stringToQuery :: String -> Query
stringToQuery str = Query $ T.pack str

-- Sphere型からOrder型に変換する関数
convertToOrder :: Sphere -> Order
convertToOrder sphere = case orders sphere of
  Left n -> Finite n
  Right "inf" -> Infinite
  Right _ -> Finite 0 -- 文字列が "inf" でない場合のデフォルト処理

-- データベースからOrder型のデータを取得する関数
getOrders :: Connection -> Query -> IO (Either String [Order])
getOrders conn query = catch (do
    rows <- query_ conn query :: IO [Sphere]
    let orders = map convertToOrder rows
    return $ Right orders
  ) handler
  where
    handler :: SomeException -> IO (Either String [Order])
    handler err = return $ Left $ "Error: " ++ show err

getGenCoes :: Connection -> Query -> IO [String]
getGenCoes conn query = do
    rows <- query_ conn query :: IO [Sphere]
    let genCoes = map gen_coe rows
    return genCoes




-- メイン関数（アプリケーションのエントリーポイント）
main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()  -- Spockアプリケーションの設定
  -- "sphere.db" というSQLiteデータベースファイルを開く
  conn <- open "sphere.db"
  -- Spockアプリケーションを実行
  runSpock 8080 (spock spockCfg (appWithConnection conn))
  -- データベース接続を閉じる
  close conn

