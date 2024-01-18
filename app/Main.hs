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
import Database.SQLite.Simple (open, close, query, query_, field, Only(..), FromRow(..), Connection, Query(..))
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Text.Printf (printf)
import Control.Applicative ((<|>))
import Control.Monad
import Control.Exception (SomeException, catch)
import Data.List (intercalate, zip4, zip5, group)

-- Sphere 型の定義
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
  , gen_coe :: Maybe String
  , hyouji :: Maybe String
  , orders2 :: Either Int String  -- 整数または文字列を格納する Either 型
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

data HomotopyGroupOfShpere = HomotopyGroupOfShpere { k :: Int, n :: Int }

data Order = Finite Int | Infinite
  deriving (Show, Eq)

type Tex = String
type ElTex = String

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
    htmlContent <- liftIO $ generateHtmlForSphere conn spheres
    let htmlString = T.unpack htmlContent
    -- 'template2.html' を読み込む
    templateContent <- liftIO $ TIO.readFile "static/template.html"
    -- プレースホルダーを実際の値で置換
    let queryRs = queryRows k n
    ords <- liftIO $ getOrders conn queryRs
    let ordsString = texToString $ ordersToGroupTex ords
    
    let groupList = ordersToGroupList ords
    gens <- liftIO $ getGenerators conn queryRs n
    -- gens <- liftIO $ getElTexs conn queryRs
    let groupGeneratorTex = intercalate "\\oplus " [gr ++ "\\{" ++ gen ++ "\\}" | (gr, gen) <- zip groupList gens]

    -- groups <- liftIO $ getGroups conn queryRs
    -- let groupString = texToString $ groupsToGroupTex groups

    let finalHtml = 
          T.replace "{k}" (T.pack $ show k) $
          T.replace "{n}" (T.pack $ show n) $
          T.replace "{nk}" (T.pack $ show (n+k)) $
          T.replace "{result}" (T.pack $ show result) $
          T.replace "{sphereList}" (T.pack $ htmlString) $
          T.replace "{group}" (T.pack $ "\\(" ++ groupGeneratorTex ++ "\\)") templateContent
          -- T.replace "{group}" (T.concat $ map T.pack gens) templateContent
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
    , "</ul>"
    , "</body></html>"
    ]

-- Spheres を HTML リストアイテムに変換する関数
spheresToHtml :: Connection -> [Sphere] -> IO T.Text
spheresToHtml conn spheres = do
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
    , "</table>"
    ]

sphereRowToHtml :: Connection -> Sphere -> IO T.Text
sphereRowToHtml conn (Sphere kk nn sId ord gene pp _ ee _ hh _ element _ hyouji ord2) = do
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
  dSum <- liftIO $ directSum conn queryCnt
  ords <- liftIO $ getOrders conn queryRs
  elTex <- liftIO $ elToTex conn nn elementList
  let -- この関数を使用して、ords を変換
    ordsString = texToString $ ordersToGroupTex ords
  return $ T.concat
    [ "<tr>"
    , "<th>", T.pack $ show sId, "</th>"
    , "<th>", T.pack $ showEither ord, "</th>"
    , "<th>", T.pack $ stripQuotes (fmap show gene), "</th>"
    , "<td>", T.pack $ stripQuotes (fmap show pp), "</td>"
    , "<td>", T.pack $ stripQuotes (fmap show ee), "</td>"
    , "<td>", T.pack $ stripQuotes (fmap show hh), "</td>"
    , "<td>", T.pack $ "\\(" ++ elTex ++ "\\)" , "</td>"
    -- , "<td>", T.concat ["\\(", latexText, "\\)", T.pack (show dSum), T.pack elTex] , "</td>"
    -- , "<td>", elemDimText , "</td>"
    , "</tr>"
    ]

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

listToString :: [Int] -> String
listToString xs = intercalate " " $ map show xs

orderToString :: Order -> String
orderToString (Finite x) = show x
orderToString Infinite   = "Inf"

ordersToString :: Either String [Order] -> String
ordersToString (Left str)   = str
ordersToString (Right ords) = concat $ map orderToString ords
-- ordersToString (Right ords) = listToString $ map orderToString ords


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

elDimList :: Connection -> Int -> [Int] -> IO [Int]
elDimList conn n elList = foldM accumulate [n] elList
  where
    accumulate acc elem = do
      kRows <- query conn "SELECT k FROM gen WHERE id = ?" (Only elem) :: IO [Only Int]
      let newSum = case kRows of
            (Only kRow:_) -> last acc + kRow
            []            -> last acc
      return (acc ++ [newSum])

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

elSqList :: [Int] -> [Int]
elSqList elList =
  concat [[length groupLi] ++ replicate ((length groupLi)-1) 0 | groupLi <- groupList]
  where
    groupList = group elList


elToTex :: Connection -> Int -> [Int] -> IO ElTex
elToTex conn nn elementList = do
  latexList    <- liftIO $ getLatexList conn elementList
  elemDimList  <- liftIO $ elDimList conn nn elementList
  elemSusList  <- liftIO $ elSusList conn nn elementList
  elemKindList <- liftIO $ elKindList conn elementList
  let elemSqList = elSqList elementList
  let texList = zip5 latexList elemDimList elemSusList elemKindList elemSqList
  return $ intercalate "" (map makeTex texList)

makeTex :: (String, Int, Int, Int, Int) -> String
makeTex (latex, dim, sus, kind, sq) = case kind of
  0 -> texKindZero latex dim sq
  1 -> texKindOne latex sus
  _ -> "Not defined"

texKindZero :: String -> Int -> Int -> String
texKindZero latex dim sq
  | sq == 0   = ""
  | otherwise = latexFormat latex dim sq

latexFormat :: String -> Int -> Int -> String
latexFormat latex dim sq
  | sq == 1 && last latex == ',' = latex ++ show dim ++ "}"
  | sq == 1 && last latex /= ',' = latex ++ "_{" ++ show dim ++ "}"
  | last latex == ','            = latex ++ show dim ++ "}^{" ++ show sq ++ "}"
  | otherwise                    = latex ++ "_{" ++ show dim ++ "}^{" ++ show sq ++ "}"

texKindOne :: String -> Int -> String
texKindOne latex sus
  | sus == 0  = latex
  | sus == 1  = "E " ++ latex
  | otherwise = "E^{" ++ show sus ++ "}" ++ latex

-- elToTex :: Connection -> Int -> [Int] -> IO ElTex
-- elToTex conn nn elementList = do
--   -- let elementList = stringToList element
--   latexList <- liftIO $ getLatexList conn elementList
--   elemDimList <- liftIO $ elDimList conn nn elementList
--   elemSusList <- liftIO $ elSusList conn nn elementList
--   elemKindList <- liftIO $ elKindList conn elementList
--   let
--     elemSqList = elSqList elementList
--     makeTex latex dim sus kind sq
--       | kind == 0 =
--         if sq == 0
--           then ""
--           else if sq == 1
--             then if last latex == ','
--               then latex ++ show dim ++ "}"
--               else latex ++ "_{" ++ show dim ++ "}"
--             else if last latex == ','
--               then latex ++ show dim ++ "}^{" ++ show sq ++ "}" 
--               else latex ++ "_{" ++ show dim ++ "}^{" ++ show sq ++ "}"
--       | kind == 1 =
--         case sus of
--           0 -> latex
--           1 -> "E " ++ latex
--           _ -> "E^{" ++ show sus ++ "}" ++ latex
--       | otherwise = "Not defined"
--     tmpList = [makeTex latex dim sus kind sq | (latex,dim,sus,kind,sq) <- zip5 latexList elemDimList elemSusList elemKindList elemSqList]
--     res = intercalate "" tmpList
--   return res



getGenerators :: Connection -> Query -> Int -> IO [ElTex]
getGenerators conn queryRs nn = do
  rows <- query_ conn queryRs :: IO [Sphere]
  mapM (convertToTex conn nn) rows

convertToTex :: Connection -> Int -> Sphere -> IO ElTex
convertToTex conn nn sphere = elToTex conn nn $ stringToList (element sphere)
-- convertToTex conn nn sphere = case element sphere of
--   elToTex conn nn $ stringToList element


piTex :: HomotopyGroupOfShpere -> String
piTex mt = "\\pi_{" ++ show (n mt + k mt) ++ "}^{" ++ show (n mt) ++ "}"

directSum :: Connection -> Query -> IO Int
directSum conn queryCount = do
  rows <- query_ conn queryCount :: IO [Only Int]
  case rows of
    (Only count:_) -> return count
    []             -> return 0  -- もし結果がない場合は0を返す

queryRows :: Int -> Int -> Query
queryRows k n
  | k <= -1   = stringToQuery "SELECT * FROM sphere WHERE n = 0 AND k = -1"
  | k+2 >= n  = stringToQuery $ "SELECT * FROM sphere WHERE n = " ++ show n ++ " AND k = " ++ show k
  | otherwise = stringToQuery $ "SELECT * FROM sphere WHERE n = " ++ show (k+2) ++ " AND k = " ++ show k

queryCount :: Int -> Int -> Query
queryCount k n
  | k <= -1   = stringToQuery "SELECT COUNT(*) FROM sphere WHERE n = 0 AND k = -1"
  | k+2 >= n  = stringToQuery $ "SELECT COUNT(*) FROM sphere WHERE n = " ++ show n ++ " AND k = " ++ show k
  | otherwise = stringToQuery $ "SELECT COUNT(*) FROM sphere WHERE n = " ++ show (k+2) ++ " AND k = " ++ show k

stringToQuery :: String -> Query
stringToQuery str = Query $ T.pack str


convertToOrder :: Sphere -> Order
convertToOrder sphere = case orders sphere of
  Left n -> Finite n
  Right "inf" -> Infinite
  Right _ -> Finite 0 -- 文字列が "inf" でない場合のデフォルト処理

-- データベースからOrderのリストを取得
getOrders :: Connection -> Query -> IO (Either String [Order])
getOrders conn query = catch (do
    rows <- query_ conn query :: IO [Sphere]
    let orders = map convertToOrder rows
    return $ Right orders
  ) handler
  where
    handler :: SomeException -> IO (Either String [Order])
    handler err = return $ Left $ "Error: " ++ show err



-- -- queryRs から ElTex のリストを得る。
-- getElTexs :: Connection -> Query -> IO (Either String [ElTex])
-- getElTexs conn query = catch (do
--     rows <- query_ conn query :: IO [Sphere]
--     let elTex = map convertToTex rows
--     return $ Right elTex
--   ) handler
--   where
--     handler :: SomeException -> IO (Either String [ElTex])
--     handler err = return $ Left $ "Error: " ++ show err


-- getOrders :: Connection -> Query -> IO (Either String [Order])
-- getOrders conn query = catch (do
--     rows <- query_ conn query :: IO [Sphere]
--     let orders = map convertToOrder rows
--     return $ Right orders
--   ) handler
--   where
--     convertToOrder row = case row of
--       n -> Finite (fromIntegral n)
--       "inf" -> Infinite
--       _ -> Finite 0 -- 適切なデフォルト値またはエラー処理
--     handler :: SomeException -> IO (Either String [Order])
--     handler e = return $ Left $ "Error: " ++ show e



-- elDimList :: Connection -> Int -> [Int] -> IO [Int]
-- elDimList conn n elList = do
--   res <- forM elList $ \elem -> do
--     genRows <- query conn "SELECT * FROM gen WHERE id = ?" (Only elem) :: IO [Gen]
--     return $ case genRows of
--       (genRow:_) -> n + genK genRow
--       []         -> n
--   return $ n : res

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

