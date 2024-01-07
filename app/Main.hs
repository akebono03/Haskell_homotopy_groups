{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Lib
-- main :: IO ()
-- main = someFunc

import Web.Spock
import Web.Spock.Config
-- import Web.Spock hiding (get)
-- import Web.Spock (get)

import Network.Wai.Middleware.Static (staticPolicy, addBase)
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Text.Read (readMaybe)
-- import Database.Persist.Sqlite

-- メインのSpockアプリケーション
app :: SpockM () () () ()
-- app :: SpockM SqlBackend () ()
app = do
  middleware (staticPolicy (addBase "static"))  -- 静的ファイルのミドルウェア

  Web.Spock.get root $ do
    -- テンプレートファイルを読み込み、レスポンスに埋め込む
    templateContent <- liftIO $ TIO.readFile "static/template.html"
    html templateContent
  post "calculate" $ do
    formData <- params
    let n = maybe 0 (readInt . T.unpack) (lookup "n" formData)
        k = maybe 0 (readInt . T.unpack) (lookup "k" formData)
        result = n + k
    html $ T.pack $ "n + k = " <> (show result)      
    -- file "static/template.html"
    -- templateContent <- liftIO $ TIO.readFile "static/template.html"
    -- html templateContent

-- 安全な整数変換関数
readInt :: String -> Int
readInt s = maybe 0 id (readMaybe s :: Maybe Int)

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()  -- Spockアプリケーションの設定
  runSpock 8080 (spock spockCfg app)

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

