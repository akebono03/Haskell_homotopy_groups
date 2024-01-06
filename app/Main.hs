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

-- メインのSpockアプリケーション
app :: SpockM () () () ()
app = do
  middleware (staticPolicy (addBase "static"))  -- 静的ファイルのミドルウェア

  get root $ do
    -- テンプレートファイルを読み込み、レスポンスに埋め込む
    templateContent <- liftIO $ TIO.readFile "static/template.html"
    html templateContent

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

