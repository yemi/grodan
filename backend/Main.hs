{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Foldable (foldMap)
import GHC.Generics
import Text.Blaze.Html5 hiding (main)
import qualified Text.Blaze.Html5 as H
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze

data Page = Page

instance ToMarkup Page where
  toMarkup _ = 
    H.docTypeHtml $ do 
      H.body $ do
        H.h1 "Hello world!"

type API = "test" :> Get '[HTML] Page

api :: Proxy API
api = Proxy

server :: Server API
server = return Page

app :: Application
app = serve api server

--main :: IO ()
main = do
  putStrLn "Running on 8081 "
  run 8081 app
