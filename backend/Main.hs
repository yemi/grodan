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
import Text.Blaze.Html
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze

type API = "/" :> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectory "resources"

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn "Running on 8080 "
  run 8080 app
