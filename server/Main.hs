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
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze

type API = "static" :> Raw
      :<|> Get '[HTML] H.Html

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectory "static"
    :<|> return page

page :: H.Html
page =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.httpEquiv "x-ua-compatible" ! A.content "ie=edge"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.title "Grodan"
      H.script ! A.src "/static/dist/grodan.js" $ ""
      H.style "body { background: rgb(30, 58, 94); }"
    H.body $ do
      H.script "Elm.fullscreen(Elm.Grodan)"

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn "Running on 8080"
  run 8080 app
