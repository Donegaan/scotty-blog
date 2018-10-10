{-# LANGUAGE OverloadedStrings #-}
-- Use records for blog posts
import Web.Scotty

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R

import Data.Text.Lazy

main = scotty 3000 $ do
  get "/" $ do
    S.html $ homePage

  get "/login" $ do
    S.html $ loginPage

--   get (literal "/greet/") $ do -- literal matches the exact url string
--       html $ "Oh, wow!"

--   get "/greet/:name" $ do
--       name <- param "name"
--       html $ longresponse name

-- response :: Text -> Text
-- response n = do R.renderHtml $ do
--                   H.h1 ( "Hello " >> H.toHtml n)

-- longresponse :: Text -> Text
-- longresponse n = do
--   R.renderHtml $ do
--     H.head $ H.title "Welcome page"
--     H.body $ do
--       H.h1 "Welcome!"
--       H.p ("Welcome to my Scotty app, " >> H.toHtml n)

homePage :: Text
homePage = do
  R.renderHtml $ do
    H.head $ H.title "Andrew's Blog"
    H.body $ do
      H.h1 "Hi to blog"

loginPage :: Text
loginPage = do
  R.renderHtml $ do
    H.head $ H.title "Login to Andrew' Blog"
    H.body $ do
      H.div H.! A.class_ "form" $ do
        H.input H.! A.type_ "text"
        -- H.button H.! A.type_ "button"
      -- H.a "Sign in"
  
-- newEntry :: Text