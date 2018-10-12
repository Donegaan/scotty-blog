{-# LANGUAGE OverloadedStrings #-}
-- Use records for blog posts
import Web.Scotty

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import Control.Monad.IO.Class (liftIO)

import Data.Text.Lazy

main = scotty 3000 $ do
  get "/" homePage

  get "/newblog" newBlogPage

-- getHead :: IO B.Html
-- getHead= do
--   head <- B.head $ do
--             B.title "Andrew's blog"
--             B.link B.! A.rel "stylesheet" B.! A.href "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
--   return head

homePage :: ActionM()
homePage = S.html $
    R.renderHtml $ do
      B.head $ B.title "Andrew's Blog"
      B.body $ do
        B.h1 "Welcome to my blog"
        B.form B.! A.action "/newblog" B.! A.formaction "get" $
          B.button B.! A.type_ "submit" $ "New Blog"


newBlogPage :: ActionM()
newBlogPage = S.html $
  R.renderHtml $ do
    B.head $ B.title "Make a new blog"
    B.body $ do
      B.form B.! A.action "/" B.! A.formmethod "get" B.! A.name "blogForm" $ do
        "Title"
        B.input B.! A.type_ "text" B.! A.name "blogTitle"
        B.input B.! A.type_ "submit"
      B.textarea B.! A.form "blogForm" B.! A.id "blogForm" B.! A.style "width: 640px; height: 260px" $ ""

-- newEntry :: Text
