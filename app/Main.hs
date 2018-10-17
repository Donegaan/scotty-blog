{-# LANGUAGE OverloadedStrings #-}
-- Use records for blog posts
import Web.Scotty

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import Control.Monad.IO.Class (liftIO)

import Data.Text.Lazy

import Data.Time.Clock
import Data.Time.Calendar

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay


data Post = Post { -- record datatype
  postTitle :: String,
  postBody :: String,
  postDate :: IO
} deriving (Show,Read)

main = scotty 3000 $ do
  get "/" homePage

  get "/newblog" newBlogPage

  post "/newentry" newEntry

  -- post request, can pick out the parameters such as title and body of blog



homePage :: ActionM()
homePage = S.html $
    R.renderHtml $ do
      B.head $ B.title "Andrew's Blog"
      B.body $ do
        B.h1 "Welcome to my blog"
        B.form B.! A.action "/newblog" B.! A.formaction "get"  $
          B.button B.! A.type_ "submit" $ "New Blog"


newBlogPage :: ActionM()
newBlogPage = S.html $
  R.renderHtml $ do
    B.head $ B.title "Make a new blog"
    B.body $
      B.form B.! A.action "/newentry" B.! A.method "post" B.! A.name "blogForm" B.! A.id "blogForm" $ do
        "Title"
        B.input B.! A.type_ "text" B.! A.name "blogTitle"
        B.textarea B.! A.form "blogForm" B.! A.name "blogBody" B.! A.style "width: 640px; height: 260px" $ ""
        B.input B.! A.type_ "submit"

newEntry :: ActionM()
newEntry = do blogBody <- S.param "blogBody" :: ActionM String
              title <- S.param "blogTitle" :: ActionM String
              let newBlog = Post {postTitle = title, postBody = blogBody, postDate = date}
              S.html $
                R.renderHtml $ do
                  B.head $ B.title $ B.toHtml title
                  B.body $
                    B.div $ do
                      B.h1 $ B.toHtml title
                      B.p $ B.toHtml date
              -- S.redirect "/" -- Redirect to homepage to display blog posts



  -- S.html $ R.renderHtml $ B.h1 title
