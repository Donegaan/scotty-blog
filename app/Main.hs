{-# LANGUAGE OverloadedStrings #-}
-- Use records for blog posts
import Web.Scotty

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text.Lazy as L
import Data.Time

fileName = "blogs.txt"

data Post = Post { -- record datatype
  postTitle :: String,
  postBody :: String
  -- ,postID :: Int
  -- ,postDate:: (Integer,Int,Int)
} deriving (Show,Read)

main = scotty 3000 $ do
  get "/" homePage

  get "/newblog" newBlogPage

  post "/newentry" newEntry

  get "/allblogs" allBlogs

  -- post request, can pick out the parameters such as title and body of blog

homePage :: ActionM()
homePage = S.html $
    R.renderHtml $ do
      B.head $ B.title "Andrew's Blog"
      B.body $ do
        B.h1 "Welcome to my blog"
        B.form B.! A.action "/newblog" B.! A.formaction "get"  $
          B.button B.! A.type_ "submit" $ "New Blog"
        B.form B.! A.action "/allblogs" B.! A.formaction "get"  $
          B.button B.! A.type_ "submit" $ "All Blogs"
        B.div $ do
          B.h1 "Blog Title 1"
          B.p "Blog body 1"


newBlogPage :: ActionM()
newBlogPage = S.html $
  R.renderHtml $ do
    B.head $ B.title "Make a new blog"
    B.body $
      B.form B.! A.action "/newentry" B.! A.method "post" B.! A.name "blogForm" B.! A.id "blogForm" $ do
        "Title: "
        B.input B.! A.type_ "text" B.! A.name "blogTitle"
        B.br
        B.textarea B.! A.form "blogForm" B.! A.name "blogBody" B.! A.style "width: 640px; height: 260px" $ ""
        B.br
        "Password: "
        B.input B.! A.type_ "password" B.! A.name "blogPass"
        B.button B.! A.type_ "submit" $ "Post Blog"

newEntry :: ActionM()
newEntry = do passWord <- S.param "blogPass" :: ActionM String
              if passWord == "pass" then do
                blogMain <- S.param "blogBody" :: ActionM String
                title <- S.param "blogTitle" :: ActionM String
                --TODO: Post ID = length of list, date: getCurrentTime and then format the date into String
                time <- liftIO getCurrentTime
                let (y,m,d) = toGregorian $ utctDay time -- to get date for post
                let newBlog = Post {postTitle = title, postBody = blogMain}
                liftIO $ storeBlog newBlog
                liftIO $ putStrLn "PASS WORKED "
              else
                liftIO $ putStrLn "NO PASS"
              S.redirect "/" -- Redirect to homepage to display blog posts
              -- let time = getCurrentTime :: IO UTCTime
              -- liftIO $ print $ time

storeBlog :: Post -> IO () -- Store newest blog in file at head of list
storeBlog newPost = do
  blogList <- getPosts
  -- print blogList
  writeFile fileName $ show $ newPost:blogList

getPosts :: IO [Post] -- Get all blog posts from text file
getPosts = do
  contents <- readFile fileName
  return $! (read contents :: [Post]) -- converts string contents to list of Posts


-- TODO: display on homepage up to 5, click on post and it displays on its own page
-- passwords for posting blog, view list of all blogs

allBlogs :: ActionM() -- Display all Blogs
allBlogs = S.html $
  R.renderHtml $
    B.body $
     B.h1 "All Blogs"
