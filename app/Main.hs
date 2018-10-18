{-# LANGUAGE OverloadedStrings #-}
-- Use records for blog posts
import Web.Scotty

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import Control.Monad.IO.Class (liftIO)

import Data.Text.Lazy
import Data.Time

fileName = "blogs.txt"

data Post = Post { -- record datatype
  postTitle :: String,
  postBody :: String
  -- ,postID :: Int
  ,postDate:: String
} deriving (Show,Read)

main = scotty 3000 $ do
  get "/" homePage

  get "/newblog" newBlogPage

  post "/newentry" newEntry

  get "/allblogs" allBlogs

  get "/blogs/:blogName" $ do
    blogName <- param "blogName"
    displaySingleBlog blogName

  -- post request, can pick out the parameters such as title and body of blog

homePage :: ActionM()
homePage = do
  posts <- liftIO getPosts :: ActionM [Post]
  S.html $
    R.renderHtml $ do
      B.head $ B.title "Andrew's Blog"
      B.body $ do
        B.h1 "Welcome to my blog"
        B.form B.! A.action "/newblog" B.! A.formaction "get"  $
          B.button B.! A.type_ "submit" $ "New Blog"
        B.form B.! A.action "/allblogs" B.! A.formaction "get"  $
          B.button B.! A.type_ "submit" $ "All Blogs"
        case Prelude.take 5 posts of -- Check if there are any posts saved.
                        [] -> B.p "No posts to display"
                        xs -> displayPosts xs

newBlogPage :: ActionM() -- Page to enter new post
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

newEntry :: ActionM() -- Save new blog post
newEntry = do passWord <- S.param "blogPass" :: ActionM String
              -- title <- S.param "blogTitle" :: ActionM String
              -- posts <- liftIO getPosts
              if passWord == "pass" then do
                blogMain <- S.param "blogBody" :: ActionM String
                title <- S.param "blogTitle" :: ActionM String
                time <- liftIO getCurrentTime
                currentDate <- let (y,m,d) = toGregorian $ utctDay time in return $ show d ++ "/" ++ show m ++ "/" ++ show y-- to get date for post
                let newBlog = Post {postTitle = title, postBody = blogMain, postDate = currentDate}
                liftIO $ storeBlog newBlog
                liftIO $ putStrLn "PASS WORKED "
              else
                liftIO $ putStrLn "NO PASS"
              S.redirect "/" -- Redirect to homepage to display blog posts

storeBlog :: Post -> IO () -- Store newest blog in file at head of list
storeBlog newPost = do
  blogList <- getPosts
  -- print blogList
  writeFile fileName $ show $ newPost:blogList

getPosts :: IO [Post] -- Get all blog posts from text file
getPosts = do
  contents <- readFile fileName
  return $! (read contents :: [Post]) -- converts string contents to list of Posts


-- TODO: click on post and it displays on its own page, no duplicate titles OR unique ID

displayPosts :: [Post] -> B.Html
displayPosts [] = B.p "" -- If no posts left in list
displayPosts (p:xs) = do
    B.div $ do
      B.h2 $
        B.a B.! A.href (B.stringValue  ("/blog/" ++ postTitle p)) $ B.toHtml (postTitle p)
      B.p $ B.toHtml (postDate p)
      B.p $ B.toHtml (postBody p)
    displayPosts xs -- call display posts again for next post

allBlogs :: ActionM() -- Display all saved Blogs
allBlogs = do
  posts <- liftIO getPosts
  S.html $
    R.renderHtml $
      B.body $ do
       B.h1 "All Blogs"
       case posts of -- Check if there are any posts saved.
                       [] -> B.p "No posts to display"
                       xs -> displayPosts xs

displaySingleBlog :: String -> ActionM()
displaySingleBlog blogName = do -- find the blog in text file
  posts <- liftIO getPosts
  S.html $ R.renderHtml $ displayPosts $ Prelude.filter (\x -> postTitle x == blogName) posts
