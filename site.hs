--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/tag.html" (tagCtx tag pattern)
            >>= loadAndApplyTemplate "templates/base.html" (tagCtx tag pattern)
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= loadAndApplyTemplate "templates/base.html" (postCtx tags)
            >>= relativizeUrls

    -- get unchanged versions too for rendering the post list
    match "posts/*" $ version "plain" $ do
        route $ setExtension "html"
        compile $ getResourceBody

    match "index.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/base.html" indexCtx
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
plainPostCtx :: Context String
plainPostCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

postListCtx :: Context String
postListCtx =
    listField "allPosts" plainPostCtx (recentFirst =<< (loadAll $ "posts/*" .&&. hasVersion "plain"))

postCtx :: Tags -> Context String
postCtx tags =
    tagsField "tags" tags <>
    postListCtx <>
    plainPostCtx

indexCtx :: Context String
indexCtx =
    constField "title" "Home" <>
    postListCtx <>
    defaultContext

tagCtx :: String -> Pattern -> Context String
tagCtx tag pattern =
    constField "title" ("Posts tagged \"" ++ tag ++ "\"") <>
    listField "tagPosts" plainPostCtx (recentFirst =<< loadAll pattern) <>
    postListCtx <>
    defaultContext
