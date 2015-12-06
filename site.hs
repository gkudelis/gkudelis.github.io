--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/base.html" postCtx
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

postCtx :: Context String
postCtx =
    postListCtx <>
    plainPostCtx

indexCtx :: Context String
indexCtx =
    constField "title" "Home" <>
    postListCtx <>
    defaultContext
