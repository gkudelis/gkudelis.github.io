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
    match "posts/*" $ version "plain" $ compile $ pandocCompiler

    match "index.html" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/base.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
plainPostCtx :: Context String
plainPostCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

baseCtx :: Context String
baseCtx =
    listField "allPosts" plainPostCtx (recentFirst =<< (loadAll $ "posts/*" .&&. hasVersion "plain"))

postCtx :: Context String
postCtx =
    baseCtx <>
    plainPostCtx

indexCtx :: Context String
indexCtx =
    constField "title" "Home" <>
    baseCtx <>
    defaultContext
