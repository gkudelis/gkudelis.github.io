--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
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
            >>= loadAndApplyTemplate "templates/post.html" (fullPostCtx tags)
            >>= saveSnapshot "styledPost"
            >>= loadAndApplyTemplate "templates/base.html" (fullPostCtx tags)
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

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots ("posts/*" .&&. hasNoVersion) "styledPost"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom feedConfiguration feedCtx

--------------------------------------------------------------------------------
plainPostCtx :: Context String
plainPostCtx =
    dateField "date" "%e %b, %Y" <>
    defaultContext

postListCtx :: Context String
postListCtx =
    listField "allPosts" plainPostCtx (recentFirst =<< (loadAll $ "posts/*" .&&. hasVersion "plain"))

taggedPostCtx :: Tags -> Context String
taggedPostCtx tags =
    tagsField "tags" tags <>
    plainPostCtx

fullPostCtx :: Tags -> Context String
fullPostCtx tags =
    taggedPostCtx tags <>
    postListCtx

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

feedCtx :: Context String
feedCtx =
    bodyField "description" <>
    plainPostCtx

--------------------------------- Feed setup

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "Blog of Giedrius Kudelis"
    , feedDescription = "This is a repository of my projects I find interesting enough to write up"
    , feedAuthorName = "Giedrius Kudelis"
    , feedAuthorEmail = "giedrius.kudelis@gmail.com"
    , feedRoot = "https://gkudelis.net"
    }
