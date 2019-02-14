--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Data.Maybe (isJust)
import           Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import           Data.Time.Clock (UTCTime)
import           Control.Applicative (Alternative (..))
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

    matchMetadata "posts/*" isPublished $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (fullPostCtx tags)
            >>= saveSnapshot "styledPost"
            >>= loadAndApplyTemplate "templates/base.html" (fullPostCtx tags)
            >>= relativizeUrls

    -- get unchanged versions too for rendering the post list
    matchMetadata "posts/*" isPublished $ version "plain" $ do
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
    maybeDateField "updated" "updated" "%e %b, %Y" <>
    dateField "published" "%e %b, %Y" <>
    defaultContext

postListCtx :: Context String
postListCtx =
    listField "allPosts" plainPostCtx (recentFirst =<< (loadAll $ "posts/*" .&&. hasVersion "plain"))

fullPostCtx :: Tags -> Context String
fullPostCtx tags =
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

-- optional field setup for the update field

maybeField :: String -> (Item a -> Compiler (Maybe String)) -> Context a
maybeField key f = field key $ \i -> do
    ms <- f i
    maybe empty return ms

maybeMetaField :: String -> String -> (String -> Maybe (String)) -> Context a
maybeMetaField key metakey f = maybeField key $ \i -> do
    mv <- getMetadataField (itemIdentifier i) metakey
    return (mv >>= f)

maybeDateField :: String -> String -> String -> Context a
maybeDateField key metakey format = maybeMetaField key metakey $ \v -> do
    parsed <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d" v :: Maybe UTCTime
    return $ formatTime defaultTimeLocale format parsed

--------------------------------- Utility functions

isPublished :: Metadata -> Bool
isPublished = isJust . lookupString "published"
