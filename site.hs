{-# LANGUAGE OverloadedStrings  #-}

import Control.Applicative

import System.Locale (defaultTimeLocale)

import Data.Monoid
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Data.List (isPrefixOf)
import qualified Data.Map as M

import qualified Text.Pandoc.Options as O
import Text.Highlighting.Kate.Styles (pygments)

import Hakyll

siteRoot :: String
siteRoot = "http://none.io"


main :: IO ()
main 
    = hakyllWith config $ do

    tags <- buildTags "posts/*.md" $ fromCapture "tags/*.html"

    tagsRules tags $ compileTags tags

    -- read templates
    match "templates/*" $ compile templateCompiler

    -- copy static images
    match (  "static/*.png" 
        .||. "static/*.svg"
        .||. "static/*.gif"
        .||. "g/*/*"
        .||. "robots.txt") $ do
        route idRoute
        compile copyFileCompiler

    -- handle images
    match "img/*" $ do
        route idRoute
        compile copyFileCompiler

    -- handle css
    match "static/css/*.css" $ do
        route idRoute
        compile compressCssCompiler

    -- handle js
    match "static/js/*.js" $ do
        route idRoute
        compile copyFileCompiler


    -- handle posts
    match "posts/*.md" $ do
        route $ setExtension ".html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions 
            >>= saveSnapshot "posts"
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= baseTemplate
            >>= relativizeUrls

    -- handle static pages
    match (fromList staticPages) $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= baseTemplate
            >>= relativizeUrls

    create ["404.html"] $ do
        route $ idRoute
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/404.html" defaultContext
            >>= baseTemplate
            >>= absoluteUrls siteRoot

    -- create index page
    create ["index.html"] $ do
        route idRoute
        compile $ makeItem "" 
            >>= loadAndApplyTemplate "templates/index.html" (indexCtx  tags)
            >>= baseTemplate
            >>= relativizeUrls

    -- create rss feed
    create ["feed.xml"] $ do
        route idRoute
        compile $ loadAllSnapshots "posts/*.md" "posts"
            >>= renderRss feedConfig defaultContext
    
    -- create sitemap.xml
    create ["sitemap.xml"] $ do
        route idRoute
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/sitemap.xml" (sitemapCtx tags)
    where
        staticPages = ["notice.md", "about.md"]


baseTemplate :: Item String -> Compiler (Item String)
baseTemplate
    = loadAndApplyTemplate "templates/base.html" defaultContext


absoluteUrls :: String -> Item String -> Compiler (Item String)
absoluteUrls
    root = return . fmap (relativizeUrlsWith root)

compileTags :: Tags -> String -> Pattern -> Rules ()
compileTags
    tags tag _ = do
        route idRoute
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/category.html" tagsCtx
            >>= loadAndApplyTemplate "templates/base.html" tagsCtx
            >>= relativizeUrls
        where
            tagsCtx = constField "title" ("Tagged with " ++ tag) 
                   <> listField "posts" (postCtx tags) (take 10 <$> (recentFirst =<< loadAll "posts/*.md"))
                   <> defaultContext

sitemapCtx :: Tags -> Context String
sitemapCtx
    tags = defaultContext
    <> listField "posts" (postCtx tags) (recentFirst =<< loadAll "posts/*.md")
    <> nowField "created" "%Y.%m.%d"

nowField :: String -> String -> Context String
nowField
    key fmt = field key $ \_ -> unsafeCompiler $ (formatTime defaultTimeLocale fmt <$> getCurrentTime)


indexCtx :: Tags -> Context String
indexCtx 
    tags = constField "title" "HOME"
        <> listField "posts" (postCtx tags) (take 5 <$> (recentFirst =<< loadAll "posts/*.md"))
        <> modificationTimeField "mod" "%Y.%m.%d"
        <> defaultContext


postCtx :: Tags -> Context String
postCtx 
    tags = tagsField "tags" tags
        <> dateField "date" "%B %d, %Y"
        <> modificationTimeField "mod" "%Y.%m.%d"
        <> field "math" mathjax
        <> defaultContext

config :: Configuration
config 
    = defaultConfiguration
        { deployCommand = "rsync -avz -e ssh ./_site/ none.io:/home/felixsch/html/"
        }

feedConfig :: FeedConfiguration
feedConfig
    = FeedConfiguration 
    { feedTitle         = "none.io - What ever comes to mind"
    , feedDescription   = "Blog of felixsch"
    , feedAuthorName    = "Felix S."
    , feedAuthorEmail   = "felix@none.io"
    , feedRoot          = "http://none.io"
    }

mathjax :: Item String -> Compiler String
mathjax
    item = do
        metadata <- getMetadata (itemIdentifier item)
        return $ case M.lookup "math" metadata of
            Just "true" -> script
            otherwise -> ""
    where
      script = "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" />"
        

pandocOptions :: O.WriterOptions
pandocOptions = defaultHakyllWriterOptions
    { O.writerHtml5 = True
    , O.writerHtmlQTags = True
    , O.writerSectionDivs = True
    , O.writerTableOfContents = True
    , O.writerHighlight = True
    , O.writerHighlightStyle = pygments
    , O.writerExtensions = O.githubMarkdownExtensions
    }
