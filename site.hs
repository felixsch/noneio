{-# LANGUAGE OverloadedStrings  #-}

import Control.Applicative

import System.Locale (defaultTimeLocale)

import qualified Data.Map as M
import Data.Monoid
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Data.List (isPrefixOf, intercalate)

import qualified Text.Pandoc.Options as O
import Text.Highlighting.Kate.Styles (pygments)


import Hakyll
import Hakyll.Web.Paginate

siteRoot = "http://none.io"
siteKeywords = "Programming, Haskell, C++, Felix, Schnizlein, Personal, Blog, Vegan, Food"
siteDescription = "Personal blog of Felix. Writing what ever comes to mind. Mainly about Haskell and other programming languages. But also about music, politics and cooking vegan food."


main :: IO ()
main = hakyllWith config $ do

    tags <- buildTags "posts/*.md" $ fromCapture "tags/*.html"
    pages <- buildPages "posts/*.md"

    -- read templates
    match "templates/*" $ compile templateCompiler

    -- copy static images
    match (  "static/**"
        .||. "robots.txt"
        .||. "img/*"
        .||. "static/js/*.js") $ do
        route idRoute
        compile copyFileCompiler

    -- handle css
    match "static/css/*.css" $ do
        route idRoute
        compile compressCssCompiler

    -- handle posts
    match "posts/*.md" $ do
        route $ setExtension ".html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions 
            >>= saveSnapshot "posts"
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= loadAndApplyTemplate "templates/base.html" (postCtx tags)
            >>= relativizeUrls

    -- handle static pages
    match (fromList staticPages) $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls


    -- 404.html uses absolute urls
    create ["404.html"] $ do
        route idRoute
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/404.html" defaultContext
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= absoluteUrls siteRoot


    paginateRules pages $ \index pattern -> do
        route $ setExtension "html"
        compile $ makeItem "" 
            >>= loadAndApplyTemplate "templates/index.html" (indexCtx index pages tags)
            >>= loadAndApplyTemplate "templates/base.html"  (indexCtx index pages tags)
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


sitemapCtx :: Tags -> Context String
sitemapCtx tags = defaultContext
    <> listField "posts" (postCtx tags) (recentFirst =<< loadAll "posts/*.md")
    <> nowField "created" "%Y-%m-%d"


indexCtx :: PageNumber -> Paginate -> Tags -> Context String
indexCtx i pages tags = defaultContext
        <> constField "title" "HOME"
        <> constField "keywords" siteKeywords
        <> constField "description" siteDescription 
        <> listField "posts" (postCtx tags) (takeFromTo start end <$> (recentFirst =<< loadAll "posts/*.md"))
        <> modificationTimeField "mod" "%Y-%m-%d"
        <> paginateContext pages i
  where
        start = 5 * (i -1)
        end   = 5 * i


postCtx :: Tags -> Context String
postCtx tags = defaultContext
        <> tagsField "tags" tags
        <> (constField "keywords" $ tagList tags)
        <> dateField "date" "%B %d, %Y"
        <> dateField "created" "%Y-%m-%d"
        <> modificationTimeField "mod" "%Y-%m-%d"
       

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


nowField :: String -> String -> Context String
nowField
    key fmt = field key $ \_ -> unsafeCompiler $ (formatTime defaultTimeLocale fmt <$> getCurrentTime)


tagList :: Tags -> String
tagList tags = intercalate "," $ map fst $ tagsMap tags


absoluteUrls :: String -> Item String -> Compiler (Item String)
absoluteUrls root = return . fmap (relativizeUrlsWith root)

buildPages :: (MonadMetadata m) => Pattern -> m Paginate
buildPages pattern = buildPaginateWith (return . paginateEvery 5) pattern $ \index ->
    if index == 1 
       then fromFilePath "index.html"
       else fromFilePath $ "index/p/" ++ show index ++ ".html"

takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo start end = drop start . take end

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
