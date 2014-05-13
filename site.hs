{-# LANGUAGE OverloadedStrings  #-}

import System.IO
import System.Directory
import System.FilePath
import Control.Applicative
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Text.Pandoc.Options as O
import Text.Highlighting.Kate.Styles (pygments)
import Hakyll

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
        .||. "posts/*.png"
        .||. "g/*/*"
        .||. "posts/*.svg") $ do
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
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls

    -- handle static pages
    match (fromList staticPages) $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= loadAndApplyTemplate "templates/base.html" (indexCtx  tags)
            >>= relativizeUrls

    -- create index page
    create ["index.html"] $ do
        route idRoute
        compile $ makeItem "" 
            >>= loadAndApplyTemplate "templates/index.html" (indexCtx  tags)
            >>= loadAndApplyTemplate "templates/base.html" (indexCtx  tags)
            >>= relativizeUrls

    -- create rss feed
    create ["feed.xml"] $ do
        route idRoute
        compile $ loadAllSnapshots "posts/*" "posts"
            >>= renderRss feedConfig defaultContext
    where
        staticPages = ["notice.md", "about.md"]


compileTags :: Tags -> String -> Pattern -> Rules ()
compileTags
    tags tag pattern = do
        route idRoute
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/category.html" tagsCtx
            >>= loadAndApplyTemplate "templates/base.html" tagsCtx
            >>= relativizeUrls
        where
            title = "Tagged with " ++ tag
            tagsCtx = constField "title" ("Tagged with " ++ tag) 
                   <> listField "posts" (postCtx tags) (take 10 <$> (recentFirst =<< loadAll "posts/*.md"))
                   <> defaultContext

indexCtx :: Tags -> Context String
indexCtx 
    tags = constField "title" "HOME"
        <> listField "posts" (postCtx tags) (take 5 <$> (recentFirst =<< loadAll "posts/*.md"))
        <> defaultContext


postCtx :: Tags -> Context String
postCtx 
    tags = tagsField "tags" tags
        <> dateField "date" "%B %d, %Y"
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
