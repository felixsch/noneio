{-# LANGUAGE OverloadedStrings  #-}

import Control.Applicative
import Data.Monoid

import Hakyll

import System.IO


main :: IO ()
main 
    = hakyllWith config $ do

    tags <- buildTags "posts/*.md" $ fromCapture "tags/*.html"

    tagsRules tags $ compileTags tags

    -- read templates
    match "templates/*" $ compile templateCompiler

    -- copy static images
    match ("static/*.png" .||. "static/*.svg") $ do
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
        compile $ pandocCompiler 
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ makeItem "" 
            >>= loadAndApplyTemplate "templates/index.html" (indexCtx tags)
            >>= loadAndApplyTemplate "templates/base.html" (indexCtx tags)
            >>= relativizeUrls

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
        <> defaultContext

config :: Configuration
config 
    = defaultConfiguration

