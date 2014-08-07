{-# LANGUAGE OverloadedStrings  #-}

import Control.Monad
import Control.Applicative

import System.Locale (defaultTimeLocale)



import qualified Data.Map as M
import Data.Monoid
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Data.List (isPrefixOf, intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Text.Pandoc.Options as O
import Text.Highlighting.Kate.Styles (pygments)

import Debug.Trace

import Hakyll

siteRoot = "http://none.io"
siteKeywords = "Programming, Haskell, C++, Felix, Schnizlein, Personal, Blog, Vegan, Food"
siteDescription = "Personal blog of Felix. Writing what ever comes to mind. Mainly about Haskell and other programming languages. But also about music, politics and cooking vegan food."

data Galleries = Galleries
  { gaMap :: M.Map String [Identifier]
  , gaMakeId :: String -> Identifier
  , gaPattern :: Pattern
  , gaDependency :: Dependency
  }


instance Show Galleries where
    show x = show $ gaMap x


getImages :: String -> Galleries -> Maybe [Identifier]
getImages g = M.lookup g . gaMap




buildGalleries :: MonadMetadata m => String -> (String -> Identifier) -> m Galleries
buildGalleries pattern makeId = makeGal =<< getMatches glob
        where
            glob        = fromGlob pattern
            parseAll    = map (parseIdentifier pattern)
            makeMap     = M.fromListWith (++)
            makeGal mat = return $ Galleries (makeMap $ parseAll mat) makeId glob (PatternDependency glob mat)

parseIdentifier :: String -> Identifier -> (String, [Identifier])
parseIdentifier p s = (name name', [s])
    where
        name [] = "default"
        name x  = init x
        name'   = removeFileName $ removePath $ toFilePath s

        path    = takeWhile (/= '*') p
        removePath = drop (length path)
        removeFileName   = reverse . dropWhile (/= '/') . reverse


galleriesRules :: Galleries -> (String -> [Identifier] -> Pattern -> Rules ()) -> Rules ()
galleriesRules gal rule = imageDeps >> (forM_ (M.assocs $ gaMap gal) $ \(name, identifiers) ->
    rulesExtraDependencies [gaDependency gal] $
        create [gaMakeId gal name] $ rule name identifiers (fromList identifiers))
    where
      imageDeps = rulesExtraDependencies [gaDependency gal] $ 
        match (gaPattern gal) $ do
            route idRoute
            compile copyFileCompiler

imagesField :: String -> String -> [Identifier] -> Context a
imagesField name gal idfs = listField name (constField "gallery" gal <> field "image" (return . itemBody)) (mapM (makeItem . toUrl . toFilePath) idfs)


galleryContext :: Galleries -> (String -> Snapshot) -> Context a
galleryContext gal mkSnapshot = mconcat $ map createField $ M.assocs $ gaMap gal
    where
        createField (name,_) = field name (html name)
        html n _ = itemBody <$> loadSnapshot (gaMakeId gal n) (mkSnapshot n)



main :: IO ()
main 
    = hakyllWith config $ do

    tags <- buildTags "posts/*.md" $ fromCapture "tags/*.html"
    galleries <- buildGalleries "gal/**" $ fromCapture "gallery/*.html"
    _ <- preprocess (print galleries)

    galleriesRules galleries $ \name images pattern -> do

        let ctx = imagesField "images" name images <> defaultContext <> constField "gal" name
        route idRoute
        compile $ makeItem name
            >>= loadAndApplyTemplate "templates/gallery.html" ctx
            >>= saveSnapshot ("gallery_" ++ name)
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls

    -- read templates
    match "templates/*" $ compile templateCompiler

    -- copy static images
    match (  "static/**"
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

    -- handle unpublished posts
    match "draft/*.md" $ do
        route $ setExtension ".html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions   
            >>= loadAndApplyTemplate "templates/post.html" (postCtx galleries tags)
            >>= loadAndApplyTemplate "templates/base.html" (postCtx galleries tags)
            >>= relativizeUrls

    -- handle posts
    match "posts/*.md" $ do
        route $ setExtension ".html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions 
            >>= saveSnapshot "posts"
            >>= loadAndApplyTemplate "templates/post.html" (postCtx galleries tags)
            >>= loadAndApplyTemplate "templates/base.html" (postCtx galleries tags)
            >>= relativizeUrls

    -- handle static pages
    match (fromList staticPages) $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls

    create ["404.html"] $ do
        route $ idRoute
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/404.html" defaultContext
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= absoluteUrls siteRoot

    -- create index page
    create ["index.html"] $ do
        route idRoute
        compile $ makeItem "" 
            >>= loadAndApplyTemplate "templates/index.html" (indexCtx galleries tags)
            >>= loadAndApplyTemplate "templates/base.html"  (indexCtx galleries tags)
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
            >>= loadAndApplyTemplate "templates/sitemap.xml" (sitemapCtx galleries tags)
    where
        staticPages = ["notice.md", "about.md"]


sitemapCtx :: Galleries -> Tags -> Context String
sitemapCtx
    gal tags = defaultContext
    <> listField "posts" (postCtx gal tags) (recentFirst =<< loadAll "posts/*.md")
    <> nowField "created" "%Y-%m-%d"


indexCtx :: Galleries -> Tags -> Context String
indexCtx 
    gal tags = defaultContext
        <> constField "title" "HOME"
        <> constField "keywords" siteKeywords
        <> constField "description" siteDescription 
        <> listField "posts" (postCtx gal tags) (take 5 <$> (recentFirst =<< loadAll "posts/*.md"))
        <> modificationTimeField "mod" "%Y-%m-%d"


postCtx :: Galleries -> Tags -> Context String
postCtx 
    gal tags = defaultContext
        <> tagsField "tags" tags
        <> (constField "keywords" $ tagList tags)
        <> dateField "date" "%B %d, %Y"
        <> dateField "created" "%Y-%m-%d"
        <> modificationTimeField "mod" "%Y-%m-%d"
        <> galleryContext gal ("gallery_" ++)
       

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
absoluteUrls
    root = return . fmap (relativizeUrlsWith root)


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
