module Hakyll.Web.Galleries
    ( Galleries(..)
    , getImages
    , buildGalleries
    , galleriesRules
    , imagesField
    , galleriesContext
    ) where

import Control.Applicative
import Control.Monad

import Data.Monoid
import qualified Data.Map as M

import Hakyll


data Galleries = Galleries
  { gaMap :: M.Map String [Identifier]
  , gaMakeId :: String -> Identifier
  , gaPattern :: Pattern
  , gaDependency :: Dependency
  }

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


galleriesContext :: Galleries -> (String -> Snapshot) -> Context a
galleriesContext gal mkSnapshot = mconcat $ map createField $ M.assocs $ gaMap gal
    where
        createField (name,_) = field name (html name)
        html n _ = itemBody <$> loadSnapshot (gaMakeId gal n) (mkSnapshot n)
