--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>), mappend)
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import           Hakyll
import           System.FilePath.Posix (takeBaseName, takeDirectory, (</>), splitFileName)
import           Data.ByteString.Char8 (isInfixOf, pack)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

  -- Static files
  match ("images/*" .||. "css/*" .||. "js/*" .||. "components/**") staticBehaviour

  -- Markdown posts
  match "posts/*.md" postsBehavior

  -- About 
  match "about.md" markdownBehavior

  -- Homepage
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx = listField "posts" postCtx (return posts) <>
                     defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/boilerplate.html" indexCtx 
        >>= relativizeUrls
        >>= removeIndexHtml

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateCtx <> authorCtx <> defaultContext
--------------------------------------------------------------------------------
dateCtx :: Context String
dateCtx = dateField "date" "%B %e, %Y" <> defaultContext
--------------------------------------------------------------------------------
authorCtx :: Context String
authorCtx = field "author" $ \item -> do 
  metadata <- getMetadata (itemIdentifier item)
  return $ fromMaybe "Jordi Aranda" $ M.lookup "author" metadata
--------------------------------------------------------------------------------
--
-- Simply copy in the right place
staticBehaviour :: Rules ()
staticBehaviour = do
  route idRoute
  compile copyFileCompiler
--------------------------------------------------------------------------------
--
-- Markdown
markdownBehavior :: Rules ()
markdownBehavior = do
  route $ prettyRoute
  compile $ do
    body <- getResourceBody
    identifier <- getUnderlying
    return $ renderPandoc body
    >>= loadAndApplyTemplate "templates/boilerplate.html" defaultContext
    >>= relativizeUrls
    >>= removeIndexHtml
--------------------------------------------------------------------------------
--
-- Posts
postsBehavior :: Rules ()
postsBehavior = do
  route $ prettyRoute
  compile $ do
    body <- getResourceBody
    identifier <- getUnderlying
    return $ renderPandoc body
    >>= loadAndApplyTemplate "templates/post.html" postCtx
    >>= loadAndApplyTemplate "templates/boilerplate.html" defaultContext
    >>= relativizeUrls
    >>= removeIndexHtml
--------------------------------------------------------------------------------
--
-- Pretty routes: replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
prettyRoute :: Routes
prettyRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = 
      takeDirectory p </> takeBaseName p </> "index.html"
      where
        p = toFilePath ident  
--------------------------------------------------------------------------------
--
-- Replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr :: String -> String
    removeIndexStr url = case splitFileName url of 
        (dir, "index.html") | isLocal dir -> dir
        _                                 -> url 
        where isLocal uri = not (isInfixOf "://" (pack $ uri) && isInfixOf "components" (pack $ uri))        


