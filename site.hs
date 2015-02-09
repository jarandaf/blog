--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>), mappend)
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import           Hakyll
import           System.FilePath.Posix (takeBaseName, takeDirectory, (</>))

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    -- Static files
    match ("images/*" .||. "css/*" .||. "components/**") staticBehaviour

    -- Markdown posts
    match "posts/*.md" postsBehavior

    -- About
    match "about.md" markdownBehavior

    -- Contact
    match "contact.md" markdownBehavior

    -- Homepage
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx = 
                  listField "posts" postCtx (return posts) <>
                  defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/boilerplate.html" indexCtx 
                >>= relativizeUrls

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
  route $ setExtension "html"
  compile $ do
    body <- getResourceBody
    identifier <- getUnderlying
    return $ renderPandoc body
    >>= loadAndApplyTemplate "templates/boilerplate.html" defaultContext
    >>= relativizeUrls
--------------------------------------------------------------------------------
--
-- Posts
postsBehavior :: Rules ()
postsBehavior = do
  route $ setExtension "html"
  compile $ do
    body <- getResourceBody
    identifier <- getUnderlying
    return $ renderPandoc body
    >>= loadAndApplyTemplate "templates/post.html" postCtx
    >>= loadAndApplyTemplate "templates/boilerplate.html" defaultContext
    >>= relativizeUrls
    


