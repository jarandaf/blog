--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import            Control.Applicative ((<$>))
import            Control.Monad (forM)
import            Data.Monoid ((<>), mappend, mconcat)
import            Data.Maybe (fromMaybe, catMaybes)
import            Data.List (intercalate, intersperse)
import            Data.List.Utils
import qualified  Data.Map as M
import            Hakyll
import            System.FilePath.Posix (takeBaseName, takeDirectory, (</>), splitFileName)
import            Data.ByteString.Char8 (isInfixOf, pack)
import            Text.Blaze.Html (toHtml, toValue, (!))
import            Text.Blaze.Html.Renderer.String (renderHtml)
import qualified  Text.Blaze.Html5 as H
import qualified  Text.Blaze.Html5.Attributes as A
import            System.Posix.Files
import            Text.Pandoc.Options
import qualified  Data.Set as S
--------------------------------------------------------------------------------
main :: IO ()
main = do
  hakyll $ do

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Static files
    match ("images/*" .||. "css/*" .||. "components/**") staticBehaviour

    -- Slides
    match "slides/*" staticBehaviour

    -- Markdown posts
    match "posts/*.md" $ do
      route prettyRoute
      compile $ pandocMathCompiler
        >>= saveSnapshot "teaser"
        >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
        >>= loadAndApplyTemplate "templates/boilerplate.html" defaultContext
        >>= relativizeUrls
        >>= removeIndexHtml

    -- About
    match "about.md" $ do
      route prettyRoute
      compile $ pandocMathCompiler
        >>= loadAndApplyTemplate "templates/about.html" defaultContext
        >>= loadAndApplyTemplate "templates/boilerplate.html" defaultContext
        >>= relativizeUrls
        >>= removeIndexHtml

    -- Tags
    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route prettyRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title <>
                  listField "posts" (postCtx tags) (return posts) <>
                  defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/boilerplate.html" ctx
          >>= relativizeUrls
          >>= removeIndexHtml

    -- Homepage
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx = listField "posts" (postCtx tags) (return posts) <>
                       defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/boilerplate.html" indexCtx
          >>= relativizeUrls
          >>= removeIndexHtml

      match "templates/*" $ compile templateCompiler

  createSymbolicLink "components" "_site/components"
--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = dateCtx <> authorCtx <> teaserCtx <> (tagsCtx tags) <> defaultContext
--------------------------------------------------------------------------------
dateCtx :: Context String
dateCtx = dateField "date" "%B %e, %Y"
--------------------------------------------------------------------------------
authorCtx :: Context String
authorCtx = field "author" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ fromMaybe "Jordi Aranda" $ M.lookup "author" metadata
--------------------------------------------------------------------------------
teaserCtx :: Context String
teaserCtx = teaserField "teaser" "teaser"
--------------------------------------------------------------------------------
tagsCtx :: Tags -> Context String
tagsCtx tags = mapContext (replace ", " " | ") (tagsField "tags" tags)
--------------------------------------------------------------------------------
staticBehaviour = do
  route idRoute
  compile copyFileCompiler
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
        where isLocal uri = not (isInfixOf "://" (pack uri) && isInfixOf "components" (pack uri))
--------------------------------------------------------------------------------
--
-- Custom compiler with MathJax support
pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
  let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = foldr S.insert defaultExtensions mathExtensions
      writerOptions = defaultHakyllWriterOptions {
        writerExtensions = newExtensions,
        writerHTMLMathMethod = MathJax ""
      }
  in pandocCompilerWith defaultHakyllReaderOptions writerOptions
