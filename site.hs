--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Hakyll
import           Text.Pandoc.Options

--------------------------------------------------------------------------------


pandoc5Compiler = pandocCompilerWith
                  defaultHakyllReaderOptions
                  defaultHakyllWriterOptions { writerHtml5 = True }


main :: IO ()
main = hakyll $ do
  match "CNAME" $ do
    route   idRoute
    compile copyFileCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandoc5Compiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= relativizeUrls

  match "intro.md" $ compile pandoc5Compiler

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      intro <- loadBody "intro.md"

      let indexCtx = mconcat [ listField  "posts" postCtx (return posts)
                             , constField "intro" intro
                             ] <> defaultContext

      getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

  match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
