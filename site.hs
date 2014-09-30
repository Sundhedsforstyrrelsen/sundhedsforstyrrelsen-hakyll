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
      >>= applyAsTemplate imgurImgField
      >>= loadAndApplyTemplate "templates/post.html" postCtx
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
postCtx = mconcat [ dateField "date" "%B %e, %Y"
                  ] <> defaultContext

imgurImgField :: Context String
imgurImgField = functionField "imgurImg" $ \a _ -> case a of
  [i] -> return $ "<a href=\"http://i.imgur.com/"
               <> i
               <> ".png\"><img src=\""
               <> "http://i.imgur.com/"
               <> i
               <> "m.png\" /></a>"
  _   -> fail "Wrong args for imgurImg"
