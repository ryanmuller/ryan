--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid                   (mappend, mconcat)
import           Data.Maybe
import           Data.Char
import qualified Data.Map                      as M
import           Data.List                     (intercalate)
import           Control.Monad
import           System.Directory
import           System.FilePath               (takeBaseName, takeFileName)
import           Hakyll
import           Text.Pandoc                   (bottomUp, Inline(..), Pandoc(..))


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "pages/*" $ do
        route $ setExtension "html"
        compile $ 
            pandocCompiler
--pandocCompilerWithTransform defaultHakyllReaderOptions  defaultHakyllWriterOptions  pandocTransform
            >>= loadAndApplyTemplate "templates/page.html"    pageCtx
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

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

pageCtx :: Context String
pageCtx = mconcat
  [ field "title" $ return . itemTitle
  , field "body" addReferences
  , defaultContext
  ]

itemTitle :: Item a -> String
itemTitle = spacify . capitalize . takeBaseName . toFilePath . itemIdentifier

itemReference :: Item a -> String
itemReference item = "[" ++ title ++ "] <" ++ title ++ ".html>"
  where title = itemTitle item

fileReference :: FilePath -> String
fileReference filepath = "[" ++ title ++ "] <" ++ title ++ ".html>"
  where title = takeBaseName filepath 

capitalize :: String -> String
capitalize "" = ""
capitalize (l:ls) = toUpper l : ls

spacify :: String -> String
spacify = map (\l -> case l of
                       '_' -> ' '
                       x -> x)

--addReferences :: Item String -> Compiler String
--addReferences item = references >>= (itemBody item ++ "\n\n" ++ references)
--  where pages = getDirectoryContents "pages/*"
--        references = liftM (intercalate "\n" . map fileReference) pages

addReferences :: Item String -> Compiler String
addReferences item = liftM ((++) $ itemBody item ++ "\n\n") references
  where pages = loadAll ("pages/*" .&&. hasVersion "raw") :: Compiler [Item String]
        references = liftM (intercalate "\n" . map itemReference) pages
