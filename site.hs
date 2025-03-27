--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid              (mappend)
import           Hakyll
import           Text.Pandoc.Highlighting (Style, haddock, styleToCss)
import           Text.Pandoc.Options      (ReaderOptions (..),
                                           WriterOptions (..))

--------------------------------------------------------------------------------
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Blog without Organs"
    , feedDescription = "Just a regular good ol fashioned blog feed"
    , feedAuthorName  = "Tsung-Ju Lii"
    , feedAuthorEmail = "usefulalgorithm@gmail.com"
    , feedRoot        = "http://usefulalgorithm.github.io"
    }

type FeedRenderer =
    FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
    renderer myFeedConfiguration feedCtx
        =<< fmap (take 10) . recentFirst
        =<< loadAllSnapshots "posts/*" "content"

config :: Configuration
config = defaultConfiguration {
    destinationDirectory = "docs"
}

pandocCodeStyle :: Style
pandocCodeStyle = haddock

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle   = Just pandocCodeStyle
      }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.hs" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

    match (fromList ["about.rst", "blogroll.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
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

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile (feedCompiler renderAtom)

    create ["rss.xml"] $ do
        route idRoute
        compile (feedCompiler renderRss)

    create ["css/syntax.css"] $ do
        route idRoute
        compile $ do
            makeItem $ styleToCss pandocCodeStyle

    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let title = "Posts tagged \"" ++ tag ++ "\""
                ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

feedCtx :: Context String
feedCtx = postCtx <> bodyField "description"
