{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Time as Time
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)

import qualified Article as ARTCL
import qualified Site as SITE

main :: IO ()
main = do
    -- Site metadata
    let site = SITE.Site
          { SITE.slug  = "index.html"
          , SITE.name  = "fugux"
          , SITE.email = Just "jg@fugu.mail"
          , SITE.links = [ "https://github.com/jgtux"
                         ]
          }

    -- Get today's date
    currentDate <- Time.utctDay <$> Time.getCurrentTime

    -- Collect Markdown files from "content/"
    files <- listDirectory "content"
    let mdFiles = [ "content" </> f | f <- files, takeExtension f == ".md" ]

    -- Convert all Markdown → Article
    articlesE <- mapM (ARTCL.saveArticleHTML currentDate) mdFiles

    -- Filter successful conversions
    let articles = [a | Right a <- articlesE]

    -- Generate index.html
    SITE.genArticleMenu site articles

    putStrLn "✅ Site generated in ./main and ./articles"
