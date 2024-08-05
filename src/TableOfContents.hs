{-# LANGUAGE OverloadedStrings #-}

module TableOfContents where

import           Data.Text            (Text, pack, replace, toLower, unpack)
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, anySingle, errorBundlePretty,
                                       manyTill, manyTill_, parse, skipManyTill,
                                       some, anySingle)
import           Text.Megaparsec.Char (char, newline, string)

type Parser = Parsec Void Text

data MarkdownHeader
    = MarkdownHeader
    { level :: Int
    , text  :: String
    }
    deriving (Show)

markdownHeadersToTableOfContents :: [MarkdownHeader] -> String
markdownHeadersToTableOfContents headers =
    markdownHeadersToTableOfContents' headers headers

markdownHeadersToTableOfContents' :: [MarkdownHeader] -> [MarkdownHeader] -> String
markdownHeadersToTableOfContents' xxs@(MarkdownHeader level' text' : xs) ys =
    replicate (2 * (level' - 1)) ' '
    ++ "- [" ++ text' ++ "]"
    ++ "(#" ++ slugify text' ++ headerIndexStr ++ ")\n"
    ++ markdownHeadersToTableOfContents' xs ys
  where
    headerIndexStr = if headerIndex == 0 then "" else "-" ++ show headerIndex
    headerIndex = countOccurences ys - countOccurences xxs
    countOccurences = length . filter ((== text') . text)
    slugify =
        unpack . toLower . replace " " "-" . pack . filter (`elem` [' ', '-'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])
markdownHeadersToTableOfContents' [] _ = ""

tocStartTag :: String
tocStartTag = "<!-- gh-actions-docs-toc-start -->"

tocEndTag :: String
tocEndTag = "<!-- gh-actions-docs-toc-end -->"

replaceTableOfContentsTagWithTableOfContents :: String -> String -> String
replaceTableOfContentsTagWithTableOfContents toc readme =
    case parse (skipManyTill anySingle tableOfContentsTagParser) "" (pack readme) of
        Right match' ->
            let toc' = tocStartTag ++ "\n" ++ toc ++ tocEndTag
             in unpack $ replace (pack match') (pack toc') (pack readme)
        Left err ->
            error $ errorBundlePretty err


markdownHeaderParser :: Parser MarkdownHeader
markdownHeaderParser = do
    level' <- length <$> some (char '#')
    _ <- char ' '
    text' <- anySingle `manyTill` newline
    return $ MarkdownHeader level' text'

tableOfContentsTagParser :: Parser String
tableOfContentsTagParser = do
    start <- skipManyTill anySingle $ string $ pack tocStartTag
    (between', end) <- manyTill_ anySingle $ string $ pack tocEndTag
    return $ unpack start ++ between' ++ unpack end
