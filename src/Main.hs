{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Actions
import           Config
import           Control.Monad      (when)
import           Data.Either        (fromRight, isLeft, lefts, rights)
import           Data.Text          (isPrefixOf, pack)
import           Data.Yaml          (ParseException, decodeFileEither)
import           System.Exit        (exitFailure)
import           System.Process     (callCommand)
import           TableOfContents
import           Text.Megaparsec    (parse)
import           Text.Pretty.Simple (pPrint)

updateActions :: Config -> String -> IO String
updateActions config readme = do
    -- Parse action metadata from tags in README
    let actionMetadataListE = parse fromTextActionMetadataParser "" $ pack readme
    when (isLeft actionMetadataListE) do
        putStrLn $ "Error parsing action metadata from file '" ++ readmeFile config ++ "':"
        pPrint actionMetadataListE
        exitFailure

    let actionMetadataList =
            filter (\file -> path file `notElem` ignoreFiles config) $ fromRight [] actionMetadataListE
    when (debug config) do
        putStrLn "actionMetdataList:"
        pPrint actionMetadataList

    -- Parse YAML of the action files defined by action metadata
    parsedFilesE <- mapM (decodeFileEither . path) actionMetadataList :: IO [Either ParseException Action]
    when (any isLeft parsedFilesE) do
        putStrLn "Error parsing files:"
        mapM_ pPrint $ lefts parsedFilesE
        exitFailure

    let parsedFiles = rights parsedFilesE
    when (debug config) do
        putStrLn "Parsed files:\n"
        mapM_ pPrint parsedFiles

    let parsedFilesWithMetadata = zip actionMetadataList parsedFiles
    when (debug config) do
        putStrLn "parsedFilesWithMetadata:"
        pPrint parsedFilesWithMetadata

    -- Update README with action docs
    let readmeWithActions = foldl (replaceActionTagWithDocs config) readme parsedFilesWithMetadata
    if readmeWithActions /= readme
        then do
            putStrLn $ readmeFile config ++ " updated successfully with documentation for actions!"
        else do
            putStrLn "No new changes to action documentation, not updated."

    return readmeWithActions

updateToc :: Config -> String -> IO String
updateToc config readme = do
    let markdownHeaderLines =
            map (++ "\n") $
                filter (`notElem` ignoreHeaders config) $
                    filter (\x -> any (`isPrefixOf` pack x) ["# ", "## ", "### ", "#### "]) $
                        lines readme
    when (debug config) do
        putStrLn "markdownHeadersLines:"
        pPrint markdownHeaderLines

    let markdownHeaders = map (parse markdownHeaderParser "" . pack) markdownHeaderLines
    when (any isLeft markdownHeaders) do
        putStrLn "Error parsing markdown headers:"
        mapM_ pPrint markdownHeaders
        exitFailure
    when (debug config) do
        putStrLn "markdownHeaders:"
        pPrint markdownHeaders

    -- Generate table of contents
    let toc = markdownHeadersToTableOfContents $ rights markdownHeaders
    when (debug config) do
        putStrLn "toc:"
        pPrint toc

    -- Update README with table of contents
    let readmeWithToc = replaceTableOfContentsTagWithTableOfContents toc readme
    if readmeWithToc /= readme then do
        putStrLn $ readmeFile config ++ " updated successfully with table of contents!"
    else do
        putStrLn "No new changes to table of contents, not updated."

    return readmeWithToc

main :: IO ()
main = do
    config <- getConfig
    when (debug config) do
        putStrLn "\nDebug mode enabled\n"
        putStrLn "Config:"
        pPrint config

    readme <- readFile $ readmeFile config

    case generateMode config of
        GenerateBoth ->
            updateActions config readme >>= updateToc config >>= writeFile (readmeFile config)
        GenerateActions ->
            updateActions config readme >>= writeFile (readmeFile config)
        GenerateToc ->
            updateToc config readme >>= writeFile (readmeFile config)

    when (runPrettier config) $
        callCommand ("prettier --write --single-quote " ++ readmeFile config)
