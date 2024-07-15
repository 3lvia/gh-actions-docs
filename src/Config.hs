{-# LANGUAGE BlockArguments #-}

module Config where

import           Control.Monad      (when)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromMaybe)
import           System.Environment (getEnvironment)
import           System.Exit        (exitFailure)

data Config
    = Config
    { readmeFile    :: String
    , debug         :: Bool
    , ignoreHeaders :: [String]
    , ignoreFiles   :: [String]
    , runPrettier   :: Bool
    , noActions     :: Bool
    , noToc         :: Bool
    , noName        :: Bool
    , noDescription :: Bool
    , noInputs      :: Bool
    , noPermissions :: Bool
    , noUsage       :: Bool
    }
    deriving (Show)

getConfig :: IO Config
getConfig = do
    env <- getEnvironment

    let readmeFile'    = fromMaybe "README.md" $ lookup "README_FILE" env
    let debug'         = lookup "DEBUG" env == Just "true"
    let ignoreHeaders' = maybe [] (splitOn ",") $ lookup "IGNORE_HEADERS" env
    let ignoreFiles'   = maybe [] (splitOn ",") $ lookup "IGNORE_FILES" env
    let runPrettier'   = lookup "RUN_PRETTIER" env == Just "true"
    let noActions'     = lookup "NO_ACTIONS" env == Just "true"
    let noToc'         = lookup "NO_TOC" env == Just "true"
    let noName'        = lookup "NO_NAME" env == Just "true"
    let noDescription' = lookup "NO_DESCRIPTION" env == Just "true"
    let noInputs'      = lookup "NO_INPUTS" env == Just "true"
    let noPermissions' = lookup "NO_PERMISSIONS" env == Just "true"
    let noUsage'       = lookup "NO_USAGE" env == Just "true"

    when (noToc' && noActions') do
        putStrLn "Both NO_TOC and NO_ACTIONS are set to true, nothing to do."
        exitFailure

    return $ Config
                readmeFile'
                debug'
                ignoreHeaders'
                ignoreFiles'
                runPrettier'
                noActions'
                noToc'
                noName'
                noDescription'
                noInputs'
                noPermissions'
                noUsage'
