{-# LANGUAGE BlockArguments #-}

module Config where

import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromMaybe)
import           System.Environment (getEnvironment)
import           System.Exit        (exitFailure)


data GenerateMode
    = GenerateActions
    | GenerateToc
    | GenerateBoth
    deriving (Show)


getGenerateMode :: IO GenerateMode
getGenerateMode = do
    env <- getEnvironment

    let noActions' = lookup "NO_ACTIONS" env == Just "true"
    let noToc'     = lookup "NO_TOC" env == Just "true"

    case (noActions', noToc') of
        (True, True)   -> do
            putStrLn "Both NO_TOC and NO_ACTIONS are set to true, nothing to do."
            exitFailure
        (True, False)  -> return GenerateToc
        (False, True)  -> return GenerateActions
        (False, False) -> return GenerateBoth

data Config
    = Config
    { readmeFile    :: String
    , debug         :: Bool
    , ignoreHeaders :: [String]
    , ignoreFiles   :: [String]
    , runPrettier   :: Bool
    , generateMode  :: GenerateMode
    , noName        :: Bool
    , noDescription :: Bool
    , noInputs      :: Bool
    , noOutputs     :: Bool
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
    generateMode' <- getGenerateMode
    let noName'        = lookup "NO_NAME" env == Just "true"
    let noDescription' = lookup "NO_DESCRIPTION" env == Just "true"
    let noInputs'      = lookup "NO_INPUTS" env == Just "true"
    let noOutputs'     = lookup "NO_OUTPUTS" env == Just "true"
    let noPermissions' = lookup "NO_PERMISSIONS" env == Just "true"
    let noUsage'       = lookup "NO_USAGE" env == Just "true"


    return $ Config
                readmeFile'
                debug'
                ignoreHeaders'
                ignoreFiles'
                runPrettier'
                generateMode'
                noName'
                noDescription'
                noInputs'
                noOutputs'
                noPermissions'
                noUsage'
