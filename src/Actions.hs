{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

-- This module contains the data types and functions for parsing action.yml files and "metadata tags" in README.md files.
-- The metadata tags are used to specify the path to the action.yml file, the owner, project, version, and permissions required for the action.
-- Between these metadata tags, the documentation generated from their corresponding action.yml is inserted.
--
-- The Action type represents all the fields in an action.yml file.
-- All types related to this type will be prefixed with `Action`.
--
-- The ActionMetadata type represents the metadata tag in a README.md file.
-- All types related to this type will be prefixed with `ActionMetadata`.

module Actions where

import           Config
import           Control.Monad        (void)
import           Data.Aeson.TH        (defaultOptions, deriveJSON,
                                       fieldLabelModifier, omitNothingFields)
import           Data.List            (intercalate)
import           Data.List.Split      (splitOn)
import           Data.Map             (Map, fromList, toList)
import           Data.Text            (Text, pack, replace, strip, unpack)
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, anySingle, eof,
                                       errorBundlePretty, many, manyTill,
                                       manyTill_, optional, parse, sepBy,
                                       skipManyTill, try, (<|>))
import           Text.Megaparsec.Char (char, newline, string)

type Parser = Parsec Void Text

data ActionInput
    = ActionInput
    { description        :: Maybe String
    , required           :: Maybe Bool
    , default'           :: Maybe String
    , deprecationMessage :: Maybe String
    }
    deriving (Show)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = filter (/= '\'')} ''ActionInput)

type ActionInputs = Map String ActionInput

newtype ActionOutput
    = ActionOutput
    { description :: String }
    deriving (Show)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = filter (/= '\'')} ''ActionOutput)

type ActionOutputs = Map String ActionOutput


-- This is the root node of an actions 'action.yml' file.

data Action
    = Action
    { name        :: String
    , author      :: Maybe String -- TODO: use this for something
    , description :: String
    , inputs      :: Maybe ActionInputs
    , outputs     :: Maybe ActionOutputs
    }
    deriving (Show)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = filter (/= '\'')} ''Action)

data ActionMetadataPermissionAccess
    = ReadAccess
    | WriteAccess
    deriving Eq

instance Read ActionMetadataPermissionAccess where
    readsPrec _ "read"  = [(ReadAccess, "")]
    readsPrec _ "write" = [(WriteAccess, "")]
    readsPrec _ _       = []

instance Show ActionMetadataPermissionAccess where
    show ReadAccess  = "read"
    show WriteAccess = "write"

type ActionMetadataPermissions = Map String ActionMetadataPermissionAccess


-- This is the metadata taken from the tag in the README.md file.

data ActionMetadata
    = ActionMetadata
    { path        :: String
    , owner       :: Maybe String
    , project     :: Maybe String
    , version     :: Maybe String
    , permissions :: Maybe ActionMetadataPermissions
    }
    deriving (Show, Eq)

toEnglishBool :: Bool -> String
toEnglishBool True  = "yes"
toEnglishBool False = "no"

replaceNewlinesWithSpaces :: String -> String
replaceNewlinesWithSpaces = unpack . strip . replace "\n" " " . pack

actionStartTagPrefix :: String
actionStartTagPrefix = "<!-- gh-actions-docs-start"

actionEndTag :: String
actionEndTag = "<!-- gh-actions-docs-end -->"

prettyPrintAction :: Config -> Action -> ActionMetadata -> String
prettyPrintAction config (Action name' _ description' inputs' outputs') actionMetadata =
    (if noName config then "" else "## " ++ name' ++ "\n\n") ++
    (if noDescription config then "" else description' ++ "\n\n") ++
    (if noInputs config then "" else prettyPrintInputs inputs') ++
    (if noOutputs config then "" else prettyPrintOutputs outputs') ++
    (if noPermissions config then "" else prettyPrintPermissions actionMetadata) ++
    (if noUsage config then "" else prettyPrintUsage name' inputs' actionMetadata)


prettyPrintInputs :: Maybe ActionInputs -> String
prettyPrintInputs (Just inputs') =
    "### Inputs\n" ++
    "|Name|Description|Required|Default|\n"
        ++ "|-|-|-|-|\n"
        ++ concatMap
            ( \(name', ActionInput description' required' default_ deprecationMessage') ->
                    "`" ++ name' ++ "`"
                    ++ "|"
                    ++ prettyPrintDeprecationMessage deprecationMessage'
                    ++ maybe "" replaceNewlinesWithSpaces description'
                    ++ "|"
                    ++ maybe "no" toEnglishBool required'
                    ++ "|"
                    ++ maybe "" (\def' -> "`" ++ def' ++ "`") default_
                    ++ "|\n"
            )
            (toList inputs')
        ++ "\n"
prettyPrintInputs _ = ""

prettyPrintDeprecationMessage :: Maybe String -> String
prettyPrintDeprecationMessage (Just deprecationMessage') =
    ":warning: **DEPRECATED**: _"
        ++ replaceNewlinesWithSpaces deprecationMessage'
        ++ "_ :warning:<br><br>"
prettyPrintDeprecationMessage Nothing = ""

prettyPrintOutputs :: Maybe ActionOutputs -> String
prettyPrintOutputs (Just outputs') =
    "### Outputs\n" ++
    "|Name|Description|\n"
        ++ "|-|-|\n"
        ++ concatMap
            ( \(name', ActionOutput des) ->
                "`" ++ name' ++ "`"
                    ++ "|"
                    ++ replaceNewlinesWithSpaces des
                    ++ "|\n"
            )
            (toList outputs')
        ++ "\n"
prettyPrintOutputs _ = ""

prettyPrintPermissions :: ActionMetadata -> String
prettyPrintPermissions (ActionMetadata _ _ _ _ (Just permissions')) =
    "### Permissions\n"
    ++ "This action requires the following base [permissions](https://docs.github.com/en/actions/using-jobs/assigning-permissions-to-jobs):\n"
    ++ concatMap
            ( \(name', access) ->
                "- `" ++ name' ++ ": " ++ show access ++ "`\n"
            )
            (toList permissions')
    ++ "\nMore permissions might be required depending on the inputs set, see the actions documentation for more information.\n"
prettyPrintPermissions _ = ""


prettyPrintUsage :: String -> Maybe ActionInputs -> ActionMetadata -> String
prettyPrintUsage name' inputs' (ActionMetadata path' (Just owner') (Just project') (Just version') _) =
    "### Usage\n"++
    "```yaml\n"
        ++ "- name: " ++ name' ++ "\n"
        ++ "  uses: "
        ++ owner' ++ "/" ++ project' ++ actionPathWithoutFile ++ "@" ++ version' ++ "\n"
        ++ prettyPrintUsageWith inputs'
        ++ "```\n"
    where
        actionPathWithoutFile = prependSlashIfNotEmpty $ intercalate "/" . init . splitOn "/" $ path'
        prependSlashIfNotEmpty "" = ""
        prependSlashIfNotEmpty x  = "/" ++ x
prettyPrintUsage _ _ _ = ""

prettyPrintUsageWith :: Maybe ActionInputs -> String
prettyPrintUsageWith (Just inputs') = "  with:\n" ++ concatMap (uncurry prettyPrintUsageInput) (toList inputs')
prettyPrintUsageWith Nothing = ""

prettyPrintUsageInput :: String -> ActionInput -> String
prettyPrintUsageInput _ (ActionInput _ _ _ (Just _)) = ""
prettyPrintUsageInput name' (ActionInput description' required' default_ Nothing) =
    indent
        ++ name'
        ++ ":\n"
        ++ ( case (description', required', default_) of
                (Just des', Just req', Just def') ->
                    formatDescription des' ++ formatRequired req' ++ formatDefault def'
                (Just des', Just req', _) ->
                    formatDescription des' ++ formatRequired req'
                (Just des', _, Just def') ->
                    formatDescription des' ++ formatDefault def'
                (Just des', _, _) ->
                    formatDescription des'
                _ ->
                    ""
           )
        ++ "\n"
  where
    indent = replicate 4 ' '
    formatDescription des' = indent ++ "# " ++ replaceNewlinesWithSpaces des' ++ "\n" ++ indent ++ "#\n"
    formatRequired req' = indent ++ "# Required: " ++ toEnglishBool req' ++ "\n"
    formatDefault def' = indent ++ "# Default: '" ++ def' ++ "'\n"

actionMetadataToString :: ActionMetadata -> String
actionMetadataToString (ActionMetadata path' owner' project' version' permissions') =
    actionStartTagPrefix
        ++ " path="
        ++ path'
        ++ maybe "" (" owner=" ++) owner'
        ++ maybe "" (" project=" ++) project'
        ++ maybe "" (" version=" ++) version'
        ++ maybe "" permissionsToString permissions'
        ++ " -->"

permissionsToString :: ActionMetadataPermissions -> String
permissionsToString permissions' =
    " permissions="
    ++ intercalate "," permissionStrList
    where
        permissionStrList =
            map
                (\(name', access) -> name' ++ ":" ++ show access)
                (toList permissions')

replaceActionTagWithDocs :: Config -> String -> (ActionMetadata, Action) -> String
replaceActionTagWithDocs config readme (meta, action) =
    case parse (skipManyTill anySingle (specificActionMetadataParser_ meta)) "" (pack readme) of
        Right "" ->
            readme
        Right match' ->
            let docs = actionMetadataToString meta ++ "\n" ++ prettyPrintAction config action meta ++ actionEndTag
             in unpack $ replace (pack match') (pack docs) (pack readme)
        Left err ->
            error $ errorBundlePretty err

actionMetadataParser :: Parser ActionMetadata
actionMetadataParser = do
    _ <- string $ pack actionStartTagPrefix
    _ <- string " path="
    path' <- manyTill anySingle (char ' ')
    owner' <- optional $ do
        _ <- string "owner="
        manyTill anySingle (char ' ')
    project' <- optional $ do
        _ <- string "project="
        manyTill anySingle (char ' ')
    version' <- optional $ do
        _ <- string "version="
        manyTill anySingle (char ' ')
    permissions' <- optional $ do
        _ <- string "permissions="
        permissions' <- permissionParser `sepBy` char ','
        _ <- char ' '
        return permissions'
    _ <- string "-->"
    _ <- skipManyTill anySingle $ string $ pack actionEndTag
    return $ ActionMetadata path' owner' project' version' (fromList <$> permissions')

permissionParser :: Parser (String, ActionMetadataPermissionAccess)
permissionParser = do
    name' <- manyTill anySingle (char ':')
    access' <- read . unpack <$> (string "read" <|> string "write")
    return (name', access')

fromTextActionMetadataParser :: Parser [ActionMetadata]
fromTextActionMetadataParser = do
    ps <- many $ try $ skipManyTill anySingle actionMetadataParser
    _ <- eof <|> void newline
    return ps

specificActionMetadataParser_ :: ActionMetadata -> Parser String
specificActionMetadataParser_ meta = do
    meta' <- string (pack $ actionMetadataToString meta)
    (between', end) <- anySingle `manyTill_` string (pack actionEndTag)
    return $ unpack meta' ++ between' ++ unpack end
