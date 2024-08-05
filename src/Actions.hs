{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Actions where

import           Config
import           Control.Monad        (void)
import           Data.Aeson.TH        (defaultOptions, deriveJSON,
                                       fieldLabelModifier, omitNothingFields)
import           Data.List            (intercalate)
import           Data.List.Split      (splitOn)
import           Data.Map             (Map, fromList, toList)
import           Data.Text            (Text, pack, replace, unpack)
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, anySingle, eof,
                                       errorBundlePretty, many, manyTill,
                                       manyTill_, optional, parse, sepBy,
                                       skipManyTill, try, (<|>), anySingle)
import           Text.Megaparsec.Char (char, newline, string)

type Parser = Parsec Void Text

data ActionInput
    = ActionInput
    { description :: Maybe String
    , required    :: Maybe Bool
    , default'    :: Maybe String
    }
    deriving (Show)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = filter (/= '\'')} ''ActionInput)

type Inputs = Map String ActionInput

data Action
    = Action
    { name        :: String
    , description :: String
    , inputs      :: Maybe Inputs
    }
    deriving (Show)

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = filter (/= '\'')} ''Action)

data ActionPermissionAccess
    = ReadAccess
    | WriteAccess
    deriving Eq

instance Read ActionPermissionAccess where
    readsPrec _ "read"  = [(ReadAccess, "")]
    readsPrec _ "write" = [(WriteAccess, "")]
    readsPrec _ _       = []

instance Show ActionPermissionAccess where
    show ReadAccess  = "read"
    show WriteAccess = "write"

type Permissions = Map String ActionPermissionAccess

data ActionMetadata
    = ActionMetadata
    { path        :: String
    , owner       :: Maybe String
    , project     :: Maybe String
    , version     :: Maybe String
    , permissions :: Maybe Permissions
    }
    deriving (Show, Eq)

toEnglishBool :: Bool -> String
toEnglishBool True  = "yes"
toEnglishBool False = "no"

replaceNewlinesWithSpaces :: String -> String
replaceNewlinesWithSpaces = unpack . replace "\n" " " . pack

actionStartTagPrefix :: String
actionStartTagPrefix = "<!-- gh-actions-docs-start"

actionEndTag :: String
actionEndTag = "<!-- gh-actions-docs-end -->"

prettyPrintAction :: Config -> Action -> ActionMetadata -> String
prettyPrintAction config (Action name' description' inputs') actionMetadata =
    (if noName config then "" else "## " ++ name' ++ "\n\n") ++
    (if noDescription config then "" else "### Description\n" ++ description' ++ "\n\n") ++
    (if noInputs config then "" else prettyPrintInputs inputs') ++
    (if noPermissions config then "" else prettyPrintPermissions actionMetadata) ++
    (if noUsage config then "" else prettyPrintUsage name' inputs' actionMetadata)

prettyPrintInputs :: Maybe Inputs -> String
prettyPrintInputs (Just inputs') =
    "### Inputs\n" ++
    "|Name|Description|Required|Default|\n"
        ++ "|-|-|-|-|\n"
        ++ concatMap
            ( \(name', ActionInput des req def) ->
                "`" ++ name' ++ "`"
                    ++ "|"
                    ++ maybe "" replaceNewlinesWithSpaces des
                    ++ "|"
                    ++ maybe "no" toEnglishBool req
                    ++ "|"
                    ++ maybe "" (\def' -> "`" ++ def' ++ "`") def
                    ++ "|\n"
            )
            (toList inputs')
        ++ "\n"
prettyPrintInputs _ = ""

prettyPrintPermissions :: ActionMetadata -> String
prettyPrintPermissions (ActionMetadata _ _ _ _ (Just permissions')) =
    "### Permissions\n"
    ++ "This action requires the following [permissions](https://docs.github.com/en/actions/using-jobs/assigning-permissions-to-jobs):\n"
    ++ concatMap
            ( \(name', access) ->
                "- `" ++ name' ++ ": " ++ show access ++ "`\n"
            )
            (toList permissions')
    ++ "\n"
prettyPrintPermissions _ = ""


prettyPrintUsage :: String -> Maybe Inputs -> ActionMetadata -> String
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

prettyPrintUsageWith :: Maybe Inputs -> String
prettyPrintUsageWith (Just inputs') = "  with:\n" ++ concatMap (uncurry prettyPrintUsageInputs) (toList inputs')
prettyPrintUsageWith Nothing = ""

prettyPrintUsageInputs :: String -> ActionInput -> String
prettyPrintUsageInputs name' (ActionInput des req def) =
    indent
        ++ name'
        ++ ":\n"
        ++ ( case (des, req, def) of
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

permissionsToString :: Permissions -> String
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

permissionParser :: Parser (String, ActionPermissionAccess)
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
