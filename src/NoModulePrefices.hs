{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module NoModulePrefices (plugin) where

import Control.Monad (when)
import Data.List (foldl')
import ErrUtils (mkPlainErrMsg, warningMsg)
import GhcPlugins hiding ((<>), getHscEnv)
import GHC.Hs (hsmodImports, ImportDecl(..))
import System.Directory (getXdgDirectory, doesFileExist, XdgDirectory(..))
import System.FilePath ((</>))
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HCS

type Mapping = HM.HashMap T.Text Destination

data Destination = Destination !T.Text !Mapping

instance Semigroup Destination where
  Destination _ m <> Destination x n = Destination x (HM.unionWith (<>) m n)

instance Yaml.FromJSON Destination where
  parseJSON = Yaml.withObject "Destination" $ \obj -> Destination
    <$> obj Yaml..: "default"
    <*> obj Yaml..:? "overrides" Yaml..!= HM.empty

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \ urls _ pm -> do
    dynFlags <- getDynFlags
    when (null urls) $ liftIO $ warningMsg dynFlags $ "NoModulePrefices: No URLs given"
    mappings <- traverse fetchMapping urls
    let mapping = foldl' (HM.unionWith (<>)) mempty mappings

    -- Try to find a module. If no matching packge is found, apply the filter
    let apply' decl = case lookupModuleWithSuggestions dynFlags (case ideclName decl of L _ a -> a) Nothing of
          LookupNotFound _ -> decl
            { ideclName = apply mapping <$> ideclName decl
            , ideclSourceSrc = NoSourceText
            }
          _ -> decl

    module' <- traverse (\m -> do
      let m' = m { hsmodImports = fmap (fmap apply') $ hsmodImports m }
      pure m') $ hpm_module pm
    pure pm { hpm_module = module' }
  , pluginRecompile = flagRecompile }

apply :: Mapping -> ModuleName -> ModuleName
apply m0 modName = maybe modName (mkModuleName . T.unpack . T.intercalate ".") $ go m0 $ T.splitOn "." $ T.pack $ moduleNameString modName where
  go m (x : xs) = do
    Destination def m' <- HM.lookup x m
    case go m' xs of
      Nothing -> Just $ def : xs
      Just ys -> Just ys
  go _ [] = Nothing

fetchMapping :: String -> Hsc Mapping
fetchMapping urlHash = do
  let (url, drop 1 -> expectedHash) = break (=='@') urlHash
  dir <- liftIO $ getXdgDirectory XdgCache "NoModulePrefices"
  let path = dir </> expectedHash
  cached <- liftIO $ doesFileExist path
  content <- liftIO $ if cached
    then B.readFile path
    else do
      req <- HC.parseUrlThrow url
      resp <- HCS.getGlobalManager >>= HC.httpLbs req
      pure $ BL.toStrict $ HC.responseBody resp
  let actualHash = show $ Crypto.hashWith Crypto.SHA256 content
  dynFlags <- getDynFlags
  when (expectedHash /= actualHash) $ throwOneError
    $ mkPlainErrMsg dynFlags noSrcSpan
    $ text actualHash <+> "doesn't match" <+> text urlHash

  liftIO $ Yaml.decodeThrow content
