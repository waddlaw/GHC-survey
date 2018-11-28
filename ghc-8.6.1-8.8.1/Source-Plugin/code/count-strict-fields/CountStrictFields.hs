module CountStrictFields (plugin) where

import GhcPlugins
import TcRnTypes
import HsExtension
import HsDecls
import HsTypes
import qualified GHC.LanguageExtensions as LangExt

plugin :: Plugin
plugin = defaultPlugin
  { renamedResultAction = renamedAction
  }

renamedAction :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
renamedAction _ tc gr = do
  dflags <- getDynFlags
  let isStrictData = xopt LangExt.StrictData dflags

  let tyClDecls = map unLoc $ tyClGroupTyClDecls $ hs_tyclds gr
      tyDataDecls = filter isDataDecl tyClDecls
      hsDataDefn = map tcdDataDefn tyDataDecls
      lConDecl = concatMap dd_cons hsDataDefn
      decls = filter isConDeclH98 $ map unLoc lConDecl
      lBangTypes = concatMap (hsConDeclArgTys . getConArgs) decls
      fieldNums =
        if isStrictData
        then length lBangTypes
        else length $ filter (isSrcStrict . getSrcStrictness . getBangStrictness) lBangTypes

  liftIO $ putStrLn $ ""
  liftIO $ putStrLn $ "========================================"
  liftIO $ putStrLn $ "StrictData : " ++ (show isStrictData)
  liftIO $ putStrLn $ "strict fields: " ++ (show fieldNums) ++ "/" ++ (show $ length lBangTypes)
  liftIO $ putStrLn $ "========================================"
  liftIO $ putStrLn $ ""

  return (tc, gr)

isConDeclH98 :: ConDecl pass -> Bool
isConDeclH98 ConDeclH98{} = True
isConDeclH98 _ = False

getSrcStrictness :: HsSrcBang -> SrcStrictness
getSrcStrictness (HsSrcBang _ _ ss) = ss