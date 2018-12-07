module ReplacePrelude (plugin) where

import GhcPlugins
import TcRnTypes (IfM, TcM, TcGblEnv, tcg_binds, tcg_rn_decls)
import HsExtension (GhcTc, GhcRn, GhcPs)
import HsDecls (HsGroup)
import HsExpr (LHsExpr)
import HsSyn

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = parsedPlugin
  }

parsedPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedPlugin _ _ pm = do
  dflags <- getDynFlags

  let extract = hsmodImports . unLoc
      customPrelude = noLoc $ simpleImportDecl $ mkModuleName "RIO"

      m = fmap (updateHsModule customPrelude) $ hpm_module pm
      pm' = pm { hpm_module = m }

  liftIO $ putStrLn $ "import modules: \n" ++ (showSDoc dflags $ ppr $ extract $ hpm_module pm')
  return pm'

updateHsModule :: LImportDecl pass -> HsModule pass -> HsModule pass
updateHsModule importDecl hsm = hsm { hsmodImports = importDecl:decls }
  where decls = hsmodImports hsm