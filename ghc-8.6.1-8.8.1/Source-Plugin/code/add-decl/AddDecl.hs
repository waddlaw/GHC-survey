module AddDecl (plugin) where

import GhcPlugins
import TcRnTypes (IfM, TcM, TcGblEnv, tcg_binds, tcg_rn_decls)
import HsExtension (GhcTc, GhcRn, GhcPs)
import HsDecls (HsGroup)
import HsExpr (LHsExpr)
import HsSyn (HsModule, hsmodDecls)
import Parser (parseDeclaration)
import Lexer (unP, ParseResult(POk), mkPState)
import HsDecls (LHsDecl)
import StringBuffer (stringToStringBuffer)

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = parsedPlugin
  }

parsedPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedPlugin _ _ pm = do
  dflags <- getDynFlags

  let f = hsmodDecls . unLoc
      g = parserDecl "g = 1+1" dflags

      m = fmap (updateHsModule g) $ hpm_module pm
      pm' = pm { hpm_module = m }

  liftIO $ putStrLn $ "parsePlugin: \n" ++ (showSDoc dflags $ ppr $ f $ hpm_module pm')
  return pm'

parserDecl :: String -> DynFlags -> LHsDecl GhcPs
parserDecl str dflags =
  case unP parseDeclaration (mkPState dflags buf loc) of
    POk _ rdr_module -> rdr_module
    _ -> undefined
  where
    loc = mkRealSrcLoc (mkFastString "plugin") 1 1
    buf = stringToStringBuffer str

updateHsModule :: LHsDecl pass -> HsModule pass -> HsModule pass
updateHsModule decl hsm = hsm { hsmodDecls = decl:decls }
  where decls = hsmodDecls hsm