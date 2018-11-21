# note

- `-fplugin-opt` オプションを使う

## 基本形

```hs
plugin :: Plugin
plugin = defaultPlugin {
    parsedResultAction = parsed
  , typeCheckResultAction = typechecked
  , spliceRunAction = spliceRun
  , interfaceLoadAction = interfaceLoad
  , renamedResultAction = renamed
  }
```

### Parsed representation

When you want to define a plugin that uses the syntax tree of the source code, you would like to override the `parsedResultAction` field.

```hs
parsed :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
```

- `ModSummary`: useful meta-information about the compiled module
- `HsParsedModule`: lexical and syntactical information 

### Type checked representation

When you want to define a plugin that needs semantic information about the source code, use the `typeCheckResultAction` field.

```hs
typechecked :: [CommandLineOption] -> ModSummary -> TcGblEnv      -> TcM TcGblEnv
renamed     :: [CommandLineOption] -> TcGblEnv   -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
```

### Evaluated code

When the compiler type checks the source code, Template Haskell Splices and Template Haskell Quasi-quotation will be replaced by the syntax tree fragments generated from them.

```hs
spliceRun :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
```

### Interface files

Sometimes when you are writing a tool, knowing the source code is not enough, you also have to know details about the modules that you import.

```hs
interfaceLoad :: forall lcl . [CommandLineOption] -> ModIface -> IfM lcl ModIface
```

## Plugin 型

[Plugins](https://www.stackage.org/haddock/nightly-2018-11-20/ghc-8.6.2/Plugins.html) モジュールで定義

```hs
data Plugin = Plugin
  { installCoreToDos :: CorePlugin
  , tcPlugin :: TcPlugin
  , pluginRecompile :: [CommandLineOption] -> IO PluginRecompile
  , parsedResultAction :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
  , renamedResultAction :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
  , typeCheckResultAction :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
  , spliceRunAction :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
  , interfaceLoadAction :: forall lcl . [CommandLineOption] -> ModIface -> IfM lcl ModIface
  }
```

### defaultPlugin

```hs
defaultPlugin :: Plugin
defaultPlugin = Plugin
  { installCoreToDos      = const return
  , tcPlugin              = const Nothing
  , pluginRecompile       = impurePlugin
  , renamedResultAction   = \_ env grp -> return (env, grp)
  , parsedResultAction    = \_ _ -> return
  , typeCheckResultAction = \_ _ -> return
  , spliceRunAction       = \_ -> return
  , interfaceLoadAction   = \_ -> return
  }

impurePlugin :: [CommandLineOption] -> IO PluginRecompile
impurePlugin _args = return ForceRecompile
```