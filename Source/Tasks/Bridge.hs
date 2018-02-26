

module Tasks.Bridge where

import Data.Maybe

import Control.Lens
import Control.Monad.Except
import Data.Text.Lens

import qualified Paths
import qualified Paths.OuterSphere as OuterSphere
import Bridge.Core
import Bridge.Cpp as Cpp
import qualified Bridge.Hsk2 as Hsk


generate :: [FilePath] -> ExceptT String IO Cpp.Class
generate fs =
  do
      definitions  <- liftIO $ mapM Hsk.parseExports fs
      definitions' <- hoistEither $ map join $ sequence $ definitions

      root <- liftIO $ Paths.root

      let hskDefs = (definitions') ++
                    [ Tagged "hsStart" (Hsk.FuncT [] Hsk.UnitT)
                    , Tagged "hsEnd" (Hsk.FuncT [] Hsk.UnitT) ]

      let defs = genTypeDef <$> hskDefs
      let vars = (genVariable <$> hskDefs) ++
                 [ Variable Static (Tagged "DllHandle" (PointerT VoidT)) "nullptr"
                 , Variable Static (Tagged "CurrentLocked" IntT) "0"
                 ]

      let code = Cpp.generateClass
                   hIncludes
                   cppIncludes
                   defs
                   vars
                   [
                       genLoadFunc hskDefs (root->>OuterSphere.binaries),
                       genUnloadFunc
                   ]
                   (ClassName "Foundation")
      return code

traceMap :: Functor f => (a -> b) -> f a -> f (a, b)
traceMap f = fmap (\a -> (a, f a))

hIncludes = [ "#include \"Internal/OS.h\""]

cppIncludes = [ "#include \"OuterSphere.h\""
              , "#include \"Foundation.h\""
              , "#include <iostream>" ]

genTypeDef :: Hsk.Definition -> Cpp.TypeDef
genTypeDef = TypeDef . mapTagged makeTypeName (FunctionPointerT . convType)

genVariable :: Hsk.Definition -> Cpp.Variable
genVariable def
  = Variable Static (Tagged name (TypeDefT $ genTypeDef $ def)) "nullptr"
  where
    name = makeFunctionVarName . getTag $ def

genLoadFunc :: [Hsk.Definition] -> FilePath -> Function
genLoadFunc defs binPath = Function Static (Tagged "Init" (FuncT [] VoidT)) $
                   [
                       "FString binPath = L\"" ++ convString binPath ++ "/\";",
                       "FString realPath = binPath + FString(\"Foundation.dll\");",
                       "FString lockedName = FString(\"Foundation.locked.dll\");",
                       "FString lockedPath = binPath + lockedName;",
                       "FPlatformFileManager::Get().GetPlatformFile().DeleteFile(*lockedPath);",
                       "FPlatformFileManager::Get().GetPlatformFile().CopyFile(*lockedPath, *realPath);",
                       "DllHandle = FPlatformProcess::GetDllHandle(*lockedName);",
                       "if (DllHandle)",
                       "{",
                       "\tstd::cout << \"loaded lib\" << std::endl;"
                   ]
                ++ (tab <$> initVar <$> defs)
                ++ [
                       tab "HsStart();",
                       "}"
                   ]
  where
    convString :: String -> Text
    convString = pack . fmap convChar

    convChar '\\' = '/'
    convChar a = a

    initVar (Tagged var _)
      = fnName ++ " = (" ++ fnType ++ ")"
        ++ "FPlatformProcess::GetDllExport(DllHandle, L\"" ++ var ++ "\");"
      where
        fnName = makeFunctionVarName var
        fnType = makeTypeName var

tab = ("\t" ++)


genUnloadFunc :: Function
genUnloadFunc = Function Static (Tagged "DeInit" (FuncT [] VoidT))
                [ "if (DllHandle)"
                , "{"
                , tab "HsEnd();"
                , tab "FPlatformProcess::FreeDllHandle(DllHandle);"
                , tab "std::cout << \"unloadLib\" << std::endl;"
                , "}"]


--- Type conversion ---
convType :: Hsk.HskType -> Cpp.CppType
convType Hsk.UnitT           = Cpp.VoidT
convType Hsk.IntT            = Cpp.IntT
convType Hsk.CStringT        = Cpp.PointerT Cpp.VoidT
convType Hsk.CharT           = Cpp.IntT
convType (Hsk.StablePtrT t)  = Cpp.PointerT Cpp.VoidT
convType (Hsk.ForeignPtrT t) = Cpp.PointerT (Cpp.ExternT (intercalate "::" t))
convType (Hsk.IOT t)         = convType t
convType (Hsk.FuncT a b)     = Cpp.FuncT
                               (Tagged "" <$> convType <$> a)
                               (convType b)

--- Renaming Identifiers ---
makeTypeName :: Text -> Text
makeTypeName = (++ "T") . (ix 0 %~ charToUpper)

makeFunctionVarName :: Text -> Text
makeFunctionVarName = ix 0 %~ charToUpper






-- targetType :: BType -> Text
-- targetType BInt = "int"

-- targetDef :: BDefinition -> Text
-- targetDef (BDefinition name types)
--   = ("void" `fromMaybe` (targetType <$> last types)) ++ " "
--   ++ name ++ "("
--   ++ (intercalate ", " $ ([] `fromMaybe` (fmap targetType <$> init types)))
--   ++ ")"


