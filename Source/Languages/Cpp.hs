
module Languages.Cpp
  (
      generateClass,
      TypeDef(..),
      Variable(..),
      Function(..),
      ClassName(..),
      Class(..),
      CppType(..),
      Static(..)
  )
where

import Languages.Core


data CppType
  = VoidT
  | IntT
  | FuncT [Named CppType] CppType
  | PointerT CppType
  | FunctionPointerT CppType
  | TypeDefT TypeDef
  | ExternT Text

instance Generate (Named CppType) where
    generate (Tagged name (TypeDefT (TypeDef (Tagged t _))))
      = t +? name

    generate (Tagged name IntT)
      = "int" +? name

    generate (Tagged name VoidT)
      = "void" +? name

    generate (Tagged name (ExternT t))
      = t +? name

    generate (Tagged name (PointerT t))
      = generate (Tagged ("*" ++ name) t)

    generate (Tagged name (FunctionPointerT t))
      = generate (Tagged ("(*" ++ name ++ ")") t)

    generate (Tagged name (FuncT argsT retT))
      = generate (Tagged func retT)
      where
        func = name ++ "(" ++ args ++ ")"
        args = intercalate ", " (generate <$> argsT)

      -- = ret ++" "++ name ++ "(" ++ args ")"
      -- where
      --   ret  = "void" `fromMaybe` (targetType <$> last types)
      --   args = intercalate ", " $ ([] `fromMaybe` (fmap targetType <$> init types))

appendWord :: Text -> Text -> Text
appendWord ""  b = b
appendWord a  "" = a
appendWord a   b = a ++ " " ++ b

(+?) = appendWord

class Generate g where
    generate :: g -> Text


newtype TypeDef = TypeDef (Named CppType)

instance Generate TypeDef where
    generate (TypeDef t) = "typedef " ++ generate t

data Static = Static | NonStatic
instance Generate Static where
    generate Static = "static"
    generate NonStatic = ""

type Value = Text
data Variable = Variable Static (Named CppType) Value
newtype VariableDef = VariableDef Variable
data VariableStaticImpl = VariableStaticImpl ClassName Variable

instance PreRender VariableDef where
    preRender (VariableDef (Variable static var _)) = [Line 0 $ generate static +? generate var ++ ";" ]

instance PreRender VariableStaticImpl where
    preRender (VariableStaticImpl cn (Variable Static var value))
      = [ Line 0 $ generate (Member cn var) +? "=" +? value ++ ";" ]
    preRender (VariableStaticImpl _  (Variable NonStatic _ _))
      = []

data Member = Member ClassName (Named CppType)
instance Generate Member where
    generate (Member (ClassName cn) t) = generate . mapTag ((cn ++ "::") ++) $ t

data Function = Function Static (Named CppType) [Text]
newtype FunctionDef = FunctionDef Function
data FunctionImpl = FunctionImpl ClassName Function

instance PreRender FunctionDef where
    preRender (FunctionDef (Function s t _)) = [Line 0 $ generate s +? generate t ++ ";"]

instance PreRender FunctionImpl where
    preRender (FunctionImpl cn (Function _ t xs))
      = [ Line 0 $ generate $ Member cn t
        , Line 0 $ "{" ]
        ++ (Line 1 <$> xs)
        ++ [ Line 0 $ "}"]
        ++ blank


data Line = Line Int Text

render :: [Line] -> Text
render = intercalate "\n" . fmap indent
  where
    indent (Line i txt) = (mconcat $ replicate i "\t") ++ txt

tab :: [Line] -> [Line]
tab = fmap $ \(Line i a) -> Line (i+1) a

class PreRender s where
    preRender :: s -> [Line]


newtype Statement g = Statement g
instance Generate g => PreRender (Statement g) where
    preRender (Statement g) = [Line 0 (generate g ++ ";")]

blank = [Line 0 ""]

newtype ClassName = ClassName Text
instance Generate ClassName where
    generate (ClassName n) = n

newtype SystemInclude = SystemInclude Text
instance PreRender SystemInclude where
    preRender (SystemInclude x) = [Line 0 $ "#include <" ++ x ++ ">"]


newtype Include = Include Text
instance PreRender Include where
    preRender (Include x) = [Line 0 $ "#include \"" ++ x ++ "\""]

generateClass :: [Text] -> [Text] -> [TypeDef] -> [Variable] -> [Function] -> ClassName -> Class
generateClass hIncludes cppIncludes typeDefs variables functions name
  = Class
    {
        header = render $
                 blank
              ++ [Line 0 "#pragma once"]
              ++ (Line 0 <$> hIncludes)
              ++ blank
              ++ (preRender =<< Statement <$> typeDefs)
              ++ blank
              ++ [ Line 0 $ "class " ++ generate name
                 , Line 0 $ "{"
                 , Line 0 $ "public:" ]
              ++ (tab $ preRender =<< VariableDef <$> variables)
              ++ blank
              ++ (tab $ preRender =<< FunctionDef <$> functions)
              ++ [ Line 0 $ "};" ],

        source = render $
                 blank
              ++ (Line 0 <$> cppIncludes)
              ++ blank
              ++ (preRender =<< VariableStaticImpl name <$> variables)
              ++ (preRender =<< FunctionImpl name <$> functions )

    }

data Class = Class
  {
      header :: Text,
      source :: Text
  }
