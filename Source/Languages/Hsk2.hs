
module Languages.Hsk2 where

import Languages.Core

import Control.Lens
import Text.Megaparsec
import Text.Megaparsec.Char


data HskType
  = IntT
  | UnitT
  | IOT HskType
  | CStringT
  | CharT
  | StablePtrT QualifiedIdentifier
  | ForeignPtrT QualifiedIdentifier
  | FuncT [HskType] HskType
  deriving (Show)

type QualifiedIdentifier = [Text]

type Definition = Named HskType

type Parser = Parsec Void Text


parseExports :: FilePath -> IO (Either String [Definition])
parseExports path =
      (first parseErrorPretty) . parse fileP path <$> readFileUtf8 path

fileP :: Parser [Definition]
fileP = catMaybes <$> many lineP

lineP :: Parser (Maybe Definition)
lineP =
  (Just <$> definitionP) <|> (manyTill anyChar eol *> pure Nothing)

definitionP :: Parser Definition
definitionP =
  do
      stringP "foreign export ccall"
      name <- identifierP
      stringP "::"

      type1 <- subTypeP
      types <- many $ stringP "->" *> subTypeP

      let (args, ret) = pushBack type1 types
      return $ Tagged name (FuncT args ret)
  where
      subTypeP :: Parser HskType
      subTypeP =
          try int
          <|> try char
          <|> try cstring
          <|> try ptr
          <|> try stablePtr
          <|> try foreignPtr
          <|> try io
          <|> try parens

        where
          int        = stringP "Int" *> pure IntT
          char       = stringP "Char" *> pure CharT
          cstring    = stringP "CString" *> pure CStringT
          stablePtr  = stringP "StablePtr" *> (StablePtrT <$> qualifiedIdentifierP)
          foreignPtr = stringP "ForeignPtr" *> (ForeignPtrT <$> qualifiedIdentifierP)
          ptr        = stringP "Ptr" *> (ForeignPtrT <$> qualifiedIdentifierP)
          io         = stringP "IO" *> (IOT <$> subTypeP)
          parens     = between (stringP "(") (stringP ")") $ subTypeP

      pushBack :: a -> [a] -> ([a], a)
      pushBack a [] = ([], a)
      pushBack a (x:xs) = _1 %~ (a:) $ pushBack x xs


qualifiedIdentifierP :: Parser QualifiedIdentifier
qualifiedIdentifierP = sepBy1 identifierP (char '.')

identifierP :: Parser Text
identifierP = pack <$> (lexemeC . try $ (:) <$> first <*> many later)
  where
    first = letterChar <|> char '_'
    later = alphaNumChar <|> char '_'



lexemeC p = p <* skipSpace

stringP :: Text -> Parser Text
stringP s = string s <* skipSpace

skipSpace = skipMany spaceChar
