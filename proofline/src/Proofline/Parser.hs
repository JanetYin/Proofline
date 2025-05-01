{-# LANGUAGE LambdaCase #-}
module Parser (parseString, parseStdin,parseStringWithErrors ) where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Coerce (coerce)
import Data.Foldable
import Data.Char
import Data.Void
import System.Exit
import Text.Megaparsec

import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

import Common
import Presyntax

-- data Precedence    
--   = Arrow --pi   
--   | Times --  *   
--   | Comma -- ,    
--   | App --   
--   | Proj  -- .1 .2   
--   | Atom 
--------------------------------------------------------------------------------

type Parser = Parsec Void String

ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

withPos :: Parser Raw -> Parser Raw
withPos ptm = do
  pos <- getSourcePos
  ptm >>= \case
    t@RSrcPos{} -> pure t
    t          -> pure (RSrcPos (coerce pos) t)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: String -> Parser String
symbol s = lexeme (C.string s)

char :: Char -> Parser Char
char c = lexeme (C.char c)

parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

pArrow :: Parser String
pArrow = symbol "→" <|> symbol "->"

pProd :: Parser String
pProd = symbol "×" <|> symbol "*"

pBind :: Parser Name
pBind = parseIdentity <|> symbol "_"

keyword :: String -> Bool
keyword x = x == "let" || x == "λ" || x == "U"

parseIdentity :: Parser Name
parseIdentity = try $ do
  x  <- C.letterChar
  xs <- takeWhileP Nothing (\c -> isAlphaNum c || c == '\'')
  guard (not (keyword (x:xs)))
  (x:xs) <$ ws

parseKeyword :: String -> Parser ()
parseKeyword kw = do
  _ <- C.string kw
  (takeWhileP Nothing (\c -> isAlphaNum c || c == '\'') *> empty) <|> ws

parseAtom :: Parser Raw
parseAtom = withPos (
        (RVar <$> parseIdentity)
    <|> (RU <$ char 'U')
    <|> (RHole <$ char '_')
    <|> parens parseTerm
  )

goProj :: Raw -> Parser Raw
goProj t =
  (char '.' *>
    (     ((char '₁' <|> char '1') *> goProj (RProj1 t))
      <|> ((char '₂' <|> char '2') *> goProj (RProj2 t))
    )
  )
  <|> pure t

parseProj :: Parser Raw
parseProj = goProj =<< parseAtom

parseApp :: Parser Raw
parseApp = do
  h <- parseProj
  args <- many parseProj
  pure $ foldl' RApp h args

parseSigma :: Parser Raw
parseSigma = do
  optional (try (char '(' *> pBind <* char ':')) >>= \case
    Nothing -> do
      t <- parseApp
      (RSigma "_" t <$> (pProd *> parseSigma)) <|> pure t
    Just x -> do
      a <- parseTerm
      _ <- char ')'
      _ <- pProd
      b <- parseSigma
      pure $ RSigma x a b

parseLam :: Parser Raw
parseLam = do
  _ <- char 'λ' <|> char '\\'
  xs <- some pBind
  _ <- char '.'
  t <- parseLamLet
  pure $ foldr (\x u -> RLam x u) t xs  

parsePiBinder :: Parser ([Name], Raw) 
parsePiBinder = parens ((,) <$> some pBind <*> (char ':' *> parseTerm))

parsePi :: Parser Raw
parsePi = do
  optional (try (some parsePiBinder)) >>= \case
    Nothing -> do
      t <- parseSigma  
      (pArrow *> (RPi "_" t <$> parsePi)) <|> pure t
    Just bs -> do
      case bs of 
        [([x], a)] -> 
            (RPi x a <$> (pArrow *> parsePi))
          <|> (do dom <- RSigma x a <$> (pProd *> parseSigma)
                  (RPi "_" dom <$> (pArrow *> parsePi )) <|> pure dom )
        dom -> do
            _ <- pArrow 
            b <- parsePi
            pure $! foldr (\(xs ,a) t -> foldr (\x -> RPi x a) t xs) b dom

parseLet :: Parser Raw
parseLet = do
  parseKeyword "let"
  x <- parseIdentity
  ann <- optional (char ':' *> parseTerm)
  _ <- char '='
  t <- parseTerm
  _ <- symbol ";"
  u <- parseLamLet
  pure $ case ann of
    Just a  -> RLet x a t u
    Nothing -> RLet x RHole t u

parseLamLet :: Parser Raw
parseLamLet = withPos (parseLam <|> parseLet <|> parsePi)

parsePair :: Parser Raw
parsePair = do
  t <- parseLamLet
  (RPair t <$> (char ',' *> parseTerm)) <|> pure t

parseTerm :: Parser Raw
parseTerm = withPos parsePair

parseSrc :: Parser Raw
parseSrc = ws *> parseTerm <* eof

parseString :: String -> IO Raw
parseString src =
  case parse parseSrc "(stdin)" src of
    Left e -> do
      putStrLn $ errorBundlePretty e
      exitFailure
    Right t ->
      pure t

parseStringWithErrors :: String -> IO (Either String Raw)
parseStringWithErrors src =
  case parse parseSrc "(stdin)" src of
    Left e -> do
      return $ Left (errorBundlePretty e)
    Right t ->
      return $ Right t

parseStdin :: IO (Raw, String)
parseStdin = do
  src <- getContents
  t <- parseString src
  pure (t, src)