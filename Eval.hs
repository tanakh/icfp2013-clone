module Eval (
  evalString,
  equivString,
  EquivResponse(..),
  ) where

import Data.Word
import Text.Parser.Token
import Text.Parser.Token.Style
import Text.Trifecta
import Control.Applicative hiding (Const)
import Data.Monoid
import Prelude
import Data.List
import Data.Bits
import Text.PrettyPrint.ANSI.Leijen (Doc, renderCompact, displayS)
import Data.SBV

data EquivResponse =
    Equivalent
  | Mismatch Word64 Word64 Word64
  | Undecidable String

data Program = Program Expr

data Expr =
    Const Word64
  | Var Int
  | If0 Expr Expr Expr
  | Fold Expr Expr Expr
  | Op1 Op1 Expr
  | Op2 Op2 Expr Expr

data Op1 = Not | Shl Int | Shr Int
data Op2 = And | Or | Xor | Plus

showDoc :: Doc -> String
showDoc d = displayS (renderCompact d) ""

-- Eval

evalString :: String -> Word64 -> Either String Word64
evalString txt input =
  case parseString program mempty txt of
    Success r -> Right $ eval (\c t e -> if c == 0 then t else e) r input
    Failure d -> Left $ showDoc d

eval :: (Num b, Bits b) => (b -> b -> b -> b) -> Program -> b -> b
eval if0 (Program p) input = f p [input] where
  f (Const c) _ = fromIntegral c
  f (Var i) env = env !! i
  f (If0 c t e) env = if0 (f c env) (f t env) (f e env)
  f (Fold v a e) env =
    let vv = f v env
        aa = f a env
    in foldr (\x y -> f e $ x : y : env) aa
         [ vv `shiftR` (i*8) .&. 0xff | i <- [7,6..0] ]
  f (Op1 opr e) env =
    let ee = f e env
    in case opr of
      Not -> complement ee
      Shl i -> ee `shiftL` i
      Shr i -> ee `shiftR` i
  f (Op2 opr l r) env =
    let ll = f l env
        rr = f r env
    in case opr of
      And  -> ll .&.   rr
      Or   -> ll .|.   rr
      Xor  -> ll `xor` rr
      Plus -> ll +     rr

-- Equiv

equivString :: String -> String -> IO (Either String EquivResponse)
equivString txt1 txt2 =
  case (parseString program mempty txt1, parseString program mempty txt2) of
    (_, Failure d) -> return $ Left $ showDoc d
    (Failure d, _) -> return $ Left $ showDoc d
    (Success p1, Success p2) -> Right <$> equiv p1 p2

equiv :: Program -> Program -> IO EquivResponse
equiv p1 p2 = do
  SatResult smtres <- satWith z3 { timeOut = Just 10 } $ do
    [x, y, z] <- sWord64s ["x", "y", "z"]
    solve [ y ./= z
          , eval (\c t e -> ite (c .== 0) t e) p1 x .== y
          , eval (\c t e -> ite (c .== 0) t e) p2 x .== z
          ]

  return $ case smtres of
    Unsatisfiable _ -> Equivalent
    Satisfiable _ _ ->
      case extractModel smtres of
        Just [x, y, z] -> Mismatch x y z
        _              -> Undecidable "undecidable equivalence"
    _ -> Undecidable "undecidable equivalence"

-- Parser

program :: Parser Program
program = p <* spaces <* eof where
  p = parens $ do
    _ <- symbol "lambda"
    var <- parens $ ident emptyIdents
    Program <$> expr [var]

expr :: [String] -> Parser Expr
expr env = constant <|> var <|> if0 <|> fold <|> op1 <|> op2 where
  constant =
    Const 0 <$ symbol "0" <|>
    Const 1 <$ symbol "1"

  var = do
    name <- ident emptyIdents
    maybe (fail $ "variable not found: " ++ name) (return . Var)
      $ elemIndex name env

  if0 = try $ parens $ do
    _ <- symbol "if0"
    If0 <$> expr env <*> expr env <*> expr env

  fold = try $ parens $ do
    _ <- symbol "fold"
    v <- expr env
    a <- expr env
    e <- parens $ do
      _ <- symbol "lambda"
      (x, y) <- parens $ (,) <$> ident emptyIdents <*> ident emptyIdents
      expr $ x : y : env
    return $ Fold v a e

  op1 = try $ parens $ do
    opr <- choice [ Not    <$ symbol "not"
                  , Shl 1  <$ symbol "shl1"
                  , Shr 1  <$ symbol "shr1"
                  , Shr 4  <$ symbol "shr4"
                  , Shr 16 <$ symbol "shr16"
                  ]
    Op1 opr <$> expr env

  op2 = try $ parens $ do
    opr <- choice [ And  <$ symbol "and"
                  , Or   <$ symbol "or"
                  , Xor  <$ symbol "xor"
                  , Plus <$ symbol "plus"
                  ]
    Op2 opr <$> expr env <*> expr env
