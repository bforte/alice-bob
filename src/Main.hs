{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, TemplateHaskell #-}

module Main where

import Control.Monad.State.Strict
import Data.Char
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import Text.Parsec hiding (State)
import System.Console.GetOpt
import System.Environment


type Prog = [Stmt]

data Stmt = One       | Len         | Pop        | Swp          -- Nilads
          | Push Prog | Negate Prog | While Prog | Ignore Prog  -- Monads

parseProg = parse (progP <* eof) "src" . filter (`elem` brackets) where

    progP = many $ choice [paren cs n m | (cs,n,m) <- funcs]

    funcs = [ ("()",One,Push)
            , ("[]",Len,Negate)
            , ("{}",Pop,While)
            , ("<>",Swp,Ignore)
            ]

    paren (a:b:_) nilad monad = between (char' a) (char' b) progP >>= \case
      [] -> return nilad
      ps -> return (monad ps)
    paren _ _ _ = error "won't happen"

    char' c = dropCs *> char c <* dropCs

    dropCs = void . many $ noneOf brackets

    brackets = "()[]{}<>"

parseInput = parse (read <$> many1 digit <* eof) "input"


class PP t where
  push :: Integer -> t -> t
  pop  :: t -> (Integer, t)
  size :: t -> Integer
  peek :: t -> Integer
  peek = fst . pop

data Queue = Queue [Integer] [Integer]

instance PP Queue where
  push v (Queue i o) = Queue (v:i) o
  pop q@(Queue [] []) = (0, q)
  pop (Queue i []) = pop $ Queue [] (reverse i)
  pop (Queue i o) = (head o, Queue i (tail o))
  size (Queue i o) = fromIntegral $ length i + length o

instance PP [Integer] where
  push v xs = v:xs
  pop (x:xs) = (x,xs)
  pop [] = (0,[])
  size = fromIntegral . length


data SQ = S | Q deriving Eq

next S = Q
next Q = S

data Env = E
  { _alice  :: [Integer]
  , _bob    :: Queue
  , _active :: SQ
  }

makeLenses ''Env


run stack inputs prog = output $ execState (mapM eval prog) init where
  init | stack = E inputs (Queue [] []) S
       | otherwise = E [] (Queue inputs []) Q

  output env
    | env ^. active == S = env ^. alice
    | (Queue i o) <- env ^. bob = o ++ reverse i

  eval One = pure 1
  eval Len = use active >>= \case
    Q -> size <$> use bob
    S -> size <$> use alice
  eval Pop = use active >>= \case
    Q -> do
      q <- use bob
      let (a,q') = pop q
      bob .= q'
      pure a
    S -> do
      s <- use alice
      let (a,s') = pop s
      alice .= s'
      pure a
  eval Swp = 0 <$ (active %= next)
  eval (Push stmts) = do
    v <- sum <$> mapM eval stmts
    use active >>= \case
      Q -> bob %= push v
      S -> alice %= push v
    pure v
  eval (Negate stmts) = negate . sum <$> mapM eval stmts
  eval w@(While stmts) = use active >>= \case
    Q -> peek <$> use bob >>= \case
      v | v /= 0 -> pure 0
        | otherwise -> (+) . sum <$> mapM eval stmts <*> eval w
    S -> peek <$> use alice >>= \case
      v | v == 0 -> pure 0
        | otherwise -> (+) . sum <$> mapM eval stmts <*> eval w
  eval (Ignore stmts) = 0 <$ mapM eval stmts


data Flags = F Bool Bool ([Integer] -> IO ())

defaults = F False True (putStrLn . unwords . map show)

options =
  [ Option "e" ["expression"] (NoArg $ \(F _ s p)-> F True s p) "evaluate expression"
  , Option "a" ["ascii"] (NoArg $ \(F e s _)-> F e s ascii) "ascii mode"
  , Option "b" ["bob"] (NoArg $ \(F e _ p)-> F e False p) "start with Bob's stack"
  ] where ascii = putStr . map (chr . (`mod` 128) . fromIntegral)


main = getOpt Permute options . map (map toLower) <$> getArgs >>= \case
  (args,a:as,[]) -> let (F e s p) = foldr ($) defaults args in
                      go s p as =<< if e then pure a else readFile a
  (_,[],_)       -> die "missing file/expression"
  (_,_,err)      -> die $ concat err

  where usage = " usage: alice-bob (-e expr | file) [-a] [-b] INPUTS"

        go s prnt as src = either (ioError . userError . show) prnt $
          run s <$> mapM parseInput as <*> parseProg src

        die m = ioError . userError $ m ++ "\n" ++ usageInfo usage options
