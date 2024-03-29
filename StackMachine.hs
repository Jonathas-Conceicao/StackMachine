module StackMachine where

import Control.Monad.Fail (MonadFail)
import Debug.Trace

type Values = [Int]

data Stack a = Stack (Values -> (a, Values))

instance (Show a) => Show (Stack a) where
  show (Stack p) = show $ (p [])

instance Functor Stack where
  fmap f m = do
    r <- m
    return (f r)

instance Applicative Stack where
  pure = return
  fm <*> mx = do
    f <- fm
    x <- mx
    return (f x)

instance Monad Stack where
  return x = Stack $ \s -> (x,s)
  (Stack h) >>= f = Stack $ \s -> let (a, newS) = h s
                                      (Stack g) = f a
                                  in g newS

instance MonadFail Stack where
fail = error

getStack :: Stack Values
getStack = Stack $ \s -> (s, s)

putStack :: Values -> Stack ()
putStack s = Stack $ \_ -> ((), s)

pushStack :: Int -> Stack ()
pushStack n = do
  l <- getStack
  putStack (n:l)

printStack :: Stack ()
printStack = do
  getStack >>= traceShowM

popStack :: Stack Int
popStack = do
  (h:hs) <- getStack
  putStack hs
  return h

multStack :: Stack ()
multStack = do
  a <- popStack
  b <- popStack
  pushStack (a * b)

sumStack :: Stack ()
sumStack = do
  a <- popStack
  b <- popStack
  pushStack (a + b)

subStack :: Stack ()
subStack = do
  a <- popStack
  b <- popStack
  pushStack (b - a)

divStack :: Stack ()
divStack = do
  a <- popStack
  b <- popStack
  pushStack (div b a)

parseStrings :: [String] -> Stack ()
parseStrings []           = return ()
parseStrings ("SUM":xs)   = sumStack >> (parseStrings xs)
parseStrings ("SUB":xs)   = subStack >> (parseStrings xs)
parseStrings ("DIV":xs)   = divStack >> (parseStrings xs)
parseStrings ("MULT":xs)  = multStack >> (parseStrings xs)
parseStrings ("PRINT":xs) = printStack >> (parseStrings xs)
parseStrings ("":xs)      = parseStrings xs
parseStrings (x:xs)
  | y == "PUSH" = (pushStack $ read $ head ys) >> (parseStrings xs)
  | otherwise = error "Invalid stack operation"
  where
    l  = words x
    y  = head l
    ys = tail l


interpret :: FilePath -> IO ()
interpret f = do
  fullFile <- readFile f
  let file = lines fullFile
  print $ parseStrings file
