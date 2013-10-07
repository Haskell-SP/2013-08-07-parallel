module Main where

import Control.Parallel	 
import Control.Monad.Par
import Control.Monad
import Control.Concurrent
import Data.IORef

fib1 :: Integer -> Integer
fib1 0 = 1
fib1 1 = 1
fib1 n = fib1 (n-1) + fib1 (n-2)

-- ghc --make -threaded -fforce-recomp -O -rtsopts Fib.hs && ./Fib +RTS -N1 -sstderr


fib2 :: Integer -> Integer
fib2 0 = 1
fib2 1 = 1
fib2 n = 
	let lado1 = fib2 (n-1)
	    lado2 = fib2 (n-2)
	in lado1 + lado2

fib3 :: Integer -> Integer
fib3 0 = 1
fib3 1 = 1
fib3 n = 
  let lado1 = fib3 (n-1)
      lado2 = fib3 (n-2)
  -- in lado1 `par` lado2 `pseq` lado1 + lado2
  in par lado1 (pseq lado2 (lado1 + lado2))

fib4 :: Integer -> Integer
fib4 0 = 1
fib4 1 = 1
fib4 n = 
  let lado1 = fib4 (n-1)
      lado2 = fib4 (n-2)
      final = lado1 + lado2
  in if n > 30 
  	  then lado1 `par` lado2 `pseq` final
  	  else final
-- fib4 40 = 165580141


fib5 :: Integer -> Integer
fib5 = runPar . fib5'
fib5' :: Integer -> Par Integer
fib5' 0 = return 1
fib5' 1 = return 1
fib5' n = do
  lado1 <- new
  lado2 <- new
  fork $ fib5' (n-1) >>= put lado1
  fork $ fib5' (n-2) >>= put lado2
  l1 <- get lado1
  l2 <- get lado2
  return $ l1 + l2
-- fib5 40 = 165580141

fib6 :: Integer -> Integer
fib6 = runPar . fib6'
fib6' :: Integer -> Par Integer
fib6' 0 = return 1
fib6' 1 = return 1
fib6' n =
  if n > 30 
  then do 
    lado1 <- new
    lado2 <- new
    fork $ fib6' (n-1) >>= put lado1
    fork $ fib6' (n-2) >>= put lado2
    l1 <- get lado1
    l2 <- get lado2
    return $ l1 + l2
  else do
    l1 <- fib6' (n-1)
    l2 <- fib6' (n-2)
    return $ l1 + l2
-- fib5 40 = 165580141


fib7 :: Integer -> Integer
fib7 = runPar . fib7'
fib7' :: Integer -> Par Integer
fib7' 0 = return 1
fib7' 1 = return 1
fib7' n =
  if n > 30 
  then do 
    lado1 <- spawn (fib7' (n-1))
    lado2 <- spawn (fib7' (n-2))
    l1 <- get lado1
    l2 <- get lado2
    return $ l1 + l2
  else do
    l1 <- fib7' (n-1)
    l2 <- fib7' (n-2)
    return $ l1 + l2
-- fib7 40 = 165580141

fib n =
  let fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
  in fibs !! n

data Fib =
  Fib {
    number :: !Integer -- n
  , fibN1  :: !Integer -- fib $ n - 1
  , fibN   :: !Integer -- fib n
}

initial :: Fib
initial = Fib 0 0 1

advance :: Fib -> Fib
advance (Fib n fn1 fn) = Fib (n+1) fn (fn1 + fn)

fib8 :: Int -> Integer
fib8 n = fibN (iterate advance initial !! n)  

calcFibForever :: IORef Fib -> IO ThreadId
calcFibForever var =
  forkIO $ 
    forever $ do
      v <- readIORef var
      writeIORef var $! advance v

fibAfter :: Int -> IO Integer
fibAfter seconds = do
  var <- newIORef initial
  threadId <- calcFibForever var
  threadDelay (seconds * 10^6)
  killThread threadId
  ret <- readIORef var
  return (number ret)


-- main = fibAfter 10 >>= print 
main = print =<< fibAfter 10 


