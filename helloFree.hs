{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad.Free
import System.IO

----------

type Name = String

data DialogueF a where
  AskName :: String -> (Name -> a) -> DialogueF a
  SayHello :: String -> (() -> a) -> DialogueF a

instance Functor DialogueF where
  fmap f = \case
    AskName s g -> AskName s (f . g)
    SayHello s g -> SayHello s (f . g)

type Dialogue = Free DialogueF

askName :: String -> Dialogue String
askName s = liftF $ AskName s id

sayHello :: String -> Dialogue ()
sayHello s = liftF $ SayHello s id

----------

myDialogue :: Dialogue ()
myDialogue = do
  xs <- askName "What is your name? "
  if (null xs)
  then return ()
  else do
    sayHello $ "Hello, " ++ xs ++ ".\n"
    myDialogue

----------

evalDialogue :: Dialogue a -> IO a
evalDialogue = \case
  Pure x -> return x     -- x :: a 
  Free y -> case y of    -- y :: DialogueF (Dialogue a)
    AskName s g -> do    -- g :: String -> Dialogue a
      putStr $ s 
      hFlush stdout
      l <- getLine
      evalDialogue (g l)
    SayHello s g -> do   -- g :: () -> Dialogue a
      putStr $ s
      hFlush stdout
      evalDialogue (g ())      

----------

main :: IO ()
main = do
  evalDialogue myDialogue

----------
