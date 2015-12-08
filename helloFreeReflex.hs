{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.Free
import Data.List
import qualified GHC.Generics as GHC
import Reflex
import Reflex.Dom

----------

type Name = String
type Info = String

data Vocab a where
  GetName :: (Name -> a) -> Vocab a
  PutInfo :: Info -> a -> Vocab a
  deriving (GHC.Generic)

instance Functor Vocab where
  fmap f = \case
    GetName g -> GetName (f . g)
    PutInfo s a -> PutInfo s (f a)

getName :: MonadFree Vocab m => m Name
getName = liftF $ GetName id

putInfo :: MonadFree Vocab m => Info -> m ()
putInfo s = liftF $ PutInfo s ()

----------

type Req = (Name -> Free Vocab ())

dissociate :: Free Vocab () -> ([Info], Maybe Req)
dissociate s0 = case s0 of
  Pure () -> ([], Nothing)
  Free w -> case w of
    GetName g -> ([], Just g)
    PutInfo x s1 -> dissociate s1 & _1 %~ (x :)

step :: Name -> ([Info], Maybe Req) -> ([Info], Maybe Req)
step n (xs, r) = case r of
  Nothing -> (xs, Nothing)
  Just g -> dissociate $ g n

----------

mySession :: MonadFree Vocab m => m ()
mySession = putInfo "Welcome to Reflex." >> go
  where
    go :: MonadFree Vocab m => m ()
    go = do
      xs <- getName 
      mapM_ putInfo $ fmap ($ xs) [id, reverse, show . length] 
      go

----------

submitName :: MonadWidget t m => m (Event t String) 
submitName = do
  rec t <- textInput $ def & setValue .~ fmap (\_ -> "") e
      e <- button "Submit"
  return $ ffilter (/= "") $ tag (current (value t)) e

sayHello :: MonadWidget t m => Event t String -> m ()
sayHello e = do
  let h = fmap (\xs -> "Hello, " ++ xs ++ ".") e 
  d <- holdDyn ((intercalate ", ") . fst $ dissociate mySession) h 
  dynText d

----------

main :: IO ()
main = mainWidget $
  el "div" $ do
    e <- submitName
    e1 <- el "div" $ do
      d <- foldDyn step (dissociate mySession) e
      d1 <- mapDyn ((intercalate ", ") . fst) d
      return $ updated d1
    el "div" $ do
      sayHello $ e1

----------
