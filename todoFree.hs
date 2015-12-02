{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Default
import Control.Lens
import Control.Monad.Free
import Control.Monad.State.Class 
import Control.Monad.Trans.State.Lazy hiding (get)
import qualified GHC.Generics as GHC
import Safe
import System.IO

----------

type Task = String
type Id = Int

data Todo = Todo { _todo_tasks :: [Task]
                 }
  deriving (Show, Read, Eq, GHC.Generic)
makeLenses ''Todo

instance Default Todo where
  def = Todo { _todo_tasks = []
             }

data Action = Quit
            | AddTask
            | DeleteTask
            | ListTasks
  deriving (Show, Read, Eq, Enum, Bounded, GHC.Generic)
makePrisms ''Action

----------

data Vocab a where
  GetActionMay :: (Maybe Action -> a) -> Vocab a
  PutInfo :: String -> a -> Vocab a 
  ListActions :: a -> Vocab a
  GetTask :: (Task -> a) -> Vocab a
  GetIdMay :: (Maybe Id -> a) -> Vocab a

instance Functor Vocab where
  fmap f = \case
    GetActionMay g -> GetActionMay (f . g)
    PutInfo s a -> PutInfo s (f a)
    ListActions a -> ListActions (f a)
    GetTask g -> GetTask (f . g)
    GetIdMay g -> GetIdMay (f . g)

getActionMay :: MonadFree Vocab m => m (Maybe Action)
getActionMay = liftF $ GetActionMay id

putInfo :: MonadFree Vocab m => String -> m ()
putInfo s = liftF $ PutInfo s ()

listActions :: MonadFree Vocab m => m ()
listActions = liftF $ ListActions ()

getTask :: MonadFree Vocab m => m Task
getTask = liftF $ GetTask id

getIdMay :: MonadFree Vocab m => m (Maybe Id)
getIdMay = liftF $ GetIdMay id

----------

eval :: Free Vocab () -> IO ()
eval = \case
  Pure x -> return x
  Free y -> case y of
    GetActionMay g -> do
      l <- getLine
      let mn = readMay l :: Maybe Int
          ma = (mn >>= toEnumMay) :: Maybe Action
      eval $ g ma
    PutInfo s a -> do
      putStrLn $ s
      hFlush stdout
      eval a
    ListActions a -> do
      let xs :: [Action]
          xs = [minBound .. maxBound]
      putStrLn . show $ zip [(0 :: Int) ..] xs
      eval a
    GetTask g -> do
      l <- getLine
      eval $ g l
    GetIdMay g -> do
      l <- getLine
      let mn = readMay l :: Maybe Id
      eval $ g mn

----------

askAction :: ( MonadFree Vocab m
--              , MonadState Todo m
             ) => m Action
askAction = do
  putInfo "Please select the next action."
  listActions
  ma <- getActionMay
  case ma of
    Nothing -> do
      putInfo "No action selected."
      askAction
    Just a -> do
      return a

askTask :: ( MonadFree Vocab m
--            , MonadState Todo m
           ) => m Task
askTask = do
  putInfo "Please describe the task: "
  t <- getTask
  return t

dispTasks :: (MonadFree Vocab m, MonadState Todo m) => m Int
dispTasks = do 
      todo <- get
      let ts = todo ^. todo_tasks
      putInfo "Current tasks: "
      mapM_ (putInfo . show) $ zip [(1 :: Int) ..] ts
      return $ length ts

askTaskId :: (MonadFree Vocab m, MonadState Todo m) => m Id
askTaskId = do
  putInfo "Please enter a task id (0 to cancel)."
  mn <- getIdMay
  case mn of
    Nothing -> do
      putInfo "No task id provided."
      nmax <- dispTasks
      putInfo $ "Total number of tasks: " ++ (show nmax)
      askTaskId
    Just n -> do
      return n

promptInfo :: ( MonadFree Vocab m
--               , MonadState Todo m
              ) => String -> m ()
promptInfo = putInfo . ("EFFECT: " ++)

processAction :: ( MonadFree Vocab m
                 , MonadState Todo m
                 ) => Action -> m ()
processAction = \case
  Quit -> do
    promptInfo $ "Good bye!\n"
  AddTask -> do
    t <- askTask
    todo_tasks %= (t :)
    promptInfo $ (show t) ++ " added to the list.\n"
  DeleteTask -> do
    n <- askTaskId
    ts <- use todo_tasks 
    let ts' = take (n - 1) ts
        ts'' = drop n ts
        mt = ts `atMay` (n - 1)
    case mt of 
      Nothing -> promptInfo $ "Nothing to delete.\n"
      Just t -> do
        todo_tasks .= ts' ++ ts''
        promptInfo $ "Task " ++ (show n) ++ " " ++ (show t) ++ " deleted from the list.\n"
  ListTasks -> do
    nmax <- dispTasks
    promptInfo $ (show nmax) ++ " task(s) observed in the list.\n"  

performAction :: (MonadFree Vocab m, MonadState Todo m) => m Action
performAction = do
  a <- askAction
  processAction a
  return a

mySession :: MonadFree Vocab m => Todo -> m ()
mySession t0 = do
  (a, t1) <- (flip runStateT) t0 $ performAction
  either (const $ mySession t1) return $ matching _Quit a

----------

main :: IO ()
main = do
  eval $ mySession def
  return ()

----------
