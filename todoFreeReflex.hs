{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.Fix
import Control.Monad.Free
import Data.Default
import qualified Data.Map as M
import qualified GHC.Generics as GHC
import Reflex
import Reflex.Dom

----------

type Task = String
type Id = Int
type IdPick = (Id, (Task, [Task]))

---

data Cmd = Cmd_Quit
         | Cmd_AddTask
         | Cmd_DeleteTask
  deriving (Show, Read, Eq, Ord, Enum, Bounded, GHC.Generic)
makePrisms ''Cmd

---

data Updt = Updt_Quit [Task]
          | Updt_AddTask Task
          | Updt_DeleteTask IdPick
  deriving (Show, Read, Eq, GHC.Generic)
makePrisms ''Updt

---

data Upgr = Upgr_CmdMay (Maybe Cmd)
          | Upgr_Updt Updt
  deriving (Show, Read, Eq, GHC.Generic)
makePrisms ''Upgr

instance Default Upgr where
  def = Upgr_CmdMay def
 
----------

data Config = Config { _config_maxMenuItemLength :: Int
                     , _config_suffix :: String
                     , _config_commands :: M.Map (Maybe Cmd) String
                     }
  deriving (Show, Read, Eq, GHC.Generic)
makeLenses ''Config

myCommands :: M.Map (Maybe Cmd) String 
myCommands = M.fromList $ [ (Cmd_Quit, "Quit")
                          , (Cmd_AddTask, "Add task")
                          , (Cmd_DeleteTask, "Delete task")
                          ] & traverse . _1 %~ review _Just
  
instance Default Config where
  def = Config { _config_maxMenuItemLength = 16
               , _config_suffix = "..."
               , _config_commands = myCommands
               }

---

data Todo = Todo { _todo_tasks :: [Task]
                 }
  deriving (Show, Read, Eq, GHC.Generic)
makeLenses ''Todo

instance Default Todo where
  def = Todo { _todo_tasks = []
             }

---

data View = View { _view_isHidden :: Bool
                 }
  deriving (Show, Read, Eq, GHC.Generic)
makeLenses ''View

instance Default View where
  def = View { _view_isHidden = False
             }

----------
----------

data Vocab i o a where
  GetIn :: o -> (i -> a) -> Vocab i o a
  deriving (GHC.Generic)

instance Functor (Vocab i o) where
  fmap f = \case
    GetIn o g -> GetIn o (f . g)

getIn :: MonadFree (Vocab i o) m => o -> m i
getIn o = liftF $ GetIn o id

freeRun :: (i -> o -> o) -> o -> Free (Vocab i o) ()
freeRun f o = do
  i <- getIn o
  freeRun f $ f i o
  
stepFree :: i -> Free (Vocab i o) () -> Free (Vocab i o) ()
stepFree i fv = case fv of
  Pure () -> fv
  Free w -> case w of
    GetIn _ g -> g i 

projOutMay :: Free (Vocab i o) () -> Maybe o
projOutMay = \case
  Pure () -> Nothing
  Free w -> case w of
    GetIn o _ -> Just o

-- foldDyn via a free monad
freeFoldDyn :: (MonadFix m, Reflex t, MonadHold t m)
            => (i -> o -> o) -> o -> Event t i -> m (Dynamic t o)
freeFoldDyn f o e = do 
  dFree <- foldDyn stepFree (freeRun f o) e 
  dOutMay <- mapDyn projOutMay dFree
  holdDyn o $ fmapMaybe id (updated dOutMay)

----------
----------

trunc :: String -> String
trunc xs =
  let n = def ^. config_maxMenuItemLength
      sfx = def ^. config_suffix
   in case (compare (length xs) n) of
        GT -> (take n xs) ++ sfx
        EQ -> xs
        LT -> xs

picks :: [a] -> [(a, [a])]    -- preserves cardinality
picks = \case
  [] -> []
  x : xs -> (x, xs) : ((picks xs) & traverse._2 %~ (x :))

myTaskIdPicks :: Todo -> M.Map (Maybe IdPick) String
myTaskIdPicks s0 =
  let ts = reverse $ s0 ^. todo_tasks   -- [Task]
      ps = picks ts   -- [(Task, [Task])]
      ns = [1 ..]   -- [Id]
      vs = fmap show $ zip ns $ fmap trunc ts -- [String]
      ks = zip ns ps  -- [(Id, (Task, [Task]))]
      zs = zip ks vs & traverse . _1 %~ review _Just
   in M.fromList zs

----------

nextCommandMay :: MonadWidget t m => m (Event t (Maybe Cmd))
nextCommandMay = do 
  text "Please select the next command: "
  dd <- dropdown Nothing (constDyn $ def ^. config_commands) def
  return $ updated (value dd)

---

confirmQuit :: MonadWidget t m => Todo -> m (Event t [Task])
confirmQuit s0 = do
  el "div" $ text "Please confirm quit. "
  b <- button "Confirm"
  return $ tag (constant $ s0 ^. todo_tasks) b

askTask :: MonadWidget t m => m (Event t Task)
askTask = do
  el "div" $ text "Please describe the task: "
  i <- el "div" $ textInput def
  b <- button "Submit"
  return $ ffilter (/= "") $ tag (current (value i)) b

askTaskIdPick :: MonadWidget t m => Todo -> m (Event t IdPick)
askTaskIdPick s0 = do
  el "div" $ text "Please select an item from the list: " 
  d <- dropdown Nothing (constDyn $ myTaskIdPicks s0) def
  b <- button "Remove" 
  return $ fmapMaybe id $ tag (current (value d)) b

---

processTodoCommand :: MonadWidget t m => (Todo, Cmd) -> m (Event t Updt)
processTodoCommand (s0, c)  = case c of 
  Cmd_Quit -> do 
    e <- confirmQuit s0
    return $ fmap (review _Updt_Quit) e
  Cmd_AddTask -> el "div" $ do
    e <- askTask
    return $ fmap (review _Updt_AddTask) e
  Cmd_DeleteTask -> do
    e <- askTaskIdPick s0
    return $ fmap (review _Updt_DeleteTask) e

processTodoCommandMay :: MonadWidget t m => (Todo, Maybe Cmd) -> m (Event t Updt)
processTodoCommandMay (s0, mc) = case mc of
  Nothing -> return $ never
  Just c -> processTodoCommand (s0, c)

stepTodo :: Updt -> Todo -> Todo
stepTodo u s0 = case u of
  Updt_Quit _ -> s0 & todo_tasks .~ (def ^. todo_tasks)
  Updt_AddTask x -> s0 & todo_tasks %~ (x :)
  Updt_DeleteTask (_, (_, xs)) -> s0 & todo_tasks .~ (reverse xs)

controlTodo :: MonadWidget Spider m
            => (Todo, Maybe Cmd) -> m (Event Spider (Todo, Upgr))
controlTodo (td, mc) = do
  rec eCmdMay <- nextCommandMay
      dCmdMay <- holdDyn mc eCmdMay
      dTodoCmdMay <- combineDyn (,) dTodo dCmdMay
      deUpdt <- widgetHold (return never) $
                  fmap processTodoCommandMay $ updated dTodoCmdMay
      let eUpdt :: Event Spider Updt
          eUpdt = switchPromptlyDyn deUpdt

      dTodo <- freeFoldDyn stepTodo td eUpdt   -- free monad
--       dTodo <- foldDyn stepTodo td eUpdt


  let eUpgr = mergeWith const $ 
        [ fmap (review _Upgr_CmdMay) eCmdMay
        , fmap (review _Upgr_Updt) eUpdt
        ]    -- leftmost

  return $ attachDyn dTodo eUpgr

----------

isUpdtQuit :: (Todo, Upgr) -> Maybe Todo
isUpdtQuit (td, ug) = 
  either (const Nothing) (const $ Just td) $
    matching (_Upgr_Updt . _Updt_Quit) ug

---

todoUpgr :: (t ~ Spider, MonadWidget t m)
         => (Todo, Maybe Cmd) -> m (Event t (Todo, Upgr))
todoUpgr (td, mc) = do 
  rec let eTodoQ = fmapMaybe isUpdtQuit eTodoUpgr 

--    deTodoUpgr :: Dynamic Spider (Event Spider (Todo, Upgr))
      deTodoUpgr <- widgetHold (controlTodo (td, mc)) $
                      fmap (\x -> controlTodo (x, mc)) eTodoQ

      let eTodoUpgr :: Event Spider (Todo, Upgr)
          eTodoUpgr = switchPromptlyDyn deTodoUpgr

  return eTodoUpgr

----------

buttonT :: (t ~ Spider, MonadWidget t m)
        => (Bool -> String) -> Bool -> m (Event t ())
buttonT f x = do
  rec d <- freeFoldDyn (const $ not) x e1   -- free monad 
--      d <- foldDyn (const $ not) x e1    -- toggle
            
      d1 <- widgetHold (button $ f x) $
              fmap (button . f) (updated d)
      let e1 :: Event Spider ()
          e1 = switchPromptlyDyn d1
  return e1

---

stepView :: () -> View -> View
stepView () v = v & view_isHidden %~ not

buttonHideShow :: (t ~ Spider, MonadWidget t m)
               => View -> m (Event t View)
buttonHideShow v0 = do
  b <- buttonT (\x -> case x of
                        False -> "Hide"
                        True -> "Show") $ v0 ^. view_isHidden

  d <- freeFoldDyn stepView v0 b    -- free monad
--   d <- foldDyn stepView v0 b  

  return $ updated d

dispTodoView :: (t ~ Spider, MonadWidget t m)
             => (Todo, View) -> m (Event t View) 
dispTodoView (td, v0) = do
  let ns = [(1 :: Id) ..]
      zs = fmap show $ zip ns $ reverse $ td ^. todo_tasks
   in do
--       e :: Event Spider View
         e <- el "div" $ do
           el "em" $ text "Current todo list: "
           buttonHideShow v0
      
         case (v0 ^. view_isHidden) of
           False -> case zs of
                      [] -> return () 
                      _ : _ -> mapM_ (el "div" . text) zs
           True -> return ()

         el "div" $ text $ "You have " ++ (show $ length zs) ++ " task(s) in the list." 

         return e

todoView :: (t ~ Spider, MonadWidget t m)
         => (Todo, View) -> Event t Todo -> m (Event t View)
todoView (td0, v0) eTodo = do
  dTodo <- holdDyn td0 eTodo
  rec dTodoView <- combineDyn (,) dTodo dView
--    deView :: Dynamic Spider (Event Spider View)
      deView <- widgetHold (dispTodoView (td0, v0)) $
                  fmap dispTodoView (updated dTodoView)
      let eView :: Event Spider View
          eView = switchPromptlyDyn deView
      dView <- holdDyn v0 eView
  return eView

----------

dispUpgr :: MonadWidget t m => Upgr -> m ()
dispUpgr = \case 
    Upgr_CmdMay _ -> return ()
    Upgr_Updt u -> do
      el "div" $ el "em" $ text $ "Status update: "
      case u of
        Updt_Quit xs -> do
          el "div" $ text $ "Quit: " ++ (show $ zip [(1 :: Id) ..] (reverse xs))
        Updt_AddTask x -> do
          el "div" $ text $ "Task " ++ (show x) ++ "added to the list."
        Updt_DeleteTask (n, (x, _)) -> do
          el "div" $ text $ "Task " ++ (show n) ++ " " ++ (show x) ++ " deleted from the list."

statusUpdate :: MonadWidget t m
             => Upgr -> Event t Upgr -> m (Event t ())
statusUpdate ug0 e = do
  d <- widgetHold (dispUpgr ug0) $ fmap dispUpgr e
  return $ updated d 

----------

main :: IO ()
main = mainWidget $ el "div" $ do

  eTodoUpgr <- todoUpgr (def, def)

  let eTodo = fmap fst eTodoUpgr
  _ <- todoView (def, def) eTodo

  let eUpgr = fmap snd eTodoUpgr
  _ <- statusUpdate def eUpgr

  return ()

----------
