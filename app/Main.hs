{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Prelude()
import Relude
import Lib
--
import Network.Wai.Handler.Replica
import Network.WebSockets.Connection
import Replica.VDOM
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Map as M
import Control.Monad.Free

main :: IO ()
main = do
  Warp.run 8000 $ app "Counter" defaultConnectionOptions (1, counter) run

{-
Free で状態を持つ場合、解釈側で持つ

Event -> IO () で ちゃんと fireEvent しないと HTML 中の AEvent が呼ばれないらしい。
-}
run
  :: (Int, Free Counter a)
  -> IO (Maybe (HTML, (Int, Free Counter a), Event -> IO ()))
run (i, v) = case v of
  Pure a -> pure Nothing
  Free (View html next) -> pure $ Just (html, (i, next), \event -> fireEvent html (evtPath event) (evtType event) (DOMEvent $ evtEvent event))
  Free (GetInt next') -> run (i, next' i)
  Free (IncInt next) -> run (i+1, next)
  Free (StepIO io next') -> io >>= run . (i,) . next'

data Counter a
  = View HTML a
  | GetInt (Int -> a)
  | IncInt a
  | forall v. StepIO (IO v) (v -> a)

instance Functor Counter where
  fmap f (View html a) = View html (f a)
  fmap f (GetInt a) = GetInt (f <$> a)
  fmap f (IncInt a) = IncInt (f a)
  fmap f (StepIO io a) = StepIO io (f <$> a)

getInt :: Free Counter Int
getInt = Free $ GetInt $ \i -> Pure i

incInt :: Free Counter ()
incInt = Free $ IncInt (Pure ())

view' :: HTML -> Free Counter ()
view' html = Free $ View html (Pure ())

view :: ((e -> IO ()) -> HTML) -> Free Counter e
view act = do
  mvar <- Free $ StepIO newEmptyMVar (\v -> Pure v)
  view' $ act $ putMVar mvar
  Free $ StepIO (takeMVar mvar) (\v -> Pure v)

counter :: Free Counter ()
counter = do
  i <- getInt
  _ <- view $ \handler ->
    [ VText $ "count: " <> show i
    , VNode "button" (M.fromList [("onClick", AEvent (\ev -> handler ()))]) [ VText "increment" ]
    ]
  incInt
  counter

{-
type HTML = [VDOM]

data VDOM
  = VNode !T.Text !Attrs ![VDOM]
  | VLeaf !T.Text !Attrs
  | VText !T.Text

type Attrs = M.Map T.Text Attr

data Attr
  = AText  !T.Text
  | ABool  !Bool
  | AEvent !(DOMEvent -> IO ())
  | AMap   !Attrs
-}
