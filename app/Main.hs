{-# LANGUAGE FlexibleInstances #-}
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
  Warp.run 8000 $ app "Counter" defaultConnectionOptions (counter 1) run

{-
Free で状態を持つ場合、解釈側で持つ

Event -> IO () で ちゃんと fireEvent しないと HTML 中の AEvent が呼ばれないらしい。
-}
run
  :: Free Counter a
  -> IO (Maybe (HTML, Free Counter a, Event -> IO ()))
run v = case v of
  Pure a -> pure Nothing
  Free (View html next) -> pure $ Just (html, next, \event -> fireEvent html (evtPath event) (evtType event) (DOMEvent $ evtEvent event))
  Free (StepIO io next') -> io >>= run . next'

data Counter a
  = View HTML a
  | forall v. StepIO (IO v) (v -> a)

instance Functor Counter where
  fmap f (View html a) = View html (f a)
  fmap f (StepIO io a) = StepIO io (f <$> a)

instance MonadIO (Free Counter) where
  liftIO io = Free $ StepIO io (\v -> Pure v)

view' :: HTML -> Free Counter ()
view' html = Free $ View html (Pure ())

view :: ((e -> IO ()) -> HTML) -> Free Counter e
view act = do
  mvar <- liftIO newEmptyMVar
  view' $ act $ putMVar mvar
  liftIO $ takeMVar mvar

updater
  :: s
  -> (s -> ((s -> s) -> IO ()) -> (e -> IO ()) -> HTML)
  -> Free Counter (s, e)
updater state act = undefined

counter :: Int -> Free Counter ()
counter i = do
  _ <- view $ \handler ->
    [ VText $ "count: " <> show i
    , VNode "button" (M.fromList [("onClick", AEvent (\ev -> handler ()))]) [ VText "increment" ]
    ]
  counter (i+1)

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
