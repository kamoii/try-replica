{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
  Warp.run 8000 $ app "Counter" defaultConnectionOptions test run

run :: Free Counter a -> IO (Maybe (HTML, Free Counter a, Event -> IO ()))
run = \case
  Pure a -> pure Nothing
  Free (ShowHtml html next) -> pure $ Just (html, next, (const (pure ())))

data Counter a
  = ShowHtml HTML a

instance Functor Counter where
  fmap f (ShowHtml html a) = ShowHtml html (f a)

showHtml :: HTML -> Free Counter ()
showHtml html = Free (ShowHtml html (Pure ()))

test :: Free Counter ()
test = do
  showHtml $
    [ VText $ "count: "
    , VNode "button" (M.fromList [("onClick", AEvent (\ev -> pure ()))]) [ VText "increment" ]
    ]
  pure ()

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
