{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
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
  Warp.run 8000 $ app "Counter" defaultConnectionOptions (1, test) run

{-
Free で状態を持つ場合、解釈側で持つ
-}
run
  :: (Int, Free Counter a)
  -> IO (Maybe (HTML, (Int, Free Counter a), Event -> IO ()))
run (i, v) = case v of
  Pure a -> pure Nothing
  Free (ShowHtml html next) -> pure $ Just (html, (i, next), (const (pure ())))
  Free (GetInt next) -> run $ (i, next i)

data Counter a
  = ShowHtml HTML a
  | GetInt (Int -> a)

instance Functor Counter where
  fmap f (ShowHtml html a) = ShowHtml html (f a)
  fmap f (GetInt a) = GetInt (f <$> a)

showHtml :: HTML -> Free Counter ()
showHtml html = Free (ShowHtml html (Pure ()))

getInt :: Free Counter Int
getInt = Free (GetInt $ \i -> Pure i)

test :: Free Counter ()
test = do
  i <- getInt
  showHtml $
    [ VText $ "count: " <> show i
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
