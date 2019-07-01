{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import qualified Data.Text as T
import Control.Monad.Free
import Control.Lens hiding (view)
import qualified Data.Aeson as A
import Data.Aeson.Lens

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

class MonadIO m => MonadReplica m where
  renderHTMLNoBlock :: HTML -> m ()

instance MonadReplica (Free Counter) where
  renderHTMLNoBlock html = Free $ View html (Pure ())

renderHTML :: MonadReplica m => ((e -> IO ()) -> HTML) -> m e
renderHTML act = do
  mvar <- liftIO newEmptyMVar
  renderHTMLNoBlock $ act $ putMVar mvar
  liftIO $ takeMVar mvar

{-| HTML' s e

type HTML = [VDOM] ということに注意
-}
render
  :: MonadReplica m
  => s
  -> HTML' s e
  -> m (s, e)
render state html = _render state (unHTML' html)
  where
    _render
      :: MonadReplica m
      => s
      -> (s -> (s -> IO ()) -> (e -> IO ()) -> HTML)
      -> m (s, e)
    _render state act = do
      se <- renderHTML $ \f -> act state (f . Left) (f . Right)
      case se of
        Left us -> _render us act
        Right e -> pure (state, e)

-- 状態を更新しても効果なし
renderNoBlock
  :: MonadReplica m
  => s
  -> HTML' s Void
  -> m ()
renderNoBlock state html =
  renderHTMLNoBlock $ (unHTML' html) state (const $ pure ()) (const $ pure ())

newtype HTML' s e = HTML'
  { unHTML' :: (s -> (s -> IO ()) -> (e -> IO ()) -> HTML)
  }

instance Functor (HTML' s) where
  fmap f (HTML' g) = HTML' $ \s u e -> g s u (e . f)

instance Semigroup (HTML' s e) where
  (HTML' a) <> (HTML' b) = HTML' $ \s u e -> (a s u e) <> (b s u e)

instance Monoid (HTML' s e) where
  mempty = HTML' $ \_ _ _ -> mempty

-- ある状態を取り出して条件分岐とかしたい場合など
withState :: (s -> HTML' s e) -> HTML' s e
withState f = HTML' $ \s u e -> (unHTML' (f s)) s u e

zoomState :: Lens' s s' -> HTML' s' e -> HTML' s e
zoomState l (HTML' f) = HTML' $ \s u e -> f (s ^. l) (\s' -> u $ s & l .~ s') e

{-
AUpdate' !(DOMEvent -> s), AEvent' !(DOMEvent -> e) と分けてしまうと、一つのイベントに対して
状況によって s/e どちらを返すか変更できないので。
利便性のために IsString を用意しておくか？
-}
data Attr' s e
  = AText'  !Text
  | ABool'  !Bool
  | AStext' !(s -> Text)
  | AEvent' !(s -> DOMEvent -> Either s e)

-- profunctor?かな
-- instance Functor (Attr' s)

instance IsString (Attr' s e) where
  fromString = AText' . toText

toAttr :: s -> (s -> IO ()) -> (e -> IO ()) -> Attr' s e -> Attr
toAttr s u e = \case
  AText' t  -> AText t
  ABool' b  -> ABool b
  AStext' f -> AText (f s)
  AEvent' f -> AEvent $ f s <&> either u e

(=:) = (,)

node' :: Text -> [(Text, Attr' s e)] -> [HTML' s e] -> HTML' s e
node' name attrs children = HTML' $ \s u e ->
  [ VNode name
      (fmap (toAttr s u e) (M.fromList attrs))
      ((unHTML' (mconcat children)) s u e)
  ]

leaf' :: Text -> [(Text, Attr' s e)] -> HTML' s e
leaf' name attrs = HTML' $ \s u e ->
  [ VLeaf name (fmap (toAttr s u e) $ M.fromList attrs) ]

text' :: Text -> HTML' s e
text' t = HTML' $ \_ _ _ -> [VText t]

stext' :: (s -> Text) -> HTML' s e
stext' f = HTML' $ \s _ _ -> [VText (f s)]

{-
helpers
-}
attrUpdateBy :: (A.Value -> Maybe s) -> Attr' s e
attrUpdateBy f = AEvent' $ \s ev -> Left $ fromMaybe s (f $ getDOMEvent ev)

attrUpdateByFo :: Fold A.Value s -> Attr' s e
attrUpdateByFo = attrUpdateBy . preview

attrEmitConst :: e -> Attr' s e
attrEmitConst e = AEvent' $ \_ _ -> Right e

attrState :: Attr' Text e
attrState = AStext' id

{-
名前を入力させる
-}
yourName :: MonadReplica m => m Text
yourName = do
  (name, ()) <- render ""
    $ node' "div" mempty
        [ leaf' "input"
            [ "type" =: "text"
            , "placeholder" =: "name"
            , "value" =: attrState
            , "onChange" =: attrUpdateByFo (key "currentTarget" . key "value" . _String . to (T.take 5))
            ]
        , node' "button" [ "onClick" =: attrEmitConst () ] [ text' "done" ]
        ]
  pure name

counter :: Int -> Free Counter ()
counter i = do
  name <- yourName
  _ <- renderHTML $ \handler ->
    [ VText $ name <> "'s count: " <> show i
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
