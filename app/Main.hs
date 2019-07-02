{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import Prelude()
import P
import Lib
--
import qualified Network.Wai.Handler.Replica as Rep
import Network.WebSockets.Connection
import Replica.VDOM
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.Free
import Control.Lens
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Data.Generics.Product
import Data.Generics.Labels

main :: IO ()
main = do
  Warp.run 8000 $ Rep.app "Counter" defaultConnectionOptions app run

{-
Free で状態を持つ場合、解釈側で持つ

Event -> IO () で ちゃんと fireEvent しないと HTML 中の AEvent が呼ばれないらしい。

一度目の dispatch された イベントで AEvent -> IO () で何もしなければ、二度目は
dispatch されない？これは意図したもの？

replica の駆動するところが問題か。
replica/src/Network/Wai/Handler/Replica.hs
90行目 一回しか dispatch 関数 (Event -> IO ()) が呼ばれない...
-}
run
  :: Free Counter a
  -> IO (Maybe (HTML, Free Counter a, Rep.Event -> IO ()))
run v = case v of
  Pure a -> pure Nothing
  Free (View html next) -> pure $ Just (html, next, \event -> fireEvent html (Rep.evtPath event) (Rep.evtType event) (DOMEvent $ Rep.evtEvent event))
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
?? HTML s s' e に分ける？？やりすぎかな？
s 使わせたくないなら forall s. HTML s Void なのか？
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

render_
  :: MonadReplica m
  => HTML' () e
  -> m e
render_ html = snd <$> render () html


-- TODO: NoBlock くそ見たいな名前だな...
-- 状態を更新しても効果なし
renderNoBlock
  :: MonadReplica m
  => s
  -> HTML' s Void
  -> m ()
renderNoBlock state html =
  renderHTMLNoBlock $ (unHTML' html) state (const $ pure ()) (const $ pure ())

renderNoBlock_ :: MonadReplica m => HTML' () Void -> m ()
renderNoBlock_ = renderNoBlock ()

newtype HTML' s e = HTML'
  { unHTML' :: (s -> (s -> IO ()) -> (e -> IO ()) -> HTML)
  }

instance Functor (HTML' s) where
  fmap f (HTML' g) = HTML' $ \s u e -> g s u (e . f)

instance Semigroup (HTML' s e) where
  (HTML' a) <> (HTML' b) = HTML' $ \s u e -> (a s u e) <> (b s u e)

instance Monoid (HTML' s e) where
  mempty = HTML' $ \_ _ _ -> mempty

{-
IsString は定義しないがいいかな？
text が必要なところとそうでないところが混在するとややこしい
-}

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
  -- ^ 現状イベントは1stepあたり一回しかdispatchされないので発生したイベントを無視
  -- することはできない(2019/07/01現在)。

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

textState' :: (s -> Text) -> HTML' s e
textState' f = HTML' $ \s _ _ -> [VText (f s)]

{-| helpers
-}
_updateBy :: (A.Value -> Maybe s) -> Attr' s e
_updateBy f = AEvent' $ \s ev -> Left $ fromMaybe s (f $ getDOMEvent ev)

_updateByFo :: Fold A.Value s -> Attr' s e
_updateByFo = _updateBy . preview

_emitConst :: e -> Attr' s e
_emitConst e = AEvent' $ \_ _ -> Right e

_stateValue :: Attr' Text e
_stateValue = AStext' id

{-| more high level helpers
-}

{-
`type`, `value`, `onChange` attributes are pre-defined, but you can overwrite
by passing attrsibutes.
-}
inputText :: [(Text, Attr' Text e)] -> HTML' Text e
inputText attrs =
  let
    defaultAttrs =
      [ "type" =: "text"
      , "value" =: _stateValue
      , "onChange" =: _updateByFo (key "currentTarget" . key "value" . _String)
      ]
  in leaf' "input" (defaultAttrs <> attrs)

-- 要素の名前付け法則を決めないと
-- button のように onClick を付けるのがデフォのやつもある

div_ :: [HTML' s e] -> HTML' s e
div_ = node' "div" mempty

span :: [(Text, Attr' s e)] -> [HTML' s e] -> HTML' s e
span = node' "span"

button_ :: Text -> HTML' s ()
button_ t = node' "button" [ "onClick" =: _emitConst () ] [ text' t ]

-- | Test app

{-
FLow という用語を使うか？
なかで表示しつつ最終的に値を
-}

app :: MonadReplica m => m ()
app = do
  acc <- loginFlow
  renderNoBlock_ $ div_ [ text' $ "Hello, " <> acc <> "!!" ]


-- | Stupid Login Page
data LoginInput = LoginInput
  { account :: Text
  , password :: Text
  } deriving Generic

loginFlow :: MonadReplica m => m Text
loginFlow =
  untilRight (False, LoginInput "" "") $ \(hasError, _i) -> do
    (i, _) <- render _i $ html hasError
    pure $ if validInput i
      then Right $ account i
      else Left (True, i { password = "" })
  where

    html :: Bool -> HTML' LoginInput ()
    html hasError =
      div_
        [ if hasError
          then div_ [ span [ "style" =: "color:red;" ] [ text' "ログインに失敗しました!" ] ]
          else mempty
        , zoomState #account  $ inputText [ "placeholder" =: "account" ]
        , zoomState #password $ inputText [ "type" =: "password" ]
        , withState $ \s ->
            if isInputDone s
               then button_ "login"
               else node' "button" [ "disabled" =: ABool' True ] [ text' "login" ]
        ]

    isInputDone :: LoginInput -> Bool
    isInputDone LoginInput{account, password} =
      account /= "" && password /= ""

    validInput :: LoginInput -> Bool
    validInput LoginInput{account, password} =
      account == "foo" && password == "bar"
