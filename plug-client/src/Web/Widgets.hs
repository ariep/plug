{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module Web.Widgets
  ( C, Make, Edit
  , editLens
  
  , focus
  , focusAfterBuild
  , focussed
  , hovered
  , afterBuildAsync
  , Html, html
  , editText
  , editTextAutofocus
  , editTextAutofocus'
  , editText'
  , editTextPlaceholder
  , buttonClass
  , buttonClassM
  , buttonDynAttr
  , openExternal
  , consumeEvent

  , forDynM
  , forDynM_
  , forDynEvent
  , onEvent
  , holdDynInit

  , toggleModes
  , Tab(Tab), tabs
  ) where
 
import           Control.Concurrent     (forkIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Lens           (Lens', view, set)
import           Control.Monad          (when, void)
import           Control.Monad.Reader   (ReaderT, runReaderT, ask)
import           Data.List              (find)
import qualified Data.Map                 as Map
import           Data.Monoid            ((<>))
import qualified Data.Text                as Text
import           Data.Text              (Text)
import           Data.Traversable       (for)
import           GHCJS.DOM              (currentWindow, currentDocument)
import           GHCJS.DOM.Element      (focus, getInnerHTML, setInnerHTML, setOuterHTML)
import           GHCJS.DOM.HTMLInputElement (HTMLInputElement)
import           GHCJS.DOM.Types        (Document, IsElement)
import           GHCJS.DOM.Window       (Window, open)
import qualified JavaScript.WebSockets.Reflex.WebSocket as WS
import           Reflex.Dom
import           Reflex.Dom.Widget.Basic
import           Reflex.Dom.Contrib.Widgets.Common

type C t m a
  = ReaderT (WS.Connection t) m a

type Make t m a
  = C t m (Dynamic t a)

type Edit t m a
  = a -> Make t m a

editLens :: (Reflex t, MonadHold t m) =>
  Lens' a b -> Edit t m b -> Edit t m a
editLens l e = \ a -> do
  d <- e (view l a)
  mapDyn (\ b -> set l b a) d


focusAfterBuild :: (IsElement e, MonadWidget t m) => e -> m ()
focusAfterBuild e = performEvent_ =<< fmap (liftIO (focus e) <$) getPostBuild

focussed :: (HasDomEvent t e, MonadWidget t m) => e -> m (Dynamic t Bool)
focussed e = holdDyn False . leftmost $
  [ True  <$ domEvent Focus e
  , False <$ domEvent Blur  e
  ]

hovered :: (HasDomEvent t e, MonadWidget t m) => e -> m (Dynamic t Bool)
hovered e = holdDyn False . leftmost $
  [ True  <$ domEvent Mouseover e
  , False <$ domEvent Mouseout  e
  ]

afterBuildAsync :: (MonadWidget t m) => C t IO () -> C t m ()
afterBuildAsync h = do
  con <- ask
  postBuild <- getPostBuild
  performEventAsync . ffor postBuild . const $
    \ trigger -> void . liftIO . forkIO $ runReaderT h con 
  return ()

type Html
  = String

forDynM_ :: (MonadWidget t m) => Dynamic t a -> (a -> m ()) -> m ()
forDynM_ d f = fmap (const ()) . dyn =<< mapDyn f d

forDynM :: (MonadWidget t m) => Dynamic t a -> (a -> m b) -> m (Event t b)
forDynM d f = dyn =<< mapDyn f d

forDynEvent :: (MonadWidget t m) => Dynamic t a -> (a -> m (Event t b)) -> m (Event t b)
forDynEvent d f = switchPromptly never =<< dyn =<< mapDyn f d

onEvent :: (MonadWidget t m) => Event t a -> (a -> WidgetHost m ()) -> m ()
onEvent e h = performEvent_ (h <$> e)

html :: (MonadWidget t m) => Html -> m ()
html h = do
  (e, ()) <- el' "span" blank
  setOuterHTML (_el_element e) $ Just h

editText :: (MonadWidget t m) => Text -> m (Dynamic t Text)
editText t = fst <$> editText' t

editTextAutofocus :: (MonadWidget t m) => Bool -> Edit t m Text
editTextAutofocus autofocus ti = fst <$> editTextAutofocus' autofocus ti

editTextAutofocus' :: (MonadWidget t m) =>
  Bool -> Text -> m (Dynamic t Text, TextInput t)
editTextAutofocus' autofocus i = do
  (d, ti) <- editText' i
  when autofocus $ schedulePostBuild $ focus $ _textInput_element ti
  return (d, ti)

editTextPlaceholder :: (MonadWidget t m) => Text -> m (Dynamic t Text)
editTextPlaceholder t = fst <$>
  editText_ Text.empty ("placeholder" =: Text.unpack t)

editText' :: (MonadWidget t m) => Text -> m (Dynamic t Text, TextInput t)
editText' t = editText_ t Map.empty

editText_ :: (MonadWidget t m) =>
  Text -> Map.Map String String -> m (Dynamic t Text, TextInput t)
editText_ t attrs = do
  ti <- textInput
    (def
      { _textInputConfig_initialValue = Text.unpack t 
      , _textInputConfig_attributes   = constDyn $ "class" =: "form-control" <> attrs
      })
  dt <- mapDyn Text.pack $ value ti
  return (dt, ti)

buttonClass :: MonadWidget t m => String -> String -> m (Event t ())
buttonClass c s = buttonClassM c (text s)

buttonClassM :: MonadWidget t m => String -> m () -> m (Event t ())
buttonClassM c = buttonDynAttr $ constDyn $ "class" =: c

buttonDynAttr :: MonadWidget t m =>
  Dynamic t (Map.Map String String) -> m () -> m (Event t ())
buttonDynAttr attrs s = do
  (e, _) <- elDynAttr' "button" attrs s
  return $ domEvent Click e

type Url
  = String

openExternal :: (Reflex t, MonadIO (PushM t)) =>
  Event t Url -> Event t Window
openExternal = push $ \ url -> do
  liftIO currentWindow >>= \case
    Nothing -> return Nothing
    Just w  -> liftIO $ putStrLn "opening external window..." >> open w url "export" ""

consumeEvent :: (MonadWidget t m) => Event t a -> m ()
consumeEvent e = dynText =<< holdDyn "" ("" <$ e)

toggleModes :: (MonadWidget t m) =>
  C t m (Event t ()) -> C t m (Event t ()) -> Make t m Bool
toggleModes a b = mdo
  toggleA <- forDynEvent d $ \case
    True  -> fmap (const False) <$> a
    False -> fmap (const True ) <$> b
  d <- holdDyn True toggleA
  return d
data Tab t m a
  = Tab String Bool (m (Dynamic t a))

tabs :: forall t m a. (MonadWidget t m) => [Tab t m a] -> m (Dynamic t a)
tabs ts = mdo
  let its = zip [(1 :: Int) ..] ts
  let initialI = maybe 1 fst $ find (\ (_, Tab _ initial _) -> initial) its
  activeTab <- holdDyn initialI $ leftmost clicks
  clicks <- elClass "ul" "nav nav-tabs" . for its $ \ (i, Tab title _ _) -> do
    active <- mapDyn ((==) i) activeTab
    attrs <- flip mapDyn active $ \ a -> Map.singleton "class" $
      if a then "active" else ""
    (i <$) . _link_clicked <$> elDynAttr "li" attrs (link title)
  ds :: [(Int, Dynamic t a)] <- divClass "tab-content" $
    fmap (zip [1 ..]) . for its $ \ (i, Tab _ _ content) -> do
      active <- mapDyn ((==) i) activeTab
      attrs <- flip mapDyn active $ \ a -> Map.singleton "class" $
        if a then "tab-pane active" else "tab-pane"
      elDynAttr "div" attrs content
  let iD = maybe e id $ lookup initialI ds
  aD <- forDynM activeTab $ \ i -> return . maybe e id $ lookup i ds
  joinDyn <$> holdDyn iD aD
 where
  e = error "Web.Widgets.tabs: undefined index"
holdDynInit :: (Reflex t, MonadHold t m) =>
  Dynamic t a -> Event t a -> m (Dynamic t a)
holdDynInit d e = do
  changed <- holdDyn Nothing (Just <$> e)
  combineDyn update d changed
 where
  update x Nothing  = x
  update _ (Just y) = y

