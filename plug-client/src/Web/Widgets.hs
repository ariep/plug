{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Widgets
  ( C, Make, Edit
  , runC'
  , Address, Tag
  , editLens

  , focus
  , focusAfterBuild
  , focussed
  , hovered
  , clickable
  , clickable'
  , asyncEvent
  , asyncEventIO
  , afterBuildAsync
  -- , Html, html
  , html
  , editDynamic
  , editText
  , editTextAutofocus
  , editTextAutofocus'
  , editText'
  , editTextPlaceholder
  , buttonClass
  , buttonClassM
  , buttonDynAttr
  , leftRightAlign
  , openExternal
  , getHost
  , getProtocol
  , getWebsocketServer
  , consumeEvent

  , dynEvent
  , onEvent
  , holdDynInit

  , toggleModes
  , toggleModesDyn

  , Tab(Tab), tabs
  ) where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (newChan, readChan, writeChan)
import           Control.Monad           (forever)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Lens            (Lens', view, set)
import           Control.Monad           (join, when, void)
import           Control.Monad.Exception (MonadException, MonadAsyncException)
import           Control.Monad.Fix       (MonadFix)
import           Control.Monad.Reader    (MonadReader, ReaderT(ReaderT), runReaderT, ask)
import           Control.Monad.Ref       (MonadRef, Ref, newRef, readRef, writeRef, modifyRef, modifyRef')
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.Control (MonadTransControl, StT, liftWith, restoreT)
import           Data.Functor.Misc       (Const2)
import           Data.List               (find)
import qualified Data.Map                 as Map
import           Data.Monoid             ((<>))
import qualified Data.Text                as Text
import           Data.Text               (Text)
import           Data.Traversable        (for)
import           GHCJS.DOM               (currentWindow, currentDocument)
import           GHCJS.DOM.Element       (focus, getInnerHTML, setInnerHTML, setOuterHTML)
import           GHCJS.DOM.HTMLInputElement (HTMLInputElement)
import           GHCJS.DOM.Types         (Document, IsElement)
import           GHCJS.DOM.Window        (Window, open)
import qualified JavaScript.WebSockets.Reflex.WebSocket as WS
import           Reflex.Dom hiding (Window, Key(Tab))
import           Reflex.Dom.Contrib.Widgets.Common
import           Reflex.Dom.Location     (getLocationHost)
import           Reflex.Dom.Widget.Basic
import           Reflex.Host.Class       (MonadReflexCreateTrigger, newEventWithTrigger, newFanEventWithTrigger)


newtype C t m a
  = C (ReaderT (Env t) m a)
  deriving
    ( Functor, Applicative, Monad, MonadTrans, MonadFix
    , MonadIO, MonadException, MonadAsyncException
    -- , MonadReader (Env t), HasDocument, HasWebView
    , MonadReader (Env t)
    , MonadSample t, MonadHold t
    , TriggerEvent t
    )
runC :: C t m a -> ReaderT (Env t) m a
runC (C h) = h
instance MonadTransControl (C t) where
    type StT (C t) a = a
    liftWith f = C . ReaderT $ \r -> f $ \t -> runReaderT (runC t) r
    restoreT = C . ReaderT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}
instance (DomBuilder t m) => DomBuilder t (C t m) where
  type DomBuilderSpace (C t m) = DomBuilderSpace m
  element t cfg child = liftWith $ \run -> element t (liftElementConfig cfg) $ run child
  selectElement cfg child = do
    let cfg' = cfg
          { _selectElementConfig_elementConfig = liftElementConfig $ _selectElementConfig_elementConfig cfg
          }
    liftWith $ \run -> selectElement cfg' $ run child
instance (PostBuild t m) => PostBuild t (C t m) where
  getPostBuild = lift getPostBuild
instance (HasWebView m) => HasWebView (C t m) where
  type WebViewPhantom (C t m) = WebViewPhantom m
  askWebView = lift askWebView
instance PerformEvent t m => PerformEvent t (C t m) where
  type Performable (C t m) = C t (Performable m)
  performEvent_ e = do
    r <- ask
    lift $ performEvent_ $ (flip runReaderT r . runC) <$> e
  performEvent e = do
    r <- ask
    lift $ performEvent $ (flip runReaderT r . runC) <$> e
instance (Reflex t, MonadAdjust t m) => MonadAdjust t (C t m) where
  runWithReplace a0 a' = do
    r <- ask
    lift $ runWithReplace (runReaderT (runC a0) r) $ fmap ((`runReaderT` r) . runC) a'
  traverseDMapWithKeyWithAdjust f dm0 dm' = do
    r <- ask
    lift $ traverseDMapWithKeyWithAdjust (\k v -> runReaderT (runC $ f k v) r) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    r <- ask
    lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> runReaderT (runC $ f k v) r) dm0 dm'
-- instance (HasPostGui t h m) => HasPostGui t h (C t m) where
--   askPostGui = lift askPostGui
--   askRunWithActions = lift askRunWithActions
instance (MonadReflexCreateTrigger t m) =>
  MonadReflexCreateTrigger t (C t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer
-- instance MonadWidget t m => MonadWidget t (C t m) where
--   type WidgetHost (C t m) = WidgetHost m
--   type GuiAction (C t m) = GuiAction m
--   askParent = lift askParent
--   subWidget n w = do
--     r <- ask
--     lift $ subWidget n $ runC' r w
--   subWidgetWithVoidActions n w = do
--     r <- ask
--     lift $ subWidgetWithVoidActions n $ runC' r w
--   liftWidgetHost = lift . liftWidgetHost
--   schedulePostBuild = lift . schedulePostBuild
--   addVoidAction = lift . addVoidAction
--   getRunWidget = do
--     r <- ask
--     runWidget <- lift getRunWidget
--     return $ \rootElement w -> do
--       (a, postBuild, voidActions) <- runWidget rootElement $ runC' r w
--       return (a, postBuild, voidActions)
instance MonadRef m => MonadRef (C t m) where
  type Ref (C t m) = Ref m

  newRef     r   = lift $ newRef     r
  readRef    r   = lift $ readRef    r
  writeRef   r x = lift $ writeRef   r x
  modifyRef  r f = lift $ modifyRef  r f
  modifyRef' r f = lift $ modifyRef' r f

runC' :: Env t -> C t m a -> m a
runC' e (C r) = runReaderT r e

type Env t
  = (WS.Connection t, EventSelector t (Const2 Address Text))

type Address
  = ([Text], Maybe Tag)

type Tag
  = Text

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

focussed :: (HasDomEvent t e 'FocusTag, HasDomEvent t e 'BlurTag, MonadWidget t m) => e -> m (Dynamic t Bool)
focussed e = holdDyn False . leftmost $
  [ True  <$ domEvent Focus e
  , False <$ domEvent Blur  e
  ]

hovered :: (HasDomEvent t e 'MouseoverTag, HasDomEvent t e 'MouseoutTag, MonadWidget t m) => e -> m (Dynamic t Bool)
hovered e = holdDyn False . leftmost $
  [ True  <$ domEvent Mouseover e
  , False <$ domEvent Mouseout  e
  ]

clickable :: (MonadWidget t m) => m a -> m (Event t ())
clickable = fmap fst . clickable'

clickable' :: (MonadWidget t m) => m a -> m (Event t (), a)
clickable' h = do
  (e, a) <- el' "div" h
  return (domEvent Click e, a)

asyncEvent :: (MonadWidget t m) => m (Event t a, a -> m ())
asyncEvent = do
  chan <- liftIO newChan
  postBuild <- getPostBuild
  event <- performEventAsync . ffor postBuild . const $
    \ trigger -> liftIO . void . forkIO . forever $
      readChan chan >>= trigger
  return (event, liftIO . writeChan chan)
asyncEventIO :: (MonadWidget t m) => m (Event t a, a -> IO ())
asyncEventIO = do
  chan <- liftIO newChan
  postBuild <- getPostBuild
  event <- performEventAsync . ffor postBuild . const $
    \ trigger -> liftIO . void . forkIO . forever $
      readChan chan >>= trigger
  return (event, writeChan chan)

afterBuildAsync ::
  ( PostBuild t m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadIO (Performable m)
  ) => C t IO () -> C t m ()
afterBuildAsync h = do
  con <- ask
  postBuild <- getPostBuild
  performEventAsync . ffor postBuild . const $
    \ trigger -> void . liftIO . forkIO $ runC' con h
  return ()

type Html
  = Text

dynEvent :: (MonadWidget t m) => Dynamic t (m (Event t a)) -> m (Event t a)
dynEvent = (switchPromptly never =<<) . dyn

onEvent :: (MonadWidget t m) => Event t a -> (a -> WidgetHost m ()) -> m ()
onEvent e h = performEvent_ (h <$> e)

html :: (MonadWidget t m) => Html -> m ()
html h = do
  (e, ()) <- el' "span" blank
  setOuterHTML (_el_element e) $ Just h

editDynamic :: (MonadWidget t m) =>
  Edit t m a -> Dynamic t a -> C t m (Dynamic t a)
editDynamic e d = fmap join . holdDyn d =<< dyn (fmap e d)

editText :: (MonadWidget t m) => Text -> m (Dynamic t Text)
editText t = fst <$> editText' t

editTextAutofocus :: (MonadWidget t m) => Bool -> Edit t m Text
editTextAutofocus autofocus ti = fst <$> editTextAutofocus' autofocus ti

editTextAutofocus' :: (MonadWidget t m) =>
  Bool -> Text -> m (Dynamic t Text, TextInput t)
editTextAutofocus' autofocus i = do
  (d, ti) <- editText' i
  when autofocus $ do
    postBuild <- getPostBuild
    schedulePostBuild $ focus $ _textInput_element ti
  return (d, ti)

editTextPlaceholder :: (MonadWidget t m) => Text -> m (Dynamic t Text)
editTextPlaceholder t = fst <$>
  editText_ Text.empty ("placeholder" =: t)

editText' :: (MonadWidget t m) => Text -> m (Dynamic t Text, TextInput t)
editText' t = editText_ t Map.empty

editText_ :: (MonadWidget t m) =>
  Text -> Map.Map Text Text -> m (Dynamic t Text, TextInput t)
editText_ t attrs = do
  ti <- textInput
    (def
      { _textInputConfig_initialValue = t
      , _textInputConfig_attributes   = constDyn $ "class" =: "form-control" <> attrs
      })
  return (value ti, ti)

buttonClass :: (MonadWidget t m) => Text -> Text -> m (Event t ())
buttonClass c s = buttonClassM c (text s)

buttonClassM :: (MonadWidget t m) => Text -> m () -> m (Event t ())
buttonClassM c = buttonDynAttr $ constDyn $ "class" =: c

buttonDynAttr :: (MonadWidget t m) =>
  Dynamic t (Map.Map Text Text) -> m () -> m (Event t ())
buttonDynAttr attrs s = do
  (e, _) <- elDynAttr' "button" attrs s
  return $ domEvent Click e

leftRightAlign :: (MonadWidget t m) => m a -> m b -> m (a, b)
leftRightAlign left right = divClass "lr-align" $ do
  a <- el "div" left
  b <- el "div" right
  return (a, b)

type Url
  = Text

openExternal :: (Reflex t, MonadIO (PushM t)) =>
  Text -> Event t Url -> Event t Window
openExternal name = push $ \ url -> liftIO currentWindow >>= \case
  Nothing -> return Nothing
  Just w  -> do
    liftIO $ putStrLn "opening external window..."
    open w url name Text.empty

getHost :: (MonadWidget t m) => m Text
getHost = liftIO . getLocationHost =<< askWebView

getProtocol :: (MonadWidget t m) => m Text
getProtocol = liftIO . getLocationProtocol =<< askWebView

getWebsocketServer :: (MonadWidget t m) => m Text
getWebsocketServer = do
  host <- getHost
  protocol <- getProtocol
  let ws = case protocol of
        "http:"  -> "ws://"
        "https:" -> "wss://"
  return $ ws <> host <> "/"

consumeEvent :: (MonadWidget t m) => Event t a -> m ()
consumeEvent e = dynText =<< holdDyn "" ("" <$ e)

toggleModes :: (MonadWidget t m) =>
  C t m (Event t ()) -> C t m (Event t ()) -> Make t m (Either () ())
toggleModes a b = toggleModesDyn (Left ()) (Left ())
  (const $ flip (,) (constDyn ()) <$> a)
  (const $ flip (,) (constDyn ()) <$> b)

toggleModesDyn :: (MonadWidget t m) =>
  Either l r ->
  Either a b ->
  (l -> C t m (Event t r, Dynamic t a)) ->
  (r -> C t m (Event t l, Dynamic t b)) ->
  Make t m (Either a b)
toggleModesDyn initState initResult a b = mdo
  event :: Event t (Event t (Either l r), Dynamic t (Either a b))
    <- dyn $ ffor state $ \case
      Left l  -> do
        (e, d) <- a l
        return (fmap Right e, Left <$> d)
      Right r -> do
        (e, d) <- b r
        return (fmap Left e, Right <$> d)
  state :: Dynamic t (Either l r)
    <- holdDyn initState =<< switchPromptly never (fst <$> event)
  result :: Dynamic t (Either a b) <-
    joinDyn <$> holdDyn (constDyn initResult) (snd <$> event)
  return result

data Tab t m a
  = Tab Text Bool (m (Dynamic t a))

-- Use Reflex.Dom.Widget.Basic.tabDisplay instead
tabs :: forall t m a. (MonadWidget t m) => [Tab t m a] -> m (Dynamic t a)
tabs ts = mdo
  let its = zip [(1 :: Int) ..] ts
  let initialI = maybe 1 fst $ find (\ (_, Tab _ initial _) -> initial) its
  activeTab :: Dynamic t Int <- holdDyn initialI $ leftmost clicks
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
  let aD = (\ i -> maybe e id $ lookup i ds) =<< activeTab
  return aD
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

