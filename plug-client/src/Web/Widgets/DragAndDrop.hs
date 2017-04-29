{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Widgets.DragAndDrop where

import Web.Widgets

import           Control.Concurrent      (forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever, void)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Foldable           (for_)
import qualified Data.Map                 as Map
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import           Data.Traversable        (for)
import           GHCJS.DOM.DataTransfer  (setData)
import           GHCJS.DOM.Element       (mouseUp, mouseDown, dragStart, dragEnd, dragEnter, dragLeave, dragOver, drop, setAttribute)
import           GHCJS.DOM.EventM        (on, event, preventDefault, target)
import           GHCJS.DOM.Node          (Node, contains)
import           GHCJS.DOM.MouseEvent    (getDataTransfer)
import           Reflex.Dom hiding (preventDefault)
import           Reflex.Dom.Widget.Basic
import           Reflex.Dom.Contrib.Widgets.Common

import Prelude hiding (drop)


data Dragzone m b
  = Entire ((forall a. m a -> m a) -> m b)
  | Handle ((forall a. m a -> m a) -> m b)

data DND t m drag drop
  = DND
    { dragzone       :: forall b. drag -> Dragzone m b -> m b
    , dropzoneFilter :: (drag -> drop -> Bool) -> drop ->
        m (Event t (drag, drop))
    }

dropzone :: DND t m drag drop -> drop -> m (Event t (drag, drop))
dropzone dnd = dropzoneFilter dnd (const $ const True)

dragAndDrop :: forall t m drag drop. (MonadWidget t m) =>
  m (DND t m drag drop)
dragAndDrop = do
  mvar <- liftIO $ newEmptyMVar
  postBuild <- getPostBuild
  dragE <- performEventAsync . ffor postBuild . const $ \ trigger ->
    liftIO . void . forkIO $ forever $ takeMVar mvar >>= trigger
  dragging <- holdDyn Nothing dragE
  return $ DND (dragzone mvar) (dropzone dragging)
 where
  dragzone mvar drag (Handle h) = h $ \ body -> mdo
    (handle, r) <- elAttr' "div" ("class" =: "dnd-handle") $ do
      let h = _el_element handle
      result <- body
      (consumeEvent =<<) . wrapDomEvent h (`on` mouseDown) $ do
        t :: Maybe Node <- target
        contains h t >>= setAttribute h ("draggable" :: Text) . show
      (consumeEvent =<<) . wrapDomEvent h (`on` dragStart) $ do
        liftIO $ putMVar mvar (Just drag)
        (event >>= getDataTransfer >>=) . flip for_ $ \ dt ->
          setData dt ("text/plain" :: Text) ("unused" :: Text)
      (consumeEvent =<<) . wrapDomEvent h (`on` dragEnd) $
        liftIO $ putMVar mvar Nothing
      return result
    return r
  dragzone mvar drag (Entire m) = m $ \ draggable -> mdo
    (handle, r) <- elAttr' "div"
      ("class" =: "dnd-draggable" <> "draggable" =: "true") $ do
        let h = _el_element handle
        result <- draggable
        (consumeEvent =<<) . wrapDomEvent h (`on` dragStart) $ do
          liftIO $ putMVar mvar (Just drag)
          (event >>= getDataTransfer >>=) . flip for_ $ \ dt ->
            setData dt ("text/plain" :: Text) ("unused" :: Text)
        (consumeEvent =<<) . wrapDomEvent h (`on` dragEnd) $
          liftIO $ putMVar mvar Nothing
        return result
    return r

  dropzone dragging dndFilter dropItem = mdo
    (e, ()) <- elDynAttr' "div" attrs $ blank
    (consumeEvent =<<) . wrapDomEvent (_el_element e) (`on` dragOver) $
      preventDefault
    enterE <- wrapDomEvent (_el_element e) (`on` dragEnter) $ return ()
    leaveE <- wrapDomEvent (_el_element e) (`on` dragLeave) $ return ()
    overD <- holdDyn False $ leftmost [True <$ enterE, False <$ leaveE]
    eligibleD <- mapDyn (maybe False $ flip dndFilter dropItem) dragging
    attrs <- (\ f -> combineDyn f eligibleD overD) $ \ eligible over ->
      Map.singleton "class" $
        (if eligible then (<> " dnd-eligible") else id) $
        (if over     then (<> " dnd-over"    ) else id) $
          "dnd-dropzone"
    dropE <- wrapDomEvent (_el_element e) (`on` drop) $ do
      preventDefault
      return dropItem
    return $ attachDynWithMaybe f dragging dropE
   where
    f (Just drag) drop | dndFilter drag drop = Just (drag, drop)
    f _           _                          = Nothing
