module Web.Widgets.Modal
  ( modal
  , dialogue
  , dialogueExtraFooter
  , confirm
  , info
  ) where

import           Web.Widgets

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map                 as Map
import           Data.Monoid            ((<>))
import           Data.Traversable       (for)
import           GHCJS.DOM              (currentWindow, currentDocument)
import           GHCJS.DOM.Document     (execCommand)
import           GHCJS.DOM.Element      (focus, getInnerHTML, setInnerHTML, setOuterHTML)
import           GHCJS.DOM.HTMLInputElement (HTMLInputElement)
import           GHCJS.DOM.Window       (Window, open)
import           Reflex.Dom
import           Reflex.Dom.Widget.Basic
import           Reflex.Dom.Contrib.Widgets.Common
import qualified Reflex.Dom.Contrib.Widgets.Modal as Modal


dialogueOld :: (MonadWidget t m)
  => Event t a -> String -> String -> m ()
  -> (a -> m (Dynamic t (Either e b))) -> m (Event t b)
dialogueOld open cancelText continueText header body = modalSuccess =<< Modal.removingModal
  (Modal.ModalConfig Map.empty)
  open (\ a -> Modal.mkModalBody
    (header >> return never) -- header with close event
    (\ state -> do
      -- escape <- fmap (const ()) . ffilter (== keycodeEscape)
        -- . domEvent Keypress <$> getBody
      cancel <- buttonClass "btn btn-default" cancelText
      save <- buttonClass "btn btn-info" continueText
      -- return (leftmost [cancel, escape], save)
      return (cancel, save)
    )
    (body a)
  )
 where
  modalSuccess :: (MonadWidget t m)
    => Dynamic t (Maybe (Event t (Either e b))) -> m (Event t b)
  modalSuccess d = fmapMaybe (either (const Nothing) Just) <$> switchJust d


dialogue :: (MonadWidget t m)
  => Event t a -> String -> String -> String -> m ()
  -> (a -> m (Dynamic t (Either e b))) -> m (Event t b)
dialogue open = dialogueExtraFooter open (const $ return never)

dialogueExtraFooter :: (MonadWidget t m)
  => Event t a -> (a -> m (Event t ())) -> String -> String -> String -> m ()
  -> (a -> m (Dynamic t (Either e b))) -> m (Event t b)
dialogueExtraFooter open extra cancelText continueText continueClass h b = modal open $
  \ header body footer a -> do
    header h
    state <- body (b a)
    (extraE, (success, cancel)) <- footer $ leftRightAlign (extra a) $ do
      cancel <- buttonClass "btn btn-default" cancelText
      attrs <- mapDyn (($ "class" =: ("btn " ++ continueClass)) .
        either (const $ mappend $ "disabled" =: "disabled") (const id)) state
      continue <- buttonDynAttr attrs $ text continueText
      let success = attachDynWithMaybe
            (\ e () -> either (const Nothing) Just e) state continue
      return (success, cancel)
    return (success, cancel <> extraE)

modal :: (MonadWidget t m) =>
  Event t a ->
  ((m x -> m x) -> (m y -> m y) -> (m z -> m z) ->
    a -> m (Event t b, Event t ())) ->
  m (Event t b)
modal open body = switchJust =<< Modal.removingModal
  (Modal.ModalConfig Map.empty)
  open
  (\ a -> do
    (success, cancel) <- divClass "modal show" . divClass "modal-dialog" .
      divClass "modal-content" $ body
        (divClass "modal-header")
        (divClass "modal-body")
        (divClass "modal-footer")
        a
    return (success, leftmost [() <$ success, cancel])
  )

confirm :: (MonadWidget t m)
  => String -> String -> String -> String -> Event t a -> m (Event t a)
confirm question cancelText continueText continueClass w = dialogue
  w cancelText continueText continueClass blank $ \ a -> do
    text question
    return $ constDyn $ Right a

info :: (MonadWidget t m)
  => Event t a -> String -> m ()
  -> (a -> m ()) -> m (Event t ())
info open continueText h b = modal open $
  \ header body footer a -> do
    header h
    body (b a)
    footer $ do
      continue <- buttonClass "btn btn-default" continueText
      return (continue, never)

switchJust :: (MonadWidget t m)
  => Dynamic t (Maybe (Event t b)) -> m (Event t b)
switchJust d = switchPromptlyDyn
  <$> mapDyn (maybe never id) d

