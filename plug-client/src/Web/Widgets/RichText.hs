{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Widgets.RichText where

import           Web.Widgets

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map                 as Map
import           Data.Monoid            ((<>))
import           GHCJS.DOM              (currentWindow, currentDocument)
import           GHCJS.DOM.Document     (execCommand)
import           GHCJS.DOM.Element      (getInnerHTML, setInnerHTML, setOuterHTML)
import           GHCJS.DOM.Types        (Document)
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common
import           Reflex.Dom.Widget.Basic


type Image
  = String

richText :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => Maybe (Event t () -> m (Event t Image)) -> Html -> m (Dynamic t Html)
richText getImage initial = divClass "rt-container panel" $ do
  doc <- liftIO currentDocument >>= \case
    Just d  -> return d
    Nothing -> error "Web.Widgets.richText: no current document"
  rec 
    divClass "panel-header" $ do
      mapM_ (uncurry $ editButton doc e)
        [ ("bold","b")
        , ("italic","i")
        , ("insertOrderedList","1.")
        , ("insertUnorderedList","•")
        ]
      maybe blank (imageButton doc e) getImage
    (e, _) <- elAttr' "div"
      ("contenteditable" =: "true" <> "class" =: "form-control panel-body")
      blank
    setInnerHTML (_el_element e) $ Just initial
  let changed = push
        (const $ liftIO $ getInnerHTML $ _el_element e)
        (domEvent Input e) -- TODO: does not work in IE
  holdDyn initial changed
 where
  editButton :: Document -> El t -> String -> String -> m ()
  editButton doc e a t = do
    b <- buttonClass "btn btn-default btn-xs" t
    onEvent b $ \ () -> do
      command doc a Nothing
      focus $ _el_element e

  -- TODO: add image at cursor position, as follows:
  -- · save div cursor position in an IORef on MouseUp and KeyUp events;
  -- · restore position from the IORef on Focus events.
  imageButton :: Document -> El t -> (Event t () -> m (Event t Image)) -> m ()
  imageButton doc e getImage = do
    addImage <- buttonClass "btn btn-default btn-xs" "Image"
    image <- getImage addImage
    onEvent image $ \ i -> do
      focus $ _el_element e
      command doc "insertImage" (Just i)

command :: (MonadIO m) => Document -> String -> Maybe String -> m ()
command doc cmd arg = execCommand doc cmd False arg >>= \case
  True  -> return ()
  False -> liftIO $ putStrLn $
    "Web.Widgets.richText: command " ++ show cmd ++ " failed."

