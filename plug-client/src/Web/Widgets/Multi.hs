{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Widgets.Multi
  ( editMulti
  , selectFrom
  , selectFromInitial
  ) where

import Web.Widgets

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Map                 as Map
import           Reflex.Dom
import           Reflex.Dom.Widget.Basic

editMulti :: (MonadWidget t m) =>
  Bool -> (Bool -> Edit t m a) -> a -> Edit t m [a]
editMulti inline editSingle empty initial = divClass
  ((if inline then (++ " multi-inline") else id) "multi") $ mdo
  inputs :: Dynamic t (Map.Map Int
    (Dynamic t a, Event t (Map.Map Int (Maybe a))))
    <- listWithKeyShallowDiff
      (Map.fromList . zip [1 ..] $ initial)
      (leftmost [addE, removeE])
      $ \ k c _changed -> el "div" $ do
        dc <- editSingle (k > length initial) c
        inputChange <- (k =: Nothing <$) <$>
          buttonClassM "btn btn-danger btn-xs"
            (elClass "span" "glyphicon glyphicon-remove" blank)
        return (dc, inputChange)

  newE <- buttonClassM "btn btn-default btn-xs"
    (elClass "span" "glyphicon glyphicon-plus" blank)
  newKey <- mapDyn (+ length initial) =<< count newE
  let addE = attachDynWith
        (\ k () -> k =: Just empty) newKey newE

  removeE <- switchPromptlyDyn <$>
    mapDyn (leftmost . map snd . Map.elems) inputs

  mapDyn Map.elems . joinDynThroughMap =<<
    mapDyn (fmap fst) inputs

selectFrom :: forall t m a. (MonadWidget t m, MonadIO (PushM t))
  => Dynamic t [a] -> (Dynamic t a -> m ())
  -> m (Dynamic t (Maybe a), Event t a)
selectFrom options optionText = do
  pick <- fmap switchPromptlyDyn . mapDyn leftmost =<<
    simpleList options displayOption
  current <- holdDyn Nothing (Just <$> pick) 
  return (current, pick)
 where
  displayOption :: Dynamic t a
    -> m (Event t a)
  displayOption dOption = do
    (e, _) <- elAttr' "div" (Map.fromList [("class", "pick")]) $
      displayOption dOption
    return $ tagDyn dOption $ domEvent Click e

-- radioInitial :: forall t m a. (Eq a, MonadWidget t m) =>
--   a -> Dynamic t [a] -> (Dynamic t a -> m ()) -> m (Dynamic t a)
-- radioInitial init options optionText = do
--   (i, _) <- elAttr' "input"
--     (Map.fromList [("type", "radio"), ("class", "form-control")])
--     (simpleList options displayOption)
--   value i
--  where
--   displayOption :: Dynamic t a -> m ()
--   displayOption dOption = do
--     attrs <- mapDyn (\ a -> if a == init
--       then Map.singleton "checked" "checked"
--       else Map.empty) dOption
--     elDynAttr "option" attrs $ optionText dOption

selectFromInitial :: forall t m a. (Eq a, MonadWidget t m, MonadIO (PushM t))
  => a -> Dynamic t [a] -> (Dynamic t a -> m ())
  -> m (Dynamic t a)
selectFromInitial init options optionText = mdo
  pick <- fmap switchPromptlyDyn . mapDyn leftmost =<<
    simpleList options (displayOption pick)
  current <- holdDyn init pick
  return current
 where
  displayOption :: Event t a -> Dynamic t a -> m (Event t a)
  displayOption someClick dOption = mdo
    initial <- mapDyn (== init) dOption
    let changeActive = attachDynWith (==) dOption someClick
    active <- holdDynInit initial changeActive 
    attrs <- flip mapDyn active $ \case
      True  -> "class" =: "pick active"
      False -> "class" =: "pick"
    (e, _) <- elDynAttr' "div" attrs $
      optionText dOption
    return $ tagDyn dOption $ domEvent Click e


