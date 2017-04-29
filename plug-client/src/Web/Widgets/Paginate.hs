{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Widgets.Paginate
  ( paginate
  ) where

import Web.Widgets

import qualified Data.Map          as Map
import qualified Data.Text         as Text
import           Data.Traversable  (for)
import           Reflex.Dom hiding (Window, Key(Tab))
import           Reflex.Dom.Widget.Basic


data PagingData
  = PagingData
    { pages :: [Page]
    }

data Page
  = Page
    { index      :: Int
    , itemOffset :: Int
    , itemCount  :: Int
    }

pagingData :: Int -> Int -> PagingData
pagingData pagingSize total = PagingData { pages = go 1 0 total } where
  go 1 _      0    = [Page 1 0 0] -- Return a single empty page if no items.
  go _ _      0    = []
  go i offset left = Page i offset count :
    go (succ i) (offset + count) (left - count)
   where
    count = min pagingSize left

paginate :: forall m t a. (MonadWidget t m)
  => Int -> Dynamic t [a] -> (Dynamic t a -> m ()) -> m ()
paginate pagingSize allItems displayOne = mdo
  simpleList sublist displayOne
  currentPage <- pager allItems
  let sublist = do
        cp <- currentPage
        its <- allItems
        return $ take (itemCount cp) . drop (itemOffset cp) $ its
  return ()
 where
  pager :: Dynamic t [a] -> m (Dynamic t Page)
  pager ls = mdo
    let psd = pages . pagingData pagingSize . length <$> ls
    pageEvent <- dynEvent $ ffor psd $ \case
      [p] -> return never
      ps  -> fmap leftmost $
        elClass "ul" "pagination" $ for ps $ \ page -> do
          let attrs = do
                current <- currentPage
                return $ if index page == index current
                  then "class" =: "active"
                  else Map.empty
          e <- elDynAttr "li" attrs $
            fmap _link_clicked . link . Text.pack . show . index $ page
          return $ page <$ e
    currentPage <- holdDynInit (head <$> psd) $ pageEvent
      -- switchPromptlyDyn $ leftmost <$> eventsD
    return currentPage
