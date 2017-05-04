{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Change where

import           Data.List (splitAt)
import qualified Data.Map as Map
import qualified Data.Set as Set


class (Monoid (Changes a)) => Changing a where
  type Changes a
  type instance Changes a = Replace a
  apply :: Changes a -> a -> a
  default apply :: Replace a -> a -> a
  apply = replacing

newtype Replace a
  = Replace (Maybe a)
instance Monoid (Replace a) where
  mempty = keep
  mappend r₁@(Replace (Just _)) _  = r₁
  mappend _                     r₂ = r₂

replace :: a -> Replace a
replace = Replace . Just

keep :: Replace a
keep = Replace Nothing

replacing :: Replace a -> a -> a
replacing (Replace Nothing ) = id
replacing (Replace (Just x)) = const x


instance (Changing a) => Changing (Maybe a) where
  type Changes (Maybe a) = MaybeChange a
  apply SetNothing   = const Nothing
  apply (SetJust x)  = const (Just x)
  apply (MapMaybe c) = fmap (apply c)

data MaybeChange a
  = SetNothing
  | SetJust a
  | MapMaybe (Changes a)
instance (Changing a) => Monoid (MaybeChange a) where
  mempty = MapMaybe mempty
  mappend SetNothing    _             = SetNothing
  mappend (SetJust a)   _             = SetJust a
  mappend (MapMaybe c ) SetNothing    = SetNothing
  mappend (MapMaybe c ) (SetJust a)   = SetJust (apply c a)
  mappend (MapMaybe c₁) (MapMaybe c₂) = MapMaybe (mappend c₁ c₂)

instance (Changing a) => Changing [a] where
  type Changes [a] = [ListChange a]
  apply = foldr (.) id . map go where
    go (ListAdd i a)     xs
      | i < 0     = xs ++ [a]
      | otherwise = let (ys, zs) = splitAt i xs in
      ys ++ a : zs
    go (ListDelete i)    xs = let (ys, a : zs) = splitAt i xs in
      ys ++ zs
    go (ListChange i c)  xs = let (ys, a : zs) = splitAt i xs in
      ys ++ apply c a : zs
    go (ListReorder i j) xs = let
      (ys, a : zs) = splitAt i xs
      (vs, ws)     = splitAt j (ys ++ zs)
     in
      vs ++ a : ws

-- Indices are zero-based.
data ListChange a
  = ListAdd Int a     -- Negative: at the end
  | ListDelete Int
  | ListChange Int (Changes a)
  | ListReorder Int Int

instance (Changing v, Ord k) => Changing (Map.Map k v) where
  type Changes (Map.Map k v) = [MapChange k v]
  apply = foldr (.) id . map go where
    go (MapAdd k v)    = Map.insert k v
    go (MapModify k c) = Map.adjust (apply c) k
    go (MapDelete k)   = Map.delete k

data MapChange k v
  = MapAdd k v
  | MapModify k (Changes v)
  | MapDelete k

instance (Ord k) => Changing (Set.Set k) where
  type Changes (Set.Set k) = [SetChange k]
  apply = foldr (.) id . map go where
    go (SetAdd k)    = Set.insert k
    go (SetDelete k) = Set.delete k
data SetChange k
  = SetAdd k
  | SetDelete k
setDiff :: (Ord k) => Set.Set k -> Set.Set k -> Changes (Set.Set k)
setDiff old new =
     map SetDelete (Set.toList $ Set.difference old new)
  ++ map SetAdd    (Set.toList $ Set.difference new old)
