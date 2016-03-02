{-# LANGUAGE LambdaCase #-}
module Web.Widgets.Upload
  ( file
  ) where

import           GHCJS.DOM.Types         (File, toBlob, Blob(Blob))
import qualified JavaScript.Web.Blob.Internal           as Blob
import qualified JavaScript.WebSockets.Reflex.WebSocket as WS
import           Reflex.Dom
import           Reflex.Dom.Widget.Input (fileInput)


file :: forall t m. (MonadWidget t m)
  => m (Dynamic t [Blob.Blob])
file = do
  f <- fileInput def
  let files = _fileInput_value f :: Dynamic t [File]
  flip mapDyn files $ map (blobBlob . toBlob)

blobBlob :: Blob -> Blob.Blob
blobBlob (Blob js) = Blob.SomeBlob js
