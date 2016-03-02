module Web.Widgets.Autocomplete where

import Web.Widgets

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

-- autocomplete :: 
