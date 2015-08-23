module Web.Giflib.DOM.Util where

import Prelude

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.Node as DOM
import qualified DOM.Node.Types as DOM
import qualified DOM.Node.ParentNode as DOM

import Control.Bind ((<=<), (=<<))
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)

-- | Appends the given node under the global querySelector value. If the query resolves
--   to an element in the DOM, the appended node will be returned, otherwise Nothing
--   is returned.
appendToQuerySelector :: forall eff. String -> DOM.Node -> Eff (dom :: DOM.DOM | eff) (Maybe DOM.Node)
appendToQuerySelector selector node = do
  win <- DOM.window
  hdoc <- DOM.document win
  let doc = DOM.htmlDocumentToDocument hdoc
  let pdoc = DOM.documentToParentNode doc
  el <- toMaybe <$> DOM.querySelector selector pdoc

  case el of
    Just el' -> pure <$> DOM.appendChild node (DOM.elementToNode el')
    Nothing  -> pure Nothing
