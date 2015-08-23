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
  doc <- DOM.documentToParentNode <<< DOM.htmlDocumentToDocument <$> (DOM.document =<< DOM.window)
  el <- (DOM.elementToNode `map`) <$> toMaybe <$> DOM.querySelector selector doc

  case el of
    Just el' -> Just <$> DOM.appendChild node el'
    Nothing  -> pure $ Nothing
