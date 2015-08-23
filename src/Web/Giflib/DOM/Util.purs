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

-- This probably shouldn't be in this module.
-- Inspired by @jbetzend
-- https://twitter.com/jbetzend/status/635446351753543680
(<$$>) :: forall f g a b. (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = map <<< map

-- | Appends the given element under the global querySelector value. If the query resolves
--   to an element in the DOM, the appended node will be returned, otherwise Nothing
--   is returned.
appendToQuerySelector :: forall eff. String -> DOM.HTMLElement -> Eff (dom :: DOM.DOM | eff) (Maybe DOM.Node)
appendToQuerySelector selector node = do
  doc <- DOM.documentToParentNode <<< DOM.htmlDocumentToDocument <$> (DOM.document =<< DOM.window)
  el <- DOM.elementToNode <$$> toMaybe <$> DOM.querySelector selector doc

  case el of
    Just el' -> Just <$> DOM.appendChild (DOM.htmlElementToNode node) el'
    Nothing  -> pure Nothing
