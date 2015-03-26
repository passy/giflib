module Main where

import Debug.Trace
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Array (map, concat)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

import Halogen (runUI, pureUI)
import Halogen.Signal (SF1(..), stateful)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Data.Date as Date

import Web.Giflib.Types (URI(), Tag(), Entry(..))
import Control.Monad.Eff.DOM (querySelector, appendChild)
import Control.Monad.Eff.Exception (error, throwException, Exception(..))


data State = State { entries :: [Entry] -- ^ All entries matching the tag
                   , tag :: Maybe Tag -- ^ Currently selected tag, if any
                   }

data Action =
    NoOp

emptyState :: State
emptyState = State { entries: [], tag: Nothing }

demoEntries :: [Entry]
demoEntries = [ { id: "CDF20EF7-A181-47B7-AB6B-5E0B994F6176"
                , uri: "http://media.giphy.com/media/JdCz7YXOZAURq/giphy.gif"
                , tags: [ "hamster", "party", "animals" ]
                , date: fromJust $ Date.date 2015 Date.January 1
                }
              , { id: "EA72E9A5-0EFA-44A3-98AA-7598C8E5CD14"
                , uri: "http://media.giphy.com/media/lkimmb3hVhjvWF0KA/giphy.gif"
                , tags: [ "cat", "wiggle", "animals" ]
                , date: fromJust $ Date.date 2015 Date.February 28
                }
              ]

demoState :: State
demoState = State { entries: demoEntries , tag: Nothing }

update :: State -> Action -> State
update s a = updateState a s
  where
  updateState NoOp = id

view :: forall p r node. (H.HTMLRepr node) => SF1 Action (node p r)
view = render <$> stateful demoState update
  where
  render :: State -> node p r
  render _ =
    H.p_ [ H.text "Hello, World" ]

main = do
  Tuple node driver <- runUI (pureUI view)
  el <- querySelector "#app-main"
  case el of
    Just e -> appendChild e node
    Nothing -> throwException $ error "Couldn't find #app-main. What've you done to the HTML?"
