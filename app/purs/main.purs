module Main where

import Debug.Trace
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Date (date, Month(..))

import Web.Giflib.Types (URI(), Tag(), Entry(..))

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Events as T
import qualified Thermite.Action as T
import qualified Thermite.Types as T

data State = State { entries :: [Entry] -- ^ All entries matching the tag
                   , tag :: Maybe Tag -- ^ Currently selected tag, if any
                   }

data Action =
    NoOp

initialState :: State
initialState = State { entries: [], tag: Nothing }

demoEntries :: [Entry]
demoEntries = [ Entry { uri: "http://media.giphy.com/media/JdCz7YXOZAURq/giphy.gif"
                      , tags: [ "hamster", "party", "animals" ]
                      , date: fromJust $ date 2015 January 1
                      }
              , Entry { uri: "http://media.giphy.com/media/lkimmb3hVhjvWF0KA/giphy.gif"
                      , tags: [ "cat", "wiggle", "animals" ]
                      , date: fromJust $ date 2015 February 28
                      }
              ]

demoState :: State
demoState = State { entries: demoEntries , tag: Nothing }

spec :: T.Spec _ State _ Action
spec = T.Spec { initialState: initialState
              , performAction: performAction
              , render: render
              , componentWillMount: Nothing
              , displayName: Just "giflib-app"
              }

performAction :: T.PerformAction _ Action (T.Action _ State)
performAction _ action = T.modifyState (updateState action)
    where
    updateState :: Action -> State -> State
    updateState NoOp = id

render :: T.Render State _ Action
render ctx (State st) _ =
    T.div [ A.className "hello-world" ] [ T.text "hello" ]

main :: forall eff. Control.Monad.Eff.Eff (dom :: DOM.DOM | eff) Unit
main = T.render (T.createClass spec) {}
