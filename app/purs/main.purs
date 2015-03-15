module Main where

import Debug.Trace
import Data.Maybe (Maybe(..))

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Events as T
import qualified Thermite.Action as T
import qualified Thermite.Types as T

type Tag = String

data Entry = Entry { uri :: String -- TODO: URI
                   , tags :: [Tag]
                   , date :: Number -- TODO: DateTime
                   }

data State = State { entries :: [Entry] -- ^ All entries matching the tag
                   , tag :: Maybe Tag -- ^ Currently selected tag, if any
                   }

data Action =
    NoOp

initialState :: State
initialState = State { entries: [], tag: Nothing }

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

main :: Control.Monad.Eff.Eff (dom :: DOM.DOM) Unit
main = T.render (T.createClass spec) {}
