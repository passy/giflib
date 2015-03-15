module Main where

import Debug.Trace
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Date (date, Month(..), Date(..))
import Data.Array (map, concat)
import Data.String (joinWith)

import Web.Giflib.Types (URI(), Tag(), Entry(..))

import Thermite.Internal (unsafeAttribute)

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

style :: forall s action. { | s } -> T.Prop action
style = unsafeAttribute "style"

emptyState :: State
emptyState = State { entries: [], tag: Nothing }

demoEntries :: [Entry]
demoEntries = [ { uri: "http://media.giphy.com/media/JdCz7YXOZAURq/giphy.gif"
                , tags: [ "hamster", "party", "animals" ]
                , date: fromJust $ date 2015 January 1
                }
              , { uri: "http://media.giphy.com/media/lkimmb3hVhjvWF0KA/giphy.gif"
                , tags: [ "cat", "wiggle", "animals" ]
                , date: fromJust $ date 2015 February 28
                }
              ]

demoState :: State
demoState = State { entries: demoEntries , tag: Nothing }

spec :: T.Spec _ State _ Action
spec = T.Spec { initialState: demoState
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
    -- TODO: Needs a key attribute
    T.div [ A.className "giflib-app" ] $ map entryCard st.entries

    where

    entryCard :: Entry -> T.Html _
    entryCard e = T.div
        [ A.className "wsk-card wsk-shadow--z3" ]
        [ T.div [ A.className "wsk-card--img-container"
                , style $ { "background-image": "url(" ++ e.uri ++ ")" }
                ] []
        , T.div [ A.className "wsk-card--heading" ]
            [ T.h2
                [ A.className "wsk-card--heading-text" ] [ T.text $ formatEntryTags e ]
            ]
        , T.div [ A.className "wsk-card--caption" ] [ T.text $ formatEntryDatetime e ]
        , T.div [ A.className "wsk-card--bottom" ]
            [ T.a
                [ A.href "#" ] [ T.text "Some Action" ]
            ]
        ]

formatEntryDatetime :: forall e. { date :: Date | e } -> String
formatEntryDatetime _ = "1970-01-01 00:00:00"

formatEntryTags :: forall e. { tags :: [Tag] | e } -> String
formatEntryTags e = joinWith " " $ map (\x -> "#" ++ x) e.tags

main :: forall eff. Control.Monad.Eff.Eff (dom :: DOM.DOM | eff) Unit
main = T.render (T.createClass spec) {}
