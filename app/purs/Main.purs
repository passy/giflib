module Main where

import Debug.Trace
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Array (map, concat)
import Data.String (joinWith)

import Web.Giflib.Types (URI(), Tag(), Entry(..))
import Control.Monad.Eff.DOM (querySelector, appendChild)
import Control.Monad.Eff.Exception (error, throwException, Exception(..))

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Html.Attributes.Extra as A
import qualified Thermite.Events as T
import qualified Thermite.Action as T
import qualified Thermite.Types as T
import qualified Data.Date as Date


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
    T.div [ A.className "giflib-app" ]
        [ T.div [ A.className "gla-card-holder" ] $ map entryCard st.entries
        ]

    where

    entryCard :: Entry -> T.Html _
    entryCard e = T.div
        [ A.className "wsk-card wsk-shadow--z3"
        , A.key e.id
        ]
        [ T.div [ A.className "wsk-card--img-container"
                , A.style $ { "backgroundImage": "url(" ++ e.uri ++ ")" }
                ] []
        , T.div [ A.className "wsk-card--heading" ]
            [ T.h2
                [ A.className "wsk-card--heading-text" ] [ T.text $ formatEntryTags e ]
            ]
        , T.div [ A.className "wsk-card--caption" ] [ T.text $ formatEntryDatetime e ]
        , T.div [ A.className "wsk-card--bottom" ]
            [ T.a
                [ A.href e.uri
                , A.className "wsk-card--uri"
                , A.target "_blank" ] [ T.text e.uri ]
            ]
        ]

formatEntryDatetime :: forall e. { date :: Date.Date | e } -> String
formatEntryDatetime e = show e.date

formatEntryTags :: forall e. { tags :: [Tag] | e } -> String
formatEntryTags e = joinWith " " $ map (\x -> "#" ++ x) e.tags

main :: forall eff. Control.Monad.Eff.Eff (dom :: DOM.DOM, err :: Exception | eff) Unit
main = do
    el <- querySelector "#app-main"
    case el of
         Just e -> T.renderTo e (T.createClass spec) {}
         Nothing -> throwException $ error "Couldn't find #app-main. What've you done to the HTML?"
