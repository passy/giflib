module Main where

import Control.Functor (($>))
import Data.Array (map, concat, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Debug.Trace

import Halogen (runUI)
import Halogen.Component (component, Component(..))
import Halogen.Signal (SF1(..), stateful)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Handler as E
import qualified Data.Date as Date
import qualified WSK as WSK
import qualified WSK.Textfield as WSK
import qualified Data.StrMap as StrMap

import Web.Giflib.Types (URI(), Tag(), Entry(..))
import Control.Monad.Eff.DOM (querySelector, appendChild)
import Control.Monad.Eff.Exception (error, throwException, Exception(..))


type State = { entries :: [Entry]   -- ^ All entries matching the tag
             , tag     :: Maybe Tag -- ^ Currently selected tag, if any
             }

data Action
  = NoOp
  | NewEntry Entry

emptyState :: State
emptyState = { entries: [], tag: Nothing }

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
demoState = { entries: demoEntries, tag: Just "animals" }

update :: State -> Action -> State
update s a = updateState a s
  where
  updateState NoOp = id
  -- TODO: Use lenses!
  updateState (NewEntry e) = \s -> s { entries = e : s.entries }

ui :: forall p m eff. (Applicative m) => Component p m Action Action
ui = component $ render <$> stateful demoState update
  where
  render :: State -> H.HTML p (m Action)
  render st =
    H.div [ A.class_ $ A.className "gla-content" ]
      -- Oh hai, this works, but I'm gonna be honest, I don't quite understand
      -- how that signalling works, in particular the ($>) operator which
      -- discards the value on its left-hand side:
      -- ($>) :: forall f a b. (Functor f) => f a -> b -> f b
      -- What we want to produce here is a function of
      -- Event fields -> EventHandler input
      -- I hope to come back here later and make sense out of this.
      [ H.form [ A.onsubmit $ (\_ -> E.preventDefault $> (NewEntry $ fromJust $ demoEntries !! 0))
               , A.class_ $ A.className "gla-layout--margin-h"
               ]
               [ H.div [ A.class_ $ A.className "gla-form--inline-group" ] [
                 WSK.textfield { id: Just "inp-new-gif"
                               , label: Just "New GIF URI"
                               , type_: "url"
                               , floatingLabel: true
                               } ]
               , H.div [ A.class_ $ A.className "gla-form--inline-group" ] [
                 WSK.textfield { id: Just "inp-new-tags"
                               , label: Just "Tags"
                               , type_: "text"
                               , floatingLabel: true
                               } ]
               ]
               -- TODO: Button!
      , H.div [ A.class_ $ A.className "gla-card-holder" ] $ map entryCard st.entries
      ]

    where

    backgroundImage :: String -> A.Styles
    backgroundImage s = A.styles $ StrMap.singleton "backgroundImage" ("url(" ++ s ++ ")")

    entryCard :: Entry -> H.HTML p (m Action)
    entryCard e = H.div
        -- TODO: halogen doesn't support keys at the moment which
        -- would certainly be desirable for diffing perf:
        -- https://github.com/Matt-Esch/virtual-dom/blob/7cd99a160f8d7c9953e71e0b26a740dae40e55fc/docs/vnode.md#arguments
        [ A.classes [WSK.card, WSK.shadow 3]
        ]
        [ H.div [ A.class_ WSK.cardImageContainer
                , A.style $ backgroundImage e.uri
                ] []
        , H.div [ A.class_ WSK.cardHeading ]
            [ H.h2
                [ A.class_ WSK.cardHeadingText ] [ H.text $ formatEntryTags e ]
            ]
        , H.div [ A.class_ WSK.cardCaption ] [ H.text $ formatEntryDatetime e ]
        , H.div [ A.class_ WSK.cardBottom ]
            [ H.a
                [ A.href e.uri
                , A.class_ WSK.cardUri
                , A.target "_blank" ] [ H.text e.uri ]
            ]
        ]

formatEntryDatetime :: forall e. { date :: Date.Date | e } -> String
formatEntryDatetime e = show e.date

formatEntryTags :: forall e. { tags :: [Tag] | e } -> String
formatEntryTags e = joinWith " " $ map (\x -> "#" ++ x) e.tags

main = do
  Tuple node driver <- runUI ui
  el <- querySelector "#app-main"
  case el of
    Just e -> appendChild node e
    Nothing -> throwException $ error "Couldn't find #app-main. What've you done to the HTML?"
