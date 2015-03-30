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
import qualified WSK as WSK
import qualified Data.StrMap as StrMap

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
demoState = State { entries: demoEntries, tag: Just "animals" }

update :: State -> Action -> State
update s a = updateState a s
  where
  updateState NoOp = id

view :: forall p r node. (H.HTMLRepr node) => SF1 Action (node p r)
view = render <$> stateful demoState update
  where
  render :: State -> node p r
  render st =
    H.div [ A.class_ $ A.className "giflib-app" ]
      [ H.div [ A.class_ $ A.className "gla-card-holder" ] $ map entryCard st.entries
      ]

    where

    backgroundImage :: String -> A.Styles
    backgroundImage s = A.styles $ StrMap.singleton "backgroundImage" ("url(" ++ s ++ ")")

    entryCard :: Entry -> node p r
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
  Tuple node driver <- runUI (pureUI view)
  el <- querySelector "#app-main"
  case el of
    Just e -> appendChild node e
    Nothing -> throwException $ error "Couldn't find #app-main. What've you done to the HTML?"
