module Main where

import Control.Alternative
import Control.Functor (($>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, throwException, Exception(..))
import Data.Array (map, concat, (!!))
import Data.DOM.Simple.Document ()
import Data.DOM.Simple.Element (querySelector, appendChild)
import Data.DOM.Simple.Window (document, globalWindow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Monoid (mempty)
import Data.String (joinWith, trim, split)
import Data.Tuple (Tuple(..))
import Halogen.HTML.Target (URL(), url, runURL)

import Halogen (runUI, HalogenEffects())
import Halogen.Component (component, Component(..))
import Halogen.Signal (SF1(..), stateful)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Data.Date as Date
import qualified MDL as MDL
import qualified MDL.Textfield as MDL
import qualified MDL.Button as MDL
import qualified Data.StrMap as StrMap
import qualified Node.UUID as UUID
import qualified Web.Firebase as FB
import qualified Web.Firebase.Types as FB
import qualified Web.Firebase.DataSnapshot as DS

import Web.Giflib.Types (Tag(), Entry(..))
import Web.Giflib.Internal.Unsafe (unsafePrintId, unsafeShow, undefined, unsafeEvalEff)
import Web.Giflib.Internal.Debug (Console(), log)
import Debug.Trace

type State = { entries :: [Entry]   -- ^ All entries matching the tag
             , tag     :: Maybe Tag -- ^ Currently selected tag, if any
             , newUrl  :: URL       -- ^ New URL to be submitted
             , newTags :: [Tag]     -- ^ New Tags to be submitted
             }

data Action
  = NoOp
  | NewEntry Entry
  | UpdateNewURL URL
  | UpdateNewTags String

data Request
  = AddNewEntry State

type AppEff eff = HalogenEffects (uuid :: UUID.UUIDEff, now :: Date.Now | eff)

emptyState :: State
emptyState = { entries: mempty
             , tag: mempty
             , newUrl: url ""
             , newTags: mempty
             }

demoEntries :: [Entry]
demoEntries = [ { id: decodeUuid "CDF20EF7-A181-47B7-AB6B-5E0B994F6176"
                , url: url "http://media.giphy.com/media/JdCz7YXOZAURq/giphy.gif"
                , tags: [ "hamster", "party", "animals" ]
                , date: fromJust $ Date.date 2015 Date.January 1
                }
              , { id: decodeUuid "EA72E9A5-0EFA-44A3-98AA-7598C8E5CD14"
                , url: url "http://media.giphy.com/media/lkimmb3hVhjvWF0KA/giphy.gif"
                , tags: [ "cat", "wiggle", "animals" ]
                , date: fromJust $ Date.date 2015 Date.February 28
                }
              ]
  where
    decodeUuid :: String -> UUID.UUID
    decodeUuid = UUID.parse >>> UUID.unparse

demoState :: State
demoState = emptyState { entries = demoEntries, tag = Just "animals" }

update :: State -> Action -> State
update s' a = updateState a s'
  where
  updateState NoOp s = s
  updateState (NewEntry e) s = s { entries = (unsafePrintId e) : s.entries }
  updateState (UpdateNewURL e) s = s { newUrl = unsafePrintId e }
  updateState (UpdateNewTags e) s = s { newTags = unsafePrintId $ processTagInput e }

-- | Handle a request to an external service
handler :: forall eff.
  Request ->
  E.Event (AppEff eff) Action
handler (AddNewEntry s) = do
  uuid <- liftEff UUID.v4
  now <- liftEff Date.now
  E.yield $ NewEntry { id: uuid
                     , tags: s.newTags
                     , url: s.newUrl
                     , date: now
                     }

ui :: forall p eff. Component p (E.Event (AppEff eff)) Action Action
ui = component $ render <$> stateful demoState update
  where
  render :: State -> H.HTML p (E.Event (AppEff eff) Action)
  render st =
    H.div [ A.class_ $ A.className "gla-content" ]
      [ H.form [ A.onsubmit \_ -> E.preventDefault $> (handler $ (AddNewEntry st))
               , A.class_ $ A.className "gla-layout--margin-h"
               ]
               [ H.div [ A.class_ $ A.className "gla-form--inline-group" ] [
                 MDL.textfield [ E.onInput (A.input UpdateNewURL <<< url) ] $
                  MDL.defaultTextfield { id = Just "inp-new-gif"
                                       , label = Just "URL"
                                       , type_ = "url"
                                       } ]
               , H.div [ A.class_ $ A.className "gla-form--inline-group" ] [
                 MDL.textfield [ E.onInput $ A.input UpdateNewTags ] $
                   MDL.defaultTextfield { id = Just "inp-new-tags"
                                        , label = Just "Tags"
                                        } ]
               , H.div [ A.class_ $ A.className "gla-form--inline-group" ] [
                 MDL.button $
                   MDL.defaultButton { text = "Add GIF"
                                     , elevation = MDL.ButtonRaised
                                     } ]
               ]
      , H.div [ A.class_ $ A.className "gla-card-holder" ] $ map entryCard st.entries
      ]

    where

    backgroundImage :: String -> A.Styles
    backgroundImage s = A.styles $ StrMap.singleton "backgroundImage" ("url(" ++ s ++ ")")

    entryCard :: Entry -> H.HTML p (E.Event (AppEff eff) Action)
    entryCard e = H.div
        -- TODO: halogen doesn't support keys at the moment which
        -- would certainly be desirable for diffing perf:
        -- https://github.com/Matt-Esch/virtual-dom/blob/7cd99a160f8d7c9953e71e0b26a740dae40e55fc/docs/vnode.md#arguments
        [ A.classes [MDL.card, MDL.shadow 3]
        ]
        [ H.div [ A.class_ MDL.cardImageContainer
                , A.style $ backgroundImage $ runURL e.url
                ] []
        , H.div [ A.class_ MDL.cardHeading ]
            [ H.h2
                [ A.class_ MDL.cardHeadingText ] [ H.text $ formatEntryTags e ]
            ]
        , H.div [ A.class_ MDL.cardCaption ] [ H.text $ formatEntryDatetime e ]
        , H.div [ A.class_ MDL.cardBottom ]
            [ H.a
                [ A.href $ runURL e.url
                , A.class_ MDL.cardUri
                , A.target "_blank" ] [ H.text $ runURL e.url ]
            ]
        ]

formatEntryDatetime :: forall e. { date :: Date.Date | e } -> String
formatEntryDatetime e = show e.date

formatEntryTags :: forall e. { tags :: [Tag] | e } -> String
formatEntryTags e = joinWith " " $ map (\x -> "#" ++ x) e.tags

processTagInput :: String -> [Tag]
processTagInput = trim >>> split " "

-- Application Main

main = do
  trace "Booting. Beep. Boop."
  Tuple node driver <- runUI ui

  fb <- FB.newFirebase $ url "https://giflib-web.firebaseio.com/"
  children <- FB.child "entries" fb
  FB.on FB.Value dscb Nothing children

  doc <- document globalWindow
  el <- querySelector "#app-main" doc
  case el of
    Just e -> appendChild e node
    Nothing -> throwException $ error "Couldn't find #app-main. What've you done to the HTML?"
  trace "Up and running."

 where
   dscb :: forall eff. FB.DataSnapshot -> Eff (console :: Console | eff) Unit
   dscb ds = unsafeEvalEff <<< log $ DS.val ds
