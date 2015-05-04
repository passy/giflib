module Main where

import Control.Alternative
import Control.Functor (($>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, throwException, Exception(..))
import Data.Argonaut (decodeJson)
import Data.Argonaut.Core (JObject(), fromObject)
import Data.Array (map, concat, (!!))
import Data.DOM.Simple.Document ()
import Data.DOM.Simple.Element (querySelector, appendChild)
import Data.DOM.Simple.Window (document, globalWindow)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Monoid (mempty)
import Data.String (joinWith, trim, split)
import Data.Tuple (Tuple(..))
import Debug.Trace (Trace(), trace)
import Halogen (runUI, Driver(), HalogenEffects())
import Halogen.Component (component, Component(..))
import Halogen.HTML.Target (URL(), url, runURL)
import Halogen.Signal (SF1(..), stateful)

import qualified Data.Date as Date
import qualified Data.Date.Locale as Date
import qualified Data.Int as Int
import qualified Data.Set as Set
import qualified Data.StrMap as StrMap
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified MDL as MDL
import qualified MDL.Button as MDL
import qualified MDL.Textfield as MDL
import qualified Node.UUID as NUUID
import qualified Web.Firebase as FB
import qualified Web.Firebase.DataSnapshot as DS
import qualified Web.Firebase.Types as FB
import qualified Data.Foreign as Foreign

import Web.Giflib.Types (Tag(), Entry(..), uuid)
import Web.Giflib.Internal.Unsafe (unsafePrintId, undefined, unsafeEvalEff)
import Web.Giflib.Internal.Debug (Console(), log)

type State = { entries :: [Entry]     -- ^ All entries matching the tag
             , tag     :: Maybe Tag   -- ^ Currently selected tag, if any
             , newUrl  :: URL         -- ^ New URL to be submitted
             , newTags :: Set.Set Tag -- ^ New Tags to be submitted
             }

data Action
  = NoOp
  | NewEntry Entry
  | UpdateNewURL URL
  | UpdateNewTags String
  | UpdateEntries [Entry]

data Request
  = AddNewEntry State

type AppEff eff = HalogenEffects ( uuid :: NUUID.UUIDEff , now :: Date.Now | eff )

emptyState :: State
emptyState = { entries: mempty
             , tag: mempty
             -- TODO: Add a Monoid instance to URL
             , newUrl: url ""
             , newTags: Set.empty
             }

-- TODO: Remove those, or move to tests, once loading works.
demoEntries :: [Entry]
demoEntries = [ Entry { id: uuid "CDF20EF7-A181-47B7-AB6B-5E0B994F6176"
                      , url: url "http://media.giphy.com/media/JdCz7YXOZAURq/giphy.gif"
                      , tags: Set.fromList [ "hamster", "party", "animals" ]
                      , date: fromJust $ Date.fromString "2015-04-01 13:37:00"
                      }
              , Entry { id: uuid "EA72E9A5-0EFA-44A3-98AA-7598C8E5CD14"
                      , url: url "http://media.giphy.com/media/lkimmb3hVhjvWF0KA/giphy.gif"
                      , tags: Set.fromList [ "cat", "wiggle", "animals" ]
                      , date: fromJust $ Date.fromString "2015-01-01 00:01:02"
                      }
              ]

additionalDemoEntry :: Entry
additionalDemoEntry = Entry { id: uuid "EA72E9A5-0EFA-45A3-98AA-7598C8E5CD14"
                            , url: url "http://media.giphy.com/media/pOEauzdwvAzok/giphy.gif"
                            , tags: Set.fromList [ "taylor", "woot" ]
                            , date: fromJust $ Date.fromString "2015-02-03 00:02:03"
                            }

demoState :: State
demoState = emptyState { entries = demoEntries, tag = Just "animals" }

update :: State -> Action -> State
update s' a = updateState a s'
  where
  updateState NoOp s = s
  updateState (NewEntry e) s = s { entries = (unsafePrintId e) : s.entries }
  updateState (UpdateNewURL e) s = s { newUrl = unsafePrintId e }
  updateState (UpdateNewTags e) s = s { newTags = unsafePrintId $ processTagInput e }
  updateState (UpdateEntries e) s = s { entries = e }

-- | Handle a request to an external service
handler :: forall eff.
  Request ->
  E.Event (AppEff eff) Action
handler (AddNewEntry s) = do
  id' <- liftEff NUUID.v4
  now <- liftEff Date.now
  E.yield $ NewEntry $ Entry { id: uuid $ show id'
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
      [ H.form [ A.onSubmit \_ -> E.preventDefault $> (handler $ (AddNewEntry st))
               , A.class_ $ A.className "gla-layout--margin-h"
               ]
               [ H.div [ A.class_ $ A.className "gla-form--inline-group" ] [
                 MDL.textfield [ E.onInput (A.input UpdateNewURL <<< url)
                               , A.required true ] $
                  MDL.defaultTextfield { id = Just "inp-new-gif"
                                       , label = Just "URL"
                                       , type_ = "url"
                                       } ]
               , H.div [ A.class_ $ A.className "gla-form--inline-group" ] [
                 MDL.textfield [ E.onInput $ A.input UpdateNewTags
                               , A.required true ] $
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
    entryCard (Entry e) = H.div
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

formatEntryTags :: forall e. { tags :: Set.Set Tag | e } -> String
formatEntryTags e = joinWith " " $ map (\x -> "#" ++ x) $ Set.toList e.tags

processTagInput :: String -> Set.Set Tag
processTagInput = trim >>> split " " >>> Set.fromList

-- Application Main

main = do
  trace "Booting. Beep. Boop."
  Tuple node driver <- runUI ui

  fb <- FB.newFirebase $ url "https://giflib-web.firebaseio.com/"
  children <- FB.child "entries" fb
  FB.on FB.Value (dscb driver) Nothing children

  doc <- document globalWindow
  el <- querySelector "#app-main" doc
  case el of
    Just e -> appendChild e node
    Nothing -> throwException $ error "Couldn't find #app-main. What've you done to the HTML?"
  trace "Up and running."

  where
    -- TODO: Use Aff instead of Eff for this.
    dscb :: forall req eff. (Action -> eff) -> FB.DataSnapshot -> eff
    dscb driver ds = do
      let f = Foreign.unsafeReadTagged "Object" $ DS.val ds
      case f of
           -- Left _ -> trace "Foreign error"
           Right str -> case (decodeEntries str) of
              -- Left err      -> trace "Something went wrong parsing the entries. " ++ err
              Right entries -> driver $ UpdateEntries entries

    decodeEntries :: JObject -> Either String [Entry]
    decodeEntries = fromObject >>> decodeJson
