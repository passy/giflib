module Main where

import Prelude

import qualified DOM.HTML as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.Node as DOM
import qualified DOM.Node.ParentNode as DOM
import qualified DOM.Node.Types as DOM
import qualified Data.Date as Date
import qualified Data.Date.UTC as Date
import qualified Data.Foreign as Foreign
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.StrMap as StrMap
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Core as H
import qualified Halogen.HTML.CSS as CSS
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Types as E
import qualified MDL as MDL
import qualified MDL.Button as MDL
import qualified MDL.Spinner as MDL
import qualified MDL.Textfield as MDL
import qualified MDL.Color as MDL
import qualified Node.UUID as NUUID
import qualified Web.Firebase as FB
import qualified Web.Firebase.Monad.Aff as FBA
import qualified Web.Firebase.DataSnapshot as DS
import qualified Web.Firebase.Types as FB

import qualified Control.Monad.Eff.Console as C

import Control.Apply ((*>))
import Control.Error.Util (hush)
import Control.Monad.Aff (Aff(), runAff, forkAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Free (liftFI, liftF)
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import Control.MonadPlus (guard)
import Control.Plus (empty)
import Css.Background (BackgroundImage(..), backgroundImage)
import Css.String (fromString)
import Data.Argonaut.Core (JObject(), fromObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(Left, Right), either)
import Data.Either.Unsafe (fromRight)
import Data.Enum (fromEnum)
import Data.Foldable (all, intercalate)
import Data.Functor (($>))
import Data.Generic (Generic, gEq, gShow)
import Data.Identity (Identity(), runIdentity)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (mempty)
import Data.String (joinWith, trim, split)
import Data.Tuple (Tuple(..))
import Data.URI (runParseURI, parseURI, printURI)
import Data.URI.Types (URI())
import Halogen.Query (modify, gets, get)
import Data.Moment.Simple (calendar, fromDate)


import Web.Giflib.Types (Tag(), Entry(..), UUID(..), uuid, runUUID, runEntryList, nodeUUIDToUUID)
import Web.Giflib.DOM.Util (appendToQuerySelector)

import Halogen
import Halogen.Component
import Halogen.Effects

data LoadingStatus
 = Loading
 | Loaded
 | LoadingError

derive instance genericLoadingStatus :: Generic LoadingStatus

instance eqLoadingStatus :: Eq LoadingStatus where
  eq = gEq

instance showLoadingStatus :: Show LoadingStatus where
  show = gShow

newtype State = State { entries         :: Array Entry         -- ^ All entries matching the tag
                      , renderedEntries :: Array RenderedEntry -- ^ Entries + effectfully rendered information
                      , tag             :: Maybe Tag           -- ^ Currently selected tag, if any
                      , newUrl          :: Maybe URI           -- ^ New URI to be submitted
                      , newTags         :: Set.Set Tag         -- ^ New Tags to be submitted
                      , error           :: String              -- ^ Global UI error to be shown
                      , loadingStatus   :: LoadingStatus       -- ^ List loading state
                      }

data Input a
  = NoOp a
  | ResetNewForm a
  | AddNewEntry a
  | UpdateLoadingStatus LoadingStatus a
  | UpdateNewURI String a
  | UpdateNewTags String a
  | UpdateEntries (Array Entry) a
  | ShowError String a

newtype AppConfig = AppConfig { firebase :: FB.Firebase }

type AppEffects = HalogenEffects ( uuid :: NUUID.UUIDEff
                                 , console :: CONSOLE
                                 , now :: Date.Now
                                 , locale :: Date.Locale
                                 , firebase :: FB.FirebaseEff)

initialState :: State
initialState = State { entries: mempty
                     , renderedEntries: mempty
                     , tag: mempty
                     , newUrl: empty
                     , newTags: mempty
                     , error: mempty
                     , loadingStatus: Loading
                     }

resetState :: State -> State
resetState (State st) =
  State $ st { newUrl = empty, newTags = mempty }

entryFromState :: State -> UUID -> Date.Date -> Maybe Entry
entryFromState (State s) uuid now = do
  let tags = s.newTags
  uri <- s.newUrl
  guard $ s.loadingStatus == Loaded

  return $ Entry { id: uuid
                 , uri: uri
                 , tags: tags
                 , date: now
                 }

ui :: forall p. AppConfig -> Component State Input (Aff AppEffects) p
ui (AppConfig conf) = component render eval
  where
    render :: Render State Input p
    render (State st) = H.div_ $
      [ H.form [ E.onSubmit (const $ E.preventDefault $> action AddNewEntry) ]
               [ H.div [ P.class_ $ H.className "gla-form--inline-group" ] [
                 MDL.textfield [ E.onValueChange $ E.input UpdateNewURI ] $
                   MDL.defaultTextfield { id = Just "inp-new-gif"
                                        , label = Just "URL"
                                        , type_ = "url"
                                        } ]
               , H.div [ P.class_ $ H.className "gla-form--inline-group" ] [
                 MDL.textfield [ E.onValueChange $ E.input UpdateNewTags ] $
                   MDL.defaultTextfield { id = Just "inp-new-tags"
                                        , label = Just "Tags"
                                        } ]
               , H.div [ P.class_ $ H.className "gla-form--inline-group" ] [
                 MDL.button $
                   MDL.defaultButton { text = "Add GIF"
                                     , elevation = MDL.ButtonRaised
                                     } ]
               ]
      , MDL.spinner (st.loadingStatus == Loading)
      , H.div [ P.class_ MDL.grid ] $ map entryCard st.renderedEntries
      ]

    entryCard :: Render RenderedEntry Input p
    entryCard (RenderedEntry e) = H.div
        [ P.classes $ [ MDL.card, MDL.shadow 3, MDL.color "white" ] <> MDL.cellCol 6
        , P.key $ runUUID e.entry.id
        ]
        [ H.div [ P.class_ $ H.className "gla-card__image-container"
                , CSS.style $ backgroundImage $ entryBackground e.entry
                ] []
        , H.div [ P.class_ MDL.cardTitle ]
            [ H.h2
                [ P.class_ MDL.cardTitleText ] [ H.text $ formatEntryTags e.entry ]
            ]
        , H.div [ P.class_ MDL.cardSubtitleText ] [ H.text $ e.dateStr ]
        , H.div [ P.classes [ MDL.cardActions, MDL.cardBorder ] ]
            [ H.a
                [ P.href $ printURI e.entry.uri
                , P.class_ MDL.cardUri
                , P.target "_blank" ] [ H.text $ printURI e.entry.uri ]
            ]
        ]

    entryBackground :: forall e. { uri :: URI | e } -> BackgroundImage
    entryBackground e =
      -- Totally unsafe! CSS injection and stuff.
      let url = "url(" <> printURI e.uri <> ")"
      in BackgroundImage $ fromString url

    -- Just to help the type checker ...
    affUuidV4 :: forall eff. Aff (uuid :: NUUID.UUIDEff | eff) NUUID.UUID
    affUuidV4 = liftEff $ NUUID.v4

    affDateNow :: forall eff. Aff (now :: Date.Now | eff) Date.Date
    affDateNow = liftEff $ Date.now

    -- All of them are no-ops for now.
    eval :: Eval Input State Input (Aff AppEffects)
    eval (NoOp next) =
      pure next
    eval (AddNewEntry next) = do
      id' <- liftFI $ nodeUUIDToUUID <$> affUuidV4
      now <- liftFI $ affDateNow
      state <- get

      let entry = entryFromState state id' now
      case entry of
        Just entry' -> liftFI $ saveEntry conf.firebase entry'
        _           -> pure unit
      -- YES! Now reset form, handle loading and so on!
      pure next
    eval (ResetNewForm next) =
      modify resetState $> next
    eval (UpdateLoadingStatus status next) =
      modify (\(State s) -> State $ s { loadingStatus = status }) $> next
    eval (UpdateNewURI str next) =
      modify (\(State s) -> State $ s { newUrl = hush $ runParseURI str }) $> next
    eval (UpdateNewTags str next) =
      modify (\(State s) -> State $ s { newTags = processTagInput str }) $> next
    eval (UpdateEntries entries next) =
      modify (\(State s) -> State $ s { entries = entries, renderedEntries = renderEntry <$> entries }) $> next
    eval (ShowError str next) =
      modify (\(State s) -> State $ s { error = str }) $> (action $ UpdateLoadingStatus LoadingError) $> next

saveEntry :: forall eff. FB.Firebase -> Entry -> Aff (firebase :: FB.FirebaseEff | eff) Unit
saveEntry firebase entry = liftEff $ do
  children <- FB.child "entries" firebase
  FB.push (Foreign.toForeign $ encodeJson entry) Nothing children

renderEntry :: forall eff. Entry -> Eff (now :: Date.Now, locale :: Date.Locale | eff) RenderedEntry
renderEntry (Entry e) = do
  -- We only recalculate the date display when the list changes. We could do it
  -- every frame, but that would be wasteful.
  dateStr <- calendar $ fromDate e.date
  return RenderedEntry $ { entry: e.entry, dateStr: dateStr }

formatEntryTags :: forall e. { tags :: Set.Set Tag | e } -> String
formatEntryTags e = joinWith " " $ map (\x -> "#" ++ x) $ List.fromList $ Set.toList e.tags

processTagInput :: String -> Set.Set Tag
processTagInput = trim >>> split " " >>> List.toList >>> Set.fromList

-- Application Main
main :: Eff AppEffects Unit
main = runAff throwException (const $ pure unit) $ do
  log "Booting. Beep. Boop."

  conf <- liftEff $ setupFB
  app <- runUI (ui conf) initialState
  mainEl <- liftEff $ appendToQuerySelector "#app-main" app.node

  case mainEl of
    Just _ -> pure unit
    Nothing -> liftEff <<< throwException $ error "Couldn't find #app-main. What've you done to my HTML?"

  let getFb (AppConfig conf) = conf.firebase
  children <- liftEff $ FB.child "entries" (getFb conf)

  _ <- forkAff $ firebaseUpdateLoop children app.driver

  log "Up and running."

  where
    -- TODO: Turn this into an aff-coroutine. It should be a producer.
    firebaseUpdateLoop children driver = do
      -- Careful, this isn't actually "blocking" but emits everything
      -- on subscription.
      ds <- FBA.on FB.Value children
      driver (action $ UpdateLoadingStatus Loading)
      case (Foreign.unsafeReadTagged "Object" $ DS.val ds) >>= decodeEntries of
        Right entries -> do
          -- Combine these. How do I free?!
          driver $ action $ UpdateEntries entries
          driver $ action $ UpdateLoadingStatus Loaded
        Left  err     -> do
          driver $ action $ ShowError $ show err

      log "New Data!"

    decodeEntries :: JObject -> Either Foreign.ForeignError (Array Entry)
    decodeEntries = rmap runEntryList <<< lmap Foreign.JSONError <<< decodeJson <<< fromObject

    setupFB :: forall eff. Eff (firebase :: FB.FirebaseEff | eff) AppConfig
    setupFB = do
      -- This *should* break loudly.
      let fbUri = fromRight $ runParseURI "https://giflib-web.firebaseio.com/"
      fb <- FB.newFirebase fbUri
      return $ AppConfig { firebase: fb }
