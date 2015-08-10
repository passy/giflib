module Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION(..))
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import Control.Monad.Trans (lift)
import Css.Background (BackgroundImage(..), backgroundImage)
import Css.String (fromString)
import Css.Stylesheet (Css(), Rule(..))
import Data.Argonaut.Core (JObject(), fromObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (concat, (!!))
import Data.Bifunctor (bimap)
import Data.DOM.Simple.Document ()
import Data.DOM.Simple.Element (querySelector, appendChild)
import Data.DOM.Simple.Window (document, globalWindow)
import Data.Either (Either(Left, Right))
import Data.Enum (fromEnum)
import Data.Foldable (intercalate)
import Data.Functor (($>))
import Data.Identity (Identity(), runIdentity)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.String (joinWith, trim, split)
import Data.Tuple (Tuple(..))
import Halogen (runUI, Driver(), HalogenEffects())
import Halogen.Component (Component(..))
import Halogen.HTML.Target (URL(), url, runURL)
import Halogen.Signal (SF1(..), stateful)
import qualified Data.Date as Date
import qualified Data.Date.UTC as Date
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.StrMap as StrMap
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.CSS as CSS
import qualified MDL as MDL
import qualified MDL.Button as MDL
import qualified MDL.Textfield as MDL
import qualified MDL.Spinner as MDL
import qualified Node.UUID as NUUID
import qualified Web.Firebase as FB
import qualified Web.Firebase.DataSnapshot as DS
import qualified Web.Firebase.Types as FB
import qualified Data.Foreign as Foreign

import Web.Giflib.Internal.Unsafe

import Web.Giflib.Types (Tag(), Entry(..), uuid, runUUID)

data LoadingStatus
 = Loading
 | Loaded
 | LoadingError String

instance eqLoadingStatus :: Eq LoadingStatus where
  eq Loading Loading                    = true
  eq Loaded Loaded                      = true
  eq (LoadingError a) (LoadingError b)  = a == b
  eq _ _                                = false

type State = { entries       :: Array Entry   -- ^ All entries matching the tag
             , tag           :: Maybe Tag     -- ^ Currently selected tag, if any
             , newUrl        :: URL           -- ^ New URL to be submitted
             , newTags       :: Set.Set Tag   -- ^ New Tags to be submitted
             , error         :: String        -- ^ Global UI error to be shown
             , loadingStatus :: LoadingStatus -- ^ List loading state
             }

data Action
  = NoOp
  | ResetNewForm
  | LoadingAction LoadingStatus Action
  | UpdateNewURL URL
  | UpdateNewTags String
  | UpdateEntries (Array Entry)
  | ShowError String

newtype AppConfig = AppConfig { firebase :: FB.Firebase }

type AppEnv = ReaderT AppConfig

data Request
  = AddNewEntry State

type AppEff eff = HalogenEffects ( uuid :: NUUID.UUIDEff
                                 , now :: Date.Now
                                 , firebase :: FB.FirebaseEff | eff)

emptyState :: State
emptyState = { entries: mempty
             , tag: mempty
             -- TODO: Add a Monoid instance to URL
             , newUrl: url mempty
             , newTags: Set.empty
             , error: mempty
             , loadingStatus: Loading
             }

update :: State -> Action -> State
update s' a = updateState a s'
  where
  updateState NoOp s                = s
  updateState ResetNewForm s        = s { newUrl  = url mempty
                                        -- Typechecker doesn't like Set.empty
                                        -- here, I don't know why.
                                        , newTags = (Set.empty :: Set.Set Tag)
                                        }
  updateState (LoadingAction l a) s = updateState a $ s { loadingStatus = l }
  updateState (UpdateNewURL e) s    = s { newUrl  = unsafeShowPrintId $ e }
  updateState (UpdateNewTags e) s   = s { newTags = processTagInput e }
  updateState (UpdateEntries e) s   = s { entries = e }
  updateState (ShowError e) s       = s { error   = e }

-- | Handle a request to an external service
handler :: forall eff m.
  AppConfig ->
  Request ->
  E.Event (AppEff eff) Action
handler (AppConfig conf) (AddNewEntry s) = do
  id' <- liftEff NUUID.v4
  now <- liftEff Date.now
  let entry = Entry { id: uuid $ show id'
                    , tags: s.newTags
                    , url: s.newUrl
                    , date: now
                    }

  -- TODO: Would be nice to have a MonadReader/ReaderT for this. Like, really
  -- nice. But I'm too stupid.
  children <- liftEff $ FB.child "entries" conf.firebase
  liftEff $ FB.push (Foreign.toForeign $ unsafeShowPrintId $ encodeJson entry) Nothing children
  E.yield ResetNewForm

ui :: forall eff. AppConfig -> Component (E.Event (AppEff eff)) Action Action
ui conf = render <$> stateful emptyState update
  where
  render :: State -> H.HTML (E.Event (AppEff eff) Action)
  render st =
    H.div [ A.class_ $ A.className "gla-content" ] $
      [ H.form [ A.onSubmit \_ -> E.preventDefault $> (handler conf $ (AddNewEntry st))
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
      , MDL.spinner (st.loadingStatus == Loading)
      , H.div [ A.class_ $ A.className "gla-card-holder" ] $ map entryCard st.entries
      ]

    where

    entryCard :: Entry -> H.HTML (E.Event (AppEff eff) Action)
    entryCard (Entry e) = H.div
        [ A.classes [ MDL.card, MDL.shadow 3 ]
        , A.key $ runUUID e.id
        ]
        [ H.div [ A.class_ MDL.cardImageContainer
                , CSS.style $ backgroundImage $ entryBackground e
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

    entryBackground :: forall e. { url :: URL | e } -> BackgroundImage
    entryBackground e =
      let url = "url(" <> runURL e.url <> ")"
      in BackgroundImage $ fromString url

formatEntryDatetime :: forall e. { date :: Date.Date | e } -> String
formatEntryDatetime e =
  intercalate "-" $ [ show <<< toNumber <<< getYear <<< Date.year $ e.date
                    , show <<< (+1) <<< fromEnum <<< Date.month $ e.date
                    , show <<< toNumber <<< getDay <<< Date.dayOfMonth $ e.date ]
  where
    getDay :: Date.DayOfMonth -> Int
    getDay (Date.DayOfMonth i) = i
    getYear :: Date.Year -> Int
    getYear (Date.Year i) = i

formatEntryTags :: forall e. { tags :: Set.Set Tag | e } -> String
formatEntryTags e = joinWith " " $ map (\x -> "#" ++ x) $ List.fromList $ Set.toList e.tags

processTagInput :: String -> Set.Set Tag
processTagInput = trim >>> split " " >>> List.toList >>> Set.fromList

-- Application Main
main = do
  log "Booting. Beep. Boop."
  fb <- FB.newFirebase $ url "https://giflib-web.firebaseio.com/"
  let conf = AppConfig { firebase: fb }

  Tuple node driver <- runUI $ ui conf

  children <- FB.child "entries" fb
  FB.on FB.Value (dscb driver) Nothing children

  doc <- document globalWindow
  el <- querySelector "#app-main" doc
  case el of
    Just e -> appendChild e node
    Nothing -> throwException $ error "Couldn't find #app-main. What've you done to the HTML?"
  log "Up and running."

  where
    -- TODO: Use Aff instead of Eff for this.
    dscb :: forall req eff. (Action -> eff) -> FB.DataSnapshot -> eff
    dscb driver ds =
      case (Foreign.unsafeReadTagged "Object" $ DS.val ds) >>= decodeEntries of
        Right entries -> driver (LoadingAction Loaded $ UpdateEntries entries)
        Left  err     -> driver $ ShowError $ show err

    decodeEntries :: JObject -> Either Foreign.ForeignError (Array Entry)
    decodeEntries = bimap Foreign.JSONError id <<< decodeJson <<< fromObject
