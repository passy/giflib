module Main where

import Prelude

import qualified DOM.HTML as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.Node.Node as DOM
import qualified DOM.Node.ParentNode as DOM
import qualified Data.Date as Date
import qualified Data.Date.UTC as Date
import qualified Data.Foreign as Foreign
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.StrMap as StrMap
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Elements as El
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Types as E
import qualified Halogen.HTML.Properties as A
import qualified MDL as MDL
import qualified MDL.Button as MDL
import qualified MDL.Spinner as MDL
import qualified MDL.Textfield as MDL
import qualified Node.UUID as NUUID
import qualified Web.Firebase as FB
import qualified Web.Firebase.DataSnapshot as DS
import qualified Web.Firebase.Types as FB

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION(..))
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import Css.Background (BackgroundImage(..), backgroundImage)
import Css.String (fromString)
import Css.Stylesheet (Css(), Rule(..))
import Data.Argonaut.Core (JObject(), fromObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(Left, Right), either)
import Data.Either.Unsafe (fromRight)
import Data.Enum (fromEnum)
import Data.Foldable (intercalate)
import Data.Functor (($>))
import Data.Identity (Identity(), runIdentity)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Nullable (toMaybe)
import Data.String (joinWith, trim, split)
import Data.Tuple (Tuple(..))
import Data.URI (runParseURI, parseURI)
import Data.URI.Types (URI())

import Web.Giflib.Internal.Unsafe
import Web.Giflib.Types (Tag(), Entry(..), uuid, runUUID, runEntryList)

-- TODO: Be more specific, getting some weird compiler errors if I
--       try to import HalogenEffects() though.
import Halogen

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
             , newUrl        :: Maybe URI     -- ^ New URI to be submitted
             , newTags       :: Set.Set Tag   -- ^ New Tags to be submitted
             , error         :: String        -- ^ Global UI error to be shown
             , loadingStatus :: LoadingStatus -- ^ List loading state
             }

data Action
  = NoOp
  | ResetNewForm
  | LoadingAction LoadingStatus Action
  | UpdateNewURI String
  | UpdateNewTags String
  | UpdateEntries (Array Entry)
  | ShowError String

newtype AppConfig = AppConfig { firebase :: FB.Firebase }

type AppEffects eff = HalogenEffects ( uuid :: NUUID.UUIDEff
                                     , console :: CONSOLE
                                     , now :: Date.Now
                                     , firebase :: FB.FirebaseEff | eff)

data Request
  = AddNewEntry State

emptyState :: State
emptyState = { entries: mempty
             , tag: mempty
             , newUrl: Nothing
             , newTags: Set.empty
             , error: mempty
             , loadingStatus: Loading
             }

{-- update :: State -> Action -> State --}
{-- update s' a = updateState a s' --}
{--   where --}
{--   updateState NoOp s                = s --}
{--   updateState ResetNewForm s        = s { newUrl = mempty --}
{--                                         -- Typechecker doesn't like Set.empty --}
{--                                         -- here, I don't know why. --}
{--                                         , newTags = (Set.empty :: Set.Set Tag) --}
{--                                         } --}
{--   updateState (LoadingAction l a) s = updateState a $ s { loadingStatus = l } --}
{--   updateState (UpdateNewURI e) s    = s { newUrl  = unsafeShowPrintId $ either (const Nothing) (pure) (runParseURI <<< parseURI $ e) } --}
{--   updateState (UpdateNewTags e) s   = s { newTags = processTagInput e } --}
{--   updateState (UpdateEntries e) s   = s { entries = e } --}
{--   updateState (ShowError e) s       = s { error   = e } --}

-- | Handle a request to an external service
{-- handler :: forall eff m. --}
{--   AppConfig -> --}
{--   Request -> --}
{--   E.Event (AppEff eff) Action --}
{-- handler (AppConfig conf) (AddNewEntry s) = do --}
{--   id' <- liftEff NUUID.v4 --}
{--   now <- liftEff Date.now --}
{--   let entry = Entry { id: uuid $ show id' --}
{--                     , tags: s.newTags --}
{--                     , url: s.newUrl --}
{--                     , date: now --}
{--                     } --}

{--   -- TODO: Would be nice to have a MonadReader/ReaderT for this. Like, really --}
{--   -- nice. But I'm too stupid. --}
{--   children <- liftEff $ FB.child "entries" conf.firebase --}
{--   liftEff $ FB.push (Foreign.toForeign $ unsafeShowPrintId $ encodeJson entry) Nothing children --}
{--   E.yield ResetNewForm --}

{-- ui :: forall eff. AppConfig -> Component (E.Event (AppEff eff)) Action Action --}
{-- ui conf = render <$> stateful emptyState update --}
{--   where --}
{--   render :: State -> H.HTML (E.Event (AppEff eff) Action) --}
{--   render st = --}
{--     H.div [ A.class_ $ A.className "gla-content" ] $ --}
{--       [ H.form [ A.onSubmit \_ -> E.preventDefault $> (handler conf $ (AddNewEntry st)) --}
{--                , A.class_ $ A.className "gla-layout--margin-h" --}
{--                ] --}
{--                [ H.div [ A.class_ $ A.className "gla-form--inline-group" ] [ --}
{--                  MDL.textfield [ E.onInput (A.input UpdateNewURI <<< url) --}
{--                                , A.required true ] $ --}
{--                   MDL.defaultTextfield { id = Just "inp-new-gif" --}
{--                                        , label = Just "URL" --}
{--                                        , type_ = "url" --}
{--                                        } ] --}
{--                , H.div [ A.class_ $ A.className "gla-form--inline-group" ] [ --}
{--                  MDL.textfield [ E.onInput $ A.input UpdateNewTags --}
{--                                , A.required true ] $ --}
{--                    MDL.defaultTextfield { id = Just "inp-new-tags" --}
{--                                         , label = Just "Tags" --}
{--                                         } ] --}
{--                , H.div [ A.class_ $ A.className "gla-form--inline-group" ] [ --}
{--                  MDL.button $ --}
{--                    MDL.defaultButton { text = "Add GIF" --}
{--                                      , elevation = MDL.ButtonRaised --}
{--                                      } ] --}
{--                ] --}
{--       , MDL.spinner (st.loadingStatus == Loading) --}
{--       , H.div [ A.class_ $ A.className "gla-card-holder" ] $ map entryCard st.entries --}
{--       ] --}

{--     where --}

{--     entryCard :: Entry -> H.HTML (E.Event (AppEff eff) Action) --}
{--     entryCard (Entry e) = H.div --}
{--         [ A.classes [ MDL.card, MDL.shadow 3 ] --}
{--         , A.key $ runUUID e.id --}
{--         ] --}
{--         [ H.div [ A.class_ MDL.cardImageContainer --}
{--                 , El.style $ backgroundImage $ entryBackground e --}
{--                 ] [] --}
{--         , H.div [ A.class_ MDL.cardHeading ] --}
{--             [ H.h2 --}
{--                 [ A.class_ MDL.cardHeadingText ] [ H.text $ formatEntryTags e ] --}
{--             ] --}
{--         , H.div [ A.class_ MDL.cardCaption ] [ H.text $ formatEntryDatetime e ] --}
{--         , H.div [ A.class_ MDL.cardBottom ] --}
{--             [ H.a --}
{--                 [ A.href $ printURI e.uri --}
{--                 , A.class_ MDL.cardUri --}
{--                 , A.target "_blank" ] [ H.text $ printURI e.uri ] --}
{--             ] --}
{--         ] --}

{--     entryBackground :: forall e. { uri :: URI | e } -> BackgroundImage --}
{--     entryBackground e = --}
{--       let url = "url(" <> printURI e.url <> ")" --}
{--       in BackgroundImage $ fromString url --}

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
main :: forall eff. Eff (AppEffects eff) Unit
main = do
  log "Booting. Beep. Boop."
  let fbUri = fromRight $ runParseURI "https://giflib-web.firebaseio.com/"
  fb <- FB.newFirebase fbUri
  let conf = AppConfig { firebase: fb }

  {-- Tuple node driver <- runUI $ ui conf --}

  {-- children <- FB.child "entries" fb --}
  {-- FB.on FB.Value (dscb driver) Nothing children --}

  {-- win <- DOM.window --}
  {-- el <- DOM.querySelector "#app-main" win --}
  {-- case (toMaybe el) of --}
  {--   Just e -> DOM.appendChild (DOM.htmlElementToNode e) node --}
  {--   Nothing -> throwException $ error "Couldn't find #app-main. What've you done to the HTML?" --}

  log "Up and running."

  where
    -- TODO: Use Aff instead of Eff for this.
    dscb :: forall req eff. (Action -> eff) -> FB.DataSnapshot -> eff
    dscb driver ds =
      case (Foreign.unsafeReadTagged "Object" $ DS.val ds) >>= decodeEntries of
        Right entries -> driver (LoadingAction Loaded $ UpdateEntries entries)
        Left  err     -> driver $ ShowError $ show err

    decodeEntries :: JObject -> Either Foreign.ForeignError (Array Entry)
    decodeEntries = rmap runEntryList <<< lmap Foreign.JSONError <<< decodeJson <<< fromObject
