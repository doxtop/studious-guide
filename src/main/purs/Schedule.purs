module Schedule (renderIn) where

import Prelude
-- | import Air (Air, TicketAction(Order), spec, init)
import Genre (Genre(..), showGenre, initGenre)
import Err (ErrorAction(..), notification)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, throw)
import Control.Monad.Aff (Aff, attempt, runAff_)
import Control.Monad.Aff.Console(CONSOLE)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)

import Data.Array(concat, cons)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.List (List, fromFoldable, (!!))
import Data.List.Types (NonEmptyList)
import Data.Tuple (Tuple(..), uncurry)
import Data.Foldable (fold, sequence_)

import Data.Lens (Lens', Prism', over, lens, prism, _Just)

import Data.Foreign (F, Foreign, ForeignError, readArray, readString, readInt, renderForeignError, readNullOrUndefined)
import Data.Foreign.Index ((!))
import Data.Traversable (traverse)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.HTML.Types (htmlDocumentToNonElementParentNode) as DOM
import DOM.Node.Types (Element, ElementId(ElementId)) as DOM
import DOM.Node.NonElementParentNode (getElementById) as DOM
import ReactDOM (render) as RDOM
import React (ReactElement, ReactClass, createElement) as R
import React.DOM (body', div, table, td', text, tr', input, a, th', span, th, h3') as R
import React.DOM.Props as RP
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

import Network.HTTP.Affjax as Affjax
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.StatusCode (StatusCode(StatusCode))

import Simple.JSON (readJSON)

-- | One row in schedule table represent Performance model from server code.
-- | React Ui specification by Thermite and tickets ordering.

data TicketAction
    = Order
    | Return

data Status
    = NotStarted
    | OpenForSale
    | SoldOut
    | InThePast

instance showStatus  :: Show Status where
    show NotStarted  = "sale not started"
    show OpenForSale = "open for sale"
    show SoldOut     = "sold out"
    show InThePast   = "in the past"

makeStatus :: String -> Maybe Status
makeStatus "not started"    = Just NotStarted
makeStatus "in the past"    = Just InThePast
makeStatus "open for sale"  = Just OpenForSale
makeStatus "sold out"       = Just SoldOut
makeStatus _                = Nothing

type Air = 
    { title     :: String
    , vacant    :: Int
    , available :: Int
    , status    :: Status
    , price     :: Int
    , genre     :: Genre
    }

-- | Table row specification
spec :: forall eff props. T.Spec eff Air props TicketAction
spec = T.simpleSpec action render
    where 
        render :: T.Render Air props TicketAction
        render dispatch _ s _  =
            [ R.tr' <<< map (R.td' <<< pure) $
                [ R.span [ RP.className "title" ]   [ R.text s.title]
                , R.text $ show s.vacant
                , R.text $ show s.available
                , R.span [ RP.className "status"] [ R.text $ show s.status ]
                , R.span [ RP.className "price" ] [ R.text $  "€" <> show s.price ]
                , case s.status of
                    -- | add tickets availability condition
                    OpenForSale ->
                    R.a   [ RP.href "javascript:void(0);"
                        , RP.className "btn"
                        , RP.value "Buy"
                        , RP.onClick \_ -> dispatch Order
                        ] [R.text "Buy"]
                    _ -> R.text ""
                ]
            ]
        action :: T.PerformAction eff Air props TicketAction
        action _           _ _ = pure unit

-- | Schedule specification

data ScheduleAction 
    = SetGenre (Maybe Genre)
    | SetDate String
    | ErrorAction ErrorAction
    | TicketAction Int TicketAction

-- | Prism to show schedule actions
_ScheduleAction :: Prism' ScheduleAction (Tuple Int TicketAction)
_ScheduleAction = prism (uncurry TicketAction) \pa -> 
    case pa of
        TicketAction i a -> Right(Tuple i a)
        _ -> Left pa

type ScheduleState = 
    { shows :: List Air
    , genre :: Maybe Genre
    , error :: Maybe String
    , date :: String
    }

type URL = String
type ContainerId = String

-- | Lens to performances of inventory
_shows :: Lens' ScheduleState (List Air)
_shows = lens _.shows (_ {shows = _})

_error :: Lens' ScheduleState (Maybe String)
_error = lens _.error (_ {error = _})

_ErrorAction :: Prism' ScheduleAction ErrorAction
_ErrorAction = prism ErrorAction \ea ->
    case ea of
        ErrorAction a -> Right(a)
        _ -> Left ea

-- | _date = lends _.date (_ {date = _})

-- | Specification for inventory as a List of show performances.
inventory :: forall e props. T.Spec (console::CONSOLE, ajax::AJAX | e) ScheduleState props ScheduleAction 
inventory = container $ fold 
    [ header
    , inventoryActions
    , T.withState \_ -> T.focus _error _ErrorAction $ T.split _Just notification
    , table $ T.withState \st ->
        T.focus _shows _ScheduleAction $
            T.foreach \_ -> applyFilter st.genre spec
    , footer
    ]
    where
        -- | Container
        container 
            :: forall state action. T.Spec (console::CONSOLE, ajax::AJAX | e) state props action 
            -> T.Spec (console::CONSOLE, ajax::AJAX | e) state props action
        container = over T._render \render d p s c ->
            [ R.div [RP.className "section"] (render d p s c) ]

        -- | Header
        header :: T.Spec (console::CONSOLE, ajax::AJAX | e) ScheduleState props ScheduleAction
        header = T.simpleSpec performAction render
            where
            render :: T.Render ScheduleState props ScheduleAction
            render dispatch _ s _ =
                    [ R.div [RP.className "content"] 
                            [ R.h3' [R.text "Tickets4Sale"]
                            , R.div [RP.className "date-filter"] 
                                    [ R.text "Show date: "
                                    , R.input   [ RP._type "date"
                                                , RP.onInput \e -> dispatch $ SetDate (unsafeCoerce e).target.value
                                                ] []
                                    ]
                            , R.div [RP.className "filter"] $ 
                                cons (R.a [ RP.href "javascript:void(0);"
                                          , RP.className "clear" 
                                          , RP.title "show all" 
                                          , RP.onClick \_ -> dispatch (SetGenre Nothing)
                                          ] [R.text "x"]) $ map filter_ [Drama, Comedy, Musical]
                            ]
                    ]
                    where
                    filter_ :: Genre -> R.ReactElement
                    filter_ g = R.a [ RP.className (case s.genre of 
                                                        Just g2 | g2 == g -> "btn active"
                                                        _ -> "btn")
                                    , RP.href "javascript:void(0);"
                                    , RP.onClick \_ -> dispatch (SetGenre (Just g))
                                    ] [ R.text (showGenre g) ]

            performAction :: T.PerformAction (console::CONSOLE, ajax::AJAX | e) ScheduleState props ScheduleAction
            performAction (SetDate date) _ _ =  do
                    resp <- lift (filterByDate date)
                    case resp of
                        Left e -> void $ T.modifyState (_ {error = Just e})
                        Right st -> void $ T.modifyState (_ {shows = st.shows, date = date, error = Nothing}) 

                    -- | handle date on js side
            performAction _           _ _ = pure unit

        -- | Notfication
        -- | Creaete compoenent and T.split the state

        -- | Genre

        applyFilter
            :: forall action. Maybe Genre 
            -> T.Spec (console::CONSOLE, ajax::AJAX | e) Air props action 
            -> T.Spec (console::CONSOLE, ajax::AJAX | e) Air props action
        applyFilter genre = over T._render \render d p s c -> 
            if matches genre s
                then render d p s c
                else []
            where
            matches Nothing _ = true
            matches (Just val) sp = sp.genre == val

        -- | Main Table
        table
            :: T.Spec (console::CONSOLE, ajax::AJAX | e) ScheduleState props ScheduleAction 
            -> T.Spec (console::CONSOLE, ajax::AJAX | e) ScheduleState props ScheduleAction
        table = over T._render \render dispatch p s c ->
            let handleKeyPress :: Int -> String -> T.EventHandler
                handleKeyPress _  _    = pure unit
            in  [ R.table   [ RP.className "content table striped"]
                            [ R.body' $ [ R.tr' [ R.th' [R.text "Title"]
                                                -- | , R.th' [R.text "From"]
                                                , R.th' [R.text "Tickets left"]
                                                , R.th' [R.text "Tickets available"]
                                                , R.th' [R.text "Status"]
                                                , R.th  [RP.colSpan 2] [R.text "Price"]
                                                ]
                                        ] <> render dispatch p s c
                            ]
                ]
        footer :: T.Spec (console::CONSOLE, ajax::AJAX | e) ScheduleState props ScheduleAction
        footer = T.simpleSpec T.defaultPerformAction render
            where
                render :: T.Render ScheduleState props ScheduleAction
                render dispatch _ _ _ = 
                    [R.div  [ RP.className "content footer"] 
                            [ R.text "t4s (c) 2018"]]

        -- | Handle buy tickets actions from child component
        inventoryActions :: T.Spec (console::CONSOLE, ajax::AJAX | e) ScheduleState props ScheduleAction
        inventoryActions = T.simpleSpec performAction T.defaultRender
            where
            performAction :: T.PerformAction (console::CONSOLE, ajax::AJAX | e) ScheduleState props ScheduleAction
            performAction (TicketAction i Order) _ st = do
                res <- lift $ fromMaybe (pure $ Left "no show selected") $ orderTicket <$> (st.shows !! i) <*> Just st.date
                case res of
                    Left e -> void $ T.modifyState (_ {error = Just e})
                    Right sws ->
                        void $ T.modifyState (_ {shows = sws.shows})

            performAction (SetGenre f)           _ _ = void $ T.modifyState (_ { genre = f })
            performAction (ErrorAction Clear)    _ _ = void $ T.modifyState (_ { error = Nothing})
            performAction _                      _ _ = pure unit

orderTicket :: forall e. Air -> String -> Aff (ajax::AJAX|e) (Either String ScheduleState)
orderTicket s d = do
    resp <- attempt $ Affjax.get ("http://localhost:8080/order/" <> s.title <> "/" <> d) 
    case resp of
        Left err -> pure $ Left (show err)
        Right { status: StatusCode 200, response: "{\"error\":\"No more tickets available\"}" } ->
            -- | todo: handle json undefined fields correctly
            pure $ Left ("No more tickets for " <> s.title <> " available today")
        Right { status: StatusCode 200, response: r } ->
            case (handleJson r :: Either (NonEmptyList ForeignError) ScheduleState) of
                Left e -> pure $ Left (show $ map renderForeignError e)
                Right rs -> pure $ Right rs
        Right { response: err } -> pure $ Left (show err)

filterByDate :: forall e. String -> Aff (ajax::AJAX | e) (Either String ScheduleState)
filterByDate d = do
    resp <- attempt $ Affjax.get ("http://localhost:8080/inventory/date/" <> d)
    case resp of 
        Left err -> pure $ Left (show err)
        Right { status: StatusCode 200, response: r } -> 
            case (handleJson r :: Either (NonEmptyList ForeignError) ScheduleState) of 
                Left e  -> pure $ Left (show $ map renderForeignError e)
                Right rs -> pure $ Right rs
        Right { response: err } ->  pure $ Left (show err)

renderIn :: ContainerId -> URL -> Eff (ajax :: AJAX, dom :: DOM, exception :: EXCEPTION) Unit
renderIn containerID url = do
  get' url throw success
    where
      success :: forall e. String -> Eff (exception :: EXCEPTION, dom :: DOM | e) Unit
      success s =
        case (handleJson s :: Either (NonEmptyList ForeignError) ScheduleState) of
          Left errs -> sequence_ $ map renderForeignError errs <#> throw
          Right initialState -> do
            let component = T.createClass inventory initialState
            renderComponent containerID component unit

renderComponent :: forall props eff. ContainerId -> R.ReactClass props -> props -> Eff (dom :: DOM, exception :: EXCEPTION | eff) Unit
renderComponent containerId component props = do
  container <- getElementById' containerId
  case container of
    Nothing -> void $ throw $ "Element with ID \"" <> containerId <> "\" doesn't exist"
    Just cont -> void $ RDOM.render (R.createElement component props []) cont
    where
      getElementById' :: forall e. ContainerId -> Eff (dom :: DOM | e) (Maybe DOM.Element)
      getElementById' cid = do
        doc <- DOM.window >>= DOM.document
        DOM.getElementById (DOM.ElementId $ cid) (DOM.htmlDocumentToNonElementParentNode doc)


get' :: forall e. URL -> (String -> Eff (ajax :: AJAX | e) Unit) -> (String -> Eff (ajax :: AJAX | e) Unit) -> Eff (ajax :: AJAX | e) Unit
get' url failure success = runAff_ f (get url)
  where
    f :: Either Error (Either String String) -> Eff (ajax :: AJAX | e) Unit
    f (Left e) = failure (show e)
    f (Right (Left e)) = failure (show e)
    f (Right (Right a)) = success a

get :: forall e. URL -> Aff (ajax :: AJAX | e) (Either String String)
get url = do 
  resp <- attempt $ Affjax.get url
  pure $ case resp of
    Left err -> Left $ "Request failed: " <> show err
    Right { status: StatusCode 200, response: r } -> Right r
    Right { response: err } -> Left $ "Request failed: " <> err

-- | consider to use Argonaut below

handleJson :: String -> Either (NonEmptyList ForeignError) ScheduleState
handleJson json = case readJSON json of
    Left errors -> Left errors
    Right x -> runExcept $ fromJson x

fromJson :: Foreign -> F ScheduleState
fromJson val = do
    inv  <- val ! "inventory" >>= readArray >>= traverse readInventory
    err  <- val ! "error" >>= readNullOrUndefined >>= traverse readString

    pure $ { shows: (fromFoldable $ concat inv), genre: Nothing, error: err , date: "" }

readInventory :: Foreign -> F (Array Air)
readInventory val = do
    g       <- val ! "genre" >>= readString
    shows   <- val ! "shows" >>= readArray >>= traverse \v -> readOnAir v (initGenre g)
    pure $ shows

readOnAir :: Foreign -> Genre -> F Air
readOnAir val g = do 
    t   <- val ! "title"             >>= readString
    v   <- val ! "tickets available" >>= readInt
    a   <- val ! "tickets left"      >>= readInt
    s   <- val ! "status"            >>= readString
    p   <- val ! "price"             >>= readInt
    pure $ { title: t, vacant:v, available:a, status: fromMaybe InThePast $ makeStatus s, price:p, genre: g}
