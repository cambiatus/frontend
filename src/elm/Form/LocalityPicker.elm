module Form.LocalityPicker exposing
    ( Model, Kind(..), Availability(..), init, availability
    , ViewConfig, view
    , Msg, UpdateResult, Request, update, msgToString
    )

{-| An autocomplete over the `localities` query: the municípios (or cantones) of
a country, or the bairros (or distritos) of one município. Follows
`Form.UserPicker`: it has its own model and messages, and the same menu and
keyboard handling (`View.Select`). The options come from the server, so the
picker waits 250 ms after the last keystroke before it asks.

The picker doesn't talk to the API itself. `update` hands back a `Request` for
the page to run with the member's token, and the page sends the answer back
through `Request.toMsg`.


# Model

@docs Model, Kind, Availability, init, availability


# View

@docs ViewConfig, view


# The elm architecture

@docs Msg, UpdateResult, Request, update, msgToString

-}

import Cambiatus.Query
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Process
import Profile.Location as Location exposing (Country(..), Locality)
import RemoteData exposing (RemoteData)
import Task
import View.Components
import View.Select



-- MODEL


type Kind
    = Municipalities Country
    | Bairros { country : Country, municipalityId : Int }


{-| Whether a list of bairros is there to choose from. `Unknown` until the first
answer arrives, and `Unavailable` when it is empty or couldn't be loaded, so the
page can offer a text field instead.
-}
type Availability
    = Unknown
    | Available
    | Unavailable


type Model
    = Model
        { id : String
        , kind : Kind
        , selectState : View.Select.State
        , options : List Locality
        , counter : Int
        , availability : Availability
        }


{-| A picker for municípios starts empty. A picker for bairros asks for its first
options at once, to know whether there are any.
-}
init : { id : String, kind : Kind } -> ( Model, Maybe Request )
init { id, kind } =
    let
        model =
            Model
                { id = id
                , kind = kind
                , selectState = View.Select.newState id
                , options = []
                , counter = 1
                , availability = Unknown
                }
    in
    case kind of
        Municipalities _ ->
            ( model, Nothing )

        Bairros _ ->
            ( model, Just (request kind 1 Nothing) )


availability : Model -> Availability
availability (Model model) =
    model.availability



-- VIEW


type alias ViewConfig =
    { label : String
    , placeholder : String
    , notFound : String
    , hasError : Bool
    }


view : ViewConfig -> Model -> Html Msg
view config (Model model) =
    div [ class "mb-4" ]
        [ View.Components.label [] { targetId = model.id, labelText = config.label }
        , View.Select.view (settings config model.kind)
            model.selectState
            model.options
            []
            |> Html.map GotSelectMsg
        ]


settings : ViewConfig -> Kind -> View.Select.Config Msg Locality
settings config kind =
    View.Select.newConfig
        { onSelect = SelectedLocality
        , toLabel = rowLabel kind
        , filter = \_ items -> Just items
        , onFocusItem = NoOp
        }
        |> View.Select.withInputClass "w-full input"
        |> View.Select.withInputClassList [ ( "with-error", config.hasError ) ]
        |> View.Select.withPrompt config.placeholder
        |> View.Select.withItemHtml (viewRow kind)
        |> View.Select.withItemClass "block w-full text-left hover:bg-gray-100 focus:bg-gray-100 active:bg-gray-200"
        |> View.Select.withNotFound config.notFound
        |> View.Select.withNotFoundClass "px-4 py-3 text-gray-333"
        |> View.Select.withMenuClass "flex flex-col w-full border border-solid border-gray-300 rounded-sm z-30 bg-white max-h-72 overflow-auto shadow-lg"
        |> View.Select.withEmptySearch (isBairros kind)
        |> View.Select.withOnBlur BlurredPicker


isBairros : Kind -> Bool
isBairros kind =
    case kind of
        Bairros _ ->
            True

        Municipalities _ ->
            False


{-| "{município} – {UF}" for Brazil, "{cantón} – {provincia}" for Costa Rica, and
just the name for bairros and distritos
-}
rowLabel : Kind -> Locality -> String
rowLabel kind locality =
    case kind of
        Bairros _ ->
            locality.name

        Municipalities country ->
            case ( country, locality.stateCode ) of
                ( CostaRica, Just code ) ->
                    locality.name ++ " – " ++ Location.provinceName code

                ( _, Just code ) ->
                    locality.name ++ " – " ++ code

                ( _, Nothing ) ->
                    locality.name


viewRow : Kind -> Locality -> Html Never
viewRow kind locality =
    div [ class "px-4 py-3 text-gray-333 text-left" ]
        [ span [] [ text (rowLabel kind locality) ] ]



-- UPDATE


type Msg
    = NoOp
    | GotSelectMsg (View.Select.Msg Locality)
    | SelectedLocality Locality
    | BlurredPicker
    | DebounceElapsed Int
    | GotOptions Int (RemoteData (Graphql.Http.Error (List Locality)) (List Locality))


{-| Something for the page to ask the API, and how to hand the answer back
-}
type alias Request =
    { selectionSet : SelectionSet (List Locality) RootQuery
    , toMsg : RemoteData (Graphql.Http.Error (List Locality)) (List Locality) -> Msg
    }


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , request : Maybe Request
    , selected : Maybe Locality
    }


debounceMillis : Float
debounceMillis =
    250


update : ViewConfig -> Msg -> Model -> UpdateResult
update config msg (Model model) =
    let
        unchanged =
            { model = Model model, cmd = Cmd.none, request = Nothing, selected = Nothing }
    in
    case msg of
        NoOp ->
            unchanged

        BlurredPicker ->
            unchanged

        GotSelectMsg subMsg ->
            let
                ( newState, cmd ) =
                    View.Select.update (settings config model.kind) subMsg model.selectState

                queryChanged =
                    View.Select.queryFromState newState /= View.Select.queryFromState model.selectState

                counter =
                    if queryChanged then
                        model.counter + 1

                    else
                        model.counter
            in
            { unchanged
                | model = Model { model | selectState = newState, counter = counter }
                , cmd =
                    Cmd.batch
                        [ cmd
                        , if queryChanged then
                            Process.sleep debounceMillis
                                |> Task.perform (\_ -> DebounceElapsed counter)

                          else
                            Cmd.none
                        ]
            }

        SelectedLocality locality ->
            -- Bumping the counter drops the answer of any search still on its way
            { unchanged
                | model = Model { model | counter = model.counter + 1 }
                , selected = Just locality
            }

        DebounceElapsed counter ->
            if counter /= model.counter then
                unchanged

            else
                case View.Select.queryFromState model.selectState of
                    Nothing ->
                        unchanged

                    Just query ->
                        if shouldSearch model.kind query then
                            { unchanged | request = Just (request model.kind counter (nonEmpty query)) }

                        else
                            { unchanged | model = Model { model | options = [] } }

        GotOptions counter result ->
            if counter /= model.counter then
                unchanged

            else
                case result of
                    RemoteData.Success options ->
                        { unchanged
                            | model =
                                Model
                                    { model
                                        | options = options
                                        , availability =
                                            if model.availability == Unknown then
                                                if List.isEmpty options then
                                                    Unavailable

                                                else
                                                    Available

                                            else
                                                model.availability
                                    }
                        }

                    RemoteData.Failure _ ->
                        { unchanged
                            | model =
                                Model
                                    { model
                                        | options = []
                                        , availability =
                                            if model.availability == Unknown then
                                                Unavailable

                                            else
                                                model.availability
                                    }
                        }

                    _ ->
                        unchanged


{-| A município needs two letters to search (a UF is two); a list of bairros
answers any query, including none
-}
shouldSearch : Kind -> String -> Bool
shouldSearch kind query =
    case kind of
        Municipalities _ ->
            String.length (String.trim query) >= 2

        Bairros _ ->
            True


nonEmpty : String -> Maybe String
nonEmpty query =
    case String.trim query of
        "" ->
            Nothing

        trimmed ->
            Just trimmed


request : Kind -> Int -> Maybe String -> Request
request kind counter maybeQuery =
    let
        fill optionals =
            case kind of
                Municipalities country ->
                    { optionals
                        | query = fromMaybe maybeQuery
                        , country = fromMaybe (Location.countryCode country)
                        , first = Present 10
                    }

                Bairros { municipalityId } ->
                    { optionals
                        | query = fromMaybe maybeQuery
                        , kind = Present "bairro"
                        , municipalityId = Present municipalityId
                        , first = Present 50
                    }
    in
    { selectionSet = Cambiatus.Query.localities fill Location.localitySelectionSet
    , toMsg = GotOptions counter
    }


fromMaybe : Maybe a -> OptionalArgument a
fromMaybe maybe =
    case maybe of
        Just value ->
            Present value

        Nothing ->
            Absent


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        GotSelectMsg _ ->
            [ "GotSelectMsg" ]

        SelectedLocality _ ->
            [ "SelectedLocality" ]

        BlurredPicker ->
            [ "BlurredPicker" ]

        DebounceElapsed _ ->
            [ "DebounceElapsed" ]

        GotOptions _ result ->
            [ "GotOptions", RemoteData.toMaybe result |> Maybe.map (always "Success") |> Maybe.withDefault "Failure" ]
