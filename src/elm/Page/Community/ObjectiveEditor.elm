module Page.Community.ObjectiveEditor exposing (Model, Msg, initEdit, initNew, jsAddressToMsg, msgToString, update, view)

import Eos as Eos exposing (Symbol)
import Html exposing (..)
import Html.Attributes exposing (..)
import I18Next exposing (Translations, t)
import Icons
import Json.Decode as Json exposing (Value)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR



-- INIT


initNew : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
initNew loggedIn communityId =
    ( { status = Creating communityId, communityId = communityId }, Cmd.none )


initEdit : LoggedIn.Model -> Symbol -> Int -> ( Model, Cmd Msg )
initEdit loggedIn communityId objectiveId =
    ( { status = Editing communityId objectiveId, communityId = communityId }, Cmd.none )



-- MODEL


type alias Model =
    { status : Status
    , communityId : Symbol
    }


type
    Status
    -- Create
    = Creating Symbol
      -- Edit
    | Editing Symbol Int
      -- Errors
    | LoadFailed
    | NotAuthorized



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    div []
        [ viewHeader loggedIn model
        , viewForm loggedIn model
        ]


viewHeader : LoggedIn.Model -> Model -> Html Msg
viewHeader ({ shared } as loggedIn) model =
    div [ class "h-16 w-full bg-indigo-500 flex px-4 items-center" ]
        [ a
            [ class "items-center flex absolute"
            , Route.href (Route.Community model.communityId)
            ]
            [ Icons.back ""
            , p [ class "text-white text-sm ml-2" ]
                [ text (t shared.translations "community.objectives.editor.back")
                ]
            ]
        , p [ class "text-white mx-auto" ] [ text "TODO: TITLE HERE" ]
        ]


viewForm : LoggedIn.Model -> Model -> Html Msg
viewForm ({ shared } as loggedIn) model =
    div [ class "bg-white w-full p-10" ]
        [ div [ class "w-form mx-auto" ]
            [ div [ class "mb-10" ]
                [ span [ class "input-label" ] [ text (t shared.translations "community.objectives.editor.description") ]
                , textarea [ class "form-textarea block w-full rounded border", rows 5 ] []
                ]
            , button [ class "button button-primary" ] [ text (t shared.translations "community.objectives.editor.submit") ]
            ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = No


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    UR.init model



-- UTILS


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        _ ->
            [ "" ]
