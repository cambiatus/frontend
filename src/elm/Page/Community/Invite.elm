module Page.Community.Invite exposing
    ( Model
    , Msg
    , init
    , msgToString
    , update
    , view
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Page exposing (Session(..))
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn


init : Session -> String -> ( Model, Cmd Msg )
init _ invitationId =
    -- TODO: get invitation then get community
    ( initModel, Cmd.none )


initModel =
    { status = NoOp }


type alias Model =
    { status : Status
    }


type Status
    = NoOp


view : Session -> Html msg
view session =
    div [ class "flex flex-col min-h-screen" ]
        [ div [ class "flex-grow" ] [ text "vish" ]
        , viewFooter session
        ]


viewFooter : Session -> Html msg
viewFooter session =
    let
        shared =
            case session of
                LoggedIn loggedIn ->
                    loggedIn.shared

                Guest guest ->
                    guest.shared
    in
    LoggedIn.viewFooter shared


type Msg
    = NoOp2


update model =
    ( model, Cmd.none )


msgToString _ =
    [ "" ]
