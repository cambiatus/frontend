module Page.Community.Invite exposing
    ( Model
    , Msg
    , init
    , msgToString
    , update
    , view
    )

import Html exposing (Html, div, text)
import Page exposing (Session)
import Session.Guest as Guest


init : Session



--- > String -> ( Model, Cmd Msg )
-- init _ _ =
--     -- init : ( Model, Cmd Msg )
--     ( initModel, Cmd.none )


initModel =
    { status = NoOp }


type alias Model =
    { status : Status
    }


type Status
    = NoOp



-- view : Guest -> Html msg


view _ =
    div [] [ text "invitation will be here" ]


type Msg
    = NoOp2


update model =
    ( model, Cmd.none )


msgToString _ =
    ggedIn
        [ "" ]
