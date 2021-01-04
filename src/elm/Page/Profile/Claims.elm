module Page.Profile.Claims exposing
    ( Model
    , Msg(..)
    , init
    , msgToString
    , view
    )

import Html exposing (Html, div, text)
import Session.LoggedIn as LoggedIn


type alias Model =
    {}


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init loggedIn account =
    ( {}, Cmd.none )


type Msg
    = NoOp



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        pageTitle =
            t "profile.claims.title"

        content =
            div [] [ text "Empty yet" ]
    in
    { title = pageTitle, content = content }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        _ ->
            []
