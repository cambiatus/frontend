module Page.Profile.KycEditor exposing
    ( Model
    , Msg(..)
    , UpdateResult
    , init
    , msgToString
    , update
    , view
    )

import Api
import Api.Graphql
import Avatar exposing (Avatar)
import File exposing (File)
import Graphql.Http
import Html exposing (Html, button, div, form, input, label, option, p, select, span, text, textarea)
import Html.Attributes exposing (accept, attribute, class, disabled, for, id, maxlength, multiple, placeholder, selected, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Icons
import Json.Decode
import Kyc exposing (ProfileKyc)
import Kyc.CostaRica.CedulaDeIdentidad as CedulaDeIdentidad
import Kyc.CostaRica.Dimex as Dimex
import Kyc.CostaRica.Nite as Nite
import Page exposing (Session)
import Profile exposing (Profile)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import Session.Shared exposing (Translators)
import UpdateResult as UR



-- INIT


type Msg
    = NoOp


type alias Model =
    {}


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( {}
    , Cmd.none
    )


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            model |> UR.init


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    { title = "Edit KYC page"
    , content = text ""
    }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]
