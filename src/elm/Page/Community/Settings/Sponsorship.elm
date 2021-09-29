module Page.Community.Settings.Sponsorship exposing (Model, Msg, init, msgToString, update, view)

import Html exposing (Html, div)
import Session.LoggedIn as LoggedIn
import UpdateResult as UR



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- TYPES


type Msg
    = NoOp


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model _ =
    UR.init model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    { title = "", content = div [] [] }



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    []
