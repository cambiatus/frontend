module Page.Login exposing (Model, Msg, init, jsAddressToMsg, msgToString, subscriptions, update, view)

import Auth
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Encode exposing (Value)
import Route
import Session.Guest as Guest exposing (External(..))
import UpdateResult as UR



-- INIT


init : Guest.Model -> ( Model, Cmd Msg )
init guest =
    let
        authModel =
            Auth.init guest.shared Nothing
    in
    ( initModel authModel
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map GotAuthMsg (Auth.subscriptions model.auth)



-- MODEL


type alias Model =
    { loginError : Maybe String
    , auth : Auth.Model
    }


initModel : Auth.Model -> Model
initModel authModel =
    { loginError = Nothing
    , auth = authModel
    }



-- VIEW


view : Guest.Model -> Model -> { title : String, content : Html Msg }
view guest model =
    let
        authView =
            Auth.view False guest.shared model.auth
                |> List.map (Html.map GotAuthMsg)
    in
    { title =
        guest.shared.translators.t "auth.login.loginTab"
    , content =
        div [ class "bg-purple-500 flex-grow flex flex-wrap md:block" ]
            authView
    }



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg External


type Msg
    = GotAuthMsg Auth.Msg


update : Msg -> Model -> Guest.Model -> UpdateResult
update msg model guest =
    case msg of
        GotAuthMsg authMsg ->
            Auth.update authMsg guest.shared model.auth
                |> UR.map
                    (\a -> { model | auth = a })
                    GotAuthMsg
                    (\extMsg uResult ->
                        case extMsg of
                            Auth.ClickedCancel ->
                                uResult

                            Auth.CompletedAuth profile ->
                                uResult
                                    |> UR.addCmd
                                        (guest.afterLoginRedirect
                                            |> Maybe.withDefault Route.Dashboard
                                            |> Route.replaceUrl guest.shared.navKey
                                        )
                                    |> UR.addExt (UpdatedGuest { guest | profile = Just profile })

                            Auth.UpdatedShared newShared ->
                                UR.addExt (UpdatedGuest { guest | shared = newShared }) uResult
                    )


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "GotAuthMsg" :: remainAddress ->
            Auth.jsAddressToMsg remainAddress val
                |> Maybe.map GotAuthMsg

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        GotAuthMsg subMsg ->
            "GotAuthMsg" :: Auth.msgToString subMsg
