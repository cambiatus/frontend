module Page.Community.Settings.Contacts exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Community
import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (class)
import Page
import Session.LoggedIn as LoggedIn
import UpdateResult as UR



-- MODEL


type Model
    = Authorized
    | Loading
    | Unauthorized


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( Loading, LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn )



-- TYPES


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            if community.creator == loggedIn.accountName then
                Authorized
                    |> UR.init

            else
                Unauthorized
                    |> UR.init



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        title =
            t "settings.contacts.title"

        content =
            case model of
                Authorized ->
                    div []
                        [ Page.viewHeader loggedIn title
                        , div [ class "bg-white container mx-auto pt-6 pb-7 px-4 lg:px-6" ]
                            [ p [ class "text-gray-900" ]
                                -- TODO - I18N
                                [ text "Adicione os contatos que ficarão disponíveis como suporte para os membros da comunidade." ]

                            -- TODO - I18N
                            , h2 [ class "label mt-10" ] [ text "Contact options" ]
                            ]
                        ]

                Loading ->
                    Page.fullPageLoading loggedIn.shared

                Unauthorized ->
                    Page.fullPageNotFound (t "community.edit.unauthorized") ""
    in
    { title = title, content = content }



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]
