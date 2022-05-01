module Page.Community.Settings.Contacts exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Community
import Contact
import Form
import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (class)
import Page
import Session.LoggedIn as LoggedIn
import UpdateResult as UR



-- MODEL


type Model
    = Authorized (Form.Model Contact.FormInput)
    | Loading
    | Unauthorized


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( Loading, LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn )



-- TYPES


type Msg
    = CompletedLoadCommunity Community.Model
    | GotFormMsg (Form.Msg Contact.FormInput)
    | SubmittedForm (List Contact.Valid)


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoadCommunity community ->
            if community.creator == loggedIn.accountName then
                -- TODO - Use community contacts
                Contact.initFormInput []
                    |> Form.init
                    |> Authorized
                    |> UR.init

            else
                Unauthorized
                    |> UR.init

        GotFormMsg subMsg ->
            case model of
                Authorized formModel ->
                    Form.update loggedIn.shared subMsg formModel
                        |> UR.fromChild (\newFormModel -> Authorized newFormModel)
                            GotFormMsg
                            LoggedIn.addFeedback
                            model

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Got form msg, but wasn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.Contacts", function = "update" }
                            []

        SubmittedForm _ ->
            Debug.todo ""



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
                Authorized formModel ->
                    div [ class "flex-grow flex flex-col" ]
                        [ Page.viewHeader loggedIn title
                        , div [ class "bg-white flex flex-col flex-grow container mx-auto pt-6 pb-7 px-4 lg:px-6" ]
                            [ p [ class "text-gray-900" ]
                                -- TODO - I18N
                                [ text "Adicione os contatos que ficarão disponíveis como suporte para os membros da comunidade." ]

                            -- TODO - I18N
                            , h2 [ class "label mt-10 mb-6" ] [ text "Contact options" ]
                            , Form.view [ class "flex flex-col flex-grow" ]
                                loggedIn.shared.translators
                                (\submitButton ->
                                    [ submitButton [ class "button button-primary mt-auto" ]
                                        -- TODO - I18N
                                        [ text "Save" ]
                                    ]
                                )
                                (Contact.form loggedIn.shared.translators)
                                formModel
                                { toMsg = GotFormMsg
                                , onSubmit = SubmittedForm
                                }
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
        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        SubmittedForm _ ->
            [ "SubmittedForm" ]
