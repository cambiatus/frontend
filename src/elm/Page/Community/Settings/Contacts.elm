module Page.Community.Settings.Contacts exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Cambiatus.Mutation
import Cambiatus.Object.Community
import Community
import Contact
import Form
import Graphql.Http
import Graphql.OptionalArgument
import Graphql.SelectionSet
import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (class)
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import UpdateResult as UR
import View.Feedback



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
    | CompletedSubmittingForm (RemoteData (Graphql.Http.Error (Maybe (List Contact.Valid))) (Maybe (List Contact.Valid)))


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoadCommunity community ->
            if community.creator == loggedIn.accountName then
                Contact.initFormInput community.contacts
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

        SubmittedForm formOutput ->
            case model of
                Authorized formModel ->
                    Authorized (Form.withDisabled True formModel)
                        |> UR.init
                        |> UR.addExt
                            (LoggedIn.mutation loggedIn
                                (Cambiatus.Mutation.community
                                    { input =
                                        { contacts =
                                            formOutput
                                                |> List.map Contact.toGraphqlInput
                                                |> Graphql.OptionalArgument.Present
                                        , hasNews = Graphql.OptionalArgument.Absent
                                        }
                                    }
                                    (Cambiatus.Object.Community.contacts Contact.selectionSet
                                        |> Graphql.SelectionSet.map (List.filterMap identity)
                                    )
                                )
                                CompletedSubmittingForm
                            )

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Submitted form, but wasn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.Contacts", function = "update" }
                            []

        CompletedSubmittingForm (RemoteData.Success maybeContacts) ->
            UR.init model
                |> UR.addExt
                    (LoggedIn.UpdatedLoggedIn
                        { loggedIn
                            | selectedCommunity =
                                RemoteData.map
                                    (\community -> { community | contacts = Maybe.withDefault [] maybeContacts })
                                    loggedIn.selectedCommunity
                        }
                    )
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.CommunityAbout)
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Success (loggedIn.shared.translators.t "contact_form.feedback.community.success"))

        CompletedSubmittingForm (RemoteData.Failure err) ->
            let
                newModel =
                    case model of
                        Authorized formModel ->
                            Authorized (Form.withDisabled False formModel)

                        _ ->
                            model
            in
            UR.init newModel
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when saving community contacts"
                    { moduleName = "Page.Community.Settings.Contacts", function = "update" }
                    []
                    err
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (loggedIn.shared.translators.t "contact_form.feedback.community.failure"))

        CompletedSubmittingForm _ ->
            let
                newModel =
                    case model of
                        Authorized formModel ->
                            Authorized (Form.withDisabled False formModel)

                        _ ->
                            model
            in
            UR.init newModel



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
                        , div [ class "container mx-auto lg:px-4 lg:mt-6 lg:mb-20" ]
                            [ div [ class "bg-white container mx-auto pt-6 pb-7 px-4 lg:px-6 lg:max-w-none lg:w-2/3 lg:rounded lg:shadow-lg" ]
                                [ p [ class "text-gray-900" ]
                                    [ text <| t "contact_form.community_header" ]
                                , h2 [ class "label mt-10" ] [ text <| t "contact_form.options" ]
                                , Form.view [ class "flex flex-col flex-grow" ]
                                    loggedIn.shared.translators
                                    (\submitButton ->
                                        [ submitButton [ class "button button-primary mt-auto w-full md:w-56" ]
                                            [ text <| t "contact_form.submit_multiple" ]
                                        ]
                                    )
                                    (Contact.form loggedIn.shared.translators)
                                    formModel
                                    { toMsg = GotFormMsg
                                    , onSubmit = SubmittedForm
                                    }
                                ]
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

        CompletedSubmittingForm r ->
            [ "CompletedSubmittingForm", UR.remoteDataToString r ]
