module Page.Profile.Editor exposing
    ( Model
    , Msg
    , init
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Avatar
import Dict
import Form
import Form.File
import Form.RichText
import Form.Text
import Graphql.Http
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Icons
import Log
import Markdown exposing (Markdown)
import Page
import Profile
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared exposing (Translators)
import UpdateResult as UR
import View.Feedback as Feedback



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( initModel
    , LoggedIn.maybeInitWith CompletedLoadProfile .profile loggedIn
    )



-- MODEL


type alias Model =
    { form : FormStatus }


type FormStatus
    = Loading
    | Loaded (Form.Model FormInput)


type alias FormInput =
    { avatar : Form.File.SingleModel
    , fullName : String
    , email : String
    , bio : Form.RichText.Model
    , location : String
    , interest : String
    , interests : List String
    }


type alias FormOutput =
    { avatar : Maybe String
    , fullName : String
    , email : String
    , bio : Markdown
    , location : String
    , interests : List String
    }


createForm : Translators -> { hasKyc : Bool } -> Form.Form msg FormInput FormOutput
createForm ({ t } as translators) { hasKyc } =
    Form.succeed FormOutput
        |> Form.with
            (Form.File.init { id = "avatar-input" }
                |> Form.File.withContainerAttributes [ class "grid place-items-center" ]
                |> Form.File.withImageClass "object-cover rounded-full mx-auto w-20 h-20"
                |> Form.File.withEntryContainerAttributes (\_ -> [ class "mx-auto rounded-full w-20 h-20" ])
                |> Form.File.withEditIconOverlay
                |> Form.File.withAddImagesView
                    [ span [ class "bg-orange-300 rounded-full p-4 w-20 h-20" ]
                        [ Icons.camera "text-white"
                        ]
                    ]
                |> Form.File.withImageCropperAttributes [ class "rounded-full" ]
                |> Form.file
                    { parser = Ok
                    , translators = translators
                    , value = .avatar
                    , update = \avatar input -> { input | avatar = avatar }
                    , externalError = always Nothing
                    }
                |> Form.optional
            )
        |> Form.with
            (Form.Text.init { label = t "profile.edit.labels.name", id = "name-input" }
                |> Form.Text.withDisabled hasKyc
                |> Form.textField
                    { parser = Ok
                    , value = .fullName
                    , update = \name input -> { input | fullName = name }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = t "profile.edit.labels.email", id = "email-input" }
                |> Form.textField
                    { parser = Ok
                    , value = .email
                    , update = \email input -> { input | email = email }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.RichText.init { label = t "profile.edit.labels.bio" }
                |> Form.RichText.withEditorContainerAttrs [ class "mb-10" ]
                |> Form.richText
                    { parser = Ok
                    , value = .bio
                    , update = \bio input -> { input | bio = bio }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = t "profile.edit.labels.localization", id = "location-input" }
                |> Form.textField
                    { parser = Ok
                    , value = .location
                    , update = \location input -> { input | location = location }
                    , externalError = always Nothing
                    }
            )
        |> Form.with (interestsForm translators)


interestsForm : Translators -> Form.Form msg FormInput (List String)
interestsForm { t } =
    let
        viewInterest interest =
            div [ class "bg-green px-3 h-8 rounded-sm text-sm flex text-white" ]
                [ span [ class "m-auto mr-3 uppercase" ] [ text interest ]
                , button
                    [ type_ "button"
                    , onClick
                        (\values ->
                            { values | interests = List.filter (\x -> x /= interest) values.interests }
                        )
                    ]
                    [ Icons.close "w-4 h-4 fill-current" ]
                ]
    in
    Form.introspect (\values -> Form.succeed values.interests)
        |> Form.withNoOutput
            (Form.succeed always
                |> Form.withGroup
                    [ class "flex" ]
                    (Form.Text.init { label = t "profile.edit.labels.interests", id = "interest-input" }
                        |> Form.Text.withContainerAttrs [ class "w-full !mb-4" ]
                        |> Form.textField
                            { parser = Ok
                            , value = .interest
                            , update = \interest input -> { input | interest = interest }
                            , externalError = always Nothing
                            }
                    )
                    (Form.arbitrary
                        (button
                            [ type_ "button"
                            , class "button button-secondary px-4 h-12 ml-4 mb-4 mt-auto max-w-min"
                            , onClick
                                (\values ->
                                    if String.isEmpty values.interest || List.member values.interest values.interests then
                                        values

                                    else
                                        { values
                                            | interest = ""
                                            , interests = values.interest :: values.interests
                                        }
                                )
                            ]
                            [ text <| t "menu.add"
                            ]
                        )
                    )
            )
        |> Form.withNoOutput
            (Form.introspect
                (\values ->
                    Form.arbitrary
                        (div [ class "flex flex-wrap mb-10 gap-x-4 gap-y-2" ]
                            (List.map viewInterest values.interests)
                        )
                )
            )


initModel : Model
initModel =
    { form = Loading }



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        title =
            t "profile.edit.title"

        content =
            case loggedIn.profile of
                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure e ->
                    Page.fullPageGraphQLError (t "profile.title") e

                RemoteData.Success profile ->
                    case model.form of
                        Loading ->
                            Page.fullPageLoading loggedIn.shared

                        Loaded form ->
                            view_ loggedIn form profile
    in
    { title = title
    , content = content
    }


view_ : LoggedIn.Model -> Form.Model FormInput -> Profile.Model -> Html Msg
view_ loggedIn form profile =
    let
        { t } =
            loggedIn.shared.translators

        translators =
            loggedIn.shared.translators

        title =
            t "menu.edit" ++ " " ++ ("menu.profile" |> t |> String.toLower)
    in
    div [ class "bg-white" ]
        [ Page.viewHeader loggedIn title
        , Form.view [ class "container mx-auto p-4" ]
            translators
            (\submitButton ->
                [ submitButton [ class "button button-primary w-full" ]
                    [ text <| t "profile.edit.submit" ]
                ]
            )
            (createForm translators { hasKyc = List.any .hasKyc profile.communities })
            form
            { toMsg = GotFormMsg
            , onSubmit = ClickedSave
            }
        ]



-- UPDATE


type Msg
    = CompletedLoadProfile Profile.Model
    | ClickedSave FormOutput
    | GotSaveResult (RemoteData (Graphql.Http.Error (Maybe Profile.Model)) (Maybe Profile.Model))
    | GotFormMsg (Form.Msg FormInput)


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        { t } =
            loggedIn.shared.translators
    in
    case msg of
        CompletedLoadProfile profile ->
            let
                nullable a =
                    Maybe.withDefault "" a
            in
            { model
                | form =
                    { avatar =
                        Form.File.initSingle
                            { fileUrl = Avatar.toMaybeString profile.avatar
                            , aspectRatio = Just 1
                            }
                    , fullName = nullable profile.name
                    , email = nullable profile.email
                    , bio = Form.RichText.initModel "bio-input" profile.bio
                    , location = nullable profile.localization
                    , interests = profile.interests
                    , interest = ""
                    }
                        |> Form.init
                        |> Loaded
            }
                |> UR.init

        ClickedSave formOutput ->
            case loggedIn.profile of
                RemoteData.Success profile ->
                    let
                        newProfile =
                            { profile
                                | name = Just formOutput.fullName
                                , email = Just formOutput.email
                                , localization = Just formOutput.location
                                , interests = formOutput.interests
                                , bio = Just formOutput.bio
                                , avatar =
                                    formOutput.avatar
                                        |> Maybe.map Avatar.fromString
                                        |> Maybe.withDefault profile.avatar
                            }
                    in
                    model
                        |> UR.init
                        |> UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | profile = RemoteData.Loading })
                        |> UR.addExt
                            (LoggedIn.mutation loggedIn
                                (Profile.mutation (Profile.profileToForm newProfile))
                                GotSaveResult
                            )

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Tried saving profile, but current profile wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Profile.Editor", function = "update" }
                            []

        GotSaveResult (RemoteData.Success (Just profile)) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ProfileLoaded profile |> LoggedIn.ExternalBroadcast)
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success (t "profile.edit_success"))
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey (Route.Profile loggedIn.accountName))
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = msg
                    , message = "Successfully saved profile"
                    , data = Dict.empty
                    , level = Log.DebugLevel
                    }

        GotSaveResult (RemoteData.Success Nothing) ->
            model
                |> UR.init

        GotSaveResult (RemoteData.Failure e) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | profile = RemoteData.Failure e })
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to save profile"
                    { moduleName = "Page.Profile.Editor", function = "update" }
                    []
                    e

        GotSaveResult _ ->
            UR.init model

        GotFormMsg subMsg ->
            case model.form of
                Loading ->
                    UR.init model

                Loaded form ->
                    Form.update loggedIn.shared subMsg form
                        |> UR.fromChild (\newForm -> { model | form = Loaded newForm })
                            GotFormMsg
                            LoggedIn.addFeedback
                            model


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.ProfileLoaded profile ->
            Just (CompletedLoadProfile profile)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoadProfile _ ->
            [ "CompletedLoadProfile" ]

        ClickedSave _ ->
            [ "ClickedSave" ]

        GotSaveResult r ->
            [ "GotSaveResult", UR.remoteDataToString r ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg
