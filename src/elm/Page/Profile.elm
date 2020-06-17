module Page.Profile exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , subscription
    , update
    , view
    )

import Api
import Api.Graphql
import Asset.Icon as Icon
import Avatar exposing (Avatar)
import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Eos as Eos
import Eos.Account as Eos
import File exposing (File)
import Graphql.Http
import Html exposing (Html, br, button, div, h2, h3, h4, img, input, label, p, span, text, textarea)
import Html.Attributes exposing (accept, class, classList, disabled, for, id, maxlength, multiple, placeholder, required, src, style, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit, stopPropagationOn)
import Http
import I18Next exposing (t)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page
import Profile exposing (Profile, ProfileForm, decode)
import PushSubscription exposing (PushSubscription)
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import Task
import UpdateResult as UR
import Utils



-- SUBSCRIPTION


subscription : Model -> Sub Msg
subscription _ =
    Sub.map PressedEnter (Browser.Events.onKeyDown Utils.decodeEnterKeyDown)



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    let
        profileQuery =
            Api.Graphql.query loggedIn.shared
                (Profile.query loggedIn.accountName)
                CompletedProfileLoad
    in
    ( initModel loggedIn
    , Cmd.batch
        [ profileQuery
        , Task.succeed CheckPushPref |> Task.perform identity
        ]
    )



-- MODEL


type alias Model =
    { status : Status
    , privateKeyModal : Maybe String
    , pushNotifications : Bool
    }


initModel : LoggedIn.Model -> Model
initModel _ =
    { status = Loading
    , privateKeyModal = Nothing
    , pushNotifications = False
    }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Profile))
    | Loaded Profile
    | Editing Profile AvatarStatus ProfileForm
    | Saving Profile AvatarStatus ProfileForm


type AvatarStatus
    = NotAsked
    | Sending File Int
    | SendingFailed File Http.Error



-- VIEW


view : LoggedIn.Model -> Model -> Document Msg
view loggedIn model =
    let
        pageTitle =
            case model.status of
                Loaded profile ->
                    Maybe.withDefault "" profile.userName

                _ ->
                    ""

        body =
            case model.status of
                Loading ->
                    Page.fullPageLoading

                LoadingFailed _ ->
                    Page.fullPageError (t loggedIn.shared.translations "profile.title") Http.Timeout

                Loaded profile ->
                    view_ loggedIn profile model

                Editing profile avatarS form ->
                    viewForm loggedIn False profile avatarS form

                Saving profile avatarS form ->
                    viewForm loggedIn True profile avatarS form
    in
    Document pageTitle [ body ]


view_ : LoggedIn.Model -> Profile -> Model -> Html Msg
view_ loggedIn profile model =
    let
        ipfsUrl =
            loggedIn.shared.endpoints.ipfs

        text_ str =
            text (t loggedIn.shared.translations str)

        notification_prompt =
            if model.pushNotifications then
                "Disable Push Notifications"

            else
                "profile.edit.push_notifications"
    in
    div [ class "container mx-auto px-4" ]
        [ Page.viewTitle (t loggedIn.shared.translations "profile.title")
        , div [ class "card profile-card" ]
            [ div [ class "profile-info" ]
                [ div [ class "profile-img" ]
                    [ Avatar.view ipfsUrl profile.avatar "profile-img-avatar" ]
                , div [ class "profile-info-container" ]
                    [ div [ class "profile-info-meta" ]
                        [ h3 [ class "profile-name" ]
                            [ text (Profile.username profile) ]
                        , span [ class "profile-email" ]
                            [ text (Maybe.withDefault "" profile.email) ]
                        , br [] []
                        , br [] []
                        , span [ class "profile-email" ]
                            [ text (Eos.nameToString profile.account) ]
                        ]
                    , p [ class "profile-description" ]
                        [ case profile.bio of
                            Just bio ->
                                text bio

                            Nothing ->
                                span [ class "text-gray" ] [ text_ "profile.edit.placeholders.bio" ]
                        ]
                    ]
                ]
            , div [ class "profile-location-interest" ]
                [ div [ class "profile-location" ]
                    [ h4 [ class "profile-title" ]
                        [ Icon.location ""
                        , span [] [ text_ "profile.edit.labels.localization" ]
                        ]
                    , case profile.localization of
                        Just location ->
                            p [] [ text location ]

                        Nothing ->
                            p [ class "text-gray" ]
                                [ text_ "profile.edit.placeholders.localization" ]
                    ]
                , div [ class "profile-interests" ]
                    [ h4 [ class "profile-title" ]
                        [ Icon.star ""
                        , span [] [ text_ "profile.edit.labels.interests" ]
                        ]
                    , if List.isEmpty profile.interests then
                        p [ class "text-gray" ]
                            [ text_ "profile.edit.placeholders.interests" ]

                      else
                        div [ class "tags" ]
                            (List.map
                                (\i ->
                                    span []
                                        [ text i ]
                                )
                                profile.interests
                            )
                    ]
                ]
            , viewBadges loggedIn
            , div [ class "notification-settings-card", onClick RequestPush ]
                [ button [ class "btn btn--primary" ]
                    [ text_ notification_prompt ]
                ]
            , case LoggedIn.maybePrivateKey loggedIn of
                Nothing ->
                    case loggedIn.shared.maybeAccount of
                        Just ( _, True ) ->
                            button
                                [ classList
                                    [ ( "key-button", True )
                                    , ( "circle-background", True )
                                    , ( "circle-background--primary", True )
                                    ]
                                , onClick ClickedViewPrivateKeyAuth
                                , type_ "button"
                                , title (t loggedIn.shared.translations "profile.actions.viewPrivatekey")
                                ]
                                [ Icon.key "" ]

                        _ ->
                            text ""

                Just privateKey ->
                    div []
                        [ button
                            [ classList
                                [ ( "key-button", True )
                                , ( "circle-background", True )
                                , ( "circle-background--primary", True )
                                ]
                            , onClick (ClickedViewPrivateKey privateKey)
                            , type_ "button"
                            , title (t loggedIn.shared.translations "profile.actions.viewPrivatekey")
                            ]
                            [ Icon.key "" ]
                        , button
                            [ classList
                                [ ( "key-button", True )
                                , ( "circle-background", True )
                                , ( "circle-background--primary", True )
                                ]
                            , onClick (DownloadPdf privateKey)
                            , type_ "button"
                            , title (t loggedIn.shared.translations "profile.actions.viewPrivatekey")
                            ]
                            [ Icon.key "" ]
                        ]
            , button
                [ classList
                    [ ( "edit-button", True )
                    , ( "circle-background", True )
                    , ( "circle-background--primary", True )
                    ]
                , onClick ClickedEdit
                , type_ "button"
                , title (t loggedIn.shared.translations "profile.actions.edit")
                ]
                [ Icon.edit "" ]
            ]
        , case model.privateKeyModal of
            Nothing ->
                text ""

            Just pk ->
                div
                    [ onClick ClickedClosePrivateKey
                    ]
                    [ div
                        [ class "card card--modal"
                        , stopPropagationOn "click"
                            (Decode.succeed ( Ignored, True ))
                        , style "position" "relative"
                        , style "align-items" "stretch"
                        ]
                        [ h2 [ class "card__title" ]
                            [ text_ "profile.edit.labels.privateKey" ]
                        , br [] []
                        , p [] [ text_ "profile.edit.placeholders.privateKey" ]
                        , br [] []
                        , p [] [ text pk ]
                        , button
                            [ class "card__close-btn"
                            , onClick ClickedClosePrivateKey
                            , type_ "button"
                            ]
                            [ Icon.close "" ]
                        ]
                    ]
        ]


viewForm : LoggedIn.Model -> Bool -> Profile -> AvatarStatus -> ProfileForm -> Html Msg
viewForm loggedIn isDisabled profile avatarS form =
    let
        ipfsUrl =
            loggedIn.shared.endpoints.ipfs

        text_ s =
            text (t loggedIn.shared.translations s)

        textr_ s m =
            text (I18Next.tr loggedIn.shared.translations I18Next.Curly s m)

        plcH s =
            placeholder (t loggedIn.shared.translations s)
    in
    Html.form
        [ onSubmit ClickedSave
        ]
        [ Page.viewTitle (t loggedIn.shared.translations "profile.title")
        , div
            [ class "card profile-card" ]
            [ div [ class "profile-info" ]
                [ viewAvatar ipfsUrl profile (t loggedIn.shared.translations "profile.edit.labels.avatar") avatarS
                , div [ class "profile-info-container" ]
                    [ div [ class "profile-info-meta" ]
                        [ h3 [ class "profile-name" ]
                            [ formField
                                [ input
                                    [ type_ "text"
                                    , class "input profile-input-name"
                                    , onInput EnteredName
                                    , value form.name
                                    , disabled isDisabled
                                    , plcH "profile.edit.placeholders.name"
                                    , maxlength 255
                                    ]
                                    []
                                , viewFieldError "name" form.errors
                                ]
                            ]
                        , span [ class "profile-email" ]
                            [ formField
                                [ input
                                    [ type_ "email"
                                    , class "input"
                                    , onInput EnteredEmail
                                    , value form.email
                                    , disabled isDisabled
                                    , plcH "profile.edit.placeholders.email"
                                    ]
                                    []
                                , viewFieldError "email" form.errors
                                ]
                            ]
                        ]
                    , p [ class "profile-description" ]
                        [ formField
                            [ textarea
                                [ class "input"
                                , onInput EnteredBio
                                , value form.bio
                                , disabled isDisabled
                                , plcH "profile.edit.placeholders.bio"
                                , maxlength 255
                                ]
                                []
                            , viewFieldError "bio" form.errors
                            ]
                        , div [ class "input-counter" ]
                            [ textr_ "edit.input_counter"
                                [ ( "current"
                                  , String.fromInt (String.length form.bio)
                                  )
                                , ( "max", "255" )
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "profile-location-interest" ]
                [ div [ class "profile-location" ]
                    [ h4 [ class "profile-title" ]
                        [ Icon.location ""
                        , span [] [ text_ "profile.edit.labels.localization" ]
                        ]
                    , formField
                        [ input
                            [ type_ "text"
                            , class "input"
                            , onInput EnteredLocalization
                            , value form.localization
                            , disabled isDisabled
                            , plcH "profile.edit.placeholders.localization"
                            , maxlength 255
                            ]
                            []
                        , viewFieldError "localization" form.errors
                        ]
                    ]
                , div [ class "profile-interests" ]
                    [ h4 [ class "profile-title" ]
                        [ Icon.star ""
                        , span [] [ text_ "profile.edit.labels.interests" ]
                        ]
                    , formField
                        [ Html.form
                            [ onSubmit ClickedAddInterest
                            , class "input-group"
                            ]
                            [ input
                                [ type_ "text"
                                , class "input flex100"
                                , onInput EnteredInterest
                                , value form.interest
                                , disabled isDisabled
                                , plcH "profile.edit.placeholders.interests"
                                , required True
                                , maxlength 255
                                ]
                                []
                            , button
                                [ class "btn btn--outline flex000"
                                , disabled isDisabled
                                ]
                                [ text_ "menu.add" ]
                            ]
                        , viewFieldError "interests" form.errors
                        ]
                    , div [ class "form-tags" ]
                        (List.indexedMap
                            (\index i ->
                                div []
                                    [ span [] [ text i ]
                                    , button
                                        [ type_ "button"
                                        , onClick (ClickedRemoveInterest index)
                                        ]
                                        [ Icon.close "" ]
                                    ]
                            )
                            form.interests
                        )
                    ]
                ]
            ]
        , div [ class "btn-row" ]
            [ button
                [ classList
                    [ ( "btn", True )
                    , ( "btn--primary", True )
                    , ( "btn--outline", True )
                    ]
                , disabled isDisabled
                , type_ "button"
                , onClick ClickedEditCancel
                ]
                [ text_ "menu.cancel" ]
            , button
                [ classList
                    [ ( "btn", True )
                    , ( "btn--primary", True )
                    ]
                , disabled isDisabled
                ]
                [ text_ "profile.edit.submit" ]
            ]
        ]


viewAvatar : String -> Profile -> String -> AvatarStatus -> Html Msg
viewAvatar ipfsUrl profile plchldr avatarS =
    let
        isUploading =
            case avatarS of
                NotAsked ->
                    False

                Sending _ _ ->
                    True

                SendingFailed _ _ ->
                    False
    in
    div []
        [ input
            [ id "profile-upload-avatar"
            , class "profile-img-input"
            , type_ "file"
            , accept "image/*"
            , Page.onFileChange EnteredAvatar
            , multiple False
            , disabled isUploading
            ]
            []
        , label
            [ class "profile-img"
            , for "profile-upload-avatar"
            , title plchldr
            ]
            [ Avatar.view ipfsUrl profile.avatar "profile-img-avatar"
            , if isUploading then
                div [ class "profile-img-loading" ]
                    [ div [ class "spinner" ] [] ]

              else
                div [ class "profile-img-hover" ] [ Icon.upload "" ]
            ]
        ]


viewBadges : LoggedIn.Model -> Html msg
viewBadges loggedIn =
    let
        text_ s =
            text (t loggedIn.shared.translations s)
    in
    div [ class "profile-badges" ]
        [ h4 [ class "profile-title" ]
            [ Icon.brightness7 ""
            , span [] [ text_ "profile.badge.title" ]
            ]
        , div [ class "profile-badges-list" ]
            [ div [ class "profile-badges-item profile-badges-item--inactive" ]
                [ img [ src "images/badges-currency-starter.svg" ]
                    []
                , span [] [ text_ "profile.badge.currencyStarter" ]
                ]
            , div [ class "profile-badges-item" ]
                [ img [ src "images/badge-developer.svg" ]
                    []
                , span [] [ text_ "profile.badge.developer" ]
                ]
            , div [ class "profile-badges-item" ]
                [ img [ src "images/badge-seed-team.svg" ]
                    []
                , span [] [ text_ "profile.badge.seedTeam" ]
                ]
            , div [ class "profile-badges-item" ]
                [ img [ src "images/badges-mentor.svg" ]
                    []
                , span [] [ text_ "profile.badge.mentor" ]
                ]
            ]
        ]



-- HELPERS


formField : List (Html msg) -> Html msg
formField =
    div [ class "form-field" ]


viewFieldError : String -> Dict String String -> Html msg
viewFieldError fieldId errors =
    case Dict.get fieldId errors of
        Just e ->
            span [ class "field-error" ] [ text e ]

        Nothing ->
            text ""



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = Ignored
    | CompletedProfileLoad (Result (Graphql.Http.Error (Maybe Profile)) (Maybe Profile))
    | ClickedEdit
    | ClickedEditCancel
    | ClickedSave
    | EnteredName String
    | EnteredEmail String
    | EnteredBio String
    | EnteredLocalization String
    | EnteredInterest String
    | ClickedAddInterest
    | ClickedRemoveInterest Int
    | EnteredAvatar (List File)
    | CompletedAvatarUpload (Result Http.Error Avatar)
    | ClickedViewPrivateKeyAuth
    | ClickedViewPrivateKey String
    | ClickedClosePrivateKey
    | RequestPush
    | GotPushSub PushSubscription
    | CompletedPushUpload (Result (Graphql.Http.Error ()) ())
    | GotPushPreference Bool
    | CheckPushPref
    | DownloadPdf String
    | PressedEnter Bool


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        t =
            I18Next.t loggedIn.shared.translations
    in
    case msg of
        Ignored ->
            UR.init model

        CompletedProfileLoad (Ok Nothing) ->
            -- TODO: not found account
            UR.init model

        CompletedProfileLoad (Ok (Just profile)) ->
            UR.init { model | status = Loaded profile }

        CompletedProfileLoad (Err err) ->
            UR.init { model | status = LoadingFailed err }
                |> UR.logGraphqlError msg err

        ClickedEdit ->
            case model.status of
                Loaded profile ->
                    { model | status = Editing profile NotAsked (Profile.profileToForm profile) }
                        |> UR.init

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        ClickedEditCancel ->
            case model.status of
                Editing profile _ _ ->
                    UR.init { model | status = Loaded profile }

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        ClickedSave ->
            case model.status of
                Editing profile avatarS form ->
                    { model | status = Saving profile avatarS form }
                        |> UR.init
                        |> UR.addCmd
                            (Api.Graphql.mutation loggedIn.shared
                                (Profile.mutation profile.account form)
                                CompletedProfileLoad
                            )
                        |> UR.addExt (ShowFeedback Success (t "profile.edit_success"))

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        EnteredName s ->
            UR.init model
                |> updateForm (\form -> { form | name = s })

        EnteredEmail s ->
            UR.init model
                |> updateForm (\form -> { form | email = s })

        EnteredBio s ->
            UR.init model
                |> updateForm (\form -> { form | bio = s })

        EnteredLocalization s ->
            UR.init model
                |> updateForm (\form -> { form | localization = s })

        EnteredInterest s ->
            UR.init model
                |> updateForm (\form -> { form | interest = s })

        ClickedAddInterest ->
            UR.init model
                |> updateForm
                    (\form ->
                        { form
                            | interest = ""
                            , interests =
                                form.interests
                                    ++ (String.split "," form.interest
                                            |> List.map String.trim
                                       )
                        }
                    )

        ClickedRemoveInterest index ->
            UR.init model
                |> updateForm
                    (\form ->
                        { form
                            | interests =
                                List.indexedMap
                                    (\i intr ->
                                        if i == index then
                                            Nothing

                                        else
                                            Just intr
                                    )
                                    form.interests
                                    |> List.filterMap identity
                        }
                    )

        EnteredAvatar (file :: _) ->
            let
                uploadAvatar file_ =
                    Api.uploadAvatar loggedIn.shared file_ CompletedAvatarUpload

                updateAvatar avatarS file_ =
                    case avatarS of
                        NotAsked ->
                            ( Sending file_ 0
                            , UR.addCmd (uploadAvatar file_)
                            )

                        SendingFailed _ _ ->
                            ( Sending file_ 0
                            , UR.addCmd (uploadAvatar file_)
                            )

                        Sending file__ progress ->
                            ( Sending file__ progress
                            , UR.logImpossible msg []
                            )

                toUResult model_ toStatus ( avatarS, addOrlog ) =
                    UR.init { model_ | status = toStatus avatarS }
                        |> addOrlog
            in
            case model.status of
                Editing profile avatarS form ->
                    updateAvatar avatarS file
                        |> toUResult model
                            (\a -> Editing profile a form)

                Saving profile avatarS form ->
                    updateAvatar avatarS file
                        |> toUResult model
                            (\a -> Saving profile a form)

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        EnteredAvatar [] ->
            UR.init model

        CompletedAvatarUpload (Ok a) ->
            UR.init model
                |> updateForm (\f -> { f | avatar = Avatar.toMaybeString a })
                |> updateProfile (\profile -> { profile | avatar = a })
                |> updateAvatarStatus (\_ -> NotAsked)

        CompletedAvatarUpload (Err err) ->
            UR.init model
                |> UR.logHttpError msg err
                |> updateAvatarStatus
                    (\avatarStatus ->
                        case avatarStatus of
                            NotAsked ->
                                NotAsked

                            SendingFailed file _ ->
                                SendingFailed file err

                            Sending file _ ->
                                SendingFailed file err
                    )

        ClickedViewPrivateKeyAuth ->
            case LoggedIn.maybePrivateKey loggedIn of
                Nothing ->
                    UR.init model
                        |> UR.addExt
                            (Just ClickedViewPrivateKeyAuth
                                |> RequiredAuthentication
                            )

                Just privateKey ->
                    UR.init { model | privateKeyModal = Just privateKey }

        ClickedViewPrivateKey privateKey ->
            UR.init { model | privateKeyModal = Just privateKey }

        ClickedClosePrivateKey ->
            UR.init { model | privateKeyModal = Nothing }

        RequestPush ->
            if model.pushNotifications then
                model
                    |> UR.init
                    |> UR.addPort
                        { responseAddress = GotPushPreference False
                        , responseData = Encode.null
                        , data =
                            Encode.object [ ( "name", Encode.string "disablePushPref" ) ]
                        }

            else
                model
                    |> UR.init
                    |> UR.addPort
                        { responseAddress = RequestPush
                        , responseData = Encode.null
                        , data =
                            Encode.object
                                [ ( "name", Encode.string "requestPushPermission" ) ]
                        }

        GotPushSub push ->
            model
                |> UR.init
                |> UR.addCmd
                    (uploadPushSubscription loggedIn push)

        CompletedPushUpload res ->
            case res of
                Ok _ ->
                    model
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = CompletedPushUpload res
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "completedPushUpload" ) ]
                            }

                Err err ->
                    model
                        |> UR.init
                        |> UR.logGraphqlError msg err

        GotPushPreference val ->
            { model | pushNotifications = val }
                |> UR.init

        CheckPushPref ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = GotPushPreference False
                    , responseData = Encode.null
                    , data =
                        Encode.object [ ( "name", Encode.string "checkPushPref" ) ]
                    }

        DownloadPdf passPhrase ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = Ignored
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "printAuthPdf" )
                            , ( "passphrase", Encode.string passPhrase )
                            ]
                    }

        PressedEnter val ->
            if val then
                UR.init model
                    |> UR.addCmd
                        (Task.succeed ClickedSave
                            |> Task.perform identity
                        )

            else
                UR.init model


updateForm : (ProfileForm -> ProfileForm) -> UpdateResult -> UpdateResult
updateForm transform ({ model } as uResult) =
    case model.status of
        Loading ->
            uResult

        LoadingFailed _ ->
            uResult

        Loaded _ ->
            uResult

        Editing profile avatarS form ->
            { model | status = Editing profile avatarS (transform form) }
                |> UR.setModel uResult

        Saving profile avatarS form ->
            { model | status = Saving profile avatarS (transform form) }
                |> UR.setModel uResult


updateProfile : (Profile -> Profile) -> UpdateResult -> UpdateResult
updateProfile transform ({ model } as uResult) =
    case model.status of
        Loading ->
            uResult

        LoadingFailed _ ->
            uResult

        Loaded profile ->
            { model | status = Loaded (transform profile) }
                |> UR.setModel uResult

        Editing profile avatarS form ->
            { model | status = Editing (transform profile) avatarS form }
                |> UR.setModel uResult

        Saving profile avatarS form ->
            { model | status = Saving (transform profile) avatarS form }
                |> UR.setModel uResult


updateAvatarStatus : (AvatarStatus -> AvatarStatus) -> UpdateResult -> UpdateResult
updateAvatarStatus transform ({ model } as uResult) =
    case model.status of
        Loading ->
            uResult

        LoadingFailed _ ->
            uResult

        Loaded _ ->
            uResult

        Editing profile avatarS form ->
            { model | status = Editing profile (transform avatarS) form }
                |> UR.setModel uResult

        Saving profile avatarS form ->
            { model | status = Saving profile (transform avatarS) form }
                |> UR.setModel uResult


uploadPushSubscription : LoggedIn.Model -> PushSubscription -> Cmd Msg
uploadPushSubscription { accountName, shared } data =
    Api.Graphql.mutation shared
        (PushSubscription.activatePushMutation accountName data)
        CompletedPushUpload


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "RequestPush" :: _ ->
            let
                push =
                    Decode.decodeValue (Decode.field "sub" Decode.string) val
                        |> Result.andThen (Decode.decodeString Decode.value)
                        |> Result.andThen (Decode.decodeValue PushSubscription.decode)
            in
            case push of
                Ok res ->
                    Just (GotPushSub res)

                Err _ ->
                    -- TODO: Handle PushSubscription Decode error
                    Nothing

        "CompletedPushUpload" :: _ ->
            decodePushPref val

        "GotPushPreference" :: _ ->
            decodePushPref val

        _ ->
            Nothing


decodePushPref : Value -> Maybe Msg
decodePushPref val =
    Decode.decodeValue (Decode.field "isSet" Decode.bool) val
        |> Result.map GotPushPreference
        |> Result.toMaybe


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        CompletedProfileLoad r ->
            [ "CompletedProfileLoad", UR.resultToString r ]

        ClickedEdit ->
            [ "ClickedEdit" ]

        ClickedEditCancel ->
            [ "ClickedEditCancel" ]

        ClickedSave ->
            [ "ClickedSave" ]

        EnteredName _ ->
            [ "EnteredName" ]

        EnteredEmail _ ->
            [ "EnteredEmail" ]

        EnteredBio _ ->
            [ "EnteredBio" ]

        EnteredLocalization _ ->
            [ "EnteredLocalization" ]

        EnteredInterest _ ->
            [ "EnteredInterest" ]

        ClickedAddInterest ->
            [ "ClickedAddInterest" ]

        ClickedRemoveInterest _ ->
            [ "ClickedRemoveInterest" ]

        EnteredAvatar _ ->
            [ "EnteredAvatar" ]

        CompletedAvatarUpload r ->
            [ "CompletedAvatarUpload", UR.resultToString r ]

        ClickedViewPrivateKeyAuth ->
            [ "ClickedViewPrivateKeyAuth" ]

        ClickedViewPrivateKey _ ->
            [ "ClickedViewPrivateKey" ]

        ClickedClosePrivateKey ->
            [ "ClickedClosePrivateKey" ]

        RequestPush ->
            [ "RequestPush" ]

        GotPushSub _ ->
            [ "GotPushSub" ]

        CompletedPushUpload _ ->
            [ "CompletedPushUpload" ]

        GotPushPreference _ ->
            [ "GotPushPreference" ]

        CheckPushPref ->
            [ "CheckPushPref" ]

        DownloadPdf _ ->
            [ "DownloadPdf" ]

        PressedEnter _ ->
            [ "PressedEnter" ]
