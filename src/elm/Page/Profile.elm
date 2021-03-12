module Page.Profile exposing
    ( Model
    , Msg
    , ProfilePage(..)
    , init
    , jsAddressToMsg
    , msgToString
    , update
    , view
    , viewSettings
    , viewUserInfo
    )

import Api.Graphql
import Avatar
import Browser.Dom as Dom
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, a, br, button, div, label, li, p, span, text, ul)
import Html.Attributes exposing (class, classList, href, target)
import Html.Events exposing (onClick)
import Http
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page
import Profile exposing (DeleteKycAndAddressResult, Model)
import Profile.Contact as Contact
import PushSubscription exposing (PushSubscription)
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..), ProfileStatus(..))
import Session.Shared exposing (Shared, Translators)
import Task
import UpdateResult as UR
import View.Modal as Modal
import View.Pin as Pin



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    let
        profileQuery =
            Api.Graphql.query loggedIn.shared
                (Just loggedIn.authToken)
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
    { status : ProfileStatus
    , currentPin : Maybe String
    , isNewPinModalVisible : Bool
    , newPin : Maybe String
    , isNewPinReadable : Bool
    , newPinErrorMsg : Maybe String
    , isPushNotificationsEnabled : Bool
    , maybePdfDownloadedSuccessfully : Maybe Bool
    , isDeleteKycModalShowed : Bool
    }


initModel : LoggedIn.Model -> Model
initModel loggedIn =
    { status = Loading loggedIn.accountName
    , currentPin = Nothing
    , isNewPinModalVisible = False
    , newPin = Nothing
    , isNewPinReadable = True
    , newPinErrorMsg = Nothing
    , isPushNotificationsEnabled = False
    , maybePdfDownloadedSuccessfully = Nothing
    , isDeleteKycModalShowed = False
    }



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        title =
            case model.status of
                Loaded profile ->
                    Maybe.withDefault "" profile.name

                _ ->
                    ""

        content =
            case model.status of
                Loading _ ->
                    Page.fullPageLoading shared

                LoadingFailed _ _ ->
                    Page.fullPageError (shared.translators.t "profile.title") Http.Timeout

                Loaded profile ->
                    div []
                        [ Page.viewHeader loggedIn (shared.translators.t "menu.profile") Route.Dashboard
                        , viewUserInfo loggedIn
                            profile
                            Private
                        , viewSettings loggedIn model profile
                        , viewNewPinModal model shared
                        , viewDownloadPdfErrorModal model loggedIn
                        , viewDeleteKycModal shared.translators model
                        ]
    in
    { title = title
    , content = content
    }


viewDeleteKycModal : Translators -> Model -> Html Msg
viewDeleteKycModal { t } model =
    Modal.initWith
        { closeMsg = ToggleDeleteKycModal
        , isVisible = model.isDeleteKycModalShowed
        }
        |> Modal.withHeader (t "community.kyc.delete.confirmationHeader")
        |> Modal.withBody
            [ div []
                [ text (t "community.kyc.delete.confirmationBody")
                ]
            ]
        |> Modal.withFooter
            [ button
                [ class "modal-cancel"
                , onClick ToggleDeleteKycModal
                ]
                [ text (t "community.kyc.delete.cancel") ]
            , button
                [ class "modal-accept"
                , onClick DeleteKycAccepted
                ]
                [ text (t "community.kyc.delete.confirm") ]
            ]
        |> Modal.toHtml


viewSettings : LoggedIn.Model -> Model -> Profile.Model -> Html Msg
viewSettings loggedIn model profile =
    let
        { t } =
            loggedIn.shared.translators

        downloadAction =
            case LoggedIn.maybePrivateKey loggedIn of
                Just _ ->
                    let
                        currentPin =
                            case model.currentPin of
                                -- The case when "Download" PDF button pressed just after changing the PIN
                                Just pin ->
                                    pin

                                Nothing ->
                                    loggedIn.auth.form.enteredPin
                    in
                    DownloadPdf currentPin

                Nothing ->
                    case loggedIn.shared.maybeAccount of
                        Just ( _, True ) ->
                            ClickedViewPrivateKeyAuth

                        _ ->
                            Ignored

        viewKycSettings =
            let
                kycLabel =
                    span [ class "flex items-center" ]
                        [ text (t "community.kyc.dataTitle")
                        , span [ class "icon-tooltip ml-1" ]
                            [ Icons.question "inline-block"
                            , p
                                [ class "icon-tooltip-content" ]
                                [ text (t "community.kyc.info")
                                ]
                            ]
                        ]
            in
            case profile.kyc of
                Just _ ->
                    viewProfileItem
                        kycLabel
                        (viewDangerButton (t "community.kyc.delete.label") ToggleDeleteKycModal)
                        Center
                        (Just
                            (div [ class "uppercase text-red pt-2 text-xs" ]
                                [ text (t "community.kyc.delete.warning")
                                ]
                            )
                        )

                Nothing ->
                    viewProfileItem
                        kycLabel
                        (viewButton (t "menu.add") AddKycClicked)
                        Center
                        Nothing
    in
    div [ class "bg-white mb-6" ]
        [ ul [ class "container divide-y divide-gray-500 mx-auto px-4" ]
            [ viewProfileItem
                (text (t "profile.12words.title"))
                (viewButton (t "profile.12words.button") downloadAction)
                Center
                Nothing
            , viewProfileItem
                (text (t "profile.pin.title"))
                (viewButton (t "profile.pin.button") ClickedChangePin)
                Center
                Nothing

            -- , viewProfileItem (text (t "notifications.title")) (viewTogglePush loggedIn model) Center Nothing
            , viewKycSettings
            ]
        ]


type ProfilePage
    = Private
    | Public


viewUserInfo : LoggedIn.Model -> Profile.Model -> ProfilePage -> Html msg
viewUserInfo loggedIn profile pageType =
    let
        ({ t } as translators) =
            loggedIn.shared.translators

        userName =
            Maybe.withDefault "" profile.name

        email =
            Maybe.withDefault "" profile.email

        bio =
            Maybe.withDefault "" profile.bio

        location =
            Maybe.withDefault "" profile.localization

        account =
            Eos.nameToString profile.account

        isPublic =
            case pageType of
                Public ->
                    True

                Private ->
                    False

        viewAddress =
            case profile.address of
                Just addr ->
                    let
                        fullAddress =
                            span []
                                [ text <|
                                    addr.country
                                        ++ ", "
                                        ++ addr.state
                                        ++ ", "
                                        ++ addr.city
                                        ++ ", "
                                        ++ addr.neighborhood
                                        ++ ", "
                                        ++ addr.street
                                        ++ (if addr.number /= "" then
                                                ", " ++ addr.number

                                            else
                                                ""
                                           )
                                , br [] []
                                , text addr.zip
                                ]
                    in
                    viewProfileItem
                        (text (t "Address"))
                        fullAddress
                        Top
                        Nothing

                Nothing ->
                    text ""

        viewKyc =
            case profile.kyc of
                Just kyc ->
                    let
                        documentLabel =
                            case kyc.documentType of
                                "cedula_de_identidad" ->
                                    "Cédula de identidad"

                                "dimex" ->
                                    "DIMEX"

                                "nite" ->
                                    "NITE"

                                "gran_empresa" ->
                                    "Gran Empresa"

                                "mipyme" ->
                                    "MIPYME"

                                _ ->
                                    "Unknown Document"
                    in
                    [ viewProfileItem
                        (text (t "community.kyc.phoneLabel"))
                        (text kyc.phone)
                        Center
                        Nothing
                    , viewProfileItem
                        (text documentLabel)
                        (text kyc.document)
                        Center
                        Nothing
                    ]

                Nothing ->
                    []

        viewContact =
            case pageType of
                Private ->
                    viewProfileItem (text "Contact Options")
                        (a
                            [ class "button-secondary button-sm uppercase cursor-pointer"
                            , Route.href Route.ProfileAddContact
                            ]
                            [ text
                                (if Maybe.map List.isEmpty profile.contacts |> Maybe.withDefault False then
                                    t "menu.add"

                                 else
                                    t "menu.edit"
                                )
                            ]
                        )
                        Center
                        Nothing

                Public ->
                    profile.contacts
                        |> Maybe.withDefault []
                        |> List.map (viewContactButton translators)
                        |> div [ class "flex flex-col space-y-4 mt-4 mb-2" ]

        leftSide =
            div
                [ classList
                    [ ( "py-4 md:w-1/2 bg-white border-r border-gray-500 flex", isPublic )
                    , ( "container mx-auto px-4", not isPublic )
                    ]
                ]
                [ div
                    [ classList [ ( "w-full px-4 md:container md:mx-auto md:max-w-md self-center", isPublic ) ]
                    ]
                    [ div
                        [ classList <|
                            let
                                hasBottomBorder =
                                    not isPublic
                            in
                            [ ( "pb-4", True )
                            , ( "border-b border-gray-500", hasBottomBorder )
                            , ( "w-full px-4 md:container md:mx-auto md:max-w-md", isPublic )
                            ]
                        ]
                        [ div [ class "flex mb-4 items-center flex-wrap" ]
                            [ Avatar.view profile.avatar "w-20 h-20 mr-6 xs-max:w-16 xs-max:h-16 xs-max:mr-3"
                            , div [ class "flex-grow flex items-center justify-between" ]
                                [ ul [ class "text-sm text-gray-900" ]
                                    [ li [ class "font-medium text-body-black text-2xl xs-max:text-xl" ]
                                        [ text userName ]
                                    , li [] [ a [ href <| "mailto:" ++ email ] [ text email ] ]
                                    , li [] [ text account ]
                                    ]
                                , case pageType of
                                    Private ->
                                        a
                                            [ class "ml-2"
                                            , Route.href Route.ProfileEditor
                                            ]
                                            [ Icons.edit "" ]

                                    Public ->
                                        text ""
                                ]
                            ]
                        , p [ class "text-sm text-gray-900" ]
                            [ text bio ]
                        ]
                    , case pageType of
                        Public ->
                            viewTransferButton
                                loggedIn.shared
                                loggedIn.selectedCommunity
                                account

                        Private ->
                            text ""
                    , case pageType of
                        Public ->
                            viewContact

                        Private ->
                            text ""
                    ]
                ]

        rightSide =
            ul
                [ class "divide-y divide-gray-500"
                , classList
                    [ ( "bg-white px-4 w-full md:max-w-md md:bg-gray-100", isPublic )
                    , ( "container mx-auto px-4", not isPublic )
                    ]
                ]
                ([ viewProfileItem
                    (text (t "profile.locations"))
                    (text location)
                    Center
                    Nothing
                 , viewAddress
                 , viewProfileItem
                    (text (t "profile.interests"))
                    (text (String.join ", " profile.interests))
                    Top
                    Nothing
                 , case pageType of
                    Public ->
                        text ""

                    Private ->
                        viewContact
                 ]
                    ++ (case pageType of
                            Private ->
                                viewKyc

                            Public ->
                                []
                       )
                )
    in
    div
        [ class "flex-grow"
        , classList
            [ ( "md:flex", isPublic )
            , ( "bg-white p-4 mb-6", not isPublic )
            ]
        ]
        [ leftSide
        , rightSide
        ]


type VerticalAlign
    = Top
    | Center


viewProfileItem : Html msg -> Html msg -> VerticalAlign -> Maybe (Html msg) -> Html msg
viewProfileItem lbl content vAlign extraContent =
    let
        vAlignClass =
            case vAlign of
                Top ->
                    "items-start"

                Center ->
                    "items-center"
    in
    li
        [ class "py-4" ]
        [ div
            [ class "flex justify-between"
            , class vAlignClass
            ]
            [ span [ class "text-sm leading-6 mr-4" ]
                [ lbl ]
            , span [ class "text-indigo-500 font-medium text-sm text-right" ]
                [ content ]
            ]
        , case extraContent of
            Just extra ->
                extra

            Nothing ->
                text ""
        ]


viewTransferButton : Shared -> Symbol -> String -> Html msg
viewTransferButton shared symbol user =
    let
        text_ s =
            text (shared.translators.t s)
    in
    div [ class "mt-3 mb-2" ]
        [ a
            [ class "button button-primary w-full"
            , Route.href (Route.Transfer symbol (Just user))
            ]
            [ text_ "transfer.title" ]
        ]


viewDownloadPdfErrorModal : Model -> LoggedIn.Model -> Html Msg
viewDownloadPdfErrorModal model loggedIn =
    let
        modalVisibility =
            case model.maybePdfDownloadedSuccessfully of
                Just isDownloaded ->
                    not isDownloaded

                Nothing ->
                    False

        privateKey =
            case LoggedIn.maybePrivateKey loggedIn of
                Nothing ->
                    ""

                Just pk ->
                    pk

        body =
            [ p [ class "my-3" ]
                [ text "Please, check if you have your 12 words saved during the registration process and use them for further signing in."
                ]
            , p [ class "my-3" ]
                [ text "If you completely lost your 12 words, please, contact us and provide this private key and we will help you to recover:"
                ]
            , p [ class "font-bold my-3 text-lg text-center border p-4 rounded-sm bg-gray-100" ]
                [ text privateKey
                ]
            ]
    in
    Modal.initWith
        { closeMsg = ClickedClosePdfDownloadError
        , isVisible = modalVisibility
        }
        |> Modal.withHeader "Sorry, we can't find your 12 words"
        |> Modal.withBody body
        |> Modal.toHtml


viewNewPinModal : Model -> Shared -> Html Msg
viewNewPinModal model shared =
    let
        tr str =
            text (shared.translators.t str)

        pinField =
            Pin.view
                shared
                { labelText = shared.translators.t "profile.newPin"
                , inputId = "pinInput"
                , inputValue = Maybe.withDefault "" model.newPin
                , onInputMsg = EnteredPin
                , onToggleMsg = TogglePinReadability
                , isVisible = True
                , errors =
                    case model.newPinErrorMsg of
                        Just err ->
                            [ err ]

                        Nothing ->
                            []
                }

        body =
            [ div []
                [ p [ class "text-sm" ]
                    [ tr "profile.changePinPrompt" ]
                ]
            , div [ class "mb-4" ] [ pinField ]
            , button
                [ class "button button-primary w-full"
                , onClick ChangePinSubmitted
                ]
                [ tr "profile.pin.button" ]
            ]
    in
    Modal.initWith
        { closeMsg = ClickedCloseChangePin
        , isVisible = model.isNewPinModalVisible
        }
        |> Modal.withHeader (shared.translators.t "profile.changePin")
        |> Modal.withBody body
        |> Modal.toHtml


viewButton : String -> Msg -> Html Msg
viewButton label msg =
    button
        [ class "button-secondary uppercase button-sm"
        , onClick msg
        ]
        [ text label
        ]


viewDangerButton : String -> Msg -> Html Msg
viewDangerButton label msg =
    button
        [ class "button-secondary uppercase button-sm text-red border-red"
        , onClick msg
        ]
        [ text label
        ]


viewContactButton : Translators -> Contact.Normalized -> Html msg
viewContactButton translators normalized =
    let
        { contactType } =
            Contact.unwrap normalized
    in
    a
        [ class
            ("button-secondary uppercase bg-gray-100 py-2 flex items-center justify-center border-none hover:bg-gray-200 text-"
                ++ Contact.contactTypeColor contactType
            )
        , Contact.toHref normalized
        , target "_blank"
        ]
        [ Contact.contactTypeToIcon "mr-2" contactType
        , text (Contact.contactTypeToString translators contactType)
        ]



-- viewTogglePush : LoggedIn.Model -> Model -> Html Msg
-- viewTogglePush loggedIn model =
--     let
--         tr str =
--             t loggedIn.shared.translations str
--         inputId =
--             "notifications"
--         lblColor =
--             if model.isPushNotificationsEnabled then
--                 "text-indigo-500"
--             else
--                 "text-gray"
--         lblText =
--             if model.isPushNotificationsEnabled then
--                 tr "settings.features.enabled"
--             else
--                 tr "settings.features.disabled"
--     in
--     div []
--         [ label
--             [ for inputId
--             , class "inline-block lowercase mr-2 cursor-pointer"
--             , class lblColor
--             ]
--             [ text lblText ]
--         , div [ class "form-switch inline-block align-middle" ]
--             [ input
--                 [ type_ "checkbox"
--                 , id inputId
--                 , name inputId
--                 , class "form-switch-checkbox"
--                 , checked model.isPushNotificationsEnabled
--                 , onClick RequestPush
--                 ]
--                 []
--             , label [ class "form-switch-label", for inputId ] []
--             ]
--         ]
-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = Ignored
    | CompletedProfileLoad (RemoteData (Graphql.Http.Error (Maybe Profile.Model)) (Maybe Profile.Model))
    | DownloadPdf String
    | DownloadPdfProcessed Bool
    | ClickedClosePdfDownloadError
    | ClickedViewPrivateKeyAuth
    | ClickedChangePin
    | ChangePinSubmitted
    | EnteredPin String
    | ClickedCloseChangePin
    | PinChanged
    | TogglePinReadability
    | GotPushPreference Bool
    | RequestPush
    | ToggleDeleteKycModal
    | AddKycClicked
    | DeleteKycAccepted
    | DeleteKycAndAddressCompleted (RemoteData (Graphql.Http.Error DeleteKycAndAddressResult) DeleteKycAndAddressResult)
    | CheckPushPref
    | GotPushSub PushSubscription
    | CompletedPushUpload (RemoteData (Graphql.Http.Error ()) ())


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        t =
            loggedIn.shared.translators.t

        downloadPdfPort pin =
            UR.addPort
                { responseAddress = DownloadPdfProcessed False
                , responseData = Encode.null
                , data =
                    Encode.object
                        [ ( "name", Encode.string "downloadAuthPdfFromProfile" )
                        , ( "pin", Encode.string pin )
                        ]
                }
    in
    case msg of
        Ignored ->
            UR.init model

        ToggleDeleteKycModal ->
            { model | isDeleteKycModalShowed = not model.isDeleteKycModalShowed }
                |> UR.init

        AddKycClicked ->
            model
                |> UR.init
                |> UR.addCmd
                    (Route.ProfileAddKyc
                        |> Route.replaceUrl loggedIn.shared.navKey
                    )

        DeleteKycAccepted ->
            { model
                | status = Loading loggedIn.accountName
                , isDeleteKycModalShowed = False
            }
                |> UR.init
                |> UR.addCmd
                    (deleteKycAndAddress loggedIn)

        DeleteKycAndAddressCompleted resp ->
            let
                reloadProfile =
                    Api.Graphql.query loggedIn.shared
                        (Just loggedIn.authToken)
                        (Profile.query loggedIn.accountName)
                        CompletedProfileLoad
            in
            case resp of
                RemoteData.Success _ ->
                    model
                        |> UR.init
                        |> UR.addCmd reloadProfile
                        |> UR.addExt (ShowFeedback Success (t "community.kyc.delete.success"))

                RemoteData.Failure err ->
                    model
                        |> UR.init
                        |> UR.logGraphqlError msg err
                        |> UR.addCmd reloadProfile

                _ ->
                    UR.init model

        CompletedProfileLoad (RemoteData.Success Nothing) ->
            UR.init model

        CompletedProfileLoad (RemoteData.Success (Just profile)) ->
            { model | status = Loaded profile }
                |> UR.init
                |> UR.addExt
                    -- Update global `LoggedIn.Model` to enable restrictions after removing KYC data
                    (LoggedIn.UpdatedLoggedIn
                        { loggedIn | profile = Loaded profile }
                    )

        CompletedProfileLoad (RemoteData.Failure err) ->
            UR.init { model | status = LoadingFailed loggedIn.accountName err }
                |> UR.logGraphqlError msg err

        CompletedProfileLoad _ ->
            UR.init model

        ClickedChangePin ->
            if LoggedIn.isAuth loggedIn then
                UR.init { model | isNewPinModalVisible = True }
                    |> UR.addCmd
                        (Dom.focus "pinInput"
                            |> Task.attempt (\_ -> Ignored)
                        )

            else
                UR.init model
                    |> UR.addExt (Just ClickedChangePin |> RequiredAuthentication)
                    |> UR.addCmd
                        (Dom.focus "pinInput"
                            |> Task.attempt (\_ -> Ignored)
                        )

        TogglePinReadability ->
            UR.init { model | isNewPinReadable = not model.isNewPinReadable }

        ChangePinSubmitted ->
            if LoggedIn.isAuth loggedIn then
                let
                    currentPin =
                        case model.currentPin of
                            Just pin ->
                                pin

                            Nothing ->
                                loggedIn.auth.form.enteredPin

                    newPin =
                        Maybe.withDefault "" model.newPin
                in
                if Pin.isValid newPin then
                    UR.init model
                        |> UR.addPort
                            { responseAddress = PinChanged
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "changePin" )
                                    , ( "currentPin", Encode.string currentPin )
                                    , ( "newPin", Encode.string newPin )
                                    ]
                            }

                else
                    UR.init { model | newPinErrorMsg = Just (t "auth.pin.shouldHaveSixDigitsError") }
                        |> UR.addCmd Cmd.none

            else
                UR.init model
                    |> UR.addExt (Just ClickedChangePin |> RequiredAuthentication)

        EnteredPin newPin ->
            UR.init { model | newPinErrorMsg = Nothing, newPin = Just newPin }

        ClickedCloseChangePin ->
            UR.init { model | isNewPinModalVisible = False }

        PinChanged ->
            { model
                | isNewPinModalVisible = False
                , currentPin = model.newPin
                , newPin = Nothing
            }
                |> UR.init
                |> UR.addExt (ShowFeedback Success (t "profile.pin.successMsg"))

        ClickedViewPrivateKeyAuth ->
            case LoggedIn.maybePrivateKey loggedIn of
                Nothing ->
                    UR.init model
                        |> UR.addExt
                            (Just ClickedViewPrivateKeyAuth
                                |> RequiredAuthentication
                            )
                        |> UR.addCmd
                            (Dom.focus "pinInput"
                                |> Task.attempt (\_ -> Ignored)
                            )

                Just _ ->
                    model
                        |> UR.init
                        |> downloadPdfPort loggedIn.auth.form.enteredPin

        DownloadPdf pin ->
            model
                |> UR.init
                |> downloadPdfPort pin

        DownloadPdfProcessed isDownloaded ->
            { model | maybePdfDownloadedSuccessfully = Just isDownloaded }
                |> UR.init

        ClickedClosePdfDownloadError ->
            { model | maybePdfDownloadedSuccessfully = Nothing }
                |> UR.init

        GotPushPreference val ->
            { model | isPushNotificationsEnabled = val }
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

        RequestPush ->
            if model.isPushNotificationsEnabled then
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
                RemoteData.Success _ ->
                    model
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = CompletedPushUpload res
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "completedPushUpload" ) ]
                            }

                RemoteData.Failure err ->
                    model
                        |> UR.init
                        |> UR.logGraphqlError msg err

                _ ->
                    UR.init model


decodePushPref : Value -> Maybe Msg
decodePushPref val =
    Decode.decodeValue (Decode.field "isSet" Decode.bool) val
        |> Result.map GotPushPreference
        |> Result.toMaybe


decodeIsPdfDownloaded : Value -> Maybe Msg
decodeIsPdfDownloaded val =
    Decode.decodeValue (Decode.field "isDownloaded" Decode.bool) val
        |> Result.map DownloadPdfProcessed
        |> Result.toMaybe


uploadPushSubscription : LoggedIn.Model -> PushSubscription -> Cmd Msg
uploadPushSubscription { accountName, shared, authToken } data =
    Api.Graphql.mutation shared
        (Just authToken)
        (PushSubscription.activatePushMutation accountName data)
        CompletedPushUpload


deleteKycAndAddress : LoggedIn.Model -> Cmd Msg
deleteKycAndAddress { accountName, shared, authToken } =
    Api.Graphql.mutation shared
        (Just authToken)
        (Profile.deleteKycAndAddressMutation accountName)
        DeleteKycAndAddressCompleted


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClickedCloseChangePin" :: [] ->
            Just ClickedCloseChangePin

        "PinChanged" :: [] ->
            Just PinChanged

        "DownloadPdfProcessed" :: _ ->
            decodeIsPdfDownloaded val

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
                    Nothing

        "ChangePinSubmitted" :: [] ->
            Just ChangePinSubmitted

        "TogglePinReadability" :: [] ->
            Just TogglePinReadability

        "GotPushPreference" :: _ ->
            decodePushPref val

        "CompletedPushUpload" :: _ ->
            decodePushPref val

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        ToggleDeleteKycModal ->
            [ "ToggleDeleteKycModal" ]

        AddKycClicked ->
            [ "AddKycClicked" ]

        DeleteKycAccepted ->
            [ "DeleteKycAccepted" ]

        DeleteKycAndAddressCompleted _ ->
            [ "DeleteKycAndAddressCompleted" ]

        CompletedProfileLoad r ->
            [ "CompletedProfileLoad", UR.remoteDataToString r ]

        DownloadPdf _ ->
            [ "DownloadPdf" ]

        DownloadPdfProcessed _ ->
            [ "DownloadPdfProcessed" ]

        ClickedClosePdfDownloadError ->
            [ "ClickedClosePdfDownloadError" ]

        ClickedChangePin ->
            [ "ClickedChangePin" ]

        ClickedCloseChangePin ->
            [ "ClickedCloseChangePin" ]

        PinChanged ->
            [ "PinChanged" ]

        ClickedViewPrivateKeyAuth ->
            [ "ClickedViewPrivateKeyAuth" ]

        TogglePinReadability ->
            [ "TogglePinReadability" ]

        ChangePinSubmitted ->
            [ "ChangePinSubmitted" ]

        EnteredPin _ ->
            [ "EnteredPin" ]

        GotPushPreference _ ->
            [ "GotPushPreference" ]

        RequestPush ->
            [ "RequestPush" ]

        CheckPushPref ->
            [ "CheckPushPref" ]

        GotPushSub _ ->
            [ "GotPushSub" ]

        CompletedPushUpload _ ->
            [ "CompletedPushUpload" ]
