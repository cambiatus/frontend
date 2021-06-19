module Page.Profile exposing
    ( Model
    , Msg
    , ProfilePage(..)
    , init
    , jsAddressToMsg
    , msgToString
    , update
    , view
    , viewUserInfo
    )

import Api.Graphql
import Avatar
import Browser.Dom as Dom
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, a, br, button, div, label, li, p, span, text, ul)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page
import Profile exposing (DeleteKycAndAddressResult)
import Profile.Contact as Contact
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared, Translators)
import Task
import UpdateResult as UR
import View.Feedback as Feedback
import View.Modal as Modal
import View.Pin as Pin



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Task.succeed CheckPushPref |> Task.perform identity
    )



-- MODEL


type alias Model =
    { currentPin : Maybe String
    , isNewPinModalVisible : Bool
    , isPushNotificationsEnabled : Bool
    , maybePdfDownloadedSuccessfully : Maybe Bool
    , isDeleteKycModalShowed : Bool
    , pinInputModel : Pin.Model
    }


initModel : Model
initModel =
    { currentPin = Nothing
    , isNewPinModalVisible = False
    , isPushNotificationsEnabled = False
    , maybePdfDownloadedSuccessfully = Nothing
    , isDeleteKycModalShowed = False
    , pinInputModel =
        Pin.init
            { label = "profile.newPin"
            , id = "new-pin-input"
            , withConfirmation = False
            , submitLabel = "profile.pin.button"
            , submittingLabel = "profile.pin.button"
            }
    }



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        title =
            case loggedIn.profile of
                RemoteData.Success profile ->
                    Maybe.withDefault "" profile.name

                _ ->
                    ""

        content =
            case loggedIn.profile of
                RemoteData.Loading ->
                    Page.fullPageLoading shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading shared

                RemoteData.Failure e ->
                    Page.fullPageGraphQLError (shared.translators.t "profile.title") e

                RemoteData.Success profile ->
                    div [ class "flex-grow flex flex-col" ]
                        [ Page.viewHeader loggedIn (shared.translators.t "menu.profile")
                        , viewUserInfo loggedIn
                            profile
                            Private
                            (viewSettings loggedIn model profile)
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
                                    loggedIn.auth.pinModel.pin
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
    div [ class "bg-white mb-6 w-full md:bg-gray-100" ]
        [ ul [ class "w-full divide-y divide-gray-500" ]
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


viewUserInfo : LoggedIn.Model -> Profile.Model -> ProfilePage -> Html msg -> Html msg
viewUserInfo loggedIn profile pageType privateView =
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
                    viewProfileItem
                        (text documentLabel)
                        (text kyc.document)
                        Center
                        Nothing

                Nothing ->
                    text ""

        viewContact =
            case pageType of
                Private ->
                    viewProfileItem (text (t "contact_form.options"))
                        (a
                            [ class "button-secondary button-sm uppercase cursor-pointer"
                            , Route.href Route.ProfileAddContact
                            ]
                            [ text
                                (if List.isEmpty profile.contacts then
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
                        |> List.map (viewContactButton translators)
                        |> div [ class "flex flex-col space-y-4 mt-4 mb-2" ]

        leftSide =
            div
                [ class "p-4 bg-white border-white border-r md:border-gray-500 flex md:w-1/2" ]
                [ div
                    [ class "w-full container mx-auto md:max-w-lg self-center" ]
                    [ div
                        [ class "pb-4 w-full" ]
                        [ div [ class "flex mb-4 items-center flex-wrap justify-center" ]
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
            div [ class "w-full bg-gray-100 md:w-1/2" ]
                [ div [ class "w-full bg-white md:bg-gray-100" ]
                    [ div [ class "px-4" ]
                        [ ul [ class "container mx-auto divide-y divide-gray-500 w-full mb-4 bg-white md:bg-gray-100" ]
                            [ viewProfileItem
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
                            , case pageType of
                                Private ->
                                    viewKyc

                                Public ->
                                    text ""
                            ]
                        ]
                    ]
                , div [ class "bg-white w-full md:bg-gray-100" ]
                    [ div [ class "px-4" ]
                        [ div [ class "container mx-auto" ]
                            [ privateView
                            ]
                        ]
                    ]
                ]
    in
    div [ class "flex-grow flex bg-gray-100 relative" ]
        [ div [ class "z-10 flex flex-col w-full md:container md:mx-auto md:flex-row bg-grey-100" ]
            [ leftSide
            , rightSide
            ]
        , div [ class "z-0 absolute w-full md:w-1/2 h-full max-h-100 md:bg-white" ] []
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


viewTransferButton : Shared -> String -> Html msg
viewTransferButton shared user =
    let
        text_ s =
            text (shared.translators.t s)
    in
    div [ class "mt-3 mb-2" ]
        [ a
            [ class "button button-primary w-full"
            , Route.href (Route.Transfer (Just user))
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
                    Eos.privateKeyToString pk

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

        body =
            [ div []
                [ p [ class "text-sm" ]
                    [ tr "profile.changePinPrompt" ]
                ]
            , Pin.view shared.translators model.pinInputModel
                |> Html.map GotPinMsg
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

        textColor =
            "text-" ++ Contact.contactTypeColor contactType
    in
    a
        [ class ("button-secondary uppercase bg-gray-100 py-2 flex items-center justify-center border-none hover:bg-gray-200 " ++ textColor)
        , Contact.toHref normalized
        , target "_blank"
        ]
        [ Contact.contactTypeToIcon "mr-2 w-6 h-6" True contactType
        , text (Contact.contactTypeToString translators contactType)
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = Ignored
    | DownloadPdf String
    | DownloadPdfProcessed Bool
    | ClickedClosePdfDownloadError
    | ClickedViewPrivateKeyAuth
    | ClickedChangePin
    | ChangePinSubmitted String
    | GotPinMsg Pin.Msg
    | ClickedCloseChangePin
    | PinChanged
    | GotPushPreference Bool
    | ToggleDeleteKycModal
    | AddKycClicked
    | DeleteKycAccepted
    | DeleteKycAndAddressCompleted (RemoteData (Graphql.Http.Error DeleteKycAndAddressResult) DeleteKycAndAddressResult)
    | CheckPushPref


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
            { model | isDeleteKycModalShowed = False }
                |> UR.init
                |> UR.addCmd (deleteKycAndAddress loggedIn)
                |> UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | profile = RemoteData.Loading })

        DeleteKycAndAddressCompleted resp ->
            case resp of
                RemoteData.Success _ ->
                    model
                        |> UR.init
                        |> UR.addExt (LoggedIn.ReloadResource LoggedIn.ProfileResource)
                        |> UR.addExt (ShowFeedback Feedback.Success (t "community.kyc.delete.success"))

                RemoteData.Failure err ->
                    model
                        |> UR.init
                        |> UR.logGraphqlError msg err
                        |> UR.addExt (LoggedIn.ReloadResource LoggedIn.ProfileResource)

                _ ->
                    UR.init model

        ClickedChangePin ->
            UR.init { model | isNewPinModalVisible = True }
                |> UR.addCmd
                    (Dom.focus "pinInput"
                        |> Task.attempt (\_ -> Ignored)
                    )
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = Ignored }

        ChangePinSubmitted newPin ->
            let
                currentPin =
                    case model.currentPin of
                        Just pin ->
                            pin

                        Nothing ->
                            loggedIn.auth.pinModel.pin
            in
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
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = Ignored }

        GotPinMsg subMsg ->
            let
                ( newPinModel, submitStatus ) =
                    Pin.update subMsg model.pinInputModel
            in
            UR.init { model | pinInputModel = newPinModel }
                |> UR.addCmd (Pin.maybeSubmitCmd submitStatus ChangePinSubmitted)

        ClickedCloseChangePin ->
            UR.init { model | isNewPinModalVisible = False }

        PinChanged ->
            { model
                | isNewPinModalVisible = False
                , currentPin = Just model.pinInputModel.pin
            }
                |> UR.init
                |> UR.addExt (ShowFeedback Feedback.Success (t "profile.pin.successMsg"))

        ClickedViewPrivateKeyAuth ->
            model
                |> UR.init
                |> downloadPdfPort loggedIn.auth.pinModel.pin
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = Ignored }

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

        "ChangePinSubmitted" :: pin :: [] ->
            Just (ChangePinSubmitted pin)

        "GotPushPreference" :: _ ->
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

        ChangePinSubmitted pin ->
            [ "ChangePinSubmitted", pin ]

        GotPushPreference _ ->
            [ "GotPushPreference" ]

        CheckPushPref ->
            [ "CheckPushPref" ]

        GotPinMsg subMsg ->
            "GotPinMsg" :: Pin.msgToString subMsg
