module Page.Register exposing (Model, Msg, init, jsAddressToMsg, msgToString, subscriptions, update, view)

import Api
import Api.Graphql
import Browser.Events
import Char
import Community exposing (Invite)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, button, div, img, input, label, li, p, span, strong, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, id, src, style, type_, value)
import Html.Events exposing (onCheck, onClick)
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Page.Register.Common as Common
import Page.Register.JuridicalForm as JuridicalForm
import Page.Register.NaturalForm as NaturalForm
import Profile exposing (Profile)
import Route
import Session.Guest as Guest exposing (External(..))
import Session.Shared exposing (Translators, viewFullLoading)
import Task
import UpdateResult as UR
import Utils exposing (decodeEnterKeyDown)
import Validate exposing (Valid, ifBlank, ifFalse, ifInvalidEmail, ifTrue, validate)



-- INIT


init : Maybe String -> Guest.Model -> ( Model, Cmd Msg )
init maybeInvitationId guest =
    ( initModel maybeInvitationId guest
    , case maybeInvitationId of
        Just invitation ->
            Api.Graphql.query guest.shared (Community.inviteQuery invitation) CompletedLoadInvite

        Nothing ->
            Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyPressed (Browser.Events.onKeyDown decodeEnterKeyDown)



-- MODEL


type alias Model =
    { accountKeys : Maybe AccountKeys
    , isLoading : Bool
    , isCheckingAccount : Bool
    , hasAgreedToSavePassphrase : Bool
    , isPassphraseCopiedToClipboard : Bool
    , accountGenerated : Bool
    , problems : List Problem
    , status : Status
    , maybeInvitationId : Maybe String
    , documentNumber : String
    , selectedForm : FormType
    , problems2 : List ( Common.Errors, String )
    }


type AccountType
    = NaturalAccount
    | JuridicalAccount


type FormType
    = None
    | Natural NaturalForm.Model
    | Juridical JuridicalForm.Model


initModel : Maybe String -> Guest.Model -> Model
initModel maybeInvitationId _ =
    { accountKeys = Nothing
    , isLoading = False
    , isCheckingAccount = False
    , hasAgreedToSavePassphrase = False
    , isPassphraseCopiedToClipboard = False
    , accountGenerated = False
    , problems = []
    , status = Loading
    , maybeInvitationId = maybeInvitationId
    , documentNumber = ""
    , selectedForm = None
    , problems2 = []
    }



---- FORM


type alias Form =
    { username : String
    , email : String
    , account : String
    }


initForm : Form
initForm =
    { username = ""
    , email = ""
    , account = ""
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type ValidatedField
    = Username
    | Email
    | Account



---- ACCOUNT KEYS


type alias AccountKeys =
    { ownerKey : String
    , activeKey : String
    , accountName : Eos.Name
    , transactionId : String
    , words : String
    , privateKey : String
    }


decodeAccount : Decoder AccountKeys
decodeAccount =
    Decode.succeed AccountKeys
        |> Decode.required "ownerKey" Decode.string
        |> Decode.required "activeKey" Decode.string
        |> Decode.required "accountName" Eos.nameDecoder
        |> Decode.required "transactionId" Decode.string
        |> Decode.required "words" Decode.string
        |> Decode.required "privateKey" Decode.string



-- VIEW


view : Guest.Model -> Model -> { title : String, content : Html Msg }
view guest model =
    let
        shared =
            guest.shared

        { t } =
            shared.translators

        isDisabled =
            model.isLoading || model.isCheckingAccount

        name =
            accName model

        passphraseTextId =
            "passphraseText"

        passphraseInputId =
            -- Passphrase text is duplicated in `input:text` to be able to copy via Browser API
            "passphraseWords"

        viewAccountGenerated =
            div
                [ class "flex-grow bg-purple-500 flex md:block"
                ]
                [ div
                    [ class "sf-wrapper"
                    , class "px-4 md:max-w-sm md:mx-auto md:pt-20 md:px-0 text-white text-body"
                    ]
                    [ div [ class "sf-content" ]
                        [ viewTitleForStep shared.translators 2
                        , p
                            [ class "text-xl mb-3" ]
                            [ text (t "register.account_created.greet")
                            , text " "
                            , strong [] [ text name ]
                            , text ", "
                            , text (t "register.account_created.last_step")
                            ]
                        , p [ class "mb-3" ]
                            [ text (t "register.account_created.instructions")
                            ]
                        , div [ class "w-1/4 m-auto relative left-1" ]
                            [ img [ src "images/reg-passphrase-boy.svg" ]
                                []
                            , img
                                [ class "absolute w-1/4 -mt-2 -ml-10"
                                , src "images/reg-passphrase-boy-hand.svg"
                                ]
                                []
                            ]
                        , div [ class "bg-white text-black text-2xl mb-12 p-4 rounded-lg" ]
                            [ p [ class "input-label" ]
                                [ text (t "register.account_created.twelve_words")
                                , if model.isPassphraseCopiedToClipboard then
                                    strong [ class "uppercase ml-1" ]
                                        [ text (t "register.account_created.words_copied")
                                        , text " ✔"
                                        ]

                                  else
                                    text ""
                                ]
                            , p
                                [ class "pb-2 leading-tight" ]
                                [ span [ id passphraseTextId ] [ text (words model) ]
                                , input
                                    -- We use `HTMLInputElement.select()` method in port to select and copy the text. This method
                                    -- works only with `input` and `textarea` elements which has to be presented in DOM (e.g. we can't
                                    -- hide it with `display: hidden`), so we hide it using position and opacity.
                                    [ type_ "text"
                                    , class "absolute opacity-0"
                                    , style "left" "-9999em"
                                    , id passphraseInputId
                                    , value (words model)
                                    ]
                                    []
                                ]
                            , button
                                [ class "button m-auto button-primary button-sm"
                                , onClick <| CopyToClipboard passphraseInputId
                                ]
                                [ text (t "register.account_created.copy") ]
                            ]
                        ]
                    , div [ class "sf-footer" ]
                        [ div [ class "my-4" ]
                            [ label [ class "form-label block" ]
                                [ input
                                    [ type_ "checkbox"
                                    , class "form-checkbox mr-2 p-1"
                                    , checked model.hasAgreedToSavePassphrase
                                    , onCheck AgreedToSave12Words
                                    ]
                                    []
                                , text (t "register.account_created.i_saved_words")
                                , text " 💜"
                                ]
                            ]
                        , case pdfData model of
                            Just data ->
                                button
                                    [ onClick <| DownloadPdf data
                                    , class "button button-primary w-full mb-8"
                                    , disabled (not model.hasAgreedToSavePassphrase)
                                    , class <|
                                        if model.hasAgreedToSavePassphrase then
                                            ""

                                        else
                                            "button-disabled text-gray-600"
                                    ]
                                    [ text (t "register.account_created.download") ]

                            Nothing ->
                                text ""
                        ]
                    ]
                ]
    in
    { title =
        t "register.registerTab"
    , content =
        div [ class "flex flex-grow flex-col bg-white md:block px-4 px-0" ]
            [ case model.status of
                Loaded invitation ->
                    if invitation.community.hasShop == True then
                        viewKycRegister guest.shared.translators model

                    else
                        viewDefaultAccountRegister guest.shared.translators model

                Loading ->
                    div [] []

                _ ->
                    Debug.todo "Error"
            , button [ onClick (ValidateForm model.selectedForm) ] [ text "validate" ]
            ]

    -- if model.accountGenerated then
    --     viewAccountGenerated
    -- else
    --     viewCreateAccount
    }


viewKycRegister : Translators -> Model -> Html Msg
viewKycRegister translators model =
    div []
        [ viewFormTypeSelector translators model
        , div [ class "sf-content" ]
            [ case model.maybeInvitationId of
                Just _ ->
                    let
                        selectedForm =
                            case model.selectedForm of
                                Natural form ->
                                    NaturalForm.view translators form |> Html.map NaturalFormMsg

                                Juridical form ->
                                    JuridicalForm.view translators form |> Html.map JuridicalFormMsg

                                None ->
                                    div [] []
                    in
                    case model.status of
                        Loaded _ ->
                            selectedForm

                        Loading ->
                            Session.Shared.viewFullLoading

                        Failed _ ->
                            Debug.todo "Implement error"

                        NotFound ->
                            Debug.todo "Implement not found"

                Nothing ->
                    div [] []
            ]
            |> Html.map FormMsg
        ]


viewFormTypeSelector : Translators -> Model -> Html Msg
viewFormTypeSelector translators model =
    div [ class "flex w-full justify-center" ]
        [ viewFormTypeRadio
            { type_ = NaturalAccount
            , label = translators.t "register.form.types.natural"
            , styles = ""
            , isSelected =
                case model.selectedForm of
                    Natural _ ->
                        True

                    _ ->
                        False
            , onClick = AccountTypeSelected
            }
        , viewFormTypeRadio
            { type_ = JuridicalAccount
            , label = translators.t "register.form.types.juridical"
            , styles = "ml-4"
            , isSelected =
                case model.selectedForm of
                    Juridical _ ->
                        True

                    _ ->
                        False
            , onClick = AccountTypeSelected
            }
        ]


type alias FormTypeRadioOptions a =
    { type_ : AccountType
    , label : String
    , styles : String
    , isSelected : Bool
    , onClick : AccountType -> a
    }


viewFormTypeRadio : FormTypeRadioOptions Msg -> Html Msg
viewFormTypeRadio options =
    let
        defaultClasses =
            "w-40 h-10 rounded-sm flex justify-center items-center cursor-pointer "

        ifSelectedClasses =
            "bg-orange-300 text-white "

        unselectedClasses =
            "bg-gray-100 text-black "

        finalClasses =
            defaultClasses
                ++ (if options.isSelected then
                        ifSelectedClasses

                    else
                        unselectedClasses
                   )

        id =
            case options.type_ of
                NaturalAccount ->
                    "natural"

                JuridicalAccount ->
                    "juridical"
    in
    div [ class (finalClasses ++ options.styles), onClick (options.onClick options.type_) ]
        [ label [ class "cursor-pointer", for id ] [ text options.label ]
        , input [ class "hidden", type_ "radio", checked options.isSelected, onClick (options.onClick options.type_) ] []
        ]


viewTitleForStep : Translators -> Int -> Html msg
viewTitleForStep translators s =
    let
        { t, tr } =
            translators

        step =
            String.fromInt s
    in
    p
        [ class "py-4 mb-4 text-body border-b border-dotted text-grey border-grey-500" ]
        [ text (tr "register.form.step" [ ( "stepNum", step ) ])
        , text " / "
        , strong
            [ class <|
                if s == 1 then
                    "text-black"

                else
                    "text-white"
            ]
            [ text <| t ("register.form.step" ++ step ++ "_title") ]
        ]


viewDefaultAccountRegister : Translators -> Model -> Html Msg
viewDefaultAccountRegister translators model =
    div []
        [ viewServerErrors model.problems
        , viewTitleForStep translators 1
        ]


viewServerErrors : List Problem -> Html msg
viewServerErrors problems =
    let
        errorList =
            problems
                |> List.filterMap
                    (\p ->
                        case p of
                            ServerError e ->
                                Just (li [] [ text e ])

                            _ ->
                                Nothing
                    )
    in
    if List.isEmpty errorList then
        text ""

    else
        ul [ class "bg-red border-lg rounded p-4 mt-2 text-white" ] errorList


accName : Model -> String
accName model =
    case model.accountKeys of
        Just keys ->
            Eos.nameToString keys.accountName

        Nothing ->
            ""


pdfData : Model -> Maybe PdfData
pdfData model =
    Maybe.andThen
        (\keys ->
            Just
                { passphrase = keys.words
                , accountName = Eos.nameToString keys.accountName
                }
        )
        model.accountKeys


words : Model -> String
words model =
    case model.accountKeys of
        Just keys ->
            keys.words

        Nothing ->
            ""



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg External


type Msg
    = ValidateForm FormType
    | GotAccountAvailabilityResponse Bool
    | KeyPressed Bool
    | AccountGenerated (Result Decode.Error AccountKeys)
    | CompletedCreateProfile AccountKeys (Result Http.Error Profile)
    | AgreedToSave12Words Bool
    | DownloadPdf PdfData
    | PdfDownloaded
    | CopyToClipboard String
    | CopiedToClipboard
    | CompletedLoadInvite (Result (Graphql.Http.Error (Maybe Invite)) (Maybe Invite))
    | AccountTypeSelected AccountType
    | FormMsg EitherFormMsg


type EitherFormMsg
    = JuridicalFormMsg JuridicalForm.Msg
    | NaturalFormMsg NaturalForm.Msg


type Status
    = Loaded Invite
    | Loading
    | Failed (Graphql.Http.Error (Maybe Invite))
    | NotFound


type alias PdfData =
    { passphrase : String
    , accountName : String
    }


update : Maybe String -> Msg -> Model -> Guest.Model -> UpdateResult
update maybeInvitation msg model guest =
    let
        { t, tr } =
            guest.shared.translators
    in
    case msg of
        ValidateForm formType ->
            UR.init model

        FormMsg formMsg ->
            case formMsg of
                JuridicalFormMsg innerMsg ->
                    case model.selectedForm of
                        Juridical form ->
                            UR.init { model | selectedForm = Juridical (JuridicalForm.update innerMsg form) }

                        _ ->
                            UR.init model

                NaturalFormMsg innerMsg ->
                    case model.selectedForm of
                        Natural form ->
                            UR.init { model | selectedForm = Natural (NaturalForm.update innerMsg form) }

                        _ ->
                            UR.init model

        AccountTypeSelected type_ ->
            UR.init
                { model
                    | selectedForm =
                        case type_ of
                            NaturalAccount ->
                                Natural NaturalForm.init

                            JuridicalAccount ->
                                Juridical
                                    JuridicalForm.init
                }

        GotAccountAvailabilityResponse isAvailable ->
            if isAvailable then
                UR.init
                    { model
                        | isLoading = True
                        , isCheckingAccount = False
                    }
                    |> UR.addPort
                        { responseAddress = GotAccountAvailabilityResponse False
                        , responseData = Encode.null
                        , data =
                            Encode.object
                                [ ( "name", Encode.string "generateAccount" )
                                , ( "invitationId"
                                  , case maybeInvitation of
                                        Nothing ->
                                            Encode.null

                                        Just invitationId ->
                                            Encode.string invitationId
                                  )

                                -- , ( "account", Encode.string model.form.account )
                                ]
                        }

            else
                let
                    fieldError =
                        InvalidEntry Account (t "error.alreadyTaken")
                in
                UR.init
                    { model
                        | isCheckingAccount = False
                    }

        AccountGenerated (Err v) ->
            UR.init
                { model
                    | isLoading = False
                }
                |> UR.logDecodeError msg v

        CompletedCreateProfile _ (Ok _) ->
            { model | accountGenerated = True }
                |> UR.init

        AgreedToSave12Words val ->
            { model | hasAgreedToSavePassphrase = val }
                |> UR.init

        CopyToClipboard elementId ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = CopiedToClipboard
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "id", Encode.string elementId )
                            , ( "name", Encode.string "copyToClipboard" )
                            ]
                    }

        CopiedToClipboard ->
            { model | isPassphraseCopiedToClipboard = True }
                |> UR.init

        DownloadPdf { passphrase, accountName } ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = PdfDownloaded
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "downloadAuthPdfFromRegistration" )
                            , ( "accountName", Encode.string accountName )
                            , ( "passphrase", Encode.string passphrase )
                            ]
                    }

        PdfDownloaded ->
            case model.accountKeys of
                Nothing ->
                    model
                        |> UR.init

                Just _ ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            -- Go to login page after downloading PDF
                            (Route.replaceUrl guest.shared.navKey (Route.Login Nothing))

        CompletedLoadInvite (Ok (Just invitation)) ->
            UR.init { model | status = Loaded invitation }

        CompletedLoadInvite (Ok Nothing) ->
            UR.init { model | status = NotFound }

        CompletedLoadInvite (Err error) ->
            { model | status = Failed error }
                |> UR.init
                |> UR.logGraphqlError msg error

        _ ->
            UR.init model



--
-- Model functions
--


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ValidateForm" :: [] ->
            Decode.decodeValue
                (Decode.field "isAvailable" Decode.bool)
                val
                |> Result.map GotAccountAvailabilityResponse
                |> Result.toMaybe

        "GotAccountAvailabilityResponse" :: _ ->
            Decode.decodeValue (Decode.field "data" decodeAccount) val
                |> AccountGenerated
                |> Just

        "PdfDownloaded" :: _ ->
            Just PdfDownloaded

        "CopiedToClipboard" :: _ ->
            Just CopiedToClipboard

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        FormMsg _ ->
            [ "FormMsg" ]

        ValidateForm _ ->
            [ "ValidateForm" ]

        GotAccountAvailabilityResponse _ ->
            [ "GotAccountAvailabilityResponse" ]

        AccountGenerated r ->
            [ "AccountGenerated", UR.resultToString r ]

        CompletedCreateProfile _ r ->
            [ "CompletedCreateProfile", UR.resultToString r ]

        AgreedToSave12Words _ ->
            [ "AgreedToSave12Words" ]

        CopyToClipboard _ ->
            [ "CopyToClipboard" ]

        CopiedToClipboard ->
            [ "CopiedToClipboard" ]

        DownloadPdf _ ->
            [ "DownloadPdf" ]

        PdfDownloaded ->
            [ "PdfDownloaded" ]

        CompletedLoadInvite _ ->
            [ "CompletedLoadInvite" ]

        AccountTypeSelected _ ->
            [ "AccountTypeSelected" ]

        KeyPressed _ ->
            [ "KeyPressed" ]
