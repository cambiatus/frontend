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
import Profile exposing (Profile)
import Route
import Session.Guest as Guest exposing (External(..))
import Session.Shared exposing (Translators, viewFullLoading)
import Task
import UpdateResult as UR
import Utils exposing (decodeEnterKeyDown)
import Validate exposing (ifBlank, ifFalse, ifInvalidEmail, ifTrue, validate)
import View.Form.Input
import View.Form.Select



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
    , form : Form
    , hasAgreedToSavePassphrase : Bool
    , isPassphraseCopiedToClipboard : Bool
    , accountGenerated : Bool
    , problems : List Problem
    , accountType : AccountType
    , status : Status
    , maybeInvitationId : Maybe String
    , selectedDocument : String
    , documentNumber : String
    }


type AccountType
    = Unspecified
    | Natural
    | Juridical


initModel : Maybe String -> Guest.Model -> Model
initModel maybeInvitationId _ =
    { accountKeys = Nothing
    , isLoading = False
    , isCheckingAccount = False
    , form = initForm
    , hasAgreedToSavePassphrase = False
    , isPassphraseCopiedToClipboard = False
    , accountGenerated = False
    , problems = []
    , accountType = Unspecified
    , status = Loading
    , maybeInvitationId = maybeInvitationId
    , selectedDocument = "ssn"
    , documentNumber = ""
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
            (case model.status of
                Loaded invitation ->
                    if invitation.community.hasShop == True then
                        viewKycRegister guest.shared.translators model

                    else
                        viewDefaultAccountRegister guest.shared.translators model

                Loading ->
                    []

                _ ->
                    Debug.todo "Error"
            )

    -- if model.accountGenerated then
    --     viewAccountGenerated
    -- else
    --     viewCreateAccount
    }


viewKycRegister : Translators -> Model -> List (Html Msg)
viewKycRegister translators model =
    [ viewAccountTypeSelector translators model
    , div [ class "sf-content" ]
        (case model.maybeInvitationId of
            Just _ ->
                let
                    selectedForm =
                        case model.accountType of
                            Natural ->
                                viewNaturalAccountRegister translators model

                            Juridical ->
                                viewJuridicalAccountRegister translators model

                            Unspecified ->
                                []
                in
                case model.status of
                    Loaded _ ->
                        selectedForm

                    Loading ->
                        [ Session.Shared.viewFullLoading ]

                    Failed _ ->
                        Debug.todo "Implement error"

                    NotFound ->
                        Debug.todo "Implement not found"

            Nothing ->
                viewDefaultAccountRegister translators model
        )
    ]


viewAccountTypeSelector : Translators -> Model -> Html Msg
viewAccountTypeSelector translators model =
    div [ class "flex w-full justify-center" ]
        [ viewAccountTypeRadio
            { type_ = Natural
            , label = translators.t "register.form.types.natural"
            , styles = ""
            , isSelected = model.accountType == Natural
            , onClick = AccountTypeSelected
            }
        , viewAccountTypeRadio
            { type_ = Juridical
            , label = translators.t "register.form.types.juridical"
            , styles = "ml-4"
            , isSelected = model.accountType == Juridical
            , onClick = AccountTypeSelected
            }
        ]


type alias AccountTypeRadioOptions a =
    { type_ : AccountType
    , label : String
    , styles : String
    , isSelected : Bool
    , onClick : AccountType -> a
    }


viewAccountTypeRadio : AccountTypeRadioOptions Msg -> Html Msg
viewAccountTypeRadio options =
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
                Natural ->
                    "natural"

                Juridical ->
                    "juridical"

                {- Chosen by fair dice roll. Guaranteed to be random. -}
                Unspecified ->
                    "4"
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


viewDefaultAccountRegister : Translators -> Model -> List (Html Msg)
viewDefaultAccountRegister translators model =
    [ viewServerErrors model.problems
    , viewTitleForStep translators 1
    ]


viewJuridicalAccountRegister : Translators -> Model -> List (Html Msg)
viewJuridicalAccountRegister translators model =
    let
        formTranslationString =
            "register.form"
    in
    [ viewServerErrors model.problems
    , viewTitleForStep translators 1
    , viewSelectField (translators.t "register.form.company_type")
        model.selectedDocument
        [ { value = "mipyme", label = translators.t "register.form.company.mipyme.label" }
        , { value = "corporation", label = translators.t "register.form.company.corporation.label" }
        ]
        translators
    , documentInput translators model (formTranslationString ++ ".company.")
    , View.Form.Input.init
        { id = "name"
        , label = "Company Name"
        , disabled = False
        , onInput = EnteredDocument
        , placeholder = Just "Ex.: Cambiatus"
        , problems = Nothing
        , translators = translators
        , value = ""
        }
        |> View.Form.Input.toHtml
    , emailInput translators model formTranslationString
    , phoneInput translators model formTranslationString
    , accountInput translators model formTranslationString
    , viewSelectField (translators.t "register.form.state")
        model.selectedDocument
        [ { value = "mipyme", label = translators.t "register.form.company.mipyme.label" }
        , { value = "corporation", label = translators.t "register.form.company.corporation.label" }
        ]
        translators
    , viewSelectField (translators.t "register.form.city")
        model.selectedDocument
        [ { value = "mipyme", label = translators.t "register.form.company.mipyme.label" }
        , { value = "corporation", label = translators.t "register.form.company.corporation.label" }
        ]
        translators
    , viewSelectField (translators.t "register.form.district")
        model.selectedDocument
        [ { value = "mipyme", label = translators.t "register.form.company.mipyme.label" }
        , { value = "corporation", label = translators.t "register.form.company.corporation.label" }
        ]
        translators
    ]


viewNaturalAccountRegister : Translators -> Model -> List (Html Msg)
viewNaturalAccountRegister translators model =
    let
        formTranslationString =
            "register.form"
    in
    [ viewServerErrors model.problems
    , viewTitleForStep translators 1
    , viewSelectField "Document Type"
        model.selectedDocument
        [ { value = "ssn", label = translators.t "register.form.document.ssn.label" }
        , { value = "dimex", label = translators.t "register.form.document.dimex.label" }
        , { value = "nite", label = translators.t "register.form.document.nite.label" }
        ]
        translators
    , documentInput translators model (formTranslationString ++ ".document.")
    , nameInput translators model formTranslationString
    , emailInput translators model formTranslationString
    , phoneInput translators model formTranslationString
    , accountInput translators model formTranslationString
    ]


documentInput : Translators -> Model -> String -> Html Msg
documentInput translators model formTranslationString =
    let
        selectedDocumentTranslationString =
            formTranslationString ++ model.selectedDocument
    in
    View.Form.Input.init
        { id = "document"
        , label = translators.t (selectedDocumentTranslationString ++ ".label")
        , onInput = EnteredDocument
        , disabled = False
        , value = model.documentNumber
        , placeholder = Just (translators.t (selectedDocumentTranslationString ++ ".placeholder"))
        , problems = Nothing
        , translators = translators
        }
        |> View.Form.Input.withCounter
            (selectedDocumentTranslationString
                ++ ".maximum"
                |> translators.t
                |> String.toInt
                |> Maybe.withDefault 10
            )
        |> View.Form.Input.toHtml


nameInput : Translators -> Model -> String -> Html Msg
nameInput translators model formTranslationString =
    View.Form.Input.init
        { id = "name"
        , label = translators.t (formTranslationString ++ ".name.label")
        , onInput = EnteredDocument
        , disabled = False
        , value = model.documentNumber
        , placeholder = Just (translators.t (formTranslationString ++ ".name.placeholder"))
        , problems = Nothing
        , translators = translators
        }
        |> View.Form.Input.toHtml


accountInput : Translators -> Model -> String -> Html Msg
accountInput translators model formTranslationString =
    View.Form.Input.init
        { id = "account"
        , label = translators.t (formTranslationString ++ ".account.label")
        , onInput = EnteredDocument
        , disabled = False
        , value = model.documentNumber
        , placeholder = Just (translators.t (formTranslationString ++ ".account.placeholder"))
        , problems = Nothing
        , translators = translators
        }
        |> View.Form.Input.withCounter 12
        |> View.Form.Input.toHtml


emailInput : Translators -> Model -> String -> Html Msg
emailInput translators model formTranslationString =
    View.Form.Input.init
        { id = "email"
        , label = translators.t (formTranslationString ++ ".email.label")
        , onInput = EnteredDocument
        , disabled = False
        , value = model.documentNumber
        , placeholder = Just (translators.t (formTranslationString ++ ".email.placeholder"))
        , problems = Nothing
        , translators = translators
        }
        |> View.Form.Input.toHtml


phoneInput : Translators -> Model -> String -> Html Msg
phoneInput translators model formTranslationString =
    View.Form.Input.init
        { id = "phone"
        , label = translators.t (formTranslationString ++ ".phone.label")
        , onInput = EnteredDocument
        , disabled = False
        , value = model.documentNumber
        , placeholder = Just (translators.t (formTranslationString ++ ".phone.placeholder"))
        , problems = Nothing
        , translators = translators
        }
        |> View.Form.Input.toHtml


viewSelectField : String -> String -> List { value : String, label : String } -> Translators -> Html Msg
viewSelectField label initialValue options translators =
    let
        form =
            View.Form.Select.init "document_select" label SelectedDocument initialValue
    in
    List.foldl View.Form.Select.withOption form options
        |> View.Form.Select.toHtml


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
    = ValidateForm Form
    | GotAccountAvailabilityResponse Bool
    | AccountGenerated (Result Decode.Error AccountKeys)
    | CompletedCreateProfile AccountKeys (Result Http.Error Profile)
    | AgreedToSave12Words Bool
    | DownloadPdf PdfData
    | PdfDownloaded
    | KeyPressed Bool
    | CopyToClipboard String
    | CopiedToClipboard
    | CompletedLoadInvite (Result (Graphql.Http.Error (Maybe Invite)) (Maybe Invite))
    | AccountTypeSelected AccountType
    | EnteredDocument String
    | SelectedDocument String


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
        AccountTypeSelected type_ ->
            UR.init
                { model
                    | accountType = type_
                    , selectedDocument =
                        case type_ of
                            Natural ->
                                "ssn"

                            Juridical ->
                                "mipyme"

                            Unspecified ->
                                "ssn"
                }

        EnteredDocument document ->
            UR.init { model | documentNumber = document }

        SelectedDocument document ->
            UR.init { model | selectedDocument = document }

        ValidateForm form ->
            let
                isLowerThan6 : Char -> Bool
                isLowerThan6 c =
                    let
                        charInt : Int
                        charInt =
                            c
                                |> String.fromChar
                                |> String.toInt
                                |> Maybe.withDefault 0
                    in
                    compare charInt 6 == Basics.LT

                isValidAlphaNum : Char -> Bool
                isValidAlphaNum c =
                    (Char.isUpper c || Char.isLower c || Char.isDigit c) && isLowerThan6 c

                formValidator =
                    Validate.all
                        [ Validate.firstError
                            [ ifBlank .username (InvalidEntry Username (t "error.required")) ]
                        , Validate.firstError
                            [ ifBlank .email (InvalidEntry Email (t "error.required"))
                            , ifInvalidEmail .email (\_ -> InvalidEntry Email (t "error.email"))
                            ]
                        , Validate.firstError
                            [ ifBlank .account (InvalidEntry Account (t "error.required"))
                            , ifTrue
                                (\f -> String.length f.account < 12)
                                (InvalidEntry Account (tr "error.tooShort" [ ( "minLength", "12" ) ]))
                            , ifTrue
                                (\f -> String.length f.account > 12)
                                (InvalidEntry Account (tr "error.tooLong" [ ( "maxLength", "12" ) ]))
                            , ifFalse
                                (\f -> String.all isValidAlphaNum f.account)
                                (InvalidEntry Account (t "register.form.accountCharError"))
                            ]
                        ]
            in
            case validate formValidator form of
                Ok _ ->
                    { model | isCheckingAccount = True }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = ValidateForm form
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "checkAccountAvailability" )
                                    , ( "accountName", Encode.string model.form.account )
                                    ]
                            }

                Err problems ->
                    { model | problems = problems }
                        |> UR.init

        GotAccountAvailabilityResponse isAvailable ->
            if isAvailable then
                UR.init
                    { model
                        | isLoading = True
                        , problems = []
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
                                , ( "account", Encode.string model.form.account )
                                ]
                        }

            else
                let
                    fieldError =
                        InvalidEntry Account (t "error.alreadyTaken")
                in
                UR.init
                    { model
                        | problems = fieldError :: model.problems
                        , isCheckingAccount = False
                    }

        AccountGenerated (Ok accountKeys) ->
            UR.init
                { model
                    | isLoading = True
                    , accountKeys = Just accountKeys
                    , form = initForm
                }
                |> UR.addCmd
                    (case maybeInvitation of
                        Nothing ->
                            Api.signUp guest.shared
                                { name = model.form.username
                                , email = model.form.email
                                , account = accountKeys.accountName
                                , invitationId = maybeInvitation
                                }
                                (CompletedCreateProfile accountKeys)

                        Just _ ->
                            Api.signUpWithInvitation guest.shared
                                { name = model.form.username
                                , email = model.form.email
                                , account = accountKeys.accountName
                                , invitationId = maybeInvitation
                                }
                                (CompletedCreateProfile accountKeys)
                    )

        AccountGenerated (Err v) ->
            UR.init
                { model
                    | isLoading = False
                    , problems = ServerError "The server acted in an unexpected way" :: model.problems
                }
                |> UR.logDecodeError msg v

        CompletedCreateProfile _ (Ok _) ->
            { model | accountGenerated = True }
                |> UR.init

        CompletedCreateProfile _ (Err err) ->
            { model | problems = ServerError "Auth failed" :: model.problems }
                |> UR.init
                |> UR.logHttpError msg err

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

        KeyPressed isEnter ->
            if isEnter then
                UR.init model
                    |> UR.addCmd
                        (Task.succeed (ValidateForm model.form)
                            |> Task.perform identity
                        )

            else
                UR.init model

        CompletedLoadInvite (Ok (Just invitation)) ->
            UR.init { model | status = Loaded invitation }

        CompletedLoadInvite (Ok Nothing) ->
            UR.init { model | status = NotFound }

        CompletedLoadInvite (Err error) ->
            { model | status = Failed error }
                |> UR.init
                |> UR.logGraphqlError msg error



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

        KeyPressed _ ->
            [ "KeyPressed" ]

        CompletedLoadInvite _ ->
            [ "CompletedLoadInvite" ]

        AccountTypeSelected _ ->
            [ "AccountTypeSelected" ]

        EnteredDocument _ ->
            [ "EnteredDocument" ]

        SelectedDocument _ ->
            [ "SelectedDocument" ]
