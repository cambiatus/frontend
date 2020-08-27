module Page.Register exposing (Model, Msg, init, jsAddressToMsg, msgToString, subscriptions, update, view)

import Api.Graphql
import Auth exposing (SignUpResult, viewFieldLabel)
import Browser.Events
import Char
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Attribute, Html, a, button, div, img, input, label, li, p, span, strong, text, ul)
import Html.Attributes exposing (attribute, checked, class, disabled, id, maxlength, placeholder, src, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Route
import Session.Guest as Guest exposing (External(..))
import Session.Shared exposing (Translators)
import Task
import UpdateResult as UR
import Utils exposing (decodeEnterKeyDown)
import Validate exposing (ifBlank, ifFalse, ifInvalidEmail, ifTrue, validate)



-- INIT


init : Guest.Model -> ( Model, Cmd Msg )
init guest =
    ( initModel guest
    , Cmd.none
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
    }


initModel : Guest.Model -> Model
initModel _ =
    { accountKeys = Nothing
    , isLoading = False
    , isCheckingAccount = False
    , form = initForm
    , hasAgreedToSavePassphrase = False
    , isPassphraseCopiedToClipboard = False
    , accountGenerated = False
    , problems = []
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
    , words : String
    , privateKey : String
    }


decodeAccount : Decoder AccountKeys
decodeAccount =
    Decode.succeed AccountKeys
        |> Decode.required "ownerKey" Decode.string
        |> Decode.required "activeKey" Decode.string
        |> Decode.required "accountName" Eos.nameDecoder
        |> Decode.required "words" Decode.string
        |> Decode.required "privateKey" Decode.string



-- VIEW


view : Guest.Model -> Model -> { title : String, content : Html Msg }
view guest model =
    let
        shared =
            guest.shared

        { t, tr } =
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

        viewTitleForStep : Int -> Html msg
        viewTitleForStep s =
            let
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

        viewCreateAccount =
            div [ class "flex-grow bg-white flex md:block" ]
                [ Html.form
                    [ class "sf-wrapper"
                    , class "px-4 md:max-w-sm md:mx-auto md:pt-20 md:px-0"
                    , onSubmit (ValidateForm model.form)
                    ]
                    [ div [ class "sf-content" ]
                        [ viewServerErrors model.problems
                        , viewTitleForStep 1
                        , viewField
                            shared.translators
                            (Field
                                "register.form.name"
                                isDisabled
                                model.form.username
                                Username
                            )
                            (identity EnteredUsername)
                            [ maxlength 255 ]
                            model.problems
                        , viewField
                            shared.translators
                            (Field
                                "register.form.account"
                                isDisabled
                                model.form.account
                                Account
                            )
                            (identity EnteredAccount)
                            Eos.nameValidationAttrs
                            model.problems
                        , viewField
                            shared.translators
                            (Field
                                "register.form.email"
                                isDisabled
                                model.form.email
                                Email
                            )
                            (identity EnteredEmail)
                            [ attribute "inputmode" "email" ]
                            model.problems
                        ]
                    , div [ class "sf-footer" ]
                        [ p [ class "text-center text-body my-6" ]
                            [ text (t "register.login")
                            , a [ Route.href (Route.Login Nothing), class "text-orange-300 underline" ] [ text (t "register.authLink") ]
                            ]
                        , button
                            [ class "button button-primary min-w-full mb-8"
                            , type_ "submit"
                            , disabled isDisabled
                            ]
                            [ if model.isCheckingAccount then
                                text (t "register.form.checkingAvailability")

                              else
                                text (t "auth.login.continue")
                            ]
                        ]
                    ]
                ]

        viewAccountGenerated =
            div
                [ class "flex-grow bg-purple-500 flex md:block"
                ]
                [ div
                    [ class "sf-wrapper"
                    , class "px-4 md:max-w-sm md:mx-auto md:pt-20 md:px-0 text-white text-body"
                    ]
                    [ div [ class "sf-content" ]
                        [ viewTitleForStep 2
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
        if model.accountGenerated then
            viewAccountGenerated

        else
            viewCreateAccount
    }


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


type alias Field =
    { translationSuffix : String
    , isDisabled : Bool
    , currentValue : String
    , name : ValidatedField
    }


viewField : Translators -> Field -> (String -> FormInputMsg) -> List (Attribute FormInputMsg) -> List Problem -> Html Msg
viewField ({ t, tr } as translators) { translationSuffix, isDisabled, currentValue, name } msg extraAttrs problems =
    let
        isCurrentFieldNameProblem p =
            case p of
                InvalidEntry validatedField _ ->
                    name == validatedField

                _ ->
                    False

        fieldProblems =
            List.filter isCurrentFieldNameProblem problems

        errorClass =
            case fieldProblems of
                [] ->
                    ""

                _ ->
                    "field-with-error"

        viewFieldErrors =
            List.map (viewFieldProblem name) problems

        id_ =
            translationSuffix
    in
    div [ class "mb-10 relative" ]
        [ viewFieldLabel translators translationSuffix id_
        , input
            ([ id id_
             , onInput msg
             , class ("input min-w-full" ++ " " ++ errorClass)
             , disabled isDisabled
             , value currentValue
             , placeholder (t (translationSuffix ++ ".placeholder"))
             ]
                ++ extraAttrs
            )
            []
            |> Html.map UpdateForm
        , case name of
            Account ->
                div [ class "input-label pr-1 text-right text-purple-100 font-bold mt-1 absolute right-0" ]
                    [ text <|
                        tr
                            "edit.input_counter"
                            [ ( "current", String.fromInt <| String.length currentValue )
                            , ( "max", "12" )
                            ]
                    ]

            _ ->
                text ""
        , ul [] viewFieldErrors
        ]


viewFieldProblem : ValidatedField -> Problem -> Html msg
viewFieldProblem field problem =
    case problem of
        ServerError _ ->
            text ""

        InvalidEntry f str ->
            if f == field then
                li [ class "form-error absolute mr-8" ] [ text str ]

            else
                text ""


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
    = UpdateForm FormInputMsg
    | ValidateForm Form
    | GotAccountAvailabilityResponse Bool
    | KeysGenerated (Result Decode.Error AccountKeys)
    | CompletedSignUp AccountKeys (Result (Graphql.Http.Error (Maybe SignUpResult)) (Maybe SignUpResult))
    | AgreedToSave12Words Bool
    | DownloadPdf PdfData
    | PdfDownloaded
    | KeyPressed Bool
    | CopyToClipboard String
    | CopiedToClipboard


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
        UpdateForm subMsg ->
            updateForm subMsg model
                |> UR.init

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
                                [ ( "name", Encode.string "generateKeys" )
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

        KeysGenerated (Ok accountKeys) ->
            UR.init
                { model
                    | isLoading = True
                    , accountKeys = Just accountKeys
                    , form = initForm
                }
                |> UR.addCmd
                    (case maybeInvitation of
                        Nothing ->
                            Api.Graphql.mutation guest.shared
                                (Auth.signUp accountKeys.accountName
                                    model.form.username
                                    model.form.email
                                    accountKeys.ownerKey
                                    Nothing
                                )
                                (CompletedSignUp accountKeys)

                        Just invitationId ->
                            Api.Graphql.mutation guest.shared
                                (Auth.signUp accountKeys.accountName
                                    model.form.username
                                    model.form.email
                                    accountKeys.ownerKey
                                    (Just invitationId)
                                )
                                (CompletedSignUp accountKeys)
                    )

        KeysGenerated (Err v) ->
            UR.init
                { model
                    | isLoading = False
                    , problems = ServerError "Key generation failed" :: model.problems
                }
                |> UR.logDecodeError msg v

        CompletedSignUp _ (Ok _) ->
            { model | accountGenerated = True }
                |> UR.init

        CompletedSignUp _ (Err err) ->
            { model | problems = ServerError "Auth failed" :: model.problems }
                |> UR.init
                |> UR.logGraphqlError msg err

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



--
-- Model functions
--


type FormInputMsg
    = EnteredUsername String
    | EnteredEmail String
    | EnteredAccount String


updateForm : FormInputMsg -> Model -> Model
updateForm msg ({ form } as model) =
    let
        updateModel newForm =
            { model
                | form = newForm
                , problems = []
            }
    in
    case msg of
        EnteredUsername str ->
            updateModel { form | username = str }

        EnteredEmail str ->
            updateModel { form | email = str }

        EnteredAccount str ->
            updateModel { form | account = str |> String.toLower }


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
                |> KeysGenerated
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
        UpdateForm _ ->
            [ "UpdateForm" ]

        ValidateForm _ ->
            [ "ValidateForm" ]

        GotAccountAvailabilityResponse _ ->
            [ "GotAccountAvailabilityResponse" ]

        KeysGenerated r ->
            [ "KeysGenerated", UR.resultToString r ]

        CompletedSignUp _ _ ->
            [ "CompletedSignUp" ]

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
