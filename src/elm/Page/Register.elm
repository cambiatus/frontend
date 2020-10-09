module Page.Register exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Address
import Api.Graphql
import Cambiatus.Enum.SignUpStatus as SignUpStatus
import Cambiatus.InputObject as InputObject
import Cambiatus.Mutation as Mutation
import Cambiatus.Object.SignUp
import Cambiatus.Scalar exposing (Id(..))
import Community exposing (Invite)
import Eos.Account as Eos
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (with)
import Html exposing (Html, a, button, div, img, input, label, p, span, strong, text)
import Html.Attributes exposing (checked, class, disabled, for, id, src, style, type_, value)
import Html.Events exposing (onCheck, onClick, onSubmit)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Page
import Page.Register.DefaultForm as DefaultForm
import Page.Register.JuridicalForm as JuridicalForm
import Page.Register.NaturalForm as NaturalForm
import Result
import Route
import Session.Guest as Guest exposing (External(..))
import Session.Shared exposing (Shared, Translators)
import UpdateResult as UR
import Validate
import View.Form



-- INIT


init : InvitationId -> Guest.Model -> ( Model, Cmd Msg )
init invitationId { shared } =
    let
        initialStatus =
            case invitationId of
                Just _ ->
                    Loading

                Nothing ->
                    FormShowed
                        (DefaultForm DefaultForm.init)

        initialModel =
            { accountKeys = Nothing
            , hasAgreedToSavePassphrase = False
            , isPassphraseCopiedToClipboard = False
            , serverError = Nothing
            , status = initialStatus
            , invitationId = invitationId
            , invitation = Nothing
            , country = Nothing
            , step = 1
            }

        loadInvitationData =
            case invitationId of
                Just id ->
                    Api.Graphql.query
                        shared
                        (Community.inviteQuery id)
                        CompletedLoadInvite

                Nothing ->
                    Cmd.none
    in
    ( initialModel
    , loadInvitationData
    )



-- MODEL


type alias Model =
    { accountKeys : Maybe AccountKeys
    , hasAgreedToSavePassphrase : Bool
    , isPassphraseCopiedToClipboard : Bool
    , serverError : ServerError
    , status : Status
    , invitationId : InvitationId
    , invitation : Maybe Invite
    , country : Maybe Address.Country
    , step : Int
    }


type alias SignUpFields =
    { name : String
    , email : String
    , account : String
    }


type alias ServerError =
    Maybe String


type alias InvitationId =
    Maybe String


type AccountType
    = NaturalAccount
    | JuridicalAccount Address.Country


type FormModel
    = NaturalForm NaturalForm.Model
    | JuridicalForm JuridicalForm.Model
    | DefaultForm DefaultForm.Model



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
view { shared } model =
    let
        { t } =
            shared.translators
    in
    { title =
        t "register.registerTab"
    , content =
        viewCreateAccount shared.translators model
    }


viewAccountGenerated : Translators -> Model -> AccountKeys -> Html Msg
viewAccountGenerated { t } model keys =
    let
        name =
            Eos.nameToString keys.accountName

        passphraseTextId =
            "passphraseText"

        passphraseInputId =
            -- Passphrase text is duplicated in `input:text` to be able to copy via Browser API
            "passphraseWords"
    in
    div
        [ class "flex-grow bg-purple-500 flex md:block"
        ]
        [ div
            [ class "sf-wrapper"
            , class "px-4 md:max-w-sm md:mx-auto md:pt-20 md:px-0 text-white text-body"
            ]
            [ div [ class "sf-content" ]
                [ p
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
                    [ img [ src "/images/reg-passphrase-boy.svg" ]
                        []
                    , img
                        [ class "absolute w-1/4 -mt-2 -ml-10"
                        , src "/images/reg-passphrase-boy-hand.svg"
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
                        [ span [ id passphraseTextId ] [ text keys.words ]
                        , input
                            -- We use `HTMLInputElement.select()` method in port to select and copy the text. This method
                            -- works only with `input` and `textarea` elements which has to be presented in DOM (e.g. we can't
                            -- hide it with `display: hidden`), so we hide it using position and opacity.
                            [ type_ "text"
                            , class "absolute opacity-0"
                            , style "left" "-9999em"
                            , id passphraseInputId
                            , value keys.words
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
                , button
                    [ onClick <| DownloadPdf (pdfData keys)
                    , class "button button-primary w-full mb-8"
                    , disabled (not model.hasAgreedToSavePassphrase)
                    , class <|
                        if model.hasAgreedToSavePassphrase then
                            ""

                        else
                            "button-disabled text-gray-600"
                    ]
                    [ text (t "register.account_created.download") ]
                ]
            ]
        ]


viewCreateAccount : Translators -> Model -> Html Msg
viewCreateAccount translators model =
    let
        backgroundColor =
            case model.step of
                2 ->
                    "bg-purple-500"

                _ ->
                    "bg-white"
    in
    div [ class ("flex flex-grow flex-col " ++ backgroundColor) ]
        [ viewTitleForStep translators model.step
        , case model.status of
            Loading ->
                viewLoading

            AccountTypeSelectorShowed ->
                div []
                    [ viewFormTypeSelector translators model
                    , viewFooter translators False
                    ]

            FormShowed formModel ->
                let
                    viewServerError : ServerError -> Html msg
                    viewServerError error =
                        case error of
                            Just message ->
                                div [ class "bg-red border-lg rounded p-4 mt-2 text-white" ]
                                    [ text message ]

                            Nothing ->
                                text ""
                in
                Html.form
                    [ class "flex flex-grow flex-col bg-white px-4 px-0 md:max-w-sm sf-wrapper self-center w-full"
                    , onSubmit (ValidateForm formModel)
                    ]
                    [ viewServerError model.serverError
                    , case formModel of
                        DefaultForm _ ->
                            text ""

                        _ ->
                            viewFormTypeSelector translators model
                    , div [ class "sf-content" ]
                        [ viewRegistrationForm translators formModel
                        , viewFooter translators True
                        ]
                    ]

            Generated keys ->
                --viewAccountGenerated translators model keys
                text "Signin up!"

            ErrorShowed _ ->
                Page.fullPageNotFound (translators.t "error.unknown") ""

            NotFound ->
                Page.fullPageNotFound (translators.t "error.pageNotFound") ""
        ]


viewFooter : Translators -> Bool -> Html msg
viewFooter translators isSubmitEnabled =
    div [ class "mt-auto flex flex-col justify-between items-center h-32" ]
        [ span []
            [ text (translators.t "register.login")
            , a [ class "underline text-orange-300", Route.href (Route.Login Nothing) ] [ text (translators.t "register.authLink") ]
            ]
        , viewSubmitButton isSubmitEnabled translators
        ]


viewRegistrationForm : Translators -> FormModel -> Html Msg
viewRegistrationForm translators currentForm =
    case currentForm of
        NaturalForm form ->
            NaturalForm.view translators form
                |> Html.map NaturalFormMsg
                |> Html.map FormMsg

        JuridicalForm form ->
            JuridicalForm.view translators form
                |> Html.map JuridicalFormMsg
                |> Html.map FormMsg

        DefaultForm form ->
            DefaultForm.view translators form
                |> Html.map DefaultFormMsg
                |> Html.map FormMsg


viewSubmitButton : Bool -> Translators -> Html msg
viewSubmitButton isEnabled translators =
    button
        [ class "button w-full mb-4"
        , class
            (if isEnabled then
                "button-primary"

             else
                "button-disabled"
            )
        , disabled (not isEnabled)
        ]
        [ text (translators.t "auth.login.continue") ]


viewFormTypeSelector : Translators -> Model -> Html Msg
viewFormTypeSelector translators model =
    case ( model.invitation, model.country ) of
        ( Just _, Just country ) ->
            div []
                [ View.Form.primaryLabel "radio" (translators.t "register.form.register_tooltip")
                , div [ class "flex w-full justify-center" ]
                    [ viewFormTypeRadio
                        { type_ = NaturalAccount
                        , label = translators.t "register.form.types.natural"
                        , styles = ""
                        , isSelected =
                            case model.status of
                                FormShowed (NaturalForm _) ->
                                    True

                                _ ->
                                    False
                        , onClick = AccountTypeSelected
                        }
                    , viewFormTypeRadio
                        { type_ = JuridicalAccount country
                        , label = translators.t "register.form.types.juridical"
                        , styles = "ml-1"
                        , isSelected =
                            case model.status of
                                FormShowed (JuridicalForm _) ->
                                    True

                                _ ->
                                    False
                        , onClick = AccountTypeSelected
                        }
                    ]
                ]

        _ ->
            text ""


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
            "w-40 h-10 rounded-sm flex justify-center items-center cursor-pointer mb-4 "

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

                JuridicalAccount _ ->
                    "juridical"
    in
    div
        [ class (finalClasses ++ options.styles)
        , onClick (options.onClick options.type_)
        , style "width" "169px"
        , style "height" "44px"
        ]
        [ label [ class "cursor-pointer", for id ] [ text options.label ]
        , input [ class "hidden", type_ "radio", checked options.isSelected, onClick (options.onClick options.type_) ] []
        ]


{-| Loading screen on this page differs from `Session.Shared.viewFullLoading`
since we don't need full width here.
-}
viewLoading : Html msg
viewLoading =
    div [ class "h-full full-spinner-container" ]
        [ div [ class "spinner" ] [] ]


viewTitleForStep : Translators -> Int -> Html msg
viewTitleForStep { t, tr } s =
    let
        step =
            String.fromInt s
    in
    p
        [ class "ml-4 py-4 mb-4 text-body border-b border-dotted text-grey border-grey-500" ]
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


pdfData : AccountKeys -> PdfData
pdfData keys =
    { passphrase = keys.words
    , accountName = Eos.nameToString keys.accountName
    }



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg External


type Msg
    = ValidateForm FormModel
    | GotAccountAvailabilityResponse Bool
    | AccountGenerated (Result Decode.Error AccountKeys)
    | AgreedToSave12Words Bool
    | DownloadPdf PdfData
    | PdfDownloaded
    | CopyToClipboard String
    | CopiedToClipboard
    | CompletedLoadInvite (Result (Graphql.Http.Error (Maybe Invite)) (Maybe Invite))
    | CompletedLoadCountry (Result (Graphql.Http.Error (Maybe Address.Country)) (Maybe Address.Country))
    | AccountTypeSelected AccountType
    | FormMsg EitherFormMsg
    | CompletedSignUp (Result (Graphql.Http.Error SignUpResponse) SignUpResponse)
    | CompletedKycUpsert (Result (Graphql.Http.Error (Maybe ())) (Maybe ()))
    | CompletedAddressUpsert (Result (Graphql.Http.Error (Maybe ())) (Maybe ()))


type EitherFormMsg
    = JuridicalFormMsg JuridicalForm.Msg
    | NaturalFormMsg NaturalForm.Msg
    | DefaultFormMsg DefaultForm.Msg


type Status
    = Loading
    | AccountTypeSelectorShowed
    | FormShowed FormModel
    | ErrorShowed Error
    | NotFound
    | Generated AccountKeys


type Error
    = FailedInvite (Graphql.Http.Error (Maybe Invite))
    | FailedCountry (Graphql.Http.Error (Maybe Address.Country))


type alias PdfData =
    { passphrase : String
    , accountName : String
    }


getSignUpFields : FormModel -> SignUpFields
getSignUpFields form =
    let
        extract : { f | account : String, name : String, email : String } -> SignUpFields
        extract f =
            { account = f.account
            , name = f.name
            , email = f.email
            }
    in
    case form of
        JuridicalForm f ->
            extract f

        NaturalForm f ->
            extract f

        DefaultForm f ->
            extract f


update : InvitationId -> Msg -> Model -> Guest.Model -> UpdateResult
update _ msg model { shared } =
    let
        { t } =
            shared.translators
    in
    case msg of
        ValidateForm formType ->
            let
                validateForm validator form =
                    case Validate.validate validator form of
                        Ok _ ->
                            []

                        Err err ->
                            err

                account =
                    getSignUpFields formType
                        |> .account

                _ =
                    Debug.log "account" account

                translators =
                    shared.translators

                problemCount =
                    case formType of
                        JuridicalForm form ->
                            List.length (validateForm (JuridicalForm.validator translators) form)

                        NaturalForm form ->
                            List.length (validateForm (NaturalForm.validator translators) form)

                        DefaultForm form ->
                            List.length (validateForm (DefaultForm.validator translators) form)

                afterValidationAction =
                    if problemCount > 0 then
                        UR.addCmd Cmd.none

                    else
                        UR.addPort
                            { responseAddress = ValidateForm formType
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "checkAccountAvailability" )
                                    , ( "account", Encode.string account )
                                    ]
                            }
            in
            { model
                | status =
                    case formType of
                        JuridicalForm form ->
                            JuridicalForm
                                { form
                                    | problems = validateForm (JuridicalForm.validator translators) form
                                }
                                |> FormShowed

                        NaturalForm form ->
                            NaturalForm
                                { form
                                    | problems = validateForm (NaturalForm.validator translators) form
                                }
                                |> FormShowed

                        DefaultForm form ->
                            DefaultForm
                                { form
                                    | problems = validateForm (DefaultForm.validator translators) form
                                }
                                |> FormShowed
            }
                |> UR.init
                |> afterValidationAction

        FormMsg formMsg ->
            case ( formMsg, model.status ) of
                ( JuridicalFormMsg innerMsg, FormShowed (JuridicalForm form) ) ->
                    { model
                        | status =
                            JuridicalForm.update innerMsg form shared.translators
                                |> JuridicalForm
                                |> FormShowed
                    }
                        |> UR.init

                ( NaturalFormMsg innerMsg, FormShowed (NaturalForm form) ) ->
                    { model
                        | status =
                            NaturalForm.update innerMsg form
                                |> NaturalForm
                                |> FormShowed
                    }
                        |> UR.init

                ( DefaultFormMsg innerMsg, FormShowed (DefaultForm form) ) ->
                    { model
                        | status =
                            DefaultForm.update innerMsg form
                                |> DefaultForm
                                |> FormShowed
                    }
                        |> UR.init

                _ ->
                    UR.init model

        AccountTypeSelected type_ ->
            let
                selectedKycForm =
                    case type_ of
                        NaturalAccount ->
                            NaturalForm
                                (NaturalForm.init
                                    { account = Nothing
                                    , email = Nothing
                                    , phone = Nothing
                                    }
                                )

                        JuridicalAccount country ->
                            JuridicalForm
                                (JuridicalForm.init
                                    { account = Nothing
                                    , email = Nothing
                                    , phone = Nothing
                                    , country = country
                                    }
                                    shared.translators
                                )
            in
            { model | status = FormShowed selectedKycForm }
                |> UR.init

        GotAccountAvailabilityResponse isAvailable ->
            if isAvailable then
                model
                    |> UR.init
                    |> UR.addPort
                        { responseAddress = GotAccountAvailabilityResponse isAvailable
                        , responseData = Encode.null
                        , data =
                            Encode.object
                                [ ( "name", Encode.string "generateKeys" )

                                -- TODO: Remove this
                                , ( "account"
                                  , Encode.string
                                        (case model.status of
                                            FormShowed (JuridicalForm form) ->
                                                form.account

                                            FormShowed (NaturalForm form) ->
                                                form.account

                                            FormShowed (DefaultForm form) ->
                                                form.account

                                            _ ->
                                                ""
                                        )
                                  )
                                ]
                        }

            else
                UR.init
                    { model
                        | status =
                            case model.status of
                                FormShowed (JuridicalForm form) ->
                                    JuridicalForm
                                        { form
                                            | problems = ( JuridicalForm.Account, t "error.alreadyTaken" ) :: form.problems
                                        }
                                        |> FormShowed

                                FormShowed (NaturalForm form) ->
                                    NaturalForm { form | problems = ( NaturalForm.Account, t "error.alreadyTaken" ) :: form.problems }
                                        |> FormShowed

                                FormShowed (DefaultForm form) ->
                                    DefaultForm { form | problems = ( DefaultForm.Account, t "error.alreadyTaken" ) :: form.problems }
                                        |> FormShowed

                                _ ->
                                    model.status
                    }

        AccountGenerated (Err v) ->
            UR.init
                model
                |> UR.logDecodeError msg v

        AccountGenerated (Ok accountKeys) ->
            case model.status of
                FormShowed f ->
                    let
                        signUpFields =
                            getSignUpFields f

                        _ =
                            Debug.log "keys, fields" ( accountKeys, signUpFields )
                    in
                    { model
                        | status = Generated accountKeys
                        , step = 2
                    }
                        |> UR.init
                        |> UR.addCmd
                            (signUp shared
                                signUpFields
                                accountKeys
                                model.invitationId
                            )

                --Cmd.none
                _ ->
                    model
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
            model
                |> UR.init
                |> UR.addCmd
                    --(case model.status of
                    --    Generated keys ->
                    --        signUp shared
                    --            model.signUpFields
                    --            keys
                    --            model.invitationId
                    --
                    --    _ ->
                    --        Cmd.none
                    --)
                    Cmd.none

        CompletedSignUp (Ok response) ->
            let
                _ =
                    Debug.log "CompleteSignUp" response
            in
            case response.status of
                SignUpStatus.Success ->
                    let
                        kycFormData : Maybe InputObject.KycDataUpdateInputRequiredFields
                        kycFormData =
                            case model.status of
                                FormShowed (NaturalForm form) ->
                                    Just
                                        { accountId = form.account
                                        , countryId = Id "1"
                                        , document = form.document
                                        , documentType = NaturalForm.documentTypeToString form.documentType
                                        , phone = form.phone
                                        , userType = "natural"
                                        }

                                FormShowed (JuridicalForm form) ->
                                    Just
                                        { accountId = form.account
                                        , countryId = Id "1"
                                        , document = form.document
                                        , documentType = JuridicalForm.companyTypeToString form.companyType
                                        , phone = form.phone
                                        , userType = "juridical"
                                        }

                                _ ->
                                    Nothing
                    in
                    model
                        |> UR.init
                        |> UR.addCmd
                            (case kycFormData of
                                Just data ->
                                    saveKycData shared data

                                Nothing ->
                                    redirectToLogin shared
                            )

                SignUpStatus.Error ->
                    UR.init { model | serverError = Just (t "error.unknown") }

        CompletedSignUp (Err err) ->
            let
                _ =
                    Debug.log "completed signup with error" err
            in
            UR.init { model | serverError = Just (t "error.unknown") }

        CompletedKycUpsert (Ok _) ->
            let
                addressData : Maybe ( InputObject.AddressUpdateInputRequiredFields, InputObject.AddressUpdateInputOptionalFields )
                addressData =
                    case model.status of
                        FormShowed (JuridicalForm form) ->
                            Just
                                ( { accountId = form.account
                                  , cityId = Tuple.first form.city |> Id
                                  , countryId = Id "1"
                                  , neighborhoodId = Tuple.first form.district |> Id
                                  , stateId = Tuple.first form.state |> Id
                                  , street = form.street
                                  , zip = form.zip
                                  }
                                , { number = Graphql.OptionalArgument.Present form.number }
                                )

                        _ ->
                            Nothing
            in
            model
                |> UR.init
                |> UR.addCmd
                    (case addressData of
                        Just data ->
                            saveAddress shared data

                        Nothing ->
                            redirectToLogin shared
                    )

        CompletedKycUpsert (Err _) ->
            UR.init { model | serverError = Just (t "error.unknown") }

        CompletedAddressUpsert (Ok _) ->
            model
                |> UR.init
                |> UR.addCmd
                    -- TODO: PDF should be shown only here, after KYC and Address has been added
                    (redirectToLogin shared)

        CompletedAddressUpsert (Err _) ->
            UR.init { model | serverError = Just (t "error.unknown") }

        CompletedLoadInvite (Ok (Just invitation)) ->
            if invitation.community.hasKyc then
                let
                    loadCountryData =
                        Api.Graphql.query
                            shared
                            (Address.countryQuery "Costa Rica")
                            CompletedLoadCountry
                in
                { model
                    | status = Loading
                    , invitation = Just invitation
                }
                    |> UR.init
                    |> UR.addCmd
                        loadCountryData

            else
                { model
                    | status = FormShowed (DefaultForm DefaultForm.init)
                }
                    |> UR.init

        CompletedLoadInvite (Ok Nothing) ->
            UR.init { model | status = NotFound }

        CompletedLoadInvite (Err error) ->
            { model
                | status = ErrorShowed (FailedInvite error)
                , serverError = Just (t "error.unknown")
            }
                |> UR.init
                |> UR.logGraphqlError msg error

        CompletedLoadCountry (Ok (Just country)) ->
            { model
                | country = Just country
                , status = AccountTypeSelectorShowed
            }
                |> UR.init

        CompletedLoadCountry (Ok Nothing) ->
            UR.init { model | status = NotFound }

        CompletedLoadCountry (Err error) ->
            { model | status = ErrorShowed (FailedCountry error) }
                |> UR.init
                |> UR.logGraphqlError msg error


type alias SignUpResponse =
    { reason : String
    , status : SignUpStatus.SignUpStatus
    }


signUp : Shared -> SignUpFields -> AccountKeys -> InvitationId -> Cmd Msg
signUp shared signUpFields keys invitationId =
    let
        requiredArgs =
            { account = Eos.nameToString keys.accountName
            , email = signUpFields.email
            , name = signUpFields.name
            , publicKey = keys.ownerKey
            }

        fillOptionals opts =
            { opts
                | invitationId =
                    Maybe.map Present invitationId
                        |> Maybe.withDefault Absent
            }

        _ =
            Debug.log "required, optional" ( requiredArgs, fillOptionals )
    in
    Api.Graphql.mutation shared
        (Mutation.signUp
            { input =
                InputObject.buildSignUpInput
                    requiredArgs
                    fillOptionals
            }
            (Graphql.SelectionSet.succeed SignUpResponse
                |> with Cambiatus.Object.SignUp.reason
                |> with Cambiatus.Object.SignUp.status
            )
        )
        CompletedSignUp


redirectToLogin : Shared -> Cmd Msg
redirectToLogin shared =
    Route.replaceUrl shared.navKey (Route.Login Nothing)


saveKycData : Shared -> InputObject.KycDataUpdateInputRequiredFields -> Cmd Msg
saveKycData shared requiredFields =
    Api.Graphql.mutation shared
        (Mutation.upsertKyc
            { input = InputObject.buildKycDataUpdateInput requiredFields }
            Graphql.SelectionSet.empty
        )
        CompletedKycUpsert


saveAddress :
    Shared
    -> ( InputObject.AddressUpdateInputRequiredFields, InputObject.AddressUpdateInputOptionalFields )
    -> Cmd Msg
saveAddress shared ( required, optional ) =
    Api.Graphql.mutation shared
        (Mutation.upsertAddress
            { input = InputObject.buildAddressUpdateInput required (\_ -> optional) }
            Graphql.SelectionSet.empty
        )
        CompletedAddressUpsert



-- INTEROP


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ValidateForm" :: _ ->
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

        CompletedSignUp _ ->
            [ "CompletedSignUp" ]

        CompletedLoadCountry _ ->
            [ "CompletedLoadCountry" ]

        CompletedKycUpsert _ ->
            [ "CompletedKycUpsert" ]

        CompletedAddressUpsert _ ->
            [ "CompletedAddressUpsert" ]
