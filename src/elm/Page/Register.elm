module Page.Register exposing (Model, Msg, init, jsAddressToMsg, msgToString, receiveBroadcast, update, view)

import Address
import Api.Graphql
import Cambiatus.Mutation as Mutation
import Cambiatus.Object.Session
import Cambiatus.Scalar exposing (Id(..))
import Community exposing (Invite)
import Eos.Account as Eos
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (with)
import Html exposing (Html, a, button, div, img, p, span, strong, text)
import Html.Attributes exposing (class, disabled, id, src)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode
import Page
import Page.Register.Common exposing (ProblemEvent(..))
import Page.Register.DefaultForm as DefaultForm
import Page.Register.JuridicalForm as JuridicalForm
import Page.Register.NaturalForm as NaturalForm
import Profile
import RemoteData exposing (RemoteData)
import Result
import Route
import Session.Guest as Guest exposing (External(..))
import Session.Shared exposing (Shared, Translators)
import UpdateResult as UR
import Validate
import View.Feedback as Feedback
import View.Form
import View.Form.Checkbox as Checkbox
import View.Form.Input as Input



-- INIT


init : InvitationId -> Guest.Model -> ( Model, Cmd Msg )
init invitationId ({ shared } as guest) =
    let
        initialModel =
            { accountKeys = Nothing
            , hasAgreedToSavePassphrase = False
            , isPassphraseCopiedToClipboard = False
            , status = Loading
            , invitationId = invitationId
            , invitation = Nothing
            , country = Nothing
            , step = FillForm
            }

        loadInvitationData =
            case invitationId of
                Just id ->
                    Api.Graphql.query shared
                        Nothing
                        (Community.inviteQuery id)
                        CompletedLoadInvite

                Nothing ->
                    Cmd.none
    in
    ( initialModel
    , Cmd.batch
        [ loadInvitationData
        , Guest.maybeInitWith CompletedLoadCommunity .community guest
        ]
    )



-- MODEL


type alias Model =
    { accountKeys : Maybe AccountKeys
    , hasAgreedToSavePassphrase : Bool
    , isPassphraseCopiedToClipboard : Bool
    , status : Status
    , invitationId : InvitationId
    , invitation : Maybe Invite
    , country : Maybe Address.Country
    , step : Step
    }


type Status
    = Loading
    | AccountTypeSelectorShowed
    | FormShowed FormModel
    | AccountCreated
    | ErrorShowed
    | NotFound


type Step
    = FillForm
    | SavePassphrase


type FormModel
    = NaturalForm NaturalForm.Model
    | JuridicalForm JuridicalForm.Model
    | DefaultForm DefaultForm.Model


type alias SignUpFields =
    { name : String
    , email : String
    , account : String
    }


type alias PdfData =
    { passphrase : String
    , accountName : String
    }


type alias InvitationId =
    Maybe String


type AccountType
    = NaturalAccount
    | JuridicalAccount Address.Country



---- ACCOUNT KEYS


type alias AccountKeys =
    { ownerKey : String
    , activeKey : String
    , accountName : Eos.Name
    , words : String
    , privateKey : String
    }



-- VIEW


view : Guest.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as guest) model =
    let
        { t } =
            shared.translators

        content =
            case guest.community of
                RemoteData.Success community ->
                    let
                        hasInvite =
                            case model.invitation of
                                Just _ ->
                                    True

                                Nothing ->
                                    False
                    in
                    if community.hasAutoInvite || hasInvite then
                        viewCreateAccount shared.translators model

                    else
                        Page.fullPageLoading shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading shared

                RemoteData.Loading ->
                    Page.fullPageLoading shared

                RemoteData.Failure e ->
                    Page.fullPageGraphQLError (t "register.registerTab") e
    in
    { title = t "register.registerTab"
    , content = content
    }


viewCreateAccount : Translators -> Model -> Html Msg
viewCreateAccount translators model =
    let
        formClasses =
            "flex flex-grow flex-col bg-white px-4 px-0 md:max-w-sm sf-wrapper self-center w-full"

        backgroundColor =
            case model.step of
                FillForm ->
                    "bg-white"

                SavePassphrase ->
                    "bg-purple-500"
    in
    div
        [ -- `id` is used for scrolling into view if error happens
          id "registrationPage"
        , class ("flex flex-grow flex-col " ++ backgroundColor)
        ]
        [ viewTitleForStep translators model.step
        , case model.status of
            Loading ->
                div [ class "h-full full-spinner-container" ]
                    [ div [ class "spinner spinner-light" ] [] ]

            AccountTypeSelectorShowed ->
                div [ class formClasses ]
                    [ viewAccountTypeSelector translators model
                    , viewFooterDisabled translators
                    ]

            FormShowed formModel ->
                Html.form
                    [ class formClasses
                    , onSubmit (ValidateForm formModel)
                    ]
                    [ viewAccountTypeSelector translators model
                    , div [ class "sf-content" ]
                        [ viewRegistrationForm translators formModel ]
                    , viewFooterEnabled translators
                    ]

            AccountCreated ->
                case model.accountKeys of
                    Just keys ->
                        viewAccountCreated translators model keys

                    Nothing ->
                        -- TODO: This should never happen
                        text ""

            ErrorShowed ->
                Page.fullPageNotFound (translators.t "error.unknown") ""

            NotFound ->
                Page.fullPageNotFound (translators.t "error.pageNotFound") ""
        ]


viewFooterEnabled : Translators -> Html msg
viewFooterEnabled translators =
    viewFooter translators True


viewFooterDisabled : Translators -> Html msg
viewFooterDisabled translators =
    viewFooter translators False


viewFooter : Translators -> Bool -> Html msg
viewFooter { t } isSubmitEnabled =
    let
        viewSubmitButton =
            button
                [ class "button w-full mb-4"
                , class
                    (if isSubmitEnabled then
                        "button-primary"

                     else
                        "button-disabled"
                    )
                , disabled (not isSubmitEnabled)
                ]
                [ text (t "auth.login.continue") ]
    in
    div [ class "mt-auto flex flex-col justify-between items-center h-32" ]
        [ span []
            [ text (t "register.login")
            , a
                [ class "underline text-orange-300"
                , Route.href (Route.Login Nothing)
                ]
                [ text (t "register.authLink") ]
            ]
        , viewSubmitButton
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


viewAccountTypeSelector : Translators -> Model -> Html Msg
viewAccountTypeSelector translators model =
    case model.country of
        Just country ->
            div []
                [ View.Form.primaryLabel "radio" (translators.t "register.form.register_tooltip")
                , div [ class "flex space-x-2" ]
                    [ viewAccountTypeButton
                        (translators.t "register.form.types.natural")
                        NaturalAccount
                        model.status
                    , viewAccountTypeButton
                        (translators.t "register.form.types.juridical")
                        (JuridicalAccount country)
                        model.status
                    ]
                ]

        _ ->
            text ""


viewAccountTypeButton : String -> AccountType -> Status -> Html Msg
viewAccountTypeButton title accountType status =
    let
        isSelected =
            case ( accountType, status ) of
                ( NaturalAccount, FormShowed (NaturalForm _) ) ->
                    True

                ( JuridicalAccount _, FormShowed (JuridicalForm _) ) ->
                    True

                _ ->
                    False
    in
    div
        [ class "w-1/2 leading-10 text-center cursor-pointer rounded-sm cursor-pointer mb-4"
        , class
            (if isSelected then
                "bg-orange-300 text-white"

             else
                "bg-gray-100 text-black"
            )
        , onClick (AccountTypeSelected accountType)
        ]
        [ text title ]


viewAccountCreated : Translators -> Model -> AccountKeys -> Html Msg
viewAccountCreated ({ t } as translators) model keys =
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
                        [ span [ class "select-all", id passphraseTextId ] [ text keys.words ]

                        -- We use `HTMLInputElement.select()` method in port to select and copy the text. This method
                        -- works only with `input` and `textarea` elements which has to be presented in DOM (e.g. we can't
                        -- hide it with `display: hidden`), so we hide it using position and opacity.
                        , Input.init
                            { label = ""
                            , id = passphraseInputId
                            , onInput = \_ -> NoOp
                            , disabled = False
                            , value = keys.words
                            , placeholder = Nothing
                            , problems = Nothing
                            , translators = translators
                            }
                            |> Input.withAttrs [ class "absolute opacity-0 left-[-9999em]" ]
                            |> Input.withContainerAttrs [ class "mb-0" ]
                            |> Input.toHtml
                        ]
                    , button
                        [ class "button m-auto button-primary button-sm"
                        , onClick <| CopyToClipboard passphraseInputId
                        ]
                        [ text (t "register.account_created.copy") ]
                    ]
                ]
            , div [ class "sf-footer" ]
                [ Checkbox.init
                    { description = text (t "register.account_created.i_saved_words" ++ " 💜")
                    , id = "agreed_save_passphrase"
                    , value = model.hasAgreedToSavePassphrase
                    , disabled = False
                    , onCheck = AgreedToSave12Words
                    }
                    |> Checkbox.withContainerAttrs [ class "my-4" ]
                    |> Checkbox.toHtml
                , button
                    [ onClick <|
                        DownloadPdf
                            { passphrase = keys.words
                            , accountName = Eos.nameToString keys.accountName
                            }
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


viewTitleForStep : Translators -> Step -> Html msg
viewTitleForStep { t, tr } s =
    let
        stepNum =
            case s of
                FillForm ->
                    "1"

                SavePassphrase ->
                    "2"
    in
    p
        [ class "ml-4 py-4 mb-4 text-body border-b border-dotted text-grey border-grey-500" ]
        [ text (tr "register.form.step" [ ( "stepNum", stepNum ) ])
        , text " / "
        , strong
            [ class <|
                case s of
                    FillForm ->
                        "text-black"

                    SavePassphrase ->
                        "text-white"
            ]
            [ text <| t ("register.form.step" ++ stepNum ++ "_title") ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg External


type Msg
    = NoOp
    | CompletedLoadCommunity Community.CommunityPreview
    | ValidateForm FormModel
    | GotAccountAvailabilityResponse Bool
    | AccountKeysGenerated (Result Decode.Error AccountKeys)
    | AgreedToSave12Words Bool
    | DownloadPdf PdfData
    | PdfDownloaded
    | CopyToClipboard String
    | CopiedToClipboard
    | CompletedLoadInvite (RemoteData (Graphql.Http.Error (Maybe Invite)) (Maybe Invite))
    | CompletedLoadCountry (RemoteData (Graphql.Http.Error (Maybe Address.Country)) (Maybe Address.Country))
    | AccountTypeSelected AccountType
    | FormMsg EitherFormMsg
    | CompletedSignUp (RemoteData (Graphql.Http.Error (Maybe SignUpResponse)) (Maybe SignUpResponse))


type EitherFormMsg
    = JuridicalFormMsg JuridicalForm.Msg
    | NaturalFormMsg NaturalForm.Msg
    | DefaultFormMsg DefaultForm.Msg


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

        scrollTop =
            UR.addPort
                { responseAddress = NoOp
                , responseData = Encode.null
                , data =
                    Encode.object
                        [ ( "id", Encode.string "registrationPage" )
                        , ( "name", Encode.string "scrollIntoView" )
                        ]
                }
    in
    case msg of
        NoOp ->
            model |> UR.init

        CompletedLoadCommunity community ->
            let
                canRegister =
                    community.hasAutoInvite || model.invitationId /= Nothing
            in
            if canRegister then
                loadedCommunity shared model community model.invitation

            else
                UR.init model
                    |> UR.addCmd (Route.pushUrl shared.navKey Route.Join)

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
                            JuridicalForm.update shared.translators innerMsg form
                                |> JuridicalForm
                                |> FormShowed
                    }
                        |> UR.init

                ( NaturalFormMsg innerMsg, FormShowed (NaturalForm form) ) ->
                    { model
                        | status =
                            NaturalForm.update shared.translators innerMsg form
                                |> NaturalForm
                                |> FormShowed
                    }
                        |> UR.init

                ( DefaultFormMsg innerMsg, FormShowed (DefaultForm form) ) ->
                    { model
                        | status =
                            DefaultForm.update shared.translators innerMsg form
                                |> DefaultForm
                                |> FormShowed
                    }
                        |> UR.init

                _ ->
                    UR.init model

        AccountTypeSelected accountType ->
            let
                selectedKycForm =
                    case accountType of
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
                let
                    account =
                        case model.status of
                            FormShowed formType ->
                                getSignUpFields formType
                                    |> .account

                            _ ->
                                ""
                in
                model
                    |> UR.init
                    |> UR.addPort
                        { responseAddress = GotAccountAvailabilityResponse isAvailable
                        , responseData = Encode.null
                        , data =
                            Encode.object
                                [ ( "name", Encode.string "generateKeys" )
                                , ( "account", Encode.string account )
                                ]
                        }

            else
                let
                    composeProblem form formType formField =
                        formType
                            { form
                                | problems = ( formField, t "error.alreadyTaken", OnSubmit ) :: form.problems
                            }
                            |> FormShowed

                    newStatus =
                        case model.status of
                            FormShowed (JuridicalForm form) ->
                                composeProblem form JuridicalForm JuridicalForm.Account

                            FormShowed (NaturalForm form) ->
                                composeProblem form NaturalForm NaturalForm.Account

                            FormShowed (DefaultForm form) ->
                                composeProblem form DefaultForm DefaultForm.Account

                            _ ->
                                model.status
                in
                { model | status = newStatus }
                    |> UR.init

        AccountKeysGenerated (Err v) ->
            UR.init
                model
                |> UR.logDecodeError msg v

        AccountKeysGenerated (Ok accountKeys) ->
            case model.status of
                FormShowed form ->
                    { model | accountKeys = Just accountKeys }
                        |> UR.init
                        |> UR.addCmd
                            (signUp shared
                                accountKeys
                                model.invitationId
                                form
                            )

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
                    (Route.replaceUrl shared.navKey (Route.Login Nothing))

        CompletedSignUp (RemoteData.Success resp) ->
            case resp of
                Just _ ->
                    { model
                        | status = AccountCreated
                        , step = SavePassphrase
                    }
                        |> UR.init

                Nothing ->
                    model
                        |> UR.init
                        |> UR.addExt (Guest.SetFeedback <| Feedback.Visible Feedback.Failure (t "register.account_error.title"))

        CompletedSignUp (RemoteData.Failure error) ->
            model
                |> UR.init
                |> UR.addExt (Guest.SetFeedback <| Feedback.Visible Feedback.Failure (t "register.account_error.title"))
                |> UR.logGraphqlError msg error
                |> scrollTop

        CompletedSignUp _ ->
            UR.init model

        CompletedLoadInvite (RemoteData.Success (Just invitation)) ->
            loadedCommunity shared model invitation.community (Just invitation)

        CompletedLoadInvite (RemoteData.Success Nothing) ->
            UR.init { model | status = NotFound }

        CompletedLoadInvite (RemoteData.Failure error) ->
            { model | status = ErrorShowed }
                |> UR.init
                |> UR.addExt (Guest.SetFeedback <| Feedback.Visible Feedback.Failure (t "error.unknown"))
                |> UR.logGraphqlError msg error

        CompletedLoadInvite _ ->
            UR.init model

        CompletedLoadCountry (RemoteData.Success (Just country)) ->
            { model
                | country = Just country
                , status = AccountTypeSelectorShowed
            }
                |> UR.init

        CompletedLoadCountry (RemoteData.Success Nothing) ->
            UR.init { model | status = NotFound }

        CompletedLoadCountry (RemoteData.Failure error) ->
            { model | status = ErrorShowed }
                |> UR.init
                |> UR.logGraphqlError msg error

        CompletedLoadCountry _ ->
            UR.init model


loadedCommunity : Shared -> Model -> Community.CommunityPreview -> Maybe Invite -> UpdateResult
loadedCommunity shared model community maybeInvite =
    if community.hasKyc then
        let
            loadCountryData =
                Api.Graphql.query
                    shared
                    Nothing
                    (Address.countryQuery "Costa Rica")
                    CompletedLoadCountry
        in
        { model
            | status = Loading
            , invitation = maybeInvite
        }
            |> UR.init
            |> UR.addCmd loadCountryData

    else
        { model
            | status = FormShowed (DefaultForm DefaultForm.init)
            , invitation = maybeInvite
        }
            |> UR.init


type alias SignUpResponse =
    { user : Profile.Minimal
    , token : String
    }


signUp : Shared -> AccountKeys -> InvitationId -> FormModel -> Cmd Msg
signUp shared { accountName, ownerKey } invitationId form =
    let
        { email, name } =
            getSignUpFields form

        requiredArgs =
            { account = Eos.nameToString accountName
            , email = email
            , name = name
            , password = shared.graphqlSecret
            , publicKey = ownerKey
            , userType =
                case form of
                    JuridicalForm _ ->
                        "juridical"

                    _ ->
                        "natural"
            }

        ( kycOpts, addressOpts ) =
            case form of
                DefaultForm _ ->
                    ( Absent, Absent )

                NaturalForm f ->
                    ( Present
                        { countryId = Id "1"
                        , document = f.document
                        , documentType = NaturalForm.documentTypeToString f.documentType
                        , phone = f.phone
                        , userType = "natural"
                        }
                    , Absent
                    )

                JuridicalForm f ->
                    ( Present
                        { countryId = Id "1"
                        , document = f.document
                        , documentType = JuridicalForm.companyTypeToString f.companyType
                        , phone = f.phone
                        , userType = "juridical"
                        }
                    , Present
                        { countryId = Id "1"
                        , cityId = Tuple.first f.city |> Id
                        , neighborhoodId = Tuple.first f.district |> Id
                        , number = Present f.number
                        , stateId = Tuple.first f.state |> Id
                        , street = f.street
                        , zip = f.zip
                        }
                    )

        fillOptionals opts =
            { opts
                | invitationId =
                    Maybe.map Present invitationId
                        |> Maybe.withDefault Absent
                , kyc = kycOpts
                , address = addressOpts
            }
    in
    Api.Graphql.mutation shared
        Nothing
        (Mutation.signUp
            fillOptionals
            requiredArgs
            (Graphql.SelectionSet.succeed SignUpResponse
                |> with (Cambiatus.Object.Session.user Profile.minimalSelectionSet)
                |> with Cambiatus.Object.Session.token
            )
        )
        CompletedSignUp



-- INTEROP


receiveBroadcast : Guest.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        Guest.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)


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
            let
                decodeAccount : Decoder AccountKeys
                decodeAccount =
                    Decode.succeed AccountKeys
                        |> DecodePipeline.required "ownerKey" Decode.string
                        |> DecodePipeline.required "activeKey" Decode.string
                        |> DecodePipeline.required "accountName" Eos.nameDecoder
                        |> DecodePipeline.required "words" Decode.string
                        |> DecodePipeline.required "privateKey" Decode.string
            in
            Decode.decodeValue (Decode.field "data" decodeAccount) val
                |> AccountKeysGenerated
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
        NoOp ->
            [ "NoOp" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        FormMsg _ ->
            [ "FormMsg" ]

        ValidateForm _ ->
            [ "ValidateForm" ]

        GotAccountAvailabilityResponse _ ->
            [ "GotAccountAvailabilityResponse" ]

        AccountKeysGenerated r ->
            [ "AccountKeysGenerated", UR.resultToString r ]

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
