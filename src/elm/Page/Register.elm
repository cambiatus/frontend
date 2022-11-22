module Page.Register exposing (Model, Msg, init, jsAddressToMsg, msgToString, receiveBroadcast, update, view)

import Address
import Api.Graphql
import Browser.Dom
import Cambiatus.Scalar exposing (Id(..))
import Community exposing (Invite)
import Eos.Account as Eos
import Form
import Form.Checkbox
import Form.Text
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, a, button, div, img, p, span, text)
import Html.Attributes exposing (class, classList, disabled, id, src)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode
import Page
import Page.Register.DefaultForm as DefaultForm
import Page.Register.JuridicalForm as JuridicalForm
import Page.Register.NaturalForm as NaturalForm
import RemoteData exposing (RemoteData)
import Result
import Route
import Session.Guest as Guest exposing (External)
import Session.Shared exposing (Shared, Translators)
import Set exposing (Set)
import Task
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback



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
    | FormShowed { unavailableAccounts : Set String } FormModel
    | AccountCreated AccountKeys
    | ErrorShowed
    | NotFound


type Step
    = FillForm
    | SavePassphrase


type FormModel
    = NaturalForm (Form.Model NaturalForm.FormInput)
    | JuridicalForm (Form.Model JuridicalForm.FormInput)
    | DefaultForm (Form.Model DefaultForm.FormInput)


type FormOutput
    = NaturalFormOutput NaturalForm.FormOutput
    | JuridicalFormOutput JuridicalForm.FormOutput
    | DefaultFormOutput DefaultForm.FormOutput


encodeFormOutput : FormOutput -> Encode.Value
encodeFormOutput output =
    case output of
        NaturalFormOutput natural ->
            NaturalForm.encode natural

        JuridicalFormOutput juridical ->
            JuridicalForm.encode juridical

        DefaultFormOutput default ->
            DefaultForm.encode default


formOutputDecoder : Decoder FormOutput
formOutputDecoder =
    Decode.oneOf
        [ Decode.map NaturalFormOutput NaturalForm.decoder
        , Decode.map JuridicalFormOutput JuridicalForm.decoder
        , Decode.map DefaultFormOutput DefaultForm.decoder
        ]


withShowAllErrors : Bool -> FormModel -> FormModel
withShowAllErrors showAllErrors formModel =
    case formModel of
        NaturalForm form ->
            NaturalForm (Form.withShowAllErrors showAllErrors form)

        JuridicalForm form ->
            JuridicalForm (Form.withShowAllErrors showAllErrors form)

        DefaultForm form ->
            DefaultForm (Form.withShowAllErrors showAllErrors form)


type alias SignUpFields =
    { name : String
    , email : String
    , account : Eos.Name
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
    , privateKey : Eos.PrivateKey
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
                        viewCreateAccount guest model

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


viewCreateAccount : Guest.Model -> Model -> Html Msg
viewCreateAccount guest model =
    let
        { translators } =
            guest.shared

        formClasses =
            "flex flex-grow flex-col bg-white px-4 md:px-0 md:pt-20 md:max-w-sm self-center w-full"

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
                    , viewFooter guest (\attrs -> button (disabled True :: attrs))
                    ]

            FormShowed unavailableAccounts formModel ->
                div [ class formClasses ]
                    [ viewAccountTypeSelector translators model
                    , viewForm guest unavailableAccounts formModel
                    ]

            AccountCreated accountKeys ->
                viewAccountCreated translators model accountKeys

            ErrorShowed ->
                Page.fullPageNotFound (translators.t "error.unknown") ""

            NotFound ->
                Page.fullPageNotFound (translators.t "error.pageNotFound") ""
        ]


viewFooter : Guest.Model -> (List (Html.Attribute msg) -> List (Html a) -> Html b) -> Html b
viewFooter guest submitButton =
    let
        { t } =
            guest.shared.translators
    in
    div [ class "mt-auto flex flex-col items-center text-gray-333 md:mt-0" ]
        [ span [ class "mb-6" ]
            [ text (t "register.login")
            , a
                [ class "underline text-orange-300"
                , Route.href (Route.Login guest.maybeInvitation guest.afterLoginRedirect)
                ]
                [ text <| t "register.authLink" ]
            ]
        , submitButton [ class "button button-primary w-full mb-4" ]
            [ text <| t "auth.login.continue" ]
        ]


viewForm : Guest.Model -> { unavailableAccounts : Set String } -> FormModel -> Html Msg
viewForm guest unavailableAccounts currentForm =
    let
        { translators } =
            guest.shared

        helper formModel form toFormMsg toFormOutput =
            Form.view []
                translators
                (viewFooter guest >> List.singleton)
                form
                formModel
                { toMsg = toFormMsg >> GotFormMsg
                , onSubmit = toFormOutput >> SubmittedForm
                }
    in
    case currentForm of
        NaturalForm form ->
            helper form (NaturalForm.form translators unavailableAccounts) NaturalFormMsg NaturalFormOutput

        JuridicalForm form ->
            helper form (JuridicalForm.form translators unavailableAccounts) JuridicalFormMsg JuridicalFormOutput

        DefaultForm form ->
            helper form (DefaultForm.form translators unavailableAccounts) DefaultFormMsg DefaultFormOutput


viewAccountTypeSelector : Translators -> Model -> Html Msg
viewAccountTypeSelector translators model =
    case model.country of
        Just country ->
            div []
                [ View.Components.label []
                    { targetId = "radio"
                    , labelText = translators.t "register.form.register_tooltip"
                    }
                , div [ class "flex space-x-2 mb-4" ]
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
                ( NaturalAccount, FormShowed _ (NaturalForm _) ) ->
                    True

                ( JuridicalAccount _, FormShowed _ (JuridicalForm _) ) ->
                    True

                _ ->
                    False
    in
    div
        [ class "w-1/2 text-center cursor-pointer rounded-sm cursor-pointer mb-4 py-2"
        , classList
            [ ( "bg-orange-300 text-white font-bold", isSelected )
            , ( "bg-gray-100 text-black", not isSelected )
            ]
        , onClick (AccountTypeSelected accountType)
        ]
        [ text title ]


viewAccountCreated : Translators -> Model -> AccountKeys -> Html Msg
viewAccountCreated ({ t, tr } as translators) model keys =
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
            [ class "flex flex-col justify-between px-4 md:max-w-sm md:mx-auto md:pt-20 md:px-0 text-white" ]
            [ div []
                [ p
                    [ class "font-bold mb-2" ]
                    [ text (tr "register.account_created.greet" [ ( "name", name ) ]) ]
                , p [ class "mb-6" ]
                    [ text (t "register.account_created.instructions")
                    ]
                , div [ class "w-1/4 m-auto relative left-4" ]
                    [ img [ src "/images/reg-passphrase-boy.svg" ]
                        []
                    , img
                        [ class "absolute w-1/4 -mt-2 -ml-10"
                        , src "/images/reg-passphrase-boy-hand.svg"
                        ]
                        []
                    ]
                , div [ class "bg-white text-black text-2xl p-4 rounded-lg" ]
                    [ p [ class "text-sm font-bold mb-4 text-black" ]
                        [ text (t "register.account_created.twelve_words")
                        , if model.isPassphraseCopiedToClipboard then
                            span [ class "ml-1" ]
                                [ text (t "register.account_created.words_copied")
                                , text " ✔"
                                ]

                          else
                            text ""
                        ]
                    , p
                        [ class "pb-4" ]
                        [ span [ class "select-all text-lg block font-bold", id passphraseTextId ]
                            [ text keys.words ]

                        -- We use `HTMLInputElement.select()` method in port to select and copy the text. This method
                        -- works only with `input` and `textarea` elements which has to be presented in DOM (e.g. we can't
                        -- hide it with `display: hidden`), so we hide it using position and opacity.
                        , Form.Text.init { label = "", id = passphraseInputId }
                            |> Form.Text.withContainerAttrs [ class "mb-0 absolute opacity-0 left-[-9999em]" ]
                            |> (\options ->
                                    Form.Text.view options
                                        { onChange = \_ -> NoOp
                                        , onBlur = NoOp
                                        , value = keys.words
                                        , error = text ""
                                        , hasError = False
                                        , translators = translators
                                        , isRequired = False
                                        }
                               )
                        ]
                    , button
                        [ class "button m-auto button-primary button-sm"
                        , onClick <| CopyToClipboard passphraseInputId
                        ]
                        [ text (t "register.account_created.copy") ]
                    ]
                ]
            , div []
                [ Form.Checkbox.init
                    { label = text (t "register.account_created.i_saved_words" ++ " 💜")
                    , id = "agreed-save-passphrase"
                    }
                    |> Form.Checkbox.withContainerAttrs [ class "my-6 font-bold" ]
                    |> (\options ->
                            Form.Checkbox.view options
                                { onCheck = AgreedToSave12Words
                                , onBlur = NoOp
                                , value = model.hasAgreedToSavePassphrase
                                , error = text ""
                                , hasError = False
                                , isRequired = True
                                }
                       )
                , button
                    [ onClick <|
                        DownloadPdf
                            { passphrase = keys.words
                            , accountName = Eos.nameToString keys.accountName
                            }
                    , class "button button-primary w-full mb-4"
                    , disabled (not model.hasAgreedToSavePassphrase)
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

        isSavingPassphrase =
            case s of
                FillForm ->
                    False

                SavePassphrase ->
                    True
    in
    p
        [ class "mx-4 py-4 font-bold md:mx-6"
        , classList
            [ ( "text-gray-333 border-t border-gray-500", not isSavingPassphrase )
            , ( "text-white", isSavingPassphrase )
            ]
        ]
        [ text (tr "register.form.step" [ ( "stepNum", stepNum ) ])
        , text " / "
        , text (t ("register.form.step" ++ stepNum ++ "_title"))
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg External


type Msg
    = NoOp
    | CompletedLoadCommunity Community.CommunityPreview
    | GotAccountAvailabilityResponse FormOutput Bool
    | AccountKeysGenerated FormOutput (Result Decode.Error AccountKeys)
    | AgreedToSave12Words Bool
    | DownloadPdf PdfData
    | PdfDownloaded
    | CopyToClipboard String
    | CopiedToClipboard
    | CompletedLoadInvite (RemoteData (Graphql.Http.Error (Maybe Invite)) (Maybe Invite))
    | CompletedLoadCountry (RemoteData (Graphql.Http.Error (Maybe Address.Country)) (Maybe Address.Country))
    | AccountTypeSelected AccountType
    | CompletedSignUp (RemoteData (Graphql.Http.Error Api.Graphql.SignUpResponse) Api.Graphql.SignUpResponse)
    | GotFormMsg FormMsg
    | SubmittedForm FormOutput


type FormMsg
    = JuridicalFormMsg (Form.Msg JuridicalForm.FormInput)
    | NaturalFormMsg (Form.Msg NaturalForm.FormInput)
    | DefaultFormMsg (Form.Msg DefaultForm.FormInput)


getSignUpFields : FormOutput -> SignUpFields
getSignUpFields form =
    let
        extract : { f | account : Eos.Name, name : String, email : String } -> SignUpFields
        extract f =
            { account = f.account
            , name = f.name
            , email = f.email
            }
    in
    case form of
        JuridicalFormOutput f ->
            extract f

        NaturalFormOutput f ->
            extract f

        DefaultFormOutput f ->
            extract f


update : InvitationId -> Msg -> Model -> Guest.Model -> UpdateResult
update _ msg model ({ shared } as guest) =
    let
        { t } =
            shared.translators

        scrollTop =
            Browser.Dom.setViewport 0 0
                |> Task.attempt (\_ -> NoOp)
                |> UR.addCmd
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
                    |> UR.addCmd (Route.pushUrl shared.navKey (Route.Join guest.afterLoginRedirect))

        AccountTypeSelected accountType ->
            let
                selectedKycForm =
                    case accountType of
                        NaturalAccount ->
                            NaturalForm NaturalForm.init

                        JuridicalAccount country ->
                            JuridicalForm (JuridicalForm.init country)
            in
            { model | status = FormShowed { unavailableAccounts = Set.empty } selectedKycForm }
                |> UR.init

        GotAccountAvailabilityResponse formOutput isAvailable ->
            let
                account =
                    getSignUpFields formOutput
                        |> .account
            in
            if isAvailable then
                model
                    |> UR.init
                    |> UR.addPort
                        { responseAddress = GotAccountAvailabilityResponse formOutput isAvailable
                        , responseData = encodeFormOutput formOutput
                        , data =
                            Encode.object
                                [ ( "name", Encode.string "generateKeys" )
                                , ( "account", Eos.encodeName account )
                                ]
                        }

            else
                let
                    newStatus =
                        case model.status of
                            FormShowed { unavailableAccounts } formModel ->
                                FormShowed
                                    { unavailableAccounts =
                                        Set.insert (Eos.nameToString account)
                                            unavailableAccounts
                                    }
                                    (withShowAllErrors True formModel)

                            _ ->
                                model.status
                in
                { model | status = newStatus }
                    |> UR.init

        AccountKeysGenerated _ (Err v) ->
            UR.init
                model
                |> UR.logDecodingError msg
                    Nothing
                    "Could not decode account keys"
                    { moduleName = "Page.Register", function = "update" }
                    []
                    v

        AccountKeysGenerated formOutput (Ok accountKeys) ->
            { model | accountKeys = Just accountKeys }
                |> UR.init
                |> UR.addCmd (signUp shared accountKeys model.invitationId formOutput)

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
                    (Route.replaceUrl shared.navKey (Route.Login guest.maybeInvitation guest.afterLoginRedirect))

        CompletedSignUp (RemoteData.Success _) ->
            case model.accountKeys of
                Nothing ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Completed signing up, but didn't have account keys"
                            Nothing
                            { moduleName = "Page.Register", function = "update" }
                            []

                Just accountKeys ->
                    { model
                        | status = AccountCreated accountKeys
                        , step = SavePassphrase
                    }
                        |> UR.init

        CompletedSignUp (RemoteData.Failure error) ->
            model
                |> UR.init
                |> UR.addExt (Guest.SetFeedback <| Feedback.Visible Feedback.Failure (t "register.account_error.title"))
                |> UR.logGraphqlError msg
                    Nothing
                    "Got an error when trying to sign up"
                    { moduleName = "Page.Register", function = "update" }
                    []
                    error
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
                |> UR.logGraphqlError msg
                    Nothing
                    "Got an error when trying to load invite"
                    { moduleName = "Page.Register", function = "update" }
                    []
                    error

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
                |> UR.logGraphqlError msg
                    Nothing
                    "Got an error when loading country data"
                    { moduleName = "Page.Register", function = "update" }
                    []
                    error

        CompletedLoadCountry _ ->
            UR.init model

        GotFormMsg subMsg ->
            case ( model.status, subMsg ) of
                ( FormShowed isAccountTaken (NaturalForm formModel), NaturalFormMsg formMsg ) ->
                    Form.update shared formMsg formModel
                        |> UR.fromChild
                            (\newForm ->
                                { model
                                    | status =
                                        newForm
                                            |> NaturalForm
                                            |> FormShowed isAccountTaken
                                }
                            )
                            (NaturalFormMsg >> GotFormMsg)
                            (Guest.SetFeedback >> UR.addExt)
                            model

                ( FormShowed isAccountTaken (JuridicalForm formModel), JuridicalFormMsg formMsg ) ->
                    Form.update shared formMsg formModel
                        |> UR.fromChild
                            (\newForm ->
                                { model
                                    | status =
                                        newForm
                                            |> JuridicalForm
                                            |> FormShowed isAccountTaken
                                }
                            )
                            (JuridicalFormMsg >> GotFormMsg)
                            (Guest.SetFeedback >> UR.addExt)
                            model

                ( FormShowed isAccountTaken (DefaultForm formModel), DefaultFormMsg formMsg ) ->
                    Form.update shared formMsg formModel
                        |> UR.fromChild
                            (\newForm ->
                                { model
                                    | status =
                                        newForm
                                            |> DefaultForm
                                            |> FormShowed isAccountTaken
                                }
                            )
                            (DefaultFormMsg >> GotFormMsg)
                            (Guest.SetFeedback >> UR.addExt)
                            model

                _ ->
                    UR.init model

        SubmittedForm formOutput ->
            let
                account =
                    case formOutput of
                        NaturalFormOutput output ->
                            output.account

                        JuridicalFormOutput output ->
                            output.account

                        DefaultFormOutput output ->
                            output.account
            in
            UR.init model
                |> UR.addPort
                    { responseAddress = SubmittedForm formOutput
                    , responseData = encodeFormOutput formOutput
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "checkAccountAvailability" )
                            , ( "account", Eos.encodeName account )
                            ]
                    }


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
            | status = FormShowed { unavailableAccounts = Set.empty } (DefaultForm DefaultForm.init)
            , invitation = maybeInvite
        }
            |> UR.init


signUp : Shared -> AccountKeys -> InvitationId -> FormOutput -> Cmd Msg
signUp shared { accountName, ownerKey } invitationId formOutput =
    let
        { email, name } =
            getSignUpFields formOutput

        ( kycOpts, addressOpts ) =
            case formOutput of
                DefaultFormOutput _ ->
                    ( Absent, Absent )

                NaturalFormOutput f ->
                    ( Present
                        { countryId = Id "1"
                        , document = f.document
                        , documentType = NaturalForm.documentTypeToString f.documentType
                        , phone = f.phoneNumber
                        , userType = "natural"
                        }
                    , Absent
                    )

                JuridicalFormOutput f ->
                    ( Present
                        { countryId = Id "1"
                        , document = f.document
                        , documentType = JuridicalForm.companyTypeToString f.companyType
                        , phone = f.phone
                        , userType = "juridical"
                        }
                    , Present
                        { countryId = Id "1"
                        , cityId = Id f.city.id
                        , neighborhoodId = Id f.district.id
                        , number = Present f.number
                        , stateId = Id f.state.id
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
    Api.Graphql.signUp shared
        { accountName = accountName
        , email = email
        , name = name
        , publicKey = ownerKey
        , userType =
            case formOutput of
                JuridicalFormOutput _ ->
                    "juridical"

                _ ->
                    "natural"
        }
        fillOptionals
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
        "SubmittedForm" :: _ ->
            Decode.decodeValue
                (Decode.map2 GotAccountAvailabilityResponse
                    (Decode.field "addressData" formOutputDecoder)
                    (Decode.field "isAvailable" Decode.bool)
                )
                val
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
                        |> DecodePipeline.required "privateKey" Eos.privateKeyDecoder

                decodedAccount : Result Decode.Error AccountKeys
                decodedAccount =
                    Decode.decodeValue (Decode.field "data" decodeAccount) val
            in
            val
                |> Decode.decodeValue (Decode.field "addressData" formOutputDecoder)
                |> Result.toMaybe
                |> Maybe.map (\formOutput -> AccountKeysGenerated formOutput decodedAccount)

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

        GotAccountAvailabilityResponse _ _ ->
            [ "GotAccountAvailabilityResponse" ]

        AccountKeysGenerated _ r ->
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

        GotFormMsg subMsg ->
            let
                formMsgToString =
                    case subMsg of
                        JuridicalFormMsg innerMsg ->
                            Form.msgToString innerMsg

                        NaturalFormMsg innerMsg ->
                            Form.msgToString innerMsg

                        DefaultFormMsg innerMsg ->
                            Form.msgToString innerMsg
            in
            "GotFormMsg" :: formMsgToString

        SubmittedForm _ ->
            [ "SubmittedForm" ]
