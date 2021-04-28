module Page.Community.Settings.Info exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Api
import Api.Graphql
import Browser.Navigation
import Community
import Eos
import Eos.Account as Eos
import File exposing (File)
import Graphql.Http
import Html exposing (Html, button, div, form, img, input, label, li, span, text, ul)
import Html.Attributes exposing (accept, class, classList, disabled, for, id, maxlength, multiple, src, type_)
import Html.Events exposing (onSubmit)
import Http
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Task
import UpdateResult as UR
import Url
import View.Feedback as Feedback
import View.Form
import View.Form.Input as Input
import View.Toggle



-- MODEL


type alias Model =
    { logoUrl : String
    , nameInput : String
    , nameErrors : List String
    , descriptionInput : String
    , descriptionErrors : List String
    , subdomainInput : String
    , subdomainErrors : List String
    , hasAutoInvite : Bool
    , inviterRewardInput : String
    , inviterRewardErrors : List String
    , invitedRewardInput : String
    , invitedRewardErrors : List String
    , isLoading : Bool
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { logoUrl = ""
      , nameInput = ""
      , nameErrors = []
      , descriptionInput = ""
      , descriptionErrors = []
      , subdomainInput = ""
      , subdomainErrors = []
      , hasAutoInvite = False
      , inviterRewardInput = ""
      , inviterRewardErrors = []
      , invitedRewardInput = ""
      , invitedRewardErrors = []
      , isLoading = True
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- UPDATE


type Msg
    = CompletedLoadCommunity Community.Model
    | EnteredLogo (List File)
    | CompletedLogoUpload (Result Http.Error String)
    | EnteredName String
    | EnteredDescription String
    | EnteredSubdomain String
    | ToggledInvitation Bool
    | EnteredInviterReward String
    | EnteredInvitedReward String
    | ClickedSave
    | GotDomainAvailableResponse (RemoteData (Graphql.Http.Error Bool) Bool)
    | GotSaveResponse (Result Value Eos.Symbol)


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared } as loggedIn) =
    case msg of
        CompletedLoadCommunity community ->
            { model
                | logoUrl = community.logo
                , nameInput = community.name
                , descriptionInput = community.description
                , subdomainInput =
                    community.subdomain
                        |> Maybe.map (String.split ".")
                        |> Maybe.andThen List.head
                        |> Maybe.withDefault ""
                , hasAutoInvite = community.hasAutoInvite
                , inviterRewardInput = String.fromFloat community.inviterReward
                , invitedRewardInput = String.fromFloat community.invitedReward
                , isLoading = False
            }
                |> UR.init

        EnteredLogo (file :: _) ->
            UR.init model
                |> UR.addCmd (Api.uploadImage shared file CompletedLogoUpload)

        EnteredLogo [] ->
            UR.init model

        CompletedLogoUpload (Ok url) ->
            { model | logoUrl = url }
                |> UR.init

        CompletedLogoUpload (Err e) ->
            UR.init model
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (shared.translators.t "settings.community_info.errors.logo_upload")
                    )
                |> UR.logHttpError msg e

        EnteredName name ->
            { model | nameInput = name }
                |> validateName
                |> UR.init

        EnteredDescription description ->
            { model | descriptionInput = description }
                |> validateDescription
                |> UR.init

        EnteredSubdomain subdomain ->
            { model | subdomainInput = subdomain }
                |> validateSubdomain
                |> UR.init

        ToggledInvitation requiresInvitation ->
            { model | hasAutoInvite = not requiresInvitation }
                |> UR.init

        EnteredInviterReward inviterReward ->
            { model | inviterRewardInput = inviterReward }
                |> validateInviterReward (RemoteData.toMaybe loggedIn.selectedCommunity)
                |> UR.init

        EnteredInvitedReward invitedReward ->
            { model | invitedRewardInput = invitedReward }
                |> validateInvitedReward (RemoteData.toMaybe loggedIn.selectedCommunity)
                |> UR.init

        ClickedSave ->
            let
                maybeCommunity =
                    RemoteData.toMaybe loggedIn.selectedCommunity

                isSameSubdomain =
                    maybeCommunity
                        |> Maybe.andThen .subdomain
                        |> Maybe.map ((==) (model.subdomainInput ++ ".cambiatus.io"))
                        |> Maybe.withDefault False
            in
            if isModelValid maybeCommunity model then
                if isSameSubdomain then
                    { model | isLoading = True }
                        |> UR.init
                        |> UR.addCmd
                            (Task.succeed (RemoteData.Success True)
                                |> Task.perform GotDomainAvailableResponse
                            )

                else
                    { model | isLoading = True }
                        |> UR.init
                        |> UR.addCmd
                            (Api.Graphql.query shared
                                (Just loggedIn.authToken)
                                (Community.domainAvailableQuery (model.subdomainInput ++ ".cambiatus.io"))
                                GotDomainAvailableResponse
                            )

            else
                UR.init model

        GotDomainAvailableResponse (RemoteData.Success True) ->
            case
                ( LoggedIn.hasPrivateKey loggedIn
                , isModelValid (RemoteData.toMaybe loggedIn.selectedCommunity) model
                , loggedIn.selectedCommunity
                )
            of
                ( True, True, RemoteData.Success community ) ->
                    let
                        authorization =
                            { actor = loggedIn.accountName
                            , permissionName = Eos.samplePermission
                            }

                        asset amount =
                            { amount = amount
                            , symbol = community.symbol
                            }
                    in
                    { model | isLoading = True }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = GotDomainAvailableResponse (RemoteData.Success True)
                            , responseData = Encode.string (Eos.symbolToString community.symbol)
                            , data =
                                Eos.encodeTransaction
                                    [ { accountName = loggedIn.shared.contracts.community
                                      , name = "update"
                                      , authorization = authorization
                                      , data =
                                            { asset = asset 0
                                            , logo = model.logoUrl
                                            , name = model.nameInput
                                            , description = model.descriptionInput
                                            , subdomain = model.subdomainInput ++ ".cambiatus.io"
                                            , inviterReward =
                                                String.toFloat model.inviterRewardInput
                                                    |> Maybe.withDefault community.inviterReward
                                                    |> asset
                                            , invitedReward =
                                                String.toFloat model.invitedRewardInput
                                                    |> Maybe.withDefault community.invitedReward
                                                    |> asset
                                            , hasObjectives = Eos.boolToEosBool community.hasObjectives
                                            , hasShop = Eos.boolToEosBool community.hasShop
                                            , hasKyc = Eos.boolToEosBool community.hasKyc
                                            , hasAutoInvite = Eos.boolToEosBool model.hasAutoInvite
                                            }
                                                |> Community.encodeUpdateData
                                      }
                                    ]
                            }

                ( False, True, RemoteData.Success _ ) ->
                    UR.init model
                        |> UR.addExt (Just ClickedSave |> LoggedIn.RequiredAuthentication)

                _ ->
                    UR.init model

        GotDomainAvailableResponse (RemoteData.Success False) ->
            { model | isLoading = False }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (shared.translators.t "settings.community_info.errors.url.already_taken")
                    )

        GotDomainAvailableResponse (RemoteData.Failure err) ->
            UR.init model
                |> UR.logGraphqlError msg err
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (shared.translators.t "error.unknown"))

        GotDomainAvailableResponse RemoteData.Loading ->
            UR.init model

        GotDomainAvailableResponse RemoteData.NotAsked ->
            UR.init model

        GotSaveResponse (Ok _) ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        newCommunity =
                            { symbol = community.symbol
                            , subdomain = Just (model.subdomainInput ++ ".cambiatus.io")
                            }

                        redirectToCommunity =
                            if newCommunity.subdomain == community.subdomain then
                                Route.replaceUrl shared.navKey Route.Dashboard

                            else
                                Route.externalCommunityLink loggedIn.shared.url
                                    newCommunity
                                    Route.Dashboard
                                    |> Url.toString
                                    |> Browser.Navigation.load
                    in
                    { model | isLoading = False }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success (shared.translators.t "community.create.success"))
                        |> UR.addCmd redirectToCommunity
                        |> UR.addExt
                            (LoggedIn.CommunityLoaded
                                { community
                                    | name = model.nameInput
                                    , description = model.descriptionInput
                                    , logo = model.logoUrl
                                    , inviterReward =
                                        model.inviterRewardInput
                                            |> String.toFloat
                                            |> Maybe.withDefault community.inviterReward
                                    , invitedReward =
                                        model.invitedRewardInput
                                            |> String.toFloat
                                            |> Maybe.withDefault community.invitedReward
                                    , hasAutoInvite = model.hasAutoInvite
                                }
                                |> LoggedIn.ExternalBroadcast
                            )

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg [ "CommunityNotLoaded" ]

        GotSaveResponse (Err _) ->
            { model | isLoading = False }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (shared.translators.t "community.error_saving"))


type Field
    = NameField
    | DescriptionField
    | SubdomainField


error : Field -> String -> String
error field key =
    let
        fieldString =
            case field of
                NameField ->
                    "name"

                DescriptionField ->
                    "description"

                SubdomainField ->
                    "url"
    in
    String.join "." [ "settings.community_info.errors", fieldString, key ]


isModelValid : Maybe Community.Model -> Model -> Bool
isModelValid maybeCommunity model =
    let
        validatedModel =
            validateModel maybeCommunity model
    in
    List.all (\f -> f validatedModel |> List.isEmpty)
        [ .nameErrors, .descriptionErrors, .subdomainErrors, .inviterRewardErrors, .invitedRewardErrors ]


validateModel : Maybe Community.Model -> Model -> Model
validateModel maybeCommunity model =
    model
        |> validateName
        |> validateDescription
        |> validateSubdomain
        |> validateInviterReward maybeCommunity
        |> validateInvitedReward maybeCommunity


validateName : Model -> Model
validateName model =
    { model
        | nameErrors =
            if String.isEmpty model.nameInput then
                [ error NameField "blank" ]

            else
                []
    }


validateDescription : Model -> Model
validateDescription model =
    { model
        | descriptionErrors =
            if String.isEmpty model.descriptionInput then
                [ error DescriptionField "blank" ]

            else
                []
    }


validateSubdomain : Model -> Model
validateSubdomain model =
    let
        isAllowed character =
            Char.isAlphaNum character || character == '-'

        validateLength =
            if String.length model.subdomainInput > 1 then
                []

            else
                [ error SubdomainField "too_short" ]

        validateChars =
            if String.all isAllowed model.subdomainInput then
                []

            else
                [ error SubdomainField "invalid_char" ]

        validateCase =
            if String.filter Char.isAlphaNum model.subdomainInput |> String.all Char.isLower then
                []

            else
                [ error SubdomainField "invalid_case" ]
    in
    { model | subdomainErrors = validateChars ++ validateCase ++ validateLength }


validateNumberInput : Maybe Eos.Symbol -> String -> Result String Float
validateNumberInput maybeSymbol numberInput =
    let
        validateParsing =
            String.toFloat numberInput
                |> Result.fromMaybe "error.validator.text.only_numbers"
    in
    case String.split "." numberInput of
        [] ->
            Err "error.required"

        [ "" ] ->
            Err "error.required"

        [ _ ] ->
            validateParsing

        _ :: decimalDigits :: _ ->
            case maybeSymbol of
                Just symbol ->
                    if String.length decimalDigits > Eos.getSymbolPrecision symbol then
                        Err "error.contracts.transfer.symbol precision mismatch"

                    else
                        validateParsing

                Nothing ->
                    validateParsing


validateInviterReward : Maybe Community.Model -> Model -> Model
validateInviterReward maybeCommunity model =
    case validateNumberInput (Maybe.map .symbol maybeCommunity) model.inviterRewardInput of
        Ok _ ->
            { model | inviterRewardErrors = [] }

        Err err ->
            { model | inviterRewardErrors = [ err ] }


validateInvitedReward : Maybe Community.Model -> Model -> Model
validateInvitedReward maybeCommunity model =
    case validateNumberInput (Maybe.map .symbol maybeCommunity) model.invitedRewardInput of
        Ok _ ->
            { model | invitedRewardErrors = [] }

        Err err ->
            { model | invitedRewardErrors = [ err ] }



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        { t } =
            shared.translators

        title =
            t "settings.community_info.title"

        content =
            case loggedIn.selectedCommunity of
                RemoteData.Failure e ->
                    Page.fullPageGraphQLError title e

                RemoteData.Loading ->
                    Page.fullPageLoading shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading shared

                RemoteData.Success community ->
                    div [ class "bg-white" ]
                        [ Page.viewHeader loggedIn title Route.CommunitySettings
                        , view_ loggedIn community model
                        ]
    in
    { title = title
    , content = content
    }


view_ : LoggedIn.Model -> Community.Model -> Model -> Html Msg
view_ loggedIn community model =
    let
        { t } =
            loggedIn.shared.translators
    in
    form
        [ class "w-full px-4 pb-10"
        , onSubmit ClickedSave
        ]
        [ div [ class "container mx-auto pt-4" ]
            [ div [ class "space-y-10" ]
                [ viewLogo loggedIn.shared model
                , viewName loggedIn.shared model
                , viewDescription loggedIn.shared model
                , viewSubdomain loggedIn.shared model
                , viewInvitation loggedIn.shared model
                , viewInviterReward loggedIn.shared community.symbol model
                , viewInvitedReward loggedIn.shared community.symbol model
                ]
            , button
                [ class "button button-primary w-full mt-14"
                , disabled model.isLoading
                ]
                [ text (t "menu.save") ]
            ]
        ]


viewLogo : Shared -> Model -> Html Msg
viewLogo shared model =
    let
        text_ =
            text << shared.translators.t
    in
    div []
        [ div [ class "input-label" ]
            [ text_ "settings.community_info.logo.title" ]
        , div [ class "mt-2 m-auto w-20 h-20 relative" ]
            [ input
                [ id "community-upload-logo"
                , class "profile-img-input"
                , type_ "file"
                , accept "image/*"
                , Page.onFileChange EnteredLogo
                , multiple False
                , disabled model.isLoading
                ]
                []
            , label
                [ for "community-upload-logo"
                , class "block cursor-pointer"
                ]
                [ img [ class "object-cover rounded-full w-20 h-20", src model.logoUrl ] []
                , span [ class "absolute bottom-0 right-0 bg-orange-300 w-8 h-8 p-2 rounded-full" ] [ Icons.camera "" ]
                ]
            ]
        , div [ class "mt-4" ]
            [ div [ class "font-bold" ] [ text_ "settings.community_info.guidance" ]
            , div [ class "text-gray-600" ] [ text_ "settings.community_info.logo.description" ]
            ]
        ]


viewName : Shared -> Model -> Html Msg
viewName shared model =
    let
        { t } =
            shared.translators
    in
    Input.init
        { label = t "settings.community_info.fields.name"
        , id = "community_name_input"
        , onInput = EnteredName
        , disabled = model.isLoading
        , value = model.nameInput
        , placeholder = Just (t "settings.community_info.placeholders.name")
        , problems =
            List.map t model.nameErrors
                |> List.head
                |> Maybe.map List.singleton
        , translators = shared.translators
        }
        |> Input.toHtml


viewDescription : Shared -> Model -> Html Msg
viewDescription shared model =
    let
        { t } =
            shared.translators
    in
    Input.init
        { label = t "settings.community_info.fields.description"
        , id = "community_description_input"
        , onInput = EnteredDescription
        , disabled = model.isLoading
        , value = model.descriptionInput
        , placeholder = Just (t "settings.community_info.placeholders.description")
        , problems =
            List.map t model.descriptionErrors
                |> List.head
                |> Maybe.map List.singleton
        , translators = shared.translators
        }
        |> Input.withType Input.TextArea
        |> Input.toHtml


viewSubdomain : Shared -> Model -> Html Msg
viewSubdomain shared model =
    let
        { t } =
            shared.translators

        text_ =
            text << t
    in
    div []
        [ Input.init
            { label = t "settings.community_info.fields.url"
            , id = "community_url_input"
            , onInput = EnteredSubdomain
            , disabled = model.isLoading
            , value = model.subdomainInput
            , placeholder = Just (t "settings.community_info.placeholders.url")
            , problems =
                List.map t model.subdomainErrors
                    |> List.head
                    |> Maybe.map (\x -> [ x ])
            , translators = shared.translators
            }
            |> Input.withCounter 30
            |> Input.withAttrs
                [ maxlength 30
                , classList [ ( "pr-29", not <| String.isEmpty model.subdomainInput ) ]
                ]
            |> Input.withElement
                (span
                    [ class "absolute inset-y-0 right-1 flex items-center bg-white pl-1 my-2"
                    , classList
                        [ ( "hidden", String.isEmpty model.subdomainInput )
                        , ( "bg-gray-500", model.isLoading )
                        ]
                    ]
                    [ text ".cambiatus.io" ]
                )
            |> Input.toHtml
        , div [ class "font-bold" ] [ text_ "settings.community_info.guidance" ]
        , ul [ class "text-gray-600" ]
            [ ul []
                [ li [] [ text_ "settings.community_info.constraints.length" ]
                , li [] [ text_ "settings.community_info.constraints.characters" ]
                ]
            , ul [ class "mt-4" ]
                [ li [] [ text_ "settings.community_info.constraints.bad_words" ]
                , li [] [ text_ "settings.community_info.constraints.casing" ]
                , li [] [ text_ "settings.community_info.constraints.accents" ]
                ]
            ]
        ]


viewInvitation : Shared -> Model -> Html Msg
viewInvitation { translators } model =
    div [ class "flex flex-col" ]
        [ View.Form.label "" (translators.t "settings.community_info.invitation.title")
        , span [ class "mt-4 mb-7" ] [ text (translators.t "settings.community_info.invitation.description") ]
        , View.Toggle.init
            { label = "settings.community_info.fields.invitation"
            , id = "invitation_toggle"
            , onToggle = ToggledInvitation
            , disabled = False
            , value = not model.hasAutoInvite
            }
            |> View.Toggle.toHtml translators
        ]


viewInviterReward : Shared -> Eos.Symbol -> Model -> Html Msg
viewInviterReward { translators } symbol model =
    let
        { t } =
            translators
    in
    Input.init
        { label = t "community.create.labels.inviter_reward"
        , id = "inviter_reward_input"
        , onInput = EnteredInviterReward
        , disabled = model.isLoading
        , value = model.inviterRewardInput
        , placeholder = Just (symbolPlaceholder symbol 10)
        , problems =
            List.map t model.inviterRewardErrors
                |> List.head
                |> Maybe.map List.singleton
        , translators = translators
        }
        |> Input.withCurrency symbol
        |> Input.toHtml


viewInvitedReward : Shared -> Eos.Symbol -> Model -> Html Msg
viewInvitedReward { translators } symbol model =
    let
        { t } =
            translators
    in
    Input.init
        { label = t "community.create.labels.invited_reward"
        , id = "invited_reward_input"
        , onInput = EnteredInvitedReward
        , disabled = model.isLoading
        , value = model.invitedRewardInput
        , placeholder = Just (symbolPlaceholder symbol 5)
        , problems =
            List.map t model.invitedRewardErrors
                |> List.head
                |> Maybe.map List.singleton
        , translators = translators
        }
        |> Input.withCurrency symbol
        |> Input.toHtml



-- UTILS


symbolPlaceholder : Eos.Symbol -> Int -> String
symbolPlaceholder symbol amount =
    let
        precision =
            Eos.getSymbolPrecision symbol
    in
    if precision == 0 then
        String.fromInt amount

    else
        String.fromInt amount ++ "." ++ String.join "" (List.repeat precision "0")


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "GotDomainAvailableResponse" :: _ ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.map2 (\_ symbol -> Ok symbol)
                        (Decode.field "transactionId" Decode.string)
                        (Decode.field "addressData" Eos.symbolDecoder)
                    , Decode.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotSaveResponse)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        EnteredLogo _ ->
            [ "EnteredLogo" ]

        CompletedLogoUpload r ->
            [ "CompletedLogoUpload", UR.resultToString r ]

        EnteredName _ ->
            [ "EnteredName" ]

        EnteredDescription _ ->
            [ "EnteredDescription" ]

        EnteredSubdomain _ ->
            [ "EnteredSubdomain" ]

        ToggledInvitation _ ->
            [ "ToggledInvitation" ]

        EnteredInviterReward _ ->
            [ "EnteredInviterReward" ]

        EnteredInvitedReward _ ->
            [ "EnteredInvitedReward" ]

        ClickedSave ->
            [ "ClickedSave" ]

        GotDomainAvailableResponse r ->
            [ "GotDomainAvailableResponse", UR.remoteDataToString r ]

        GotSaveResponse r ->
            [ "GotSaveResponse", UR.resultToString r ]
