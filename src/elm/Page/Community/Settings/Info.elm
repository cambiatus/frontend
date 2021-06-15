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
import Community
import Eos
import Eos.Account as Eos
import File exposing (File)
import Graphql.Http
import Html exposing (Html, button, div, form, li, p, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, maxlength, required)
import Html.Events exposing (onSubmit)
import Http
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
import View.Form.FileUploader as FileUploader
import View.Form.Input as Input
import View.Form.Toggle



-- MODEL


type alias Model =
    { logo : RemoteData Http.Error String
    , nameInput : String
    , nameErrors : List String
    , descriptionInput : String
    , descriptionErrors : List String
    , websiteInput : String
    , websiteErrors : List String
    , coverPhoto : RemoteData Http.Error String
    , subdomainInput : String
    , subdomainErrors : List String
    , hasAutoInvite : Bool
    , inviterRewardInput : String
    , inviterRewardErrors : List String
    , invitedRewardInput : String
    , invitedRewardErrors : List String
    , isLoading : Bool
    , hasSavedCoverPhoto : Bool
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { logo = RemoteData.Loading
      , nameInput = ""
      , nameErrors = []
      , descriptionInput = ""
      , descriptionErrors = []
      , websiteInput = ""
      , websiteErrors = []
      , coverPhoto = RemoteData.Loading
      , subdomainInput = ""
      , subdomainErrors = []
      , hasAutoInvite = False
      , inviterRewardInput = ""
      , inviterRewardErrors = []
      , invitedRewardInput = ""
      , invitedRewardErrors = []
      , isLoading = True
      , hasSavedCoverPhoto = False
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
    | EnteredWebsite String
    | EnteredCoverPhoto (List File)
    | CompletedCoverPhotoUpload (Result Http.Error String)
    | EnteredSubdomain String
    | ToggledInvitation Bool
    | EnteredInviterReward String
    | EnteredInvitedReward String
    | ClickedSave
    | GotDomainAvailableResponse (RemoteData (Graphql.Http.Error Bool) Bool)
    | CompletedAddingCoverPhoto String (RemoteData (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | GotSaveResponse (Result Value Eos.Symbol)


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared } as loggedIn) =
    case msg of
        CompletedLoadCommunity community ->
            { model
                | logo = RemoteData.Success community.logo
                , nameInput = community.name
                , descriptionInput = community.description
                , coverPhoto =
                    case List.head community.uploads of
                        Just photo ->
                            RemoteData.Success photo

                        Nothing ->
                            RemoteData.NotAsked
                , subdomainInput =
                    community.subdomain
                        |> String.split "."
                        |> List.head
                        |> Maybe.withDefault ""
                , hasAutoInvite = community.hasAutoInvite
                , inviterRewardInput = String.fromFloat community.inviterReward
                , invitedRewardInput = String.fromFloat community.invitedReward
                , isLoading = False
                , websiteInput = Maybe.withDefault "" community.website
            }
                |> UR.init

        EnteredLogo (file :: _) ->
            UR.init model
                |> UR.addCmd (Api.uploadImage shared file CompletedLogoUpload)

        EnteredLogo [] ->
            UR.init model

        CompletedLogoUpload (Ok url) ->
            { model | logo = RemoteData.Success url }
                |> UR.init

        CompletedLogoUpload (Err e) ->
            { model | logo = RemoteData.Failure e }
                |> UR.init
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

        EnteredWebsite website ->
            { model | websiteInput = website }
                |> validateWebsite
                |> UR.init

        EnteredCoverPhoto (file :: _) ->
            UR.init { model | coverPhoto = RemoteData.Loading }
                |> UR.addCmd (Api.uploadImage shared file CompletedCoverPhotoUpload)

        EnteredCoverPhoto [] ->
            UR.init model

        CompletedCoverPhotoUpload (Ok url) ->
            { model | coverPhoto = RemoteData.Success url }
                |> UR.init

        CompletedCoverPhotoUpload (Err e) ->
            { model | coverPhoto = RemoteData.Failure e }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (shared.translators.t "settings.community_info.errors.cover_upload")
                    )
                |> UR.logHttpError msg e

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

                inputFullDomain =
                    Route.communityFullDomain shared model.subdomainInput

                isSameSubdomain =
                    maybeCommunity
                        |> Maybe.map .subdomain
                        |> Maybe.map ((==) inputFullDomain)
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
                                (Community.domainAvailableQuery inputFullDomain)
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

                        newUpload =
                            case ( model.coverPhoto, List.head community.uploads ) of
                                ( RemoteData.Success url, Just firstUpload ) ->
                                    if url == firstUpload then
                                        Nothing

                                    else
                                        Just url

                                ( RemoteData.Success url, Nothing ) ->
                                    Just url

                                _ ->
                                    Nothing
                    in
                    { model
                        | isLoading = True
                        , hasSavedCoverPhoto =
                            case newUpload of
                                Nothing ->
                                    True

                                Just _ ->
                                    False
                    }
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
                                            , logo = RemoteData.withDefault community.logo model.logo
                                            , name = model.nameInput
                                            , description = model.descriptionInput
                                            , subdomain = Route.communityFullDomain shared model.subdomainInput
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
                                            , website =
                                                if String.startsWith "https://" model.websiteInput || String.startsWith "http://" model.websiteInput then
                                                    model.websiteInput

                                                else
                                                    "http://" ++ model.websiteInput
                                            }
                                                |> Community.encodeUpdateData
                                      }
                                    ]
                            }
                        |> UR.addCmd
                            (case newUpload of
                                Just url ->
                                    Api.Graphql.mutation
                                        shared
                                        (Just loggedIn.authToken)
                                        (Community.addPhotosMutation
                                            community.symbol
                                            (url :: community.uploads)
                                        )
                                        (CompletedAddingCoverPhoto url)

                                Nothing ->
                                    Cmd.none
                            )

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
            UR.init { model | isLoading = False }
                |> UR.logGraphqlError msg err
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (shared.translators.t "error.unknown"))

        GotDomainAvailableResponse RemoteData.Loading ->
            UR.init model

        GotDomainAvailableResponse RemoteData.NotAsked ->
            UR.init model

        CompletedAddingCoverPhoto url (RemoteData.Success (Just community)) ->
            { model
                | coverPhoto = RemoteData.Success url
                , hasSavedCoverPhoto = True
            }
                |> UR.init
                |> UR.addMsg (GotSaveResponse (Ok community.symbol))

        CompletedAddingCoverPhoto _ (RemoteData.Failure err) ->
            { model | coverPhoto = RemoteData.NotAsked, isLoading = False }
                |> UR.init
                |> UR.logGraphqlError msg err
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (shared.translators.t "settings.community_info.errors.cover_upload")
                    )

        CompletedAddingCoverPhoto _ _ ->
            UR.init model

        GotSaveResponse (Ok _) ->
            if not model.hasSavedCoverPhoto then
                model |> UR.init

            else
                case loggedIn.selectedCommunity of
                    RemoteData.Success community ->
                        let
                            newCommunity =
                                { symbol = community.symbol
                                , subdomain = Route.communityFullDomain shared model.subdomainInput
                                }

                            redirectToCommunity =
                                if newCommunity.subdomain == community.subdomain || not shared.useSubdomain then
                                    Route.replaceUrl shared.navKey Route.Dashboard

                                else
                                    Route.loadExternalCommunity loggedIn.shared
                                        newCommunity
                                        Route.Dashboard

                            newUploads =
                                case model.coverPhoto of
                                    RemoteData.Success coverPhoto ->
                                        coverPhoto :: community.uploads

                                    _ ->
                                        community.uploads
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
                                        , website =
                                            if String.isEmpty model.websiteInput then
                                                Nothing

                                            else
                                                Just (addProtocolToUrlString model.websiteInput)
                                        , logo = RemoteData.withDefault community.logo model.logo
                                        , inviterReward =
                                            model.inviterRewardInput
                                                |> String.toFloat
                                                |> Maybe.withDefault community.inviterReward
                                        , invitedReward =
                                            model.invitedRewardInput
                                                |> String.toFloat
                                                |> Maybe.withDefault community.invitedReward
                                        , hasAutoInvite = model.hasAutoInvite
                                        , uploads = newUploads
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
    | WebsiteField
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

                WebsiteField ->
                    "website"

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
        [ .nameErrors
        , .descriptionErrors
        , .websiteErrors
        , .subdomainErrors
        , .inviterRewardErrors
        , .invitedRewardErrors
        ]
        && RemoteData.isSuccess model.logo
        && (RemoteData.isNotAsked model.coverPhoto || RemoteData.isSuccess model.coverPhoto)


validateModel : Maybe Community.Model -> Model -> Model
validateModel maybeCommunity model =
    model
        |> validateName
        |> validateDescription
        |> validateWebsite
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


addProtocolToUrlString : String -> String
addProtocolToUrlString url =
    if String.startsWith "https://" url || String.startsWith "http://" url then
        url

    else
        "https://" ++ url


validateWebsite : Model -> Model
validateWebsite model =
    let
        withProtocol =
            addProtocolToUrlString model.websiteInput
    in
    { model
        | websiteErrors =
            if String.isEmpty model.websiteInput then
                []

            else
                case Url.fromString withProtocol of
                    Nothing ->
                        [ error WebsiteField "invalid" ]

                    Just _ ->
                        []
    }


validateSubdomain : Model -> Model
validateSubdomain model =
    let
        isAllowed character =
            Char.isAlpha character || character == '-'

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
            if String.filter Char.isAlpha model.subdomainInput |> String.all Char.isLower then
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
                        [ Page.viewHeader loggedIn title
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
                , viewWebsite loggedIn.shared model
                , viewCoverPhoto loggedIn.shared model
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
        [ FileUploader.init
            { label = "settings.community_info.logo.title"
            , id = "community_logo_upload"
            , onFileInput = EnteredLogo
            , status = model.logo
            }
            |> FileUploader.withVariant FileUploader.Small
            |> FileUploader.toHtml shared.translators
        , div [ class "mt-4 md:text-center" ]
            [ div [ class "font-bold" ] [ text_ "settings.community_info.logo.guidance" ]
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
        |> Input.withAttrs [ required True ]
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
        |> Input.withInputType Input.TextArea
        |> Input.toHtml


viewWebsite : Shared -> Model -> Html Msg
viewWebsite shared model =
    let
        { t } =
            shared.translators
    in
    Input.init
        { label = t "settings.community_info.fields.website"
        , id = "community_website_input"
        , onInput = EnteredWebsite
        , disabled = model.isLoading
        , value = model.websiteInput
        , placeholder = Just "cambiatus.com"
        , problems =
            List.map t model.websiteErrors
                |> List.head
                |> Maybe.map List.singleton
        , translators = shared.translators
        }
        |> Input.toHtml


viewCoverPhoto : Shared -> Model -> Html Msg
viewCoverPhoto { translators } model =
    div []
        [ FileUploader.init
            { label = "settings.community_info.cover_photo.title"
            , id = "community_cover_upload"
            , onFileInput = EnteredCoverPhoto
            , status = model.coverPhoto
            }
            |> FileUploader.withAttrs [ class "w-full" ]
            |> FileUploader.toHtml translators
        , p [ class "mt-2 text-center text-gray-900 uppercase text-caption tracking-wide" ]
            [ text (translators.t "Be sure to add a picture that has good quality") ]
        ]


viewSubdomain : Shared -> Model -> Html Msg
viewSubdomain shared model =
    let
        { t } =
            shared.translators

        text_ =
            text << t
    in
    div [ class "border-b border-gray-500 pb-4" ]
        [ Input.init
            { label = t "settings.community_info.url.title"
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
                , required True
                ]
            |> Input.withElements
                [ span
                    [ class "absolute inset-y-0 right-1 flex items-center bg-white pl-1 my-2"
                    , classList
                        [ ( "hidden", String.isEmpty model.subdomainInput )
                        , ( "bg-gray-500", model.isLoading )
                        ]
                    ]
                    [ text ".cambiatus.io" ]
                ]
            |> Input.toHtml
        , span [ class "font-bold" ] [ text_ "settings.community_info.url.guidance" ]
        , ul [ class "text-gray-600" ]
            [ li [] [ text_ "settings.community_info.constraints.length" ]
            , li [] [ text_ "settings.community_info.constraints.characters" ]
            , li [] [ text_ "settings.community_info.constraints.donts" ]
            ]
        ]


viewInvitation : Shared -> Model -> Html Msg
viewInvitation { translators } model =
    div [ class "flex flex-col" ]
        [ View.Form.label "" (translators.t "settings.community_info.invitation.title")
        , View.Form.Toggle.init
            { label = text (translators.t "settings.community_info.fields.invitation")
            , id = "invitation_toggle"
            , onToggle = ToggledInvitation
            , disabled = False
            , value = not model.hasAutoInvite
            }
            |> View.Form.Toggle.withAttrs [ class "mt-4" ]
            |> View.Form.Toggle.toHtml translators
        , span [ class "font-bold mt-7" ] [ text (translators.t "settings.community_info.invitation.guidance") ]
        , span [ class "text-gray-600" ] [ text (translators.t "settings.community_info.invitation.description") ]
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
        |> Input.withAttrs [ required True ]
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
        |> Input.withAttrs [ required True ]
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
        String.fromInt amount ++ "." ++ String.concat (List.repeat precision "0")


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

        EnteredWebsite _ ->
            [ "EnteredWebsite" ]

        EnteredCoverPhoto _ ->
            [ "EnteredCoverPhoto" ]

        CompletedCoverPhotoUpload r ->
            [ "CompletedCoverPhotoUpload", UR.resultToString r ]

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

        CompletedAddingCoverPhoto _ r ->
            [ "CompletedAddingCoverPhoto", UR.remoteDataToString r ]

        GotSaveResponse r ->
            [ "GotSaveResponse", UR.resultToString r ]
