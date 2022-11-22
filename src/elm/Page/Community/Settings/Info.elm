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

import Community
import Dict
import Eos
import Eos.Account as Eos
import Form
import Form.File
import Form.RichText
import Form.Text
import Form.Toggle
import Form.Validate
import Graphql.Http
import Html exposing (Html, div, li, p, span, text, ul)
import Html.Attributes exposing (class, classList, maxlength)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode
import Log
import Markdown exposing (Markdown)
import Maybe.Extra
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import UpdateResult as UR
import Url exposing (Url)
import View.Feedback as Feedback



-- MODEL


type alias Model =
    { isLoading : Bool
    , hasSavedCoverPhoto : Bool
    , form : FormStatus
    }


type FormStatus
    = Loading
    | Loaded (Form.Model FormInput)


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { isLoading = True
      , hasSavedCoverPhoto = False
      , form = Loading
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )


type alias FormInput =
    { logo : Form.File.SingleModel
    , name : String
    , description : Form.RichText.Model
    , website : String
    , coverPhoto : Form.File.SingleModel
    , subdomain : String
    , hasAutoInvite : Bool
    , inviterReward : String
    , invitedReward : String
    }


type alias FormOutput =
    { logo : String
    , name : String
    , description : Markdown
    , website : Maybe Url
    , coverPhoto : Maybe String
    , subdomain : Url
    , hasAutoInvite : Bool
    , inviterReward : Float
    , invitedReward : Float
    }


formOutputDecoder : Decode.Decoder FormOutput
formOutputDecoder =
    let
        urlDecoder =
            Decode.string
                |> Decode.andThen
                    (\url ->
                        case Url.fromString url of
                            Just validUrl ->
                                Decode.succeed validUrl

                            Nothing ->
                                Decode.fail ("Expecting a valid url. Got " ++ url ++ " instead")
                    )
    in
    Decode.succeed FormOutput
        |> DecodePipeline.required "logo" Decode.string
        |> DecodePipeline.required "name" Decode.string
        |> DecodePipeline.required "description" Markdown.decoder
        |> DecodePipeline.required "website" (Decode.maybe urlDecoder)
        |> DecodePipeline.required "coverPhoto" (Decode.maybe Decode.string)
        |> DecodePipeline.required "subdomain" urlDecoder
        |> DecodePipeline.required "hasAutoInvite" Decode.bool
        |> DecodePipeline.required "inviterReward" Decode.float
        |> DecodePipeline.required "invitedReward" Decode.float


encodeFormOutput : FormOutput -> Value
encodeFormOutput o =
    Encode.object
        [ ( "logo", Encode.string o.logo )
        , ( "name", Encode.string o.name )
        , ( "description", Markdown.encode o.description )
        , ( "website"
          , case o.website of
                Nothing ->
                    Encode.null

                Just website ->
                    Encode.string (Url.toString website)
          )
        , ( "coverPhoto"
          , case o.coverPhoto of
                Nothing ->
                    Encode.null

                Just coverPhoto ->
                    Encode.string coverPhoto
          )
        , ( "subdomain", Encode.string (Url.toString o.subdomain) )
        , ( "hasAutoInvite", Encode.bool o.hasAutoInvite )
        , ( "inviterReward", Encode.float o.inviterReward )
        , ( "invitedReward", Encode.float o.invitedReward )
        ]



-- UPDATE


type Msg
    = CompletedLoadCommunity Community.Model
    | CompletedLoadUploads (List String)
    | ClosedAuthModal
    | ClickedSave FormOutput
    | GotDomainAvailableResponse FormOutput (RemoteData (Graphql.Http.Error Bool) Bool)
    | CompletedAddingCoverPhoto FormOutput (RemoteData (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | GotSaveResponse (Result Value FormOutput)
    | GotFormMsg (Form.Msg FormInput)


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared } as loggedIn) =
    case msg of
        CompletedLoadCommunity community ->
            let
                ( maybeCoverPhoto, maybeRequestUploads ) =
                    case community.uploads of
                        RemoteData.Success uploads ->
                            ( List.head uploads, identity )

                        RemoteData.Loading ->
                            ( Nothing, identity )

                        _ ->
                            ( Nothing
                            , UR.addExt (LoggedIn.RequestedCommunityField Community.UploadsField)
                            )
            in
            { model
                | isLoading = False
                , form =
                    { logo =
                        Form.File.initSingle
                            { fileUrl = Just community.logo
                            , aspectRatio = Just 1
                            }
                    , name = community.name
                    , description = Form.RichText.initModel "description-input" (Just community.description)
                    , website = Maybe.withDefault "" community.website
                    , coverPhoto = Form.File.initSingle { fileUrl = maybeCoverPhoto, aspectRatio = Just 1 }
                    , subdomain =
                        community.subdomain
                            |> String.split "."
                            |> List.head
                            |> Maybe.withDefault ""
                    , hasAutoInvite = community.hasAutoInvite
                    , inviterReward = String.fromFloat community.inviterReward
                    , invitedReward = String.fromFloat community.invitedReward
                    }
                        |> Form.init
                        |> Loaded
            }
                |> UR.init
                |> maybeRequestUploads

        CompletedLoadUploads uploads ->
            case model.form of
                Loading ->
                    UR.init model

                Loaded form ->
                    { model
                        | form =
                            Form.updateValues
                                (\values ->
                                    { values
                                        | coverPhoto =
                                            Form.File.initSingle
                                                { fileUrl = List.head uploads
                                                , aspectRatio = Just 1
                                                }
                                    }
                                )
                                form
                                |> Loaded
                    }
                        |> UR.init

        ClosedAuthModal ->
            { model | isLoading = False }
                |> UR.init

        ClickedSave formOutput ->
            let
                maybeCommunity =
                    RemoteData.toMaybe loggedIn.selectedCommunity

                isSameSubdomain =
                    maybeCommunity
                        |> Maybe.map .subdomain
                        |> Maybe.map ((==) formOutput.subdomain.host)
                        |> Maybe.withDefault False
            in
            if isSameSubdomain then
                update (GotDomainAvailableResponse formOutput (RemoteData.Success True))
                    { model | isLoading = True }
                    loggedIn

            else
                { model | isLoading = True }
                    |> UR.init
                    |> UR.addExt
                        (LoggedIn.query loggedIn
                            (Community.domainAvailableQuery formOutput.subdomain.host)
                            (GotDomainAvailableResponse formOutput)
                        )

        GotDomainAvailableResponse formOutput (RemoteData.Success True) ->
            case Community.getField loggedIn.selectedCommunity .uploads of
                RemoteData.Success ( community, communityUploads ) ->
                    let
                        authorization =
                            { actor = loggedIn.accountName
                            , permissionName = Eos.samplePermission
                            }

                        asset amount =
                            { amount = amount
                            , symbol = community.symbol
                            }

                        addCoverPhoto =
                            case newUpload of
                                Nothing ->
                                    identity

                                Just url ->
                                    LoggedIn.mutation loggedIn
                                        (Community.addPhotosMutation
                                            community.symbol
                                            (url :: communityUploads)
                                        )
                                        (CompletedAddingCoverPhoto formOutput)
                                        |> UR.addExt

                        newUpload =
                            if List.head communityUploads == formOutput.coverPhoto then
                                Nothing

                            else
                                formOutput.coverPhoto
                    in
                    { model
                        | isLoading = True
                        , hasSavedCoverPhoto = Maybe.Extra.isNothing newUpload
                    }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = GotDomainAvailableResponse formOutput (RemoteData.Success True)
                            , responseData = encodeFormOutput formOutput
                            , data =
                                Eos.encodeTransaction
                                    [ { accountName = loggedIn.shared.contracts.community
                                      , name = "update"
                                      , authorization = authorization
                                      , data =
                                            { asset = asset 0
                                            , logo = formOutput.logo
                                            , name = formOutput.name
                                            , description = formOutput.description
                                            , subdomain = formOutput.subdomain.host
                                            , inviterReward = asset formOutput.inviterReward
                                            , invitedReward = asset formOutput.invitedReward
                                            , hasObjectives = Eos.boolToEosBool community.hasObjectives
                                            , hasShop = Eos.boolToEosBool community.hasShop
                                            , hasKyc = Eos.boolToEosBool community.hasKyc
                                            , hasAutoInvite = Eos.boolToEosBool (not formOutput.hasAutoInvite)
                                            , website =
                                                Maybe.map Url.toString formOutput.website
                                                    |> Maybe.withDefault ""
                                            }
                                                |> Community.encodeUpdateData
                                      }
                                    ]
                            }
                        |> addCoverPhoto
                        |> LoggedIn.withPrivateKey loggedIn
                            []
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }
                        |> UR.addBreadcrumb
                            { type_ = Log.DebugBreadcrumb
                            , category = msg
                            , message = "Checked that domain is available"
                            , data = Dict.fromList [ ( "domain", Encode.string formOutput.subdomain.host ) ]
                            , level = Log.DebugLevel
                            }

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Community is not loaded when checking if domain is available in Community Info"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.Info", function = "update" }
                            []

        GotDomainAvailableResponse formOutput (RemoteData.Success False) ->
            { model | isLoading = False }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (shared.translators.t "settings.community_info.errors.url.already_taken")
                    )
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = msg
                    , message = "Tried domain that is unavailable"
                    , data = Dict.fromList [ ( "domain", Encode.string formOutput.subdomain.host ) ]
                    , level = Log.DebugLevel
                    }

        GotDomainAvailableResponse formOutput (RemoteData.Failure err) ->
            UR.init { model | isLoading = False }
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to check if domain is available"
                    { moduleName = "Page.Community.Settings.Info", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity
                    , { name = "Domain"
                      , extras = Dict.fromList [ ( "tried", Encode.string formOutput.subdomain.host ) ]
                      }
                    ]
                    err
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (shared.translators.t "error.unknown"))

        GotDomainAvailableResponse _ RemoteData.Loading ->
            UR.init model

        GotDomainAvailableResponse _ RemoteData.NotAsked ->
            UR.init model

        CompletedAddingCoverPhoto formOutput (RemoteData.Success _) ->
            { model | hasSavedCoverPhoto = True }
                |> UR.init
                |> UR.addMsg (GotSaveResponse (Ok formOutput))

        CompletedAddingCoverPhoto _ (RemoteData.Failure err) ->
            { model | isLoading = False }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to add a cover photo"
                    { moduleName = "Page.Community.Settings.Info", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (shared.translators.t "settings.community_info.errors.cover_upload")
                    )

        CompletedAddingCoverPhoto _ _ ->
            UR.init model

        GotSaveResponse (Ok formOutput) ->
            if not model.hasSavedCoverPhoto then
                model |> UR.init

            else
                case loggedIn.selectedCommunity of
                    RemoteData.Success community ->
                        let
                            newCommunity =
                                { symbol = community.symbol
                                , subdomain = formOutput.subdomain.host
                                }

                            redirectToCommunity =
                                if newCommunity.subdomain == community.subdomain || not shared.useSubdomain then
                                    Route.replaceUrl shared.navKey Route.Dashboard

                                else
                                    Route.loadExternalCommunity loggedIn.shared
                                        newCommunity
                                        Route.Dashboard

                            newUploads =
                                case community.uploads of
                                    RemoteData.Success uploads ->
                                        case formOutput.coverPhoto of
                                            Nothing ->
                                                RemoteData.Success uploads

                                            Just coverPhoto ->
                                                RemoteData.Success (coverPhoto :: uploads)

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
                                        | name = formOutput.name
                                        , description = formOutput.description
                                        , website = Maybe.map Url.toString formOutput.website
                                        , logo = formOutput.logo
                                        , inviterReward = formOutput.inviterReward
                                        , invitedReward = formOutput.invitedReward
                                        , hasAutoInvite = formOutput.hasAutoInvite
                                        , uploads = newUploads
                                    }
                                    |> LoggedIn.ExternalBroadcast
                                )
                            |> UR.addBreadcrumb
                                { type_ = Log.DebugBreadcrumb
                                , category = msg
                                , message = "Saved community information"
                                , data = Dict.empty
                                , level = Log.DebugLevel
                                }

                    _ ->
                        model
                            |> UR.init
                            |> UR.logImpossible msg
                                "Saved community, but it wasn't loaded"
                                (Just loggedIn.accountName)
                                { moduleName = "Page.Community.Settings.Info", function = "update" }
                                []

        GotSaveResponse (Err _) ->
            { model | isLoading = False }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (shared.translators.t "community.error_saving"))

        GotFormMsg subMsg ->
            case model.form of
                Loading ->
                    UR.init model

                Loaded form ->
                    Form.update loggedIn.shared subMsg form
                        |> UR.fromChild (\newForm -> { model | form = Loaded newForm })
                            GotFormMsg
                            LoggedIn.addFeedback
                            model



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
                    case model.form of
                        Loading ->
                            Page.fullPageLoading shared

                        Loaded form ->
                            div [ class "bg-white" ]
                                [ Page.viewHeader loggedIn title
                                , Form.view
                                    [ class "container mx-auto px-4 pb-10 pt-4" ]
                                    shared.translators
                                    (\submitButton ->
                                        [ submitButton [ class "button button-primary w-full mt-14" ]
                                            [ text <| t "menu.save" ]
                                        ]
                                    )
                                    (createForm shared community { isLoading = model.isLoading })
                                    (Form.withDisabled (model.isLoading || not loggedIn.hasAcceptedCodeOfConduct) form)
                                    { toMsg = GotFormMsg
                                    , onSubmit = ClickedSave
                                    }
                                ]
    in
    { title = title
    , content = content
    }


createForm : Shared -> Community.Model -> { isLoading : Bool } -> Form.Form msg FormInput FormOutput
createForm shared community { isLoading } =
    let
        ({ t } as translators) =
            shared.translators
    in
    Form.succeed FormOutput
        |> Form.with
            (Form.File.init { id = "logo-input" }
                |> Form.File.withLabel (t "settings.community_info.logo.title")
                |> Form.File.withContainerAttributes [ class "mb-4" ]
                |> Form.File.withImageClass "object-cover rounded-full mx-auto w-20 h-20"
                |> Form.File.withEntryContainerAttributes (\_ -> [ class "mx-auto rounded-full w-20 h-20 self-center" ])
                |> Form.File.withEditIconOverlay
                |> Form.File.withAddImagesContainerAttributes [ class "mx-auto rounded-full" ]
                |> Form.File.withAddImagesView
                    [ span [ class "bg-orange-300 rounded-full p-4 w-20 h-20 mx-auto" ]
                        [ Icons.camera "text-white"
                        ]
                    ]
                |> Form.File.withImageCropperAttributes [ class "rounded-full" ]
                |> Form.file
                    { parser = Ok
                    , translators = translators
                    , value = .logo
                    , update = \logo input -> { input | logo = logo }
                    , externalError = always Nothing
                    }
            )
        |> Form.withDecoration
            (div [ class "md:text-center mb-10" ]
                [ p [ class "font-bold" ] [ text <| t "settings.community_info.logo.guidance" ]
                , p [ class "text-gray-600" ] [ text <| t "settings.community_info.logo.description" ]
                ]
            )
        |> Form.with
            (Form.Text.init { label = t "settings.community_info.fields.name", id = "name-input" }
                |> Form.Text.withPlaceholder (t "settings.community_info.placeholders.name")
                |> Form.textField
                    { parser = Ok
                    , value = .name
                    , update = \name input -> { input | name = name }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.RichText.init { label = t "settings.community_info.placeholders.description" }
                |> Form.RichText.withEditorContainerAttrs [ class "mb-10" ]
                |> Form.RichText.withPlaceholder (t "settings.community_info.placeholders.description")
                |> Form.richText
                    { parser = Ok
                    , value = .description
                    , update = \description input -> { input | description = description }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = t "settings.community_info.fields.website", id = "website-input" }
                |> Form.Text.withPlaceholder "cambiatus.com"
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.url
                            >> Form.Validate.validate translators
                    , value = .website
                    , update = \website input -> { input | website = website }
                    , externalError = always Nothing
                    }
                |> Form.optional
            )
        |> Form.with
            (Form.File.init { id = "cover-photo-input" }
                |> Form.File.withLabel (t "settings.community_info.cover_photo.title")
                |> Form.File.withEntryContainerAttributes (\_ -> [ class "w-full h-56 bg-purple-500 rounded-sm grid place-items-center overflow-hidden relative" ])
                |> Form.File.withImageClass "h-56"
                |> Form.File.withImageSiblingElement
                    (div [ class "bg-orange-300 rounded-full absolute right-4 bottom-4 h-8 w-8 grid place-items-center" ]
                        [ Icons.edit "text-white w-4 h-4"
                        ]
                    )
                |> Form.File.withAddImagesContainerAttributes [ class "!w-full" ]
                |> Form.File.withAddImagesView
                    [ div [ class "h-56 w-full bg-purple-500 rounded-sm text-white flex flex-col items-center justify-center" ]
                        [ Icons.camera "w-10 mb-2"
                        , p [ class "px-4 font-bold" ]
                            [ text <| translators.t "community.actions.proof.upload_hint"
                            ]
                        ]
                    ]
                |> Form.file
                    { parser = Ok
                    , translators = translators
                    , value = .coverPhoto
                    , update = \coverPhoto input -> { input | coverPhoto = coverPhoto }
                    , externalError = always Nothing
                    }
                |> Form.optional
            )
        |> Form.withDecoration
            (p [ class "mt-2 text-center text-gray-900 uppercase text-sm tracking-wide mb-10" ]
                [ text <| t "settings.community_info.cover_photo.description" ]
            )
        |> Form.with
            (Form.introspect
                (\values ->
                    Form.Text.init { label = t "settings.community_info.url.title", id = "subdomain-input" }
                        |> Form.Text.withPlaceholder (t "settings.community_info.placeholders.url")
                        |> Form.Text.withCounter (Form.Text.CountLetters 30)
                        |> Form.Text.withExtraAttrs
                            [ maxlength 30
                            , classList [ ( "pr-29", not <| String.isEmpty values.subdomain ) ]
                            ]
                        |> Form.Text.withElements
                            [ span
                                [ class "absolute inset-y-0 right-4 flex items-center bg-white pl-1 my-2"
                                , classList [ ( "bg-gray-500", isLoading ) ]
                                ]
                                [ text ".cambiatus.io" ]
                            ]
                        |> Form.textField
                            { parser =
                                Form.Validate.succeed
                                    >> Form.Validate.custom
                                        (\stringInput ->
                                            if String.all (\char -> Char.isAlpha char || char == '-') stringInput then
                                                Ok stringInput

                                            else
                                                Err (\translators_ -> translators_.t "settings.community_info.errors.url.invalid_char")
                                        )
                                    >> Form.Validate.custom
                                        (\stringInput ->
                                            if String.filter Char.isAlpha stringInput |> String.all Char.isLower then
                                                Ok stringInput

                                            else
                                                Err (\translators_ -> translators_.t "settings.community_info.errors.url.invalid_case")
                                        )
                                    >> Form.Validate.stringLongerThan 1
                                    >> Form.Validate.url
                                    >> Form.Validate.map (Route.addEnvironmentToUrl shared.environment)
                                    >> Form.Validate.validate translators
                            , value = .subdomain
                            , update = \subdomain input -> { input | subdomain = subdomain }
                            , externalError = always Nothing
                            }
                )
            )
        |> Form.withDecoration
            (div [ class "border-b border-gray-500 pb-4 mb-10" ]
                [ p [ class "font-bold" ] [ text <| t "settings.community_info.url.guidance" ]
                , ul [ class "text-gray-600" ]
                    [ li [] [ text <| t "settings.community_info.constraints.length" ]
                    , li [] [ text <| t "settings.community_info.constraints.characters" ]
                    , li [] [ text <| t "settings.community_info.constraints.donts" ]
                    ]
                ]
            )
        |> Form.with
            (Form.Toggle.init
                { label = text <| t "settings.community_info.fields.invitation"
                , id = "require-invitation-toggle"
                }
                |> Form.Toggle.withTopLabel (t "settings.community_info.invitation.title")
                |> Form.toggle
                    { parser = Ok
                    , value = .hasAutoInvite >> not
                    , update = \hasAutoInvite input -> { input | hasAutoInvite = not hasAutoInvite }
                    , externalError = always Nothing
                    }
            )
        |> Form.withDecoration
            (div [ class "mb-10" ]
                [ p [ class "font-bold mt-7" ] [ text <| t "settings.community_info.invitation.guidance" ]
                , p [ class "text-gray-600" ] [ text <| t "settings.community_info.invitation.description" ]
                ]
            )
        |> Form.with
            (Form.Text.init { label = t "community.create.labels.inviter_reward", id = "inviter-reward-input" }
                |> Form.Text.withCurrency community.symbol
                |> Form.Text.withPlaceholder
                    (Eos.formatSymbolAmount translators
                        community.symbol
                        10
                    )
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.maskedFloat translators
                            >> Form.Validate.floatGreaterThanOrEqualTo 0
                            >> Form.Validate.validate translators
                    , value = .inviterReward
                    , update = \inviterReward input -> { input | inviterReward = inviterReward }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = t "community.create.labels.invited_reward", id = "invited-reward-input" }
                |> Form.Text.withCurrency community.symbol
                |> Form.Text.withPlaceholder
                    (Eos.formatSymbolAmount translators
                        community.symbol
                        5
                    )
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.maskedFloat translators
                            >> Form.Validate.floatGreaterThanOrEqualTo 0
                            >> Form.Validate.validate translators
                    , value = .invitedReward
                    , update = \invitedReward input -> { input | invitedReward = invitedReward }
                    , externalError = always Nothing
                    }
            )



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        LoggedIn.CommunityFieldLoaded _ (Community.UploadsValue uploads) ->
            Just (CompletedLoadUploads uploads)

        _ ->
            Nothing


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "GotDomainAvailableResponse" :: _ ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.map2 (\_ formOutput -> Ok formOutput)
                        (Decode.field "transactionId" Decode.string)
                        (Decode.field "addressData" formOutputDecoder)
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

        CompletedLoadUploads _ ->
            [ "CompletedLoadUploads" ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        ClickedSave _ ->
            [ "ClickedSave" ]

        GotDomainAvailableResponse _ r ->
            [ "GotDomainAvailableResponse", UR.remoteDataToString r ]

        CompletedAddingCoverPhoto _ r ->
            [ "CompletedAddingCoverPhoto", UR.remoteDataToString r ]

        GotSaveResponse r ->
            [ "GotSaveResponse", UR.resultToString r ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg
