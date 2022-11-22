module Page.Community.New exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , update
    , view
    )

import Community
import Dict
import Environment exposing (Environment)
import Eos
import Eos.Account as Eos
import Form
import Form.File
import Form.RichText
import Form.Text
import Form.Toggle
import Form.Validate
import Graphql.Document
import Graphql.Http
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList, disabled, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra
import Log
import Markdown exposing (Markdown)
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared as Shared
import Token
import UpdateResult as UR
import Url
import View.Components
import View.Feedback as Feedback



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )



-- MODEL


type alias Model =
    { isDisabled : Bool
    , form : Form.Model FormInput
    }


initModel : Model
initModel =
    { isDisabled = False
    , form =
        Form.init
            { description = Form.RichText.initModel "description-editor" Nothing
            , name = ""
            , url = ""
            , symbol = ""
            , invitedReward = "10"
            , inviterReward = "0"
            , minimumBalance = "-100"
            , website = ""
            , requireInvitation = True
            , logo = Form.File.initMultiple { fileUrls = defaultLogos, aspectRatio = Just 1 }
            , selectedLogoIndex = 0
            }
    }


defaultLogos : List String
defaultLogos =
    [ "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/community_1.png"
    , "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/community_2.png"
    , "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/community_3.png"
    , "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/community_4.png"
    , "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/community_5.png"
    ]


type alias FormInput =
    { description : Form.RichText.Model
    , name : String
    , url : String
    , symbol : String
    , invitedReward : String
    , inviterReward : String
    , minimumBalance : String
    , website : String
    , requireInvitation : Bool
    , logo : Form.File.MultipleModel
    , selectedLogoIndex : Int
    }


type alias FormOutput =
    { description : Markdown
    , name : String
    , url : Url.Url
    , symbol : Eos.Symbol
    , invitedReward : Float
    , inviterReward : Float
    , minimumBalance : Float
    , website : Maybe String
    , requireInvitation : Bool
    , logo : String
    }


createForm : Shared.Translators -> Environment -> { isDisabled : Bool } -> Form.Form Msg FormInput FormOutput
createForm ({ t } as translators) environment { isDisabled } =
    Form.succeed FormOutput
        |> Form.with
            (Form.RichText.init { label = t "community.create.labels.description" }
                |> Form.RichText.withEditorContainerAttrs [ class "mb-10" ]
                |> Form.richText
                    { parser = Ok
                    , value = .description
                    , update = \description input -> { input | description = description }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init
                { label = t "community.create.labels.currency_name"
                , id = "currency-name-input"
                }
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.stringLongerThan 1
                            >> Form.Validate.validate translators
                    , value = .name
                    , update = \name input -> { input | name = name }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init
                { label = t "settings.community_info.url.title"
                , id = "subdomain-input"
                }
                |> Form.Text.withElements
                    [ span
                        [ class "absolute inset-y-0 right-4 flex items-center bg-white pl-1 my-2"
                        , classList [ ( "bg-gray-500", isDisabled ) ]
                        ]
                        [ text ".cambiatus.io" ]
                    ]
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.map String.toLower
                            >> Form.Validate.url
                            >> Form.Validate.map (Route.addEnvironmentToUrl environment)
                            >> Form.Validate.validate translators
                    , value = .url
                    , update = \url input -> { input | url = url }
                    , externalError = always Nothing
                    }
            )
        |> Form.withGroup
            [ class "grid grid-cols-2 gap-4" ]
            (Form.Text.init
                { label = t "community.create.labels.currency_symbol"
                , id = "community-symbol-input"
                }
                |> Form.Text.withPlaceholder ("_, " ++ String.join " " (List.repeat Eos.maxSymbolLength "_"))
                |> Form.Text.withMask { mask = "#," ++ String.concat (List.repeat Eos.maxSymbolLength "#"), replace = '#' }
                |> Form.Text.withExtraAttrs [ class "uppercase" ]
                |> Form.textField
                    { parser =
                        Eos.symbolFromString
                            >> Result.fromMaybe (t "error.invalidSymbol")
                    , value = .symbol
                    , update = \symbol input -> { input | symbol = symbol }
                    , externalError = always Nothing
                    }
            )
            (Form.Text.init
                { label = t "community.create.labels.invited_reward"
                , id = "invited-reward-input"
                }
                |> Form.Text.asNumeric
                |> Form.Text.withType Form.Text.Number
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.maskedFloat translators
                            >> Form.Validate.validate translators
                    , value = .invitedReward
                    , update = \invitedReward input -> { input | invitedReward = invitedReward }
                    , externalError = always Nothing
                    }
            )
        |> Form.withGroup
            [ class "grid grid-cols-2 gap-4" ]
            (Form.Text.init
                { label = t "community.create.labels.inviter_reward"
                , id = "inviter-reward-input"
                }
                |> Form.Text.asNumeric
                |> Form.Text.withType Form.Text.Number
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.maskedFloat translators
                            >> Form.Validate.validate translators
                    , value = .inviterReward
                    , update = \inviterReward input -> { input | inviterReward = inviterReward }
                    , externalError = always Nothing
                    }
            )
            (Form.Text.init
                { label = t "community.create.labels.min_balance"
                , id = "minimum-balance-input"
                }
                |> Form.Text.asNumeric
                |> Form.Text.withType Form.Text.Number
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.maskedFloat translators
                            >> Form.Validate.validate translators
                    , value = .minimumBalance
                    , update = \minimumBalance input -> { input | minimumBalance = minimumBalance }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init
                { label = t "settings.community_info.fields.website"
                , id = "website-input"
                }
                |> Form.Text.withPlaceholder "cambiatus.com"
                |> Form.textField
                    { parser = Ok
                    , value = .website
                    , update = \website input -> { input | website = website }
                    , externalError = always Nothing
                    }
                |> Form.optional
            )
        |> Form.with (requireInvitationToggle translators)
        |> Form.with
            (Form.introspect
                (\{ selectedLogoIndex } ->
                    Form.File.init { id = "logo-input" }
                        |> Form.File.withLabel (t "settings.community_info.logo.title")
                        |> Form.File.withContainerAttributes [ class "w-full mt-10" ]
                        |> Form.File.withEntryContainerAttributes
                            (\index ->
                                [ class "w-22 h-22 p-2 grid place-items-center border rounded-md"
                                , classList
                                    [ ( "border-gray-900 shadow-lg", index == selectedLogoIndex )
                                    , ( "border-transparent", index /= selectedLogoIndex )
                                    ]
                                ]
                            )
                        |> Form.File.withImageClass "max-w-16 max-h-16"
                        |> Form.File.withAddImagesView (Form.File.defaultAddImagesView [ class "!w-22 !h-22" ])
                        |> Form.File.withEntryActions
                            (\entryIndex ->
                                [ Form.File.DeleteEntry
                                , Form.File.CustomAction
                                    (button
                                        [ class "uppercase text-orange-300 font-bold focus-ring rounded-sm hover:opacity-60"
                                        , onClick (SelectedEntry entryIndex)
                                        , type_ "button"
                                        ]
                                        [ if entryIndex == selectedLogoIndex then
                                            text <| t "settings.community_info.logo.selected"

                                          else
                                            text <| t "settings.community_info.logo.select"
                                        ]
                                    )
                                , Form.File.ReplaceEntry
                                , Form.File.SaveEntry
                                ]
                            )
                        |> Form.fileMultiple
                            { parser =
                                List.Extra.getAt selectedLogoIndex
                                    >> Result.fromMaybe (t "settings.community_info.logo.choose_one")
                            , translators = translators
                            , value = .logo
                            , update = \logo input -> { input | logo = logo }
                            , externalError = always Nothing
                            }
                )
            )


requireInvitationToggle : Shared.Translators -> Form.Form msg { b | requireInvitation : Bool } Bool
requireInvitationToggle { t } =
    Form.succeed (\_ requiresInvitation -> requiresInvitation)
        |> Form.withGroup [ class "border rounded-md px-3 py-2" ]
            (View.Components.label []
                { targetId = "require-invitation-toggle"
                , labelText = t "settings.community_info.invitation.title"
                }
                |> Form.arbitraryWith ()
            )
            (Form.Toggle.init
                { label = text <| t "settings.community_info.fields.invitation"
                , id = "require-invitation-toggle"
                }
                |> Form.Toggle.withTooltip
                    { message = t "settings.community_info.invitation.description"
                    , iconClass = "text-orange-300"
                    , containerClass = ""
                    }
                |> Form.Toggle.withContainerAttrs []
                |> Form.toggle
                    { parser = Ok
                    , value = .requireInvitation
                    , update = \requireInvitation input -> { input | requireInvitation = requireInvitation }
                    , externalError = always Nothing
                    }
            )



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        t =
            shared.translators.t
    in
    { title = t "community.create.title"
    , content =
        div [ class "bg-white pb-10" ]
            [ Page.viewHeader loggedIn (t "community.create.title")
            , Form.view [ class "container mx-auto px-4 mt-10" ]
                shared.translators
                (\submitButton ->
                    [ submitButton
                        [ class "button button-primary w-full mt-10"
                        , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                        ]
                        [ text <| t "community.create.submit" ]
                    ]
                )
                (createForm shared.translators shared.environment { isDisabled = model.isDisabled })
                model.form
                { toMsg = GotFormMsg
                , onSubmit = SubmittedForm
                }
            ]
    }



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = GotFormMsg (Form.Msg FormInput)
    | SelectedEntry Int
    | SubmittedForm FormOutput
    | GotDomainAvailableResponse FormOutput (RemoteData (Graphql.Http.Error Bool) Bool)
    | StartedCreatingCommunity Community.CreateCommunityData Token.CreateTokenData
    | GotCreateCommunityResponse (Result Value Eos.Symbol)
    | Redirect Community.CreateCommunityData
    | ClosedAuthModal


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        t =
            loggedIn.shared.translators.t
    in
    case msg of
        SubmittedForm formOutput ->
            { model | isDisabled = True }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.query loggedIn
                        (Community.domainAvailableQuery formOutput.url.host)
                        (GotDomainAvailableResponse formOutput)
                    )

        SelectedEntry index ->
            { model
                | form =
                    Form.updateValues
                        (\form -> { form | selectedLogoIndex = index })
                        model.form
            }
                |> UR.init

        GotDomainAvailableResponse formOutput (RemoteData.Success True) ->
            let
                createCommunityData =
                    Community.createCommunityData
                        { accountName = loggedIn.accountName
                        , symbol = formOutput.symbol
                        , logoUrl = formOutput.logo
                        , name = formOutput.name
                        , description = formOutput.description
                        , subdomain = formOutput.url.host
                        , inviterReward = formOutput.inviterReward
                        , invitedReward = formOutput.invitedReward
                        , hasShop = True
                        , hasObjectives = True
                        , hasKyc = False
                        , hasAutoInvite = not formOutput.requireInvitation
                        , website =
                            case formOutput.website of
                                Nothing ->
                                    ""

                                Just website ->
                                    if String.startsWith "https://" website || String.startsWith "http://" website then
                                        website

                                    else
                                        "http://" ++ website
                        }

                createTokenData =
                    { creator = loggedIn.accountName
                    , maxSupply = { amount = 21000000.0, symbol = formOutput.symbol }
                    , minBalance = { amount = formOutput.minimumBalance, symbol = formOutput.symbol }
                    , tokenType = Token.Mcc
                    }

                subscriptionDoc =
                    Community.newCommunitySubscription createCommunityData.cmmAsset.symbol
                        |> Graphql.Document.serializeSubscription
            in
            { model | isDisabled = True }
                |> UR.init
                |> UR.addPort
                    { responseAddress = GotDomainAvailableResponse formOutput (RemoteData.Success True)
                    , responseData =
                        Encode.object
                            [ ( "createCommunityData", Community.encodeCreateCommunityData createCommunityData )
                            , ( "createTokenData", Token.encodeCreateTokenData createTokenData )
                            ]
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "subscribeToNewCommunity" )
                            , ( "subscription", Encode.string subscriptionDoc )
                            ]
                    }
                |> LoggedIn.withPrivateKey loggedIn
                    []
                    model
                    { successMsg = msg, errorMsg = ClosedAuthModal }
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = msg
                    , message = "Checked that domain is available"
                    , data = Dict.fromList [ ( "domain", Encode.string formOutput.url.host ) ]
                    , level = Log.DebugLevel
                    }

        GotDomainAvailableResponse formOutput (RemoteData.Success False) ->
            { model | isDisabled = False }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (loggedIn.shared.translators.t "settings.community_info.errors.url.already_taken")
                    )
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = msg
                    , message = "Tried domain that is unavailable"
                    , data = Dict.fromList [ ( "domain", Encode.string formOutput.url.host ) ]
                    , level = Log.DebugLevel
                    }

        GotDomainAvailableResponse formOutput (RemoteData.Failure err) ->
            { model | isDisabled = False }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when checking if community domain is available"
                    { moduleName = "Page.Community.New", function = "update" }
                    [ { name = "Domain"
                      , extras = Dict.fromList [ ( "tried", Encode.string formOutput.url.host ) ]
                      }
                    ]
                    err
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (loggedIn.shared.translators.t "error.unknown"))

        GotDomainAvailableResponse _ RemoteData.NotAsked ->
            UR.init model

        GotDomainAvailableResponse _ RemoteData.Loading ->
            UR.init model

        StartedCreatingCommunity createCommunityData createTokenData ->
            let
                authorization =
                    { actor = loggedIn.accountName
                    , permissionName = Eos.samplePermission
                    }
            in
            UR.init model
                |> UR.addPort
                    { responseAddress = StartedCreatingCommunity createCommunityData createTokenData
                    , responseData =
                        Encode.object
                            [ ( "symbol", Eos.encodeSymbol createCommunityData.cmmAsset.symbol )
                            , ( "subdomain", Encode.string createCommunityData.subdomain )
                            ]
                    , data =
                        Eos.encodeTransaction
                            [ { accountName = loggedIn.shared.contracts.community
                              , name = "create"
                              , authorization = authorization
                              , data = Community.encodeCreateCommunityData createCommunityData
                              }
                            , { accountName = loggedIn.shared.contracts.token
                              , name = "create"
                              , authorization = authorization
                              , data = Token.encodeCreateTokenData createTokenData
                              }
                            ]
                    }

        GotCreateCommunityResponse (Ok _) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success (loggedIn.shared.translators.t "community.create.created"))
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.Dashboard)

        GotCreateCommunityResponse (Err val) ->
            { model | isDisabled = False }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (t "error.unknown"))
                |> UR.logJsonValue msg
                    (Just loggedIn.accountName)
                    "Got an error when creating a community"
                    { moduleName = "Page.Community.New", function = "update" }
                    []
                    val

        Redirect communityData ->
            let
                communityInfo =
                    { symbol = communityData.cmmAsset.symbol
                    , name = communityData.name
                    , logo = communityData.logoUrl
                    , subdomain = communityData.subdomain
                    , hasShop = Eos.eosBoolToBool communityData.hasShop
                    , hasActions = Eos.eosBoolToBool communityData.hasObjectives
                    , hasKyc = Eos.eosBoolToBool communityData.hasKyc
                    }
            in
            UR.init model
                |> UR.addExt (LoggedIn.AddedCommunity communityInfo)

        ClosedAuthModal ->
            { model | isDisabled = False }
                |> UR.init

        GotFormMsg subMsg ->
            Form.update loggedIn.shared subMsg model.form
                |> UR.fromChild (\newForm -> { model | form = newForm })
                    GotFormMsg
                    LoggedIn.addFeedback
                    model


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "GotDomainAvailableResponse" :: _ ->
            let
                response =
                    Decode.decodeValue
                        (Decode.map2 Tuple.pair
                            (Decode.field "state" Decode.string)
                            (Decode.field "addressData"
                                (Decode.map2 Tuple.pair
                                    (Decode.field "createCommunityData" Community.createCommunityDataDecoder)
                                    (Decode.field "createTokenData" Token.createTokenDataDecoder)
                                )
                            )
                        )
                        val
            in
            case response of
                Ok ( "starting", ( createCommunityData, createTokenData ) ) ->
                    Just (StartedCreatingCommunity createCommunityData createTokenData)

                Ok ( "responded", ( createCommunityData, _ ) ) ->
                    Just (Redirect createCommunityData)

                _ ->
                    Nothing

        "StartedCreatingCommunity" :: [] ->
            Decode.decodeValue
                (Decode.map2 (\_ symbol -> symbol)
                    (Decode.field "transactionId" Decode.string)
                    (Decode.at [ "addressData", "symbol" ] Eos.symbolDecoder)
                )
                val
                |> Result.mapError (\_ -> val)
                |> GotCreateCommunityResponse
                |> Just

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        SelectedEntry _ ->
            [ "SelectedEntry" ]

        SubmittedForm _ ->
            [ "SubmittedForm" ]

        GotDomainAvailableResponse _ r ->
            [ "GotDomainAvailableResponse", UR.remoteDataToString r ]

        StartedCreatingCommunity _ _ ->
            [ "StartedCreatingCommunity" ]

        GotCreateCommunityResponse _ ->
            [ "GotCreateCommunityResponse" ]

        Redirect _ ->
            [ "Redirect" ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]
