module Page.Community.New exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , subscriptions
    , update
    , view
    )

import Api
import Api.Graphql
import Browser.Events as Events
import Community
import Eos
import Eos.Account as Eos
import File exposing (File)
import Graphql.Document
import Graphql.Http
import Html exposing (Html, button, div, label, span, text)
import Html.Attributes exposing (class, classList, disabled, for, maxlength, minlength, required, type_)
import Html.Events exposing (onClick, onSubmit)
import Http
import Icons
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Task
import Token
import UpdateResult as UR
import Utils exposing (decodeEnterKeyDown)
import View.Components
import View.Feedback as Feedback
import View.Form
import View.Form.FileUploader as FileUploader
import View.Form.Input as Input
import View.Form.Toggle as Toggle



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map PressedEnter (Events.onKeyDown decodeEnterKeyDown)



-- MODEL


type alias Model =
    { name : String
    , description : String
    , subdomain : String
    , symbol : String
    , logoSelected : Int
    , logoList : List LogoStatus
    , inviterReward : String
    , invitedReward : String
    , minimumBalance : String
    , website : String
    , hasAutoInvite : Bool
    , isDisabled : Bool
    , errors : List Error
    }


initModel : Model
initModel =
    { name = ""
    , description = ""
    , subdomain = ""
    , symbol = ""
    , logoSelected = 0
    , logoList = defaultLogos
    , inviterReward = "0"
    , invitedReward = "10"
    , minimumBalance = "-100"
    , website = ""
    , hasAutoInvite = False
    , isDisabled = False
    , errors = []
    }


type alias Error =
    ( FormField, FormError )


type FormField
    = Description
    | CurrencyName
    | SymbolField
    | InvitedReward
    | InviterReward
    | MinimumBalance
    | Logo


defaultLogos : List LogoStatus
defaultLogos =
    [ Uploaded "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/community_1.png"
    , Uploaded "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/community_2.png"
    , Uploaded "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/community_3.png"
    , Uploaded "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/community_4.png"
    , Uploaded "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/community_5.png"
    ]


type LogoStatus
    = Uploading
    | Uploaded String


type FormError
    = ChooseLogo
    | WaitForLogoUpload
    | InvalidSymbol
    | EmptyRequired
    | InvalidNumber



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
            , Html.form
                [ class "container mx-auto px-4"
                , onSubmit SubmittedForm
                ]
                [ div [ class "mt-10 mb-14" ]
                    [ viewDescription shared model.isDisabled model.description model.errors
                    , viewCurrencyName shared model.isDisabled model.name model.errors
                    , viewSubdomain shared model.isDisabled model.subdomain model.errors
                    , div [ class "flex flex-row mt-4" ]
                        [ div [ class "w-1/2 pr-2" ]
                            [ viewSymbol shared model.isDisabled model.symbol model.errors
                            , viewInviterReward shared model.isDisabled model.inviterReward model.errors
                            ]
                        , div [ class "w-1/2 pl-2" ]
                            [ viewInvitedReward shared model.isDisabled model.invitedReward model.errors
                            , viewMinimumBalance shared model.isDisabled model.minimumBalance model.errors
                            ]
                        ]
                    , viewWebsite shared model.isDisabled model.website model.errors
                    , viewAutoInvite shared model.isDisabled model.hasAutoInvite model.errors
                    , viewLogo shared model.isDisabled model.logoSelected model.logoList
                    ]
                , button
                    [ class "button button-primary w-full"
                    , disabled (model.isDisabled || not (isLogoUploaded model))
                    ]
                    [ text (t "community.create.submit") ]
                ]
            ]
    }


isLogoUploaded : Model -> Bool
isLogoUploaded model =
    case List.getAt model.logoSelected model.logoList of
        Just Uploading ->
            False

        Just (Uploaded _) ->
            True

        Nothing ->
            False


viewDescription : Shared -> Bool -> String -> List Error -> Html Msg
viewDescription ({ translators } as shared) isDisabled defVal errors =
    div []
        [ span [ class "input-label" ] [ text (translators.t "community.create.labels.description") ]
        , Input.init
            { label = translators.t "community.create.tooltips.description"
            , id = "comm-description"
            , onInput = EnteredDescription
            , disabled = isDisabled
            , value = defVal
            , placeholder = Nothing
            , problems = Just (getFieldProblems shared Description errors)
            , translators = translators
            }
            |> Input.withInputType Input.TextArea
            |> Input.withAttrs [ maxlength 255, class "h-40" ]
            |> Input.toHtml
        ]


viewCurrencyName : Shared -> Bool -> String -> List Error -> Html Msg
viewCurrencyName ({ translators } as shared) isDisabled defVal errors =
    Input.init
        { label = translators.t "community.create.labels.currency_name"
        , id = "comm-currency-name"
        , onInput = EnteredName
        , disabled = isDisabled
        , value = defVal
        , placeholder = Nothing
        , problems = Just (getFieldProblems shared CurrencyName errors)
        , translators = translators
        }
        |> Input.withAttrs [ maxlength 255, required True ]
        |> Input.toHtml


viewSubdomain : Shared -> Bool -> String -> List Error -> Html Msg
viewSubdomain { translators } isDisabled defVal _ =
    Input.init
        { label = translators.t "settings.community_info.url.title"
        , id = "comm-subdomain"
        , onInput = EnteredSubdomain
        , disabled = isDisabled
        , value = defVal
        , placeholder = Nothing
        , problems = Nothing
        , translators = translators
        }
        |> Input.withElements
            [ span
                [ class "absolute inset-y-0 right-1 flex items-center bg-white pl-1 my-2"
                , classList
                    [ ( "hidden", String.isEmpty defVal )
                    , ( "bg-gray-500", isDisabled )
                    ]
                ]
                [ text ".cambiatus.io" ]
            ]
        |> Input.withAttrs [ required True ]
        |> Input.toHtml


viewWebsite : Shared -> Bool -> String -> List Error -> Html Msg
viewWebsite { translators } isDisabled defVal _ =
    Input.init
        { label = translators.t "settings.community_info.fields.website"
        , id = "comm-website"
        , onInput = EnteredWebsite
        , disabled = isDisabled
        , value = defVal
        , placeholder = Just "cambiatus.com"
        , problems = Nothing
        , translators = translators
        }
        |> Input.toHtml


viewAutoInvite : Shared -> Bool -> Bool -> List Error -> Html Msg
viewAutoInvite { translators } isDisabled defVal _ =
    div [ class "flex flex-col" ]
        [ View.Form.label "comm-autoinvite-title" (translators.t "settings.community_info.invitation.title")
        , Toggle.init
            { label = text (translators.t "settings.community_info.fields.invitation")
            , id = "comm-autoinvite"
            , onToggle = ToggledAutoInvite
            , disabled = isDisabled
            , value = not defVal
            }
            |> Toggle.withAttrs [ class "mb-10" ]
            |> Toggle.withTooltip "settings.community_info.invitation.description"
            |> Toggle.toHtml translators
        ]


viewSymbol : Shared -> Bool -> String -> List Error -> Html Msg
viewSymbol ({ translators } as shared) isDisabled defVal errors =
    Input.init
        { label = translators.t "community.create.labels.currency_symbol"
        , id = "comm-currency-symbol"
        , onInput = EnteredSymbol
        , disabled = isDisabled
        , value = defVal
        , placeholder = Just ("_, " ++ String.join " " (List.repeat Eos.maxSymbolLength "_"))
        , problems = Just (getFieldProblems shared SymbolField errors)
        , translators = translators
        }
        |> Input.withAttrs [ minlength (2 + Eos.minSymbolLength), maxlength (2 + Eos.maxSymbolLength), required True ]
        |> Input.toHtml


viewLogo : Shared -> Bool -> Int -> List LogoStatus -> Html Msg
viewLogo shared isDisabled selected logos =
    let
        t =
            shared.translators.t

        id_ =
            "community-editor-logo-upload"

        activeClass =
            "border border-gray-900 shadow-lg"

        itemClass =
            String.words activeClass
                |> List.map (\word -> String.join " " [ "hover:" ++ word, "focus:" ++ word ])
                |> String.join " "
                |> String.append "p-4 border border-white focus:outline-none rounded-md w-full h-full flex items-center justify-center "

        item index logoStatus =
            button
                [ class itemClass
                , classList [ ( activeClass, index == selected ) ]
                , type_ "button"
                , disabled isDisabled
                , onClick (ClickedLogo index)
                ]
                [ case logoStatus of
                    Uploading ->
                        div [ class "w-16 h-16" ]
                            [ View.Components.loadingLogoAnimatedFluid ]

                    Uploaded url ->
                        div
                            [ class "w-16 h-16 bg-contain bg-center bg-no-repeat"
                            , Community.logoBackground (Just url)
                            ]
                            []
                ]
    in
    div [ class "grid gap-4 xs-max:grid-cols-1 grid-cols-2 sm:grid-cols-3 md:grid-cols-5 lg:grid-cols-7" ]
        (List.indexedMap item logos
            ++ [ FileUploader.init
                    { label = ""
                    , id = id_
                    , onFileInput = EnteredLogo (List.length logos)
                    , status = RemoteData.NotAsked
                    }
                    |> FileUploader.withAttrs [ class "hidden", disabled isDisabled ]
                    |> FileUploader.toHtml shared.translators
               , label
                    [ for id_
                    , class ("flex-col text-center cursor-pointer " ++ itemClass)
                    , classList [ ( "disabled", isDisabled ) ]
                    ]
                    [ div [ class "bg-gradient-to-bl from-orange-300 to-orange-500 rounded-full p-2 mb-1 w-12 h-12 flex items-center justify-center" ]
                        [ Icons.imageMultiple "text-white fill-current w-8 h-8" ]
                    , text (t "community.create.labels.upload_icon")
                    ]
               ]
        )


viewInviterReward : Shared -> Bool -> String -> List Error -> Html Msg
viewInviterReward ({ translators } as shared) isDisabled defVal errors =
    Input.init
        { label = translators.t "community.create.labels.inviter_reward"
        , id = "comm-inviter-reward"
        , onInput = EnteredInviterReward
        , disabled = isDisabled
        , value = defVal
        , placeholder = Nothing
        , problems = Just (getFieldProblems shared InviterReward errors)
        , translators = translators
        }
        |> Input.withAttrs [ maxlength 255, required True ]
        |> Input.asNumeric
        |> Input.withType Input.Number
        |> Input.toHtml


viewInvitedReward : Shared -> Bool -> String -> List Error -> Html Msg
viewInvitedReward ({ translators } as shared) isDisabled defVal errors =
    Input.init
        { label = translators.t "community.create.labels.invited_reward"
        , id = "comm-invited-reward"
        , onInput = EnteredInvitedReward
        , disabled = isDisabled
        , value = defVal
        , placeholder = Nothing
        , problems = Just (getFieldProblems shared InvitedReward errors)
        , translators = translators
        }
        |> Input.withAttrs [ maxlength 255, required True ]
        |> Input.asNumeric
        |> Input.withType Input.Number
        |> Input.toHtml


viewMinimumBalance : Shared -> Bool -> String -> List Error -> Html Msg
viewMinimumBalance ({ translators } as shared) isDisabled defVal errors =
    Input.init
        { label = translators.t "community.create.labels.min_balance"
        , id = "min-balance"
        , onInput = EnteredMinimumBalance
        , disabled = isDisabled
        , value = defVal
        , placeholder = Nothing
        , problems = Just (getFieldProblems shared MinimumBalance errors)
        , translators = translators
        }
        |> Input.withAttrs [ maxlength 255, required True ]
        |> Input.asNumeric
        |> Input.withType Input.Number
        |> Input.toHtml



-- VALIDATING


validateField : (Model -> Result Error a) -> FormField -> Model -> Model
validateField validation field model =
    let
        errorsWithoutField =
            List.filter (\( errorField, _ ) -> errorField /= field) model.errors
    in
    case validation model of
        Ok _ ->
            { model | errors = errorsWithoutField }

        Err error ->
            { model | errors = error :: errorsWithoutField }


validateName : Model -> Result Error String
validateName model =
    if String.isEmpty model.name then
        Err ( CurrencyName, EmptyRequired )

    else
        Ok model.name


validateSymbol : Model -> Result Error Eos.Symbol
validateSymbol model =
    if String.isEmpty model.symbol then
        Err ( SymbolField, EmptyRequired )

    else
        case Eos.symbolFromString model.symbol of
            Nothing ->
                Err ( SymbolField, InvalidSymbol )

            Just symbol ->
                Ok symbol


validateInviterReward : Model -> Result Error Float
validateInviterReward model =
    if String.isEmpty model.inviterReward then
        Err ( InviterReward, EmptyRequired )

    else
        case String.toFloat model.inviterReward of
            Nothing ->
                Err ( InviterReward, InvalidNumber )

            Just inviterReward ->
                Ok inviterReward


validateInvitedReward : Model -> Result Error Float
validateInvitedReward model =
    if String.isEmpty model.invitedReward then
        Err ( InvitedReward, EmptyRequired )

    else
        case String.toFloat model.invitedReward of
            Nothing ->
                Err ( InvitedReward, InvalidNumber )

            Just invitedReward ->
                Ok invitedReward


validateMinimumBalance : Model -> Result Error Float
validateMinimumBalance model =
    if String.isEmpty model.minimumBalance then
        Err ( MinimumBalance, EmptyRequired )

    else
        case String.toFloat model.minimumBalance of
            Nothing ->
                Err ( MinimumBalance, InvalidNumber )

            Just minimumBalance ->
                Ok minimumBalance


validateLogoUrl : Model -> Result Error String
validateLogoUrl model =
    case List.getAt model.logoSelected model.logoList of
        Just (Uploaded logoUrl) ->
            Ok logoUrl

        Just Uploading ->
            Err ( Logo, WaitForLogoUpload )

        Nothing ->
            Err ( Logo, ChooseLogo )


{-| Assumes `Model.subdomain` is available
-}
validateModel : Shared -> Eos.Name -> Model -> Result Model ( Community.CreateCommunityData, Token.CreateTokenData )
validateModel shared accountName model =
    let
        nameValidation =
            validateName model

        symbolValidation =
            validateSymbol model

        logoValidation =
            validateLogoUrl model

        inviterRewardValidation =
            validateInviterReward model

        invitedRewardValidation =
            validateInvitedReward model

        minimumBalanceValidation =
            validateMinimumBalance model

        createCommunityData =
            Result.map5
                (\symbol logoUrl name inviterReward invitedReward ->
                    Community.createCommunityData
                        { accountName = accountName
                        , symbol = symbol
                        , logoUrl = logoUrl
                        , name = name
                        , description = model.description
                        , subdomain = Route.communityFullDomain shared model.subdomain
                        , inviterReward = inviterReward
                        , invitedReward = invitedReward
                        , hasShop = True
                        , hasObjectives = True
                        , hasKyc = False
                        , hasAutoInvite = model.hasAutoInvite
                        , website =
                            if String.startsWith "https://" model.website || String.startsWith "http://" model.website then
                                model.website

                            else
                                "http://" ++ model.website
                        }
                )
                symbolValidation
                logoValidation
                nameValidation
                inviterRewardValidation
                invitedRewardValidation

        createTokenData =
            Result.map2
                (\symbol minimumBalance ->
                    let
                        asset amount =
                            { amount = amount
                            , symbol = symbol
                            }
                    in
                    { creator = accountName
                    , maxSupply = asset 21000000.0
                    , minBalance = asset minimumBalance
                    , tokenType = Token.Mcc
                    }
                )
                symbolValidation
                minimumBalanceValidation
    in
    case Result.map2 Tuple.pair createCommunityData createTokenData of
        Ok valid ->
            Ok valid

        Err _ ->
            let
                turnToString =
                    Result.map (\_ -> "")

                errors =
                    [ nameValidation, turnToString symbolValidation, logoValidation, turnToString inviterRewardValidation, turnToString invitedRewardValidation, turnToString minimumBalanceValidation ]
                        |> List.filterMap
                            (\r ->
                                case r of
                                    Err err ->
                                        Just err

                                    Ok _ ->
                                        Nothing
                            )
            in
            Err { model | errors = errors }


getFieldProblems : Shared -> FormField -> List Error -> List String
getFieldProblems shared formField errors =
    errors
        |> List.filter (\( field, _ ) -> field == formField)
        |> List.map (Tuple.second >> errorToString shared)


errorToString : Shared -> FormError -> String
errorToString shared error =
    let
        t =
            shared.translators.t
    in
    case error of
        ChooseLogo ->
            t "error.chooseOrUploadLogo"

        WaitForLogoUpload ->
            t "error.waitForLogoUpload"

        InvalidSymbol ->
            t "error.invalidSymbol"

        EmptyRequired ->
            t "error.required"

        InvalidNumber ->
            t "error.validator.text.only_numbers"



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = EnteredName String
    | EnteredDescription String
    | EnteredSubdomain String
    | EnteredSymbol String
    | EnteredInviterReward String
    | EnteredInvitedReward String
    | EnteredMinimumBalance String
    | EnteredWebsite String
    | ToggledAutoInvite Bool
    | ClickedLogo Int
    | EnteredLogo Int (List File)
    | CompletedLogoUpload Int (Result Http.Error String)
    | SubmittedForm
    | GotDomainAvailableResponse (RemoteData (Graphql.Http.Error Bool) Bool)
    | StartedCreatingCommunity Community.CreateCommunityData Token.CreateTokenData
    | GotCreateCommunityResponse (Result Value ( Eos.Symbol, String ))
    | Redirect Community.CreateCommunityData
    | PressedEnter Bool


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        t =
            loggedIn.shared.translators.t
    in
    case msg of
        EnteredName name ->
            { model | name = name }
                |> validateField validateName CurrencyName
                |> UR.init

        EnteredDescription description ->
            { model | description = description }
                |> UR.init

        EnteredSubdomain subdomain ->
            { model | subdomain = subdomain }
                |> UR.init

        EnteredInviterReward inviterReward ->
            { model | inviterReward = inviterReward }
                |> validateField validateInviterReward InviterReward
                |> UR.init

        EnteredInvitedReward invitedReward ->
            { model | invitedReward = invitedReward }
                |> validateField validateInvitedReward InvitedReward
                |> UR.init

        EnteredMinimumBalance minimumBalance ->
            { model | minimumBalance = minimumBalance }
                |> validateField validateMinimumBalance MinimumBalance
                |> UR.init

        EnteredWebsite website ->
            { model | website = website }
                |> UR.init

        ToggledAutoInvite hasAutoInvite ->
            { model | hasAutoInvite = not hasAutoInvite }
                |> UR.init

        EnteredSymbol symbol ->
            { model | symbol = symbol }
                |> validateField validateSymbol SymbolField
                |> UR.init

        ClickedLogo index ->
            let
                newModel =
                    { model | logoSelected = index }
            in
            { newModel | isDisabled = not (isLogoUploaded newModel) }
                |> UR.init

        EnteredLogo index (file :: _) ->
            { model | logoSelected = index, logoList = model.logoList ++ [ Uploading ] }
                |> UR.init
                |> UR.addCmd (Api.uploadImage loggedIn.shared file (CompletedLogoUpload index))

        EnteredLogo _ [] ->
            UR.init model

        CompletedLogoUpload index (Err err) ->
            { model | logoList = List.removeAt index model.logoList, logoSelected = 0, isDisabled = False }
                |> UR.init
                |> UR.logHttpError msg err
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (t "settings.community_info.errors.logo_upload"))

        CompletedLogoUpload index (Ok url) ->
            let
                newModel =
                    { model | logoList = List.updateAt index (\_ -> Uploaded url) model.logoList }
            in
            { newModel
                | isDisabled = not (isLogoUploaded newModel)
            }
                |> UR.init

        SubmittedForm ->
            { model | isDisabled = True }
                |> UR.init
                |> UR.addCmd
                    (Api.Graphql.query loggedIn.shared
                        (Just loggedIn.authToken)
                        (Community.domainAvailableQuery (Route.communityFullDomain loggedIn.shared model.subdomain))
                        GotDomainAvailableResponse
                    )

        GotDomainAvailableResponse (RemoteData.Success True) ->
            case validateModel loggedIn.shared loggedIn.accountName model of
                Ok ( createCommunityData, createTokenData ) ->
                    if LoggedIn.hasPrivateKey loggedIn then
                        let
                            subscriptionDoc =
                                Community.newCommunitySubscription createCommunityData.cmmAsset.symbol
                                    |> Graphql.Document.serializeSubscription
                        in
                        { model | isDisabled = True }
                            |> UR.init
                            |> UR.addPort
                                { responseAddress = GotDomainAvailableResponse (RemoteData.Success True)
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

                    else
                        UR.init model
                            |> UR.addExt
                                (Just SubmittedForm
                                    |> RequiredAuthentication
                                )

                Err withError ->
                    UR.init withError

        GotDomainAvailableResponse (RemoteData.Success False) ->
            { model | isDisabled = False }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (loggedIn.shared.translators.t "settings.community_info.errors.url.already_taken")
                    )

        GotDomainAvailableResponse (RemoteData.Failure err) ->
            { model | isDisabled = False }
                |> UR.init
                |> UR.logGraphqlError msg err
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (loggedIn.shared.translators.t "error.unknown"))

        GotDomainAvailableResponse RemoteData.NotAsked ->
            UR.init model

        GotDomainAvailableResponse RemoteData.Loading ->
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

        GotCreateCommunityResponse (Ok ( symbol, subdomain )) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.CreatedCommunity symbol subdomain)

        GotCreateCommunityResponse (Err val) ->
            { model | isDisabled = False }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (t "error.unknown"))
                |> UR.logDebugValue msg val

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

        PressedEnter isEnter ->
            if isEnter then
                UR.init model
                    |> UR.addCmd
                        (Task.succeed SubmittedForm
                            |> Task.perform identity
                        )

            else
                UR.init model


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
                (Decode.map2 (\_ s -> s)
                    (Decode.field "transactionId" Decode.string)
                    (Decode.field "addressData"
                        (Decode.map2 Tuple.pair
                            (Decode.field "symbol" Eos.symbolDecoder)
                            (Decode.field "subdomain" Decode.string)
                        )
                    )
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
        EnteredName _ ->
            [ "EnteredName" ]

        EnteredDescription _ ->
            [ "EnteredDescription" ]

        EnteredSubdomain _ ->
            [ "EnteredSubdomain" ]

        EnteredSymbol _ ->
            [ "EnteredSymbol" ]

        EnteredInvitedReward _ ->
            [ "EnteredInvitedReward" ]

        EnteredInviterReward _ ->
            [ "EnteredInviterReward" ]

        EnteredMinimumBalance _ ->
            [ "EnteredMinimumBalance" ]

        EnteredWebsite _ ->
            [ "EnteredWebsite" ]

        ToggledAutoInvite _ ->
            [ "ToggledAutoInvite" ]

        ClickedLogo _ ->
            [ "ClickedLogo" ]

        EnteredLogo _ _ ->
            [ "EnteredLogo" ]

        CompletedLogoUpload _ r ->
            [ "CompletedLogoUpload", UR.resultToString r ]

        SubmittedForm ->
            [ "SubmittedForm" ]

        GotDomainAvailableResponse r ->
            [ "GotDomainAvailableResponse", UR.remoteDataToString r ]

        StartedCreatingCommunity _ _ ->
            [ "StartedCreatingCommunity" ]

        GotCreateCommunityResponse _ ->
            [ "GotCreateCommunityResponse" ]

        Redirect _ ->
            [ "Redirect" ]

        PressedEnter _ ->
            [ "PressedEnter" ]
