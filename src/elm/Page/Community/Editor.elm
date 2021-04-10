module Page.Community.Editor exposing
    ( Model
    , Msg
    , initEdit
    , initNew
    , jsAddressToMsg
    , msgToString
    , subscriptions
    , update
    , view
    )

import Api
import Api.Graphql
import Asset.Icon as Icon
import Browser.Events as Events
import Community exposing (Model)
import Dict exposing (Dict)
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Graphql.Document
import Graphql.Http
import Html exposing (Html, br, button, div, input, label, span, text, textarea)
import Html.Attributes exposing (accept, class, classList, disabled, for, id, maxlength, minlength, multiple, placeholder, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Task
import UpdateResult as UR
import Utils exposing (decodeEnterKeyDown)
import View.Feedback as Feedback



-- INIT


initNew : LoggedIn.Model -> ( Model, Cmd Msg )
initNew _ =
    ( EditingNew Dict.empty newForm
    , Cmd.none
    )


initEdit : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
initEdit { shared, authToken } symbol =
    ( Loading symbol
    , Api.Graphql.query shared (Just authToken) (Community.communityQuery symbol) CompletedCommunityLoad
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map PressedEnter (Events.onKeyDown decodeEnterKeyDown)



-- MODEL


type alias Model =
    Status


type
    Status
    -- Edit Community
    = Loading Symbol
    | NotFound
    | LoadingFailed (Graphql.Http.Error (Maybe Community.Model))
    | Unauthorized Community.Model
    | Editing Community.Model (Dict String FormError) Form
    | WaitingEditLogoUpload Community.Model Form
    | Saving Community.Model Form
      -- New Community
    | EditingNew (Dict String FormError) Form
    | WaitingNewLogoUpload Form
    | Creating Form


type alias Form =
    { name : String
    , description : String
    , symbol : String
    , logoSelected : Int
    , logoList : List LogoStatus
    , inviterReward : String
    , invitedReward : String
    , minBalance : String
    , hasShop : Bool
    , hasObjectives : Bool
    , hasKyc : Bool
    }


newForm : Form
newForm =
    { name = ""
    , description = ""
    , symbol = ""
    , logoSelected = 0
    , logoList = defaultLogos
    , inviterReward = "0"
    , invitedReward = "10"
    , minBalance = "-100"
    , hasShop = True
    , hasObjectives = True
    , hasKyc = False
    }


editForm : Community.Model -> Form
editForm community =
    let
        ( logoSelected, logoList ) =
            case List.elemIndex (Uploaded community.logo) defaultLogos of
                Just index ->
                    ( index
                    , defaultLogos
                    )

                Nothing ->
                    ( List.length defaultLogos
                    , defaultLogos ++ [ Uploaded community.logo ]
                    )
    in
    { name = community.title
    , description = community.description
    , symbol = Eos.symbolToString community.symbol
    , logoSelected = logoSelected
    , logoList = logoList
    , inviterReward = String.fromFloat community.inviterReward
    , invitedReward = String.fromFloat community.invitedReward
    , minBalance = String.fromFloat (community.minBalance |> Maybe.withDefault 0.0)
    , hasShop = community.hasShop
    , hasObjectives = community.hasObjectives
    , hasKyc = community.hasKyc
    }


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
    | UploadingFailed Http.Error
    | Uploaded String


type FormError
    = ChooseOrUploadLogo
    | InternalError
    | InvalidSymbol


type FormStatus
    = Valid Community.CreateCommunityData
    | Invalid (Dict String FormError)
    | UploadingLogo


encodeForm : LoggedIn.Model -> Form -> FormStatus
encodeForm loggedIn form =
    case List.getAt form.logoSelected form.logoList of
        Just Uploading ->
            UploadingLogo

        Just (Uploaded logoUrl) ->
            encodeFormHelper logoUrl loggedIn form

        Just (UploadingFailed _) ->
            Invalid (Dict.singleton fieldLogoId ChooseOrUploadLogo)

        Nothing ->
            encodeFormHelper "" loggedIn form


encodeFormHelper : String -> LoggedIn.Model -> Form -> FormStatus
encodeFormHelper logoUrl { accountName } form =
    let
        maybeSymbol =
            Eos.symbolFromString form.symbol
    in
    case maybeSymbol of
        Nothing ->
            Invalid (Dict.singleton fieldSymbolId InvalidSymbol)

        Just symbol ->
            { accountName = accountName
            , symbol = symbol
            , logoUrl = logoUrl
            , name = form.name
            , description = form.description
            , inviterReward = String.toFloat form.inviterReward |> Maybe.withDefault 0.0
            , invitedReward = String.toFloat form.invitedReward |> Maybe.withDefault 0.0
            , minBalance = String.toFloat form.minBalance |> Maybe.withDefault 0.0
            , hasShop = form.hasShop
            , hasObjectives = form.hasObjectives
            , hasKyc = form.hasKyc
            }
                |> Community.createCommunityData
                |> Valid



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        t =
            shared.translators.t

        content =
            case model of
                Loading _ ->
                    Page.fullPageLoading shared

                LoadingFailed e ->
                    Page.fullPageGraphQLError (t "community.edit.title") e

                NotFound ->
                    Page.viewCardEmpty [ text "Community not found" ]

                Unauthorized _ ->
                    div [ class "container mx-auto px-4" ]
                        [ Page.viewTitle (t "community.edit.title")
                        , div [ class "card" ]
                            [ text (t "community.edit.unauthorized") ]
                        ]

                Editing _ problems form ->
                    viewForm loggedIn True False problems form model

                WaitingEditLogoUpload _ form ->
                    viewForm loggedIn True True Dict.empty form model

                Saving _ form ->
                    viewForm loggedIn True True Dict.empty form model

                EditingNew problems form ->
                    viewForm loggedIn False False problems form model

                WaitingNewLogoUpload form ->
                    viewForm loggedIn False True Dict.empty form model

                Creating form ->
                    viewForm loggedIn False True Dict.empty form model
    in
    { title = t "community.edit.title"
    , content = content
    }


viewForm : LoggedIn.Model -> Bool -> Bool -> Dict String FormError -> Form -> Model -> Html Msg
viewForm ({ shared } as loggedIn) isEdit isDisabled errors form model =
    let
        t =
            shared.translators.t

        ( titleText, actionText ) =
            if isEdit then
                ( t "community.edit.title", t "community.edit.submit" )

            else
                ( t "community.create.title", t "community.create.submit" )

        cmd =
            case model of
                EditingNew _ _ ->
                    NewCommunitySubscription form.symbol

                _ ->
                    ClickedSave
    in
    div [ class "bg-white pb-10" ]
        [ Page.viewHeader loggedIn titleText Route.Dashboard
        , Html.form
            [ class "container mx-auto px-4"
            , onSubmit cmd
            ]
            [ div [ class "my-10" ]
                [ viewFieldDescription shared isDisabled form.description errors
                , viewFieldCurrencyName shared isDisabled form.name errors
                , div [ class "flex flex-row mt-4" ]
                    [ div [ class "w-1/2 pr-2" ]
                        [ viewFieldCurrencySymbol shared (isEdit || isDisabled) form.symbol errors
                        , viewFieldInviterReward shared isDisabled form.inviterReward errors
                        ]
                    , div [ class "w-1/2 pl-2" ]
                        [ viewFieldInvitedReward shared isDisabled form.invitedReward errors
                        , viewFieldMinBalance shared isDisabled form.minBalance errors
                        ]
                    ]
                , viewFieldLogo shared isDisabled form.logoSelected form.logoList errors
                , viewFieldError shared "form" errors
                ]
            , button
                [ class "button button-primary w-full"
                , disabled isDisabled
                ]
                [ text actionText ]
            ]
        ]


viewFieldDescription : Shared -> Bool -> String -> Dict String FormError -> Html Msg
viewFieldDescription shared isDisabled defVal errors =
    let
        id_ =
            "comm-description"

        t =
            shared.translators.t
    in
    formField
        [ span [ class "input-label" ]
            [ text (t "community.create.labels.description")
            , br [] []
            , span [] [ text (t "community.create.tooltips.description") ]
            ]
        , textarea
            [ class "w-full input rounded-sm"
            , id id_
            , value defVal
            , maxlength 255
            , onInput EnteredDescription
            , disabled isDisabled
            ]
            []
        , viewFieldError shared id_ errors
        ]


viewFieldCurrencyName : Shared -> Bool -> String -> Dict String FormError -> Html Msg
viewFieldCurrencyName shared isDisabled defVal errors =
    let
        id_ =
            "comm-currency-name"

        t =
            shared.translators.t
    in
    formField
        [ span [ class "input-label" ]
            [ text <| t "community.create.labels.currency_name" ]
        , input
            [ class "w-full input rounded-sm"
            , id id_
            , value defVal
            , maxlength 255
            , required True
            , onInput EnteredTitle
            , disabled isDisabled
            ]
            []
        , viewFieldError shared id_ errors
        ]


viewFieldCurrencySymbol : Shared -> Bool -> String -> Dict String FormError -> Html Msg
viewFieldCurrencySymbol shared isDisabled defVal errors =
    let
        t =
            shared.translators.t
    in
    formField
        [ span [ class "input-label" ]
            [ text <| t "community.create.labels.currency_symbol" ]
        , input
            [ class "w-full input rounded-sm"
            , id fieldSymbolId
            , value defVal
            , minlength 5
            , maxlength 6
            , required True
            , onInput EnteredSymbol
            , disabled isDisabled
            , placeholder "_ , _ _ _ _"
            ]
            []
        , viewFieldError shared fieldSymbolId errors
        ]


viewFieldLogo : Shared -> Bool -> Int -> List LogoStatus -> Dict String FormError -> Html Msg
viewFieldLogo shared isDisabled selected logos errors =
    let
        t =
            shared.translators.t

        logoClass s =
            "create-community-logo-list-item" ++ s

        item index logoStatus =
            button
                [ classList
                    [ ( logoClass "", True )
                    , ( logoClass "--selected", index == selected )
                    ]
                , type_ "button"
                , disabled isDisabled
                , onClick (ClickedLogo index)
                ]
                [ case logoStatus of
                    Uploading ->
                        div [ class "spinner" ] []

                    UploadingFailed _ ->
                        span [] [ text (t "error.unknown") ]

                    Uploaded url ->
                        div
                            [ class (logoClass "-img")
                            , Community.logoBackground (Just url)
                            ]
                            []
                ]
    in
    div [ class "create-community-logo-list mt-8" ]
        (List.indexedMap item logos
            ++ [ div
                    [ classList
                        [ ( "create-community-logo-upload", True ) ]
                    , type_ "button"
                    ]
                    [ input
                        [ id fieldLogoId
                        , class "hidden-img-input"
                        , type_ "file"
                        , accept "image/*"
                        , Page.onFileChange (EnteredLogo (List.length logos))
                        , multiple False
                        , disabled isDisabled
                        ]
                        []
                    , label
                        [ for fieldLogoId
                        , classList [ ( "disabled", isDisabled ) ]
                        ]
                        [ div [ class "create-community-logo-upload-svg" ] [ Icon.imageMultiple "" ]
                        , text (t "community.create.labels.upload_icon")
                        ]
                    ]
               , viewFieldError shared fieldLogoId errors
               ]
        )


viewFieldInviterReward : Shared -> Bool -> String -> Dict String FormError -> Html Msg
viewFieldInviterReward shared isDisabled defVal errors =
    let
        id_ =
            "comm-inviter-reward"

        t =
            shared.translators.t
    in
    formField
        [ span [ class "input-label" ]
            [ text <| t "community.create.labels.inviter_reward" ]
        , input
            [ class "w-full input rounded-sm"
            , id id_
            , value defVal
            , maxlength 255
            , required True
            , onInput EnteredInviterReward
            , disabled isDisabled
            ]
            []
        , viewFieldError shared id_ errors
        ]


viewFieldInvitedReward : Shared -> Bool -> String -> Dict String FormError -> Html Msg
viewFieldInvitedReward shared isDisabled defVal errors =
    let
        id_ =
            "comm-invited-reward"

        t =
            shared.translators.t
    in
    formField
        [ span [ class "input-label" ]
            [ text <| t "community.create.labels.invited_reward" ]
        , input
            [ class "w-full input rounded-sm"
            , value defVal
            , maxlength 255
            , required True
            , onInput EnteredInvitedReward
            , disabled isDisabled
            ]
            []
        , viewFieldError shared id_ errors
        ]


viewFieldMinBalance : Shared -> Bool -> String -> Dict String FormError -> Html Msg
viewFieldMinBalance shared isDisabled defVal errors =
    let
        id_ =
            "min-balance"

        t =
            shared.translators.t
    in
    formField
        [ span [ class "input-label" ] [ text <| t "community.create.labels.min_balance" ]
        , input
            [ class "w-full input rounded-sm"
            , value defVal
            , maxlength 255
            , required True
            , onInput EnteredMinBalance
            , disabled isDisabled
            ]
            []
        , viewFieldError shared id_ errors
        ]


fieldLogoId : String
fieldLogoId =
    "community-editor-logo-upload"


fieldSymbolId : String
fieldSymbolId =
    "comm-currency-symbol"



-- HELPERS


formField : List (Html msg) -> Html msg
formField =
    div [ class "form-field" ]


viewFieldError : Shared -> String -> Dict String FormError -> Html msg
viewFieldError shared fieldId errors =
    case Dict.get fieldId errors of
        Just e ->
            span [ class "field-error" ]
                [ text (errorToString shared e) ]

        Nothing ->
            text ""


errorToString : Shared -> FormError -> String
errorToString shared v =
    let
        t =
            shared.translators.t
    in
    case v of
        ChooseOrUploadLogo ->
            t "error.chooseOrUploadLogo"

        InternalError ->
            t "error.unknown"

        InvalidSymbol ->
            t "error.invalidSymbol"



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedCommunityLoad (RemoteData (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | EnteredTitle String
    | EnteredDescription String
    | EnteredSymbol String
    | EnteredInviterReward String
    | EnteredInvitedReward String
    | EnteredMinBalance String
    | ClickedLogo Int
    | EnteredLogo Int (List File)
    | CompletedLogoUpload Int (Result Http.Error String)
    | ClickedSave
    | GotSaveResponse (Result Value Symbol)
      -- New Community
    | NewCommunitySubscription String
    | Redirect
    | PressedEnter Bool


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        t =
            loggedIn.shared.translators.t
    in
    case msg of
        CompletedCommunityLoad (RemoteData.Success community) ->
            case community of
                Just c ->
                    if LoggedIn.isAccount c.creator loggedIn then
                        Editing c Dict.empty (editForm c)
                            |> UR.init

                    else
                        Unauthorized c
                            |> UR.init

                Nothing ->
                    NotFound
                        |> UR.init

        CompletedCommunityLoad (RemoteData.Failure err) ->
            LoadingFailed err
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedCommunityLoad _ ->
            UR.init model

        EnteredTitle input ->
            UR.init model
                |> updateForm (\form -> { form | name = input })

        EnteredDescription input ->
            UR.init model
                |> updateForm (\form -> { form | description = input })

        EnteredInviterReward input ->
            UR.init model
                |> updateForm (\form -> { form | inviterReward = input })

        EnteredInvitedReward input ->
            UR.init model
                |> updateForm (\form -> { form | invitedReward = input })

        EnteredMinBalance input ->
            UR.init model
                |> updateForm (\f -> { f | minBalance = input })

        EnteredSymbol input ->
            UR.init model
                |> updateForm (\form -> { form | symbol = input })

        ClickedLogo index ->
            UR.init model
                |> updateForm (\form -> { form | logoSelected = index })

        NewCommunitySubscription stringSymbol ->
            case Eos.symbolFromString stringSymbol of
                Nothing ->
                    model
                        |> UR.init

                Just s ->
                    let
                        subscriptionDoc =
                            Community.newCommunitySubscription s
                                |> Graphql.Document.serializeSubscription
                    in
                    model
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = NewCommunitySubscription stringSymbol
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "subscribeToNewCommunity" )
                                    , ( "subscription", Encode.string subscriptionDoc )
                                    ]
                            }

        ClickedSave ->
            if LoggedIn.hasPrivateKey loggedIn then
                save msg loggedIn (UR.init model)

            else
                UR.init model
                    |> UR.addExt
                        (Just ClickedSave
                            |> RequiredAuthentication
                        )

        EnteredLogo index (file :: _) ->
            UR.init model
                |> UR.addCmd (Api.uploadImage loggedIn.shared file (CompletedLogoUpload index))
                |> updateForm
                    (\form ->
                        { form
                            | logoSelected = index
                            , logoList = form.logoList ++ [ Uploading ]
                        }
                    )

        EnteredLogo _ [] ->
            UR.init model

        CompletedLogoUpload index resultUrl ->
            let
                ( newLogoUrl, addHttpError ) =
                    case resultUrl of
                        Ok url ->
                            ( Uploaded url, identity )

                        Err err ->
                            ( UploadingFailed err
                            , UR.logHttpError msg err
                            )

                uResult =
                    updateForm
                        (\form ->
                            { form
                                | logoList = List.setAt index newLogoUrl form.logoList
                            }
                        )
                        (UR.init model)
                        |> addHttpError
            in
            case model of
                WaitingEditLogoUpload _ _ ->
                    save msg loggedIn uResult

                WaitingNewLogoUpload _ ->
                    save msg loggedIn uResult

                _ ->
                    uResult

        GotSaveResponse (Ok symbol) ->
            case model of
                Saving _ _ ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Route.Community symbol
                                |> Route.replaceUrl loggedIn.shared.navKey
                            )
                        |> UR.addExt (ShowFeedback Feedback.Success (t "community.create.success"))

                _ ->
                    model
                        |> UR.init

        GotSaveResponse (Err val) ->
            let
                err =
                    Dict.singleton "form" InternalError
            in
            case model of
                Saving community form ->
                    Editing community err form
                        |> UR.init
                        |> UR.logDebugValue msg val
                        |> UR.addExt (ShowFeedback Feedback.Failure (t "error.unknown"))

                Creating form ->
                    EditingNew err form
                        |> UR.init
                        |> UR.logDebugValue msg val
                        |> UR.addExt (ShowFeedback Feedback.Failure (t "error.unknown"))

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []
                        |> UR.logDebugValue msg val
                        |> UR.addExt (ShowFeedback Feedback.Failure (t "error.unknown"))

        Redirect ->
            case model of
                Creating form ->
                    let
                        sym =
                            Eos.symbolFromString form.symbol
                    in
                    case sym of
                        Just s ->
                            model
                                |> UR.init
                                |> UR.addCmd
                                    (Route.Community s
                                        |> Route.replaceUrl loggedIn.shared.navKey
                                    )

                        Nothing ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg []

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        PressedEnter val ->
            if val then
                UR.init model
                    |> UR.addCmd
                        (Task.succeed ClickedSave
                            |> Task.perform identity
                        )

            else
                UR.init model


updateForm : (Form -> Form) -> UpdateResult -> UpdateResult
updateForm transform ({ model } as uResult) =
    case model of
        Loading _ ->
            uResult

        LoadingFailed _ ->
            uResult

        NotFound ->
            uResult

        Unauthorized _ ->
            uResult

        Editing community errors form ->
            Editing community errors (transform form)
                |> UR.setModel uResult

        WaitingEditLogoUpload community form ->
            WaitingEditLogoUpload community (transform form)
                |> UR.setModel uResult

        Saving community form ->
            Saving community (transform form)
                |> UR.setModel uResult

        EditingNew errors form ->
            EditingNew errors (transform form)
                |> UR.setModel uResult

        WaitingNewLogoUpload form ->
            WaitingNewLogoUpload (transform form)
                |> UR.setModel uResult

        Creating form ->
            Creating (transform form)
                |> UR.setModel uResult


save : Msg -> LoggedIn.Model -> UpdateResult -> UpdateResult
save msg loggedIn ({ model } as uResult) =
    let
        save_ form toLoading toUploadingLogo toError isEdit =
            case encodeForm loggedIn form of
                Valid createAction ->
                    let
                        authorization =
                            { actor = loggedIn.accountName
                            , permissionName = Eos.samplePermission
                            }
                    in
                    toLoading form
                        |> UR.setModel uResult
                        |> UR.addPort
                            (if isEdit then
                                { responseAddress = ClickedSave
                                , responseData = Encode.string form.symbol
                                , data =
                                    Eos.encodeTransaction
                                        [ { accountName = loggedIn.shared.contracts.community
                                          , name = "update"
                                          , authorization = authorization
                                          , data =
                                                { asset = createAction.cmmAsset
                                                , logo = createAction.logoUrl
                                                , name = createAction.name
                                                , description = createAction.description
                                                , inviterReward = createAction.inviterReward
                                                , invitedReward = createAction.invitedReward
                                                , hasObjectives = 1
                                                , hasShop = 1

                                                -- , hasKyc = 0
                                                }
                                                    |> Community.encodeUpdateLogoData
                                          }
                                        ]
                                }

                             else
                                { responseAddress = ClickedSave
                                , responseData = Encode.string form.symbol
                                , data =
                                    Eos.encodeTransaction
                                        [ { accountName = loggedIn.shared.contracts.community
                                          , name = "create"
                                          , authorization = authorization
                                          , data =
                                                createAction
                                                    |> Community.encodeCreateCommunityData
                                          }
                                        , { accountName = loggedIn.shared.contracts.token
                                          , name = "create"
                                          , authorization = authorization
                                          , data =
                                                { creator = loggedIn.accountName
                                                , maxSupply = { amount = 21000000.0, symbol = createAction.cmmAsset.symbol }
                                                , minBalance = createAction.minBalance
                                                , tokenType = "mcc"
                                                }
                                                    |> Community.encodeCreateTokenData
                                          }
                                        ]
                                }
                            )

                UploadingLogo ->
                    toUploadingLogo form
                        |> UR.setModel uResult

                Invalid problems ->
                    toError problems form
                        |> UR.setModel uResult
    in
    case model of
        Editing community _ form ->
            save_ form (Saving community) (WaitingEditLogoUpload community) (Editing community) True

        WaitingEditLogoUpload community form ->
            save_ form (Saving community) (WaitingEditLogoUpload community) (Editing community) True

        EditingNew _ form ->
            save_ form Creating WaitingNewLogoUpload EditingNew False

        WaitingNewLogoUpload form ->
            save_ form Creating WaitingNewLogoUpload EditingNew False

        _ ->
            uResult
                |> UR.logImpossible msg []


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClickedSave" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.map2 (\_ symbol -> Ok symbol)
                        (Decode.field "transactionId" Decode.string)
                        (Decode.at [ "addressData" ] Eos.symbolDecoder)
                    , Decode.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotSaveResponse)
                |> Result.withDefault Nothing

        "NewCommunitySubscription" :: [] ->
            let
                resp =
                    Decode.decodeValue
                        (Decode.field "state" Decode.string)
                        val
                        |> Result.withDefault ""
            in
            case resp of
                "starting" ->
                    Just ClickedSave

                "responded" ->
                    Just Redirect

                _ ->
                    Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedCommunityLoad r ->
            [ "CompletedCommunityLoad", UR.remoteDataToString r ]

        EnteredTitle _ ->
            [ "EnteredTitle" ]

        EnteredDescription _ ->
            [ "EnteredDescription" ]

        EnteredSymbol _ ->
            [ "EnteredSymbol" ]

        EnteredInvitedReward _ ->
            [ "EnteredInvitedReward" ]

        EnteredInviterReward _ ->
            [ "EnteredInviterReward" ]

        EnteredMinBalance _ ->
            [ "EnteredMinBalance" ]

        ClickedLogo _ ->
            [ "ClickedLogo" ]

        EnteredLogo _ _ ->
            [ "EnteredLogo" ]

        CompletedLogoUpload _ r ->
            [ "CompletedLogoUpload", UR.resultToString r ]

        NewCommunitySubscription _ ->
            [ "NewCommunitySubscription" ]

        ClickedSave ->
            [ "ClickedSave" ]

        GotSaveResponse r ->
            [ "GotSaveResponse", UR.resultToString r ]

        Redirect ->
            [ "Redirect" ]

        PressedEnter _ ->
            [ "PressedEnter" ]
