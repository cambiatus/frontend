module Page.Community.Editor exposing (Model, Msg, initEdit, initNew, jsAddressToMsg, msgToString, subscriptions, update, view)

import Account
import Api
import Api.Graphql
import Asset.Icon as Icon
import Community exposing (Community)
import Dict exposing (Dict)
import Eos as Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Graphql.Document
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, targetValue)
import Http
import I18Next
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value, object, string)
import List.Extra as List
import Log
import Page
import Ports
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared exposing (Shared)
import UpdateResult as UR



-- INIT


initNew : LoggedIn.Model -> ( Model, Cmd Msg )
initNew loggedIn =
    ( { status = EditingNew Dict.empty newForm
      }
    , Cmd.none
    )


initEdit : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
initEdit ({ shared } as loggedIn) symbol =
    ( { status = Loading symbol
      }
    , Api.Graphql.query shared (Community.communityQuery symbol) CompletedCommunityLoad
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { status : Status
    }


type
    Status
    -- Edit Community
    = Loading Symbol
    | NotFound
    | LoadingFailed (Graphql.Http.Error (Maybe Community))
    | Unauthorized Community
    | Editing Community (Dict String FormError) Form
    | WaitingEditLogoUpload Community Form
    | Saving Community Form
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
    , inviterReward : Int
    , invitedReward : Int
    }


newForm : Form
newForm =
    { name = ""
    , description = ""
    , symbol = ""
    , logoSelected = 0
    , logoList = defaultLogos
    , inviterReward = 0
    , invitedReward = 10
    }


editForm : Community -> Form
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
    , inviterReward = 0
    , invitedReward = 0
    }


defaultLogos : List LogoStatus
defaultLogos =
    [ Uploaded "QmXfNDMypPFQ5ysmR2xQSKJ4tbNW3Ee5YfF7vozJ1hRAbn"
    , Uploaded "QmYgwc7vNXWwEvLwe9NKcKdYt8mbhFW52Xm9MZYTbV6LBx"
    , Uploaded "Qma7hrece5teG6D4D5dy7Qof8Acznoutw6Xes1xuCDm75d"
    , Uploaded "Qmcqdan49Sc49jecs58873z9uXK3acKEXoeHjsvfKWUWCy"
    , Uploaded "QmeEzsyEN7CxDRN7Q3UgfoQ4dNu7hVLqtd6k3vWLXp3hrY"
    ]


type LogoStatus
    = Uploading
    | UploadingFailed Http.Error
    | Uploaded String


type FormError
    = Required
    | TooShort Int
    | TooLong Int
    | InvalidChar Char
    | AlreadyTaken
    | ChooseOrUploadLogo
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

        Just (Uploaded logoHash) ->
            encodeFormHelper logoHash loggedIn form

        Just (UploadingFailed _) ->
            Invalid (Dict.singleton fieldLogoId ChooseOrUploadLogo)

        Nothing ->
            encodeFormHelper "" loggedIn form


encodeFormHelper : String -> LoggedIn.Model -> Form -> FormStatus
encodeFormHelper logoHash { shared, accountName } form =
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
            , logoHash = logoHash
            , name = form.name
            , description = form.description
            }
                |> Community.createCommunityData
                |> Valid



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    let
        defaultContainer =
            div [ class "container mx-auto px-4" ]

        shared =
            loggedIn.shared

        t str =
            I18Next.t shared.translations str
    in
    case model.status of
        Loading _ ->
            Page.fullPageLoading

        LoadingFailed e ->
            Page.fullPageGraphQLError (t "community.edit.title") e

        NotFound ->
            Page.viewCardEmpty [ text "Community not found" ]

        Unauthorized community ->
            defaultContainer
                [ Page.viewTitle (t "community.edit.title")
                , div [ class "card" ]
                    [ text (t "community.edit.unauthorized") ]
                ]

        Editing _ problems form ->
            viewForm shared True False problems form model

        WaitingEditLogoUpload _ form ->
            viewForm shared True True Dict.empty form model

        Saving _ form ->
            viewForm shared True True Dict.empty form model

        EditingNew problems form ->
            viewForm shared False False problems form model

        WaitingNewLogoUpload form ->
            viewForm shared False True Dict.empty form model

        Creating form ->
            viewForm shared False True Dict.empty form model


viewForm : Shared -> Bool -> Bool -> Dict String FormError -> Form -> Model -> Html Msg
viewForm shared isEdit isDisabled errors form model =
    let
        t =
            I18Next.t shared.translations

        ( titleText, actionText ) =
            if isEdit then
                ( t "community.edit.title", t "community.edit.submit" )

            else
                ( t "community.create.title", t "community.create.submit" )

        cmd =
            case model.status of
                EditingNew _ _ ->
                    NewCommunitySubscription form.symbol

                _ ->
                    ClickedSave
    in
    Html.form
        [ class "container mx-auto px-4"
        , onSubmit cmd
        ]
        [ Page.viewTitle titleText
        , div [ class "card card--form" ]
            [ viewFieldDescription shared isDisabled form.description errors
            , div [ class "create-community-two-column" ]
                [ viewFieldCurrencyName shared isDisabled form.name errors
                , viewFieldCurrencySymbol shared (isEdit || isDisabled) form.symbol errors
                ]
            , viewFieldLogo shared isDisabled form.logoSelected form.logoList errors
            , viewFieldError shared "form" errors
            ]
        , button
            [ class "btn btn--primary btn--big"
            , disabled isDisabled
            ]
            [ text actionText ]
        ]


viewFieldDescription : Shared -> Bool -> String -> Dict String FormError -> Html Msg
viewFieldDescription shared isDisabled defVal errors =
    let
        id_ =
            "comm-description"

        t =
            I18Next.t shared.translations
    in
    formField
        [ label [ for id_ ]
            [ text (t "community.create.labels.description")
            , br [] []
            , span [] [ text (t "community.create.tooltips.description") ]
            ]
        , textarea
            [ class "input"
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
            I18Next.t shared.translations
    in
    formField
        [ Page.labelWithTooltip id_
            (t "community.create.labels.currency_name")
            (t "community.create.tooltips.currency_name")
        , input
            [ class "input"
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
            I18Next.t shared.translations
    in
    formField
        [ Page.labelWithTooltip fieldSymbolId
            (t "community.create.labels.currency_symbol")
            (t "community.create.tooltips.currency_symbol")
        , input
            [ class "input input-symbol"
            , id fieldSymbolId
            , value defVal
            , minlength 3
            , maxlength 4
            , required True
            , onInput EnteredSymbol
            , disabled isDisabled
            , placeholder "____"
            ]
            []
        , viewFieldError shared fieldSymbolId errors
        ]


viewFieldLogo : Shared -> Bool -> Int -> List LogoStatus -> Dict String FormError -> Html Msg
viewFieldLogo shared isDisabled selected logos errors =
    let
        ipfsUrl =
            shared.endpoints.ipfs

        t =
            I18Next.t shared.translations

        logoClass s =
            "create-community-logo-list-item" ++ s

        item index logoStatus =
            button
                [ classList
                    [ ( logoClass "", True )
                    , ( logoClass "--selected"
                      , index == selected
                      )
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

                    Uploaded hash ->
                        div
                            [ class (logoClass "-img")
                            , Community.logoBackground ipfsUrl (Just hash)
                            ]
                            []
                ]
    in
    div [ class "create-community-logo-list" ]
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
        t s =
            I18Next.t shared.translations s

        tr str values =
            I18Next.tr shared.translations I18Next.Curly str values
    in
    case v of
        Required ->
            t "error.required"

        TooShort i ->
            tr "error.tooShort" [ ( "minLength", String.fromInt i ) ]

        TooLong i ->
            tr "error.tooLong" [ ( "maxLength", String.fromInt i ) ]

        InvalidChar c ->
            tr "error.invalidChar" [ ( "invalidChar", String.fromChar c ) ]

        AlreadyTaken ->
            t "error.alreadyTaken"

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
    = CompletedCommunityLoad (Result (Graphql.Http.Error (Maybe Community)) (Maybe Community))
    | EnteredTitle String
    | EnteredDescription String
    | EnteredSymbol String
    | ClickedLogo Int
    | EnteredLogo Int (List File)
    | CompletedLogoUpload Int (Result Http.Error String)
    | ClickedSave
    | GotSaveResponse (Result Value Symbol)
      -- New Community
    | NewCommunitySubscription String
    | Redirect


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedCommunityLoad (Ok community) ->
            case community of
                Just c ->
                    if LoggedIn.isAccount c.creator loggedIn then
                        Editing c Dict.empty (editForm c)
                            |> updateStatus model
                            |> UR.init

                    else
                        { model | status = Unauthorized c }
                            |> UR.init

                Nothing ->
                    { model | status = NotFound }
                        |> UR.init

        CompletedCommunityLoad (Err err) ->
            { model | status = LoadingFailed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        EnteredTitle input ->
            UR.init model
                |> updateForm (\form -> { form | name = input })

        EnteredDescription input ->
            UR.init model
                |> updateForm (\form -> { form | description = input })

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
            if LoggedIn.isAuth loggedIn then
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

        CompletedLogoUpload index resultHash ->
            let
                ( newLogoHash, addHttpError ) =
                    case resultHash of
                        Ok hash ->
                            ( Uploaded hash, identity )

                        Err err ->
                            ( UploadingFailed err
                            , UR.logHttpError msg err
                            )

                uResult =
                    updateForm
                        (\form ->
                            { form
                                | logoList = List.setAt index newLogoHash form.logoList
                            }
                        )
                        (UR.init model)
                        |> addHttpError
            in
            case model.status of
                WaitingEditLogoUpload _ _ ->
                    save msg loggedIn uResult

                WaitingNewLogoUpload _ ->
                    save msg loggedIn uResult

                _ ->
                    uResult

        GotSaveResponse (Ok symbol) ->
            case model.status of
                Saving _ _ ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Route.Community symbol
                                |> Route.replaceUrl loggedIn.shared.navKey
                            )

                _ ->
                    model
                        |> UR.init

        GotSaveResponse (Err val) ->
            let
                err =
                    Dict.singleton "form" InternalError
            in
            case model.status of
                Saving community form ->
                    { model | status = Editing community err form }
                        |> UR.init
                        |> UR.logDebugValue msg val

                Creating form ->
                    { model | status = EditingNew err form }
                        |> UR.init
                        |> UR.logDebugValue msg val

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []
                        |> UR.logDebugValue msg val

        Redirect ->
            case model.status of
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


updateStatus : Model -> Status -> Model
updateStatus model status =
    { model | status = status }


updateForm : (Form -> Form) -> UpdateResult -> UpdateResult
updateForm transform ({ model } as uResult) =
    case model.status of
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
                |> updateStatus model
                |> UR.setModel uResult

        WaitingEditLogoUpload community form ->
            WaitingEditLogoUpload community (transform form)
                |> updateStatus model
                |> UR.setModel uResult

        Saving community form ->
            Saving community (transform form)
                |> updateStatus model
                |> UR.setModel uResult

        EditingNew errors form ->
            EditingNew errors (transform form)
                |> updateStatus model
                |> UR.setModel uResult

        WaitingNewLogoUpload form ->
            WaitingNewLogoUpload (transform form)
                |> updateStatus model
                |> UR.setModel uResult

        Creating form ->
            Creating (transform form)
                |> updateStatus model
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
                        |> updateStatus model
                        |> UR.setModel uResult
                        |> UR.addPort
                            (if isEdit then
                                { responseAddress = ClickedSave
                                , responseData = Encode.string form.symbol
                                , data =
                                    Eos.encodeTransaction
                                        { actions =
                                            [ { accountName = "bes.cmm"
                                              , name = "update"
                                              , authorization = authorization
                                              , data =
                                                    { asset = createAction.cmmAsset
                                                    , logo = createAction.logoHash
                                                    , name = createAction.name
                                                    , description = createAction.description
                                                    , inviterReward = createAction.inviterReward
                                                    , invitedReward = createAction.invitedReward
                                                    }
                                                        |> Community.encodeUpdateLogoData
                                              }
                                            ]
                                        }
                                }

                             else
                                { responseAddress = ClickedSave
                                , responseData = Encode.string form.symbol
                                , data =
                                    Eos.encodeTransaction
                                        { actions =
                                            [ { accountName = "bes.cmm"
                                              , name = "create"
                                              , authorization = authorization
                                              , data =
                                                    createAction
                                                        |> Community.encodeCreateCommunityData
                                              }
                                            , { accountName = "bes.token"
                                              , name = "create"
                                              , authorization = authorization
                                              , data =
                                                    { creator = loggedIn.accountName
                                                    , maxSupply = { amount = 21000000.0, symbol = createAction.cmmAsset.symbol }
                                                    , minBalance = { amount = -1000.0, symbol = createAction.cmmAsset.symbol }
                                                    , tokenType = "mcc"
                                                    }
                                                        |> Community.encodeCreateTokenData
                                              }
                                            ]
                                        }
                                }
                            )

                UploadingLogo ->
                    toUploadingLogo form
                        |> updateStatus model
                        |> UR.setModel uResult

                Invalid problems ->
                    toError problems form
                        |> updateStatus model
                        |> UR.setModel uResult
    in
    case model.status of
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
            [ "CompletedCommunityLoad", UR.resultToString r ]

        EnteredTitle _ ->
            [ "EnteredTitle" ]

        EnteredDescription _ ->
            [ "EnteredDescription" ]

        EnteredSymbol _ ->
            [ "EnteredSymbol" ]

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
