module Page.Community.Editor exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , receiveBroadcast
    , subscriptions
    , update
    , view
    )

import Api
import Asset.Icon as Icon
import Browser.Events as Events
import Community exposing (Model)
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Graphql.Document
import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes exposing (accept, class, classList, disabled, for, id, maxlength, minlength, multiple, required, type_)
import Html.Events exposing (onClick, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Task
import UpdateResult as UR
import Utils exposing (decodeEnterKeyDown)
import View.Components
import View.Feedback as Feedback
import View.Form.Input as Input



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( Editing [] initForm
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map PressedEnter (Events.onKeyDown decodeEnterKeyDown)



-- MODEL


type alias Model =
    Status


type alias Errors =
    List ( FormField, FormError )


type Status
    = Editing Errors Form
    | WaitingLogoUpload Form
    | Creating Form


type FormField
    = Description
    | CurrencyName
    | SymbolField
    | InvitedReward
    | InviterReward
    | MinimumBalance


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


initForm : Form
initForm =
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



-- TODO - Check which are needed


type FormError
    = ChooseOrUploadLogo
    | InternalError
    | InvalidSymbol


type FormStatus
    = Valid Community.CreateCommunityData
    | Invalid Errors
    | UploadingLogo


encodeForm : LoggedIn.Model -> Form -> FormStatus
encodeForm loggedIn form =
    case List.getAt form.logoSelected form.logoList of
        Just Uploading ->
            UploadingLogo

        Just (Uploaded logoUrl) ->
            encodeFormHelper logoUrl loggedIn form

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
            Invalid [ ( SymbolField, InvalidSymbol ) ]

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

        ( errors, isDisabled, form ) =
            case model of
                Editing problems form_ ->
                    ( problems, False, form_ )

                WaitingLogoUpload form_ ->
                    ( [], True, form_ )

                Creating form_ ->
                    ( [], True, form_ )

        cmd =
            case model of
                Editing _ _ ->
                    NewCommunitySubscription form.symbol

                _ ->
                    ClickedSave
    in
    { title = t "community.create.title"
    , content =
        div [ class "bg-white pb-10" ]
            [ Page.viewHeader loggedIn (t "community.create.title") Route.Dashboard
            , Html.form
                [ class "container mx-auto px-4"
                , onSubmit cmd
                ]
                [ div [ class "my-10" ]
                    [ viewDescription shared isDisabled form.description errors
                    , viewCurrencyName shared isDisabled form.name errors
                    , div [ class "flex flex-row mt-4" ]
                        [ div [ class "w-1/2 pr-2" ]
                            [ viewSymbol shared isDisabled form.symbol errors
                            , viewInviterReward shared isDisabled form.inviterReward errors
                            ]
                        , div [ class "w-1/2 pl-2" ]
                            [ viewInvitedReward shared isDisabled form.invitedReward errors
                            , viewMinBalance shared isDisabled form.minBalance errors
                            ]
                        ]
                    , viewLogo shared isDisabled form.logoSelected form.logoList
                    ]
                , button
                    [ class "button button-primary w-full"
                    , disabled isDisabled
                    ]
                    [ text (t "community.create.submit") ]
                ]
            ]
    }


viewDescription : Shared -> Bool -> String -> Errors -> Html Msg
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
            |> Input.withType Input.TextArea
            |> Input.withAttrs [ maxlength 255, class "h-40" ]
            |> Input.toHtml
        ]


viewCurrencyName : Shared -> Bool -> String -> Errors -> Html Msg
viewCurrencyName ({ translators } as shared) isDisabled defVal errors =
    Input.init
        { label = translators.t "community.create.labels.currency_name"
        , id = "comm-currency-name"
        , onInput = EnteredTitle
        , disabled = isDisabled
        , value = defVal
        , placeholder = Nothing
        , problems = Just (getFieldProblems shared CurrencyName errors)
        , translators = translators
        }
        |> Input.withAttrs [ maxlength 255, required True ]
        |> Input.toHtml


viewSymbol : Shared -> Bool -> String -> Errors -> Html Msg
viewSymbol ({ translators } as shared) isDisabled defVal errors =
    Input.init
        { label = translators.t "community.create.labels.currency_symbol"
        , id = "comm-currency-symbol"
        , onInput = EnteredSymbol
        , disabled = isDisabled
        , value = defVal
        , placeholder = Just "_, _ _ _ _"
        , problems = Just (getFieldProblems shared SymbolField errors)
        , translators = translators
        }
        |> Input.withAttrs [ minlength 5, maxlength 6, required True ]
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
            ++ [ div []
                    [ input
                        [ id id_
                        , class "hidden"
                        , type_ "file"
                        , accept "image/*"
                        , Page.onFileChange (EnteredLogo (List.length logos))
                        , multiple False
                        , disabled isDisabled
                        ]
                        []
                    , label
                        [ for id_
                        , class ("flex-col text-center cursor-pointer " ++ itemClass)
                        , classList [ ( "disabled", isDisabled ) ]
                        ]
                        [ div [ class "bg-gradient-to-bl from-orange-300 to-orange-500 rounded-full p-2 mb-1 w-12 h-12 flex items-center justify-center" ]
                            [ Icon.imageMultiple "text-white fill-current w-8 h-8" ]
                        , text (t "community.create.labels.upload_icon")
                        ]
                    ]
               ]
        )


viewInviterReward : Shared -> Bool -> String -> Errors -> Html Msg
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
        |> Input.withAttrs [ maxlength 255 ]
        |> Input.asNumeric
        |> Input.toHtml


viewInvitedReward : Shared -> Bool -> String -> Errors -> Html Msg
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
        |> Input.toHtml


viewMinBalance : Shared -> Bool -> String -> Errors -> Html Msg
viewMinBalance ({ translators } as shared) isDisabled defVal errors =
    Input.init
        { label = translators.t "community.create.labels.min_balance"
        , id = "min-balance"
        , onInput = EnteredMinBalance
        , disabled = isDisabled
        , value = defVal
        , placeholder = Nothing
        , problems = Just (getFieldProblems shared MinimumBalance errors)
        , translators = translators
        }
        |> Input.withAttrs [ maxlength 255, required True ]
        |> Input.asNumeric
        |> Input.toHtml



-- HELPERS


getFieldProblems : Shared -> FormField -> Errors -> List String
getFieldProblems shared formField errors =
    errors
        |> List.filter (\( field, _ ) -> field == formField)
        |> List.map (Tuple.second >> errorToString shared)


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
    = EnteredTitle String
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
        -- CompletedLoadCommunity community ->
        --     case model of
        -- Loading ->
        --     if LoggedIn.isAccount community.creator loggedIn then
        --         Editing community Dict.empty (editForm community)
        --             |> UR.init
        --     else
        --         Unauthorized community
        --             |> UR.init
        -- _ ->
        --     UR.init model
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

        CompletedLogoUpload index (Err err) ->
            UR.init model
                |> updateForm
                    (\form ->
                        { form
                            | logoList = List.removeAt index form.logoList
                            , logoSelected = 0
                        }
                    )
                |> UR.logHttpError msg err
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (t "error.unknown"))

        CompletedLogoUpload index (Ok url) ->
            case model of
                WaitingLogoUpload _ ->
                    UR.init model
                        |> updateForm (\form -> { form | logoList = List.setAt index (Uploaded url) form.logoList })
                        |> save msg loggedIn

                _ ->
                    UR.init model

        GotSaveResponse (Ok _) ->
            case model of
                -- Saving community form ->
                --     model
                --         |> UR.init
                --         |> UR.addExt
                --             (updateCommunity form community
                --                 |> LoggedIn.CommunityLoaded
                --                 |> LoggedIn.ExternalBroadcast
                --             )
                --         |> UR.addCmd (Route.Community |> Route.replaceUrl loggedIn.shared.navKey)
                --         |> UR.addExt (ShowFeedback Feedback.Success (t "community.create.success"))
                _ ->
                    model
                        |> UR.init

        GotSaveResponse (Err val) ->
            -- TODO
            -- let
            --     err =
            --         Dict.singleton "form" InternalError
            -- in
            case model of
                -- Saving community form ->
                --     Editing community err form
                --         |> UR.init
                --         |> UR.logDebugValue msg val
                --         |> UR.addExt (ShowFeedback Feedback.Failure (t "error.unknown"))
                Creating form ->
                    Editing [] form
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
                        Just _ ->
                            model
                                |> UR.init
                                |> UR.addCmd
                                    (Route.Community
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
        -- Loading ->
        --     uResult
        -- Unauthorized _ ->
        --     uResult
        -- Editing community errors form ->
        --     Editing community errors (transform form)
        --         |> UR.setModel uResult
        -- WaitingEditLogoUpload community form ->
        --     WaitingEditLogoUpload community (transform form)
        --         |> UR.setModel uResult
        -- Saving community form ->
        --     Saving community (transform form)
        --         |> UR.setModel uResult
        Editing errors form ->
            Editing errors (transform form)
                |> UR.setModel uResult

        WaitingLogoUpload form ->
            WaitingLogoUpload (transform form)
                |> UR.setModel uResult

        Creating form ->
            Creating (transform form)
                |> UR.setModel uResult



-- updateCommunity : Form -> Community.Model -> Community.Model
-- updateCommunity form community =
--     { community
--         | name = form.name
--         , description = form.description
--         , symbol =
--             Eos.symbolFromString form.symbol
--                 |> Maybe.withDefault community.symbol
--         , logo =
--             case List.getAt form.logoSelected form.logoList of
--                 Just (Uploaded logo) ->
--                     logo
--                 _ ->
--                     community.logo
--         , invitedReward =
--             String.toFloat form.invitedReward
--                 |> Maybe.withDefault community.invitedReward
--         , inviterReward =
--             String.toFloat form.inviterReward
--                 |> Maybe.withDefault community.inviterReward
--         , minBalance = String.toFloat form.minBalance
--         , hasShop = form.hasShop
--         , hasObjectives = form.hasObjectives
--         , hasKyc = form.hasKyc
--     }


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
                                                , hasObjectives = createAction.hasObjectives
                                                , hasShop = createAction.hasShop

                                                -- , hasKyc = 0
                                                }
                                                    |> Community.encodeUpdateData
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
        -- Editing community _ form ->
        --     save_ form (Saving community) (WaitingEditLogoUpload community) (Editing community) True
        -- WaitingEditLogoUpload community form ->
        --     save_ form (Saving community) (WaitingEditLogoUpload community) (Editing community) True
        Editing _ form ->
            save_ form Creating WaitingLogoUpload Editing False

        WaitingLogoUpload form ->
            save_ form Creating WaitingLogoUpload Editing False

        _ ->
            uResult
                |> UR.logImpossible msg []


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    -- TODO
    case broadcastMsg of
        _ ->
            Nothing


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
        -- CompletedLoadCommunity _ ->
        --     [ "CompletedLoadCommunity" ]
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
