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
import Community
import Eos
import Eos.Account as Eos
import File exposing (File)
import Html exposing (Html, button, div, form, img, input, label, li, span, text, ul)
import Html.Attributes exposing (accept, class, disabled, for, id, maxlength, multiple, src, type_)
import Html.Events exposing (onSubmit)
import Http
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import UpdateResult as UR
import View.Form.Input



-- MODEL


type alias Model =
    { logoUrl : String
    , nameInput : String
    , nameErrors : List String
    , descriptionInput : String
    , descriptionErrors : List String
    , urlInput : String
    , urlErrors : List String
    , isLoading : Bool
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { logoUrl = ""
      , nameInput = ""
      , nameErrors = []
      , descriptionInput = ""
      , descriptionErrors = []
      , urlInput = ""
      , urlErrors = []
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
    | EnteredUrl String
    | ClickedSave
    | GotSaveResponse (Result Value Eos.Symbol)


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoadCommunity community ->
            { model
                | logoUrl = community.logo
                , nameInput = community.name
                , descriptionInput = community.description
                , isLoading = False

                -- TODO - use community subdomain
                , urlInput =
                    String.toLower community.name
                        |> String.replace " " "_"
            }
                |> UR.init

        EnteredLogo (file :: _) ->
            UR.init model
                |> UR.addCmd (Api.uploadImage loggedIn.shared file CompletedLogoUpload)

        EnteredLogo [] ->
            UR.init model

        CompletedLogoUpload (Ok url) ->
            { model | logoUrl = url }
                |> UR.init

        CompletedLogoUpload (Err e) ->
            UR.init model
                |> UR.addExt
                    (LoggedIn.ShowFeedback LoggedIn.Failure
                        (loggedIn.shared.translators.t "settings.community_info.errors.logo_upload")
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

        EnteredUrl url ->
            { model | urlInput = url }
                |> validateUrl
                |> UR.init

        ClickedSave ->
            case ( LoggedIn.isAuth loggedIn, isModelValid model, loggedIn.selectedCommunity ) of
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
                            { responseAddress = ClickedSave
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
                                            , inviterReward = asset community.inviterReward
                                            , invitedReward = asset community.invitedReward
                                            , hasObjectives = boolToInt community.hasObjectives
                                            , hasShop = boolToInt community.hasShop
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

        GotSaveResponse (Ok _) ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    { model | isLoading = False }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback LoggedIn.Success (loggedIn.shared.translators.t "community.create.success"))
                        |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey Route.Dashboard)
                        |> UR.addExt
                            (LoggedIn.CommunityLoaded
                                { community
                                    | name = model.nameInput
                                    , description = model.descriptionInput
                                    , logo = model.logoUrl
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
                |> UR.addExt (LoggedIn.ShowFeedback LoggedIn.Failure (loggedIn.shared.translators.t "community.error_saving"))


boolToInt : Bool -> Int
boolToInt bool =
    if bool then
        1

    else
        0


type Field
    = NameField
    | DescriptionField
    | UrlField


error : Field -> String -> String
error field key =
    let
        fieldString =
            case field of
                NameField ->
                    "name"

                DescriptionField ->
                    "description"

                UrlField ->
                    "url"
    in
    String.join "." [ "settings.community_info.errors", fieldString, key ]


isModelValid : Model -> Bool
isModelValid model =
    let
        validatedModel =
            validateModel model
    in
    List.all (\f -> f validatedModel |> List.isEmpty) [ .nameErrors, .descriptionErrors, .urlErrors ]


validateModel : Model -> Model
validateModel model =
    model
        |> validateName
        |> validateDescription
        |> validateUrl


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


validateUrl : Model -> Model
validateUrl model =
    let
        isAllowed character =
            Char.isAlphaNum character || character == '-'

        validateLength =
            if String.length model.urlInput > 1 then
                []

            else
                [ error UrlField "too_short" ]

        validateChars =
            if String.all isAllowed model.urlInput then
                []

            else
                [ error UrlField "invalid_char" ]

        validateCase =
            if String.filter Char.isAlphaNum model.urlInput |> String.all Char.isLower then
                []

            else
                [ error UrlField "invalid_case" ]
    in
    { model | urlErrors = validateChars ++ validateCase ++ validateLength }



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

                RemoteData.Success _ ->
                    div [ class "bg-white" ]
                        [ Page.viewHeader loggedIn title Route.CommunitySettings
                        , view_ loggedIn model
                        ]
    in
    { title = title
    , content = content
    }


view_ : LoggedIn.Model -> Model -> Html Msg
view_ loggedIn model =
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
                , viewUrl loggedIn.shared model
                ]
            , button
                [ class "button button-primary w-full mt-8"
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
    View.Form.Input.init
        { label = t "settings.community_info.fields.name"
        , id = "community_name_input"
        , onInput = EnteredName
        , disabled = model.isLoading
        , value = model.nameInput
        , placeholder = Just (t "settings.community_info.placeholders.name")
        , problems =
            List.map t model.nameErrors
                |> List.head
                |> Maybe.map (\x -> [ x ])
        , translators = shared.translators
        }
        |> View.Form.Input.toHtml


viewDescription : Shared -> Model -> Html Msg
viewDescription shared model =
    let
        { t } =
            shared.translators
    in
    View.Form.Input.init
        { label = t "settings.community_info.fields.description"
        , id = "community_description_input"
        , onInput = EnteredDescription
        , disabled = model.isLoading
        , value = model.descriptionInput
        , placeholder = Just (t "settings.community_info.placeholders.description")
        , problems =
            List.map t model.descriptionErrors
                |> List.head
                |> Maybe.map (\x -> [ x ])
        , translators = shared.translators
        }
        |> View.Form.Input.toHtmlTextArea


viewUrl : Shared -> Model -> Html Msg
viewUrl shared model =
    let
        { t } =
            shared.translators

        text_ =
            text << t
    in
    div []
        [ View.Form.Input.init
            { label = t "settings.community_info.fields.url"
            , id = "community_description_input"
            , onInput = EnteredUrl
            , disabled = model.isLoading
            , value = model.urlInput
            , placeholder = Just (t "settings.community_info.placeholders.url")
            , problems =
                List.map t model.urlErrors
                    |> List.head
                    |> Maybe.map (\x -> [ x ])
            , translators = shared.translators
            }
            |> View.Form.Input.withCounter 30
            |> View.Form.Input.withAttrs [ maxlength 30 ]
            |> View.Form.Input.withElement (text ".cambiatus.io")
            |> View.Form.Input.toHtml
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



-- UTILS


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
        "ClickedSave" :: [] ->
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

        EnteredUrl _ ->
            [ "EnteredUrl" ]

        ClickedSave ->
            [ "ClickedSave" ]

        GotSaveResponse r ->
            [ "GotSaveResponse", UR.resultToString r ]
