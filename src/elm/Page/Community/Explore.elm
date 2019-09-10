module Page.Community.Explore exposing (Model, Msg(..), init, msgToString, update, view)

import Api
import Api.Graphql
import Asset.Icon as Icon
import Community exposing (Metadata)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Http
import I18Next exposing (t)
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Log
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Transfer
import UpdateResult as UR



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init ({ shared } as loggedIn) =
    ( initModel loggedIn
    , Api.Graphql.query shared Community.communitiesQuery CompletedCommunitiesLoad
    )



-- MODEL


type alias Model =
    { communities : Status
    , userMessage : Maybe UserMessage
    }


initModel : LoggedIn.Model -> Model
initModel loggedIn =
    { communities = Loading
    , userMessage = Nothing
    }


type alias UserMessage =
    { title : String
    , description : String
    , class : String
    }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (List Metadata))
    | Loaded (List Metadata)



-- VIEW


view : LoggedIn.Model -> Model -> Html msg
view loggedIn model =
    case model.communities of
        Loading ->
            Page.fullPageLoading

        LoadingFailed e ->
            Page.fullPageGraphQLError (t loggedIn.shared.translations "menu.explore_communities") e

        Loaded communities ->
            Page.mainContentContainer
                [ renderUserMessage model
                , Page.viewTitle (t loggedIn.shared.translations "menu.explore_communities")
                , div [ class "card-grid card-grid--communities" ]
                    (viewCommunities loggedIn (String.toUpper loggedIn.searchText) communities)
                ]


renderUserMessage : Model -> Html msg
renderUserMessage model =
    case model.userMessage of
        Just userMessage ->
            div [ class "row" ]
                [ div [ class "column" ]
                    [ div [ class ("ui message " ++ userMessage.class) ]
                        [ i
                            [ class "close icon" ]
                            []
                        , div [ class "header" ] [ text userMessage.title ]
                        , p [] [ text userMessage.description ]
                        ]
                    ]
                ]

        Nothing ->
            text ""


viewCommunities : LoggedIn.Model -> String -> List Metadata -> List (Html msg)
viewCommunities loggedIn searchTerm communities =
    List.filterMap
        (\community ->
            let
                searchByTitle : Bool
                searchByTitle =
                    String.contains searchTerm (String.toUpper community.title)

                searchBySymbol : Bool
                searchBySymbol =
                    String.contains searchTerm (Eos.symbolToString community.symbol)
            in
            if searchByTitle || searchBySymbol then
                Just (viewCommunity loggedIn community)

            else
                Nothing
        )
        communities


viewCommunity : LoggedIn.Model -> Metadata -> Html msg
viewCommunity loggedIn community =
    let
        ipfsUrl =
            loggedIn.shared.endpoints.ipfs

        tr =
            I18Next.tr loggedIn.shared.translations I18Next.Curly

        maybeTransferCount =
            Transfer.getTotalCount (Just community)

        cardInfo =
            case maybeTransferCount of
                Just transferCount ->
                    [ div [ class "card__info__item" ]
                        [ Icon.account ""
                        , span [] [ text (tr "community.index.members" [ ( "quantity", String.fromInt community.memberCount ) ]) ]
                        ]
                    , div [ class "card__info__item" ]
                        [ Icon.swapHorizontal ""
                        , span [] [ text (tr "community.index.transfers" [ ( "quantity", String.fromInt transferCount ) ]) ]
                        ]
                    ]

                Nothing ->
                    [ div [ class "card__info__item" ]
                        [ Icon.account ""
                        , span [] [ text (tr "community.index.members" [ ( "quantity", String.fromInt community.memberCount ) ]) ]
                        ]
                    ]
    in
    a
        [ class "card"
        , Route.href (Route.Community community.symbol)
        ]
        [ span [ class "card__currency-symbol" ]
            [ text (Eos.symbolToString community.symbol) ]
        , div
            [ class "card__image-background"
            , Community.logoBackground ipfsUrl (Just community.logo)
            ]
            []
        , span [ class "card__title card__title--community" ]
            [ text community.title ]
        , span [ class "card__created-by" ]
            [ text ("Created by " ++ Eos.nameToString community.creator) ]
        , span [ class "card__description" ]
            [ text community.description ]
        , div [ class "card__info" ]
            cardInfo
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedCommunitiesLoad (Result (Graphql.Http.Error (List Metadata)) (List Metadata))


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedCommunitiesLoad (Ok communities) ->
            UR.init { model | communities = Loaded communities }

        CompletedCommunitiesLoad (Err err) ->
            UR.init { model | communities = LoadingFailed err }
                |> UR.logGraphqlError msg err


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedCommunitiesLoad r ->
            [ "CompletedCommunitiesLoad", UR.resultToString r ]
