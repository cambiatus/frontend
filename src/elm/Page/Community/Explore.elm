module Page.Community.Explore exposing (Model, Msg(..), init, msgToString, update, view)

import Api
import Api.Graphql
import Asset.Icon as Icon
import Community exposing (Metadata)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (class, src, style)
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
            Page.fullPageGraphQLError (t loggedIn.shared.translations "menu.communities") e

        Loaded communities ->
            div [ class "container mx-auto px-4" ]
                [ renderUserMessage model
                , Page.viewTitle (t loggedIn.shared.translations "menu.communities")
                , if loggedIn.shared.allowCommunityCreation then
                    Page.viewButtonNew (I18Next.t loggedIn.shared.translations "community.create_button") Route.NewCommunity

                  else
                    text ""
                , div [ class "flex flex-wrap -mx-2" ]
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
    in
    a
        [ class "w-full sm:w-1/2 md:w-1/3 lg:w-1/3 xl:w-1/4 px-2 mb-6 h-60"
        , Route.href (Route.Community community.symbol)
        ]
        [ div [ class "flex flex-wrap pt-5 pb-2 rounded-lg hover:shadow-lg bg-white" ]
            [ img [ class "w-full object-center object-scale-down h-20", src (ipfsUrl ++ "/" ++ community.logo) ] []
            , p [ class "absolute bg-gray-200 rounded-full ml-6 px-3 py-1 text-sm font-sans font-bold text-gray-700" ]
                [ text (Eos.symbolToString community.symbol) ]
            , div [ class "px-6 py-4" ]
                [ div [ class "font-sans font-bold text-xl" ]
                    [ text community.title
                    ]
                , p [ class "font-sans text-sm mb-2" ]
                    [ text (tr "community.index.created_by" [ ( "account_name", Eos.nameToString community.creator ) ]) ]
                , p [ class "text-gray-700 font-sans overflow-hidden h-12" ] [ text community.description ]
                ]
            , div [ class "w-full px-6 py-4" ]
                [ p [ class "flex-1 bg-indigo-500 mt-2 rounded-full px-3 py-1 text-sm font-bold text-white mr-2" ]
                    [ text (tr "community.index.members" [ ( "quantity", String.fromInt community.memberCount ) ]) ]
                , case maybeTransferCount of
                    Just transferCount ->
                        p [ class "flex-1 bg-indigo-500 mt-2 rounded-full px-3 py-1 text-sm font-bold text-white mr-2" ]
                            [ text (tr "community.index.transfers" [ ( "quantity", String.fromInt transferCount ) ])
                            ]

                    Nothing ->
                        text ""
                ]
            ]
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
