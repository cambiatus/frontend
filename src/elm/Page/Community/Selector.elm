module Page.Community.Selector exposing (Model, init, view)

import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (class, src)
import Icons
import Page
import Profile
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> ( Model, Cmd msg )
init _ =
    ( {}
    , Cmd.none
    )



-- UPDATE
-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html msg }
view loggedIn _ =
    let
        title =
            "Select community"

        content =
            case loggedIn.profile of
                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError title err

                RemoteData.Success profile ->
                    view_ loggedIn.shared profile
    in
    { title = title
    , content = content
    }


view_ : Shared -> Profile.Model -> Html msg
view_ shared profile =
    div [ class "flex-grow flex flex-col" ]
        [ div [ class "flex flex-col items-center" ]
            [ img [ src "/images/map.svg" ] []
            , span [ class "font-medium text-black text-xl" ] [ text "Welcome" ]
            , span [ class "text-black text-sm" ] [ text "Choose the community you would like to join" ]
            ]
        , div [ class "bg-white flex-grow mt-8" ]
            [ div [ class "container mx-auto px-4 divide-y divide-solid divide-gray-500" ]
                (List.map (viewCommunity shared) profile.communities)
            ]
        ]


viewCommunity : Shared -> Profile.CommunityInfo -> Html msg
viewCommunity shared community =
    a
        [ class "flex justify-between items-center py-6 text-black hover:text-orange-300"
        , Route.externalHref shared community Route.Dashboard
        ]
        [ div [ class "flex items-center" ]
            [ img
                [ class "w-10 h-10"
                , src community.logo
                ]
                []
            , span [ class "ml-4 text-sm" ] [ text community.name ]
            ]
        , Icons.arrowDown "-rotate-90 fill-current"
        ]
