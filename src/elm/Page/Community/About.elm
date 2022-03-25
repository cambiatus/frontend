module Page.Community.About exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Community
import Html exposing (Html, a, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (alt, class, href, media, src, style)
import Html.Events exposing (onClick)
import Icons
import Json.Encode as Encode
import Markdown
import Profile.Contact as Contact
import RemoteData
import Session.LoggedIn as LoggedIn
import Translation
import UpdateResult as UR
import Url



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> UpdateResult
init _ =
    UR.init {}
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.UploadsField)



-- TYPES


type Msg
    = NoOp
    | ClickedShareCommunity


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        ClickedShareCommunity ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    UR.init model
                        |> UR.addPort
                            { responseAddress = msg
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "share" )
                                    , ( "title", Encode.string community.name )

                                    -- TODO - Figure out a nice description
                                    -- TODO - Detect if can share. If can't, do what? Just hide the button?
                                    -- , ( "text", Markdown.encode community.description )
                                    , ( "url"
                                      , loggedIn.shared.url
                                            |> Url.toString
                                            |> Encode.string
                                      )
                                    ]
                            }

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Tried sharing community, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.About", function = "update" }
                            []



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        defaultCoverPhoto =
            Html.node "picture"
                []
                [ Html.source
                    [ Html.Attributes.attribute "srcset" "/images/community-bg-mobile.svg"

                    -- md breakpoint
                    , media "(min-width:768px)"
                    ]
                    []
                , img
                    [ src "/images/community-bg-desktop.svg"
                    , class "mb-6 object-cover min-w-full max-h-[10.75rem] sm:max-h-60 lg:max-h-80"
                    , style "object-position" "50% 62%"
                    , alt ""
                    ]
                    []
                ]
    in
    { title = "TODO"
    , content =
        case loggedIn.selectedCommunity of
            RemoteData.Success community ->
                div []
                    [ case RemoteData.map List.head community.uploads of
                        RemoteData.Success (Just upload) ->
                            img
                                [ src upload
                                , class "mb-6 object-cover min-w-full max-h-43 sm:max-h-60 lg:max-h-80"
                                , alt ""
                                ]
                                []

                        _ ->
                            defaultCoverPhoto
                    , div [ class "container mx-auto px-4 py-10" ]
                        [ viewCommunityCard loggedIn.shared.translators community
                        ]
                    ]

            _ ->
                text "TODO"
    }


viewCommunityCard : Translation.Translators -> Community.Model -> Html Msg
viewCommunityCard translators community =
    div [ class "flex flex-col bg-white rounded relative px-4 pt-4 pb-6" ]
        -- TW doesn't support translating on the x and y axies simultaneously,
        -- so we need a parent element to center the icon horizontally
        [ div [ class "absolute w-full top-0 left-0 flex justify-center -translate-y-1/2 pointer-events-none" ]
            [ div
                [ class "h-21 w-21 bg-white p-4 flex items-center justify-center rounded-full shadow-sm"
                ]
                [ img
                    [ src community.logo
                    , class "max-w-full max-h-full"

                    -- TODO - Add alt explaining it's the community's logo
                    , alt ""
                    ]
                    []
                ]
            ]
        , button
            [ class "bg-gray-100 p-2 rounded-full ml-auto focus-ring"
            , onClick ClickedShareCommunity
            ]
            [ Icons.share "" ]
        , h1 [ class "text-lg font-bold text-center" ] [ text community.name ]
        , case community.website of
            Nothing ->
                text ""

            Just website ->
                let
                    removeProtocol site =
                        if String.startsWith "http://" site then
                            -- Remove http://
                            String.dropLeft 7 site

                        else if String.startsWith "https://" website then
                            -- Remove https://
                            String.dropLeft 8 site

                        else
                            site

                    removeTrailingSlash site =
                        if String.endsWith "/" site then
                            String.dropRight 1 site

                        else
                            site
                in
                a
                    [ href website
                    , class "mx-auto text-sm font-bold text-gray-900 hover:underline focus-ring"
                    ]
                    [ website
                        |> removeProtocol
                        |> removeTrailingSlash
                        |> text
                    ]
        , Markdown.view [ class "mt-6" ] community.description
        , ul [ class "flex flex-wrap mt-8 gap-x-6 gap-y-4 items-center justify-center" ]
            (community.contacts
                |> List.map
                    (\contact ->
                        li [ class "w-10 h-10" ]
                            [ Contact.circularIconWithGrayBg translators "" contact
                            ]
                    )
            )
        ]



-- UTILS


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr _ =
    case addr of
        "ClickedShareCommunity" :: _ ->
            Just NoOp

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        ClickedShareCommunity ->
            [ "ClickedShareCommunity" ]
