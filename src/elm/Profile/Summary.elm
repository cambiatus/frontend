module Profile.Summary exposing (Model, Msg, init, initMany, msgToString, subscriptions, update, view)

import Avatar
import Browser.Dom
import Eos.Account as Eos
import Html exposing (Html, a, button, div, li, p, text, ul)
import Html.Attributes exposing (class, classList, href, id)
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Ports
import Profile
import Profile.Contact as Contact
import Route
import Session.Shared exposing (Shared)
import Task
import UpdateResult as UR
import Utils exposing (onClickNoBubble)
import View.Components
import View.Modal as Modal



-- MODEL


type alias Model =
    { isExpanded : Bool
    , isLarge : Bool
    , viewStyle : Maybe ViewStyle
    , id : String
    }


type ViewStyle
    = Modal
    | Bubble View.Components.Orientation


init : Bool -> String -> ( Model, Cmd Msg )
init isLarge id =
    ( { isExpanded = False
      , isLarge = isLarge
      , viewStyle = Nothing
      , id = id
      }
    , Browser.Dom.getElement id
        |> Task.attempt GotElement
    )


initMany : Bool -> (Int -> String) -> (Int -> Msg -> msg) -> Int -> ( List Model, Cmd msg )
initMany isLarge id toMsg amount =
    List.range 0 amount
        |> List.map
            (\index ->
                init isLarge (id index)
                    |> Tuple.mapSecond (Cmd.map (toMsg index))
            )
        |> List.unzip
        |> Tuple.mapSecond Cmd.batch



-- UPDATE


type Msg
    = OpenedInfo
    | ClosedInfo
    | GotElement (Result Browser.Dom.Error Browser.Dom.Element)
    | ScrolledOrResized


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenedInfo ->
            ( { model | isExpanded = True }, Cmd.none )

        ClosedInfo ->
            ( { model | isExpanded = False }, Cmd.none )

        GotElement (Ok { element, viewport }) ->
            let
                width =
                    25 * 16

                height =
                    400

                orientation =
                    if element.height >= viewport.height || element.width >= viewport.width then
                        Modal

                    else if element.x - viewport.x < (width / 2) then
                        Bubble View.Components.Right

                    else if viewport.width - element.x < (width / 2) then
                        Bubble View.Components.Left

                    else if element.y - height <= viewport.y then
                        Bubble View.Components.Down

                    else
                        Bubble View.Components.Up
            in
            ( { model | viewStyle = Just orientation }, Cmd.none )

        GotElement (Err _) ->
            ( { model | viewStyle = Nothing }, Cmd.none )

        ScrolledOrResized ->
            ( model
            , Browser.Dom.getElement model.id
                |> Task.attempt GotElement
            )


subscriptions : Sub Msg
subscriptions =
    Ports.onScrollOrResize (\_ -> ScrolledOrResized)



-- VIEW


view : Shared -> Eos.Name -> Profile.Basic profile -> Model -> Html Msg
view shared loggedInAccount profile model =
    div [ id model.id ]
        (case model.viewStyle of
            Nothing ->
                []

            Just (Bubble orientation) ->
                [ desktopView shared loggedInAccount profile orientation model
                , mobileView shared loggedInAccount profile False model
                ]

            Just Modal ->
                [ mobileView shared loggedInAccount profile True model ]
        )


mobileView : Shared -> Eos.Name -> Profile.Basic profile -> Bool -> Model -> Html Msg
mobileView shared loggedInAccount profile isModalView model =
    div
        [ class "cursor-auto"
        , classList [ ( "md:hidden", not isModalView ) ]
        ]
        [ viewUserImg shared loggedInAccount profile True model
        , Modal.initWith { closeMsg = ClosedInfo, isVisible = model.isExpanded }
            |> Modal.withBody
                [ div [ class "pt-14" ]
                    [ viewUserInfo profile ]
                ]
            |> Modal.toHtml
        ]


desktopView : Shared -> Eos.Name -> Profile.Basic profile -> View.Components.Orientation -> Model -> Html Msg
desktopView shared loggedInAccount profile orientation model =
    let
        position =
            case orientation of
                View.Components.Down ->
                    "top-full right-1/2 translate-x-1/2"

                View.Components.Up ->
                    "bottom-full right-1/2 translate-x-1/2"

                -- TODO - Check this
                View.Components.Left ->
                    "right-full bottom-1/2 translate-y-1/2"

                View.Components.Right ->
                    "left-full bottom-1/2 translate-y-1/2"
    in
    div
        [ class "mx-auto hidden md:block relative"
        , onMouseEnter OpenedInfo
        , onMouseLeave ClosedInfo
        ]
        [ viewUserImg shared loggedInAccount profile False model
        , if model.isExpanded then
            View.Components.dialogBubble orientation
                [ class ("absolute transform cursor-auto z-50 min-w-100 " ++ position)
                , classList [ ( "hidden", not model.isExpanded ) ]
                ]
                [ viewUserInfo profile ]

          else
            text ""
        ]


viewUserImg : Shared -> Eos.Name -> Profile.Basic profile -> Bool -> Model -> Html Msg
viewUserImg shared loggedInAccount profile isMobile model =
    let
        size =
            if model.isLarge then
                "w-20 h-20"

            else
                "w-10 h-10"

        container attrs =
            if isMobile then
                button (onClickNoBubble OpenedInfo :: attrs)

            else
                let
                    route =
                        if loggedInAccount == profile.account then
                            Route.Profile

                        else
                            Route.ProfilePublic (Eos.nameToString profile.account)
                in
                a (Route.href route :: attrs)
    in
    container [ class "flex flex-col items-center" ]
        [ div [ class ("rounded-full " ++ size) ]
            [ Avatar.view profile.avatar size
            ]
        , div [ class "mt-2" ]
            [ Profile.viewProfileNameTag shared loggedInAccount profile ]
        ]


viewUserInfo : Profile.Basic profile -> Html Msg
viewUserInfo profile =
    let
        userName =
            profile.name |> Maybe.withDefault ""

        email =
            profile.email |> Maybe.withDefault ""

        account =
            profile.account |> Eos.nameToString

        bio =
            profile.bio |> Maybe.withDefault ""
    in
    div [ class "flex flex-col w-full" ]
        [ div [ class "flex mb-4 items-center justify-center" ]
            [ Avatar.view profile.avatar "w-20 h-20 mr-6 flex-shrink-0"
            , div [ class "flex items-center justify-between" ]
                [ ul [ class "text-sm text-gray-900" ]
                    [ li [ class "font-medium text-body-black text-2xl xs-max:text-xl" ]
                        [ text userName ]
                    , li [] [ a [ href <| "mailto:" ++ email ] [ text email ] ]
                    , li [] [ text account ]
                    ]
                ]
            ]
        , p [ class "text-sm text-gray-900" ]
            [ text bio ]
        , div [ class "flex justify-evenly mt-6" ]
            -- TODO - Check telegram icon
            (List.map (Contact.circularIcon "w-9 h-9 hover:opacity-75") profile.contacts)
        , a
            [ class "button button-primary w-full mt-6 cursor-pointer"
            , Route.href (Route.ProfilePublic account)
            ]
            [ text "View full profile" ]
        ]



-- INTEROP
-- TODO - Use this in other pages


msgToString : Msg -> List String
msgToString msg =
    case msg of
        OpenedInfo ->
            [ "OpenedInfo" ]

        ClosedInfo ->
            [ "ClosedInfo" ]

        GotElement r ->
            [ "GotElement", UR.resultToString r ]

        ScrolledOrResized ->
            [ "ScrolledOrResized" ]
