module Profile.Summary exposing (Model, Msg, init, initMany, msgToString, update, view)

import Avatar
import Eos.Account as Eos
import Html exposing (Html, a, button, div, li, p, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Profile
import Profile.Contact as Contact
import Route
import Session.Shared exposing (Shared)
import Utils exposing (onClickNoBubble)
import View.Components
import View.Modal as Modal



-- MODEL


type alias Model =
    { isExpanded : Bool
    , isLarge : Bool
    }


init : Bool -> Model
init isLarge =
    { isExpanded = False
    , isLarge = isLarge
    }


initMany : Bool -> Int -> List Model
initMany isLarge amount =
    init isLarge
        |> List.repeat amount



-- UPDATE


type Msg
    = OpenedInfo
    | ClosedInfo


update : Msg -> Model -> Model
update msg model =
    case msg of
        OpenedInfo ->
            { model | isExpanded = True }

        ClosedInfo ->
            { model | isExpanded = False }



-- VIEW


view : Shared -> Eos.Name -> Profile.Basic profile -> Model -> Html Msg
view shared loggedInAccount profile model =
    div []
        [ desktopView shared loggedInAccount profile model
        , mobileView shared loggedInAccount profile model
        ]


mobileView : Shared -> Eos.Name -> Profile.Basic profile -> Model -> Html Msg
mobileView shared loggedInAccount profile model =
    div [ class "md:hidden cursor-auto" ]
        [ viewUserImg shared loggedInAccount profile True model
        , Modal.initWith { closeMsg = ClosedInfo, isVisible = model.isExpanded }
            |> Modal.withBody
                [ div [ class "pt-14" ]
                    [ viewUserInfo profile ]
                ]
            |> Modal.toHtml
        ]


desktopView : Shared -> Eos.Name -> Profile.Basic profile -> Model -> Html Msg
desktopView shared loggedInAccount profile model =
    div
        [ class "mx-auto hidden md:block relative"
        , onMouseEnter OpenedInfo
        , onMouseLeave ClosedInfo
        ]
        [ viewUserImg shared loggedInAccount profile False model
        , if model.isExpanded then
            View.Components.dialogBubble { class_ = "min-w-100", minWidth = 400 }
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
            (List.map (Contact.circularIcon "w-9 h-9 hover:opacity-75") profile.contacts)
        , a
            [ class "button button-primary w-full mt-6 cursor-pointer"
            , Route.href (Route.ProfilePublic account)
            ]
            [ text "View full profile" ]
        ]



-- INTEROP


msgToString : Msg -> List String
msgToString msg =
    case msg of
        OpenedInfo ->
            [ "OpenedInfo" ]

        ClosedInfo ->
            [ "ClosedInfo" ]
