module Profile.Summary exposing (Model, Msg, init, initMany, msgToString, update, view, withRelativeSelector, withScrollSelector)

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
    , relativeSelector : Maybe String
    , scrollSelector : Maybe String
    }


init : Bool -> Model
init isLarge =
    { isExpanded = False
    , isLarge = isLarge
    , relativeSelector = Nothing
    , scrollSelector = Nothing
    }


initMany : Bool -> Int -> List Model
initMany isLarge amount =
    init isLarge
        |> List.repeat amount



-- UPDATE


type Msg
    = Ignored
    | OpenedInfo
    | ClosedInfo


update : Msg -> Model -> Model
update msg model =
    case msg of
        Ignored ->
            model

        OpenedInfo ->
            { model | isExpanded = True }

        ClosedInfo ->
            { model | isExpanded = False }



-- VIEW


withRelativeSelector : String -> Model -> Model
withRelativeSelector relativeSelector model =
    { model | relativeSelector = Just relativeSelector }


withScrollSelector : String -> Model -> Model
withScrollSelector scrollSelector model =
    { model | scrollSelector = Just scrollSelector }


view : Shared -> Eos.Name -> Profile.Basic profile -> Model -> Html Msg
view shared loggedInAccount profile model =
    div [ Utils.onClickPreventAll Ignored ]
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
            |> Modal.withPreventScrolling View.Components.PreventScrollOnMobile
            |> Modal.toHtml
        ]


desktopView : Shared -> Eos.Name -> Profile.Basic profile -> Model -> Html Msg
desktopView shared loggedInAccount profile model =
    div
        [ class "mx-auto hidden md:block"
        , onMouseEnter OpenedInfo
        , onMouseLeave ClosedInfo
        ]
        [ viewUserImg shared loggedInAccount profile False model
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
    div [ class "flex flex-col items-center" ]
        [ div [ class ("rounded-full " ++ size) ]
            [ container [] [ Avatar.view profile.avatar size ]
            , if not isMobile && model.isExpanded then
                View.Components.dialogBubble
                    { class_ = "w-120"
                    , relativeSelector = model.relativeSelector
                    , scrollSelector = model.scrollSelector
                    }
                    [ viewUserInfo profile ]

              else
                text ""
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
        Ignored ->
            [ "Ignored" ]

        OpenedInfo ->
            [ "OpenedInfo" ]

        ClosedInfo ->
            [ "ClosedInfo" ]
