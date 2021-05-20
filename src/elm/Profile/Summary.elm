module Profile.Summary exposing (Model, Msg, init, msgToString, update, view)

import Avatar
import Eos.Account as Eos
import Html exposing (Html, a, div, li, p, text, ul)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Profile
import Profile.Contact as Contact
import Route
import Session.Shared exposing (Shared)
import View.Components



-- MODEL


type alias Model =
    { isExpanded : Bool
    , isLarge : Bool
    , extraAttrs : List (Html.Attribute Msg)
    }


init : Bool -> Model
init isLarge =
    { isExpanded = False
    , isLarge = isLarge
    , extraAttrs = []
    }



-- UPDATE


type Msg
    = MouseEntered
    | MouseLeft


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseEntered ->
            { model | isExpanded = True }

        MouseLeft ->
            { model | isExpanded = False }



-- VIEW


view : Shared -> Eos.Name -> Profile.Basic profile -> Model -> Html Msg
view shared loggedInAccount profile model =
    let
        size =
            if model.isLarge then
                "w-20 h-20"

            else
                "w-10 h-10"
    in
    div
        [ class "relative"
        , onMouseEnter MouseEntered
        , onMouseLeave MouseLeft
        ]
        [ a
            [ class "flex flex-col items-center"
            , href ("/profile/" ++ Eos.nameToString profile.account)
            ]
            [ div [ class ("rounded-full " ++ size) ]
                [ Avatar.view profile.avatar size
                ]
            , div [ class "mt-2" ]
                [ Profile.viewProfileNameTag shared loggedInAccount profile ]
            ]
        , if model.isExpanded then
            View.Components.dialogBubble
                [ class "absolute bottom-full right-1/2 transform translate-x-1/2 cursor-auto"
                , classList [ ( "hidden", not model.isExpanded ) ]
                ]
                [ viewUserInfo profile ]

          else
            text ""
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
    div [ class "flex flex-col" ]
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
        MouseEntered ->
            [ "MouseEntered" ]

        MouseLeft ->
            [ "MouseLeft" ]
