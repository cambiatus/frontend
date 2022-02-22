module Profile.Summary exposing
    ( Model
    , Msg
    , expand
    , init
    , initMany
    , msgToString
    , update
    , view
    , withAttrs
    , withImageSize
    , withNameBg
    , withPreventScrolling
    , withRelativeSelector
    , withScrollSelector
    , withoutName
    )

import Avatar
import Eos.Account as Eos
import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes exposing (class, href)
import Html.Attributes.Aria exposing (ariaHidden, ariaLabel)
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Markdown
import Profile
import Profile.Contact as Contact
import Route
import Translation
import Utils exposing (onClickNoBubble)
import View.Components
import View.Modal as Modal



-- MODEL


type alias Model =
    { isExpanded : Bool
    , imageSize : String
    , preventScrolling : View.Components.PreventScroll
    , relativeSelector : Maybe String
    , scrollSelector : Maybe String
    , showNameTag : Bool
    , showNameTagBg : Bool
    , extraAttrs : List (Html.Attribute Msg)
    }


init : Bool -> Model
init isLarge =
    { isExpanded = False
    , imageSize =
        if isLarge then
            "w-20 h-20"

        else
            "h-10 w-10"
    , preventScrolling = View.Components.PreventScrollOnMobile
    , relativeSelector = Nothing
    , scrollSelector = Nothing
    , showNameTag = True
    , showNameTagBg = True
    , extraAttrs = []
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


expand : Msg
expand =
    OpenedInfo


withPreventScrolling : View.Components.PreventScroll -> Model -> Model
withPreventScrolling preventScrolling model =
    { model | preventScrolling = preventScrolling }



-- VIEW


withRelativeSelector : String -> Model -> Model
withRelativeSelector relativeSelector model =
    { model | relativeSelector = Just relativeSelector }


withScrollSelector : String -> Model -> Model
withScrollSelector scrollSelector model =
    { model | scrollSelector = Just scrollSelector }


withNameBg : Bool -> Model -> Model
withNameBg showNameTagBg model =
    { model | showNameTagBg = showNameTagBg }


withoutName : Model -> Model
withoutName model =
    { model | showNameTag = False }


withImageSize : String -> Model -> Model
withImageSize imageSize model =
    { model | imageSize = imageSize }


withAttrs : List (Html.Attribute Msg) -> Model -> Model
withAttrs attrs model =
    { model | extraAttrs = model.extraAttrs ++ attrs }


view : Translation.Translators -> Eos.Name -> Profile.Basic profile -> Model -> Html Msg
view translators loggedInAccount profile model =
    div [ Utils.onClickPreventAll Ignored ]
        [ desktopView translators loggedInAccount profile model
        , mobileView translators loggedInAccount profile model
        ]


mobileView : Translation.Translators -> Eos.Name -> Profile.Basic profile -> Model -> Html Msg
mobileView translators loggedInAccount profile model =
    div [ class "md:hidden cursor-auto" ]
        [ viewUserImg translators profile loggedInAccount True model
        , viewUserNameTag translators loggedInAccount profile model
        , Modal.initWith { closeMsg = ClosedInfo, isVisible = model.isExpanded }
            |> Modal.withBody [ viewUserInfo profile ]
            |> Modal.withPreventScrolling model.preventScrolling
            |> Modal.toHtml
        ]


desktopView : Translation.Translators -> Eos.Name -> Profile.Basic profile -> Model -> Html Msg
desktopView translators loggedInAccount profile model =
    div
        [ class "mx-auto hidden md:block" ]
        [ div
            [ onMouseEnter OpenedInfo
            , onMouseLeave ClosedInfo
            ]
            [ viewUserImg translators profile loggedInAccount False model ]
        , viewUserNameTag translators loggedInAccount profile model
        ]


viewUserNameTag : Translation.Translators -> Eos.Name -> Profile.Basic profile -> Model -> Html Msg
viewUserNameTag translators loggedInAccount profile model =
    if model.showNameTag then
        div [ class "mt-2 w-20", ariaHidden True ]
            [ Profile.viewProfileNameTag translators
                { showBg = model.showNameTagBg }
                loggedInAccount
                profile
            ]

    else
        text ""


viewUserImg : Translation.Translators -> Profile.Basic profile -> Eos.Name -> Bool -> Model -> Html Msg
viewUserImg { t, tr } profile loggedInAccount isMobile model =
    let
        container attrs =
            if isMobile then
                button (onClickNoBubble OpenedInfo :: model.extraAttrs ++ attrs)

            else
                a (Route.href (Route.Profile profile.account) :: model.extraAttrs ++ attrs)
    in
    div [ class "flex flex-col items-center" ]
        [ div [ class ("rounded-full " ++ model.imageSize) ]
            [ container
                [ ariaLabel
                    (if loggedInAccount == profile.account then
                        t "profile.summary.your_summary"

                     else
                        tr "profile.summary.user_summary"
                            [ ( "user"
                              , profile.name
                                    |> Maybe.withDefault (Eos.nameToString profile.account)
                              )
                            ]
                    )
                ]
                [ Avatar.view profile.avatar model.imageSize ]
            , if not isMobile && model.isExpanded then
                View.Components.dialogBubble
                    { class_ = "w-120 animate-fade-in opacity-0"
                    , relativeSelector = model.relativeSelector
                    , scrollSelector = model.scrollSelector
                    }
                    [ viewUserInfo profile ]

              else
                text ""
            ]
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
            profile.bio |> Maybe.withDefault Markdown.empty
    in
    div [ class "flex flex-col w-full" ]
        [ div [ class "flex mb-4 items-center justify-center" ]
            [ Avatar.view profile.avatar "w-20 h-20 mr-6 flex-shrink-0"
            , div [ class "flex items-center justify-between" ]
                [ ul [ class "text-sm text-gray-900" ]
                    [ li [ class "font-semibold text-body-black text-2xl xs-max:text-xl" ]
                        [ text userName ]
                    , li [] [ a [ href <| "mailto:" ++ email, class "hover:underline hover:text-orange-300 focus:outline-none focus:underline focus:text-orange-300" ] [ text email ] ]
                    , li [] [ text account ]
                    ]
                ]
            ]
        , Markdown.view [ class "text-sm text-gray-900" ] bio
        , div [ class "flex justify-evenly mt-6" ]
            (List.map (Contact.circularIcon "w-9 h-9 hover:opacity-75") profile.contacts)
        , a
            [ class "button button-primary w-full mt-6 cursor-pointer"
            , Route.href (Route.Profile profile.account)
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
