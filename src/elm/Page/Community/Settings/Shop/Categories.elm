module Page.Community.Settings.Shop.Categories exposing
    ( Model
    , Msg
    , init
    , msgToString
    , update
    , view
    )

import Community
import EverySet exposing (EverySet)
import Html exposing (Html, button, details, li, summary, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Icons
import Page
import RemoteData
import Session.LoggedIn as LoggedIn
import Shop.Category
import Tree
import UpdateResult as UR
import Utils



-- MODEL


type alias Model =
    { expandedCategories : EverySet Shop.Category.Id
    }


init : LoggedIn.Model -> UpdateResult
init _ =
    -- TODO - Should we start them all expanded?
    { expandedCategories = EverySet.empty }
        |> UR.init
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.ShopCategoriesField)



-- TYPES


type Msg
    = NoOp
    | ClickedToggleExpandCategory Shop.Category.Id


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        ClickedToggleExpandCategory categoryId ->
            { model
                | expandedCategories =
                    if EverySet.member categoryId model.expandedCategories then
                        EverySet.remove categoryId model.expandedCategories

                    else
                        EverySet.insert categoryId model.expandedCategories
            }
                |> UR.init



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            "TODO"
    in
    { title = title
    , content =
        case Community.getField loggedIn.selectedCommunity .shopCategories of
            RemoteData.NotAsked ->
                Page.fullPageLoading loggedIn.shared

            RemoteData.Loading ->
                Page.fullPageLoading loggedIn.shared

            RemoteData.Failure fieldErr ->
                case fieldErr of
                    Community.CommunityError err ->
                        Page.fullPageGraphQLError title err

                    Community.FieldError err ->
                        Page.fullPageGraphQLError title err

            RemoteData.Success ( community, categories ) ->
                view_ model categories
    }


view_ : Model -> List Shop.Category.Tree -> Html Msg
view_ model categories =
    ul []
        (List.map (viewCategoryTree model) categories)


viewCategoryTree : Model -> Shop.Category.Tree -> Html Msg
viewCategoryTree model =
    Tree.restructure identity (viewCategoryWithChildren model)


viewCategory : Shop.Category.Model -> Html msg
viewCategory category =
    text category.name


viewCategoryWithChildren : Model -> Shop.Category.Model -> List (Html Msg) -> Html Msg
viewCategoryWithChildren model category children =
    let
        isOpen =
            EverySet.member category.id model.expandedCategories

        openArrowClass =
            if isOpen then
                ""

            else
                "-rotate-90"
    in
    if List.isEmpty children then
        li [ class "flex items-center" ]
            [ button
                [ class "opacity-0 pointer-events-none"
                , onClick (ClickedToggleExpandCategory category.id)
                ]
                [ Icons.arrowDown openArrowClass
                ]
            , viewCategory category
            ]

    else
        li []
            [ details
                [ if isOpen then
                    Html.Attributes.attribute "open" "true"

                  else
                    class ""
                , Utils.onClickPreventAll NoOp
                ]
                [ summary [ class "marker-hidden flex items-center" ]
                    [ button [ onClick (ClickedToggleExpandCategory category.id) ]
                        [ Icons.arrowDown ("transition-transform " ++ openArrowClass)
                        ]
                    , viewCategory category
                    ]
                , ul [ class "ml-4" ] children
                ]
            ]



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        ClickedToggleExpandCategory _ ->
            [ "ClickedToggleExpandCategory" ]
