module Page.Community.Settings.Shop.Categories exposing
    ( Model
    , Msg
    , init
    , msgToString
    , subscriptions
    , update
    , view
    )

import Community
import EverySet exposing (EverySet)
import Form
import Form.Text
import Form.Validate
import Graphql.Http
import Html exposing (Html, button, details, div, li, span, summary, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Icons
import Json.Decode
import Page
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Shop.Category
import Slug exposing (Slug)
import Translation
import Tree
import UpdateResult as UR
import Utils
import View.Feedback



-- MODEL


type alias Model =
    { expandedCategories : EverySet Shop.Category.Id
    , newCategoryState : NewCategoryState
    }


init : LoggedIn.Model -> UpdateResult
init _ =
    -- TODO - Should we start them all expanded?
    { expandedCategories = EverySet.empty

    -- TODO - Should we allow multiple editors to be open at once?
    , newCategoryState = NotEditing
    }
        |> UR.init
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.ShopCategoriesField)



-- TYPES


type NewCategoryState
    = NotEditing
    | EditingNewCategory
        { parent : Maybe Shop.Category.Id
        , form : Form.Model NewCategoryFormInput
        }


type Msg
    = NoOp
    | ClickedToggleExpandCategory Shop.Category.Id
    | ClickedAddCategory (Maybe Shop.Category.Id)
    | ClickedCancelAddCategory
    | GotAddCategoryFormMsg (Form.Msg NewCategoryFormInput)
    | SubmittedAddCategoryForm NewCategoryFormOutput
    | FinishedCreatingCategory (RemoteData (Graphql.Http.Error (Maybe Shop.Category.Model)) (Maybe Shop.Category.Model))


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

        ClickedAddCategory maybeParentId ->
            { model
                | newCategoryState =
                    EditingNewCategory
                        { parent = maybeParentId
                        , form =
                            Form.init
                                { name = ""
                                , description = ""
                                }
                        }
            }
                |> UR.init

        ClickedCancelAddCategory ->
            { model | newCategoryState = NotEditing }
                |> UR.init

        GotAddCategoryFormMsg subMsg ->
            case model.newCategoryState of
                NotEditing ->
                    UR.init model

                EditingNewCategory newCategoryData ->
                    Form.update loggedIn.shared subMsg newCategoryData.form
                        |> UR.fromChild
                            (\newForm ->
                                { model
                                    | newCategoryState =
                                        { newCategoryData | form = newForm }
                                            |> EditingNewCategory
                                }
                            )
                            GotAddCategoryFormMsg
                            LoggedIn.addFeedback
                            model

        SubmittedAddCategoryForm { name, slug, description } ->
            let
                parentId =
                    case model.newCategoryState of
                        NotEditing ->
                            Nothing

                        EditingNewCategory { parent } ->
                            parent
            in
            { model
                | newCategoryState =
                    case model.newCategoryState of
                        NotEditing ->
                            NotEditing

                        EditingNewCategory newCategoryData ->
                            { newCategoryData | form = Form.withDisabled True newCategoryData.form }
                                |> EditingNewCategory
            }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.mutation loggedIn
                        (Shop.Category.create
                            { name = name
                            , description = description
                            , slug = slug
                            , icon = Nothing
                            , image = Nothing
                            , parentId = parentId
                            }
                        )
                        FinishedCreatingCategory
                    )

        FinishedCreatingCategory (RemoteData.Success _) ->
            -- TODO - Should we open the form to create another category?
            { model | newCategoryState = NotEditing }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Success "Yay!")

        FinishedCreatingCategory (RemoteData.Failure _) ->
            UR.init model
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure "Aww :(")

        FinishedCreatingCategory _ ->
            UR.init model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            -- TODO - I18N
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

            RemoteData.Success ( _, categories ) ->
                view_ loggedIn.shared.translators model categories
    }


view_ : Translation.Translators -> Model -> List Shop.Category.Tree -> Html Msg
view_ translators model categories =
    div []
        [ ul []
            (List.map
                (\category ->
                    li []
                        [ viewCategoryTree translators model category
                        ]
                )
                categories
            )
        , viewAddCategory translators model Nothing
        ]


viewCategoryTree : Translation.Translators -> Model -> Shop.Category.Tree -> Html Msg
viewCategoryTree translators model =
    Tree.restructure identity (viewCategoryWithChildren translators model)


viewCategory : Shop.Category.Model -> Html msg
viewCategory category =
    text category.name


viewCategoryWithChildren : Translation.Translators -> Model -> Shop.Category.Model -> List (Html Msg) -> Html Msg
viewCategoryWithChildren translators model category children =
    let
        isOpen =
            EverySet.member category.id model.expandedCategories

        openArrowClass =
            if isOpen then
                ""

            else
                "-rotate-90"
    in
    li
        [ classList [ ( "ml-8", List.isEmpty children ) ]
        ]
        (if List.isEmpty children then
            [ viewCategory category
            , viewAddCategory translators model (Just category)
            ]

         else
            [ details
                [ if isOpen then
                    Html.Attributes.attribute "open" "true"

                  else
                    class ""
                ]
                [ summary
                    [ class "marker-hidden flex items-center"
                    , Html.Events.preventDefaultOn "click"
                        (Json.Decode.succeed ( NoOp, True ))
                    ]
                    [ button
                        [ onClick (ClickedToggleExpandCategory category.id)
                        , classList [ ( "opacity-0 pointer-events-none", List.isEmpty children ) ]
                        ]
                        [ Icons.arrowDown (String.join " " [ "transition-transform", openArrowClass ])
                        ]
                    , viewCategory category
                    ]
                , div
                    [ class "ml-4" ]
                    [ ul [] children
                    , viewAddCategory translators model (Just category)
                    ]
                ]
            ]
        )


viewAddCategory : Translation.Translators -> Model -> Maybe Shop.Category.Model -> Html Msg
viewAddCategory translators model maybeParentCategory =
    let
        parentId =
            Maybe.map .id maybeParentCategory

        viewAddCategoryButton =
            button
                [ class "flex ml-4 items-center"
                , onClick (ClickedAddCategory parentId)
                ]
                [ Icons.plus "w-3 h-3 mr-2"
                , case maybeParentCategory of
                    Nothing ->
                        -- TODO - I18N
                        text "Add new category"

                    Just { name } ->
                        -- TODO - I18N
                        text ("Add sub-category of " ++ name)
                ]
    in
    case model.newCategoryState of
        NotEditing ->
            viewAddCategoryButton

        EditingNewCategory newCategoryData ->
            if newCategoryData.parent == parentId then
                Form.view [ class "ml-4 bg-white border border-black rounded-md p-6" ]
                    translators
                    (\submitButton ->
                        [ div [ class "flex justify-end gap-4" ]
                            [ button
                                [ class "button button-secondary"
                                , onClick ClickedCancelAddCategory
                                ]
                                -- TODO - I18N
                                [ text "Cancel" ]
                            , submitButton [ class "button button-primary" ]
                                -- TODO - I18N
                                [ text "Create" ]
                            ]
                        ]
                    )
                    (newCategoryForm translators)
                    newCategoryData.form
                    { toMsg = GotAddCategoryFormMsg
                    , onSubmit = SubmittedAddCategoryForm
                    }

            else
                viewAddCategoryButton



-- FORM


type alias NewCategoryFormInput =
    { name : String
    , description : String
    }


type alias NewCategoryFormOutput =
    { name : String
    , slug : Slug
    , description : String
    }


newCategoryForm : Translation.Translators -> Form.Form msg NewCategoryFormInput NewCategoryFormOutput
newCategoryForm translators =
    Form.succeed NewCategoryFormOutput
        |> Form.with
            (Form.Text.init
                -- TODO - I18N
                { label = "Name"
                , id = "new-category-name-input"
                }
                |> Form.Text.withContainerAttrs [ class "mb-4" ]
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.stringLongerThan 2
                            >> Form.Validate.custom
                                (\name ->
                                    case Slug.generate name of
                                        Nothing ->
                                            -- TODO - Show meaningful error?
                                            Err (\_ -> "")

                                        Just _ ->
                                            Ok name
                                )
                            >> Form.Validate.validate translators
                    , value = .name
                    , update = \newName values -> { values | name = newName }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            ((\{ name } ->
                case Slug.generate name of
                    Nothing ->
                        -- TODO - Show error message?
                        Form.arbitrary
                            (div [ class "mb-10" ]
                                [ span [ class "label" ]
                                    -- TODO - I18N
                                    [ text "Slug" ]
                                , span [ class "text-gray-400 italic" ]
                                    -- TODO - I18N
                                    [ text "Insert a name to generate the slug" ]
                                ]
                            )

                    Just slug ->
                        Form.arbitraryWith slug
                            (div [ class "mb-10" ]
                                [ span [ class "label" ]
                                    -- TODO - I18N
                                    [ text "Slug" ]
                                , text (Slug.toString slug)
                                ]
                            )
             )
                |> Form.introspect
            )
        |> Form.with
            (Form.Text.init
                -- TODO - I18N
                { label = "Description"
                , id = "new-category-description"
                }
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.stringLongerThan 3
                            >> Form.Validate.validate translators
                    , value = .description
                    , update = \newDescription values -> { values | description = newDescription }
                    , externalError = always Nothing
                    }
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.newCategoryState of
        NotEditing ->
            Sub.none

        EditingNewCategory _ ->
            Utils.escSubscription ClickedCancelAddCategory



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        ClickedToggleExpandCategory _ ->
            [ "ClickedToggleExpandCategory" ]

        ClickedAddCategory _ ->
            [ "ClickedAddCategory" ]

        ClickedCancelAddCategory ->
            [ "ClickedCancelAddCategory" ]

        GotAddCategoryFormMsg subMsg ->
            "GotAddCategoryFormMsg" :: Form.msgToString subMsg

        SubmittedAddCategoryForm _ ->
            [ "SubmittedAddCategoryForm" ]

        FinishedCreatingCategory r ->
            [ "FinishedCreatingCategory", UR.remoteDataToString r ]
