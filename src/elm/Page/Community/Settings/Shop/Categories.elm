module Page.Community.Settings.Shop.Categories exposing
    ( Model
    , Msg
    , init
    , msgToString
    , subscriptions
    , update
    , view
    )

import Api.Graphql.DeleteStatus
import Browser.Events
import Community
import Dict
import Dnd
import EverySet exposing (EverySet)
import Form
import Form.RichText
import Form.Text
import Form.Validate
import Graphql.Http
import Html exposing (Html, button, details, div, li, p, span, summary, text, ul)
import Html.Attributes exposing (class, classList, id, type_)
import Html.Attributes.Aria exposing (ariaHidden)
import Html.Events exposing (onClick)
import Icons
import Json.Decode
import List.Extra
import Markdown exposing (Markdown)
import Maybe.Extra
import Page
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Shop.Category
import Slug exposing (Slug)
import Svg exposing (Svg)
import Translation
import Tree
import Tree.Zipper
import UpdateResult as UR
import Utils
import View.Components
import View.Feedback
import View.Modal as Modal



-- MODEL


type alias Model =
    { expandedCategories : EverySet Shop.Category.Id
    , newCategoryState : NewCategoryState
    , categoryModalState : CategoryFormState UpdateCategoryFormInput
    , categoryMetadataModalState : CategoryFormState MetadataFormInput
    , actionsDropdown : Maybe Shop.Category.Id
    , askingForDeleteConfirmation : Maybe Shop.Category.Id
    , deleting : EverySet Shop.Category.Id
    , dnd : Dnd.Model Shop.Category.Id DropZone
    }


type DropZone
    = OnTopOf Shop.Category.Id


init : LoggedIn.Model -> UpdateResult
init _ =
    -- TODO - Should we start them all expanded?
    { expandedCategories = EverySet.empty
    , newCategoryState = NotEditing
    , categoryModalState = Closed
    , categoryMetadataModalState = Closed
    , actionsDropdown = Nothing
    , askingForDeleteConfirmation = Nothing
    , deleting = EverySet.empty
    , dnd = Dnd.init
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


type CategoryFormState formInput
    = Closed
    | Open Shop.Category.Id (Form.Model formInput)


type Msg
    = NoOp
    | PressedEsc
    | ClickedToggleExpandCategory Shop.Category.Id
    | ClickedAddCategory (Maybe Shop.Category.Id)
    | ClickedCancelAddCategory
    | ClickedCategory Shop.Category.Id
    | ClosedCategoryModal
    | GotAddCategoryFormMsg (Form.Msg NewCategoryFormInput)
    | SubmittedAddCategoryForm NewCategoryFormOutput
    | FinishedCreatingCategory (RemoteData (Graphql.Http.Error (Maybe Shop.Category.Model)) (Maybe Shop.Category.Model))
    | GotUpdateCategoryFormMsg (Form.Msg UpdateCategoryFormInput)
    | SubmittedUpdateCategoryForm UpdateCategoryFormOutput
    | FinishedUpdatingCategory (RemoteData (Graphql.Http.Error (Maybe Shop.Category.Model)) (Maybe Shop.Category.Model))
    | ClickedDeleteCategory Shop.Category.Id
    | ClosedConfirmDeleteModal
    | ConfirmedDeleteCategory Shop.Category.Id
    | CompletedDeletingCategory Shop.Category.Id (RemoteData (Graphql.Http.Error Api.Graphql.DeleteStatus.DeleteStatus) Api.Graphql.DeleteStatus.DeleteStatus)
    | ClickedShowActionsDropdown Shop.Category.Id
    | ClosedActionsDropdown
    | ClickedOpenMetadataModal Shop.Category.Id
    | GotMetadataFormMsg (Form.Msg MetadataFormInput)
    | SubmittedMetadataForm MetadataFormOutput
    | ClosedMetadataModal
    | GotDndMsg (Dnd.Msg Shop.Category.Id DropZone)


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        getCategoryZipper : Shop.Category.Id -> Maybe (Tree.Zipper.Zipper Shop.Category.Model)
        getCategoryZipper categoryId =
            case Community.getField loggedIn.selectedCommunity .shopCategories of
                RemoteData.Success ( _, categories ) ->
                    findInForest (\category -> category.id == categoryId) categories

                _ ->
                    Nothing
    in
    case msg of
        NoOp ->
            UR.init model

        PressedEsc ->
            { model
                | newCategoryState = NotEditing
                , categoryModalState = Closed
                , actionsDropdown = Nothing
                , askingForDeleteConfirmation = Nothing
            }
                |> UR.init

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
                                , description = Form.RichText.initModel "new-description-input" Nothing
                                }
                        }
            }
                |> UR.init

        ClickedCancelAddCategory ->
            { model | newCategoryState = NotEditing }
                |> UR.init

        ClickedCategory categoryId ->
            case getCategoryZipper categoryId |> Maybe.map Tree.Zipper.label of
                Just category ->
                    { model
                        | categoryModalState =
                            Open categoryId
                                (Form.init
                                    { name = category.name
                                    , description = Form.RichText.initModel "update-category-description" (Just category.description)
                                    }
                                )
                        , actionsDropdown = Nothing
                    }
                        |> UR.init

                Nothing ->
                    { model | actionsDropdown = Nothing }
                        |> UR.init

        ClosedCategoryModal ->
            { model | categoryModalState = Closed }
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
                            , parentId = parentId
                            }
                            Shop.Category.selectionSet
                        )
                        FinishedCreatingCategory
                    )

        FinishedCreatingCategory (RemoteData.Success (Just category)) ->
            let
                insertInForest : List Shop.Category.Tree -> List Shop.Category.Tree
                insertInForest forest =
                    case category.parentId of
                        Nothing ->
                            forest ++ [ Tree.singleton category ]

                        Just parentId ->
                            case findInForest (\parent -> parent.id == parentId) forest of
                                Nothing ->
                                    forest ++ [ Tree.singleton category ]

                                Just zipper ->
                                    zipper
                                        |> Tree.Zipper.mapTree (Tree.appendChild (Tree.singleton category))
                                        |> Tree.Zipper.toForest
                                        |> (\( first, others ) -> first :: others)

                insertInCommunity : UpdateResult -> UpdateResult
                insertInCommunity =
                    case Community.getField loggedIn.selectedCommunity .shopCategories of
                        RemoteData.Success ( _, categories ) ->
                            insertInForest categories
                                |> Community.ShopCategories
                                |> LoggedIn.SetCommunityField
                                |> UR.addExt

                        _ ->
                            identity
            in
            { model
                | newCategoryState =
                    EditingNewCategory
                        { parent = category.parentId
                        , form = Form.init { name = "", description = Form.RichText.initModel "new-description-input" Nothing }
                        }
            }
                |> UR.init
                |> insertInCommunity

        FinishedCreatingCategory (RemoteData.Success Nothing) ->
            { model
                | newCategoryState =
                    case model.newCategoryState of
                        NotEditing ->
                            NotEditing

                        EditingNewCategory newCategoryData ->
                            { newCategoryData | form = Form.withDisabled False newCategoryData.form }
                                |> EditingNewCategory
            }
                |> UR.init
                -- TODO - I18N
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure "Aww :(")
                |> UR.logImpossible msg
                    "Got Nothing after creating category"
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Community.Settings.Shop.Categories", function = "update" }
                    []

        FinishedCreatingCategory (RemoteData.Failure _) ->
            { model
                | newCategoryState =
                    case model.newCategoryState of
                        NotEditing ->
                            NotEditing

                        EditingNewCategory newCategoryData ->
                            { newCategoryData | form = Form.withDisabled False newCategoryData.form }
                                |> EditingNewCategory
            }
                |> UR.init
                -- TODO - I18N
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure "Aww :(")

        FinishedCreatingCategory _ ->
            UR.init model

        GotUpdateCategoryFormMsg subMsg ->
            case model.categoryModalState of
                Closed ->
                    UR.init model

                Open categoryId formModel ->
                    Form.update loggedIn.shared subMsg formModel
                        |> UR.fromChild
                            (\newFormModel -> { model | categoryModalState = Open categoryId newFormModel })
                            GotUpdateCategoryFormMsg
                            LoggedIn.addFeedback
                            model

        SubmittedUpdateCategoryForm formOutput ->
            case getCategoryZipper formOutput.id of
                Nothing ->
                    UR.init model

                Just zipper ->
                    { model
                        | categoryModalState =
                            case model.categoryModalState of
                                Closed ->
                                    Closed

                                Open categoryId formModel ->
                                    Open categoryId (Form.withDisabled True formModel)
                    }
                        |> UR.init
                        |> UR.addExt
                            (LoggedIn.mutation loggedIn
                                (Shop.Category.update (Tree.Zipper.label zipper)
                                    -- TODO - Include children
                                    { name = formOutput.name
                                    , slug = formOutput.slug
                                    , description = formOutput.description
                                    }
                                    Shop.Category.selectionSet
                                )
                                FinishedUpdatingCategory
                            )

        FinishedUpdatingCategory (RemoteData.Success (Just category)) ->
            let
                updateInCommunity : UpdateResult -> UpdateResult
                updateInCommunity =
                    case getCategoryZipper category.id of
                        Nothing ->
                            identity

                        Just zipper ->
                            zipper
                                |> Tree.Zipper.replaceLabel category
                                |> Tree.Zipper.toForest
                                |> (\( first, others ) -> first :: others)
                                |> Community.ShopCategories
                                |> LoggedIn.SetCommunityField
                                |> UR.addExt
            in
            { model | categoryModalState = Closed }
                |> UR.init
                |> updateInCommunity

        FinishedUpdatingCategory (RemoteData.Success Nothing) ->
            { model | categoryModalState = Closed }
                |> UR.init
                -- TODO - I18N
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure "Aww :(")
                |> UR.logImpossible msg
                    "Got Nothing after updating category"
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Community.Settings.Shop.Categories", function = "update" }
                    []

        FinishedUpdatingCategory (RemoteData.Failure err) ->
            { model | categoryModalState = Closed }
                |> UR.init
                -- TODO - I18N
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure "Aww :(")
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when updating category"
                    { moduleName = "Page.Community.Settings.Shop.Categories"
                    , function = "update"
                    }
                    []
                    err

        FinishedUpdatingCategory _ ->
            UR.init model

        ClickedDeleteCategory categoryId ->
            case getCategoryZipper categoryId of
                Nothing ->
                    model
                        |> UR.init

                Just zipper ->
                    if List.isEmpty (Tree.Zipper.children zipper) then
                        { model | deleting = EverySet.insert categoryId model.deleting }
                            |> UR.init
                            |> UR.addExt
                                (LoggedIn.mutation
                                    loggedIn
                                    (Api.Graphql.DeleteStatus.selectionSet
                                        (Shop.Category.delete categoryId)
                                    )
                                    (CompletedDeletingCategory categoryId)
                                )

                    else
                        { model | askingForDeleteConfirmation = Just categoryId }
                            |> UR.init

        ClosedConfirmDeleteModal ->
            { model | askingForDeleteConfirmation = Nothing }
                |> UR.init

        ConfirmedDeleteCategory categoryId ->
            { model
                | deleting = EverySet.insert categoryId model.deleting
                , askingForDeleteConfirmation = Nothing
            }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.mutation
                        loggedIn
                        (Api.Graphql.DeleteStatus.selectionSet
                            (Shop.Category.delete categoryId)
                        )
                        (CompletedDeletingCategory categoryId)
                    )

        CompletedDeletingCategory categoryId (RemoteData.Success Api.Graphql.DeleteStatus.Deleted) ->
            let
                removeFromForest : Tree.Zipper.Zipper Shop.Category.Model -> List Shop.Category.Tree
                removeFromForest zipper =
                    zipper
                        |> Tree.Zipper.removeTree
                        |> Maybe.map (Tree.Zipper.toForest >> (\( first, others ) -> first :: others))
                        |> Maybe.withDefault []

                removeFromCommunity : UpdateResult -> UpdateResult
                removeFromCommunity =
                    case getCategoryZipper categoryId of
                        Nothing ->
                            identity

                        Just zipper ->
                            zipper
                                |> removeFromForest
                                |> Community.ShopCategories
                                |> LoggedIn.SetCommunityField
                                |> UR.addExt
            in
            { model | deleting = EverySet.remove categoryId model.deleting }
                |> UR.init
                |> removeFromCommunity

        CompletedDeletingCategory categoryId (RemoteData.Success (Api.Graphql.DeleteStatus.Error reason)) ->
            { model | deleting = EverySet.remove categoryId model.deleting }
                |> UR.init
                -- TODO - I18N
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure "error :(")
                |> UR.logDeletionStatusError msg
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Community.Settings.Shop.Categories"
                    , function = "update"
                    }
                    [ { name = "Category"
                      , extras = Dict.fromList [ ( "id", Shop.Category.encodeId categoryId ) ]
                      }
                    ]
                    reason

        CompletedDeletingCategory categoryId (RemoteData.Failure err) ->
            { model | deleting = EverySet.remove categoryId model.deleting }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (loggedIn.shared.translators.t "error.unknown"))
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to delete category"
                    { moduleName = "Page.Community.Settings.Shop.Categories"
                    , function = "update"
                    }
                    [ { name = "Category"
                      , extras = Dict.fromList [ ( "id", Shop.Category.encodeId categoryId ) ]
                      }
                    ]
                    err

        CompletedDeletingCategory _ _ ->
            model
                |> UR.init

        ClickedShowActionsDropdown categoryId ->
            { model
                | actionsDropdown =
                    if model.actionsDropdown == Just categoryId then
                        Nothing

                    else
                        Just categoryId
            }
                |> UR.init

        ClosedActionsDropdown ->
            { model | actionsDropdown = Nothing }
                |> UR.init

        ClickedOpenMetadataModal categoryId ->
            { model
                | categoryMetadataModalState =
                    Open categoryId
                        (Form.init
                            -- TODO - Initialize with category's values
                            { metaTitle = ""
                            , metaDescription = ""
                            , metaKeywords = ""
                            }
                        )
            }
                |> UR.init

        GotMetadataFormMsg subMsg ->
            case model.categoryMetadataModalState of
                Closed ->
                    UR.init model

                Open categoryId formModel ->
                    Form.update loggedIn.shared subMsg formModel
                        |> UR.fromChild
                            (\newFormModel -> { model | categoryMetadataModalState = Open categoryId newFormModel })
                            GotMetadataFormMsg
                            LoggedIn.addFeedback
                            model

        SubmittedMetadataForm formOutput ->
            case getCategoryZipper formOutput.id of
                Nothing ->
                    UR.init model

                Just zipper ->
                    { model
                        | categoryMetadataModalState =
                            case model.categoryMetadataModalState of
                                Closed ->
                                    Closed

                                Open categoryId formModel ->
                                    Open categoryId (Form.withDisabled True formModel)
                    }
                        |> UR.init
                        |> UR.addExt
                            (LoggedIn.mutation loggedIn
                                (Shop.Category.updateMetadata (Tree.Zipper.label zipper)
                                    { metaTitle = formOutput.metaTitle
                                    , metaDescription = formOutput.metaDescription
                                    , metaKeywords = formOutput.metaKeywords
                                    }
                                    Shop.Category.selectionSet
                                )
                                FinishedUpdatingCategory
                            )

        ClosedMetadataModal ->
            { model | categoryMetadataModalState = Closed }
                |> UR.init

        GotDndMsg subMsg ->
            Dnd.update subMsg model.dnd
                |> UR.fromChild
                    (\newDnd -> { model | dnd = newDnd })
                    GotDndMsg
                    (updateDnd loggedIn)
                    model


updateDnd : LoggedIn.Model -> Dnd.ExtMsg Shop.Category.Id DropZone -> UpdateResult -> UpdateResult
updateDnd loggedIn ext ur =
    case ext of
        Dnd.Dropped { draggedElement, dropZone } ->
            case Community.getField loggedIn.selectedCommunity .shopCategories of
                RemoteData.Success ( _, categories ) ->
                    case findInForest (\{ id } -> id == draggedElement) categories of
                        Nothing ->
                            ur

                        Just draggedTree ->
                            let
                                insertInForest : Tree.Zipper.Zipper Shop.Category.Model -> List Shop.Category.Tree
                                insertInForest forest =
                                    case dropZone of
                                        OnTopOf parentId ->
                                            forest
                                                |> Tree.Zipper.findFromRoot (\{ id } -> id == parentId)
                                                |> Maybe.map
                                                    (Tree.Zipper.mapTree (Tree.prependChild (Tree.Zipper.tree draggedTree))
                                                        >> Tree.Zipper.toForest
                                                        >> (\( first, others ) -> first :: others)
                                                    )
                                                |> Maybe.withDefault categories
                            in
                            ur
                                |> UR.addExt
                                    (draggedTree
                                        |> Tree.Zipper.removeTree
                                        |> Maybe.map insertInForest
                                        |> Maybe.withDefault []
                                        |> Community.ShopCategories
                                        |> LoggedIn.SetCommunityField
                                    )

                _ ->
                    ur



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
        div []
            [ Page.viewHeader loggedIn title
            , case Community.getField loggedIn.selectedCommunity .shopCategories of
                RemoteData.NotAsked ->
                    viewLoading

                RemoteData.Loading ->
                    viewLoading

                RemoteData.Failure fieldErr ->
                    case fieldErr of
                        Community.CommunityError err ->
                            Page.fullPageGraphQLError title err

                        Community.FieldError err ->
                            Page.fullPageGraphQLError title err

                RemoteData.Success ( community, categories ) ->
                    if community.creator == loggedIn.accountName then
                        view_ loggedIn.shared.translators community model categories

                    else
                        Page.fullPageNotFound (loggedIn.shared.translators.t "community.edit.unauthorized") ""
            ]
    }


viewPageContainer : { children : List (Html Msg), modals : List (Html Msg) } -> Html Msg
viewPageContainer { children, modals } =
    div [ class "container mx-auto sm:px-4 sm:mt-6 sm:mb-20" ]
        (div [ class "bg-white container mx-auto pt-6 pb-7 px-4 w-full sm:px-6 sm:rounded sm:shadow-lg lg:w-2/3" ]
            children
            :: modals
        )


viewLoading : Html Msg
viewLoading =
    let
        viewBar : List (Html.Attribute Msg) -> Html Msg
        viewBar attributes =
            div (class "animate-skeleton-loading max-w-full h-8 rounded-sm mt-2" :: attributes)
                []
    in
    viewPageContainer
        { modals = []
        , children =
            [ viewBar [ class "mt-0" ]
            , viewBar [ class "ml-4" ]
            , viewBar [ class "ml-4" ]
            , viewBar [ class "!mt-6" ]
            , viewBar [ class "!mt-6" ]
            , viewBar [ class "ml-4" ]
            , viewBar [ class "ml-8" ]
            , viewBar [ class "!mt-6" ]
            ]
        }


view_ : Translation.Translators -> Community.Model -> Model -> List Shop.Category.Tree -> Html Msg
view_ translators community model categories =
    viewPageContainer
        { children =
            [ ul [ class "mb-2 grid gap-y-2" ]
                (List.map
                    (\category ->
                        li []
                            [ viewCategoryTree translators model category
                            ]
                    )
                    categories
                )
            , viewAddCategory translators [ class "w-full" ] model Nothing
            ]
        , modals =
            [ case model.categoryModalState of
                Closed ->
                    text ""

                Open categoryId formModel ->
                    case findInTrees (\category -> category.id == categoryId) categories of
                        Nothing ->
                            text ""

                        Just openCategory ->
                            viewCategoryModal translators openCategory formModel
            , case model.askingForDeleteConfirmation of
                Nothing ->
                    text ""

                Just categoryId ->
                    viewConfirmDeleteCategoryModal categoryId
            , case model.categoryMetadataModalState of
                Closed ->
                    text ""

                Open categoryId formModel ->
                    case findInTrees (\category -> category.id == categoryId) categories of
                        Nothing ->
                            text ""

                        Just openCategory ->
                            viewCategoryMetadataModal translators community openCategory formModel
            ]
        }


viewCategoryTree : Translation.Translators -> Model -> Shop.Category.Tree -> Html Msg
viewCategoryTree translators model rootTree =
    let
        rootZipper =
            Tree.Zipper.fromTree rootTree
    in
    Tree.restructure
        (\category ->
            rootZipper
                |> Tree.Zipper.findFromRoot (\{ id } -> id == category.id)
                |> Maybe.withDefault (Tree.singleton category |> Tree.Zipper.fromTree)
        )
        (viewCategoryWithChildren translators model)
        rootTree


viewCategory : Shop.Category.Model -> Html Msg
viewCategory category =
    button
        [ class "hover:underline"
        , Utils.onClickNoBubble (ClickedCategory category.id)
        ]
        [ text category.name
        ]


viewCategoryWithChildren : Translation.Translators -> Model -> Tree.Zipper.Zipper Shop.Category.Model -> List (Html Msg) -> Html Msg
viewCategoryWithChildren translators model zipper children =
    let
        category =
            Tree.Zipper.label zipper

        isOpen =
            EverySet.member category.id model.expandedCategories

        openArrowClass =
            if isOpen then
                ""

            else
                "-rotate-90"

        isParentOfNewCategoryForm =
            case model.newCategoryState of
                NotEditing ->
                    False

                EditingNewCategory { parent } ->
                    case parent of
                        Nothing ->
                            False

                        Just parentId ->
                            isAncestorOf (\childId { id } -> childId == id)
                                parentId
                                zipper

        hasActionsMenuOpen =
            case model.actionsDropdown of
                Nothing ->
                    False

                Just actionsDropdown ->
                    isAncestorOf (\childId { id } -> childId == id)
                        actionsDropdown
                        zipper

        isValidDropzone =
            case Dnd.getDraggingElement model.dnd of
                Nothing ->
                    True

                Just draggingId ->
                    let
                        isDraggingChild =
                            Tree.Zipper.children zipper
                                |> List.any
                                    (\child ->
                                        Tree.label child
                                            |> .id
                                            |> (==) draggingId
                                    )

                        isDraggingItself =
                            draggingId == category.id

                        isDraggingAncestor =
                            Tree.Zipper.findFromRoot (\{ id } -> id == draggingId) zipper
                                |> Maybe.map
                                    (\draggingZipper ->
                                        isAncestorOf2 category.id (Tree.Zipper.tree draggingZipper)
                                    )
                                |> Maybe.withDefault False
                    in
                    not isDraggingItself && not isDraggingChild && not isDraggingAncestor

        isDraggingSomething =
            Dnd.getDraggingElement model.dnd
                |> Maybe.Extra.isJust

        isDraggingOver =
            case Dnd.getDraggingOverElement model.dnd of
                Nothing ->
                    False

                Just (OnTopOf draggingOverId) ->
                    draggingOverId == category.id
    in
    div
        (class "transition-colors select-none rounded-sm border border-dashed border-transparent"
            :: classList
                [ ( "bg-gray-300 rounded-sm cursor-wait", EverySet.member category.id model.deleting )
                , ( "!bg-green/30", isValidDropzone && isDraggingSomething )
                , ( "border-black", isValidDropzone && isDraggingSomething && isDraggingOver )
                ]
            :: (if isValidDropzone then
                    Dnd.dropZone (OnTopOf category.id) GotDndMsg

                else
                    []
               )
        )
        [ details
            [ if isOpen then
                Html.Attributes.attribute "open" "true"

              else
                class ""
            , class "parent"
            , classList [ ( "pointer-events-none", EverySet.member category.id model.deleting ) ]
            ]
            [ summary
                (class "marker-hidden flex items-center rounded-sm transition-colors cursor-pointer"
                    :: classList
                        [ ( "!bg-green/20", isParentOfNewCategoryForm )
                        , ( "parent-hover:bg-orange-100/20", not isParentOfNewCategoryForm && not isDraggingSomething )
                        , ( "bg-orange-100/20", hasActionsMenuOpen )
                        ]
                    :: onClick (ClickedToggleExpandCategory category.id)
                    :: Dnd.draggable category.id GotDndMsg
                )
                [ div [ class "flex items-center w-full" ]
                    [ Icons.arrowDown (String.join " " [ "transition-transform", openArrowClass ])
                    , viewCategory category
                    ]
                , viewActions model category
                ]
            , div [ class "ml-4 flex flex-col mb-4 mt-2" ]
                [ ul
                    [ class "grid gap-y-2"
                    , classList [ ( "mb-2", not (List.isEmpty children) ) ]
                    ]
                    (List.map (\child -> li [] [ child ]) children)
                , viewAddCategory translators [] model (Just category)
                ]
            ]
        ]


viewAddCategory : Translation.Translators -> List (Html.Attribute Msg) -> Model -> Maybe Shop.Category.Model -> Html Msg
viewAddCategory translators attrs model maybeParentCategory =
    let
        parentId =
            Maybe.map .id maybeParentCategory

        viewAddCategoryButton customAttrs =
            button
                (class "flex items-center px-2 h-8 font-bold transition-colors hover:bg-orange-100/20 rounded-sm whitespace-nowrap"
                    :: onClick (ClickedAddCategory parentId)
                    :: customAttrs
                )
                [ Icons.plus "w-4 h-4 mr-2"
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
            viewAddCategoryButton attrs

        EditingNewCategory newCategoryData ->
            if newCategoryData.parent == parentId then
                Form.view (class "bg-white border border-gray-300 rounded-md p-4" :: attrs)
                    translators
                    (\submitButton ->
                        [ div [ class "flex flex-col sm:flex-row justify-end gap-4 mt-10" ]
                            [ button
                                [ class "button button-secondary w-full sm:w-40"
                                , type_ "button"
                                , onClick ClickedCancelAddCategory
                                ]
                                -- TODO - I18N
                                [ text "Cancel" ]
                            , submitButton [ class "button button-primary w-full sm:w-40" ]
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
                viewAddCategoryButton attrs


viewActions : Model -> Shop.Category.Model -> Html Msg
viewActions model category =
    let
        isDropdownOpen =
            case model.actionsDropdown of
                Nothing ->
                    False

                Just actionsDropdown ->
                    actionsDropdown == category.id
    in
    div [ class "relative" ]
        [ button
            [ class "h-8 px-2 rounded-sm transition-colors hover:bg-orange-300/30 active:bg-orange-300/60 action-opener"
            , classList [ ( "bg-orange-300/60", isDropdownOpen ) ]
            , Utils.onClickNoBubble (ClickedShowActionsDropdown category.id)
            ]
            -- TODO - Use correct icon
            [ Icons.plus "h-4 pointer-events-none" ]
        , if not isDropdownOpen then
            text ""

          else
            ul
                [ class "absolute z-10 right-0 bg-white border border-gray-300 rounded-md p-2 text-sm shadow-lg animate-fade-in-from-above-sm"
                ]
                [ li []
                    [ viewAction []
                        { icon = Icons.edit

                        -- TODO - I18N
                        , label = "Edit main information"
                        , onClickMsg = ClickedCategory category.id
                        }
                    ]
                , li []
                    [ viewAction []
                        { icon = Icons.edit

                        -- TODO - I18N
                        , label = "Edit sharing data"
                        , onClickMsg = ClickedOpenMetadataModal category.id
                        }
                    ]
                , li []
                    [ viewAction [ class "text-red hover:bg-red/10" ]
                        { icon = Icons.trash

                        -- TODO - I18N
                        , label = "Delete"
                        , onClickMsg = ClickedDeleteCategory category.id
                        }
                    ]
                ]
        ]


viewAction :
    List (Html.Attribute Msg)
    ->
        { icon : String -> Svg Msg
        , label : String
        , onClickMsg : Msg
        }
    -> Html Msg
viewAction containerAttrs { icon, label, onClickMsg } =
    button
        (class "flex items-center w-full pl-2 pr-8 py-1 rounded-md transition-colors whitespace-nowrap font-bold class hover:bg-gray-200"
            :: Utils.onClickNoBubble onClickMsg
            :: containerAttrs
        )
        [ icon "w-4 mr-2"
        , text label
        ]


viewCategoryModal : Translation.Translators -> Shop.Category.Model -> Form.Model UpdateCategoryFormInput -> Html Msg
viewCategoryModal translators category formModel =
    Modal.initWith
        { isVisible = True
        , closeMsg = ClosedCategoryModal
        }
        -- TODO - I18N
        |> Modal.withHeader "Editing category"
        |> Modal.withBody
            [ Form.viewWithoutSubmit [ class "mt-2" ]
                translators
                (\_ -> [])
                (updateCategoryForm translators category.id)
                formModel
                { toMsg = GotUpdateCategoryFormMsg }
            ]
        |> Modal.withFooter
            [ div [ class "flex flex-col w-full sm:flex-row gap-4 items-center justify-center" ]
                [ button
                    [ class "button button-secondary w-full sm:w-40"
                    , onClick ClosedCategoryModal
                    ]
                    -- TODO - I18N
                    [ text "Cancel" ]
                , button
                    [ class "button button-primary w-full sm:w-40"
                    , onClick
                        (Form.parse (updateCategoryForm translators category.id)
                            formModel
                            { onError = GotUpdateCategoryFormMsg
                            , onSuccess = SubmittedUpdateCategoryForm
                            }
                        )
                    ]
                    -- TODO - I18N
                    [ text "Save" ]
                ]
            ]
        |> Modal.withSize Modal.FullScreen
        |> Modal.toHtml


viewCategoryMetadataModal : Translation.Translators -> Community.Model -> Shop.Category.Model -> Form.Model MetadataFormInput -> Html Msg
viewCategoryMetadataModal translators community category formModel =
    Modal.initWith
        { isVisible = True
        , closeMsg = ClosedMetadataModal
        }
        |> Modal.withHeader "Editing category sharing data"
        |> Modal.withBody
            [ p [ class "mb-6" ]
                -- TODO - I18N
                [ text "This information will be used to display rich links when sharing links to this category" ]
            , Form.viewWithoutSubmit [ class "mt-2" ]
                translators
                (\_ -> [])
                (metadataForm translators community category.id)
                formModel
                { toMsg = GotMetadataFormMsg }
            ]
        |> Modal.withFooter
            [ div [ class "flex flex-col w-full sm:flex-row gap-4 items-center justify-center" ]
                [ button
                    [ class "button button-secondary w-full sm:w-40"
                    , onClick ClosedMetadataModal
                    ]
                    -- TODO - I18N
                    [ text "Cancel" ]
                , button
                    [ class "button button-primary w-full sm:w-40"
                    , onClick
                        (Form.parse (metadataForm translators community category.id)
                            formModel
                            { onError = GotMetadataFormMsg
                            , onSuccess = SubmittedMetadataForm
                            }
                        )
                    ]
                    -- TODO - I18N
                    [ text "Save" ]
                ]
            ]
        |> Modal.withSize Modal.FullScreen
        |> Modal.toHtml


viewConfirmDeleteCategoryModal : Shop.Category.Id -> Html Msg
viewConfirmDeleteCategoryModal categoryId =
    Modal.initWith
        { isVisible = True
        , closeMsg = ClosedConfirmDeleteModal
        }
        -- TODO - I18N - Include category name
        |> Modal.withHeader "Delete category"
        |> Modal.withBody
            -- TODO - I18N
            [ p [] [ text "If you delete this category, all of its sub-categories will also be permanently deleted." ]

            -- TODO - I18N
            , p [] [ text "Are you sure you want to delete this category?" ]
            ]
        |> Modal.withFooter
            [ div [ class "flex flex-col sm:flex-row gap-4" ]
                [ button
                    [ class "button button-secondary w-full sm:w-40"
                    , onClick ClosedConfirmDeleteModal
                    ]
                    -- TODO - I18N
                    [ text "Cancel" ]
                , button
                    [ class "button button-danger w-full sm:w-40"
                    , onClick (ConfirmedDeleteCategory categoryId)
                    ]
                    -- TODO - I18N
                    [ text "Delete" ]
                ]
            ]
        |> Modal.toHtml


viewShareCategoryPreview : Community.Model -> MetadataFormInput -> Html msg
viewShareCategoryPreview community values =
    div []
        [ -- TODO - I18N
          p [ class "label" ] [ text "Preview" ]

        -- TODO - I18N
        , p [ class "mb-4" ] [ text "This is an aproximation of what the shared content will look like. It will change depending on the platform the link is being shared on." ]
        , div [ class "isolate mr-3 z-10 ml-auto w-full sm:w-3/4 md:w-2/3 border border-gray-300 rounded-large relative before:absolute before:bg-white before:border-t before:border-r before:border-gray-300 before:-top-px before:rounded-br-super before:rounded-tr-sm before:-right-2 before:w-8 before:h-4 before:-z-10" ]
            [ div [ class "bg-white p-1 rounded-large" ]
                [ div [ class "flex w-full bg-gray-100 rounded-large" ]
                    [ div [ class "bg-gray-200 p-6 rounded-l-large w-1/4 flex-shrink-0 grid place-items-center" ]
                        [ Icons.image "" ]
                    , div [ class "py-2 mx-4 w-full" ]
                        [ if String.isEmpty values.metaTitle then
                            div [ class "w-3/4 bg-gray-300 rounded font-bold" ]
                                [ span
                                    [ class "opacity-0 pointer-events-none"
                                    , ariaHidden True
                                    ]
                                    -- This text is only here to define the height
                                    [ text "height" ]
                                ]

                          else
                            p [ class "font-bold line-clamp-1" ]
                                [ text values.metaTitle ]
                        , if String.isEmpty values.metaDescription then
                            div [ class "w-full bg-gray-200 rounded mt-1 text-sm" ]
                                [ span
                                    [ class "opacity-0 pointer-events-none"
                                    , ariaHidden True
                                    ]
                                    -- This text is only here to define the height
                                    [ text "height" ]
                                ]

                          else
                            p [ class "text-sm mt-1 line-clamp-2" ]
                                [ text values.metaDescription ]
                        , p [ class "text-sm opacity-70 mt-2 mb-4" ]
                            [ text community.subdomain ]
                        ]
                    ]
                , p [ class "mt-2 mb-1 ml-2 text-blue-600" ]
                    -- TODO - Show correct route in url
                    [ text ("https://" ++ community.subdomain) ]
                ]
            ]
        ]



-- FORM


type alias NewCategoryFormInput =
    { name : String
    , description : Form.RichText.Model
    }


type alias NewCategoryFormOutput =
    { name : String
    , slug : Slug
    , description : Markdown
    }


newCategoryForm : Translation.Translators -> Form.Form msg NewCategoryFormInput NewCategoryFormOutput
newCategoryForm translators =
    Form.succeed
        (\{ name, slug } description ->
            { name = name, slug = slug, description = description }
        )
        |> Form.with (nameAndSlugForm translators { nameFieldId = "new-category-name" })
        |> Form.with
            -- TODO - I18N
            (Form.RichText.init { label = "Description" }
                |> Form.richText
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.markdownLongerThan 3
                            >> Form.Validate.validate translators
                    , value = .description
                    , update = \newDescription values -> { values | description = newDescription }
                    , externalError = always Nothing
                    }
            )


type alias UpdateCategoryFormInput =
    { name : String
    , description : Form.RichText.Model
    }


type alias UpdateCategoryFormOutput =
    { id : Shop.Category.Id
    , name : String
    , slug : Slug
    , description : Markdown
    }


updateCategoryForm : Translation.Translators -> Shop.Category.Id -> Form.Form msg UpdateCategoryFormInput UpdateCategoryFormOutput
updateCategoryForm translators id =
    Form.succeed
        (\{ name, slug } description ->
            { name = name
            , slug = slug
            , description = description
            , id = id
            }
        )
        |> Form.with (nameAndSlugForm translators { nameFieldId = "update-category-name" })
        |> Form.with
            -- TODO - I18N
            (Form.RichText.init { label = "Description" }
                |> Form.RichText.withContainerAttrs [ class "mb-0" ]
                |> Form.richText
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.markdownLongerThan 3
                            >> Form.Validate.validate translators
                    , value = .description
                    , update = \newDescription values -> { values | description = newDescription }
                    , externalError = always Nothing
                    }
            )


type alias MetadataFormInput =
    { metaTitle : String
    , metaDescription : String
    , metaKeywords : String
    }


type alias MetadataFormOutput =
    { id : Shop.Category.Id
    , metaTitle : String
    , metaDescription : String

    -- TODO - Should this be a List String?
    , metaKeywords : String
    }


metadataForm : Translation.Translators -> Community.Model -> Shop.Category.Id -> Form.Form msg MetadataFormInput MetadataFormOutput
metadataForm translators community categoryId =
    Form.succeed
        (\metaTitle metaDescription metaKeywords ->
            { id = categoryId
            , metaTitle = metaTitle
            , metaDescription = metaDescription
            , metaKeywords = metaKeywords
            }
        )
        |> Form.with
            (Form.Text.init
                -- TODO - I18N
                { label = "Title"
                , id = "meta-title-input"
                }
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.stringLongerThan 3
                            >> Form.Validate.stringShorterThan 40
                            >> Form.Validate.validate translators
                    , value = .metaTitle
                    , update = \newMetaTitle values -> { values | metaTitle = newMetaTitle }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init
                -- TODO - I18N
                { label = "Description"
                , id = "meta-description-input"
                }
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.stringLongerThan 3
                            >> Form.Validate.stringShorterThan 100
                            >> Form.Validate.validate translators
                    , value = .metaDescription
                    , update = \newMetaDescription values -> { values | metaDescription = newMetaDescription }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init
                -- TODO - I18N
                { label = "Keywords"
                , id = "meta-keywords-input"
                }
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            -- TODO - Review this validation
                            >> Form.Validate.validate translators
                    , value = .metaKeywords
                    , update = \newMetaKeywords values -> { values | metaKeywords = newMetaKeywords }
                    , externalError = always Nothing
                    }
            )
        |> Form.withNoOutput ((viewShareCategoryPreview community >> Form.arbitrary) |> Form.introspect)


nameAndSlugForm : Translation.Translators -> { nameFieldId : String } -> Form.Form msg { values | name : String } { name : String, slug : Slug }
nameAndSlugForm translators { nameFieldId } =
    Form.succeed (\name slug -> { name = name, slug = slug })
        |> Form.with
            (Form.Text.init
                -- TODO - I18N
                { label = "Name"
                , id = nameFieldId
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
                                            -- Errors are shown below, on the slug field
                                            Err (\_ -> "")

                                        Just slug ->
                                            if String.length (Slug.toString slug) < 2 then
                                                -- Errors are shown below, on the slug field
                                                Err (\_ -> "")

                                            else
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
                        Form.arbitrary
                            (div [ class "mb-10" ]
                                [ View.Components.label []
                                    -- TODO - I18N
                                    { targetId = nameFieldId, labelText = "Slug" }
                                , if String.isEmpty name then
                                    span [ class "text-gray-400 italic" ]
                                        -- TODO - I18N
                                        [ text "Insert a name to generate the slug" ]

                                  else
                                    span [ class "form-error" ]
                                        -- TODO - I18N
                                        [ text "Invalid slug" ]
                                ]
                            )

                    Just slug ->
                        Form.arbitraryWith slug
                            (div [ class "mb-10" ]
                                [ View.Components.label []
                                    -- TODO - I18N
                                    { targetId = nameFieldId, labelText = "Slug" }

                                -- TODO - We should show a preview of the url, like:
                                -- TODO - "Your url will look like muda.cambiatus.io/shop/categories/organicos--1234"
                                , text (Slug.toString slug)
                                ]
                            )
             )
                |> Form.introspect
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Utils.escSubscription PressedEsc
        , case model.actionsDropdown of
            Nothing ->
                Sub.none

            Just _ ->
                Browser.Events.onClick
                    (Json.Decode.oneOf
                        [ Json.Decode.at [ "target", "className" ] Json.Decode.string
                            |> Json.Decode.andThen
                                (\targetClass ->
                                    if String.contains "action-opener" targetClass then
                                        Json.Decode.fail ""

                                    else
                                        Json.Decode.succeed ClosedActionsDropdown
                                )
                        , Json.Decode.succeed ClosedActionsDropdown
                        ]
                    )
        ]



-- UTILS


getDraggingOverCategoryId : Model -> Maybe Shop.Category.Id
getDraggingOverCategoryId model =
    Dnd.getDraggingOverElement model.dnd
        |> Maybe.map
            (\dropZone ->
                case dropZone of
                    OnTopOf id ->
                        id
            )


findInTrees : (a -> Bool) -> List (Tree.Tree a) -> Maybe a
findInTrees fn trees =
    trees
        |> List.concatMap Tree.flatten
        |> List.Extra.find fn


findInForest : (a -> Bool) -> List (Tree.Tree a) -> Maybe (Tree.Zipper.Zipper a)
findInForest fn trees =
    case trees of
        [] ->
            Nothing

        firstTree :: otherTrees ->
            Tree.Zipper.fromForest firstTree otherTrees
                |> Tree.Zipper.findFromRoot fn


isAncestorOf2 : Shop.Category.Id -> Shop.Category.Tree -> Bool
isAncestorOf2 childId parentTree =
    let
        parentId =
            Tree.label parentTree |> .id

        isItself =
            childId == parentId

        isIndirectAncestor =
            Tree.children parentTree
                |> List.any
                    (\childTree ->
                        if childId == (Tree.label childTree |> .id) then
                            True

                        else
                            isAncestorOf2 childId childTree
                    )
    in
    isItself || isIndirectAncestor


isAncestorOf : (child -> a -> Bool) -> child -> Tree.Zipper.Zipper a -> Bool
isAncestorOf equals child parentZipper =
    let
        isDirectParent =
            equals child (Tree.Zipper.label parentZipper)

        isIndirectAncestor =
            Tree.Zipper.children parentZipper
                |> List.any
                    (\childTree ->
                        if equals child (Tree.label childTree) then
                            True

                        else
                            isAncestorOf equals
                                child
                                (Tree.Zipper.fromTree childTree)
                    )
    in
    isDirectParent || isIndirectAncestor


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        PressedEsc ->
            [ "PressedEsc" ]

        ClickedToggleExpandCategory _ ->
            [ "ClickedToggleExpandCategory" ]

        ClickedAddCategory _ ->
            [ "ClickedAddCategory" ]

        ClickedCancelAddCategory ->
            [ "ClickedCancelAddCategory" ]

        ClickedCategory _ ->
            [ "ClickedCategory" ]

        ClosedCategoryModal ->
            [ "ClosedCategoryModal" ]

        GotAddCategoryFormMsg subMsg ->
            "GotAddCategoryFormMsg" :: Form.msgToString subMsg

        GotUpdateCategoryFormMsg subMsg ->
            "GotUpdateCategoryFormMsg" :: Form.msgToString subMsg

        SubmittedAddCategoryForm _ ->
            [ "SubmittedAddCategoryForm" ]

        FinishedCreatingCategory r ->
            [ "FinishedCreatingCategory", UR.remoteDataToString r ]

        SubmittedUpdateCategoryForm _ ->
            [ "SubmittedUpdateCategoryForm" ]

        FinishedUpdatingCategory r ->
            [ "FinishedUpdatingCategory", UR.remoteDataToString r ]

        ClickedDeleteCategory _ ->
            [ "ClickedDeleteCategory" ]

        ClosedConfirmDeleteModal ->
            [ "ClosedConfirmDeleteModal" ]

        ConfirmedDeleteCategory _ ->
            [ "ConfirmedDeleteCategory" ]

        CompletedDeletingCategory _ r ->
            [ "CompletedDeletingCategory", UR.remoteDataToString r ]

        ClickedShowActionsDropdown _ ->
            [ "ClickedShowActionsDropdown" ]

        ClosedActionsDropdown ->
            [ "ClosedActionsDropdown" ]

        ClickedOpenMetadataModal _ ->
            [ "ClickedOpenMetadataModal" ]

        GotMetadataFormMsg subMsg ->
            "GotMetadataFormMsg" :: Form.msgToString subMsg

        SubmittedMetadataForm _ ->
            [ "SubmittedMetadataForm" ]

        ClosedMetadataModal ->
            [ "ClosedMetadataModal" ]

        GotDndMsg subMsg ->
            "GotDndMsg" :: Dnd.msgToString subMsg
