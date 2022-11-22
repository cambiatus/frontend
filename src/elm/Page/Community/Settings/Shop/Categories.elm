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
import Browser.Dom
import Browser.Events
import Community
import Dict
import Dnd
import EverySet exposing (EverySet)
import Form
import Form.File
import Form.RichText
import Form.Text
import Form.Validate
import Graphql.Http
import Graphql.SelectionSet
import Html exposing (Html, button, details, div, h2, img, li, menu, p, span, summary, text, ul)
import Html.Attributes exposing (alt, class, classList, disabled, src, style, type_)
import Html.Attributes.Aria exposing (ariaHasPopup, ariaHidden, ariaLabel)
import Html.Events exposing (onClick)
import Icons
import Json.Decode
import List.Extra
import Markdown exposing (Markdown)
import Maybe.Extra
import Page
import Process
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Shop.Category
import Slug exposing (Slug)
import Svg exposing (Svg)
import Task
import Translation
import Tree
import Tree.Zipper
import UpdateResult as UR
import Utils
import Utils.Tree
import View.Components
import View.Feedback
import View.Modal as Modal



-- MODEL


type alias Model =
    { expandedCategories : EverySet Shop.Category.Id
    , newCategoryState : NewCategoryState
    , categoryModalState : CategoryFormState UpdateCategoryFormInput
    , categoryMetadataModalState : CategoryFormState MetadataFormInput
    , actionsDropdown : DropdownState
    , askingForDeleteConfirmation : Maybe Shop.Category.Id
    , deleting : EverySet Shop.Category.Id
    , dnd : Dnd.Model Shop.Category.Id DropZone
    }


type DropdownState
    = DropdownClosed
    | DropdownOpenOnMouse { x : Float, y : Float } Shop.Category.Id Browser.Dom.Viewport
    | DropdownOpenOnButton Shop.Category.Id


type DropZone
    = OnTopOf Shop.Category.Id
      -- TODO - Add options to select position
    | OnRoot


init : LoggedIn.Model -> UpdateResult
init _ =
    { expandedCategories = EverySet.empty
    , newCategoryState = NotEditing
    , categoryModalState = Closed
    , categoryMetadataModalState = Closed
    , actionsDropdown = DropdownClosed
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
    | ClickedEditCategory Shop.Category.Id
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
    | OpenedContextMenuForAction { x : Float, y : Float } Shop.Category.Id
    | GotViewportForContextMenuForAction { x : Float, y : Float } Shop.Category.Id Browser.Dom.Viewport
    | ClosedActionsDropdown
    | ClickedOpenMetadataModal Shop.Category.Id
    | GotMetadataFormMsg (Form.Msg MetadataFormInput)
    | SubmittedMetadataForm MetadataFormOutput
    | ClosedMetadataModal
    | GotDndMsg (Dnd.Msg Shop.Category.Id DropZone)
    | DraggedOverCategoryForAWhile Shop.Category.Id
    | CompletedMovingCategory Shop.Category.Id (RemoteData (Graphql.Http.Error (Maybe Shop.Category.Id)) (Maybe Shop.Category.Id))
    | CompletedMovingCategoryToRoot Shop.Category.Id (RemoteData (Graphql.Http.Error (Maybe ())) (Maybe ()))
    | ClickedMoveUp Shop.Category.Id
    | ClickedMoveDown Shop.Category.Id
    | GotKeywordDndMsg (Dnd.Msg Int Int)
    | ClickedRemoveMetaKeyword Int


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        { t } =
            loggedIn.shared.translators

        getCategoryZipper : Shop.Category.Id -> Maybe (Tree.Zipper.Zipper Shop.Category.Model)
        getCategoryZipper categoryId =
            case Community.getField loggedIn.selectedCommunity .shopCategories of
                RemoteData.Success ( _, categories ) ->
                    Utils.Tree.findZipperInForest (\category -> category.id == categoryId) categories

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
                , actionsDropdown = DropdownClosed
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
                , newCategoryState =
                    case model.newCategoryState of
                        NotEditing ->
                            NotEditing

                        EditingNewCategory { parent } ->
                            case parent of
                                Nothing ->
                                    model.newCategoryState

                                Just parentId ->
                                    getCategoryZipper categoryId
                                        |> Maybe.map
                                            (\categoryZipper ->
                                                if isAncestorOf parentId (Tree.Zipper.tree categoryZipper) then
                                                    NotEditing

                                                else
                                                    model.newCategoryState
                                            )
                                        |> Maybe.withDefault model.newCategoryState
            }
                |> UR.init

        ClickedAddCategory maybeParentId ->
            { model
                | newCategoryState =
                    EditingNewCategory
                        { parent = maybeParentId
                        , form =
                            Form.init
                                { icon = Form.File.initSingle { fileUrl = Nothing, aspectRatio = Just 1 }
                                , name = ""
                                , description = Form.RichText.initModel (newDescriptionInputId maybeParentId) Nothing
                                , image = Form.File.initSingle { fileUrl = Nothing, aspectRatio = Nothing }
                                }
                        }
            }
                |> UR.init
                |> UR.addCmd
                    (Browser.Dom.focus "new-category-icon"
                        |> Task.attempt (\_ -> NoOp)
                    )

        ClickedCancelAddCategory ->
            { model | newCategoryState = NotEditing }
                |> UR.init

        ClickedEditCategory categoryId ->
            case getCategoryZipper categoryId |> Maybe.map Tree.Zipper.label of
                Just category ->
                    { model
                        | categoryModalState =
                            Open categoryId
                                (Form.init
                                    { icon =
                                        Form.File.initSingle
                                            { fileUrl = category.icon
                                            , aspectRatio = Just 1
                                            }
                                    , name = category.name
                                    , description =
                                        Form.RichText.initModel
                                            ("update-category-description-" ++ Shop.Category.idToString categoryId)
                                            (Just category.description)
                                    , image =
                                        Form.File.initSingle
                                            { fileUrl = category.image
                                            , aspectRatio = Nothing
                                            }
                                    }
                                )
                        , actionsDropdown = DropdownClosed
                    }
                        |> UR.init

                Nothing ->
                    { model | actionsDropdown = DropdownClosed }
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

        SubmittedAddCategoryForm { icon, name, slug, description, image } ->
            let
                parentId =
                    case model.newCategoryState of
                        NotEditing ->
                            Nothing

                        EditingNewCategory { parent } ->
                            parent

                position =
                    case Community.getField loggedIn.selectedCommunity .shopCategories of
                        RemoteData.Success ( _, categories ) ->
                            case parentId of
                                Nothing ->
                                    categories
                                        |> List.map (Tree.label >> .position)
                                        |> List.maximum
                                        |> Maybe.map ((+) 1)
                                        |> Maybe.withDefault 0

                                Just validParentId ->
                                    categories
                                        |> Utils.Tree.findZipperInForest (\category -> category.id == validParentId)
                                        |> Maybe.andThen
                                            (Tree.Zipper.tree
                                                >> Tree.children
                                                >> List.map (Tree.label >> .position)
                                                >> List.maximum
                                                >> Maybe.map ((+) 1)
                                            )
                                        |> Maybe.withDefault 0

                        _ ->
                            0
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
                            { icon = icon
                            , name = name
                            , description = description
                            , slug = slug
                            , image = image
                            , parentId = parentId
                            , position = position
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
                            case Utils.Tree.findZipperInForest (\parent -> parent.id == parentId) forest of
                                Nothing ->
                                    forest ++ [ Tree.singleton category ]

                                Just zipper ->
                                    zipper
                                        |> Tree.Zipper.mapTree (Tree.appendChild (Tree.singleton category))
                                        |> Utils.Tree.toFlatForest

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
            { model | newCategoryState = NotEditing }
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
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (t "shop.categories.create_error"))
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
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (t "shop.categories.create_error"))

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
                                    { icon = formOutput.icon
                                    , name = formOutput.name
                                    , slug = formOutput.slug
                                    , description = formOutput.description
                                    , image = formOutput.image
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
                                |> Utils.Tree.toFlatForest
                                |> Community.ShopCategories
                                |> LoggedIn.SetCommunityField
                                |> UR.addExt
            in
            { model
                | categoryModalState = Closed
                , categoryMetadataModalState = Closed
            }
                |> UR.init
                |> updateInCommunity

        FinishedUpdatingCategory (RemoteData.Success Nothing) ->
            { model | categoryModalState = Closed }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (t "shop.categories.update_error"))
                |> UR.logImpossible msg
                    "Got Nothing after updating category"
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Community.Settings.Shop.Categories", function = "update" }
                    []

        FinishedUpdatingCategory (RemoteData.Failure err) ->
            { model | categoryModalState = Closed }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (t "shop.categories.update_error"))
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
                        |> Maybe.map Utils.Tree.toFlatForest
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
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (t "shop.categories.delete_error"))
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
                    case model.actionsDropdown of
                        DropdownClosed ->
                            DropdownOpenOnButton categoryId

                        DropdownOpenOnButton _ ->
                            DropdownClosed

                        DropdownOpenOnMouse _ _ _ ->
                            DropdownOpenOnButton categoryId
            }
                |> UR.init

        OpenedContextMenuForAction coordinates categoryId ->
            let
                getViewport =
                    Browser.Dom.getViewport
                        |> Task.perform (\viewport -> GotViewportForContextMenuForAction coordinates categoryId viewport)
            in
            model
                |> UR.init
                |> UR.addCmd getViewport

        GotViewportForContextMenuForAction coordinates categoryId viewport ->
            { model
                | actionsDropdown =
                    case model.actionsDropdown of
                        DropdownClosed ->
                            DropdownOpenOnMouse coordinates categoryId viewport

                        DropdownOpenOnButton _ ->
                            DropdownOpenOnMouse coordinates categoryId viewport

                        DropdownOpenOnMouse _ _ _ ->
                            DropdownClosed
            }
                |> UR.init

        ClosedActionsDropdown ->
            { model | actionsDropdown = DropdownClosed }
                |> UR.init

        ClickedOpenMetadataModal categoryId ->
            case getCategoryZipper categoryId of
                Nothing ->
                    UR.init model

                Just zipper ->
                    let
                        category =
                            Tree.Zipper.label zipper
                    in
                    { model
                        | categoryMetadataModalState =
                            Open categoryId
                                (Form.init
                                    { metaTitle = Maybe.withDefault category.name category.metaTitle
                                    , metaDescription = Maybe.withDefault (Markdown.toUnformattedString category.description) category.metaDescription
                                    , metaKeyword = ""
                                    , metaKeywords = category.metaKeywords
                                    , keywordsDnd = Dnd.init
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
                    let
                        category =
                            Tree.Zipper.label zipper
                    in
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
                                (Shop.Category.updateMetadata category
                                    { metaTitle =
                                        if Maybe.Extra.isNothing category.metaTitle && formOutput.metaTitle == category.name then
                                            Nothing

                                        else
                                            Just formOutput.metaTitle
                                    , metaDescription =
                                        if Maybe.Extra.isNothing category.metaDescription && formOutput.metaDescription == Markdown.toUnformattedString category.description then
                                            Nothing

                                        else
                                            Just formOutput.metaDescription
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

        DraggedOverCategoryForAWhile categoryId ->
            case Dnd.getDraggingOverElement model.dnd of
                Nothing ->
                    UR.init model

                Just (OnTopOf currentCategoryId) ->
                    if currentCategoryId == categoryId then
                        { model | expandedCategories = EverySet.insert categoryId model.expandedCategories }
                            |> UR.init

                    else
                        UR.init model

                Just OnRoot ->
                    UR.init model

        CompletedMovingCategory categoryId (RemoteData.Success (Just parentId)) ->
            case getCategoryZipper categoryId of
                Just childZipper ->
                    let
                        zipperWithMovedChild =
                            moveChild
                                { childZipper = childZipper
                                , parentId = parentId
                                }
                    in
                    { model
                        | expandedCategories =
                            zipperWithMovedChild
                                |> Tree.Zipper.findFromRoot (\{ id } -> id == categoryId)
                                |> Maybe.map
                                    (Utils.Tree.getAllAncestors
                                        >> List.foldl
                                            (Tree.Zipper.label
                                                >> .id
                                                >> EverySet.insert
                                            )
                                            model.expandedCategories
                                    )
                                |> Maybe.withDefault model.expandedCategories
                    }
                        |> UR.init
                        |> UR.addExt
                            (zipperWithMovedChild
                                |> Utils.Tree.toFlatForest
                                |> Community.ShopCategories
                                |> LoggedIn.SetCommunityField
                            )

                Nothing ->
                    model
                        |> UR.init

        CompletedMovingCategory categoryId (RemoteData.Success Nothing) ->
            UR.init model
                |> UR.logImpossible msg
                    "Got Nothing when trying to move a child category"
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Community.Settings.Shop.Categories"
                    , function = "update"
                    }
                    [ { name = "Category"
                      , extras = Dict.fromList [ ( "id", Shop.Category.encodeId categoryId ) ]
                      }
                    ]

        CompletedMovingCategory categoryId (RemoteData.Failure err) ->
            UR.init model
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to move a child category"
                    { moduleName = "Page.Community.Settings.Shop.Categories"
                    , function = "update"
                    }
                    [ { name = "Category"
                      , extras = Dict.fromList [ ( "id", Shop.Category.encodeId categoryId ) ]
                      }
                    ]
                    err
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (t "shop.categories.reorder_error"))

        CompletedMovingCategory _ _ ->
            UR.init model

        CompletedMovingCategoryToRoot categoryId (RemoteData.Success (Just ())) ->
            case Community.getField loggedIn.selectedCommunity .shopCategories of
                RemoteData.Success ( _, categories ) ->
                    case Utils.Tree.findZipperInForest (\{ id } -> id == categoryId) categories of
                        Nothing ->
                            UR.init model

                        Just childZipper ->
                            let
                                zipperWithMovedChild =
                                    moveToRoot { childZipper = childZipper }
                            in
                            model
                                |> UR.init
                                |> UR.addExt
                                    (zipperWithMovedChild
                                        |> Utils.Tree.toFlatForest
                                        |> Community.ShopCategories
                                        |> LoggedIn.SetCommunityField
                                    )

                _ ->
                    UR.init model

        CompletedMovingCategoryToRoot categoryId (RemoteData.Success Nothing) ->
            UR.init model
                |> UR.logImpossible msg
                    "Got Nothing when trying to move a child category to root"
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Community.Settings.Shop.Categories"
                    , function = "update"
                    }
                    [ { name = "Category"
                      , extras = Dict.fromList [ ( "id", Shop.Category.encodeId categoryId ) ]
                      }
                    ]

        CompletedMovingCategoryToRoot categoryId (RemoteData.Failure err) ->
            UR.init model
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to move a child category to root"
                    { moduleName = "Page.Community.Settings.Shop.Categories"
                    , function = "update"
                    }
                    [ { name = "Category"
                      , extras = Dict.fromList [ ( "id", Shop.Category.encodeId categoryId ) ]
                      }
                    ]
                    err
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (t "shop.categories.reorder_error"))

        CompletedMovingCategoryToRoot _ _ ->
            UR.init model

        ClickedMoveUp categoryId ->
            case getCategoryZipper categoryId of
                Just zipper ->
                    case Utils.Tree.goUpWithoutChildren zipper of
                        Nothing ->
                            model
                                |> UR.init
                                |> UR.addExt
                                    (LoggedIn.mutation loggedIn
                                        (Shop.Category.moveToRoot categoryId
                                            (Graphql.SelectionSet.succeed ())
                                        )
                                        (CompletedMovingCategoryToRoot categoryId)
                                    )

                        Just newParentZipper ->
                            model
                                |> UR.init
                                |> UR.addExt
                                    (LoggedIn.mutation loggedIn
                                        (Shop.Category.addChild (Tree.Zipper.tree newParentZipper)
                                            categoryId
                                            Shop.Category.idSelectionSet
                                        )
                                        (CompletedMovingCategory categoryId)
                                    )

                Nothing ->
                    UR.init model

        ClickedMoveDown categoryId ->
            case getCategoryZipper categoryId of
                Just zipper ->
                    case Utils.Tree.goDownWithoutChildren zipper of
                        Nothing ->
                            model
                                |> UR.init
                                |> UR.addExt
                                    (LoggedIn.mutation loggedIn
                                        (Shop.Category.moveToRoot categoryId
                                            (Graphql.SelectionSet.succeed ())
                                        )
                                        (CompletedMovingCategoryToRoot categoryId)
                                    )

                        Just newParentZipper ->
                            model
                                |> UR.init
                                |> UR.addExt
                                    (LoggedIn.mutation loggedIn
                                        (Shop.Category.addChild (Tree.Zipper.tree newParentZipper)
                                            categoryId
                                            Shop.Category.idSelectionSet
                                        )
                                        (CompletedMovingCategory categoryId)
                                    )

                Nothing ->
                    UR.init model

        GotKeywordDndMsg subMsg ->
            case model.categoryMetadataModalState of
                Closed ->
                    UR.init model

                Open categoryId formModel ->
                    Dnd.update subMsg (Form.getValue .keywordsDnd formModel)
                        |> UR.fromChild
                            (\newDnd ->
                                { model
                                    | categoryMetadataModalState =
                                        Open categoryId
                                            (Form.updateValues
                                                (\values -> { values | keywordsDnd = newDnd })
                                                formModel
                                            )
                                }
                            )
                            GotKeywordDndMsg
                            updateKeywordsDnd
                            model

        ClickedRemoveMetaKeyword index ->
            case model.categoryMetadataModalState of
                Closed ->
                    UR.init model

                Open categoryId formModel ->
                    { model
                        | categoryMetadataModalState =
                            formModel
                                |> Form.updateValues (\values -> { values | metaKeywords = List.Extra.removeAt index values.metaKeywords })
                                |> Open categoryId
                    }
                        |> UR.init


updateKeywordsDnd : Dnd.ExtMsg Int Int -> UpdateResult -> UpdateResult
updateKeywordsDnd ext ur =
    case ext of
        Dnd.Dropped { draggedElement, dropZone } ->
            case ur.model.categoryMetadataModalState of
                Closed ->
                    ur

                Open categoryId formModel ->
                    UR.mapModel
                        (\model ->
                            { model
                                | categoryMetadataModalState =
                                    Open categoryId
                                        (Form.updateValues
                                            (\values ->
                                                { values
                                                    | metaKeywords =
                                                        List.Extra.swapAt draggedElement
                                                            dropZone
                                                            values.metaKeywords
                                                }
                                            )
                                            formModel
                                        )
                            }
                        )
                        ur

        Dnd.DraggedOver _ ->
            ur


updateDnd : LoggedIn.Model -> Dnd.ExtMsg Shop.Category.Id DropZone -> UpdateResult -> UpdateResult
updateDnd loggedIn ext ur =
    case ext of
        Dnd.Dropped { draggedElement, dropZone } ->
            case Community.getField loggedIn.selectedCommunity .shopCategories of
                RemoteData.Success ( _, categories ) ->
                    case dropZone of
                        OnTopOf parentId ->
                            case Utils.Tree.findZipperInForest (\{ id } -> id == parentId) categories of
                                Nothing ->
                                    ur

                                Just parentZipper ->
                                    UR.addExt
                                        (LoggedIn.mutation loggedIn
                                            (Shop.Category.addChild (Tree.Zipper.tree parentZipper)
                                                draggedElement
                                                Shop.Category.idSelectionSet
                                            )
                                            (CompletedMovingCategory draggedElement)
                                        )
                                        ur

                        OnRoot ->
                            UR.addExt
                                (LoggedIn.mutation loggedIn
                                    (Shop.Category.moveToRoot draggedElement
                                        (Graphql.SelectionSet.succeed ())
                                    )
                                    (CompletedMovingCategoryToRoot draggedElement)
                                )
                                ur

                _ ->
                    ur

        Dnd.DraggedOver (OnTopOf categoryId) ->
            let
                millisToWait =
                    250
            in
            ur
                |> UR.addCmd
                    (Process.sleep millisToWait
                        |> Task.perform (\_ -> DraggedOverCategoryForAWhile categoryId)
                    )

        Dnd.DraggedOver OnRoot ->
            ur


newDescriptionInputId : Maybe Shop.Category.Id -> String
newDescriptionInputId maybeParentId =
    case maybeParentId of
        Nothing ->
            "new-description-input-root"

        Just parentId ->
            "new-description-input-" ++ Shop.Category.idToString parentId



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            loggedIn.shared.translators.t "settings.shop.categories.title"
    in
    { title = title
    , content =
        div []
            [ Page.viewHeader loggedIn title
            , case Community.getField loggedIn.selectedCommunity .shopCategories of
                RemoteData.NotAsked ->
                    viewLoading loggedIn.shared.translators model

                RemoteData.Loading ->
                    viewLoading loggedIn.shared.translators model

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


viewPageContainer : Translation.Translators -> { children : List (Html Msg), modals : List (Html Msg) } -> Model -> Html Msg
viewPageContainer translators { children, modals } model =
    let
        isActionsDropdownOpen =
            case model.actionsDropdown of
                DropdownClosed ->
                    False

                DropdownOpenOnButton _ ->
                    True

                DropdownOpenOnMouse _ _ _ ->
                    True
    in
    div [ class "container mx-auto sm:px-4 sm:mt-6 pb-40 overflow-x-hidden" ]
        (div
            [ class "bg-white container mx-auto pt-6 pb-7 w-full px-4 sm:px-6 sm:rounded sm:shadow-lg lg:w-2/3"
            , classList
                [ ( "overflow-x-scroll", not isActionsDropdownOpen )
                , ( "overflow-y-visible", isActionsDropdownOpen )
                ]
            ]
            (p [ class "text-gray-900 mb-10" ]
                [ text <| translators.t "shop.categories.admin_page_description" ]
                :: children
            )
            :: modals
        )


viewLoading : Translation.Translators -> Model -> Html Msg
viewLoading translators model =
    let
        viewBar : List (Html.Attribute Msg) -> Html Msg
        viewBar attributes =
            div (class "animate-skeleton-loading max-w-full h-8 rounded-sm mt-2" :: attributes)
                []
    in
    viewPageContainer translators
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
        model


view_ : Translation.Translators -> Community.Model -> Model -> List Shop.Category.Tree -> Html Msg
view_ translators community model categories =
    let
        isDraggingSomething =
            Maybe.Extra.isJust (Dnd.getDraggingElement model.dnd)

        isDraggingRootCategory =
            case Dnd.getDraggingElement model.dnd of
                Nothing ->
                    False

                Just draggingId ->
                    categories
                        |> List.map Tree.label
                        |> List.any (\tree -> tree.id == draggingId)

        isDraggingOverAddCategory =
            case Dnd.getDraggingOverElement model.dnd of
                Nothing ->
                    False

                Just (OnTopOf _) ->
                    False

                Just OnRoot ->
                    True

        maybeNewRootCategoryForm =
            case model.newCategoryState of
                NotEditing ->
                    Nothing

                EditingNewCategory { parent, form } ->
                    if Maybe.Extra.isNothing parent then
                        Just form

                    else
                        Nothing
    in
    viewPageContainer translators
        { children =
            [ case categories of
                [] ->
                    div []
                        [ img
                            [ src "/images/not_found.svg"
                            , alt ""
                            , class "w-2/3 md:w-1/3 mx-auto"
                            ]
                            []
                        , h2 [ class "w-full md:w-2/3 mx-auto font-bold text-lg text-center mt-4" ]
                            [ text <| translators.t "shop.categories.empty.description" ]
                        , p [ class "w-full md:w-2/3 mx-auto text-center mt-2 mb-6" ]
                            [ text <| translators.t "shop.categories.empty.description" ]
                        ]

                first :: others ->
                    let
                        rootZipper =
                            Tree.Zipper.fromForest first others
                    in
                    ul [ class "mb-2 grid gap-y-2" ]
                        (List.map
                            (\category ->
                                li []
                                    [ viewCategoryTree translators model rootZipper category
                                    ]
                            )
                            categories
                        )
            , div
                (class "w-full rounded-sm transition-all ease-out"
                    :: classList
                        [ ( "h-0", not isDraggingSomething )
                        , ( "bg-green/30 h-8", isDraggingSomething && not isDraggingRootCategory )
                        , ( "outline-black outline-offset-0", isDraggingSomething && isDraggingOverAddCategory && not isDraggingRootCategory )
                        ]
                    :: (if isDraggingRootCategory then
                            []

                        else
                            Dnd.dropZone OnRoot GotDndMsg
                       )
                )
                []
            , case maybeNewRootCategoryForm of
                Nothing ->
                    button
                        [ class "button button-primary w-full sm:w-auto whitespace-nowrap sm:px-4 sticky left-0 mt-8"
                        , onClick (ClickedAddCategory Nothing)
                        ]
                        [ text <| translators.t "shop.categories.add_root" ]

                Just formModel ->
                    viewNewCategoryForm [] translators formModel
            ]
        , modals =
            [ case model.categoryModalState of
                Closed ->
                    text ""

                Open categoryId formModel ->
                    case Utils.Tree.findInForest (\category -> category.id == categoryId) categories of
                        Nothing ->
                            text ""

                        Just openCategory ->
                            viewCategoryModal translators openCategory formModel
            , case model.askingForDeleteConfirmation of
                Nothing ->
                    text ""

                Just categoryId ->
                    case Utils.Tree.findInForest (\category -> category.id == categoryId) categories of
                        Nothing ->
                            text ""

                        Just openCategory ->
                            viewConfirmDeleteCategoryModal translators openCategory
            , case model.categoryMetadataModalState of
                Closed ->
                    text ""

                Open categoryId formModel ->
                    case Utils.Tree.findInForest (\category -> category.id == categoryId) categories of
                        Nothing ->
                            text ""

                        Just openCategory ->
                            viewCategoryMetadataModal translators community openCategory formModel
            ]
        }
        model


viewCategoryTree :
    Translation.Translators
    -> Model
    -> Tree.Zipper.Zipper Shop.Category.Model
    -> Shop.Category.Tree
    -> Html Msg
viewCategoryTree translators model rootZipper currentTree =
    Tree.restructure
        (\category ->
            rootZipper
                |> Tree.Zipper.findFromRoot (\{ id } -> id == category.id)
                |> Maybe.withDefault (Tree.singleton category |> Tree.Zipper.fromTree)
        )
        (viewCategoryWithChildren translators model)
        currentTree


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
                            isAncestorOf
                                parentId
                                (Tree.Zipper.tree zipper)

        hasActionsMenuOpen =
            case model.actionsDropdown of
                DropdownClosed ->
                    False

                DropdownOpenOnButton actionsDropdown ->
                    isAncestorOf
                        actionsDropdown
                        (Tree.Zipper.tree zipper)

                DropdownOpenOnMouse _ actionsDropdown _ ->
                    isAncestorOf
                        actionsDropdown
                        (Tree.Zipper.tree zipper)

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
                                    (Tree.Zipper.tree
                                        >> isAncestorOf category.id
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

                Just OnRoot ->
                    False
    in
    div
        (class "transition-colors rounded-sm"
            :: classList
                [ ( "bg-gray-300 rounded-sm cursor-wait", EverySet.member category.id model.deleting )
                , ( "!bg-green/30", isValidDropzone && isDraggingSomething )
                , ( "outline-black outline-offset-0", isValidDropzone && isDraggingSomething && isDraggingOver )
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
            , class "parent grand-parent"
            , classList [ ( "pointer-events-none", EverySet.member category.id model.deleting ) ]
            ]
            [ summary
                (class "marker-hidden flex items-center rounded-sm transition-colors cursor-pointer grand-parent focus-ring"
                    :: classList
                        [ ( "!bg-green/20", isParentOfNewCategoryForm )
                        , ( "focus:bg-orange-100/20 parent-hover:bg-orange-100/20", not isParentOfNewCategoryForm && not isDraggingSomething )
                        , ( "bg-orange-100/20", hasActionsMenuOpen )
                        ]
                    :: onClick (ClickedToggleExpandCategory category.id)
                    :: Html.Events.preventDefaultOn "contextmenu"
                        (Json.Decode.map3
                            (\x y button ->
                                if button == 0 then
                                    ( NoOp, False )

                                else
                                    ( OpenedContextMenuForAction { x = x, y = y } category.id
                                    , case model.actionsDropdown of
                                        DropdownOpenOnMouse _ _ _ ->
                                            False

                                        _ ->
                                            True
                                    )
                            )
                            (Json.Decode.field "clientX" Json.Decode.float)
                            (Json.Decode.field "clientY" Json.Decode.float)
                            (Json.Decode.field "button" Json.Decode.int)
                        )
                    :: Dnd.draggable category.id GotDndMsg
                )
                [ div [ class "flex items-center sticky left-0 w-full" ]
                    [ Icons.arrowDown (String.join " " [ "transition-transform", openArrowClass ])
                    , case category.icon of
                        Nothing ->
                            text ""

                        Just icon ->
                            img [ src icon, alt "", class "h-6 w-6 rounded-full mr-2" ] []
                    , span
                        [ class "whitespace-nowrap text-black"
                        , classList [ ( "font-bold", Maybe.Extra.isNothing category.parentId ) ]
                        ]
                        [ text category.name
                        ]
                    ]
                , div
                    [ class "sticky right-0 bg-white rounded-md transition-color"
                    , classList
                        [ ( "bg-transparent", isDraggingSomething )
                        , ( "z-10"
                          , case model.actionsDropdown of
                                DropdownClosed ->
                                    False

                                DropdownOpenOnButton actionsDropdown ->
                                    actionsDropdown == category.id

                                DropdownOpenOnMouse _ actionsDropdown _ ->
                                    actionsDropdown == category.id
                          )
                        ]
                    ]
                    [ viewActions translators
                        { isParentOfNewCategoryForm = isParentOfNewCategoryForm
                        , isDraggingSomething = isDraggingSomething
                        }
                        model
                        zipper
                    ]
                ]
            , div [ class "ml-4 mt-2" ]
                [ ul
                    [ class "grid gap-y-2"
                    , classList [ ( "mb-2", not (List.isEmpty children) ) ]
                    ]
                    (List.map (\child -> li [] [ child ]) children)
                ]
            , div [ class "ml-4 mb-4" ]
                [ viewAddCategory translators [ class "w-full" ] model category
                ]
            ]
        ]


viewAddCategory :
    Translation.Translators
    -> List (Html.Attribute Msg)
    -> Model
    -> Shop.Category.Model
    -> Html Msg
viewAddCategory translators attrs model parentCategory =
    let
        viewAddCategoryButton customAttrs =
            button
                (class "flex items-center px-2 h-8 transition-colors hover:bg-orange-100/20 rounded-sm whitespace-nowrap focus:bg-orange-100/20 focus-ring"
                    :: type_ "button"
                    :: onClick (ClickedAddCategory (Just parentCategory.id))
                    :: customAttrs
                )
                [ span [ class "sticky left-2 flex items-center transition-colors text-orange-500/70 grand-parent-2-hover:text-orange-500/90" ]
                    [ Icons.plus "w-4 h-4 mr-2 fill-current"
                    , text <| translators.tr "shop.categories.add_child" [ ( "parent_name", parentCategory.name ) ]
                    ]
                ]
    in
    case model.newCategoryState of
        NotEditing ->
            viewAddCategoryButton attrs

        EditingNewCategory newCategoryData ->
            if newCategoryData.parent == Just parentCategory.id then
                viewNewCategoryForm attrs translators newCategoryData.form

            else
                viewAddCategoryButton attrs


viewNewCategoryForm : List (Html.Attribute Msg) -> Translation.Translators -> Form.Model NewCategoryFormInput -> Html Msg
viewNewCategoryForm attrs translators formModel =
    Form.view (class "border !border-gray-300 rounded-md p-4" :: attrs)
        translators
        (\submitButton ->
            [ div [ class "flex flex-col sm:flex-row justify-end gap-4 mt-10" ]
                [ button
                    [ class "button button-secondary w-full sm:w-40"
                    , type_ "button"
                    , onClick ClickedCancelAddCategory
                    ]
                    [ text <| translators.t "menu.cancel" ]
                , submitButton [ class "button button-primary w-full sm:w-40" ]
                    [ text <| translators.t "menu.create" ]
                ]
            ]
        )
        (newCategoryForm translators)
        formModel
        { toMsg = GotAddCategoryFormMsg
        , onSubmit = SubmittedAddCategoryForm
        }


viewActions :
    Translation.Translators
    ->
        { isParentOfNewCategoryForm : Bool
        , isDraggingSomething : Bool
        }
    -> Model
    -> Tree.Zipper.Zipper Shop.Category.Model
    -> Html Msg
viewActions translators { isParentOfNewCategoryForm, isDraggingSomething } model zipper =
    let
        category =
            Tree.Zipper.label zipper

        isDropdownOpen =
            case model.actionsDropdown of
                DropdownClosed ->
                    False

                DropdownOpenOnButton actionsDropdown ->
                    actionsDropdown == category.id

                DropdownOpenOnMouse _ actionsDropdown _ ->
                    actionsDropdown == category.id

        isDropdownOpenOnButton =
            case model.actionsDropdown of
                DropdownClosed ->
                    False

                DropdownOpenOnButton actionsDropdown ->
                    actionsDropdown == category.id

                DropdownOpenOnMouse _ _ _ ->
                    False

        hasActionsMenuOpen =
            case model.actionsDropdown of
                DropdownClosed ->
                    False

                DropdownOpenOnButton actionsDropdown ->
                    isAncestorOf
                        actionsDropdown
                        (Tree.Zipper.tree zipper)

                DropdownOpenOnMouse _ actionsDropdown _ ->
                    isAncestorOf
                        actionsDropdown
                        (Tree.Zipper.tree zipper)

        canGoDown =
            not
                (Maybe.Extra.isNothing (Utils.Tree.goDownWithoutChildren zipper)
                    && Maybe.Extra.isNothing (Tree.Zipper.parent zipper)
                )

        canGoUp =
            not
                (Maybe.Extra.isNothing (Utils.Tree.goUpWithoutChildren zipper)
                    && Maybe.Extra.isNothing (Tree.Zipper.parent zipper)
                )

        buttonClassListsFromParent =
            classList
                [ ( "hover:!bg-orange-300/30 active:!bg-orange-300/60 focus:bg-orange-300/30", not isParentOfNewCategoryForm )
                , ( "hover:!bg-green/30 active:!bg-green/60 focus:!bg-green/30", isParentOfNewCategoryForm )
                ]
    in
    div
        [ class "relative flex rounded-sm transition-colors"
        , classList
            [ ( "bg-green/20", isParentOfNewCategoryForm )
            , ( "grand-parent-1-focus:bg-orange-100/20 grand-parent-2-hover:bg-orange-100/20", not isParentOfNewCategoryForm && not isDraggingSomething )
            , ( "bg-orange-100/20", hasActionsMenuOpen )
            ]
        ]
        [ button
            [ class "hidden sm:block h-8 px-2 mr-2 rounded-sm transition-colors hover:!bg-orange-300/30 active:!bg-orange-300/60 focus-ring focus:bg-orange-300/30"
            , Utils.onClickNoBubble (ClickedEditCategory category.id)
            , buttonClassListsFromParent
            , ariaLabel (translators.tr "shop.categories.click_category_to_edit" [ ( "category_name", category.name ) ])
            ]
            [ Icons.edit "max-h-4 w-4"
            ]
        , button
            [ class "h-8 px-2 rounded-sm transition-colors action-opener focus-ring"
            , classList
                [ ( "bg-orange-300/60", isDropdownOpenOnButton && not isParentOfNewCategoryForm )
                , ( "bg-green/30", isDropdownOpenOnButton && isParentOfNewCategoryForm )
                ]
            , buttonClassListsFromParent
            , Utils.onClickNoBubble (ClickedShowActionsDropdown category.id)
            , ariaHasPopup "true"
            , ariaLabel (translators.tr "shop.categories.action_for" [ ( "category_name", category.name ) ])
            ]
            [ Icons.ellipsis "h-4 w-4 pointer-events-none text-gray-800" ]
        , if not isDropdownOpen then
            text ""

          else
            let
                openOnMouseAttrs =
                    case model.actionsDropdown of
                        DropdownOpenOnMouse { x, y } _ { viewport } ->
                            [ class "fixed"
                            , style "top" (String.fromFloat y ++ "px")
                            , if x < viewport.width / 2 then
                                style "left" (String.fromFloat x ++ "px")

                              else
                                style "right" (String.fromFloat (viewport.width - x) ++ "px")
                            ]

                        _ ->
                            []
            in
            menu
                (class "bg-white border border-gray-300 rounded-md p-2 my-0 text-sm shadow-lg animate-fade-in-from-above-sm marker-hidden"
                    :: classList
                        [ ( "absolute right-0 top-full", model.actionsDropdown == DropdownOpenOnButton category.id )
                        ]
                    :: openOnMouseAttrs
                )
                [ li []
                    [ viewAction [ class "sm:hidden" ]
                        { icon = Icons.edit "w-4 ml-1 mr-3"
                        , label = translators.t "shop.categories.actions.edit_main_information"
                        , onClickMsg = ClickedEditCategory category.id
                        }
                    ]
                , li []
                    [ viewAction []
                        { icon = Icons.edit "w-4 ml-1 mr-3"
                        , label = translators.t "shop.categories.actions.edit_sharing_data"
                        , onClickMsg = ClickedOpenMetadataModal category.id
                        }
                    ]
                , if canGoUp then
                    li []
                        [ viewAction []
                            { icon = Icons.arrowDown "rotate-180 w-6 mr-2"
                            , label = translators.t "shop.categories.actions.move_up"
                            , onClickMsg = ClickedMoveUp category.id
                            }
                        ]

                  else
                    text ""
                , if canGoDown then
                    li []
                        [ viewAction []
                            { icon = Icons.arrowDown "w-6 mr-2"
                            , label = translators.t "shop.categories.actions.move_down"
                            , onClickMsg = ClickedMoveDown category.id
                            }
                        ]

                  else
                    text ""
                , li []
                    [ viewAction [ class "text-red hover:bg-red/10 focus:bg-red/10" ]
                        { icon = Icons.trash "w-4 ml-1 mr-3"
                        , label = translators.t "shop.categories.actions.delete"
                        , onClickMsg = ClickedDeleteCategory category.id
                        }
                    ]
                ]
        ]


viewAction :
    List (Html.Attribute Msg)
    ->
        { icon : Svg Msg
        , label : String
        , onClickMsg : Msg
        }
    -> Html Msg
viewAction containerAttrs { icon, label, onClickMsg } =
    button
        (class "flex items-center w-full pl-2 pr-8 py-1 rounded-md transition-colors whitespace-nowrap font-bold class hover:bg-gray-200 focus:bg-gray-200 focus-ring"
            :: Utils.onClickNoBubble onClickMsg
            :: containerAttrs
        )
        [ icon
        , text label
        ]


viewCategoryModal : Translation.Translators -> Shop.Category.Model -> Form.Model UpdateCategoryFormInput -> Html Msg
viewCategoryModal translators category formModel =
    Modal.initWith
        { isVisible = True
        , closeMsg = ClosedCategoryModal
        }
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
                    , disabled (Form.isDisabled formModel)
                    ]
                    [ text <| translators.t "menu.cancel" ]
                , button
                    [ class "button button-primary w-full sm:w-40"
                    , onClick
                        (Form.parse (updateCategoryForm translators category.id)
                            formModel
                            { onError = GotUpdateCategoryFormMsg
                            , onSuccess = SubmittedUpdateCategoryForm
                            }
                        )
                    , disabled (Form.isDisabled formModel)
                    ]
                    [ text <| translators.t "menu.save" ]
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
        |> Modal.withHeaderElement (p [ class "md:px-2 md:mt-2" ] [ text <| translators.t "shop.categories.metadata.title" ])
        |> Modal.withBody
            [ p [ class "mb-6 md:px-2" ]
                [ text <| translators.t "shop.categories.metadata.guidance" ]
            , Form.viewWithoutSubmit [ class "mt-2 md:px-2" ]
                translators
                (\_ -> [])
                (metadataForm translators community category)
                formModel
                { toMsg = GotMetadataFormMsg }
            ]
        |> Modal.withFooter
            [ div [ class "flex flex-col w-full sm:flex-row gap-4 items-center justify-center" ]
                [ button
                    [ class "button button-secondary w-full sm:w-40"
                    , onClick ClosedMetadataModal
                    , disabled (Form.isDisabled formModel)
                    ]
                    [ text <| translators.t "menu.cancel" ]
                , button
                    [ class "button button-primary w-full sm:w-40"
                    , onClick
                        (Form.parse (metadataForm translators community category)
                            formModel
                            { onError = GotMetadataFormMsg
                            , onSuccess = SubmittedMetadataForm
                            }
                        )
                    , disabled (Form.isDisabled formModel)
                    ]
                    [ text <| translators.t "menu.save" ]
                ]
            ]
        |> Modal.withSize Modal.FullScreen
        |> Modal.toHtml


viewConfirmDeleteCategoryModal : Translation.Translators -> Shop.Category.Model -> Html Msg
viewConfirmDeleteCategoryModal translators category =
    Modal.initWith
        { isVisible = True
        , closeMsg = ClosedConfirmDeleteModal
        }
        |> Modal.withHeader (translators.tr "shop.categories.delete.title" [ ( "category_name", category.name ) ])
        |> Modal.withBody
            [ p [] [ text <| translators.t "shop.categories.delete.warning" ]
            , p [] [ text <| translators.t "shop.categories.delete.confirmation" ]
            ]
        |> Modal.withFooter
            [ div [ class "flex flex-col sm:flex-row gap-4" ]
                [ button
                    [ class "button button-secondary w-full sm:w-40"
                    , onClick ClosedConfirmDeleteModal
                    ]
                    [ text <| translators.t "menu.cancel" ]
                , button
                    [ class "button button-danger w-full sm:w-40"
                    , onClick (ConfirmedDeleteCategory category.id)
                    ]
                    [ text <| translators.t "shop.delete" ]
                ]
            ]
        |> Modal.toHtml


viewShareCategoryPreview : Translation.Translators -> Community.Model -> Shop.Category.Model -> MetadataFormInput -> Html msg
viewShareCategoryPreview translators community category values =
    div []
        [ p [ class "label" ]
            [ text <| translators.t "shop.categories.metadata.preview" ]
        , p [ class "mb-4" ]
            [ text <| translators.t "shop.categories.metadata.preview_text" ]
        , div [ class "isolate mr-3 z-10 ml-auto w-full sm:w-3/4 md:w-2/3 border border-gray-300 rounded-large relative before:absolute before:bg-white before:border-t before:border-r before:border-gray-300 before:-top-px before:rounded-br-super before:rounded-tr-sm before:-right-2 before:w-8 before:h-4 before:-z-10" ]
            [ div [ class "bg-white p-1 rounded-large" ]
                [ div [ class "flex w-full bg-gray-100 rounded-large" ]
                    [ case Maybe.Extra.or category.icon category.image of
                        Nothing ->
                            div [ class "bg-gray-200 p-6 rounded-l-large w-1/4 flex-shrink-0 grid place-items-center" ]
                                [ Icons.image "" ]

                        Just previewImage ->
                            img
                                [ src previewImage
                                , alt ""
                                , class "bg-gray-100 rounded-l-large w-1/4 flex-shrink-0 object-contain"
                                ]
                                []
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
                                [ text values.metaDescription
                                , text ". "
                                , text (String.join ", " values.metaKeywords)
                                ]
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
    { icon : Form.File.SingleModel
    , name : String
    , description : Form.RichText.Model
    , image : Form.File.SingleModel
    }


type alias NewCategoryFormOutput =
    { icon : Maybe String
    , name : String
    , slug : Slug
    , description : Markdown
    , image : Maybe String
    }


newCategoryForm : Translation.Translators -> Form.Form msg NewCategoryFormInput NewCategoryFormOutput
newCategoryForm translators =
    Form.succeed
        (\icon { name, slug } description image ->
            { icon = icon
            , name = name
            , slug = slug
            , description = description
            , image = image
            }
        )
        |> Form.with (iconForm translators "new-category-icon")
        |> Form.with (nameAndSlugForm translators { nameFieldId = "new-category-name" })
        |> Form.with
            (Form.RichText.init { label = translators.t "shop.categories.fields.description" }
                |> Form.RichText.withContainerAttrs [ class "mb-10" ]
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
        |> Form.with (imageForm translators "new-category-image")


type alias UpdateCategoryFormInput =
    { icon : Form.File.SingleModel
    , name : String
    , description : Form.RichText.Model
    , image : Form.File.SingleModel
    }


type alias UpdateCategoryFormOutput =
    { id : Shop.Category.Id
    , icon : Maybe String
    , name : String
    , slug : Slug
    , description : Markdown
    , image : Maybe String
    }


iconForm : Translation.Translators -> String -> Form.Form msg { input | icon : Form.File.SingleModel } (Maybe String)
iconForm translators fieldId =
    Form.File.init { id = fieldId }
        |> Form.File.withImageClass "object-cover rounded-full w-20 h-20"
        |> Form.File.withEntryContainerAttributes (\_ -> [ class "mx-auto rounded-full w-20 h-20" ])
        |> Form.File.withAddImagesView (Form.File.defaultAddImagesView [ class "!rounded-full w-20 h-20" ])
        |> Form.File.withAddImagesContainerAttributes [ class "mx-auto w-20 h-20" ]
        |> Form.File.withImageCropperAttributes [ class "rounded-full" ]
        |> Form.File.withContainerAttributes [ class "mb-10" ]
        |> Form.File.withEditIconOverlay
        |> Form.file
            { parser = Ok
            , translators = translators
            , value = .icon
            , update = \newIcon values -> { values | icon = newIcon }
            , externalError = always Nothing
            }
        |> Form.optional


imageForm : Translation.Translators -> String -> Form.Form msg { input | image : Form.File.SingleModel } (Maybe String)
imageForm translators fieldId =
    Form.File.init { id = fieldId }
        |> Form.File.withLabel (translators.t "shop.categories.fields.image")
        |> Form.File.withContainerAttributes [ class "w-full" ]
        |> Form.File.withAddImagesContainerAttributes [ class "!w-full" ]
        |> Form.File.withAddImagesView (Form.File.defaultAddImagesView [ class "!w-full min-h-48" ])
        |> Form.File.withImageClass "rounded-md"
        |> Form.File.withEditIconOverlay
        |> Form.file
            { parser = Ok
            , translators = translators
            , value = .image
            , update = \newImage values -> { values | image = newImage }
            , externalError = always Nothing
            }
        |> Form.optional
        |> Form.withNoOutput
            (Form.arbitrary
                (p [ class "mt-2 text-gray-900" ]
                    [ text <| translators.t "shop.categories.form.image_guidance" ]
                )
            )


updateCategoryForm : Translation.Translators -> Shop.Category.Id -> Form.Form msg UpdateCategoryFormInput UpdateCategoryFormOutput
updateCategoryForm translators id =
    Form.succeed
        (\icon { name, slug } description image ->
            { id = id
            , icon = icon
            , name = name
            , slug = slug
            , description = description
            , image = image
            }
        )
        |> Form.with (iconForm translators "update-category-icon")
        |> Form.with (nameAndSlugForm translators { nameFieldId = "update-category-name" })
        |> Form.with
            (Form.RichText.init { label = translators.t "shop.categories.fields.description" }
                |> Form.RichText.withContainerAttrs [ class "mb-10" ]
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
        |> Form.with (imageForm translators "update-category-image")


type alias MetadataFormInput =
    { metaTitle : String
    , metaDescription : String
    , metaKeyword : String
    , metaKeywords : List String
    , keywordsDnd : Dnd.Model Int Int
    }


type alias MetadataFormOutput =
    { id : Shop.Category.Id
    , metaTitle : String
    , metaDescription : String
    , metaKeywords : List String
    }


metadataForm : Translation.Translators -> Community.Model -> Shop.Category.Model -> Form.Form Msg MetadataFormInput MetadataFormOutput
metadataForm translators community category =
    Form.succeed
        (\metaTitle metaDescription metaKeywords ->
            { id = category.id
            , metaTitle = metaTitle
            , metaDescription = metaDescription
            , metaKeywords = metaKeywords
            }
        )
        |> Form.with
            (Form.Text.init
                { label = translators.t "shop.categories.fields.meta.title"
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
                { label = translators.t "shop.categories.fields.meta.description"
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
        |> Form.with (keywordsForm translators)
        |> Form.withNoOutput
            (Form.introspect
                (\values -> Form.arbitrary (viewShareCategoryPreview translators community category values))
            )


keywordsForm :
    Translation.Translators
    ->
        Form.Form
            Msg
            { input
                | metaKeyword : String
                , metaKeywords : List String
                , keywordsDnd : Dnd.Model Int Int
            }
            (List String)
keywordsForm translators =
    let
        viewKeyword dnd index keyword =
            div
                (class "bg-green/50 border border-green px-3 py-2 rounded-full text-sm flex items-center text-black cursor-move transition-colors"
                    :: classList
                        [ ( "border-dashed border-black !bg-green/80", Dnd.getDraggingOverElement dnd == Just index )
                        , ( "!bg-green/80", Dnd.getDraggingElement dnd == Just index )
                        ]
                    :: Dnd.draggable index GotKeywordDndMsg
                    ++ (if Dnd.getDraggingElement dnd == Just index then
                            []

                        else
                            Dnd.dropZone index GotKeywordDndMsg
                       )
                )
                [ span [ class "uppercase mr-3" ] [ text keyword ]
                , button
                    [ type_ "button"
                    , onClick (ClickedRemoveMetaKeyword index)
                    , class "hover:text-red"
                    ]
                    [ Icons.close "w-3 h-3 fill-current" ]
                ]
    in
    Form.introspect (\values -> Form.succeed values.metaKeywords)
        |> Form.withNoOutput
            (Form.succeed always
                |> Form.withGroup [ class "flex mb-2" ]
                    (Form.Text.init
                        { label = translators.t "shop.categories.fields.meta.keywords"
                        , id = "meta-keyword-input"
                        }
                        |> Form.Text.withContainerAttrs [ class "mb-0 w-full" ]
                        |> Form.textField
                            { parser = Ok
                            , value = .metaKeyword
                            , update = \metaKeyword value -> { value | metaKeyword = metaKeyword }
                            , externalError = always Nothing
                            }
                    )
                    (Form.arbitrary
                        (button
                            [ type_ "button"
                            , class "button button-secondary flex-shrink-0 h-12 ml-4 mt-auto w-auto px-6"
                            , onClick
                                (\values ->
                                    if String.isEmpty values.metaKeyword || List.member values.metaKeyword values.metaKeywords then
                                        values

                                    else
                                        { values
                                            | metaKeyword = ""
                                            , metaKeywords = values.metaKeyword :: values.metaKeywords
                                        }
                                )
                            ]
                            [ text <| translators.t "menu.add" ]
                        )
                    )
            )
        |> Form.withNoOutput
            (Form.introspect
                (\values ->
                    Form.unsafeArbitrary
                        (div [ class "flex flex-wrap mb-10 gap-2" ]
                            (List.indexedMap (viewKeyword values.keywordsDnd)
                                values.metaKeywords
                            )
                        )
                )
            )


nameAndSlugForm : Translation.Translators -> { nameFieldId : String } -> Form.Form msg { values | name : String } { name : String, slug : Slug }
nameAndSlugForm translators { nameFieldId } =
    Form.succeed (\name slug -> { name = name, slug = slug })
        |> Form.with
            (Form.Text.init
                { label = translators.t "shop.categories.fields.name"
                , id = nameFieldId
                }
                |> Form.Text.withContainerAttrs [ class "mb-4" ]
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.stringLongerThan 2
                            >> Form.Validate.custom
                                (\name ->
                                    case Utils.slugify name of
                                        Just _ ->
                                            Ok name

                                        Nothing ->
                                            -- We show the error on the slug field below
                                            Err (\_ -> "")
                                )
                            >> Form.Validate.validate translators
                    , value = .name
                    , update = \newName values -> { values | name = newName }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            ((\{ name } ->
                case Utils.slugify name of
                    Nothing ->
                        Form.arbitrary
                            (div [ class "mb-10" ]
                                [ View.Components.label []
                                    { targetId = nameFieldId, labelText = translators.t "shop.categories.fields.slug" }
                                , if String.isEmpty name then
                                    span [ class "text-gray-400 italic" ]
                                        [ text <| translators.t "shop.categories.form.insert_name" ]

                                  else
                                    div [ class "flex" ]
                                        [ span [ class "form-error" ]
                                            [ text <| translators.t "shop.categories.form.invalid_slug" ]
                                        , View.Components.tooltip
                                            { message = translators.t "shop.categories.form.invalid_slug_tooltip"
                                            , iconClass = "text-red"
                                            , containerClass = "self-end"
                                            }
                                        ]
                                ]
                            )

                    Just slug ->
                        Form.arbitraryWith slug
                            (div [ class "mb-10" ]
                                [ View.Components.label []
                                    { targetId = nameFieldId, labelText = translators.t "shop.categories.fields.slug" }

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
    let
        closeDropdownSubscription =
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
    in
    Sub.batch
        [ Utils.escSubscription PressedEsc
        , case model.actionsDropdown of
            DropdownClosed ->
                Sub.none

            DropdownOpenOnButton _ ->
                closeDropdownSubscription

            DropdownOpenOnMouse _ _ _ ->
                closeDropdownSubscription
        ]



-- UTILS


moveChild :
    { childZipper : Tree.Zipper.Zipper Shop.Category.Model
    , parentId : Shop.Category.Id
    }
    -> Tree.Zipper.Zipper Shop.Category.Model
moveChild { childZipper, parentId } =
    case Tree.Zipper.removeTree childZipper of
        Nothing ->
            childZipper

        Just zipperWithoutChild ->
            case Tree.Zipper.findFromRoot (\{ id } -> id == parentId) zipperWithoutChild of
                Nothing ->
                    childZipper

                Just newParentZipper ->
                    Tree.Zipper.mapTree
                        (\parentTree ->
                            parentTree
                                |> Tree.prependChild (Tree.Zipper.tree childZipper)
                                |> Tree.mapChildren
                                    (\newChildren ->
                                        newChildren
                                            |> List.sortBy (Tree.label >> .name)
                                    )
                        )
                        newParentZipper


moveToRoot :
    { childZipper : Tree.Zipper.Zipper Shop.Category.Model
    }
    -> Tree.Zipper.Zipper Shop.Category.Model
moveToRoot { childZipper } =
    let
        insertNewChild : List Shop.Category.Tree -> List Shop.Category.Tree
        insertNewChild forest =
            Tree.Zipper.tree childZipper
                |> (\newChild -> newChild :: forest)
    in
    childZipper
        |> Tree.Zipper.removeTree
        |> Maybe.andThen
            (\zipperWithoutChild ->
                Utils.Tree.toFlatForest zipperWithoutChild
                    |> insertNewChild
                    |> List.sortBy (Tree.label >> .name)
                    |> Utils.Tree.fromFlatForest
            )
        |> Maybe.withDefault childZipper


isAncestorOf : Shop.Category.Id -> Shop.Category.Tree -> Bool
isAncestorOf childId parentTree =
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
                            isAncestorOf childId childTree
                    )
    in
    isItself || isIndirectAncestor


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

        ClickedEditCategory _ ->
            [ "ClickedEditCategory" ]

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

        OpenedContextMenuForAction _ _ ->
            [ "OpenedContextMenuForAction" ]

        GotViewportForContextMenuForAction _ _ _ ->
            [ "GotViewportForContextMenuForAction" ]

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

        DraggedOverCategoryForAWhile _ ->
            [ "DraggedOverCategoryForAWhile" ]

        CompletedMovingCategory _ r ->
            [ "CompletedMovingCategory", UR.remoteDataToString r ]

        CompletedMovingCategoryToRoot _ r ->
            [ "CompletedMovingCategoryToRoot", UR.remoteDataToString r ]

        ClickedMoveUp _ ->
            [ "ClickedMoveUp" ]

        ClickedMoveDown _ ->
            [ "ClickedMoveDown" ]

        GotKeywordDndMsg subMsg ->
            "GotKeywordDndMsg" :: Dnd.msgToString subMsg

        ClickedRemoveMetaKeyword _ ->
            [ "ClickedRemoveMetaKeyword" ]
