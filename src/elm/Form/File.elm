module Form.File exposing
    ( init, Options
    , Model, initSingle, SingleModel, initMultiple, MultipleModel
    , withLabel, withEditIconOverlay, withAddImagesView, defaultAddImagesView, withImageSiblingElement
    , withContainerAttributes, withDisabled, withEntryContainerAttributes, withImageClass, withImageCropperAttributes, withAddImagesContainerAttributes
    , withFileTypes, FileType(..), withEntryActions, EntryAction(..)
    , withGrayBoxVariant
    , update, view, Msg, ExtMsg(..), msgToString
    , fromSingleModel, toSingleModel, fromMultipleModel, toMultipleModel
    , parser, parserMultiple, getId, isEmpty, FileTypeStatus
    )

{-| Creates a file uploader input that supports single or multiple files, error
messages, cropping images, and automatically uploading files to our servers.
Use it within a `Form.Form`:

    { icon =
        Form.File.initSingle
            { fileUrl = Nothing
            , aspectRatio = Just (1 / 1)
            }
    }

    Form.File.init { id = "icon-uploader" }
        |> Form.File.withImageClass "rounded-full w-20 h-20"
        |> Form.File.withEntryContainerAttributes (\_ -> [ class "rounded-full w-20 h-20" ])
        |> Form.File.withImageCropperAttributes [ class "rounded-full" ]
        |> Form.File.withEditIconOverlay
        |> Form.file
            { parser = Ok
            , translators = translators
            , value = .icon
            , update = \icon input -> { input | icon = icon }
            , externalError = always Nothing
            }


# Initializing

In order to use this input, you need a model and an `Options` type. The `Options`
is always the same, and is constructed with the `init` function.

@docs init, Options

When it comes to the model, you can choose between a model that accepts a single
file or a model that accepts multiple files

@docs Model, initSingle, SingleModel, initMultiple, MultipleModel


# Helpers

These functions use the builder pattern to modify the `Options` type and customize
the input.


## Adding or changing elements

@docs withLabel, withEditIconOverlay, withAddImagesView, defaultAddImagesView, withImageSiblingElement


## Adding attributes

@docs withContainerAttributes, withDisabled, withEntryContainerAttributes, withImageClass, withImageCropperAttributes, withAddImagesContainerAttributes


## Customizing behavior

@docs withFileTypes, FileType, withEntryActions, EntryAction


## Variants

@docs withGrayBoxVariant


# The elm architecture

In order to actually use this component, we use the elm architecture, with view
and update. In order to make code more reusable, these accept `Model` as input,
so you need to map/convert the `SingleModel` or `MultipleModel` to `Model`

@docs update, view, Msg, ExtMsg, msgToString


## Mapping

@docs fromSingleModel, toSingleModel, fromMultipleModel, toMultipleModel


# General helpers

These are some helper functions to parse and check the status of the model

@docs parser, parserMultiple, getId, isEmpty, FileTypeStatus

-}

import Api
import File exposing (File)
import Html exposing (Html, a, button, div, img, input, li, p, text)
import Html.Attributes exposing (accept, alt, class, classList, disabled, for, id, multiple, required, src, type_)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (on, onClick)
import Html.Keyed
import Http
import Icons
import Json.Decode
import List.Extra
import Maybe.Extra
import Translation
import UpdateResult as UR
import View.Components
import View.ImageCropper
import View.Modal as Modal



-- TYPES


{-| The main kind of `Model` for this module. You can't create this directly, but
you can use the `initSingle` or `initMultiple` functions to create something that
can be transformed into a `Model`.
-}
type Model
    = Model
        { entries : Entries
        , isSavingExistingImage : Bool
        , numberOfEntriesLoading : Int
        , aspectRatio : Maybe Float
        , openImageCropperIndex : Maybe Int
        }


{-| A kind of Model that is meant to accept multiple files at once
-}
type MultipleModel
    = MultipleModel
        { entries : List Entry
        , isSavingExistingImage : Bool
        , numberOfEntriesLoading : Int
        , aspectRatio : Maybe Float
        , openImageCropperIndex : Maybe Int
        }


{-| A kind of Model that is meant to accept a single file
-}
type SingleModel
    = SingleModel
        { entry : Maybe Entry
        , isEntryLoading : Bool
        , isSavingExistingImage : Bool
        , aspectRatio : Maybe Float
        , isImageCropperOpen : Bool
        }


{-| This is what we use to represent a file
-}
type alias Entry =
    { fileType : FileTypeStatus
    , url : UrlStatus
    , imageCropper : ImageCropper
    }


type Entries
    = SingleEntry (Maybe Entry)
    | MultipleEntries (List Entry)


{-| All the actions that can be performed on an entry. By default, we have (in
order): `DeleteEntry`, `OpenEntry` (if the file is a PDF), `ReplaceEntry` and `SaveEntry`.
-}
type EntryAction msg
    = DeleteEntry
    | OpenEntry
    | ReplaceEntry
    | SaveEntry
    | CustomAction (Html msg)


type ImageCropper
    = WithoutImageCropper
    | WithImageCropper View.ImageCropper.Model


{-| All kinds of file types that can be accepted by this input. We use this
custom type as an abstraction over MIME types
-}
type FileType
    = Pdf
    | Image


{-| Represents the current status of a file
-}
type FileTypeStatus
    = LoadingFileType
    | LoadedFileType FileType


{-| Represents the status of an entry's url.

The lifecycle goes like this:

1.  Right after the user selects a local file, the status is `Loading`. That
    file is uploaded to our servers
2.  After that file is uploaded to our servers, the status is `Loaded`, with
    the url of the uploaded file
3.  If an error happens when uploading the file, the status is `WithError`
4.  If the file is an image, we can crop it. Whenever the user changes the crop
    size or location, the status becomes `LoadedWithCropped`, which holds the
    original URL and the cropped file.
5.  Once the user clicks "Save", the cropped file is uploaded to our servers.
    While that is happening, the status is `LoadingWithCropped`
6.  When we finish uploading the cropped file, the status becomes `LoadedWithCroppedUploaded`

-}
type UrlStatus
    = Loading File
    | Loaded String
    | LoadedWithCropped { original : String, cropped : File }
    | LoadingWithCropped { original : String, cropped : File }
    | LoadedWithCroppedUploaded
        { original : String
        , cropped : File
        , croppedUrl : String
        }
    | WithError Http.Error


{-| This is what is used to configure the input. It works the same for
`SingleModel` and `MultipleModel`
-}
type Options msg
    = Options
        { id : String
        , label : Maybe String
        , disabled : Bool
        , fileTypes : List FileType
        , entryActions : Int -> List (EntryAction msg)
        , containerAttributes : List (Html.Attribute Never)
        , imageClass : String
        , entryContainerAttributes : Int -> List (Html.Attribute Never)
        , imageSiblingElement : Maybe (Html Never)
        , addImagesView : Maybe (List (Html Never))
        , addImagesContainerAttributes : List (Html.Attribute Never)
        , imageCropperAttributes : List (Html.Attribute Never)
        }



-- INITIALIZING


{-| Initialize an input that accepts a single file at once. If you're creating a
form that's in an editing phase, you might already have an image uploaded, so you
can use that as the `fileUrl`.

In order to crop images, we need to know the target aspect ratio. If you don't
provide it, the image cropper UI won't be shown. A valid aspect ratio is just a
float, but it's usually represented as a fraction, like `4 / 3` or `16 / 9`.

-}
initSingle : { fileUrl : Maybe String, aspectRatio : Maybe Float } -> SingleModel
initSingle { fileUrl, aspectRatio } =
    SingleModel
        { entry =
            fileUrl
                |> Maybe.map
                    (\file ->
                        { fileType = LoadingFileType
                        , url = Loaded file
                        , imageCropper =
                            case aspectRatio of
                                Nothing ->
                                    WithoutImageCropper

                                Just validAspectRatio ->
                                    { aspectRatio = validAspectRatio }
                                        |> View.ImageCropper.init
                                        |> WithImageCropper
                        }
                    )
        , isEntryLoading = False
        , isSavingExistingImage = False
        , aspectRatio = aspectRatio
        , isImageCropperOpen = False
        }


{-| Initialize an input that accepts multiple files at once. If you're creating
a form that's in an editing phase, you might already have some images uploaded,
so you can use those as the `fileUrls`.

In order to crop images, we need to know the target aspect ratio. If you don't
provide it, the image cropper UI won't be shown. A valid aspect ratio is just a
float, but it's usually represented as a fraction, like `4 / 3` or `16 / 9`.

-}
initMultiple : { fileUrls : List String, aspectRatio : Maybe Float } -> MultipleModel
initMultiple { fileUrls, aspectRatio } =
    MultipleModel
        { entries =
            List.map
                (\file ->
                    { fileType = LoadingFileType
                    , url = Loaded file
                    , imageCropper =
                        case aspectRatio of
                            Nothing ->
                                WithoutImageCropper

                            Just validAspectRatio ->
                                { aspectRatio = validAspectRatio }
                                    |> View.ImageCropper.init
                                    |> WithImageCropper
                    }
                )
                fileUrls
        , numberOfEntriesLoading = 0
        , isSavingExistingImage = False
        , aspectRatio = aspectRatio
        , openImageCropperIndex = Nothing
        }



-- CHANGING OPTIONS


{-| Initialize the `Options` type
-}
init : { id : String } -> Options msg
init { id } =
    Options
        { id = id
        , label = Nothing
        , disabled = False
        , fileTypes = [ Image ]
        , entryActions = \_ -> [ DeleteEntry, ReplaceEntry, SaveEntry ]
        , containerAttributes = []
        , imageClass = ""
        , entryContainerAttributes = \_ -> []
        , imageSiblingElement = Nothing
        , addImagesView = Nothing
        , addImagesContainerAttributes = []
        , imageCropperAttributes = []
        }


{-| Add a label to the input. The label should already be translated.
-}
withLabel : String -> Options msg -> Options msg
withLabel label (Options options) =
    Options { options | label = Just label }


{-| Disable the input
-}
withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


{-| -}
withFileTypes : List FileType -> Options msg -> Options msg
withFileTypes fileTypes (Options options) =
    Options { options | fileTypes = fileTypes }


{-| -}
withContainerAttributes : List (Html.Attribute Never) -> Options msg -> Options msg
withContainerAttributes attributes (Options options) =
    Options { options | containerAttributes = options.containerAttributes ++ attributes }


{-| -}
withImageClass : String -> Options msg -> Options msg
withImageClass class_ (Options options) =
    Options { options | imageClass = options.imageClass ++ " " ++ class_ }


{-| -}
withEntryContainerAttributes : (Int -> List (Html.Attribute Never)) -> Options msg -> Options msg
withEntryContainerAttributes attributes (Options options) =
    Options
        { options
            | entryContainerAttributes =
                \index ->
                    options.entryContainerAttributes index ++ attributes index
        }


{-| -}
withImageSiblingElement : Html Never -> Options msg -> Options msg
withImageSiblingElement sibling (Options options) =
    Options { options | imageSiblingElement = Just sibling }


{-| -}
withEditIconOverlay : Options msg -> Options msg
withEditIconOverlay (Options options) =
    Options options
        |> withImageSiblingElement
            (div
                [ class "absolute top-0 bottom-0 left-0 right-0 bg-black/40 grid place-items-center hover:bg-black/50"
                , class options.imageClass
                ]
                [ Icons.edit "text-white"
                ]
            )
        |> withEntryContainerAttributes (\_ -> [ class "relative" ])


{-| -}
withAddImagesView : List (Html Never) -> Options msg -> Options msg
withAddImagesView newView (Options options) =
    Options { options | addImagesView = Just newView }


{-| -}
withAddImagesContainerAttributes : List (Html.Attribute Never) -> Options msg -> Options msg
withAddImagesContainerAttributes attributes (Options options) =
    Options { options | addImagesContainerAttributes = options.addImagesContainerAttributes ++ attributes }


{-| -}
withGrayBoxVariant : Translation.Translators -> Options msg -> Options msg
withGrayBoxVariant translators (Options options) =
    Options options
        |> withEntryContainerAttributes (\_ -> [ class "bg-gray-100 rounded-sm overflow-hidden grid place-items-center" ])
        |> withAddImagesView
            [ div [ class "w-full bg-gray-100 rounded-sm flex flex-col justify-center items-center hover:bg-gray-200" ]
                [ Icons.addPhoto "fill-current text-body-black w-10 mb-2"
                , p [ class "px-4 font-bold text-center" ]
                    [ if not (List.member Pdf options.fileTypes) && List.member Image options.fileTypes then
                        text <| translators.t "upload_image"

                      else
                        text <| translators.t "community.actions.proof.upload_hint"
                    ]
                ]
            ]
        |> withAddImagesContainerAttributes [ class "!w-full rounded-sm" ]
        |> withImageCropperAttributes [ class "rounded-sm" ]


{-| -}
withImageCropperAttributes : List (Html.Attribute Never) -> Options msg -> Options msg
withImageCropperAttributes attributes (Options options) =
    Options { options | imageCropperAttributes = options.imageCropperAttributes ++ attributes }


{-| -}
withEntryActions : (Int -> List (EntryAction msg)) -> Options msg -> Options msg
withEntryActions buildActions (Options options) =
    Options { options | entryActions = buildActions }



-- PARSING


{-| Parse a `SingleModel`. It will only succeed if the image is uploaded, and all
of the errors are already translated
-}
parser : Translation.Translators -> SingleModel -> Result String String
parser ({ t } as translators) (SingleModel model) =
    case model.entry of
        Nothing ->
            Err (t "error.required")

        Just entry ->
            parseUrlStatus translators entry.url


{-| Parse a `Multiple`. It will only succeed if all of the images are uploaded,
and all of the errors are already translated
-}
parserMultiple : Translation.Translators -> MultipleModel -> Result String (List String)
parserMultiple translators (MultipleModel model) =
    List.foldr
        (\entry result ->
            case parseUrlStatus translators entry.url of
                Err err ->
                    Err err

                Ok url ->
                    case result of
                        Err err ->
                            Err err

                        Ok validEntries ->
                            Ok (url :: validEntries)
        )
        (Ok [])
        model.entries


parseUrlStatus : Translation.Translators -> UrlStatus -> Result String String
parseUrlStatus { t } urlStatus =
    case urlStatus of
        Loading _ ->
            Err (t "error.wait_file_upload")

        Loaded url ->
            Ok url

        LoadedWithCropped _ ->
            Err (t "error.wait_file_upload")

        LoadingWithCropped _ ->
            Err (t "error.wait_file_upload")

        LoadedWithCroppedUploaded { croppedUrl } ->
            Ok croppedUrl

        WithError _ ->
            Err (t "error.file_upload")



-- UPDATE


type Msg
    = RequestedUploadFiles (List File)
    | RequestedReplaceFile File
    | CompletedUploadingFile Int (Result Http.Error String)
    | ClickedEntry Int
    | ClickedCloseEntryModal
    | GotImageCropperMsg View.ImageCropper.Msg
    | DiscoveredFileType Int View.Components.PdfViewerFileType
    | ClickedDeleteEntry
    | ClickedSaveEntry


type ExtMsg
    = SetLoadingState Bool


type alias UpdateResult =
    UR.UpdateResult Model Msg ExtMsg


update :
    { shared
        | endpoints : { endpoints | api : String }
        , translators : Translation.Translators
    }
    -> Msg
    -> Model
    -> UpdateResult
update shared msg (Model model) =
    case msg of
        RequestedUploadFiles files ->
            case model.entries of
                SingleEntry _ ->
                    case
                        files
                            |> List.head
                            |> Maybe.andThen (entryFromFile { aspectRatio = model.aspectRatio })
                    of
                        Just newEntry ->
                            { model
                                | entries =
                                    newEntry
                                        |> Just
                                        |> SingleEntry
                                , numberOfEntriesLoading = 1
                            }
                                |> Model
                                |> UR.init
                                |> UR.addCmd (uploadEntry shared 0 newEntry)
                                |> UR.addExt (SetLoadingState True)

                        Nothing ->
                            model
                                |> Model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Input file is not a valid entry"
                                    Nothing
                                    { moduleName = "Form.File", function = "update" }
                                    []

                MultipleEntries entries ->
                    let
                        newEntries : List Entry
                        newEntries =
                            List.filterMap
                                (entryFromFile { aspectRatio = model.aspectRatio })
                                files

                        uploadNewEntries : Cmd Msg
                        uploadNewEntries =
                            newEntries
                                |> List.indexedMap
                                    (\index entry ->
                                        uploadEntry shared
                                            (index + List.length entries)
                                            entry
                                    )
                                |> Cmd.batch
                    in
                    { model
                        | entries = MultipleEntries (entries ++ newEntries)
                        , numberOfEntriesLoading = model.numberOfEntriesLoading + List.length newEntries
                    }
                        |> Model
                        |> UR.init
                        |> UR.addCmd uploadNewEntries
                        |> UR.addExt (SetLoadingState True)

        RequestedReplaceFile file ->
            case entryFromFile { aspectRatio = model.aspectRatio } file of
                Just newEntry ->
                    let
                        ( newEntries, uploadEntryCmd, succeeded ) =
                            case model.entries of
                                SingleEntry _ ->
                                    ( SingleEntry (Just newEntry)
                                    , uploadEntry shared 0 newEntry
                                    , True
                                    )

                                MultipleEntries entries ->
                                    case model.openImageCropperIndex of
                                        Just index ->
                                            ( entries
                                                |> List.Extra.setAt index newEntry
                                                |> MultipleEntries
                                            , uploadEntry shared index newEntry
                                            , True
                                            )

                                        Nothing ->
                                            ( model.entries, Cmd.none, False )

                        addMaybeExtMsg =
                            if succeeded then
                                UR.addExt (SetLoadingState True)

                            else
                                identity
                    in
                    { model
                        | entries = newEntries
                        , numberOfEntriesLoading =
                            if succeeded then
                                model.numberOfEntriesLoading + 1

                            else
                                model.numberOfEntriesLoading
                    }
                        |> Model
                        |> UR.init
                        |> UR.addCmd uploadEntryCmd
                        |> addMaybeExtMsg

                Nothing ->
                    model
                        |> Model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Input file is not a valid entry to replace"
                            Nothing
                            { moduleName = "Form.File", function = "update" }
                            []

        CompletedUploadingFile index result ->
            let
                maybeSetLoadingStateAsFalse =
                    if model.numberOfEntriesLoading == 1 then
                        UR.addExt (SetLoadingState False)

                    else
                        identity
            in
            { model
                | entries =
                    case model.entries of
                        SingleEntry previousEntry ->
                            let
                                newUrlStatus =
                                    updateUrlStatusWithResult result (Maybe.map .url previousEntry)
                            in
                            { fileType = LoadingFileType
                            , url = newUrlStatus
                            , imageCropper =
                                case previousEntry of
                                    Nothing ->
                                        WithoutImageCropper

                                    Just previous ->
                                        imageCropperFromPreviousEntry
                                            { previous = previous.url
                                            , new = newUrlStatus
                                            , previousImageCropper = previous.imageCropper
                                            }
                            }
                                |> Just
                                |> SingleEntry

                        MultipleEntries entries ->
                            entries
                                |> List.Extra.updateAt index
                                    (\previousEntry ->
                                        let
                                            newUrlStatus =
                                                updateUrlStatusWithResult result (Just previousEntry.url)
                                        in
                                        { fileType = LoadingFileType
                                        , url = newUrlStatus
                                        , imageCropper =
                                            imageCropperFromPreviousEntry
                                                { previous = previousEntry.url
                                                , new = newUrlStatus
                                                , previousImageCropper = previousEntry.imageCropper
                                                }
                                        }
                                    )
                                |> MultipleEntries
                , openImageCropperIndex =
                    case model.entries of
                        SingleEntry _ ->
                            if model.isSavingExistingImage then
                                Nothing

                            else
                                Just 0

                        MultipleEntries _ ->
                            model.openImageCropperIndex
                , isSavingExistingImage = False
                , numberOfEntriesLoading = model.numberOfEntriesLoading - 1
            }
                |> Model
                |> UR.init
                |> maybeSetLoadingStateAsFalse

        ClickedEntry index ->
            { model | openImageCropperIndex = Just index }
                |> Model
                |> UR.init

        ClickedCloseEntryModal ->
            { model | openImageCropperIndex = Nothing }
                |> Model
                |> UR.init

        GotImageCropperMsg subMsg ->
            let
                updateEntry : (Model -> Maybe Entry) -> (Model -> Entry -> Model) -> UpdateResult
                updateEntry getEntry updateEntryInModel =
                    case getEntry (Model model) of
                        Nothing ->
                            UR.init (Model model)

                        Just entry ->
                            case entry.imageCropper of
                                WithoutImageCropper ->
                                    UR.init (Model model)

                                WithImageCropper imageCropper ->
                                    View.ImageCropper.update subMsg imageCropper
                                        |> UR.fromChild
                                            (\newImageCropper ->
                                                { entry | imageCropper = WithImageCropper newImageCropper }
                                                    |> updateEntryInModel (Model model)
                                            )
                                            GotImageCropperMsg
                                            (handleExt getEntry updateEntryInModel)
                                            (Model model)

                handleExt : (Model -> Maybe Entry) -> (Model -> Entry -> Model) -> View.ImageCropper.ExtMsg -> UpdateResult -> UpdateResult
                handleExt getEntry updateEntryInModel (View.ImageCropper.CompletedCropping newFile) =
                    UR.mapModel
                        (\m ->
                            case getEntry m of
                                Nothing ->
                                    m

                                Just entry ->
                                    entry
                                        |> setEntryFile newFile
                                        |> updateEntryInModel m
                        )

                setEntryFile : File -> Entry -> Entry
                setEntryFile file entry =
                    { entry
                        | url =
                            case entry.url of
                                Loaded original ->
                                    LoadedWithCropped { original = original, cropped = file }

                                LoadedWithCropped { original } ->
                                    LoadedWithCropped { original = original, cropped = file }

                                LoadingWithCropped { original } ->
                                    LoadingWithCropped { original = original, cropped = file }

                                LoadedWithCroppedUploaded { original } ->
                                    LoadedWithCropped { original = original, cropped = file }

                                WithError err ->
                                    WithError err

                                Loading loading ->
                                    Loading loading
                    }
            in
            updateEntry
                (\(Model m) ->
                    case m.entries of
                        SingleEntry maybeEntry ->
                            maybeEntry

                        MultipleEntries entries ->
                            m.openImageCropperIndex
                                |> Maybe.andThen (\index -> List.Extra.getAt index entries)
                )
                (\(Model m) newEntry ->
                    case m.entries of
                        SingleEntry _ ->
                            Model { m | entries = SingleEntry (Just newEntry) }

                        MultipleEntries entries ->
                            case m.openImageCropperIndex of
                                Nothing ->
                                    Model m

                                Just index ->
                                    { m
                                        | entries =
                                            List.Extra.setAt index newEntry entries
                                                |> MultipleEntries
                                    }
                                        |> Model
                )

        DiscoveredFileType index fileType ->
            let
                updateEntry : Entry -> Entry
                updateEntry entry =
                    { entry
                        | fileType =
                            fileType
                                |> fileTypeFromPdfViewerFileType
                                |> LoadedFileType
                        , imageCropper =
                            case fileTypeFromPdfViewerFileType fileType of
                                Image ->
                                    case model.aspectRatio of
                                        Nothing ->
                                            WithoutImageCropper

                                        Just aspectRatio ->
                                            case entry.imageCropper of
                                                WithoutImageCropper ->
                                                    { aspectRatio = aspectRatio }
                                                        |> View.ImageCropper.init
                                                        |> WithImageCropper

                                                WithImageCropper cropper ->
                                                    WithImageCropper cropper

                                Pdf ->
                                    WithoutImageCropper
                    }
            in
            case model.entries of
                SingleEntry Nothing ->
                    UR.init (Model model)

                SingleEntry (Just entry) ->
                    { model
                        | entries =
                            entry
                                |> updateEntry
                                |> Just
                                |> SingleEntry
                    }
                        |> Model
                        |> UR.init

                MultipleEntries entries ->
                    { model
                        | entries =
                            entries
                                |> List.Extra.updateAt index updateEntry
                                |> MultipleEntries
                    }
                        |> Model
                        |> UR.init

        ClickedDeleteEntry ->
            { model
                | entries =
                    case model.entries of
                        SingleEntry _ ->
                            SingleEntry Nothing

                        MultipleEntries entries ->
                            case model.openImageCropperIndex of
                                Nothing ->
                                    MultipleEntries entries

                                Just index ->
                                    MultipleEntries (List.Extra.removeAt index entries)
                , openImageCropperIndex = Nothing
            }
                |> Model
                |> UR.init

        ClickedSaveEntry ->
            let
                updateEntry : Int -> Entry -> ( Entry, Cmd Msg )
                updateEntry index entry =
                    let
                        fromOriginalAndCropped : { original : String, cropped : File } -> ( Entry, Cmd Msg )
                        fromOriginalAndCropped data =
                            let
                                newEntry =
                                    { entry | url = LoadingWithCropped data }
                            in
                            ( newEntry, uploadEntry shared index newEntry )
                    in
                    case entry.url of
                        LoadedWithCropped data ->
                            fromOriginalAndCropped data

                        LoadingWithCropped data ->
                            fromOriginalAndCropped data

                        LoadedWithCroppedUploaded { original, cropped } ->
                            fromOriginalAndCropped { original = original, cropped = cropped }

                        Loading _ ->
                            ( entry, Cmd.none )

                        Loaded _ ->
                            ( entry, Cmd.none )

                        WithError _ ->
                            ( entry, Cmd.none )
            in
            case model.entries of
                SingleEntry Nothing ->
                    { model
                        | openImageCropperIndex = Nothing
                        , isSavingExistingImage = True
                    }
                        |> Model
                        |> UR.init

                SingleEntry (Just entry) ->
                    let
                        ( newEntry, cmd ) =
                            updateEntry 0 entry
                    in
                    { model
                        | openImageCropperIndex = Nothing
                        , entries =
                            newEntry
                                |> Just
                                |> SingleEntry
                        , isSavingExistingImage = True
                    }
                        |> Model
                        |> UR.init
                        |> UR.addCmd cmd

                MultipleEntries entries ->
                    case model.openImageCropperIndex of
                        Nothing ->
                            model |> Model |> UR.init

                        Just openImageCropperIndex ->
                            case List.Extra.getAt openImageCropperIndex entries of
                                Nothing ->
                                    { model | openImageCropperIndex = Nothing }
                                        |> Model
                                        |> UR.init

                                Just entry ->
                                    let
                                        ( newEntry, cmd ) =
                                            updateEntry openImageCropperIndex entry
                                    in
                                    { model
                                        | openImageCropperIndex = Nothing
                                        , entries =
                                            List.Extra.setAt openImageCropperIndex newEntry entries
                                                |> MultipleEntries
                                    }
                                        |> Model
                                        |> UR.init
                                        |> UR.addCmd cmd



-- VIEW


type alias ViewConfig msg =
    { value : Model
    , error : Html msg
    , hasError : Bool
    , isRequired : Bool
    , translators : Translation.Translators
    }


view : Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
view options viewConfig toMsg =
    let
        (Model model) =
            viewConfig.value
    in
    div []
        [ case model.entries of
            SingleEntry _ ->
                viewSingle (toSingleModel (Model model)) options viewConfig toMsg

            MultipleEntries _ ->
                viewMultiple (toMultipleModel (Model model)) options viewConfig toMsg
        , if viewConfig.hasError then
            viewConfig.error

          else
            text ""
        ]


viewSingle : SingleModel -> Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
viewSingle (SingleModel model) (Options options) viewConfig toMsg =
    case model.entry of
        Nothing ->
            viewAddImages { allowMultiple = False }
                (Options options)
                viewConfig
                toMsg

        Just entry ->
            div (class "flex flex-col" :: fromNeverAttributes options.containerAttributes)
                [ case options.label of
                    Nothing ->
                        text ""

                    Just label ->
                        button
                            [ class "label w-max focus-ring"
                            , onClick (ClickedEntry 0 |> toMsg)
                            , disabled options.disabled
                            , type_ "button"
                            ]
                            [ text label ]
                , viewEntry viewConfig.translators
                    (Options options)
                    0
                    entry
                    |> Html.map toMsg
                , viewEntryModal (Options options)
                    viewConfig
                    { isVisible = model.isImageCropperOpen, index = 0 }
                    entry
                    toMsg
                ]


viewMultiple : MultipleModel -> Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
viewMultiple (MultipleModel model) (Options options) viewConfig toMsg =
    let
        maybeOpenEntry : Maybe Entry
        maybeOpenEntry =
            model.openImageCropperIndex
                |> Maybe.andThen (\index -> List.Extra.getAt index model.entries)
    in
    div (fromNeverAttributes options.containerAttributes)
        [ case options.label of
            Nothing ->
                text ""

            Just label ->
                View.Components.label []
                    { targetId = options.id
                    , labelText = label
                    }
        , Html.Keyed.ul
            [ class "flex flex-wrap gap-4" ]
            (List.indexedMap
                (\index entry ->
                    ( "entry-" ++ String.fromInt index
                    , li []
                        [ viewEntry viewConfig.translators
                            (Options options)
                            index
                            entry
                            |> Html.map toMsg
                        ]
                    )
                )
                model.entries
                ++ [ ( "add-images-input"
                     , viewAddImages
                        { allowMultiple = True }
                        (Options options)
                        viewConfig
                        toMsg
                     )
                   ]
            )
        , case maybeOpenEntry of
            Nothing ->
                text ""

            Just entry ->
                viewEntryModal (Options options)
                    viewConfig
                    { isVisible = True
                    , index =
                        model.openImageCropperIndex
                            |> Maybe.withDefault 0
                    }
                    entry
                    toMsg
        ]


{-| This is what we use when there isn't an image uploaded yet
-}
viewAddImages : { allowMultiple : Bool } -> Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
viewAddImages { allowMultiple } (Options options) viewConfig toMsg =
    div
        (if allowMultiple then
            []

         else
            class "flex flex-col" :: fromNeverAttributes options.containerAttributes
        )
        [ if allowMultiple then
            text ""

          else
            case options.label of
                Nothing ->
                    text ""

                Just label ->
                    button
                        [ class "label w-max focus-ring"
                        , onClick (ClickedEntry 0)
                        , disabled options.disabled
                        , type_ "button"
                        ]
                        [ text label ]
        , viewInput (Options options)
            viewConfig
            { allowMultiple = allowMultiple }
        , Html.label
            (for options.id
                :: class "cursor-pointer flex file-decoration w-max"
                :: fromNeverAttributes options.addImagesContainerAttributes
            )
            (options.addImagesView
                |> Maybe.withDefault (defaultAddImagesView [])
                |> List.map (Html.map Basics.never)
            )
        ]
        |> Html.map toMsg


defaultAddImagesView : List (Html.Attribute Never) -> List (Html Never)
defaultAddImagesView attrs =
    [ div
        (class "p-2 bg-gray-100 flex items-center justify-center w-24 h-24 rounded hover:bg-gray-200"
            :: attrs
        )
        [ Icons.plus "text-orange-300 fill-current"
        ]
    ]


viewInput : Options msg -> ViewConfig msg -> { allowMultiple : Bool } -> Html Msg
viewInput (Options options) viewConfig { allowMultiple } =
    input
        [ type_ "file"
        , id options.id
        , class "sr-only form-file"
        , on "change"
            (Json.Decode.at [ "target", "files" ]
                (Json.Decode.list File.decoder)
                |> Json.Decode.map RequestedUploadFiles
            )
        , multiple allowMultiple
        , acceptFileTypes options.fileTypes
        , disabled options.disabled
        , required viewConfig.isRequired
        ]
        []


viewReplaceImageInput : Options msg -> { isDisabled : Bool, index : Int } -> Html Msg
viewReplaceImageInput (Options options) { isDisabled, index } =
    input
        [ type_ "file"
        , id (replaceInputId (Options options) index)
        , class "sr-only form-file"
        , on "change"
            (File.decoder
                |> Json.Decode.index 0
                |> Json.Decode.at [ "target", "files" ]
                |> Json.Decode.map RequestedReplaceFile
            )
        , multiple False
        , acceptFileTypes options.fileTypes
        , disabled (isDisabled || options.disabled)
        ]
        []


replaceInputId : Options msg -> Int -> String
replaceInputId (Options options) index =
    options.id ++ "-replace-image-" ++ String.fromInt index


viewEntry : Translation.Translators -> Options msg -> Int -> Entry -> Html Msg
viewEntry translators (Options options) index entry =
    let
        buttonAriaLabel =
            case entry.fileType of
                LoadedFileType Image ->
                    case entry.imageCropper of
                        WithImageCropper _ ->
                            translators.t "form.file.edit_image"

                        WithoutImageCropper ->
                            translators.t "form.file.edit_file"

                _ ->
                    translators.t "form.file.edit_file"

        viewWithUrl : String -> Html Msg
        viewWithUrl url =
            button
                (onClick (ClickedEntry index)
                    :: type_ "button"
                    :: class "hover:opacity-60 focus-ring"
                    :: ariaLabel buttonAriaLabel
                    :: fromNeverAttributes (options.entryContainerAttributes index)
                )
                [ case entry.fileType of
                    LoadedFileType Image ->
                        img [ src url, alt "", class options.imageClass ] []

                    LoadedFileType Pdf ->
                        View.Components.pdfViewer []
                            { url = url
                            , childClass = options.imageClass
                            , maybeTranslators = Just translators
                            , onFileTypeDiscovered = Nothing
                            }

                    LoadingFileType ->
                        View.Components.pdfViewer []
                            { url = url
                            , childClass = options.imageClass
                            , maybeTranslators = Just translators
                            , onFileTypeDiscovered = Just (DiscoveredFileType index)
                            }
                , options.imageSiblingElement
                    |> Maybe.withDefault (text "")
                    |> Html.map Basics.never
                ]
    in
    case entry.url of
        Loading _ ->
            div
                (class "p-4 bg-gray-100 grid place-items-center"
                    :: fromNeverAttributes (options.entryContainerAttributes index)
                )
                [ View.Components.loadingLogoWithNoText "max-w-27"
                ]

        Loaded url ->
            viewWithUrl url

        LoadedWithCropped { original } ->
            viewWithUrl original

        LoadingWithCropped _ ->
            div
                (class "p-4 bg-gray-100 grid place-items-center"
                    :: fromNeverAttributes (options.entryContainerAttributes index)
                )
                [ View.Components.loadingLogoWithNoText "w-full h-full max-w-27"
                ]

        LoadedWithCroppedUploaded { croppedUrl } ->
            viewWithUrl croppedUrl

        WithError _ ->
            button
                (onClick (ClickedEntry index)
                    :: type_ "button"
                    :: class "grid place-items-center bg-gray-100"
                    :: fromNeverAttributes (options.entryContainerAttributes index)
                )
                [ Icons.exclamation ("text-red w-1/2 h-1/2 " ++ options.imageClass)
                ]


viewEntryModal : Options msg -> ViewConfig msg -> { isVisible : Bool, index : Int } -> Entry -> (Msg -> msg) -> Html msg
viewEntryModal (Options options) viewConfig { isVisible, index } entry toMsg =
    let
        { translators } =
            viewConfig

        viewWithUrl : String -> Html msg
        viewWithUrl url =
            case entry.fileType of
                LoadedFileType Image ->
                    case entry.imageCropper of
                        WithoutImageCropper ->
                            img [ src url, alt "", class "mx-auto w-full md:w-auto max-h-[35vh] lg:max-h-[60vh]" ] []

                        WithImageCropper imageCropper ->
                            View.ImageCropper.view imageCropper
                                { imageUrl = url
                                , cropperAttributes = options.imageCropperAttributes
                                }
                                |> Html.map (GotImageCropperMsg >> toMsg)

                LoadedFileType Pdf ->
                    View.Components.pdfViewer []
                        { url = url
                        , childClass = "mx-auto w-full md:w-auto max-h-full max-h-[35vh] lg:max-h-[60vh]"
                        , maybeTranslators = Just translators
                        , onFileTypeDiscovered = Nothing
                        }

                LoadingFileType ->
                    View.Components.pdfViewer []
                        { url = url
                        , childClass = "mx-auto w-full md:w-auto max-h-full max-h-[35vh] lg:max-h-[60vh]"
                        , maybeTranslators = Just translators
                        , onFileTypeDiscovered = Nothing
                        }

        addOpenEntryAction : List (EntryAction msg) -> List (EntryAction msg)
        addOpenEntryAction actions =
            List.Extra.takeWhile (\action -> action /= ReplaceEntry) actions
                ++ (OpenEntry
                        :: List.Extra.dropWhile (\action -> action /= ReplaceEntry) actions
                   )

        actionsWithOpenAction =
            if List.member OpenEntry (options.entryActions index) then
                options.entryActions index

            else if entry.fileType == LoadedFileType Pdf then
                options.entryActions index
                    |> addOpenEntryAction

            else
                options.entryActions index

        ( header, bodyText ) =
            case entry.fileType of
                LoadedFileType Image ->
                    case entry.imageCropper of
                        WithImageCropper _ ->
                            ( translators.t "form.file.edit_image"
                            , translators.t "form.file.body_image"
                            )

                        WithoutImageCropper ->
                            ( translators.t "form.file.edit_file"
                            , translators.t "form.file.body_file"
                            )

                LoadedFileType Pdf ->
                    ( translators.t "form.file.edit_file"
                    , translators.t "form.file.body_file"
                    )

                LoadingFileType ->
                    ( ""
                    , ""
                    )
    in
    Modal.initWith
        { closeMsg = toMsg ClickedCloseEntryModal
        , isVisible = isVisible
        }
        |> Modal.withHeader header
        |> Modal.withBody
            [ p [] [ text bodyText ]
            , div [ class "flex items-center justify-center mt-4" ]
                [ case entry.url of
                    Loading _ ->
                        View.Components.loadingLogoAnimated translators ""

                    Loaded url ->
                        viewWithUrl url

                    LoadedWithCropped { original } ->
                        viewWithUrl original

                    LoadingWithCropped { original } ->
                        viewWithUrl original

                    LoadedWithCroppedUploaded { original } ->
                        viewWithUrl original

                    WithError _ ->
                        div [ class "w-full" ]
                            [ Icons.exclamation "mb-4 mx-auto bg-gray-100 p-4 w-1/4 text-red w-full h-full rounded-sm"
                            , p [ class "form-error text-center" ]
                                [ text <| translators.t "error.file_upload" ]
                            ]
                ]
            ]
        |> Modal.withFooter
            [ div [ class "flex flex-wrap items-center justify-center md:flex gap-6 w-full px-10" ]
                (List.foldr
                    (\action actions ->
                        let
                            viewAction =
                                case action of
                                    DeleteEntry ->
                                        deleteEntryAction translators entry
                                            |> Html.map toMsg

                                    OpenEntry ->
                                        openEntryAction translators entry
                                            |> Html.map toMsg

                                    ReplaceEntry ->
                                        replaceEntryAction translators (Options options) index entry
                                            |> Html.map toMsg

                                    SaveEntry ->
                                        saveEntryAction translators entry
                                            |> Html.map toMsg

                                    CustomAction customView ->
                                        customView
                        in
                        viewAction :: actions
                    )
                    []
                    actionsWithOpenAction
                )
            ]
        |> Modal.withSize Modal.FullScreen
        |> Modal.toHtml


deleteEntryAction : Translation.Translators -> Entry -> Html Msg
deleteEntryAction { t } entry =
    button
        [ class "text-center w-full md:w-auto uppercase text-orange-300 font-bold md:mr-auto focus-ring rounded-sm hover:opacity-60"
        , onClick ClickedDeleteEntry
        , disabled (isEntryLoading entry)
        , type_ "button"
        ]
        [ text <| t "form.file.delete" ]


openEntryAction : Translation.Translators -> Entry -> Html Msg
openEntryAction { t } entry =
    let
        maybeUrl =
            case entry.url of
                Loaded url ->
                    Just url

                LoadedWithCropped { original } ->
                    Just original

                LoadedWithCroppedUploaded { croppedUrl } ->
                    Just croppedUrl

                _ ->
                    Nothing
    in
    case maybeUrl of
        Nothing ->
            text ""

        Just url ->
            a
                [ class "flex items-center justify-center gap-2 w-full md:w-auto uppercase text-orange-300 font-bold focus-ring rounded-sm hover:opacity-60"
                , Html.Attributes.target "_blank"
                , Html.Attributes.href url
                ]
                [ Icons.externalLink "w-4 h-4 fill-current"
                , text <| t "form.file.open"
                ]


replaceEntryAction : Translation.Translators -> Options msg -> Int -> Entry -> Html Msg
replaceEntryAction { t } options index entry =
    let
        (Options unwrappedOptions) =
            options
    in
    div [ class "w-full md:w-40 flex-shrink-0" ]
        [ viewReplaceImageInput options { index = index, isDisabled = isEntryLoading entry }
        , Html.label
            [ for (replaceInputId options index)
            , class "cursor-pointer file-decoration button button-secondary w-full"
            , classList
                [ ( "button-disabled", unwrappedOptions.disabled || isEntryLoading entry )

                -- Cropping the image is pretty fast, so we don't want the button to flicker
                -- At the same time, we don't want the user to be able to do stuff while cropping
                , ( "!bg-white !text-orange-300", isImageCropperLoading entry )
                ]
            ]
            [ Icons.camera "w-4 mr-1"
            , text <| t "form.file.change"
            ]
        ]


saveEntryAction : Translation.Translators -> Entry -> Html Msg
saveEntryAction { t } entry =
    button
        [ class "button button-primary w-full flex-shrink-0 md:w-40"

        -- Cropping the image is pretty fast, so we don't want the button to flicker
        -- At the same time, we don't want the user to be able to submit while cropping
        , classList [ ( "!bg-orange-300 !text-white cursor-wait", isImageCropperLoading entry ) ]
        , onClick ClickedSaveEntry
        , disabled (isEntryLoading entry)
        , type_ "button"
        ]
        [ text <| t "form.file.save" ]



-- HELPER FUNCTIONS


getId : Options msg -> String
getId (Options options) =
    options.id


isEmpty : Model -> Bool
isEmpty (Model model) =
    case model.entries of
        SingleEntry Nothing ->
            True

        SingleEntry (Just entry) ->
            isUrlStatusEmpty entry.url

        MultipleEntries entries ->
            List.isEmpty entries
                || List.any (\entry -> not (isUrlStatusEmpty entry.url)) entries


isUrlStatusEmpty : UrlStatus -> Bool
isUrlStatusEmpty urlStatus =
    case urlStatus of
        Loading _ ->
            True

        Loaded _ ->
            False

        LoadedWithCropped _ ->
            True

        LoadingWithCropped _ ->
            True

        LoadedWithCroppedUploaded _ ->
            False

        WithError _ ->
            True


isEntryLoading : Entry -> Bool
isEntryLoading entry =
    isUrlStatusLoading entry.url || isImageCropperLoading entry


isUrlStatusLoading : UrlStatus -> Bool
isUrlStatusLoading urlStatus =
    case urlStatus of
        Loading _ ->
            True

        Loaded _ ->
            False

        LoadedWithCropped _ ->
            False

        LoadingWithCropped _ ->
            True

        LoadedWithCroppedUploaded _ ->
            False

        WithError _ ->
            False


isImageCropperLoading : Entry -> Bool
isImageCropperLoading entry =
    case entry.imageCropper of
        WithoutImageCropper ->
            False

        WithImageCropper imageCropper ->
            imageCropper.isRequestingCroppedImage


fileTypeToString : FileType -> String
fileTypeToString fileType =
    case fileType of
        Image ->
            "image/*"

        Pdf ->
            "application/pdf"


fileTypeFromString : String -> Maybe FileType
fileTypeFromString fileType =
    case fileType of
        "application/pdf" ->
            Just Pdf

        _ ->
            if String.startsWith "image/" fileType then
                Just Image

            else
                Nothing


acceptFileTypes : List FileType -> Html.Attribute msg
acceptFileTypes fileTypes =
    fileTypes
        |> List.map fileTypeToString
        |> String.join ","
        |> accept


fileTypeFromPdfViewerFileType : View.Components.PdfViewerFileType -> FileType
fileTypeFromPdfViewerFileType fileType =
    case fileType of
        View.Components.Pdf ->
            Pdf

        View.Components.Image ->
            Image


entryFromFile : { aspectRatio : Maybe Float } -> File -> Maybe Entry
entryFromFile { aspectRatio } file =
    file
        |> File.mime
        |> fileTypeFromString
        |> Maybe.map
            (\fileType ->
                { fileType = LoadedFileType fileType
                , url = Loading file
                , imageCropper =
                    case aspectRatio of
                        Nothing ->
                            WithoutImageCropper

                        Just validAspectRatio ->
                            case fileType of
                                Pdf ->
                                    WithoutImageCropper

                                Image ->
                                    { aspectRatio = validAspectRatio }
                                        |> View.ImageCropper.init
                                        |> WithImageCropper
                }
            )


uploadEntry :
    { shared | endpoints : { endpoints | api : String } }
    -> Int
    -> Entry
    -> Cmd Msg
uploadEntry shared index entry =
    case entry.url of
        Loading file ->
            Api.uploadImage shared file (CompletedUploadingFile index)

        Loaded _ ->
            Cmd.none

        LoadedWithCropped { cropped } ->
            Api.uploadImage shared cropped (CompletedUploadingFile index)

        LoadingWithCropped { cropped } ->
            Api.uploadImage shared cropped (CompletedUploadingFile index)

        LoadedWithCroppedUploaded _ ->
            Cmd.none

        WithError _ ->
            Cmd.none


updateUrlStatusWithResult : Result Http.Error String -> Maybe UrlStatus -> UrlStatus
updateUrlStatusWithResult result maybePreviousStatus =
    case maybePreviousStatus of
        Nothing ->
            urlStatusFromResult result

        Just previousStatus ->
            case result of
                Err err ->
                    WithError err

                Ok url ->
                    case previousStatus of
                        Loading _ ->
                            Loaded url

                        Loaded _ ->
                            Loaded url

                        LoadedWithCropped { original, cropped } ->
                            LoadedWithCroppedUploaded { original = original, cropped = cropped, croppedUrl = url }

                        LoadingWithCropped { original, cropped } ->
                            LoadedWithCroppedUploaded { original = original, cropped = cropped, croppedUrl = url }

                        LoadedWithCroppedUploaded { original, cropped } ->
                            LoadedWithCroppedUploaded { original = original, cropped = cropped, croppedUrl = url }

                        WithError _ ->
                            Loaded url


urlStatusFromResult : Result Http.Error String -> UrlStatus
urlStatusFromResult result =
    case result of
        Ok url ->
            Loaded url

        Err err ->
            WithError err


originalUrlFromUrlStatus : UrlStatus -> Maybe String
originalUrlFromUrlStatus urlStatus =
    case urlStatus of
        Loading _ ->
            Nothing

        Loaded url ->
            Just url

        LoadedWithCropped { original } ->
            Just original

        LoadingWithCropped { original } ->
            Just original

        LoadedWithCroppedUploaded { original } ->
            Just original

        WithError _ ->
            Nothing


imageCropperFromPreviousEntry : { previous : UrlStatus, new : UrlStatus, previousImageCropper : ImageCropper } -> ImageCropper
imageCropperFromPreviousEntry { previous, new, previousImageCropper } =
    case ( originalUrlFromUrlStatus previous, originalUrlFromUrlStatus new ) of
        ( Just previousUrl, Just newUrl ) ->
            if previousUrl == newUrl then
                previousImageCropper

            else
                WithoutImageCropper

        _ ->
            WithoutImageCropper


fromMultipleModel : MultipleModel -> Model
fromMultipleModel (MultipleModel model) =
    Model
        { entries = MultipleEntries model.entries
        , numberOfEntriesLoading = model.numberOfEntriesLoading
        , isSavingExistingImage = model.isSavingExistingImage
        , aspectRatio = model.aspectRatio
        , openImageCropperIndex = model.openImageCropperIndex
        }


toMultipleModel : Model -> MultipleModel
toMultipleModel (Model model) =
    MultipleModel
        { entries =
            case model.entries of
                SingleEntry (Just entry) ->
                    [ entry ]

                SingleEntry Nothing ->
                    []

                MultipleEntries entries ->
                    entries
        , numberOfEntriesLoading = model.numberOfEntriesLoading
        , isSavingExistingImage = model.isSavingExistingImage
        , aspectRatio = model.aspectRatio
        , openImageCropperIndex = model.openImageCropperIndex
        }


fromSingleModel : SingleModel -> Model
fromSingleModel (SingleModel model) =
    Model
        { entries = SingleEntry model.entry
        , numberOfEntriesLoading =
            if model.isEntryLoading then
                1

            else
                0
        , isSavingExistingImage = model.isSavingExistingImage
        , aspectRatio = model.aspectRatio
        , openImageCropperIndex =
            if model.isImageCropperOpen then
                Just 0

            else
                Nothing
        }


toSingleModel : Model -> SingleModel
toSingleModel (Model model) =
    SingleModel
        { entry =
            case model.entries of
                SingleEntry maybeEntry ->
                    maybeEntry

                MultipleEntries entries ->
                    List.head entries
        , isEntryLoading = model.numberOfEntriesLoading > 0
        , isSavingExistingImage = model.isSavingExistingImage
        , aspectRatio = model.aspectRatio
        , isImageCropperOpen = Maybe.Extra.isJust model.openImageCropperIndex
        }


fromNeverAttributes : List (Html.Attribute Never) -> List (Html.Attribute msg)
fromNeverAttributes =
    List.map (Html.Attributes.map Basics.never)


msgToString : Msg -> List String
msgToString msg =
    case msg of
        RequestedUploadFiles _ ->
            [ "RequestedUploadFiles" ]

        RequestedReplaceFile _ ->
            [ "RequestedReplaceFile" ]

        CompletedUploadingFile _ _ ->
            [ "CompletedUploadingFile" ]

        ClickedEntry _ ->
            [ "ClickedEntry" ]

        ClickedCloseEntryModal ->
            [ "ClickedCloseEntryModal" ]

        GotImageCropperMsg subMsg ->
            "GotImageCropperMsg" :: View.ImageCropper.msgToString subMsg

        DiscoveredFileType _ _ ->
            [ "DiscoveredFileType" ]

        ClickedDeleteEntry ->
            [ "ClickedDeleteEntry" ]

        ClickedSaveEntry ->
            [ "ClickedSaveEntry" ]
