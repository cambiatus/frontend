module Form.File2 exposing
    ( FileType(..)
    , FileTypeStatus
    , Model
    , Msg
    , MultipleModel
    , Options
    , SingleModel
    , fromMultipleModel
    , fromSingleModel
    , getId
    , init
    , initMultiple
    , initSingle
    , isEmpty
    , msgToString
    , parser
    , parserMultiple
    , toMultipleModel
    , toSingleModel
    , update
    , view
    , withAddImagesView
    , withContainerClass
    , withDisabled
    , withEditIconOverlay
    , withEntryContainerClass
    , withFileTypes
    , withImageClass
    , withImageCropperClass
    , withImageSiblingElement
    , withLabel
    )

import Api
import File exposing (File)
import Html exposing (Html, button, div, img, input, li, p, text)
import Html.Attributes exposing (accept, alt, class, disabled, for, id, multiple, required, src, type_)
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
import View.Feedback as Feedback
import View.ImageCropper
import View.Modal as Modal



-- TYPES


type Model
    = Model
        { entries : Entries
        , aspectRatio : Maybe Float
        , openImageCropperIndex : Maybe Int
        }


type MultipleModel
    = MultipleModel
        { entries : List Entry
        , aspectRatio : Maybe Float
        , openImageCropperIndex : Maybe Int
        }


type SingleModel
    = SingleModel
        { entry : Maybe Entry
        , aspectRatio : Maybe Float
        , isImageCropperOpen : Bool
        }


type alias Entry =
    { fileType : FileTypeStatus
    , url : UrlStatus
    , imageCropper : ImageCropper
    }


type Entries
    = SingleEntry (Maybe Entry)
    | MultipleEntries (List Entry)


type ImageCropper
    = WithoutImageCropper
    | WithImageCropper View.ImageCropper.Model


type FileType
    = Pdf
    | Image


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


type Options msg
    = Options
        { id : String
        , label : Maybe String
        , disabled : Bool
        , fileTypes : List FileType
        , containerClass : String
        , imageClass : String
        , entryContainerClass : String
        , imageSiblingElement : Maybe (Html Never)
        , addImagesView : Maybe (List (Html Never))
        , imageCropperClass : String
        }



-- INITIALIZING


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
        , aspectRatio = aspectRatio
        , isImageCropperOpen = False
        }


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
        , aspectRatio = aspectRatio
        , openImageCropperIndex = Nothing
        }



-- CHANGING OPTIONS


init : { id : String } -> Options msg
init { id } =
    Options
        { id = id
        , label = Nothing
        , disabled = False
        , fileTypes = [ Image ]
        , containerClass = ""
        , imageClass = ""
        , entryContainerClass = ""
        , imageSiblingElement = Nothing
        , addImagesView = Nothing
        , imageCropperClass = ""
        }


withLabel : String -> Options msg -> Options msg
withLabel label (Options options) =
    Options { options | label = Just label }


withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


withFileTypes : List FileType -> Options msg -> Options msg
withFileTypes fileTypes (Options options) =
    Options { options | fileTypes = fileTypes }


withContainerClass : String -> Options msg -> Options msg
withContainerClass class_ (Options options) =
    Options { options | containerClass = options.containerClass ++ " " ++ class_ }


withImageClass : String -> Options msg -> Options msg
withImageClass class_ (Options options) =
    Options { options | imageClass = options.imageClass ++ " " ++ class_ }


withEntryContainerClass : String -> Options msg -> Options msg
withEntryContainerClass class_ (Options options) =
    Options { options | entryContainerClass = options.entryContainerClass ++ " " ++ class_ }


withImageSiblingElement : Html Never -> Options msg -> Options msg
withImageSiblingElement sibling (Options options) =
    Options { options | imageSiblingElement = Just sibling }


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
        |> withEntryContainerClass "relative"


withAddImagesView : List (Html Never) -> Options msg -> Options msg
withAddImagesView newView (Options options) =
    Options { options | addImagesView = Just newView }


withImageCropperClass : String -> Options msg -> Options msg
withImageCropperClass cropperClass (Options options) =
    Options { options | imageCropperClass = options.imageCropperClass ++ " " ++ cropperClass }



-- PARSING


parser : Translation.Translators -> SingleModel -> Result String String
parser ({ t } as translators) (SingleModel model) =
    case model.entry of
        Nothing ->
            Err (t "error.required")

        Just entry ->
            parseUrlStatus translators entry.url


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
    | RequestedReplaceFile Int File
    | CompletedUploadingFile Int (Result Http.Error String)
    | ClickedEntry Int
    | ClickedCloseEntryModal
    | GotImageCropperMsg View.ImageCropper.Msg
    | DiscoveredFileType Int View.Components.PdfViewerFileType
    | ClickedDeleteEntry
    | ClickedSaveEntry


type alias UpdateResult =
    UR.UpdateResult Model Msg Feedback.Model


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
                            }
                                |> Model
                                |> UR.init
                                |> UR.addCmd (uploadEntry shared 0 newEntry)

                        Nothing ->
                            model
                                |> Model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Input file is not a valid entry"
                                    Nothing
                                    { moduleName = "Form.File2", function = "update" }
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
                    { model | entries = MultipleEntries (entries ++ newEntries) }
                        |> Model
                        |> UR.init
                        |> UR.addCmd uploadNewEntries

        RequestedReplaceFile index file ->
            case entryFromFile { aspectRatio = model.aspectRatio } file of
                Just newEntry ->
                    let
                        newEntries =
                            case model.entries of
                                SingleEntry _ ->
                                    SingleEntry (Just newEntry)

                                MultipleEntries entries ->
                                    entries
                                        |> List.Extra.setAt index newEntry
                                        |> MultipleEntries
                    in
                    { model | entries = newEntries }
                        |> Model
                        |> UR.init
                        |> UR.addCmd (uploadEntry shared index newEntry)

                Nothing ->
                    model
                        |> Model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Input file is not a valid entry to replace"
                            Nothing
                            { moduleName = "Form.File2", function = "update" }
                            []

        CompletedUploadingFile index result ->
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
                            Just 0

                        MultipleEntries _ ->
                            model.openImageCropperIndex
            }
                |> Model
                |> UR.init

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
                    { model | openImageCropperIndex = Nothing }
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
    case model.entries of
        SingleEntry _ ->
            viewSingle (toSingleModel (Model model)) options viewConfig toMsg

        MultipleEntries _ ->
            viewMultiple (toMultipleModel (Model model)) options viewConfig toMsg


viewSingle : SingleModel -> Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
viewSingle (SingleModel model) (Options options) viewConfig toMsg =
    case model.entry of
        Nothing ->
            viewAddImages { allowMultiple = False }
                (Options options)
                viewConfig
                toMsg

        Just entry ->
            div [ class options.containerClass ]
                -- TODO - ariaLive?
                [ case options.label of
                    Nothing ->
                        text ""

                    Just label ->
                        button
                            [ class "label inline w-max"
                            , onClick (ClickedEntry 0)
                            , type_ "button"
                            ]
                            [ text label ]
                , viewEntry viewConfig.translators
                    (Options options)
                    0
                    entry
                , viewEntryModal (Options options)
                    viewConfig
                    { isVisible = model.isImageCropperOpen, index = 0 }
                    entry
                ]
                |> Html.map toMsg


viewMultiple : MultipleModel -> Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
viewMultiple (MultipleModel model) (Options options) viewConfig toMsg =
    let
        maybeOpenEntry : Maybe Entry
        maybeOpenEntry =
            model.openImageCropperIndex
                |> Maybe.andThen (\index -> List.Extra.getAt index model.entries)
    in
    div [ class options.containerClass ]
        [ case options.label of
            Nothing ->
                text ""

            Just label ->
                p [ class "label" ] [ text label ]
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
                    |> Html.map toMsg
        ]


{-| This is what we use when there isn't an image uploaded yet
-}
viewAddImages : { allowMultiple : Bool } -> Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
viewAddImages allowMultiple (Options options) viewConfig toMsg =
    div [ class options.containerClass ]
        [ viewInput (Options options)
            viewConfig
            allowMultiple
            |> Html.map toMsg
        , Html.label
            [ for options.id

            -- TODO - Aria Hidden?
            , class "cursor-pointer flex file-decoration"
            ]
            (options.addImagesView
                |> Maybe.withDefault defaultAddImagesView
                |> List.map (Html.map Basics.never)
            )
        ]


defaultAddImagesView : List (Html Never)
defaultAddImagesView =
    [ div [ class "p-2 bg-gray-100 flex items-center justify-center w-24 h-24 rounded hover:bg-gray-200" ]
        [ Icons.plus "text-orange-300 fill-current"
        ]
    ]


viewInput : Options msg -> ViewConfig msg -> { allowMultiple : Bool } -> Html Msg
viewInput (Options options) viewConfig { allowMultiple } =
    input
        -- TODO - Aria?
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


viewReplaceImageInput : Options msg -> Int -> Html Msg
viewReplaceImageInput (Options options) index =
    input
        [ type_ "file"
        , id (replaceInputId (Options options) index)
        , class "sr-only form-file"
        , on "change"
            (Json.Decode.at [ "target", "files" ]
                (Json.Decode.index 0 File.decoder)
                |> Json.Decode.map (RequestedReplaceFile index)
            )
        , multiple False
        , acceptFileTypes options.fileTypes
        , disabled options.disabled
        ]
        []


replaceInputId : Options msg -> Int -> String
replaceInputId (Options options) index =
    options.id ++ "-replace-image-" ++ String.fromInt index


viewEntry : Translation.Translators -> Options msg -> Int -> Entry -> Html Msg
viewEntry translators (Options options) index entry =
    let
        viewWithUrl : String -> Html Msg
        viewWithUrl url =
            button
                [ onClick (ClickedEntry index)
                , type_ "button"
                , class options.entryContainerClass
                ]
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
                [ class "p-4 bg-gray-100 grid place-items-center"
                , class options.entryContainerClass
                ]
                [ View.Components.loadingLogoWithNoText "max-w-27"
                ]

        Loaded url ->
            viewWithUrl url

        LoadedWithCropped { original } ->
            viewWithUrl original

        LoadingWithCropped _ ->
            div
                [ class "p-4 bg-gray-100 grid place-items-center"
                , class options.entryContainerClass
                ]
                [ View.Components.loadingLogoWithNoText "max-w-27"
                ]

        LoadedWithCroppedUploaded { croppedUrl } ->
            viewWithUrl croppedUrl

        WithError _ ->
            button
                [ onClick (ClickedEntry index)
                , type_ "button"
                , class options.entryContainerClass
                , class "grid place-items-center bg-gray-100"
                ]
                [ Icons.exclamation ("text-red w-1/2 h-1/2 " ++ options.imageClass)
                ]


viewEntryModal : Options msg -> ViewConfig msg -> { isVisible : Bool, index : Int } -> Entry -> Html Msg
viewEntryModal (Options options) viewConfig { isVisible, index } entry =
    let
        { translators } =
            viewConfig

        viewWithUrl url =
            case entry.fileType of
                LoadedFileType Image ->
                    case entry.imageCropper of
                        WithoutImageCropper ->
                            img [ src url, alt "", class "" ] []

                        WithImageCropper imageCropper ->
                            View.ImageCropper.view imageCropper
                                { imageUrl = url
                                , cropperClass = options.imageCropperClass
                                }
                                |> Html.map GotImageCropperMsg

                LoadedFileType Pdf ->
                    View.Components.pdfViewer []
                        { url = url
                        , childClass = ""
                        , maybeTranslators = Just translators
                        , onFileTypeDiscovered = Nothing
                        }

                LoadingFileType ->
                    View.Components.pdfViewer []
                        { url = url
                        , childClass = ""
                        , maybeTranslators = Just translators
                        , onFileTypeDiscovered = Nothing
                        }
    in
    Modal.initWith
        { closeMsg = ClickedCloseEntryModal
        , isVisible = isVisible
        }
        -- TODO - I18N
        |> Modal.withHeader "Edit image"
        |> Modal.withBody
            -- TODO - I18N
            [ p [] [ text "Drag to reposition the image" ]
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
            [ button
                [ class "uppercase text-orange-300 font-bold"
                , onClick ClickedDeleteEntry
                , type_ "button"
                ]
                -- TODO - I18N
                [ text "Delete" ]
            , div []
                [ viewReplaceImageInput (Options options) index
                , Html.label
                    [ for (replaceInputId (Options options) index)
                    , class "cursor-pointer file-decoration button button-secondary"
                    ]
                    -- TODO - Make it orange!
                    [ Icons.camera "w-4"

                    -- TODO - I18N
                    , text "Change image"
                    ]
                ]
            , button
                [ class "button button-primary"
                , onClick ClickedSaveEntry
                , type_ "button"
                ]
                -- TODO - I18N
                [ text "Save image" ]
            ]
        -- TODO - Should it be fullscreen?
        |> Modal.withSize Modal.FullScreen
        |> Modal.toHtml



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
        , aspectRatio = model.aspectRatio
        , openImageCropperIndex = model.openImageCropperIndex
        }


fromSingleModel : SingleModel -> Model
fromSingleModel (SingleModel model) =
    Model
        { entries = SingleEntry model.entry
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
        , aspectRatio = model.aspectRatio
        , isImageCropperOpen = Maybe.Extra.isJust model.openImageCropperIndex
        }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        RequestedUploadFiles _ ->
            [ "RequestedUploadFiles" ]

        RequestedReplaceFile _ _ ->
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
