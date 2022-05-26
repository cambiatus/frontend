module Form.File2 exposing
    ( FileTypeStatus
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
    , withDisabled
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

        -- TODO - Join these together
        , openEntryIndex : Maybe Int
        , imageCropper : View.ImageCropper.Model
        }


type MultipleModel
    = MultipleModel
        { entries : List Entry
        }


type SingleModel
    = SingleModel
        { entry : Maybe Entry
        }


type alias Entry =
    { fileType : FileTypeStatus
    , url : UrlStatus
    }


type Entries
    = SingleEntry (Maybe Entry)
    | MultipleEntries (List Entry)


type FileType
    = Pdf
    | Image


type FileTypeStatus
    = LoadingFileType
    | LoadedFileType FileType


type UrlStatus
    = Loading File
    | Loaded String
    | WithError Http.Error


type Options msg
    = Options
        { id : String
        , disabled : Bool
        , fileTypes : List FileType
        , addImagesViewContainerAttrs : List (Html.Attribute msg)
        , addImagesView : List (Html msg)
        }



-- INITIALIZING


initSingle : { fileUrl : Maybe String, aspectRatio : Float } -> SingleModel
initSingle { fileUrl, aspectRatio } =
    SingleModel
        { entry =
            fileUrl
                |> Maybe.map
                    (\file ->
                        { fileType = LoadingFileType, url = Loaded file }
                    )
        }


initMultiple : { fileUrls : List String, aspectRatio : Float } -> MultipleModel
initMultiple { fileUrls, aspectRatio } =
    MultipleModel
        { entries =
            List.map
                (\file ->
                    { fileType = LoadingFileType
                    , url = Loaded file
                    }
                )
                fileUrls
        }



-- CHANGING OPTIONS


init : { id : String } -> Options msg
init { id } =
    Options
        { id = id
        , disabled = False
        , fileTypes = [ Image ]
        , addImagesViewContainerAttrs = []
        , addImagesView = defaultAddImagesView
        }


withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }



-- PARSING


parser : Translation.Translators -> SingleModel -> Result String String
parser { t } _ =
    -- case List.head model.entries |> Maybe.map .url of
    --     Nothing ->
    --         Err (t "error.required")
    --     Just RemoteData.NotAsked ->
    --         Err (t "error.required")
    --     Just RemoteData.Loading ->
    --         Err (t "error.wait_file_upload")
    --     Just (RemoteData.Failure _) ->
    --         Err (t "error.file_upload")
    --     Just (RemoteData.Success url) ->
    --         Ok url
    -- TODO
    Err "Implement this"


parserMultiple : Translation.Translators -> MultipleModel -> Result String (List String)
parserMultiple { t } _ =
    -- List.foldl
    --     (\entry result ->
    --         case result of
    --             Err err ->
    --                 Err err
    --             Ok validEntries ->
    --                 case entry.url of
    --                     RemoteData.NotAsked ->
    --                         Ok validEntries
    --                     RemoteData.Loading ->
    --                         Err (t "error.wait_file_upload")
    --                     RemoteData.Failure _ ->
    --                         Err (t "error.file_upload")
    --                     RemoteData.Success url ->
    --                         Ok (url :: validEntries)
    --     )
    --     (Ok [])
    --     model.entries
    --     |> Result.map List.reverse
    -- TODO
    Err "Implement this"



-- UPDATE


type Msg
    = NoOp
    | RequestedUploadFiles (List File)
    | CompletedUploadingFile Int (Result Http.Error String)
    | ClickedEntry Int
    | ClickedCloseEntryModal
    | GotImageCropperMsg View.ImageCropper.Msg


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
        NoOp ->
            UR.init (Model model)

        RequestedUploadFiles files ->
            case model.entries of
                SingleEntry _ ->
                    case
                        files
                            |> List.head
                            |> Maybe.andThen entryFromFile
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
                            List.filterMap entryFromFile files

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

        CompletedUploadingFile index result ->
            { model
                | entries =
                    case model.entries of
                        SingleEntry _ ->
                            { fileType = LoadingFileType
                            , url = urlStatusFromResult result
                            }
                                |> Just
                                |> SingleEntry

                        MultipleEntries entries ->
                            entries
                                |> List.Extra.updateAt index
                                    (\entry -> { entry | url = urlStatusFromResult result })
                                |> MultipleEntries
            }
                |> Model
                |> UR.init

        ClickedEntry index ->
            { model | openEntryIndex = Just index }
                |> Model
                |> UR.init

        ClickedCloseEntryModal ->
            { model | openEntryIndex = Nothing }
                |> Model
                |> UR.init

        GotImageCropperMsg subMsg ->
            View.ImageCropper.update subMsg model.imageCropper
                |> UR.fromChild (\newImageCropper -> Model { model | imageCropper = newImageCropper })
                    GotImageCropperMsg
                    (\_ -> identity)
                    (Model model)



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
        SingleEntry entry ->
            viewSingle entry options viewConfig toMsg

        MultipleEntries entries ->
            viewMultiple entries options viewConfig toMsg


viewSingle : Maybe Entry -> Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
viewSingle maybeEntry options viewConfig toMsg =
    case maybeEntry of
        Nothing ->
            viewAddImages { allowMultiple = False }
                options
                viewConfig
                toMsg

        Just entry ->
            -- TODO - Image cropper
            viewEntry viewConfig.translators 0 entry
                |> Html.map toMsg


viewMultiple : List Entry -> Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
viewMultiple entries (Options options) viewConfig toMsg =
    div []
        [ Html.Keyed.ul
            []
            (List.indexedMap
                (\index entry ->
                    ( "entry-" ++ String.fromInt index
                    , li []
                        [ viewEntry viewConfig.translators index entry
                            |> Html.map toMsg
                        ]
                    )
                )
                entries
            )
        , viewAddImages
            { allowMultiple = True }
            (Options options)
            viewConfig
            toMsg

        -- TODO - Image cropper
        -- , case maybeOpenEntry of
        --     Nothing ->
        --         text ""
        --     Just entry ->
        --         viewEntryModal viewConfig.translators model.imageCropper entry
        --             |> Html.map toMsg
        ]


{-| This is what we use when there is not an image uploaded yet
-}
viewAddImages : { allowMultiple : Bool } -> Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
viewAddImages allowMultiple (Options options) viewConfig toMsg =
    div []
        [ viewInput (Options options)
            viewConfig
            allowMultiple
            toMsg
        , Html.label
            (for options.id
                -- TODO - Aria Hidden?
                :: class "cursor-pointer flex file-decoration w-min"
                :: options.addImagesViewContainerAttrs
            )
            options.addImagesView
        ]


defaultAddImagesView : List (Html msg)
defaultAddImagesView =
    [ div [ class "bg-white p-2 border border-orange-300 flex items-center justify-center w-10 h-10 rounded-sm" ]
        [ Icons.plus "text-orange-300 fill-current"
        ]
    ]


viewInput : Options msg -> ViewConfig msg -> { allowMultiple : Bool } -> (Msg -> msg) -> Html msg
viewInput (Options options) viewConfig { allowMultiple } toMsg =
    -- TODO - We should give it `allowMultiple = False` when inside the modal
    input
        -- TODO - Aria?
        [ type_ "file"
        , id options.id
        , class "sr-only form-file"
        , on "change"
            (Json.Decode.at [ "target", "files" ]
                -- TODO - Should we make sure it's not empty? (Decode.andThen ...)
                (Json.Decode.list File.decoder)
                |> Json.Decode.map RequestedUploadFiles
            )
        , multiple allowMultiple
        , acceptFileTypes options.fileTypes
        , disabled options.disabled
        , required viewConfig.isRequired
        ]
        []
        |> Html.map toMsg


viewEntry : Translation.Translators -> Int -> Entry -> Html Msg
viewEntry translators index entry =
    case entry.url of
        Loading _ ->
            View.Components.loadingLogoAnimated translators ""

        Loaded url ->
            button
                [ onClick (ClickedEntry index)
                ]
                [ case entry.fileType of
                    LoadedFileType Image ->
                        img [ src url, alt "" ] []

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
                ]

        WithError _ ->
            -- TODO
            text ""


viewEntryModal : Translation.Translators -> View.ImageCropper.Model -> Entry -> Html Msg
viewEntryModal translators imageCropper entry =
    Modal.initWith
        { closeMsg = ClickedCloseEntryModal
        , isVisible = True
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
                        case entry.fileType of
                            LoadedFileType Image ->
                                View.ImageCropper.view imageCropper { imageUrl = url }
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

                    WithError _ ->
                        -- TODO
                        text ""
                ]
            ]
        |> Modal.withFooter
            -- TODO - Add event handler
            [ button [ class "uppercase text-orange-300 font-bold" ]
                -- TODO - I18N
                [ text "Delete" ]

            -- TODO - Add event handler
            -- TODO - This should be an input
            , button [ class "button button-secondary" ]
                -- TODO - Make it orange!
                [ Icons.camera "w-4"

                -- TODO - I18N
                , text "Change image"
                ]

            -- TODO - Add event handler
            , button [ class "button button-primary" ]
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
    -- List.isEmpty model.entries
    --     || List.any (\entry -> not (RemoteData.isSuccess entry.url)) model.entries
    -- TODO
    True


isLoading : Model -> Bool
isLoading (Model model) =
    -- List.any (\entry -> RemoteData.isLoading entry.url) model.entries
    -- TODO
    False


fileTypeToString : FileType -> String
fileTypeToString fileType =
    case fileType of
        Image ->
            "image/*"

        Pdf ->
            -- TODO - Test this
            -- ".pdf"
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


entryFromFile : File -> Maybe Entry
entryFromFile file =
    file
        |> File.mime
        |> fileTypeFromString
        |> Maybe.map
            (\fileType ->
                { fileType = LoadedFileType fileType
                , url = Loading file
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

        WithError _ ->
            Cmd.none


urlStatusFromResult : Result Http.Error String -> UrlStatus
urlStatusFromResult result =
    case result of
        Ok url ->
            Loaded url

        Err err ->
            WithError err


fromMultipleModel : MultipleModel -> Model
fromMultipleModel _ =
    Debug.todo ""


toMultipleModel : Model -> MultipleModel
toMultipleModel _ =
    Debug.todo ""


fromSingleModel : SingleModel -> Model
fromSingleModel _ =
    Debug.todo ""


toSingleModel : Model -> SingleModel
toSingleModel _ =
    Debug.todo ""


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        RequestedUploadFiles _ ->
            [ "RequestedUploadFiles" ]

        CompletedUploadingFile _ _ ->
            [ "CompletedUploadingFile" ]

        ClickedEntry _ ->
            [ "ClickedEntry" ]

        ClickedCloseEntryModal ->
            [ "ClickedCloseEntryModal" ]

        GotImageCropperMsg subMsg ->
            "GotImageCropperMsg" :: View.ImageCropper.msgToString subMsg
