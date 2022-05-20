module Form.File2 exposing (FileTypeStatus, Model, Msg, Options, getId, init, initMultiple, initSingle, isEmpty, msgToString, update, view, withDisabled, withMultipleFiles)

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
        { entries : List Entry

        -- TODO - Join these together
        , openEntryIndex : Maybe Int
        , imageCropper : View.ImageCropper.Model
        }


type alias Entry =
    { fileType : FileTypeStatus
    , url : UrlStatus
    }


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
        , acceptMultipleFiles : Bool
        , addImagesViewContainerAttrs : List (Html.Attribute msg)
        , addImagesView : List (Html msg)
        }



-- INITIALIZING


initSingle : { imageUrl : Maybe String, aspectRatio : Float } -> Model
initSingle { imageUrl, aspectRatio } =
    Model
        { entries =
            imageUrl
                |> Maybe.map
                    (\image ->
                        [ { fileType = LoadingFileType
                          , url = Loaded image
                          }
                        ]
                    )
                |> Maybe.withDefault []
        , openEntryIndex = Nothing
        , imageCropper = View.ImageCropper.init { aspectRatio = aspectRatio }
        }


initMultiple : { images : List String, aspectRatio : Float } -> Model
initMultiple { images, aspectRatio } =
    Model
        { entries =
            List.map
                (\image ->
                    { fileType = LoadingFileType
                    , url = Loaded image
                    }
                )
                images
        , openEntryIndex = Nothing
        , imageCropper = View.ImageCropper.init { aspectRatio = aspectRatio }
        }



-- CHANGING OPTIONS


init : { id : String } -> Options msg
init { id } =
    Options
        { id = id
        , disabled = False
        , fileTypes = [ Image ]
        , acceptMultipleFiles = False
        , addImagesViewContainerAttrs = []
        , addImagesView = defaultAddImagesView
        }


withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


withMultipleFiles : Bool -> Options msg -> Options msg
withMultipleFiles acceptMultipleFiles (Options options) =
    Options { options | acceptMultipleFiles = acceptMultipleFiles }



-- PARSING


parser : Translation.Translators -> Model -> Result String String
parser { t } (Model model) =
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


parserMultiple : Translation.Translators -> Model -> Result String (List String)
parserMultiple { t } (Model model) =
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
                                    (index + List.length model.entries)
                                    entry
                            )
                        |> Cmd.batch
            in
            { model | entries = model.entries ++ newEntries }
                |> Model
                |> UR.init
                |> UR.addCmd uploadNewEntries

        CompletedUploadingFile index result ->
            { model
                | entries =
                    List.Extra.updateAt index
                        (\entry -> { entry | url = urlStatusFromResult result })
                        model.entries
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
            let
                ( newImageCropper, cmd ) =
                    View.ImageCropper.update subMsg model.imageCropper
            in
            { model | imageCropper = newImageCropper }
                |> Model
                |> UR.init
                |> UR.addCmd (Cmd.map GotImageCropperMsg cmd)



-- VIEW


type alias ViewConfig msg =
    { value : Model
    , error : Html msg
    , hasError : Bool
    , isRequired : Bool
    , translators : Translation.Translators
    }


view : Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
view (Options options) viewConfig toMsg =
    let
        (Model model) =
            viewConfig.value

        maybeOpenEntry =
            model.openEntryIndex
                |> Maybe.andThen (\index -> List.Extra.getAt index model.entries)
    in
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
                model.entries
            )

        -- TODO - Only show when appropriate
        , viewAddImages (Options options) viewConfig toMsg
        , case maybeOpenEntry of
            Nothing ->
                text ""

            Just entry ->
                viewEntryModal viewConfig.translators model.imageCropper entry
                    |> Html.map toMsg
        ]


{-| This is what we use when there is not an image uploaded yet
-}
viewAddImages : Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
viewAddImages (Options options) viewConfig toMsg =
    div []
        [ viewInput (Options options)
            viewConfig
            { allowMultiple = options.acceptMultipleFiles }
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
