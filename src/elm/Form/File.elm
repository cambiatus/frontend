module Form.File exposing
    ( init, Options
    , withDisabled, withContainerAttrs, withFileTypes, FileType(..), withVariant, Variant(..), RectangleBackground(..)
    , getId
    , isEmpty, parser
    , view
    , Model, initModel, initModelWithChoices, update, Msg, msgToString
    )

{-| Creates a Cambiatus-style File. Use it within a `Form.Form`:

    Form.File.init
        { label = "Photo"
        , id = "photo-input"
        }


# Initializing

@docs init, Options


# Helpers


## Adding attributes

@docs withDisabled, withContainerAttrs, withFileTypes, FileType, withVariant, Variant, RectangleBackground


# Getters

@docs getId


# Helpers

@docs isEmpty, parser


# View

@docs view


# The elm architecture

@docs Model, initModel, initModelWithChoices, update, Msg, msgToString

-}

import Api
import File exposing (File)
import Html exposing (Html, button, div, img, input, p, span)
import Html.Attributes exposing (accept, alt, class, classList, disabled, for, id, multiple, required, src, type_)
import Html.Attributes.Aria exposing (ariaLive)
import Html.Events exposing (on, onClick)
import Http
import Icons
import Json.Decode
import List.Extra
import RemoteData exposing (RemoteData)
import Session.Shared as Shared
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback


type Model
    = SingleFile (RemoteData Http.Error String)
    | WithChoices
        { files : List (RemoteData Http.Error String)
        , selected : Int
        }


initModel : Maybe String -> Model
initModel maybeImage =
    maybeImage
        |> Maybe.map RemoteData.Success
        |> Maybe.withDefault RemoteData.NotAsked
        |> SingleFile


initModelWithChoices : List String -> Model
initModelWithChoices choices =
    WithChoices
        { files = List.map RemoteData.Success choices
        , selected = 0
        }


parser : Shared.Translators -> Model -> Result String String
parser { t } model =
    let
        fromRemoteData remoteData =
            case remoteData of
                RemoteData.Success a ->
                    Ok a

                RemoteData.Failure _ ->
                    Err (t "error.file_upload")

                RemoteData.Loading ->
                    Err (t "error.wait_file_upload")

                RemoteData.NotAsked ->
                    Err (t "error.required")
    in
    case model of
        SingleFile file ->
            fromRemoteData file

        WithChoices { files, selected } ->
            case List.Extra.getAt selected files of
                Just remoteData ->
                    fromRemoteData remoteData

                Nothing ->
                    Err (t "error.file_upload")


isEmpty : Model -> Bool
isEmpty model =
    let
        fromRemoteData remoteData =
            not (RemoteData.isSuccess remoteData)
    in
    case model of
        SingleFile file ->
            fromRemoteData file

        WithChoices { files, selected } ->
            case List.Extra.getAt selected files of
                Just remoteData ->
                    fromRemoteData remoteData

                Nothing ->
                    True



-- OPTIONS


type Options msg
    = Options
        { label : String
        , id : String
        , disabled : Bool
        , containerAttrs : List (Html.Attribute msg)
        , fileTypes : List FileType
        , variant : Variant
        }


{-| Initializes a File
-}
init : { label : String, id : String } -> Options msg
init { label, id } =
    Options
        { label = label
        , id = id
        , disabled = False
        , containerAttrs = []
        , fileTypes = [ Image ]
        , variant = LargeRectangle Purple
        }


type FileType
    = Image
    | PDF


type Variant
    = SmallCircle
    | LargeRectangle RectangleBackground


type RectangleBackground
    = Purple
    | Gray



-- UPDATE


type Msg
    = RequestedUploadFile File
    | CompletedUploadingFile Int (Result Http.Error String)
    | SelectedFile Int


type alias UpdateResult =
    UR.UpdateResult Model Msg Feedback.Model


{-| We don't use Shared directly in order to be able to use the update function
in elm-book more easily, but you can just use `Shared` directly
-}
update :
    { shared
        | endpoints : { endpoints | api : String }
        , translators : Shared.Translators
    }
    -> Msg
    -> Model
    -> UpdateResult
update shared msg model =
    case msg of
        RequestedUploadFile file ->
            let
                ( newModel, index ) =
                    case model of
                        SingleFile _ ->
                            ( SingleFile RemoteData.Loading, 0 )

                        WithChoices { files } ->
                            ( WithChoices
                                { selected = List.length files
                                , files = files ++ [ RemoteData.Loading ]
                                }
                            , List.length files
                            )
            in
            newModel
                |> UR.init
                |> UR.addCmd
                    (Api.uploadImage shared
                        file
                        (CompletedUploadingFile index)
                    )

        CompletedUploadingFile index (Err error) ->
            let
                newModel =
                    case model of
                        SingleFile _ ->
                            SingleFile (RemoteData.Failure error)

                        WithChoices choices ->
                            WithChoices
                                { choices
                                    | files =
                                        List.Extra.updateAt
                                            index
                                            (\_ -> RemoteData.Failure error)
                                            choices.files
                                }
            in
            newModel
                |> UR.init
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure
                        (shared.translators.t "error.file_upload")
                    )
                |> UR.logHttpError msg
                    Nothing
                    "Error uploading file"
                    { moduleName = "Form.File", function = "update" }
                    []
                    error

        CompletedUploadingFile index (Ok url) ->
            let
                newModel =
                    case model of
                        SingleFile _ ->
                            SingleFile (RemoteData.Success url)

                        WithChoices choices ->
                            WithChoices
                                { choices
                                    | files =
                                        List.Extra.updateAt index
                                            (\_ -> RemoteData.Success url)
                                            choices.files
                                }
            in
            newModel
                |> UR.init

        SelectedFile index ->
            let
                newModel =
                    case model of
                        SingleFile file ->
                            SingleFile file

                        WithChoices choices ->
                            WithChoices { choices | selected = index }
            in
            newModel
                |> UR.init



-- ADDING ATTRIBUTES


{-| Determines if the File should be disabled
-}
withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


{-| Adds attributes to the element that contains the file uploader itself, the
label and the error message
-}
withContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withContainerAttrs attrs (Options options) =
    Options { options | containerAttrs = options.containerAttrs ++ attrs }


{-| Adds file types that the input accepts
-}
withFileTypes : List FileType -> Options msg -> Options msg
withFileTypes fileTypes (Options options) =
    Options { options | fileTypes = fileTypes }


{-| Selects the variant to display. This will greatly affect how the input looks
-}
withVariant : Variant -> Options msg -> Options msg
withVariant variant (Options options) =
    Options { options | variant = variant }



-- VIEW


type alias ViewConfig msg =
    { value : Model
    , error : Html msg
    , hasError : Bool
    , isRequired : Bool
    , translators : Shared.Translators
    }


view : Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
view (Options options) viewConfig toMsg =
    case viewConfig.value of
        SingleFile file ->
            -- TODO - Focus styles
            case options.variant of
                LargeRectangle background ->
                    viewLargeRectangle background (Options options) viewConfig file toMsg

                SmallCircle ->
                    viewSmallCircle (Options options) viewConfig file toMsg

        WithChoices choices ->
            viewHardcodedChoices (Options options) viewConfig choices toMsg


viewInput : Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
viewInput (Options options) viewConfig toMsg =
    input
        [ id options.id
        , class "sr-only"
        , type_ "file"
        , onFileChange (RequestedUploadFile >> toMsg)
        , acceptFileTypes options.fileTypes
        , multiple False
        , required viewConfig.isRequired
        , disabled options.disabled
        ]
        []


viewLargeRectangle : RectangleBackground -> Options msg -> ViewConfig msg -> RemoteData Http.Error String -> (Msg -> msg) -> Html msg
viewLargeRectangle background (Options options) viewConfig value toMsg =
    let
        ( backgroundColor, foregroundColor, icon ) =
            case background of
                Purple ->
                    ( "bg-purple-500", "text-white", Icons.camera "" )

                Gray ->
                    ( "bg-gray-100", "text-body-black", Icons.addPhoto "fill-current text-body-black" )
    in
    div options.containerAttrs
        [ View.Components.label [] { targetId = options.id, labelText = options.label }
        , Html.label
            [ class "relative w-full h-56 rounded-sm flex justify-center items-center"
            , class backgroundColor
            , class foregroundColor
            , classList
                [ ( "cursor-pointer", not options.disabled )
                , ( "cursor-not-allowed", options.disabled )
                ]
            ]
            [ viewInput (Options options) viewConfig toMsg
            , case value of
                RemoteData.Loading ->
                    View.Components.loadingLogoAnimated viewConfig.translators ""

                RemoteData.Success url ->
                    div [ class "w-full h-full flex items-center justify-center" ]
                        [ span [ class "absolute bottom-4 right-4 bg-orange-300 w-8 h-8 p-2 rounded-full" ]
                            [ Icons.camera "" ]
                        , if List.member PDF options.fileTypes then
                            View.Components.pdfViewer [ class "h-full w-full" ]
                                { url = url
                                , childClass = "max-h-full max-w-full"
                                , maybeTranslators = Just viewConfig.translators
                                }

                          else
                            img
                                [ src url
                                , class "max-h-full max-w-full"
                                , alt ""
                                ]
                                []
                        ]

                _ ->
                    div [ class "font-bold text-center" ]
                        [ div [ class "w-10 mx-auto mb-2" ] [ icon ]
                        , p []
                            [ Html.text (viewConfig.translators.t "community.actions.proof.upload_hint") ]
                        ]
            ]
        , viewConfig.error
        ]


viewSmallCircle : Options msg -> ViewConfig msg -> RemoteData Http.Error String -> (Msg -> msg) -> Html msg
viewSmallCircle (Options options) viewConfig value toMsg =
    let
        imgClasses =
            "object-cover rounded-full w-20 h-20"

        viewImg =
            case value of
                RemoteData.Success url ->
                    if List.member PDF options.fileTypes then
                        View.Components.pdfViewer [ class imgClasses ]
                            { url = url
                            , childClass = imgClasses
                            , maybeTranslators = Nothing
                            }

                    else
                        img
                            [ class imgClasses
                            , src url
                            , alt ""
                            ]
                            []

                _ ->
                    div
                        [ class (imgClasses ++ " bg-gray-500")
                        , classList [ ( "animate-skeleton-loading", RemoteData.isLoading value ) ]
                        , ariaLive "polite"
                        ]
                        [ if RemoteData.isLoading value then
                            span [ class "sr-only" ] [ Html.text <| viewConfig.translators.t "menu.loading" ]

                          else
                            Html.text ""
                        ]
    in
    div options.containerAttrs
        [ View.Components.label [] { targetId = options.id, labelText = options.label }
        , div [ class "mt-2 m-auto w-20 h-20 relative" ]
            [ viewInput (Options options) viewConfig toMsg
            , Html.label
                [ for options.id
                , class "block"
                , classList
                    [ ( "cursor-pointer", not options.disabled )
                    , ( "cursor-not-allowed", options.disabled )
                    ]
                ]
                [ viewImg
                , span
                    [ class "absolute bottom-0 right-0 bg-orange-300 rounded-full transition-all"
                    , classList
                        [ ( "w-8 h-8 p-2", not (RemoteData.isNotAsked value) )
                        , ( "w-full h-full p-4", RemoteData.isNotAsked value )
                        ]
                    ]
                    [ Icons.camera "" ]
                ]
            ]
        , viewConfig.error
        ]


viewHardcodedChoices :
    Options msg
    -> ViewConfig msg
    -> { selected : Int, files : List (RemoteData Http.Error String) }
    -> (Msg -> msg)
    -> Html msg
viewHardcodedChoices (Options options) viewConfig choices toMsg =
    let
        activeClass =
            "border border-gray-900 shadow-lg"

        itemClass =
            "p-4 border border-white rounded-md w-full h-full flex items-center justify-center focus-outline-none focus:border hover:border-gray-900 focus:border-gray-900 hover:shadow-lg focus:shadow-lg"

        viewItem index choiceStatus =
            button
                [ class itemClass
                , classList [ ( activeClass, index == choices.selected ) ]
                , type_ "button"
                , onClick (SelectedFile index)
                ]
                [ case choiceStatus of
                    RemoteData.Loading ->
                        div [ class "w-16 h-16" ]
                            [ View.Components.loadingLogoAnimatedFluid ]

                    RemoteData.Success url ->
                        div
                            [ class "w-16 h-16 bg-contain bg-center bg-no-repeat"
                            , Html.Attributes.style "background-image"
                                ("url(" ++ url ++ ")")
                            ]
                            []

                    _ ->
                        div []
                            []
                ]
                |> Html.map toMsg
    in
    div options.containerAttrs
        [ View.Components.label []
            { targetId = options.id, labelText = options.label }
        , div
            [ class "grid gap-4 xs-max:grid-cols-1 grid-cols-2 sm:grid-cols-3 md:grid-cols-5 lg:grid-cols-7" ]
            (List.indexedMap viewItem choices.files
                ++ [ viewInput (Options options) viewConfig toMsg
                   , Html.label
                        [ for options.id
                        , class ("flex-col text-center cursor-pointer " ++ itemClass)

                        -- TODO - Disable
                        ]
                        [ div [ class "bg-gradient-to-bl from-orange-300 to-orange-500 rounded-full p-2 mb-1 w-12 h-12 flex items-center justify-center" ]
                            [ Icons.imageMultiple "text-white fill-current w-8 h-8"
                            ]
                        , Html.text (viewConfig.translators.t "community.create.labels.upload_icon")
                        ]
                   ]
            )
        ]



-- GETTERS


getId : Options msg -> String
getId (Options options) =
    options.id



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    case msg of
        RequestedUploadFile _ ->
            [ "RequestedUploadFile" ]

        CompletedUploadingFile _ r ->
            [ "CompletedUploadingFile", UR.resultToString r ]

        SelectedFile _ ->
            [ "SelectedFile" ]



-- INTERNAL HELPERS


onFileChange : (File -> msg) -> Html.Attribute msg
onFileChange toMsg =
    on "change"
        (Json.Decode.at [ "target", "files" ] (Json.Decode.index 0 File.decoder)
            |> Json.Decode.map toMsg
        )


fileTypeToString : FileType -> String
fileTypeToString fileType =
    case fileType of
        Image ->
            "image/*"

        PDF ->
            ".pdf"


acceptFileTypes : List FileType -> Html.Attribute msg
acceptFileTypes fileTypes =
    fileTypes
        |> List.map fileTypeToString
        |> String.join ","
        |> accept
