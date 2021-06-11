module View.Form.FileUploader exposing
    ( init
    , withAttrs, withVariant, withBackground, withFileTypes
    , toHtml
    , Background(..), FileType(..), Variant(..)
    )

{-| Creates a Cambiatus-style file uploader that supports pictures

    View.Form.FileUploader.init
        { label = "Photo"
        , id = "photo_input"
        , onFileInput = EnteredPhoto
        , status = model.photoStatus
        }
        |> View.Form.FileUploader.toHtml


# Initializing

@docs InitialOptions, init


# Helpers

@docs withAttrs, withVariant, withBackground, withFileTypes


# Converting to HTML

@docs toHtml

-}

import File exposing (File)
import Html exposing (Html, div, img, input, label, span, text)
import Html.Attributes exposing (accept, class, for, id, multiple, src, type_)
import Html.Events exposing (on)
import Http
import Icons
import Json.Decode as Decode
import RemoteData exposing (RemoteData)
import Session.Shared exposing (Translators)
import View.Components



-- MODEL


{-| Represents the file uploader completely
-}
type alias Options msg =
    { label : String
    , id : String
    , onFileInput : List File -> msg
    , status : RemoteData Http.Error String
    , extraAttrs : List (Html.Attribute msg)
    , variant : Variant
    , background : Background
    , fileTypes : List FileType
    }



-- INITIALIZING


{-| Minimum required options for a file input
-}
type alias InitialOptions msg =
    { label : String
    , id : String
    , onFileInput : List File -> msg
    , status : RemoteData Http.Error String
    }


type Variant
    = Small
    | Large


type Background
    = Purple
    | Gray


type FileType
    = Image
    | PDF


{-| Initializes a file uploader
-}
init : InitialOptions msg -> Options msg
init options =
    { label = options.label
    , id = options.id
    , onFileInput = options.onFileInput
    , status = options.status
    , extraAttrs = []
    , variant = Large
    , background = Purple
    , fileTypes = [ Image ]
    }



-- HELPERS


{-| Adds attributes to the div that holds the label and the file uploader
-}
withAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withAttrs attrs options =
    { options | extraAttrs = options.extraAttrs ++ attrs }


{-| Select the `Variant` to be used
-}
withVariant : Variant -> Options msg -> Options msg
withVariant variant options =
    { options | variant = variant }


withBackground : Background -> Options msg -> Options msg
withBackground background options =
    { options | background = background }


{-| Define the file types we can accept
-}
withFileTypes : List FileType -> Options msg -> Options msg
withFileTypes fileTypes options =
    { options | fileTypes = fileTypes }



-- TO HTML


{-| Transforms `Options` into `Html`
-}
toHtml : Translators -> Options msg -> Html msg
toHtml translators options =
    case options.variant of
        Large ->
            viewLarge translators options

        Small ->
            viewSmall translators options



-- INTERNAL


onFileChange : (List File -> msg) -> Html.Attribute msg
onFileChange toMsg =
    Decode.list File.decoder
        |> Decode.at [ "target", "files" ]
        |> Decode.map toMsg
        |> on "change"


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


viewLarge : Translators -> Options msg -> Html msg
viewLarge ({ t } as translators) options =
    let
        ( backgroundColor, foregroundColor, icon ) =
            case options.background of
                Purple ->
                    ( "bg-purple-500", "text-white", Icons.camera "" )

                Gray ->
                    ( "bg-gray-100", "text-body-black", Icons.addPhoto "fill-current text-body-black" )
    in
    div options.extraAttrs
        [ span [ class "input-label" ] [ text (t options.label) ]
        , label
            [ class "relative w-full h-56 rounded-sm flex justify-center items-center cursor-pointer"
            , class backgroundColor
            , class foregroundColor
            ]
            [ input
                [ id options.id
                , class "hidden-img-input"
                , type_ "file"
                , acceptFileTypes options.fileTypes
                , onFileChange options.onFileInput
                , multiple False
                ]
                []
            , case options.status of
                RemoteData.Loading ->
                    View.Components.loadingLogoAnimated translators ""

                RemoteData.Success url ->
                    div [ class "w-full h-full flex items-center justify-center" ]
                        [ span [ class "absolute bottom-0 right-0 mr-4 mb-4 bg-orange-300 w-8 h-8 p-2 rounded-full" ]
                            [ Icons.camera "" ]
                        , if List.member PDF options.fileTypes then
                            View.Components.pdfViewer [ class "h-full w-full text-white" ]
                                { url = url
                                , childClass = "max-h-full max-w-full"
                                , maybeTranslators = Just translators
                                }

                          else
                            img [ src url, class "max-h-full max-w-full" ] []
                        ]

                _ ->
                    div [ class "text-body font-bold text-center" ]
                        [ div [ class "w-10 mx-auto mb-2" ] [ icon ]
                        , div [] [ text (t "community.actions.proof.upload_hint") ]
                        ]
            ]
        ]


viewSmall : Translators -> Options msg -> Html msg
viewSmall { t } options =
    let
        imgClasses =
            "object-cover rounded-full w-20 h-20"

        viewImg =
            case options.status of
                RemoteData.Success url ->
                    if List.member PDF options.fileTypes then
                        View.Components.pdfViewer [ class imgClasses ]
                            { url = url
                            , childClass = imgClasses
                            , maybeTranslators = Nothing
                            }

                    else
                        img [ class imgClasses, src url ] []

                _ ->
                    div [ class (imgClasses ++ " bg-gray-500") ] []
    in
    div options.extraAttrs
        [ div [ class "input-label" ]
            [ text (t options.label) ]
        , div [ class "mt-2 m-auto w-20 h-20 relative" ]
            [ input
                [ id options.id
                , class "profile-img-input"
                , type_ "file"
                , acceptFileTypes options.fileTypes
                , onFileChange options.onFileInput
                , multiple False
                ]
                []
            , label
                [ for options.id
                , class "block cursor-pointer"
                ]
                [ viewImg
                , span [ class "absolute bottom-0 right-0 bg-orange-300 w-8 h-8 p-2 rounded-full" ] [ Icons.camera "" ]
                ]
            ]
        ]
