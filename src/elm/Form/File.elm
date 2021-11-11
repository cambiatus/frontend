module Form.File exposing
    ( init, Options
    , withDisabled, withContainerAttrs, withFileTypes, withVariant, Variant(..), RectangleBackground(..)
    , getId
    , view
    )

{-| Creates a Cambiatus-style File. Use it within a `Form.Form`:

    Form.File.init
        { label = text "Photo"
        , id = "photo-input"
        }


# Initializing

@docs init, Options


# Helpers


## Adding attributes

@docs withDisabled, withContainerAttrs, withFileTypes, withVariant, Variant, RectangleBackground


# Getters

@docs getId


# View

@docs view

-}

import File exposing (File)
import Html exposing (Html, div, img, input, label, p, span)
import Html.Attributes exposing (accept, alt, class, classList, disabled, for, id, multiple, required, src, type_)
import Html.Attributes.Aria exposing (ariaLive)
import Html.Events exposing (on, onBlur)
import Http
import Icons
import Json.Decode
import RemoteData exposing (RemoteData)
import Session.Shared exposing (Translators)
import View.Components
import View.Form



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
    { onInput : File -> msg
    , value : RemoteData Http.Error String
    , error : Html msg
    , hasError : Bool
    , isRequired : Bool
    , translators : Translators
    }


view : Options msg -> ViewConfig msg -> Html msg
view (Options options) viewConfig =
    case options.variant of
        LargeRectangle background ->
            viewLargeRectangle background (Options options) viewConfig

        SmallCircle ->
            viewSmallCircle (Options options) viewConfig


viewInput : Options msg -> ViewConfig msg -> Html msg
viewInput (Options options) viewConfig =
    input
        [ id options.id
        , class "sr-only"
        , type_ "file"
        , onFileChange viewConfig.onInput
        , acceptFileTypes options.fileTypes
        , multiple False
        , required viewConfig.isRequired
        ]
        []


viewLargeRectangle : RectangleBackground -> Options msg -> ViewConfig msg -> Html msg
viewLargeRectangle background (Options options) viewConfig =
    let
        ( backgroundColor, foregroundColor, icon ) =
            case background of
                Purple ->
                    ( "bg-purple-500", "text-white", Icons.camera "" )

                Gray ->
                    ( "bg-gray-100", "text-body-black", Icons.addPhoto "fill-current text-body-black" )
    in
    div options.containerAttrs
        [ View.Form.label [] options.id options.label
        , label
            [ class "relative w-full h-56 rounded-sm flex justify-center items-center cursor-pointer"
            , class backgroundColor
            , class foregroundColor
            ]
            [ viewInput (Options options) viewConfig
            , case viewConfig.value of
                RemoteData.Loading ->
                    View.Components.loadingLogoAnimated viewConfig.translators ""

                RemoteData.Success url ->
                    div [ class "w-full h-full flex items-center justify-center" ]
                        [ span [ class "absolute bottom-4 right-4 bg-orange-300 w-8 h-8 p-2 rounded-full" ]
                            [ Icons.camera "" ]
                        , if List.member PDF options.fileTypes then
                            View.Components.pdfViewer [ class "h-full w-full text-white" ]
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


viewSmallCircle : Options msg -> ViewConfig msg -> Html msg
viewSmallCircle (Options options) viewConfig =
    let
        imgClasses =
            "object-cover rounded-full w-20 h-20"

        viewImg =
            case viewConfig.value of
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
                        , classList [ ( "animate-skeleton-loading", RemoteData.isLoading viewConfig.value ) ]
                        , ariaLive "polite"
                        ]
                        [ if RemoteData.isLoading viewConfig.value then
                            span [ class "sr-only" ] [ Html.text <| viewConfig.translators.t "menu.loading" ]

                          else
                            Html.text ""
                        ]
    in
    div options.containerAttrs
        [ View.Form.label [] options.id options.label
        , div [ class "mt-2 m-auto w-20 h-20 relative" ]
            [ viewInput (Options options) viewConfig
            , label
                [ for options.id
                , class "block cursor-pointer"
                ]
                [ viewImg
                , span
                    [ class "absolute bottom-0 right-0 bg-orange-300 rounded-full transition-all"
                    , classList
                        [ ( "w-8 h-8 p-2", not (RemoteData.isNotAsked viewConfig.value) )
                        , ( "w-full h-full p-4", RemoteData.isNotAsked viewConfig.value )
                        ]
                    ]
                    [ Icons.camera "" ]
                ]
            ]
        , viewConfig.error
        ]



-- GETTERS


getId : Options msg -> String
getId (Options options) =
    options.id



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
