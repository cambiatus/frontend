module View.ImageCropper exposing (ExtMsg(..), Model, Msg, init, msgToString, update, view)

import Browser.Dom
import Dict
import File
import Html exposing (Html, button, div, img, input, node)
import Html.Attributes exposing (alt, attribute, class, classList, draggable, id, src, style, type_, value)
import Html.Events
import Icons
import Json.Decode
import Json.Encode
import Task
import UpdateResult as UR
import View.Components


type alias Model =
    { dimmensions : DimmensionsState
    , aspectRatio : Float
    , isDragging : Bool
    , isChangingDimmensions : Bool
    , isRequestingCroppedImage : Bool
    , isReflowing : Bool
    }


init : { aspectRatio : Float } -> Model
init { aspectRatio } =
    { dimmensions = Loading
    , aspectRatio = aspectRatio
    , isDragging = False
    , isChangingDimmensions = False
    , isRequestingCroppedImage = False
    , isReflowing = False
    }


type DimmensionsState
    = Loading
    | Loaded Dimmensions


type alias Dimmensions =
    { container :
        { left : Float
        , width : Float
        , top : Float
        , height : Float
        }
    , image :
        { left : Float
        , width : Float
        , top : Float
        , height : Float
        }
    , maximumWidthRatioPossible : Float
    , leftOffset : Float
    , topOffset : Float
    , selectorBoxSizeMultiplier : Float
    }


type Msg
    = ImageLoaded
    | GotImageDimmensions (Result Browser.Dom.Error { imageElement : Browser.Dom.Element, containerElement : Browser.Dom.Element })
    | StartedDragging
    | StoppedDragging
    | Dragged { x : Float, y : Float, previousX : Float, previousY : Float }
    | ChangedDimmensions String
    | CompletedChangingDimmensions
    | ClickedZoomOperation ZoomOperation
    | GotCroppedImage File.File


type ZoomOperation
    = Minus
    | Plus


type ExtMsg
    = CompletedCropping File.File


type alias UpdateResult =
    UR.UpdateResult Model Msg ExtMsg


update : Msg -> Model -> UpdateResult
update msg model =
    case msg of
        ImageLoaded ->
            model
                |> UR.init
                |> UR.addCmd
                    (Browser.Dom.getElement entireImageId
                        |> Task.andThen
                            (\imageElement ->
                                Browser.Dom.getElement imageContainerId
                                    |> Task.map
                                        (\containerElement ->
                                            { imageElement = imageElement
                                            , containerElement = containerElement
                                            }
                                        )
                            )
                        |> Task.attempt GotImageDimmensions
                    )

        GotImageDimmensions (Ok { imageElement, containerElement }) ->
            let
                image =
                    imageElement.element

                container =
                    containerElement.element

                maximumWidthPossible =
                    min container.width (container.height * model.aspectRatio)

                isExistingImage =
                    case model.dimmensions of
                        Loading ->
                            False

                        Loaded existingDimmensions ->
                            existingDimmensions.image.width == image.width && existingDimmensions.image.height == image.height

                normalizeFromExisting dimmensions =
                    case model.dimmensions of
                        Loading ->
                            dimmensions

                        Loaded existingDimmensions ->
                            if isExistingImage then
                                { dimmensions
                                    | selectorBoxSizeMultiplier = existingDimmensions.selectorBoxSizeMultiplier
                                    , leftOffset = existingDimmensions.leftOffset + dimmensions.container.left - existingDimmensions.container.left
                                    , topOffset = existingDimmensions.topOffset + dimmensions.container.top - existingDimmensions.container.top
                                }

                            else
                                dimmensions

                center dimmensions =
                    if isExistingImage then
                        dimmensions

                    else
                        let
                            selection =
                                calculateSelectionDimmensions model dimmensions
                        in
                        { dimmensions
                            | leftOffset = dimmensions.leftOffset - (selection.width / 2)
                            , topOffset = dimmensions.topOffset - (selection.height / 2)
                        }

                newDimmensions =
                    { container =
                        { left = container.x
                        , width = container.width
                        , top = container.y
                        , height = container.height
                        }
                    , image =
                        { left = image.x
                        , width = image.width
                        , top = image.y
                        , height = image.height
                        }
                    , maximumWidthRatioPossible = maximumWidthPossible / container.width
                    , leftOffset = image.x + (image.width / 2)
                    , topOffset = image.y + (image.height / 2)
                    , selectorBoxSizeMultiplier = (maximumWidthPossible / image.width) * 0.75
                    }
                        |> normalizeFromExisting
                        |> center
            in
            { model
                | dimmensions = Loaded newDimmensions
                , isReflowing =
                    case model.dimmensions of
                        Loading ->
                            False

                        Loaded existingDimmensions ->
                            (abs (existingDimmensions.image.width - newDimmensions.image.width) > 50)
                                || (abs (existingDimmensions.image.height - newDimmensions.image.height) > 50)
            }
                |> UR.init

        GotImageDimmensions (Err (Browser.Dom.NotFound id)) ->
            UR.init model
                |> UR.logImpossible msg
                    "Could not get image dimmensions"
                    Nothing
                    { moduleName = "View.ImageCropper", function = "update" }
                    [ { name = "Browser.Dom error"
                      , extras = Dict.fromList [ ( "id", Json.Encode.string id ) ]
                      }
                    ]

        StartedDragging ->
            { model | isDragging = True }
                |> UR.init
                |> UR.addPort
                    { responseAddress = StartedDragging
                    , responseData = Json.Encode.null
                    , data =
                        Json.Encode.object
                            [ ( "name", Json.Encode.string "addClassToDocument" )
                            , ( "className", Json.Encode.string "cursor-move" )
                            ]
                    }

        StoppedDragging ->
            { model
                | isDragging = False
                , isRequestingCroppedImage = True
            }
                |> UR.init
                |> UR.addPort
                    { responseAddress = StartedDragging
                    , responseData = Json.Encode.null
                    , data =
                        Json.Encode.object
                            [ ( "name", Json.Encode.string "removeClassFromDocument" )
                            , ( "className", Json.Encode.string "cursor-move" )
                            ]
                    }

        Dragged { x, y } ->
            case model.dimmensions of
                Loading ->
                    UR.init model

                Loaded dimmensions ->
                    if not model.isDragging then
                        UR.init model

                    else
                        let
                            selection =
                                calculateSelectionDimmensions model dimmensions
                        in
                        { model
                            | dimmensions =
                                Loaded
                                    { dimmensions
                                        | topOffset = y - selection.height / 2
                                        , leftOffset = x - selection.width / 2
                                    }
                        }
                            |> UR.init

        ChangedDimmensions percentageString ->
            case model.dimmensions of
                Loading ->
                    UR.init model

                Loaded dimmensions ->
                    case String.toFloat percentageString of
                        Nothing ->
                            UR.init model

                        Just percentageValue ->
                            { model
                                | dimmensions =
                                    Loaded { dimmensions | selectorBoxSizeMultiplier = percentageValue }
                                , isChangingDimmensions = abs (percentageValue - dimmensions.selectorBoxSizeMultiplier) <= 0.05
                            }
                                |> UR.init

        CompletedChangingDimmensions ->
            { model
                | isRequestingCroppedImage = True
                , isChangingDimmensions = False
            }
                |> UR.init

        ClickedZoomOperation operation ->
            case model.dimmensions of
                Loading ->
                    UR.init model

                Loaded dimmensions ->
                    let
                        entireRange =
                            dimmensions.maximumWidthRatioPossible - 0.1

                        clampPossibleValues =
                            clamp 0.1 dimmensions.maximumWidthRatioPossible

                        delta =
                            0.1 * entireRange
                    in
                    { model
                        | dimmensions =
                            Loaded
                                { dimmensions
                                    | selectorBoxSizeMultiplier =
                                        case operation of
                                            Minus ->
                                                clampPossibleValues (dimmensions.selectorBoxSizeMultiplier - delta)

                                            Plus ->
                                                clampPossibleValues (dimmensions.selectorBoxSizeMultiplier + delta)
                                }
                    }
                        |> UR.init

        GotCroppedImage file ->
            { model | isRequestingCroppedImage = False }
                |> UR.init
                |> UR.addExt (CompletedCropping file)


view : Model -> { imageUrl : String, cropperAttributes : List (Html.Attribute Never) } -> Html Msg
view model { imageUrl, cropperAttributes } =
    div [ class "mx-auto w-full md:flex md:flex-col md:w-auto" ]
        [ div
            [ class "relative max-w-max mx-auto flex items-center justify-center max-h-[35vh] lg:max-h-[60vh]"
            , style "aspect-ratio" (String.fromFloat model.aspectRatio)
            , id imageContainerId
            ]
            (img
                [ src imageUrl
                , alt ""
                , id entireImageId
                , class "opacity-20 pointer-events-none select-none max-w-full max-h-[35vh] lg:max-h-[60vh]"
                , Html.Events.on "load" (Json.Decode.succeed ImageLoaded)
                ]
                []
                :: (case model.dimmensions of
                        Loading ->
                            []

                        Loaded dimmensions ->
                            viewCropper model
                                dimmensions
                                { imageUrl = imageUrl
                                , cropperAttributes = cropperAttributes
                                }
                   )
            )
        , case model.dimmensions of
            Loading ->
                Html.text ""

            Loaded dimmensions ->
                viewSlider dimmensions
        ]


viewCropper :
    Model
    -> Dimmensions
    -> { imageUrl : String, cropperAttributes : List (Html.Attribute Never) }
    -> List (Html Msg)
viewCropper model dimmensions { imageUrl, cropperAttributes } =
    let
        selection =
            calculateSelectionDimmensions model dimmensions

        leftOffset =
            clamp 0
                (dimmensions.container.width - selection.width)
                (dimmensions.leftOffset - dimmensions.container.left)

        topOffset =
            clamp 0
                (dimmensions.container.height - selection.height)
                (dimmensions.topOffset - dimmensions.container.top)

        floatToPx offset =
            String.fromFloat offset ++ "px"
    in
    [ View.Components.pointerListener
        { document =
            { onDragOver =
                if model.isDragging then
                    Just Dragged

                else
                    Nothing
            }
        , element =
            { onDragStart =
                Just
                    { dragImage = Nothing
                    , listener = StartedDragging
                    }
            , onDragEnd = Just StoppedDragging
            }
        }
        (class "absolute overflow-hidden border border-dashed border-gray-400 cursor-move z-20 select-none mx-auto"
            :: classList [ ( "transition-all origin-center", not model.isDragging && not model.isChangingDimmensions && not model.isReflowing ) ]
            :: style "top" (floatToPx topOffset)
            :: style "left" (floatToPx leftOffset)
            :: style "width" (floatToPx selection.width)
            :: style "height" (floatToPx selection.height)
            :: draggable "true"
            :: List.map (Html.Attributes.map Basics.never) cropperAttributes
        )
        [ img
            [ src imageUrl
            , alt ""
            , class "absolute object-cover max-w-none pointer-events-none select-none"
            , classList [ ( "transition-all origin-center", not model.isDragging && not model.isChangingDimmensions && not model.isReflowing ) ]
            , style "width" (floatToPx dimmensions.image.width)
            , style "height" (floatToPx dimmensions.image.height)

            -- We need the -1 to compensate for the border
            , style "top" (floatToPx (dimmensions.image.top - dimmensions.container.top - topOffset - 1))
            , style "left" (floatToPx (dimmensions.image.left - dimmensions.container.left - leftOffset - 1))
            ]
            []
        ]
    , node "image-cropper"
        [ if model.isRequestingCroppedImage then
            attribute "elm-generate-new-cropped-image" "true"

          else
            class ""
        , attribute "elm-url" imageUrl
        , attribute "elm-image-width" (String.fromFloat dimmensions.image.width)
        , attribute "elm-image-height" (String.fromFloat dimmensions.image.height)
        , attribute "elm-image-left" (String.fromFloat dimmensions.image.left)
        , attribute "elm-image-top" (String.fromFloat dimmensions.image.top)
        , attribute "elm-container-left" (String.fromFloat dimmensions.container.left)
        , attribute "elm-container-top" (String.fromFloat dimmensions.container.top)
        , attribute "elm-selection-left" (String.fromFloat leftOffset)
        , attribute "elm-selection-top" (String.fromFloat topOffset)
        , attribute "elm-selection-width" (String.fromFloat selection.width)
        , attribute "elm-selection-height" (String.fromFloat selection.height)
        , Html.Events.on "crop-image"
            (Json.Decode.at [ "detail", "image" ] File.decoder
                |> Json.Decode.map GotCroppedImage
            )
        , Html.Events.on "document-resize" (Json.Decode.succeed ImageLoaded)
        ]
        []
    ]


viewSlider : Dimmensions -> Html Msg
viewSlider dimmensions =
    div [ class "flex mt-6 flex-grow-0" ]
        [ button
            [ Html.Events.onClick (ClickedZoomOperation Minus)
            , class "flex-shrink-0 bg-gray-100 p-2 w-10 h-10 rounded-full focus-ring"
            , type_ "button"
            ]
            [ Icons.magnifyingGlassWithMinus ""
            ]

        -- TODO - This doesn't get initialized with the right value
        , input
            [ type_ "range"
            , class "w-full mx-2"
            , value (String.fromFloat dimmensions.selectorBoxSizeMultiplier)
            , Html.Events.onInput ChangedDimmensions
            , Html.Events.on "change" (Json.Decode.succeed CompletedChangingDimmensions)
            , Html.Attributes.min "0.1"
            , Html.Attributes.max (String.fromFloat dimmensions.maximumWidthRatioPossible)
            , Html.Attributes.step "0.001"
            ]
            []
        , button
            [ Html.Events.onClick (ClickedZoomOperation Plus)
            , class "flex-shrink-0 bg-gray-100 p-2 w-10 h-10 rounded-full focus-ring"
            , type_ "button"
            ]
            [ Icons.magnifyingGlassWithPlus ""
            ]
        ]


entireImageId : String
entireImageId =
    "image-cropper-bg-image"


imageContainerId : String
imageContainerId =
    "image-cropper-image-container"


calculateSelectionDimmensions : Model -> Dimmensions -> { width : Float, height : Float }
calculateSelectionDimmensions model dimmensions =
    let
        calculatedSelectionWidth =
            dimmensions.container.width * dimmensions.selectorBoxSizeMultiplier

        calculatedSelectionHeight =
            calculatedSelectionWidth / model.aspectRatio

        selectionWidth =
            if calculatedSelectionHeight > dimmensions.container.height then
                dimmensions.container.height * model.aspectRatio

            else
                calculatedSelectionWidth
    in
    { width = selectionWidth
    , height = selectionWidth / model.aspectRatio
    }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        ImageLoaded ->
            [ "ImageLoaded" ]

        GotImageDimmensions r ->
            [ "GotImageDimmensions", UR.resultToString r ]

        StartedDragging ->
            [ "StartedDragging" ]

        Dragged _ ->
            [ "Dragged" ]

        StoppedDragging ->
            [ "StoppedDragging" ]

        ChangedDimmensions _ ->
            [ "ChangedDimmensions" ]

        CompletedChangingDimmensions ->
            [ "CompletedChangingDimmensions" ]

        ClickedZoomOperation _ ->
            [ "ClickedZoomOperation" ]

        GotCroppedImage _ ->
            [ "GotCroppedImage" ]
