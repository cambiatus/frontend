module View.ImageCropper exposing (Model, Msg, init, msgToString, update, view)

import Browser.Dom
import File
import Html exposing (Html, button, div, img, input, node)
import Html.Attributes exposing (alt, attribute, class, classList, id, src, style, type_, value)
import Html.Events
import Icons
import Json.Decode
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
    { left : Float
    , width : Float
    , top : Float
    , height : Float
    , maximumWidthRatioPossible : Float
    , leftOffset : Float
    , topOffset : Float
    , selectorBoxSizeMultiplier : Float
    }


type Msg
    = ImageLoaded
    | GotImageDimmensions (Result Browser.Dom.Error Browser.Dom.Element)
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


type alias UpdateResult extMsg =
    UR.UpdateResult Model Msg extMsg


update : Msg -> Model -> UpdateResult extMsg
update msg model =
    case msg of
        ImageLoaded ->
            model
                |> UR.init
                |> UR.addCmd
                    (Browser.Dom.getElement entireImageId
                        |> Task.attempt GotImageDimmensions
                    )

        GotImageDimmensions (Ok { element }) ->
            let
                maximumWidthPossible =
                    min element.width (element.height * model.aspectRatio)

                newDimmensions =
                    { left = element.x
                    , width = element.width
                    , top = element.y
                    , height = element.height
                    , maximumWidthRatioPossible = maximumWidthPossible / element.width
                    , leftOffset = element.x + (element.width / 2)
                    , topOffset = element.y + (element.height / 2)
                    , selectorBoxSizeMultiplier = (maximumWidthPossible / element.width) * 0.75
                    }
            in
            { model
                | dimmensions =
                    case model.dimmensions of
                        Loading ->
                            Loaded newDimmensions

                        Loaded existingDimmensions ->
                            if existingDimmensions.width == element.width && existingDimmensions.height == element.height then
                                Loaded
                                    { newDimmensions
                                        | selectorBoxSizeMultiplier = existingDimmensions.selectorBoxSizeMultiplier
                                        , leftOffset = existingDimmensions.leftOffset + newDimmensions.left - existingDimmensions.left
                                        , topOffset = existingDimmensions.topOffset + newDimmensions.top - existingDimmensions.top
                                    }

                            else
                                Loaded newDimmensions
                , isReflowing =
                    case model.dimmensions of
                        Loading ->
                            False

                        Loaded existingDimmensions ->
                            (abs (existingDimmensions.width - newDimmensions.width) > 50)
                                || (abs (existingDimmensions.height - newDimmensions.height) > 50)
            }
                |> UR.init

        GotImageDimmensions (Err _) ->
            -- TODO - Log something?
            UR.init model

        StartedDragging ->
            { model | isDragging = True }
                -- TODO - Create port to add `cursor: move` to html, body
                |> UR.init

        StoppedDragging ->
            { model
                | isDragging = False
                , isRequestingCroppedImage = True
            }
                -- TODO - Create port to remove `cursor: move` from html, body
                |> UR.init

        Dragged { x, y, previousX, previousY } ->
            case model.dimmensions of
                Loading ->
                    UR.init model

                Loaded dimmensions ->
                    let
                        selection =
                            calculateSelectionDimmensions model dimmensions
                    in
                    { model
                        | dimmensions =
                            Loaded
                                { dimmensions
                                    | topOffset =
                                        clamp dimmensions.top
                                            (dimmensions.top + dimmensions.height - selection.height)
                                            (dimmensions.topOffset + y - previousY)
                                    , leftOffset =
                                        clamp dimmensions.left
                                            (dimmensions.left + dimmensions.width - selection.width)
                                            (dimmensions.leftOffset + x - previousX)
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
            -- TODO - Do something with the file
            { model | isRequestingCroppedImage = False }
                |> UR.init


view : Model -> { imageUrl : String } -> Html Msg
view model { imageUrl } =
    div [ class "mx-auto w-full md:flex md:flex-col md:w-auto" ]
        [ div [ class "relative max-w-max mx-auto" ]
            (img
                [ src imageUrl
                , alt ""
                , id entireImageId
                , class "opacity-20 pointer-events-none select-none max-h-64 lg:max-h-96"
                , Html.Events.on "load" (Json.Decode.succeed ImageLoaded)
                ]
                []
                :: (case model.dimmensions of
                        Loading ->
                            []

                        Loaded dimmensions ->
                            viewCropper model dimmensions { imageUrl = imageUrl }
                   )
            )
        , case model.dimmensions of
            Loading ->
                Html.text ""

            Loaded dimmensions ->
                viewSlider dimmensions
        ]


viewCropper : Model -> Dimmensions -> { imageUrl : String } -> List (Html Msg)
viewCropper model dimmensions { imageUrl } =
    let
        selection =
            calculateSelectionDimmensions model dimmensions

        leftOffset =
            clamp 0
                (dimmensions.width - selection.width)
                (dimmensions.leftOffset - dimmensions.left)

        topOffset =
            clamp 0
                (dimmensions.height - selection.height)
                (dimmensions.topOffset - dimmensions.top)

        floatToPx offset =
            String.fromFloat offset ++ "px"
    in
    [ div
        [ class "absolute overflow-hidden border border-dashed border-gray-400 cursor-move z-20 select-none mx-auto"
        , classList [ ( "transition-all origin-center", not model.isDragging && not model.isChangingDimmensions && not model.isReflowing ) ]
        , style "top" (floatToPx topOffset)
        , style "left" (floatToPx leftOffset)
        , style "width" (floatToPx selection.width)
        , style "height" (floatToPx selection.height)
        , onPointerDown StartedDragging
        ]
        [ img
            [ src imageUrl
            , alt ""
            , class "absolute object-cover max-w-none pointer-events-none select-none"
            , classList [ ( "transition-all origin-center", not model.isDragging && not model.isChangingDimmensions && not model.isReflowing ) ]
            , style "width" (floatToPx dimmensions.width)
            , style "height" (floatToPx dimmensions.height)

            -- We need to compensate for the border
            , style "top" (floatToPx (-topOffset - 1))
            , style "left" (floatToPx (-leftOffset - 1))
            ]
            []
        ]
    , if model.isDragging then
        View.Components.pointerListener
            { onPointerMove = Just Dragged
            , onPointerUp = Just StoppedDragging
            }

      else
        Html.text ""
    , node "image-cropper"
        [ if model.isRequestingCroppedImage then
            attribute "elm-generate-new-cropped-image" "true"

          else
            class ""
        , attribute "elm-url" imageUrl
        , attribute "elm-image-width" (String.fromFloat dimmensions.width)
        , attribute "elm-image-height" (String.fromFloat dimmensions.height)
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
            ]
            [ Icons.magnifyingGlassWithMinus ""
            ]

        -- TODO - Are we using this or rolling our own slider?
        -- TODO - Style it better
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
            ]
            [ Icons.magnifyingGlassWithPlus ""
            ]
        ]


entireImageId : String
entireImageId =
    "image-cropper-bg-image"


calculateSelectionDimmensions : Model -> Dimmensions -> { width : Float, height : Float }
calculateSelectionDimmensions model dimmensions =
    let
        calculatedSelectionWidth =
            dimmensions.width * dimmensions.selectorBoxSizeMultiplier

        calculatedSelectionHeight =
            calculatedSelectionWidth / model.aspectRatio

        selectionWidth =
            if calculatedSelectionHeight > dimmensions.height then
                dimmensions.height * model.aspectRatio

            else
                calculatedSelectionWidth
    in
    { width = selectionWidth
    , height = selectionWidth / model.aspectRatio
    }


onPointerDown : msg -> Html.Attribute msg
onPointerDown msg =
    Html.Events.custom "pointerdown"
        (Json.Decode.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )


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
            [ "GotCroppedImageUrl" ]
