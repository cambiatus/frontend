module View.ImageCropper exposing (Model, Msg, init, msgToString, update, view)

import Browser.Dom
import Html exposing (Html, div, img, input)
import Html.Attributes exposing (alt, class, id, src, style, type_, value)
import Html.Events
import Icons
import Json.Decode
import Task
import UpdateResult as UR
import View.Components


type alias Model =
    { topOffset : Float
    , leftOffset : Float
    , isDragging : Bool
    , aspectRatio : Float
    , left : Float
    , width : Float
    , top : Float
    , height : Float
    , maximumWidthRatioPossible : Float
    , selectorBoxSizeMultiplier : Float
    }


init : Model
init =
    -- TODO - Start in the middle
    { topOffset = 0
    , leftOffset = 0
    , isDragging = False
    , aspectRatio = 1 / 1
    , top = 0
    , width = 0
    , left = 0
    , height = 0
    , maximumWidthRatioPossible = 0
    , selectorBoxSizeMultiplier = 0
    }


type Msg
    = ImageLoaded
    | GotImageDimmensions (Result Browser.Dom.Error Browser.Dom.Element)
    | StartedDragging
    | StoppedDragging
    | Dragged { x : Float, y : Float }
    | ChangedDimmensions String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageLoaded ->
            ( model
            , Browser.Dom.getElement entireImageId
                |> Task.attempt GotImageDimmensions
            )

        GotImageDimmensions (Ok { element }) ->
            let
                maximumWidthPossible =
                    min element.width (element.height * model.aspectRatio)
            in
            ( { model
                | left = element.x
                , width = element.width
                , top = element.y
                , height = element.height
                , maximumWidthRatioPossible = maximumWidthPossible / element.width
                , leftOffset = element.x + (element.width / 2)
                , topOffset = element.y + (element.height / 2)
                , selectorBoxSizeMultiplier = (maximumWidthPossible / element.width) * 0.75
              }
            , Cmd.none
            )

        GotImageDimmensions _ ->
            ( model, Cmd.none )

        StartedDragging ->
            ( { model | isDragging = True }
            , Cmd.none
            )

        StoppedDragging ->
            ( { model | isDragging = False }
            , Cmd.none
            )

        Dragged { x, y } ->
            ( { model | topOffset = y, leftOffset = x }
            , Cmd.none
            )

        ChangedDimmensions percentageString ->
            case String.toFloat percentageString of
                Nothing ->
                    ( model, Cmd.none )

                Just percentageValue ->
                    ( { model | selectorBoxSizeMultiplier = percentageValue }
                    , Cmd.none
                    )


view : Model -> { imageUrl : String } -> Html Msg
view model { imageUrl } =
    let
        calculatedSelectionWidth =
            model.width * model.selectorBoxSizeMultiplier

        calculatedSelectionHeight =
            calculatedSelectionWidth / model.aspectRatio

        selectionWidth =
            if calculatedSelectionHeight > model.height then
                model.height * model.aspectRatio

            else
                calculatedSelectionWidth

        selectionHeight =
            selectionWidth / model.aspectRatio

        leftOffset =
            clamp 0
                (model.width - selectionWidth)
                (model.leftOffset - (selectionWidth / 2) - model.left)

        topOffset =
            clamp 0
                (model.height - selectionHeight)
                (model.topOffset - (selectionHeight / 2) - model.top)

        floatToPx offset =
            String.fromFloat offset ++ "px"
    in
    div [ class "relative max-w-max mx-auto" ]
        [ img
            [ src imageUrl
            , alt ""
            , id entireImageId
            , class "opacity-20 pointer-events-none select-none max-h-64 lg:max-h-96"
            , Html.Events.on "load" (Json.Decode.succeed ImageLoaded)
            ]
            []
        , div
            [ class "absolute overflow-hidden border border-dashed border-gray-400 cursor-move z-20 select-none"
            , style "top" (floatToPx topOffset)
            , style "left" (floatToPx leftOffset)
            , style "width" (floatToPx selectionWidth)
            , style "height" (floatToPx selectionHeight)
            , onPointerDown StartedDragging
            ]
            [ img
                [ src imageUrl
                , alt ""
                , class "absolute object-cover max-w-none pointer-events-none select-none"
                , style "width" (floatToPx model.width)
                , style "height" (floatToPx model.height)

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
        , div [ class "flex mt-6" ]
            -- TODO - Make this into a button
            [ Icons.magnifyingGlassWithMinus "flex-shrink-0 bg-gray-100 p-2 w-10 h-10 rounded-full"

            -- TODO - Are we using this or rolling our own slider?
            -- TODO - Style it better
            , input
                [ type_ "range"
                , class "w-full mx-2"
                , value (String.fromFloat model.selectorBoxSizeMultiplier)
                , Html.Events.onInput ChangedDimmensions
                , Html.Attributes.min "0.1"
                , Html.Attributes.max (String.fromFloat model.maximumWidthRatioPossible)
                , Html.Attributes.step "0.001"
                ]
                []

            -- TODO - Make this into a button
            , Icons.magnifyingGlassWithPlus "flex-shrink-0 bg-gray-100 p-2 w-10 h-10 rounded-full"
            ]
        ]


entireImageId : String
entireImageId =
    "image-cropper-bg-image"


onPointerDown : msg -> Html.Attribute msg
onPointerDown msg =
    Html.Events.preventDefaultOn "pointerdown"
        (Json.Decode.succeed ( msg, True ))


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
