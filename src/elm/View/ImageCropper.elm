module View.ImageCropper exposing (Model, Msg, init, msgToString, update, view)

import Browser.Dom
import Html exposing (Html, div, img)
import Html.Attributes exposing (alt, class, id, src, style)
import Html.Events
import Json.Decode
import Task
import UpdateResult as UR
import View.Components


type alias Model =
    { topOffset : Float
    , leftOffset : Float
    , isDragging : Bool
    , minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    }


init : Model
init =
    -- TODO - Start in the middle
    { topOffset = 0
    , leftOffset = 0
    , isDragging = False

    -- TODO - Figure this out better
    , minX = 0
    , maxX = 0
    , minY = 0
    , maxY = 0
    }


type Msg
    = ImageLoaded
    | GotImageDimmensions (Result Browser.Dom.Error Browser.Dom.Element)
    | StartedDragging
    | StoppedDragging
    | Dragged { x : Float, y : Float }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageLoaded ->
            ( model
            , Browser.Dom.getElement entireImageId
                |> Task.attempt GotImageDimmensions
            )

        GotImageDimmensions (Ok { element }) ->
            ( { model
                | minX = element.x
                , maxX = element.x + element.width
                , minY = element.y
                , maxY = element.y + element.height
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


view : Model -> { imageUrl : String } -> Html Msg
view model { imageUrl } =
    let
        selectionWidth =
            160

        selectionHeight =
            80

        leftOffset =
            clamp 0
                (model.maxX - model.minX - selectionWidth)
                (model.leftOffset - (selectionWidth / 2) - model.minX)

        topOffset =
            clamp 0
                (model.maxY - model.minY - selectionHeight)
                (model.topOffset - (selectionHeight / 2) - model.minY)

        floatToPx offset =
            String.fromFloat offset ++ "px"
    in
    div [ class "relative" ]
        [ img
            [ src imageUrl
            , alt ""
            , id entireImageId

            -- TODO - Do we need to/Should we keep hardcoded height?
            , class "opacity-40 pointer-events-none select-none h-96"
            , Html.Events.on "load" (Json.Decode.succeed ImageLoaded)
            ]
            []
        , div
            [ class "absolute overflow-hidden border border-dashed border-gray-500 cursor-move z-20 select-none"
            , style "top" (floatToPx topOffset)
            , style "left" (floatToPx leftOffset)
            , style "width" (floatToPx selectionWidth)
            , style "height" (floatToPx selectionHeight)
            , onPointerDown StartedDragging
            ]
            [ img
                [ src imageUrl
                , alt ""
                , class "absolute object-cover max-w-none pointer-events-none select-none h-96"
                , style "width" (floatToPx (model.maxX - model.minX))
                , style "height" (floatToPx (model.maxY - model.minY))

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
        ]


entireImageId : String
entireImageId =
    "image-cropper-bg-image"


onPointerDown : msg -> Html.Attribute msg
onPointerDown msg =
    Html.Events.on "pointerdown"
        (Json.Decode.succeed msg)


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
