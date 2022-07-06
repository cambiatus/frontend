module Dnd exposing (ExtMsg(..), Model, Msg, draggable, dropZone, getDraggingElement, getDraggingOverElement, init, msgToString, update)

import Html
import Html.Attributes
import Html.Events
import Json.Decode
import UpdateResult as UR


type Model draggable dropZone
    = Model
        { draggingElement : Maybe draggable
        , draggingOverElement : Maybe dropZone
        }


init : Model draggable dropZone
init =
    Model
        { draggingElement = Nothing
        , draggingOverElement = Nothing
        }


type Msg draggable dropZone
    = StartedDragging draggable
    | StoppedDragging
    | DroppedOn dropZone
    | DragLeft dropZone
    | DraggedOverInternal dropZone


type alias UpdateResult draggable dropZone =
    UR.UpdateResult (Model draggable dropZone) (Msg draggable dropZone) (ExtMsg draggable dropZone)


type ExtMsg draggable dropZone
    = Dropped { draggedElement : draggable, dropZone : dropZone }
    | DraggedOver dropZone


update : Msg draggable dropZone -> Model draggable dropZone -> UpdateResult draggable dropZone
update msg (Model model) =
    case msg of
        StartedDragging elementId ->
            { model | draggingElement = Just elementId }
                |> Model
                |> UR.init

        StoppedDragging ->
            { model
                | draggingElement = Nothing
                , draggingOverElement = Nothing
            }
                |> Model
                |> UR.init

        DroppedOn elementId ->
            case model.draggingElement of
                Nothing ->
                    model
                        |> Model
                        |> UR.init

                Just draggingElement ->
                    { model
                        | draggingElement = Nothing
                        , draggingOverElement = Nothing
                    }
                        |> Model
                        |> UR.init
                        |> UR.addExt (Dropped { draggedElement = draggingElement, dropZone = elementId })

        DragLeft dropZone_ ->
            { model
                | draggingOverElement =
                    if model.draggingOverElement == Just dropZone_ then
                        Nothing

                    else
                        model.draggingOverElement
            }
                |> Model
                |> UR.init

        DraggedOverInternal dropZone_ ->
            let
                fireDraggedOver =
                    if model.draggingOverElement == Just dropZone_ then
                        identity

                    else
                        UR.addExt (DraggedOver dropZone_)
            in
            { model | draggingOverElement = Just dropZone_ }
                |> Model
                |> UR.init
                |> fireDraggedOver



-- DRAGGABLE ATTRIBUTES


onDragStart : draggable -> Html.Attribute (Msg draggable dropZone)
onDragStart draggableId =
    Html.Events.on "dragstart" (Json.Decode.succeed (StartedDragging draggableId))


onDragEnd : Html.Attribute (Msg draggable dropZone)
onDragEnd =
    Html.Events.on "dragend" (Json.Decode.succeed StoppedDragging)


draggableAttr : Html.Attribute (Msg draggable dropZone)
draggableAttr =
    Html.Attributes.draggable "true"


draggable : draggable -> (Msg draggable dropZone -> msg) -> List (Html.Attribute msg)
draggable draggableId toMsg =
    [ onDragStart draggableId
    , onDragEnd
    , draggableAttr
    ]
        |> List.map (Html.Attributes.map toMsg)



-- DROP ZONE ATTRIBUTES


onDrop : dropZone -> Html.Attribute (Msg draggable dropZone)
onDrop dropZoneId =
    Html.Events.custom "drop"
        (Json.Decode.succeed
            { message = DroppedOn dropZoneId
            , stopPropagation = True
            , preventDefault = True
            }
        )


onDragOver : dropZone -> Html.Attribute (Msg draggable dropZone)
onDragOver dropZoneId =
    Html.Events.custom "dragover"
        (Json.Decode.succeed
            { message = DraggedOverInternal dropZoneId
            , stopPropagation = True
            , preventDefault = True
            }
        )


onDragLeave : dropZone -> Html.Attribute (Msg draggable dropZone)
onDragLeave dropZoneId =
    Html.Events.preventDefaultOn "dragleave" (Json.Decode.succeed ( DragLeft dropZoneId, True ))


dropZone : dropZone -> (Msg draggable dropZone -> msg) -> List (Html.Attribute msg)
dropZone dropZoneId toMsg =
    [ onDrop dropZoneId
    , onDragOver dropZoneId
    , onDragLeave dropZoneId
    ]
        |> List.map (Html.Attributes.map toMsg)



-- GETTERS


getDraggingElement : Model draggable dropZone -> Maybe draggable
getDraggingElement (Model model) =
    model.draggingElement


getDraggingOverElement : Model draggable dropZone -> Maybe dropZone
getDraggingOverElement (Model model) =
    model.draggingOverElement



-- UTILS


msgToString : Msg draggable dropZone -> List String
msgToString msg =
    case msg of
        StartedDragging _ ->
            [ "StartedDragging" ]

        StoppedDragging ->
            [ "StoppedDragging" ]

        DroppedOn _ ->
            [ "DroppedOn" ]

        DragLeft _ ->
            [ "DragLeft" ]

        DraggedOverInternal _ ->
            [ "DraggedOverInternal" ]
