module View.Select.Update exposing (update)

import Browser.Dom
import Task
import View.Select.Config exposing (Config)
import View.Select.Messages exposing (Msg(..))
import View.Select.Models exposing (State)


update : Config msg item -> Msg item -> State -> ( State, Cmd msg )
update config msg model =
    let
        queryChangeCmd value =
            case config.onQueryChange of
                Nothing ->
                    Cmd.none

                Just constructor ->
                    Task.succeed value
                        |> Task.perform constructor
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnEsc ->
            ( { model | showMenu = False }, Cmd.none )

        OnDownArrow ->
            let
                newHightlightedItem =
                    case model.highlightedItem of
                        Nothing ->
                            0

                        Just n ->
                            n + 1
            in
            ( { model | highlightedItem = Just newHightlightedItem }
            , focusItem
                { inputId = config.inputId
                , itemIndex = newHightlightedItem
                }
                config.onFocusItem
            )

        OnUpArrow ->
            let
                newHightlightedItem =
                    case model.highlightedItem of
                        Nothing ->
                            Nothing

                        Just 0 ->
                            Nothing

                        Just n ->
                            Just (n - 1)
            in
            ( { model | highlightedItem = newHightlightedItem }
            , case newHightlightedItem of
                Nothing ->
                    Browser.Dom.focus config.inputId
                        |> Task.attempt (\_ -> Maybe.withDefault config.onFocusItem config.onFocus)

                Just highlightedItem ->
                    focusItem
                        { inputId = config.inputId
                        , itemIndex = highlightedItem
                        }
                        config.onFocusItem
            )

        OnResetFocusToFirstItem ->
            ( { model | highlightedItem = Just 0 }
            , focusItem { inputId = config.inputId, itemIndex = 0 } config.onFocusItem
            )

        OnFocus ->
            let
                cmd =
                    case config.onFocus of
                        Nothing ->
                            Cmd.none

                        Just focusMessage ->
                            Task.succeed Nothing
                                |> Task.perform (\_ -> focusMessage)
            in
            if config.emptySearch then
                ( { model | query = Just "", showMenu = True }
                , Cmd.batch
                    [ cmd
                    , queryChangeCmd ""
                    ]
                )

            else
                ( { model | showMenu = True }, cmd )

        OnBlur ->
            let
                cmd =
                    case config.onBlur of
                        Nothing ->
                            Cmd.none

                        Just blurMessage ->
                            Task.succeed Nothing
                                |> Task.perform (\_ -> blurMessage)
            in
            ( { model | showMenu = False }, cmd )

        OnRemoveItem item ->
            let
                cmd =
                    case config.onRemoveItem of
                        Just onRemoveItem ->
                            Task.succeed item
                                |> Task.perform onRemoveItem

                        Nothing ->
                            Cmd.none
            in
            ( model, cmd )

        OnQueryChange value ->
            let
                newQuery =
                    value |> config.transformQuery

                cmd =
                    case newQuery of
                        "" ->
                            Cmd.none

                        _ ->
                            queryChangeCmd newQuery
            in
            ( { model | highlightedItem = Nothing, query = Just value }, cmd )

        OnSelect item ->
            let
                cmd =
                    Task.succeed item
                        |> Task.perform config.onSelect

                blurCmd =
                    case config.onBlur of
                        Nothing ->
                            Cmd.none

                        Just blurMessage ->
                            Task.succeed Nothing
                                |> Task.perform (\_ -> blurMessage)
            in
            ( { model | query = Nothing }, Cmd.batch [ cmd, blurCmd ] )


focusItem : { inputId : String, itemIndex : Int } -> msg -> Cmd msg
focusItem { inputId, itemIndex } onFocusItem =
    Browser.Dom.focus (inputId ++ "-menu-item-" ++ String.fromInt itemIndex)
        |> Task.attempt (\_ -> onFocusItem)
