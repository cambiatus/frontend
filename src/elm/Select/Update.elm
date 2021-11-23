module Select.Update exposing (update)

import Select.Config exposing (Config)
import Select.Messages exposing (Msg(..))
import Select.Models exposing (State)
import Task


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
                            Just 0

                        Just n ->
                            Just (n + 1)
            in
            ( { model | highlightedItem = newHightlightedItem }, Cmd.none )

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
            ( { model | highlightedItem = newHightlightedItem }, Cmd.none )

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
                    , if config.emptySearch then
                        queryChangeCmd ""

                      else
                        Cmd.none
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
            in
            ( { model | query = Nothing }, cmd )
