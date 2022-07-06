module Form.DatePicker exposing
    ( init, Options, map
    , withDisabled, withAbsolutePositioning, withContainerAttrs
    , getId, getDate
    , view, ViewConfig, mapViewConfig
    , Model, initModel, update, Msg, msgToString
    )

{-| Creates a Cambiatus-style DatePicker. Use it within a `Form.Form`:

    Form.DatePicker.init
        { label = text "Transfer date"
        , id = "transfer-date-picker"
        }


# Initializing

@docs init, Options, map


# Helpers


## Adding attributes

@docs withDisabled, withAbsolutePositioning, withContainerAttrs


# Getters

@docs getId, getDate


# View

@docs view, ViewConfig, mapViewConfig


# The elm architecture

This is how you actually use this component!

@docs Model, initModel, update, Msg, msgToString

-}

import Browser.Dom
import Date exposing (Date)
import DatePicker exposing (DatePicker)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, disabled, tabindex, type_)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import Icons
import Maybe.Extra
import Session.Shared as Shared
import Task
import View.Components exposing (Key(..))



-- OPTIONS


type Options msg
    = Options
        { label : String
        , id : String
        , disabled : Bool
        , absolutePositioning : Bool
        , containerAttrs : List (Html.Attribute msg)
        }


{-| Initializes a DatePicker
-}
init : { label : String, id : String } -> Options msg
init { label, id } =
    Options
        { label = label
        , id = id
        , disabled = False
        , absolutePositioning = True
        , containerAttrs = []
        }


{-| Change the kind of `msg` on an Options record
-}
map : (msg -> mappedMsg) -> Options msg -> Options mappedMsg
map fn (Options options) =
    Options
        { label = options.label
        , id = options.id
        , disabled = options.disabled
        , absolutePositioning = options.absolutePositioning
        , containerAttrs = List.map (Html.Attributes.map fn) options.containerAttrs
        }



-- ADDING ATTRIBUTES


{-| Determines if the DatePicker should be disabled
-}
withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


{-| Add attributes to the container that holds the label and the input itself
-}
withContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withContainerAttrs attrs (Options options) =
    Options { options | containerAttrs = options.containerAttrs ++ attrs }


{-| Determines wheter or not to use an `absolute` class on the calendar that is
shown when the input is focused. If `True`, the calendar will not take displace
other elements, and will float above them. By default, the calendar takes space,
and displaces other elements.
-}
withAbsolutePositioning : Bool -> Options msg -> Options msg
withAbsolutePositioning absolutePositioning (Options options) =
    Options { options | absolutePositioning = absolutePositioning }



-- VIEW


type alias ViewConfig msg =
    { value : Model
    , error : Html msg
    , hasError : Bool
    , isRequired : Bool
    , translators : Shared.Translators
    }


{-| Change the kind of `msg` on a ViewConfig record
-}
mapViewConfig : (msg -> mappedMsg) -> ViewConfig msg -> ViewConfig mappedMsg
mapViewConfig fn viewConfig =
    { value = viewConfig.value
    , error = Html.map fn viewConfig.error
    , hasError = viewConfig.hasError
    , isRequired = viewConfig.isRequired
    , translators = viewConfig.translators
    }


settings : Options msg -> ViewConfig msg -> DatePicker.Settings
settings (Options options) viewConfig =
    let
        defaultSettings_ =
            DatePicker.defaultSettings
    in
    { defaultSettings_
        | changeYear = DatePicker.off
        , placeholder = viewConfig.translators.t "dates.select"
        , inputClassList =
            [ ( "input w-full", True )
            , ( "with-error", viewConfig.hasError )
            ]
        , containerClassList = [ ( "relative-table w-full", not options.absolutePositioning ) ]
        , dateFormatter = Date.format "E, d MMM y"
        , inputId = Just options.id

        -- The isDisabled call only disables the dates inside the expanded
        -- calendar. We manually add the disabled attribute on the input so the
        -- calendar can't be open
        , isDisabled = \_ -> options.disabled
        , inputAttributes =
            [ disabled options.disabled
            , Html.Attributes.attribute "inputmode" "none"
            ]
    }


view : Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
view ((Options options) as wrappedOptions) viewConfig toMsg =
    let
        (Model model) =
            viewConfig.value
    in
    div options.containerAttrs
        [ View.Components.label [] { targetId = options.id, labelText = options.label }
        , span [ class "flex" ]
            [ span [ class "relative w-full" ]
                [ DatePicker.view model.selectedDate
                    (settings wrappedOptions viewConfig)
                    model.picker
                    |> Html.map (GotDatePickerMsg { usingArrowKeys = False })
                , button
                    [ class "absolute right-0 top-0 focus:outline-none"
                    , tabindex -1
                    , onClick ClickedCalendarIcon
                    , ariaLabel (viewConfig.translators.t "dates.select")
                    , type_ "button"
                    , disabled options.disabled
                    ]
                    [ Icons.calendar "h-12"
                    ]
                ]
            , if Maybe.Extra.isJust model.selectedDate then
                button
                    [ class "h-12 ml-4 group focus-ring rounded-sm focus-visible:ring-red focus-visible:ring-opacity-30"
                    , onClick ClickedClear
                    , ariaLabel (viewConfig.translators.t "dates.clear")
                    , type_ "button"
                    , disabled options.disabled
                    ]
                    [ Icons.trash "text-red group-hover:opacity-80" ]

              else
                text ""
            ]
            |> Html.map toMsg
        , viewConfig.error
        , if DatePicker.isOpen model.picker then
            View.Components.keyListener
                { acceptedKeys =
                    [ Space
                    , Enter
                    , ArrowUp
                    , ArrowDown
                    , ArrowLeft
                    , ArrowRight
                    ]
                , toMsg = ClickedKey >> toMsg
                , stopPropagation = True
                , preventDefault = True
                }

          else
            text ""
        ]



-- GETTERS


getId : Options msg -> String
getId (Options options) =
    options.id


getDate : Model -> Maybe Date
getDate (Model model) =
    model.selectedDate



-- THE ELM ARCHITECTURE
-- MODEL


type Model
    = Model
        { selectedDate : Maybe Date
        , picker : DatePicker
        }


initModel : Date -> Model
initModel date =
    Model
        { selectedDate = Just date
        , picker = DatePicker.initFromDate date
        }



-- UPDATE


type Msg
    = NoOp
    | GotDatePickerMsg { usingArrowKeys : Bool } DatePicker.Msg
    | ClickedCalendarIcon
    | ClickedClear
    | ClickedKey Key


updateDatePicker : { usingArrowKeys : Bool } -> Options msg -> ViewConfig msg -> DatePicker.Msg -> Model -> ( Model, Cmd Msg )
updateDatePicker { usingArrowKeys } options viewConfig msg (Model model) =
    let
        openWithArrowKeys : Model -> ( Model, Cmd Msg )
        openWithArrowKeys newModel =
            if usingArrowKeys then
                updateDatePicker { usingArrowKeys = False }
                    options
                    viewConfig
                    DatePicker.open
                    newModel

            else
                ( newModel, Cmd.none )
    in
    case DatePicker.update (settings options viewConfig) msg model.picker of
        ( newPicker, DatePicker.Picked newDate ) ->
            { model
                | picker = newPicker
                , selectedDate = Just newDate
            }
                |> Model
                |> openWithArrowKeys

        ( newPicker, _ ) ->
            { model | picker = newPicker }
                |> Model
                |> openWithArrowKeys


update : Options msg -> ViewConfig msg -> Msg -> Model -> ( Model, Cmd Msg )
update ((Options options) as wrappedOptions) viewConfig msg (Model model) =
    case msg of
        NoOp ->
            ( Model model, Cmd.none )

        GotDatePickerMsg usingArrowKeys subMsg ->
            updateDatePicker usingArrowKeys
                wrappedOptions
                viewConfig
                subMsg
                (Model model)

        ClickedCalendarIcon ->
            ( Model model
            , Browser.Dom.focus options.id
                |> Task.attempt (\_ -> NoOp)
            )

        ClickedClear ->
            ( Model { model | selectedDate = Nothing }
            , Cmd.none
            )

        ClickedKey key ->
            let
                addDays : Int -> ( Model, Cmd Msg )
                addDays amount =
                    case model.selectedDate of
                        Nothing ->
                            ( Model model, Cmd.none )

                        Just selectedDate ->
                            updateDatePicker { usingArrowKeys = True }
                                wrappedOptions
                                viewConfig
                                (DatePicker.pick (Date.add Date.Days amount selectedDate))
                                (Model model)

                pickOrOpen : ( Model, Cmd Msg )
                pickOrOpen =
                    if DatePicker.isOpen model.picker then
                        case model.selectedDate of
                            Nothing ->
                                ( Model model, Cmd.none )

                            Just selectedDate ->
                                updateDatePicker { usingArrowKeys = False }
                                    wrappedOptions
                                    viewConfig
                                    (DatePicker.pick selectedDate)
                                    (Model model)

                    else
                        updateDatePicker { usingArrowKeys = False }
                            wrappedOptions
                            viewConfig
                            DatePicker.open
                            (Model model)
            in
            case key of
                Space ->
                    pickOrOpen

                Enter ->
                    pickOrOpen

                ArrowUp ->
                    addDays -7

                ArrowDown ->
                    addDays 7

                ArrowLeft ->
                    addDays -1

                ArrowRight ->
                    addDays 1

                _ ->
                    ( Model model, Cmd.none )


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        GotDatePickerMsg _ _ ->
            [ "GotDatePickerMsg" ]

        ClickedCalendarIcon ->
            [ "ClickedCalendarIcon" ]

        ClickedClear ->
            [ "ClickedClear" ]

        ClickedKey _ ->
            [ "ClickedKey" ]
