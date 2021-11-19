module Form.DatePicker exposing
    ( init, Options
    , withDisabled
    , getId
    , view
    , Model, Msg, getDate, initModel, msgToString, update, withAbsolutePositioning
    )

{-| Creates a Cambiatus-style DatePicker. Use it within a `Form.Form`:

    Form.DatePicker.init
        { label = text "Transfer date"
        , id = "transfer-date-picker"
        }


# Initializing

@docs init, Options


# Helpers


## Adding attributes

@docs withDisabled


# Getters

@docs getId


# View

@docs view

-}

import Browser.Dom
import Date exposing (Date)
import DatePicker exposing (DatePicker)
import Html exposing (Html, button, div, label, span, text)
import Html.Attributes exposing (class, disabled, tabindex)
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
        }



-- ADDING ATTRIBUTES


{-| Determines if the DatePicker should be disabled
-}
withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


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


settings : Options msg -> DatePicker.Settings
settings (Options options) =
    let
        defaultSettings_ =
            DatePicker.defaultSettings
    in
    { defaultSettings_
        | changeYear = DatePicker.off
        , inputClassList = [ ( "input w-full", True ) ]
        , containerClassList = [ ( "relative-table w-full", not options.absolutePositioning ) ]
        , dateFormatter = Date.format "E, d MMM y"
        , inputId = Just options.id
        , isDisabled = \_ -> options.disabled
    }


view : Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
view options viewConfig toMsg =
    let
        (Model model) =
            viewConfig.value
    in
    div []
        [ span [ class "flex" ]
            [ span [ class "relative w-full" ]
                [ DatePicker.view model.selectedDate
                    (settings options)
                    model.picker
                    |> Html.map (GotDatePickerMsg { usingArrowKeys = False })
                , button
                    [ class "absolute right-0 top-0 focus:outline-none"
                    , tabindex -1
                    , onClick ClickedCalendarIcon

                    -- TODO - I18N
                    , ariaLabel "Select a date"
                    ]
                    [ Icons.calendar "h-12"
                    ]
                ]
            , if Maybe.Extra.isJust model.selectedDate then
                button
                    [ class "h-12 ml-4 group focus-ring rounded-sm focus-visible:ring-red focus-visible:ring-opacity-30"
                    , onClick ClickedClear

                    -- TODO - I18N
                    , ariaLabel "Clear date"
                    ]
                    [ Icons.trash "group-hover:opacity-80" ]

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


updateDatePicker : { usingArrowKeys : Bool } -> Options msg -> DatePicker.Msg -> Model -> ( Model, Cmd Msg )
updateDatePicker { usingArrowKeys } options msg (Model model) =
    let
        openWithArrowKeys : Model -> ( Model, Cmd Msg )
        openWithArrowKeys newModel =
            if usingArrowKeys then
                updateDatePicker { usingArrowKeys = False }
                    options
                    DatePicker.open
                    newModel

            else
                ( newModel, Cmd.none )
    in
    case DatePicker.update (settings options) msg model.picker of
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


update : Options msg -> Msg -> Model -> ( Model, Cmd Msg )
update ((Options options) as wrappedOptions) msg (Model model) =
    case msg of
        NoOp ->
            ( Model model, Cmd.none )

        GotDatePickerMsg usingArrowKeys subMsg ->
            updateDatePicker usingArrowKeys wrappedOptions subMsg (Model model)

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
                                    (DatePicker.pick selectedDate)
                                    (Model model)

                    else
                        updateDatePicker { usingArrowKeys = False }
                            wrappedOptions
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
