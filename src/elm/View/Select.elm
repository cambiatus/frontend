module View.Select exposing
    ( RequiredConfig, Config, State, Msg
    , newConfig
    , withInputClass, withInputClassList, withOnBlur
    , withItemClass, withItemHtml
    , withMenuClass, withEmptyMenuClass
    , withNotFound, withNotFoundClass
    , withPrompt
    , newState, queryFromState
    , view
    , update
    , withDisabled
    )

{-| Select input with auto-complete

See a full example of the select input [here](https://github.com/sporto/elm-select/tree/master/demo/src/Example1.elm)

See a full example of the select input in multi mode [here](https://github.com/sporto/elm-select/tree/master/demo/src/Example3.elm)


# Types

@docs RequiredConfig, Config, State, Msg


# Configuration

@docs newConfig


# Configure the input

@docs withInputClass, withInputClassList, withOnBlur


# Configure the items

@docs withItemClass, withItemHtml


# Configure the menu

@docs withMenuClass, withEmptyMenuClass


# Configure the not found message

@docs withNotFound, withNotFoundClass


# Configure the prompt

@docs withPrompt


# State

@docs newState, queryFromState


# View

@docs view


# Update

@docs update

-}

import Html exposing (Html)
import View.Select.Config as Config
import View.Select.Internal
import View.Select.Messages as Messages
import View.Select.Models as Models
import View.Select.Update


{-| Required initial configuration

  - onSelect: A message to trigger when an item is selected
  - toLabel: A function to get a label to display from an item
  - filter: A function that takes the typed query and the list of all items, and return the filtered items.

-}
type alias RequiredConfig msg item =
    { onSelect : item -> msg
    , toLabel : item -> String
    , filter : String -> List item -> Maybe (List item)
    , onFocusItem : msg
    }


{-| Opaque type that holds all the configuration
-}
type Config msg item
    = PrivateConfig (Config.Config msg item)


{-| Opaque type that holds the current state
-}
type State
    = PrivateState Models.State


{-| Opaque type for internal library messages
-}
type Msg item
    = PrivateMsg (Messages.Msg item)


{-| Create a new configuration. This takes as `RequiredConfig` record.
-}
newConfig : RequiredConfig msg item -> Config msg item
newConfig requiredConfig =
    Config.newConfig requiredConfig
        |> PrivateConfig


{-| Add classes to the input

    Select.withInputClass "col-12" config

-}
withInputClass : String -> Config msg item -> Config msg item
withInputClass classes config =
    let
        fn c =
            { c | inputClass = classes }
    in
    mapConfig fn config


{-| Add conditional classes to the input

    Select.withInputClassList [ ( "with-error", hasError ) ] config

-}
withInputClassList : List ( String, Bool ) -> Config msg item -> Config msg item
withInputClassList classes config =
    let
        fn c =
            { c | inputClassList = classes }
    in
    mapConfig fn config


{-| Add classes to the items

    Select.withItemClass "border-bottom" config

-}
withItemClass : String -> Config msg item -> Config msg item
withItemClass classes config =
    let
        fn c =
            { c | itemClass = classes }
    in
    mapConfig fn config


{-| Custom item element HTML

    Select.withItemHtml (\i -> Html.li [] [ text i ]) config

When this is used the original `toLabel` function in the config is ignored.

-}
withItemHtml : (item -> Html Never) -> Config msg item -> Config msg item
withItemHtml html config =
    let
        fn c =
            { c | itemHtml = Just html }
    in
    mapConfig fn config


{-| Add classes to the menu

    Select.withMenuClass "bg-white" config

-}
withMenuClass : String -> Config msg item -> Config msg item
withMenuClass classes config =
    let
        fn c =
            { c | menuClass = String.join " " [ c.menuClass, classes ] }
    in
    mapConfig fn config


{-| Add classes to the menu when there are no matches

    Select.withEmptyMenuClass "bg-white" config

-}
withEmptyMenuClass : String -> Config msg item -> Config msg item
withEmptyMenuClass classes config =
    let
        fn c =
            { c | emptyMenuClass = String.join " " [ c.emptyMenuClass, classes ] }
    in
    mapConfig fn config


{-| Text that will appear when no matches are found

    Select.withNotFound "No matches" config

-}
withNotFound : String -> Config msg item -> Config msg item
withNotFound text config =
    let
        fn c =
            { c | notFound = text }
    in
    mapConfig fn config


{-| Class for the not found message

    Select.withNotFoundClass "red" config

-}
withNotFoundClass : String -> Config msg item -> Config msg item
withNotFoundClass class config =
    let
        fn c =
            { c | notFoundClass = class }
    in
    mapConfig fn config


{-| Add a callback for when the input field loses focus

    Select.withOnBlur OnBlur

-}
withOnBlur : msg -> Config msg item -> Config msg item
withOnBlur msg config =
    let
        fn c =
            { c | onBlur = Just msg }
    in
    mapConfig fn config


{-| Add a prompt text to be displayed when no element is selected

    Select.withPrompt "Select a movie" config

-}
withPrompt : String -> Config msg item -> Config msg item
withPrompt prompt config =
    let
        fn c =
            { c | prompt = prompt }
    in
    mapConfig fn config


{-| Add disabled attribute to element

    Select.withDisabled False config

-}
withDisabled : Bool -> Config msg item -> Config msg item
withDisabled isDisabled config =
    let
        fn c =
            { c | disabled = isDisabled }
    in
    mapConfig fn config


{-| @priv
-}
mapConfig : (Config.Config msg item -> Config.Config msg item) -> Config msg item -> Config msg item
mapConfig fn config =
    let
        config_ =
            unwrapConfig config
    in
    PrivateConfig (fn config_)


{-| Create a new state. You must pass a unique identifier for each select component.

    {
        ...
        selectState = Select.newState "select1"
    }

-}
newState : String -> State
newState id =
    PrivateState (Models.newState id)


{-| Return the query string from the current state model

    Select.queryFromState model.selectState

-}
queryFromState : State -> Maybe String
queryFromState model =
    model
        |> unwrapModel
        |> .query


{-| Render the view

    Select.view
        selectConfig
        model.selectState
        model.items
        selectedItems

-}
view :
    Config msg item
    -> State
    -> List item
    -> List item
    -> Html (Msg item)
view config model items selected =
    View.Select.Internal.view
        (unwrapConfig config)
        (unwrapModel model)
        items
        selected
        |> Html.map PrivateMsg


{-| Update the component state

    SelectMsg subMsg ->
        let
            ( updated, cmd ) =
                Select.update selectConfig subMsg model.selectState
        in
            ( { model | selectState = updated }, cmd )

-}
update : Config msg item -> Msg item -> State -> ( State, Cmd msg )
update config msg model =
    let
        config_ =
            unwrapConfig config

        msg_ =
            unwrapMsg msg

        model_ =
            unwrapModel model

        ( mdl, cmd ) =
            View.Select.Update.update config_ msg_ model_
    in
    ( PrivateState mdl, cmd )


{-| @priv
-}
unwrapConfig : Config msg item -> Config.Config msg item
unwrapConfig config =
    case config of
        PrivateConfig c ->
            c


{-| @priv
-}
unwrapMsg : Msg item -> Messages.Msg item
unwrapMsg msg =
    case msg of
        PrivateMsg m ->
            m


{-| @priv
-}
unwrapModel : State -> Models.State
unwrapModel model =
    case model of
        PrivateState m ->
            m
