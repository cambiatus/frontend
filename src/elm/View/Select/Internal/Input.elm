module View.Select.Internal.Input exposing (view)

import Array
import Html exposing (Attribute, Html)
import Html.Attributes
    exposing
        ( attribute
        , autocomplete
        , class
        , classList
        , disabled
        , id
        , placeholder
        , style
        , value
        )
import Html.Attributes.Aria as Aria
import Html.Events exposing (keyCode, on, onFocus, onInput, stopPropagationOn)
import Json.Decode as Decode
import View.Select.Config exposing (Config)
import View.Select.Events exposing (onBlurAttribute)
import View.Select.Internal.RemoveItem as RemoveItem
import View.Select.Messages as Msg exposing (Msg)
import View.Select.Models exposing (State)
import View.Select.Search as Search
import View.Select.Styles as Styles
import View.Select.Utils as Utils


onKeyPressAttribute : Maybe item -> Attribute (Msg item)
onKeyPressAttribute maybeItem =
    let
        fn code =
            case code of
                9 ->
                    maybeItem
                        |> Maybe.map (Decode.succeed << Msg.OnSelect)
                        |> Maybe.withDefault (Decode.fail "nothing selected")

                13 ->
                    maybeItem
                        |> Maybe.map (Decode.succeed << Msg.OnSelect)
                        |> Maybe.withDefault (Decode.fail "nothing selected")

                _ ->
                    Decode.fail "not TAB or ENTER"
    in
    on "keypress" (Decode.andThen fn keyCode)


onKeyUpAttribute : Maybe item -> Attribute (Msg item)
onKeyUpAttribute maybeItem =
    let
        selectItem =
            case maybeItem of
                Nothing ->
                    Decode.fail "not Enter"

                Just item ->
                    Decode.succeed (Msg.OnSelect item)

        fn code =
            case code of
                13 ->
                    selectItem

                38 ->
                    Decode.succeed Msg.OnUpArrow

                40 ->
                    Decode.succeed Msg.OnDownArrow

                27 ->
                    Decode.succeed Msg.OnEsc

                _ ->
                    Decode.fail "not ENTER"
    in
    on "keyup" (Decode.andThen fn keyCode)


view : Config msg item -> State -> List item -> List item -> Html (Msg item)
view config model availableItems selectedItems =
    let
        inputControlClass : String
        inputControlClass =
            Styles.inputControlClass ++ config.inputControlClass

        inputControlStyles : List ( String, String )
        inputControlStyles =
            List.append
                Styles.inputControlStyles
                config.inputControlStyles

        inputControlStylesAttrs =
            Utils.stylesToAttrs inputControlStyles

        inputWrapperClass : String
        inputWrapperClass =
            Styles.inputWrapperClass ++ config.inputWrapperClass

        inputWrapperStyles : List ( String, String )
        inputWrapperStyles =
            List.append
                Styles.inputWrapperStyles
                config.inputWrapperStyles

        inputWrapperStylesAttrs =
            Utils.stylesToAttrs inputWrapperStyles

        underlineStyles : List ( String, String )
        underlineStyles =
            List.append
                Styles.underlineStyles
                config.underlineStyles

        underline : Html (Msg item)
        underline =
            Html.div
                (class config.underlineClass
                    :: (underlineStyles |> List.map (\( f, s ) -> style f s))
                )
                []

        maybeMatchedItems : Maybe (List item)
        maybeMatchedItems =
            Search.matchedItemsWithCutoff
                config
                model.query
                availableItems
                selectedItems

        input =
            if config.isMultiSelect then
                multiInput
                    config
                    model
                    availableItems
                    selectedItems
                    maybeMatchedItems

            else
                singleInput
                    config
                    model
                    availableItems
                    selectedItems
                    maybeMatchedItems
    in
    Html.div
        (class inputControlClass :: inputControlStylesAttrs)
        [ Html.div
            (class inputWrapperClass :: inputWrapperStylesAttrs)
            input
        , underline
        ]


multiInput : Config msg item -> State -> List item -> List item -> Maybe (List item) -> List (Html (Msg item))
multiInput config model availableItems selected maybeMatchedItems =
    let
        multiInputItemContainerClasses : String
        multiInputItemContainerClasses =
            Styles.multiInputItemContainerClass
                ++ config.multiInputItemContainerClass

        multiInputItemContainerStyles : List ( String, String )
        multiInputItemContainerStyles =
            List.append
                Styles.multiInputItemContainerStyles
                config.multiInputItemContainerStyles

        multiInputItemContainerStylesAttrs =
            Utils.stylesToAttrs multiInputItemContainerStyles

        multiInputItemClasses : String
        multiInputItemClasses =
            Styles.multiInputItemClass ++ config.multiInputItemClass

        multiInputItemStyles : List ( String, String )
        multiInputItemStyles =
            List.append
                Styles.multiInputItemStyles
                config.multiInputItemStyles

        multiInputItemStylesAttrs =
            Utils.stylesToAttrs multiInputItemStyles

        viewMultiItems : List item -> Html (Msg item)
        viewMultiItems subItems =
            Html.div
                (class multiInputItemContainerClasses
                    :: multiInputItemContainerStylesAttrs
                )
                (List.map
                    (\item ->
                        Html.div
                            (class multiInputItemClasses :: multiInputItemStylesAttrs)
                            [ Html.div (Styles.multiInputItemText |> List.map (\( f, s ) -> style f s)) [ Html.text (config.toLabel item) ]
                            , Maybe.withDefault (Html.span [] []) <|
                                Maybe.map
                                    (\_ ->
                                        Html.div
                                            (onClickWithoutPropagation (Msg.OnRemoveItem item)
                                                :: (Styles.multiInputRemoveItem
                                                        |> List.map (\( f, s ) -> style f s)
                                                   )
                                            )
                                            [ RemoveItem.view config ]
                                    )
                                    config.onRemoveItem
                            ]
                    )
                    subItems
                )

        val =
            model.query |> Maybe.withDefault ""
    in
    [ viewMultiItems selected
    , Html.input
        (value val
            :: inputAttributes config model availableItems selected maybeMatchedItems
            ++ (if List.isEmpty selected then
                    [ placeholder config.prompt ]

                else
                    []
               )
        )
        []
    ]


singleInput : Config msg item -> State -> List item -> List item -> Maybe (List item) -> List (Html (Msg item))
singleInput config model availableItems selectedItems maybeMatchedItems =
    let
        val =
            model.query
                |> Maybe.withDefault ""
    in
    [ Html.input
        (inputAttributes config model availableItems selectedItems maybeMatchedItems ++ [ value val, placeholder config.prompt ])
        []
    ]


inputAttributes : Config msg item -> State -> List item -> List item -> Maybe (List item) -> List (Attribute (Msg item))
inputAttributes config model _ selectedItems maybeMatchedItems =
    let
        inputClasses : String
        inputClasses =
            String.join " "
                [ Styles.inputClass
                , config.inputClass
                , promptClass
                ]

        inputStyles : List ( String, String )
        inputStyles =
            List.concat
                [ Styles.inputStyles
                , config.inputStyles
                , promptStyles
                ]

        ( promptClass, promptStyles ) =
            if List.isEmpty selectedItems then
                ( config.promptClass, config.promptStyles )

            else
                ( "", [] )

        inputStylesAttrs =
            Utils.stylesToAttrs inputStyles

        -- item that will be selected if enter if pressed
        preselectedItem : Maybe item
        preselectedItem =
            case maybeMatchedItems of
                Just matchedItems ->
                    if config.isMultiSelect then
                        case model.highlightedItem of
                            Nothing ->
                                List.head matchedItems

                            Just n ->
                                Array.fromList matchedItems
                                    |> Array.get (remainderBy (List.length matchedItems) n)

                    else
                        List.head matchedItems

                _ ->
                    Nothing

        activeDescendant =
            case model.highlightedItem of
                Nothing ->
                    class ""

                Just highlightedItem ->
                    Aria.ariaActiveDescendant
                        (Utils.menuItemId config highlightedItem)
    in
    autocomplete False
        :: attribute "autocorrect" "off"
        :: attribute "autocomplete" "off"
        :: attribute "autocapitalize" "off"
        :: attribute "spellcheck" "false"
        :: id config.inputId
        :: disabled config.disabled
        :: onBlurAttribute config model
        :: onKeyUpAttribute preselectedItem
        :: onKeyPressAttribute preselectedItem
        :: onInput Msg.OnQueryChange
        :: onFocus Msg.OnFocus
        :: class inputClasses
        :: classList config.inputClassList
        :: Aria.role "combobox"
        :: activeDescendant
        :: attribute "aria-owns" (model.id ++ "-items-list")
        :: Aria.ariaExpanded
            (if model.showMenu then
                "true"

             else
                "false"
            )
        :: attribute "aria-autocomplete" "list"
        :: inputStylesAttrs


onClickWithoutPropagation : Msg item -> Attribute (Msg item)
onClickWithoutPropagation msg =
    Decode.succeed ( msg, False )
        |> stopPropagationOn "click"
