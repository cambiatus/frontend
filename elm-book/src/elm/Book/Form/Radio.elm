module Book.Form.Radio exposing (Model, Msg, chapter, initModel)

import Book.Helpers exposing (Fruit(..))
import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Form.Radio
import Html exposing (Html)



-- MODEL


type alias Model =
    { selectedFruit : Maybe Fruit
    , selectedVerticalFruit : Maybe Fruit
    , selectedErrorFruit : Maybe Fruit
    }


initModel : Model
initModel =
    { selectedFruit = Nothing
    , selectedVerticalFruit = Nothing
    , selectedErrorFruit = Nothing
    }



-- UPDATE


type Msg
    = SelectedFruit (Maybe Fruit)
    | SelectedVerticalFruit (Maybe Fruit)
    | SelectedErrorFruit (Maybe Fruit)
    | BlurredField


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectedFruit fruit ->
            { model | selectedFruit = fruit }

        SelectedVerticalFruit verticalFruit ->
            { model | selectedVerticalFruit = verticalFruit }

        SelectedErrorFruit errorFruit ->
            { model | selectedErrorFruit = errorFruit }

        BlurredField ->
            model


updateSharedState : Msg -> { x | radioModel : Model } -> { x | radioModel : Model }
updateSharedState msg sharedState =
    { sharedState | radioModel = update msg sharedState.radioModel }



-- VIEW


defaultOptions : { id : String } -> Form.Radio.Options (Maybe Fruit) msg
defaultOptions { id } =
    let
        addOptions radio =
            List.foldl
                (\fruit ->
                    Form.Radio.withOption (Just fruit) (Html.text (Book.Helpers.fruitToString fruit))
                )
                radio
                Book.Helpers.allFruits
    in
    Form.Radio.init
        { label = "Pick a fruit"
        , id = id
        , optionToString =
            Maybe.map Book.Helpers.fruitToString
                >> Maybe.withDefault ""
        }
        |> addOptions


view : Model -> Html Msg
view model =
    let
        options =
            defaultOptions { id = "fruit-picker-radio" }
    in
    Form.Radio.view options
        { onSelect = SelectedFruit
        , onBlur = BlurredField
        , value = model.selectedFruit
        , error = Html.text ""
        , hasError = False
        }


viewVertical : Model -> Html Msg
viewVertical model =
    let
        options =
            defaultOptions { id = "fruit-picker-vertical-radio" }
                |> Form.Radio.withDirection Form.Radio.Vertical
    in
    Form.Radio.view options
        { onSelect = SelectedVerticalFruit
        , onBlur = BlurredField
        , value = model.selectedVerticalFruit
        , error = Html.text ""
        , hasError = False
        }


viewDisabled : Html (ElmBook.Msg state)
viewDisabled =
    let
        options =
            defaultOptions { id = "disabled-example" }
                |> Form.Radio.withDisabled True
    in
    Form.Radio.view options
        { onSelect =
            Actions.logActionWith
                (Maybe.map Book.Helpers.fruitToString
                    >> Maybe.withDefault "No fruit"
                )
                "Selected fruit"
        , onBlur = Actions.logAction "Blurred disabled select"
        , value = Just Grapes
        , error = Html.text ""
        , hasError = False
        }


viewWithError : Model -> Html Msg
viewWithError model =
    let
        options =
            defaultOptions { id = "fruit-picker-error-radio" }
    in
    Form.Radio.view options
        { onSelect = SelectedErrorFruit
        , onBlur = BlurredField
        , value = model.selectedErrorFruit
        , error = Book.Helpers.viewError [] True (Just "Something went wrong")
        , hasError = True
        }



-- CHAPTER


chapter : Chapter { x | radioModel : Model }
chapter =
    Chapter.chapter "Radio"
        |> Chapter.withComponentList
            [ ( "Disabled"
              , viewDisabled
              )
            ]
        |> Chapter.withStatefulComponentList
            [ ( "Basic example"
              , \{ radioModel } ->
                    view radioModel
                        |> Html.map (Actions.updateStateWith updateSharedState)
              )
            , ( "Vertical example"
              , \{ radioModel } ->
                    viewVertical radioModel
                        |> Html.map (Actions.updateStateWith updateSharedState)
              )
            , ( "With error"
              , \{ radioModel } ->
                    viewWithError radioModel
                        |> Html.map (Actions.updateStateWith updateSharedState)
              )
            ]
        |> Chapter.render """
Radio buttons are similar to [Select menus](/forms/select), in that it allows the
user to select an option out of a group of options. However, they offer different
user experiences, and one may be better than the other in some situations.

Similar to other controls, we wrap the default browser `radio` tag, to get some
nice defaults out of the box, but we allow custom types as options, so the developer
experience is nice. Here's a simple example:

<component with-label="Basic example" />

The options can also be stacked on top of each other:

<component with-label="Vertical example" />

They can be disabled:

<component with-label="Disabled" />

And also show errors:

<component with-label="With error" />
"""
