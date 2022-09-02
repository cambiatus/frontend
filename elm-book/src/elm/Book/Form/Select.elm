module Book.Form.Select exposing (Model, Msg, chapter, initModel)

import Book.Helpers exposing (Fruit(..))
import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Form.Select
import Html exposing (Html)



-- MODEL


type alias Model =
    { selectedFruit : Fruit
    , selectedFruitWithError : Fruit
    }


initModel : Model
initModel =
    { selectedFruit = Banana
    , selectedFruitWithError = Banana
    }



-- UPDATE


type Msg
    = SelectedFruit Fruit
    | SelectedFruitWithError Fruit
    | BlurredField


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectedFruit fruit ->
            { model | selectedFruit = fruit }

        SelectedFruitWithError fruit ->
            { model | selectedFruitWithError = fruit }

        BlurredField ->
            model


updateSharedState : Msg -> { x | selectModel : Model } -> { x | selectModel : Model }
updateSharedState msg sharedState =
    { sharedState | selectModel = update msg sharedState.selectModel }



-- VIEW


baseOptions : { label : String, id : String } -> Form.Select.Options Fruit msg
baseOptions { label, id } =
    let
        allOptions =
            Book.Helpers.allFruits
                |> List.map
                    (\fruit ->
                        { option = fruit, label = Book.Helpers.fruitToString fruit }
                    )
    in
    Form.Select.init
        { label = label
        , id = id
        , optionToString = Book.Helpers.fruitToString
        }
        |> Form.Select.withOptions allOptions


viewBasic : Model -> Html Msg
viewBasic model =
    let
        options =
            baseOptions
                { label = "Pick a fruit"
                , id = "fruit-picker-example"
                }
    in
    Form.Select.view options
        { onSelect = SelectedFruit
        , onBlur = BlurredField
        , value = model.selectedFruit
        , error = Html.text ""
        , hasError = False
        , isRequired = True
        }


viewDisabled : Html (ElmBook.Msg state)
viewDisabled =
    let
        options =
            baseOptions
                { label = "Pick a fruit"
                , id = "fruit-disabled-example"
                }
                |> Form.Select.withDisabled True
    in
    Form.Select.view options
        { onSelect = Actions.logActionWith Book.Helpers.fruitToString "Selected fruit"
        , onBlur = Actions.logAction "Blurred disabled input"
        , value = Banana
        , error = Html.text ""
        , hasError = False
        , isRequired = False
        }


viewWithError : String -> Model -> Html Msg
viewWithError error model =
    let
        options =
            baseOptions
                { label = "Pick a fruit"
                , id = "fruit-error-example"
                }
    in
    Form.Select.view options
        { onSelect = SelectedFruitWithError
        , onBlur = BlurredField
        , value = model.selectedFruitWithError
        , error = Book.Helpers.viewError [] True (Just error)
        , hasError = True
        , isRequired = True
        }



-- CHAPTER


chapter : Chapter { x | selectModel : Model }
chapter =
    Chapter.chapter "Select"
        |> Chapter.withComponentList
            [ ( "Disabled"
              , viewDisabled
              )
            ]
        |> Chapter.withStatefulComponentList
            [ ( "Basic example"
              , \{ selectModel } ->
                    viewBasic selectModel
                        |> Html.map (Actions.updateStateWith updateSharedState)
              )
            , ( "With error"
              , \{ selectModel } ->
                    viewWithError "Something went wrong" selectModel
                        |> Html.map (Actions.updateStateWith updateSharedState)
              )
            ]
        |> Chapter.render """
Select menus can be useful when we want the user to select an option out of many.
This components wraps the default HTML `select` element, so it has nice user experience
by default, but it can be used to select custom types in Elm, so it has nice developer
experience. Everyone's happy!

However, since it uses the default `select` element, we don't have many choices
in customization, so it's a fairly simple component. Here's an example of it:

<component with-label="Basic example" />

It can also be disabled, or display an error:

<component with-label="Disabled" />

<component with-label="With error" />
"""
