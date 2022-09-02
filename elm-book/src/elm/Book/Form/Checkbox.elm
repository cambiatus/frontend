module Book.Form.Checkbox exposing (Model, Msg, chapter, initModel)

import Book.Helpers
import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Form.Checkbox
import Html exposing (Html)
import Html.Attributes



-- MODEL


type alias Model =
    { basicExample : Bool
    , errorExample : Bool
    }


initModel : Model
initModel =
    { basicExample = False
    , errorExample = False
    }



-- UPDATE


type Msg
    = ToggledBasicExample Bool
    | ToggledErrorExample Bool
    | BlurredField


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggledBasicExample basicExample ->
            { model | basicExample = basicExample }

        ToggledErrorExample errorExample ->
            { model | errorExample = errorExample }

        BlurredField ->
            model


updateSharedState : Msg -> { x | checkboxModel : Model } -> { x | checkboxModel : Model }
updateSharedState msg sharedState =
    { sharedState | checkboxModel = update msg sharedState.checkboxModel }



-- VIEW


label : Html msg
label =
    Html.span []
        [ Html.text "Some label here with "
        , Html.span [ Html.Attributes.class "text-orange-300" ] [ Html.text "arbitrary" ]
        , Html.text " HTML"
        ]


view : Model -> Html Msg
view model =
    let
        options =
            Form.Checkbox.init
                { label = label
                , id = "basic-example-checkbox"
                }
    in
    Form.Checkbox.view options
        { onCheck = ToggledBasicExample
        , onBlur = BlurredField
        , value = model.basicExample
        , error = Html.text ""
        , hasError = False
        , isRequired = True
        }


viewWithError : Model -> Html Msg
viewWithError model =
    let
        options =
            Form.Checkbox.init
                { label = label
                , id = "example-with-error-checkbox"
                }
    in
    Form.Checkbox.view options
        { onCheck = ToggledErrorExample
        , onBlur = BlurredField
        , value = model.errorExample
        , error = Book.Helpers.viewError [] True (Just "Something went wrong")
        , hasError = True
        , isRequired = True
        }


viewBase : { id : String, value : Bool } -> Html (ElmBook.Msg state)
viewBase { id, value } =
    let
        options =
            Form.Checkbox.init
                { label = label
                , id = id
                }
                |> Form.Checkbox.withDisabled True
    in
    Form.Checkbox.view options
        { onCheck = Actions.logActionWithBool "Toggled disabled checkbox"
        , onBlur = Actions.logAction "Blurred disabled checkbox"
        , value = value
        , error = Html.text ""
        , hasError = False
        , isRequired = True
        }



-- CHAPTER


chapter : Chapter { x | checkboxModel : Model }
chapter =
    Chapter.chapter "Checkbox"
        |> Chapter.withComponentList
            [ ( "Disabled"
              , viewBase { id = "disabled-off-checkbox", value = False }
              )
            , ( "Disabled and on"
              , viewBase { id = "disabled-on-checkbox", value = True }
              )
            ]
        |> Chapter.withStatefulComponentList
            [ ( "Basic example"
              , \{ checkboxModel } ->
                    view checkboxModel
                        |> Html.map (Actions.updateStateWith updateSharedState)
              )
            , ( "With error"
              , \{ checkboxModel } ->
                    viewWithError checkboxModel
                        |> Html.map (Actions.updateStateWith updateSharedState)
              )
            ]
        |> Chapter.render """
A checkbox represents whether or not an item is active. We use the default browser
checkbox - so we have some nice defaults out of the box, and Tailwind makes it
easy to style it.

<component with-label="Basic example" />

It can also be disabled:

<component with-label="Disabled" />
<component with-label="Disabled and on" />

Or display an error:

<component with-label="With error" />
"""
