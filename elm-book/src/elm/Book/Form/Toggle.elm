module Book.Form.Toggle exposing (Model, Msg, chapter, initModel)

import Book.Helpers
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Form.Toggle
import Html exposing (Html)
import Html.Attributes
import Html.Events



-- MODEL


type alias Model =
    { tooltip : String
    , error : String
    , disabled : Bool
    , value : Bool
    }


initModel : Model
initModel =
    { tooltip = ""
    , error = ""
    , disabled = False
    , value = False
    }



-- UPDATE


type Msg
    = ToggledValue Bool
    | BlurredField
    | EnteredError String
    | EnteredTooltip String
    | ToggledDisabled


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggledValue newValue ->
            { model | value = newValue }

        BlurredField ->
            model

        EnteredError error ->
            { model | error = error }

        EnteredTooltip tooltip ->
            { model | tooltip = tooltip }

        ToggledDisabled ->
            { model | disabled = not model.disabled }


updateSharedState : Msg -> { x | toggleModel : Model } -> { x | toggleModel : Model }
updateSharedState msg sharedState =
    { sharedState | toggleModel = update msg sharedState.toggleModel }



-- VIEW


view : Model -> Html Msg
view model =
    let
        hasError =
            not (String.isEmpty model.error)

        addTooltip =
            if String.isEmpty model.tooltip then
                identity

            else
                Form.Toggle.withTooltip { message = model.tooltip, iconClass = "", containerClass = "" }
    in
    Html.div [ Html.Attributes.class "space-y-4" ]
        [ Html.div []
            [ Html.label [ Html.Attributes.for "error-input" ]
                [ Html.text "Error string"
                ]
            , Html.input
                [ Html.Events.onInput EnteredError
                , Html.Attributes.class "border focus-ring ml-2"
                , Html.Attributes.id "error-input"
                ]
                []
            ]
        , Html.div []
            [ Html.label [ Html.Attributes.for "tooltip-input" ]
                [ Html.text "Tooltip string"
                ]
            , Html.input
                [ Html.Events.onInput EnteredTooltip
                , Html.Attributes.class "border focus-ring ml-2"
                , Html.Attributes.id "tooltip-input"
                ]
                []
            ]
        , Html.button
            [ Html.Events.onClick ToggledDisabled
            , Html.Attributes.class "button button-primary"
            ]
            [ Html.text "Toggle disabled" ]
        , Form.Toggle.view
            (Form.Toggle.init
                { label = Html.text "Toggle description"
                , id = "live-toggle"
                }
                |> addTooltip
                |> Form.Toggle.withDisabled model.disabled
            )
            { onToggle = ToggledValue
            , onBlur = BlurredField
            , value = model.value
            , error = Book.Helpers.viewError [] hasError (Just model.error)
            , hasError = hasError
            , isRequired = False
            , translators = Book.Helpers.mockTranslators
            }
        ]



-- CHAPTER


chapter : Chapter { x | toggleModel : Model }
chapter =
    let
        baseToggle id =
            Form.Toggle.init
                { label = Html.text "Toggle description"
                , id = id
                }

        baseViewOptions value =
            { onToggle = Actions.logActionWithBool "Toggled. New value"
            , onBlur = Actions.logAction "Blurred toggle"
            , value = value
            , error = Html.text ""
            , hasError = False
            , isRequired = False
            , translators = Book.Helpers.mockTranslators
            }
    in
    Chapter.chapter "Toggle"
        |> Chapter.withComponentList
            [ ( "On"
              , Form.Toggle.view
                    (baseToggle "on-toggle")
                    (baseViewOptions True)
              )
            , ( "Off"
              , Form.Toggle.view (baseToggle "off-toggle")
                    (baseViewOptions False)
              )
            , ( "With Tooltip"
              , Form.Toggle.view
                    (baseToggle "tooltip-toggle"
                        |> Form.Toggle.withTooltip
                            { message = "Some longer description about what the toggle does. By the way, we can also add attributes to the question mark icon, such as changing the color on it"
                            , iconClass = ""
                            , containerClass = ""
                            }
                    )
                    (baseViewOptions False)
              )
            , ( "Disabled and on"
              , Form.Toggle.view
                    (baseToggle "on-disabled-toggle"
                        |> Form.Toggle.withDisabled True
                    )
                    (baseViewOptions True)
              )
            , ( "Disabled and off"
              , Form.Toggle.view
                    (baseToggle "off-disabled-toggle"
                        |> Form.Toggle.withDisabled True
                    )
                    (baseViewOptions False)
              )
            , ( "With error"
              , Form.Toggle.view
                    (baseToggle "toggle-with-error")
                    (baseViewOptions False
                        |> (\options ->
                                { options
                                    | hasError = True
                                    , error = Book.Helpers.viewError [] True (Just "Some error that needs to be addressed")
                                }
                           )
                    )
              )
            ]
        |> Chapter.withStatefulComponentList
            [ ( "Live example"
              , \{ toggleModel } ->
                    view toggleModel
                        |> Html.map (Actions.updateStateWith updateSharedState)
              )
            ]
        |> Chapter.render """
A toggle is very similar to a [checkbox](/forms/checkbox) - it represents either
`True` or `False`, `yes` or `no`, `enabled` or `disabled`, etc.

With that said, they could be on:

<component with-label="On" />

or off:

<component with-label="Off" />

Since we don't have much space for text on these, we could also include a tooltip:

<component with-label="With Tooltip" />

Sometimes we might not want the user to be able to click on them, so we can make
them disabled:

<component with-label="Disabled and on" />

<component with-label="Disabled and off" />

We might also show an error if the toggle is required:

<component with-label="With error" />

And finally, here's an example you can play with:

<component with-label="Live example"/>
"""
