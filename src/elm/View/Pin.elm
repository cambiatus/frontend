module View.Pin exposing
    ( Background(..)
    , External(..)
    , Field(..)
    , Model
    , Msg
    , RequiredOptions
    , init
    , msgToString
    , postSubmitAction
    , update
    , view
    , withBackgroundColor
    , withDisabled
    , withIsSubmitting
    , withLastKnownPin
    , withProblem
    )

{-| Creates an input box to get pin input

You should store a Pin.Model in your Model, and run Pin.update on your update.
You can then use Pin.view in your view

-}

import Browser.Dom
import Form
import Form.Text
import Form.Validate
import Html exposing (Html, button, text)
import Html.Attributes exposing (attribute, autocomplete, autofocus, class, disabled, maxlength, type_)
import Html.Events exposing (onClick)
import List.Extra
import Maybe.Extra
import Ports
import Session.Shared exposing (Shared, Translators)
import Task
import UpdateResult as UR
import View.Feedback as Feedback



-- MODEL


type alias Pin =
    String


{-| A PIN model
-}
type alias Model =
    { label : String
    , disabled : Bool
    , id : String
    , form : Form.Model FormInput
    , lastKnownPin : Maybe String
    , problems : List ( Field, String )
    , needsConfirmation : Bool
    , isPinVisible : Bool
    , isPinConfirmationVisible : Bool
    , isSubmitting : Bool
    , submitLabel : String
    , submittingLabel : String
    , background : Background
    }



-- INITIALIZING


{-| Minimum required options
-}
type alias RequiredOptions =
    { label : String
    , id : String
    , withConfirmation : Bool
    , submitLabel : String
    , submittingLabel : String
    , pinVisibility : Bool
    , lastKnownPin : Maybe String
    }


{-| Initializes a `Model` with some initial `RequiredOptions`
-}
init : RequiredOptions -> ( Model, Cmd Msg )
init { label, id, withConfirmation, submitLabel, submittingLabel, pinVisibility, lastKnownPin } =
    ( { label = label
      , disabled = False
      , id = id
      , form = Form.init { pin = "", confirmation = "" }
      , lastKnownPin = lastKnownPin
      , problems = []
      , needsConfirmation = withConfirmation
      , isPinVisible = pinVisibility
      , isPinConfirmationVisible = pinVisibility
      , isSubmitting = False
      , submitLabel = submitLabel
      , submittingLabel = submittingLabel
      , background = Light
      }
    , Browser.Dom.focus "pin-input"
        |> Task.attempt (\_ -> Ignored)
    )


type alias FormInput =
    { pin : String
    , confirmation : String
    }


type alias FormOutput =
    { pin : String
    }


createForm : Translators -> Model -> Form.Form Msg FormInput FormOutput
createForm ({ t } as translators) model =
    let
        backgroundAttrs =
            case model.background of
                Dark ->
                    Form.Text.withLabelAttrs [ class "text-white" ]
                        >> Form.Text.withCounterAttrs [ class "!text-white" ]

                Light ->
                    identity

        commonOptions field =
            Form.Text.withPlaceholder (String.repeat pinLength "*")
                >> Form.Text.withDisabled (model.disabled || model.isSubmitting)
                >> Form.Text.withCounter (Form.Text.CountLetters pinLength)
                >> Form.Text.withElements [ viewToggleVisibility field model translators ]
                >> Form.Text.withExtraAttrs
                    [ maxlength pinLength
                    , autocomplete False
                    , class "text-body-black tracking-widest"
                    ]
                >> backgroundAttrs
                >> Form.Text.withType
                    (if isVisible field model then
                        Form.Text.Text

                     else
                        Form.Text.Password
                    )
                >> Form.Text.asNumeric

        validatePin =
            Form.Validate.succeed
                >> Form.Validate.stringLengthExactly pinLength
                >> Form.Validate.custom
                    (\pin ->
                        if String.all Char.isDigit pin then
                            Ok pin

                        else
                            Err (\translators_ -> translators_.t "auth.pin.shouldHaveSixDigitsError")
                    )
                >> Form.Validate.validate translators
    in
    Form.succeed (\pin _ -> { pin = pin })
        |> Form.with
            (Form.Text.init
                { label = t model.label
                , id = "pin-input"
                }
                |> commonOptions Pin
                |> Form.Text.withExtraAttrs [ autofocus True ]
                |> Form.Text.withContainerAttrs [ class "mt-6" ]
                |> Form.textField
                    { parser = validatePin
                    , value = .pin
                    , update = \pin input -> { input | pin = pin }
                    , externalError =
                        \_ ->
                            model.problems
                                |> List.Extra.find (\( problemField, _ ) -> problemField == Pin)
                                |> Maybe.map (Tuple.second >> t)
                    }
            )
        |> Form.with
            (if model.needsConfirmation then
                Form.Text.init
                    { label = t model.label
                    , id = "pin-confirmation-input"
                    }
                    |> commonOptions PinConfirmation
                    |> Form.textField
                        { parser = validatePin
                        , value = .confirmation
                        , update = \confirmation input -> { input | confirmation = confirmation }
                        , externalError =
                            \{ pin, confirmation } ->
                                let
                                    externalProblem =
                                        model.problems
                                            |> List.Extra.find (\( problemField, _ ) -> problemField == Pin)
                                            |> Maybe.map (Tuple.second >> t)

                                    confirmationProblem =
                                        if pin == confirmation then
                                            Nothing

                                        else
                                            Just (t "auth.pinConfirmation.differsFromPinError")
                                in
                                Maybe.Extra.or confirmationProblem externalProblem
                        }

             else
                Form.succeed ""
            )



-- VIEW


{-| Converts the `Model` into `Html`
-}
view : Translators -> Model -> Html Msg
view ({ t } as translators) model =
    let
        text_ =
            t >> text
    in
    Form.view [ class "flex flex-col flex-grow" ]
        translators
        (\submitButton ->
            [ submitButton
                [ class "button button-primary min-w-full mt-auto"
                , disabled (model.disabled || model.isSubmitting)
                ]
                [ if model.isSubmitting then
                    text_ model.submittingLabel

                  else
                    text_ model.submitLabel
                ]
            ]
        )
        (createForm translators model)
        model.form
        { toMsg = GotFormMsg
        , onSubmit = SubmittedForm
        }


viewToggleVisibility : Field -> Model -> Translators -> Html Msg
viewToggleVisibility field model { t } =
    let
        text_ =
            t >> text
    in
    button
        [ class "absolute inset-y-0 uppercase text-sm font-bold right-0 mr-3 text-orange-300"
        , onClick (ToggledPinVisibility field)
        , attribute "tabindex" "-1"
        , type_ "button"
        ]
        [ if isVisible field model then
            text_ "auth.pin.toggle.hide"

          else
            text_ "auth.pin.toggle.show"
        ]


isVisible : Field -> Model -> Bool
isVisible field model =
    case field of
        Pin ->
            model.isPinVisible

        PinConfirmation ->
            model.isPinConfirmationVisible



-- UPDATE


type Msg
    = Ignored
    | GotFormMsg (Form.Msg FormInput)
    | SubmittedForm FormOutput
    | ToggledPinVisibility Field


type External
    = SubmitPin Pin
    | SendFeedback Feedback.Model


type alias UpdateResult =
    UR.UpdateResult Model Msg External


update : Shared -> Msg -> Model -> UpdateResult
update shared msg model =
    case msg of
        Ignored ->
            UR.init model

        GotFormMsg subMsg ->
            let
                hasChangedField field newForm =
                    Form.getValue field model.form /= Form.getValue field newForm

                clearFieldErrors : Field -> (FormInput -> value) -> UpdateResult -> UpdateResult
                clearFieldErrors field fieldAccessor ur =
                    let
                        newForm =
                            ur.model.form
                    in
                    if hasChangedField fieldAccessor newForm then
                        UR.mapModel
                            (\m ->
                                { m
                                    | problems =
                                        List.filter (\( field_, _ ) -> field /= field_)
                                            m.problems
                                }
                            )
                            ur

                    else
                        ur
            in
            Form.update shared subMsg model.form
                |> UR.fromChild (\newForm -> { model | form = newForm })
                    GotFormMsg
                    (\feedback -> UR.addExt (SendFeedback feedback))
                    model
                |> clearFieldErrors Pin .pin
                |> clearFieldErrors PinConfirmation .confirmation

        SubmittedForm { pin } ->
            { model
                | isSubmitting = True
                , lastKnownPin = Just pin
                , problems = []
            }
                |> UR.init
                |> UR.addExt (SubmitPin pin)

        ToggledPinVisibility field ->
            case field of
                Pin ->
                    { model | isPinVisible = not model.isPinVisible }
                        |> UR.init
                        |> UR.addCmd
                            (Browser.Dom.focus "pin-input"
                                |> Task.attempt (\_ -> Ignored)
                            )

                PinConfirmation ->
                    { model | isPinConfirmationVisible = not model.isPinConfirmationVisible }
                        |> UR.init
                        |> UR.addCmd
                            (Browser.Dom.focus "pin-confirmation-input"
                                |> Task.attempt (\_ -> Ignored)
                            )



-- UTILS


postSubmitAction : Model -> Pin -> Shared -> (String -> msg) -> ( Shared, Cmd msg )
postSubmitAction model pin shared toMsg =
    ( { shared | pinVisibility = model.isPinVisible }
    , Cmd.batch
        [ Task.succeed pin
            |> Task.perform toMsg
        , Ports.storePinVisibility model.isPinVisible
        ]
    )


withDisabled : Bool -> Model -> Model
withDisabled disabled model =
    { model | disabled = disabled }


withIsSubmitting : Bool -> Model -> Model
withIsSubmitting isSubmitting model =
    { model | isSubmitting = isSubmitting }


withProblem : Field -> String -> Model -> Model
withProblem field problem model =
    { model | problems = ( field, problem ) :: model.problems }


withBackgroundColor : Background -> Model -> Model
withBackgroundColor background model =
    { model | background = background }


withLastKnownPin : String -> Model -> Model
withLastKnownPin newPin model =
    { model | lastKnownPin = Just newPin }


type Background
    = Light
    | Dark


{-| The length of a PIN
-}
pinLength : Int
pinLength =
    6


type Field
    = Pin
    | PinConfirmation


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        SubmittedForm _ ->
            [ "SubmittedForm" ]

        ToggledPinVisibility _ ->
            [ "ToggledPinVisibility" ]
