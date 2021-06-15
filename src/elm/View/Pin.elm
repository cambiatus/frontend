module View.Pin exposing
    ( Field(..)
    , Model
    , Msg
    , RequiredOptions
    , SubmitStatus(..)
    , init
    , maybeSubmitCmd
    , msgToString
    , update
    , view
    , withAttrs
    , withDisabled
    , withProblem
    )

{-| Creates an input box to get pin input

You should store a Pin.Model in your Model, and run Pin.update on your update.
You can then use Pin.view in your view

-}

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute, autocomplete, class, classList, disabled, maxlength, required, type_)
import Html.Events exposing (keyCode, onClick, preventDefaultOn)
import Json.Decode as Decode
import Session.Shared exposing (Translators)
import Task
import Validate
import View.Form.Input



-- MODEL


type alias Pin =
    String


{-| A PIN model
-}
type alias Model =
    { label : String
    , disabled : Bool
    , id : String
    , pin : Pin
    , pinConfirmation : Maybe Pin
    , placeholder : String
    , problems : List ( Field, String )
    , isPinVisible : Bool
    , isPinConfirmationVisible : Bool
    , isSubmitting : Bool
    , submitLabel : String
    , submittingLabel : String
    , extraAttrs : List (Html.Attribute Msg)
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
    }


{-| Initializes a `Model` with some initial `RequiredOptions`
-}
init : RequiredOptions -> Model
init { label, id, withConfirmation, submitLabel, submittingLabel } =
    { label = label
    , disabled = False
    , id = id
    , pin = ""
    , pinConfirmation =
        if withConfirmation then
            Just ""

        else
            Nothing
    , placeholder = String.repeat pinLength "*"
    , problems = []
    , isPinVisible = True
    , isPinConfirmationVisible = True
    , isSubmitting = False
    , submitLabel = submitLabel
    , submittingLabel = submittingLabel
    , extraAttrs = []
    }



-- VIEW


{-| Converts the `Model` into `Html`
-}
view : Translators -> Model -> Html Msg
view ({ t } as translators) model =
    let
        text_ =
            t >> text
    in
    div (class "flex flex-col flex-grow" :: model.extraAttrs)
        [ viewField Pin model translators
        , case model.pinConfirmation of
            Nothing ->
                text ""

            Just _ ->
                viewField PinConfirmation model translators
        , button
            [ class "button button-primary min-w-full mt-auto"
            , disabled (model.disabled || model.isSubmitting)
            , onClick ClickedSubmit
            ]
            [ if model.isSubmitting then
                text_ model.submittingLabel

              else
                text_ model.submitLabel
            ]
        ]


viewField : Field -> Model -> Translators -> Html Msg
viewField field model ({ t } as translators) =
    let
        enterKeyCode =
            13
    in
    View.Form.Input.init
        { label = t model.label
        , id =
            case field of
                Pin ->
                    model.id

                PinConfirmation ->
                    model.id ++ "-confirmation"
        , onInput =
            case field of
                Pin ->
                    EnteredPin

                PinConfirmation ->
                    EnteredPinConfirmation
        , disabled = model.disabled || model.isSubmitting
        , value =
            case field of
                Pin ->
                    model.pin

                PinConfirmation ->
                    Maybe.withDefault "" model.pinConfirmation
        , placeholder = Just model.placeholder
        , problems =
            List.filterMap
                (\( errorField, error ) ->
                    if errorField == field then
                        Just (t error)

                    else
                        Nothing
                )
                model.problems
                |> Just
        , translators = translators
        }
        |> View.Form.Input.withCounter pinLength
        |> View.Form.Input.withCounterAttrs [ class "text-white" ]
        |> View.Form.Input.withElements [ viewToggleVisibility field model translators ]
        |> View.Form.Input.withErrorAttrs [ class "form-error-on-dark-bg" ]
        |> View.Form.Input.withAttrs
            [ class "form-input text-base text-body-black tracking-widest"
            , classList
                [ ( "field-with-error"
                  , model.problems
                        |> List.filter (\( errorField, _ ) -> errorField == field)
                        |> List.isEmpty
                        |> not
                  )
                ]
            , maxlength pinLength
            , attribute "inputmode" "numeric"
            , if isVisible field model then
                type_ "text"

              else
                type_ "password"
            , required True
            , autocomplete False
            , preventDefaultOn "keydown"
                (keyCode
                    |> Decode.map
                        (\code ->
                            if code == enterKeyCode then
                                ( ClickedSubmit, True )

                            else
                                ( Ignored, False )
                        )
                )
            ]
        |> View.Form.Input.toHtml


viewToggleVisibility : Field -> Model -> Translators -> Html Msg
viewToggleVisibility field model { t } =
    let
        text_ =
            t >> text
    in
    button
        [ class "absolute inset-y-0 uppercase text-xs right-0 mr-3 text-orange-300"
        , onClick (ToggledPinVisibility field)
        , attribute "tabindex" "-1"
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
    | EnteredPin String
    | EnteredPinConfirmation String
    | ToggledPinVisibility Field
    | ClickedSubmit


type SubmitStatus
    = NotAsked
    | Success Pin
    | WithError


update : Msg -> Model -> ( Model, SubmitStatus )
update msg model =
    case msg of
        Ignored ->
            ( model, NotAsked )

        EnteredPin pin ->
            ( { model
                | pin = String.filter Char.isDigit pin
                , problems = []
              }
            , NotAsked
            )

        EnteredPinConfirmation pinConfirmation ->
            ( { model
                | pinConfirmation = Maybe.map (\_ -> pinConfirmation) model.pinConfirmation
                , problems = []
              }
            , NotAsked
            )

        ToggledPinVisibility field ->
            case field of
                Pin ->
                    ( { model | isPinVisible = not model.isPinVisible }, NotAsked )

                PinConfirmation ->
                    ( { model | isPinConfirmationVisible = not model.isPinConfirmationVisible }
                    , NotAsked
                    )

        ClickedSubmit ->
            case Validate.validate validate model of
                Ok _ ->
                    ( { model | isSubmitting = True }, Success model.pin )

                Err errors ->
                    ( { model | problems = errors }, WithError )



-- UTILS


maybeSubmitCmd : SubmitStatus -> (String -> msg) -> Cmd msg
maybeSubmitCmd status toMsg =
    case status of
        NotAsked ->
            Cmd.none

        WithError ->
            Cmd.none

        Success pin ->
            Task.succeed pin
                |> Task.perform toMsg


withDisabled : Bool -> Model -> Model
withDisabled disabled model =
    { model | disabled = disabled }


withProblem : Field -> String -> Model -> Model
withProblem field problem model =
    { model | problems = ( field, problem ) :: model.problems }


withAttrs : List (Html.Attribute Msg) -> Model -> Model
withAttrs attrs model =
    { model | extraAttrs = attrs }


{-| The length of a PIN
-}
pinLength : Int
pinLength =
    6


type Field
    = Pin
    | PinConfirmation


{-| Determines if a PIN is valid
-}
isValid : String -> Bool
isValid pin =
    let
        hasCorrectLength p =
            String.length p == pinLength

        hasOnlyDigits =
            String.all Char.isDigit
    in
    hasCorrectLength pin && hasOnlyDigits pin


{-| Used to validate a `Model`
-}
validate : Validate.Validator ( Field, String ) Model
validate =
    Validate.fromErrors
        (\model ->
            if not (isValid model.pin) then
                [ ( Pin, "auth.pin.shouldHaveSixDigitsError" ) ]

            else
                case model.pinConfirmation of
                    Nothing ->
                        []

                    Just confirmationValue ->
                        if not (isValid confirmationValue) then
                            [ ( PinConfirmation, "auth.pin.shouldHaveSixDigitsError" ) ]

                        else if model.pin /= confirmationValue then
                            [ ( PinConfirmation, "auth.pinConfirmation.differsFromPinError" ) ]

                        else
                            []
        )


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        EnteredPin _ ->
            [ "EnteredPin" ]

        EnteredPinConfirmation _ ->
            [ "EnteredPinConfirmation" ]

        ToggledPinVisibility _ ->
            [ "ToggledPinVisibility" ]

        ClickedSubmit ->
            [ "ClickedSubmit" ]
