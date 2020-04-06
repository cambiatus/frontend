module Feedback exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput, onMouseEnter, onSubmit, stopPropagationOn)
import UpdateResult as UR



-- MODEL


type alias Model =
    { message : String
    , success : Bool
    }



-- UPDATE


type Msg
    = ShowFeedback Model



-- VIEW


getBackgroundColor : Bool -> String
getBackgroundColor isSuccess =
    if isSuccess == True then
        "#8ACC9E"

    else
        "#DB1B1B"


view : Model -> Html msg
view model =
    div
        [ class "flex justify-center items-center"
        , style "background-color" (getBackgroundColor model.success)
        ]
        [ span [ class "ml-auto invisible" ] []
        , span [ class "flex items-center text-sm h-10 leading-snug text-white font-bold" ]
            [ text model.message ]
        , span [ class "ml-auto text-lg text-white font-bold mr-5" ]
            [ text "X" ]
        ]
