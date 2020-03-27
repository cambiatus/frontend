module Feedback exposing (..)

import UpdateResult as UR
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput, onMouseEnter, onSubmit, stopPropagationOn)

-- MODEL


type alias Model =
    { message : String
    , success : Bool
    }

-- UPDATE

type Msg
    = ShowFeedback Model

update : Msg -> Model -> Model
update msg model =
    case msg of
        ShowFeedback feedback -> model
        

-- VIEW


getBackgroundColor : Bool -> String
getBackgroundColor isSuccess =
    if isSuccess == True then
        "bg-green-700"
    else 
        "bg-red-700"

view : Model -> Html msg
view model =
    div [ class (( getBackgroundColor model.success ) ++ "") ]
        [ text model.message ]
            
            
