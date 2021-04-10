module View.Feedback exposing (Model(..), Msg, Status(..), update, view)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Icons



-- TYPES


type Status
    = Success
    | Failure


type Model
    = Hidden
    | Visible Status String



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Hidden ->
            text ""

        Visible status content ->
            let
                bgColor =
                    case status of
                        Success ->
                            "bg-green"

                        Failure ->
                            "bg-red"
            in
            div
                [ class ("w-full sticky top-0 text-white py-2 grid grid-cols-10 z-40 " ++ bgColor)
                ]
                [ span
                    [ class "text-center text-sm text-white font-bold col-start-2 col-end-10" ]
                    [ text content ]
                , button
                    [ class "ml-auto mr-6 cursor-pointer"
                    , onClick ClickedClose
                    ]
                    [ Icons.close "fill-current text-white w-4 h-4" ]
                ]



-- UPDATE


type Msg
    = ClickedClose


update : Msg -> Model -> Model
update msg _ =
    case msg of
        ClickedClose ->
            Hidden
