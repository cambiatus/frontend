module View.Select.Events exposing (onBlurAttribute)

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Decode
import View.Select.Config exposing (Config)
import View.Select.Messages exposing (Msg(..))
import View.Select.Models exposing (State)


onBlurAttribute : Config msg item -> State -> Attribute (Msg item)
onBlurAttribute config state =
    let
        dataDecoder =
            Decode.at [ "relatedTarget", "id" ] Decode.string
                |> Decode.andThen
                    (\targetId ->
                        if String.startsWith (config.inputId ++ "-menu-item-") targetId then
                            case state.highlightedItem of
                                Nothing ->
                                    Decode.fail "Focus is not leaving select container"

                                Just highlightedItem ->
                                    if String.endsWith (String.fromInt highlightedItem) targetId then
                                        Decode.fail "Focus is not leaving select container"

                                    else
                                        Decode.succeed OnBlur

                        else
                            Decode.succeed OnBlur
                    )
    in
    on "focusout" dataDecoder
