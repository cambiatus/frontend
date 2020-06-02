module Select.Events exposing (onBlurAttribute)

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Decode
import Select.Config exposing (Config)
import Select.Messages exposing (Msg(..))
import Select.Models exposing (State)
import Select.Utils exposing (referenceDataName)


onBlurAttribute : Config msg item -> State -> Attribute (Msg item)
onBlurAttribute _ state =
    let
        dataDecoder =
            Decode.at [ "relatedTarget", "attributes", referenceDataName, "value" ] Decode.string

        attrToMsg attr =
            if attr == state.id then
                NoOp

            else
                OnBlur

        blur =
            Decode.maybe dataDecoder
                |> Decode.map (Maybe.map attrToMsg)
                |> Decode.map (Maybe.withDefault OnBlur)
    in
    on "focusout" blur
