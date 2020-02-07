port module Ports exposing (JavascriptOut, JavascriptOutModel, javascriptInPort, javascriptOut, javascriptOutCmd, loginWithScatter, mapAddress, onScatterLoginSub, storeLanguage)

import Community
import Eos exposing (Symbol)
import Eos.Account as Eos
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)


port javascriptOutPort : Value -> Cmd msg


type JavascriptOut a
    = JavascriptOut (JavascriptOutModel a)


type alias JavascriptOutModel a =
    { responseAddress : a
    , responseData : Value
    , data : Value
    }


mapAddress : (a -> b) -> JavascriptOut a -> JavascriptOut b
mapAddress transform (JavascriptOut js) =
    JavascriptOut
        { responseAddress = transform js.responseAddress
        , responseData = js.responseData
        , data = js.data
        }


javascriptOut : JavascriptOutModel a -> JavascriptOut a
javascriptOut =
    JavascriptOut


javascriptOutCmd : (a -> List String) -> JavascriptOut a -> Cmd msg
javascriptOutCmd toJsAddress (JavascriptOut js) =
    Encode.object
        [ ( "responseAddress", Encode.list Encode.string (toJsAddress js.responseAddress) )
        , ( "responseData", js.responseData )
        , ( "data", js.data )
        ]
        |> javascriptOutPort


port javascriptInPort : (Value -> msg) -> Sub msg



--
-- Commands
--


port storeLanguage : String -> Cmd msg


port loginWithScatter : () -> Cmd msg



--
-- Callbacks
--


port onScatterLogin : (Value -> msg) -> Sub msg


onScatterLoginSub : (Result String ( Eos.Name, Maybe String ) -> msg) -> Sub msg
onScatterLoginSub toMsg =
    onScatterLogin (decodeAccountNameOrStringError >> toMsg)



-- Helpers


decodeAccountNameOrStringError : Value -> Result String ( Eos.Name, Maybe String )
decodeAccountNameOrStringError value =
    Decode.decodeValue
        (Decode.succeed (\accountName maybePrivateKey -> ( accountName, maybePrivateKey ))
            |> Decode.required "accountName" Eos.nameDecoder
            |> Decode.optional "privateKey" (Decode.nullable Decode.string) Nothing
        )
        value
        |> Result.mapError
            (\s ->
                Decode.decodeValue Decode.string value
                    |> Result.withDefault "Failed to decode"
            )
