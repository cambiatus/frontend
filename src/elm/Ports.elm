port module Ports exposing
    ( JavascriptOut
    , JavascriptOutModel
    , javascriptInPort
    , javascriptOut
    , javascriptOutCmd
    , mapAddress
    , storeLanguage
    )

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
