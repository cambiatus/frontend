port module Ports exposing
    ( JavascriptOut
    , JavascriptOutModel
    , getRecentSearches
    , gotRecentSearches
    , javascriptInPort
    , javascriptOut
    , javascriptOutCmd
    , mapAddress
    , storeAuthToken
    , storeLanguage
    , storeRecentSearches
    , storeSelectedCommunitySymbol
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


{-| Store recent searches to the `localStorage`.
-}
port storeRecentSearches : String -> Cmd msg


{-| Ping JS to send back the recent searches from the `localStorage`.
-}
port getRecentSearches : () -> Cmd msg


{-| Stores the auth token given by the server after signing in.
-}
port storeAuthToken : String -> Cmd msg


{-| Store the selected community symbol. Useful for when running the app with
`USE_SUBDOMAIN = false`
-}
port storeSelectedCommunitySymbol : String -> Cmd msg



--
-- Subscriptions
--


{-| Receive recent searches from JS.
-}
port gotRecentSearches : (String -> msg) -> Sub msg
