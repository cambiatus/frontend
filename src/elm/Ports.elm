port module Ports exposing
    ( ContributionData(..)
    , JavascriptOut
    , JavascriptOutModel
    , createAbsintheSocket
    , getRecentSearches
    , gotRecentSearches
    , javascriptInPort
    , javascriptOut
    , javascriptOutCmd
    , mapAddress
    , requestPaypalInfoFromJs
    , sendMarkdownLink
    , sendPaypalInfo
    , setMarkdownContent
    , storeAuthToken
    , storeHasSeenSponsorModal
    , storeLanguage
    , storePinVisibility
    , storeRecentSearches
    , storeSelectedCommunitySymbol
    )

import Cambiatus.Enum.CurrencyType
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


{-| Store recent searches
-}
port storeRecentSearches : String -> Cmd msg


{-| Ping JS to send back the recent searches
-}
port getRecentSearches : () -> Cmd msg


{-| Stores the auth token given by the server after signing in.
-}
port storeAuthToken : String -> Cmd msg


{-| Store the selected community symbol. Useful for when running the app with
`USE_SUBDOMAIN = false`
-}
port storeSelectedCommunitySymbol : String -> Cmd msg


{-| Store whether to show or hide the pin by default
-}
port storePinVisibility : Bool -> Cmd msg


{-| Store whether or not the user has seen the sponsor modal
-}
port storeHasSeenSponsorModal : Bool -> Cmd msg


{-| Send info about a link in a MarkdownEditor to be treated on JS
-}
sendMarkdownLink : { id : String, label : String, url : String } -> Cmd msg
sendMarkdownLink { id, label, url } =
    Encode.object
        [ ( "id", Encode.string id )
        , ( "label", Encode.string label )
        , ( "url", Encode.string url )
        ]
        |> markdownLink


port markdownLink : Value -> Cmd msg


setMarkdownContent : { id : String, content : Value } -> Cmd msg
setMarkdownContent { id, content } =
    Encode.object
        [ ( "id", Encode.string id )
        , ( "content", content )
        ]
        |> setMarkdown


port setMarkdown : Value -> Cmd msg


type ContributionData
    = SuccessfulContribution
        { amount : Float
        , communityName : String
        , targetId : String
        , invoiceId : String
        , currency : Cambiatus.Enum.CurrencyType.CurrencyType
        }
    | ContributionWithError


sendPaypalInfo : ContributionData -> Cmd msg
sendPaypalInfo contributionData =
    case contributionData of
        SuccessfulContribution contribution ->
            Encode.object
                [ ( "amount", Encode.float contribution.amount )
                , ( "communityName", Encode.string contribution.communityName )
                , ( "targetId", Encode.string contribution.targetId )
                , ( "invoiceId", Encode.string contribution.invoiceId )
                , ( "currency", Encode.string (Cambiatus.Enum.CurrencyType.toString contribution.currency) )
                ]
                |> paypalInfo

        ContributionWithError ->
            Encode.object [ ( "error", Encode.bool True ) ]
                |> paypalInfo


port paypalInfo : Value -> Cmd msg


port requestPaypalInfoFromJs : (String -> msg) -> Sub msg


{-| Given an auth token, create the absinthe socket to receive subscription data
-}
port createAbsintheSocket : String -> Cmd msg



--
-- Subscriptions
--


{-| Receive recent searches from JS.
-}
port gotRecentSearches : (String -> msg) -> Sub msg
