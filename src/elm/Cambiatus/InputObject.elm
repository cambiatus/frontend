-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Cambiatus.InputObject exposing (..)

import Cambiatus.Enum.VerificationType
import Cambiatus.Interface
import Cambiatus.Object
import Cambiatus.Scalar
import Cambiatus.ScalarCodecs
import Cambiatus.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


buildActionsInput : (ActionsInputOptionalFields -> ActionsInputOptionalFields) -> ActionsInput
buildActionsInput fillOptionals =
    let
        optionals =
            fillOptionals
                { creator = Absent, isCompleted = Absent, validator = Absent, verificationType = Absent }
    in
    { creator = optionals.creator, isCompleted = optionals.isCompleted, validator = optionals.validator, verificationType = optionals.verificationType }


type alias ActionsInputOptionalFields =
    { creator : OptionalArgument String
    , isCompleted : OptionalArgument Bool
    , validator : OptionalArgument String
    , verificationType : OptionalArgument Cambiatus.Enum.VerificationType.VerificationType
    }


{-| Type for the ActionsInput input object.
-}
type alias ActionsInput =
    { creator : OptionalArgument String
    , isCompleted : OptionalArgument Bool
    , validator : OptionalArgument String
    , verificationType : OptionalArgument Cambiatus.Enum.VerificationType.VerificationType
    }


{-| Encode a ActionsInput into a value that can be used as an argument.
-}
encodeActionsInput : ActionsInput -> Value
encodeActionsInput input =
    Encode.maybeObject
        [ ( "creator", Encode.string |> Encode.optional input.creator ), ( "isCompleted", Encode.bool |> Encode.optional input.isCompleted ), ( "validator", Encode.string |> Encode.optional input.validator ), ( "verificationType", Encode.enum Cambiatus.Enum.VerificationType.toString |> Encode.optional input.verificationType ) ]


buildChecksInput : (ChecksInputOptionalFields -> ChecksInputOptionalFields) -> ChecksInput
buildChecksInput fillOptionals =
    let
        optionals =
            fillOptionals
                { validator = Absent }
    in
    { validator = optionals.validator }


type alias ChecksInputOptionalFields =
    { validator : OptionalArgument String }


{-| Type for the ChecksInput input object.
-}
type alias ChecksInput =
    { validator : OptionalArgument String }


{-| Encode a ChecksInput into a value that can be used as an argument.
-}
encodeChecksInput : ChecksInput -> Value
encodeChecksInput input =
    Encode.maybeObject
        [ ( "validator", Encode.string |> Encode.optional input.validator ) ]


buildClaimInput : ClaimInputRequiredFields -> ClaimInput
buildClaimInput required =
    { id = required.id }


type alias ClaimInputRequiredFields =
    { id : Int }


{-| Type for the ClaimInput input object.
-}
type alias ClaimInput =
    { id : Int }


{-| Encode a ClaimInput into a value that can be used as an argument.
-}
encodeClaimInput : ClaimInput -> Value
encodeClaimInput input =
    Encode.maybeObject
        [ ( "id", Encode.int input.id |> Just ) ]


buildClaimsInput : (ClaimsInputOptionalFields -> ClaimsInputOptionalFields) -> ClaimsInput
buildClaimsInput fillOptionals =
    let
        optionals =
            fillOptionals
                { claimer = Absent, symbol = Absent, validator = Absent }
    in
    { claimer = optionals.claimer, symbol = optionals.symbol, validator = optionals.validator }


type alias ClaimsInputOptionalFields =
    { claimer : OptionalArgument String
    , symbol : OptionalArgument String
    , validator : OptionalArgument String
    }


{-| Type for the ClaimsInput input object.
-}
type alias ClaimsInput =
    { claimer : OptionalArgument String
    , symbol : OptionalArgument String
    , validator : OptionalArgument String
    }


{-| Encode a ClaimsInput into a value that can be used as an argument.
-}
encodeClaimsInput : ClaimsInput -> Value
encodeClaimsInput input =
    Encode.maybeObject
        [ ( "claimer", Encode.string |> Encode.optional input.claimer ), ( "symbol", Encode.string |> Encode.optional input.symbol ), ( "validator", Encode.string |> Encode.optional input.validator ) ]


buildInviteInput : (InviteInputOptionalFields -> InviteInputOptionalFields) -> InviteInput
buildInviteInput fillOptionals =
    let
        optionals =
            fillOptionals
                { id = Absent }
    in
    { id = optionals.id }


type alias InviteInputOptionalFields =
    { id : OptionalArgument String }


{-| Type for the InviteInput input object.
-}
type alias InviteInput =
    { id : OptionalArgument String }


{-| Encode a InviteInput into a value that can be used as an argument.
-}
encodeInviteInput : InviteInput -> Value
encodeInviteInput input =
    Encode.maybeObject
        [ ( "id", Encode.string |> Encode.optional input.id ) ]


buildNewCommunityInput : NewCommunityInputRequiredFields -> NewCommunityInput
buildNewCommunityInput required =
    { symbol = required.symbol }


type alias NewCommunityInputRequiredFields =
    { symbol : String }


{-| Type for the NewCommunityInput input object.
-}
type alias NewCommunityInput =
    { symbol : String }


{-| Encode a NewCommunityInput into a value that can be used as an argument.
-}
encodeNewCommunityInput : NewCommunityInput -> Value
encodeNewCommunityInput input =
    Encode.maybeObject
        [ ( "symbol", Encode.string input.symbol |> Just ) ]


buildObjectiveInput : ObjectiveInputRequiredFields -> ObjectiveInput
buildObjectiveInput required =
    { id = required.id }


type alias ObjectiveInputRequiredFields =
    { id : Int }


{-| Type for the ObjectiveInput input object.
-}
type alias ObjectiveInput =
    { id : Int }


{-| Encode a ObjectiveInput into a value that can be used as an argument.
-}
encodeObjectiveInput : ObjectiveInput -> Value
encodeObjectiveInput input =
    Encode.maybeObject
        [ ( "id", Encode.int input.id |> Just ) ]


buildProfileInput : (ProfileInputOptionalFields -> ProfileInputOptionalFields) -> ProfileInput
buildProfileInput fillOptionals =
    let
        optionals =
            fillOptionals
                { account = Absent }
    in
    { account = optionals.account }


type alias ProfileInputOptionalFields =
    { account : OptionalArgument String }


{-| Type for the ProfileInput input object.
-}
type alias ProfileInput =
    { account : OptionalArgument String }


{-| Encode a ProfileInput into a value that can be used as an argument.
-}
encodeProfileInput : ProfileInput -> Value
encodeProfileInput input =
    Encode.maybeObject
        [ ( "account", Encode.string |> Encode.optional input.account ) ]


buildProfileUpdateInput : ProfileUpdateInputRequiredFields -> (ProfileUpdateInputOptionalFields -> ProfileUpdateInputOptionalFields) -> ProfileUpdateInput
buildProfileUpdateInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { avatar = Absent, bio = Absent, email = Absent, interests = Absent, location = Absent, name = Absent }
    in
    { account = required.account, avatar = optionals.avatar, bio = optionals.bio, email = optionals.email, interests = optionals.interests, location = optionals.location, name = optionals.name }


type alias ProfileUpdateInputRequiredFields =
    { account : String }


type alias ProfileUpdateInputOptionalFields =
    { avatar : OptionalArgument String
    , bio : OptionalArgument String
    , email : OptionalArgument String
    , interests : OptionalArgument String
    , location : OptionalArgument String
    , name : OptionalArgument String
    }


{-| Type for the ProfileUpdateInput input object.
-}
type alias ProfileUpdateInput =
    { account : String
    , avatar : OptionalArgument String
    , bio : OptionalArgument String
    , email : OptionalArgument String
    , interests : OptionalArgument String
    , location : OptionalArgument String
    , name : OptionalArgument String
    }


{-| Encode a ProfileUpdateInput into a value that can be used as an argument.
-}
encodeProfileUpdateInput : ProfileUpdateInput -> Value
encodeProfileUpdateInput input =
    Encode.maybeObject
        [ ( "account", Encode.string input.account |> Just ), ( "avatar", Encode.string |> Encode.optional input.avatar ), ( "bio", Encode.string |> Encode.optional input.bio ), ( "email", Encode.string |> Encode.optional input.email ), ( "interests", Encode.string |> Encode.optional input.interests ), ( "location", Encode.string |> Encode.optional input.location ), ( "name", Encode.string |> Encode.optional input.name ) ]


buildPushSubscriptionInput : PushSubscriptionInputRequiredFields -> PushSubscriptionInput
buildPushSubscriptionInput required =
    { account = required.account, authKey = required.authKey, endpoint = required.endpoint, pKey = required.pKey }


type alias PushSubscriptionInputRequiredFields =
    { account : String
    , authKey : String
    , endpoint : String
    , pKey : String
    }


{-| Type for the PushSubscriptionInput input object.
-}
type alias PushSubscriptionInput =
    { account : String
    , authKey : String
    , endpoint : String
    , pKey : String
    }


{-| Encode a PushSubscriptionInput into a value that can be used as an argument.
-}
encodePushSubscriptionInput : PushSubscriptionInput -> Value
encodePushSubscriptionInput input =
    Encode.maybeObject
        [ ( "account", Encode.string input.account |> Just ), ( "authKey", Encode.string input.authKey |> Just ), ( "endpoint", Encode.string input.endpoint |> Just ), ( "pKey", Encode.string input.pKey |> Just ) ]


buildReadNotificationInput : ReadNotificationInputRequiredFields -> ReadNotificationInput
buildReadNotificationInput required =
    { id = required.id }


type alias ReadNotificationInputRequiredFields =
    { id : Int }


{-| Type for the ReadNotificationInput input object.
-}
type alias ReadNotificationInput =
    { id : Int }


{-| Encode a ReadNotificationInput into a value that can be used as an argument.
-}
encodeReadNotificationInput : ReadNotificationInput -> Value
encodeReadNotificationInput input =
    Encode.maybeObject
        [ ( "id", Encode.int input.id |> Just ) ]


buildSaleInput : SaleInputRequiredFields -> SaleInput
buildSaleInput required =
    { id = required.id }


type alias SaleInputRequiredFields =
    { id : Int }


{-| Type for the SaleInput input object.
-}
type alias SaleInput =
    { id : Int }


{-| Encode a SaleInput into a value that can be used as an argument.
-}
encodeSaleInput : SaleInput -> Value
encodeSaleInput input =
    Encode.maybeObject
        [ ( "id", Encode.int input.id |> Just ) ]


buildSalesInput : (SalesInputOptionalFields -> SalesInputOptionalFields) -> SalesInput
buildSalesInput fillOptionals =
    let
        optionals =
            fillOptionals
                { account = Absent, all = Absent, communities = Absent }
    in
    { account = optionals.account, all = optionals.all, communities = optionals.communities }


type alias SalesInputOptionalFields =
    { account : OptionalArgument String
    , all : OptionalArgument String
    , communities : OptionalArgument String
    }


{-| Type for the SalesInput input object.
-}
type alias SalesInput =
    { account : OptionalArgument String
    , all : OptionalArgument String
    , communities : OptionalArgument String
    }


{-| Encode a SalesInput into a value that can be used as an argument.
-}
encodeSalesInput : SalesInput -> Value
encodeSalesInput input =
    Encode.maybeObject
        [ ( "account", Encode.string |> Encode.optional input.account ), ( "all", Encode.string |> Encode.optional input.all ), ( "communities", Encode.string |> Encode.optional input.communities ) ]


buildTransferInput : TransferInputRequiredFields -> TransferInput
buildTransferInput required =
    { id = required.id }


type alias TransferInputRequiredFields =
    { id : Int }


{-| Type for the TransferInput input object.
-}
type alias TransferInput =
    { id : Int }


{-| Encode a TransferInput into a value that can be used as an argument.
-}
encodeTransferInput : TransferInput -> Value
encodeTransferInput input =
    Encode.maybeObject
        [ ( "id", Encode.int input.id |> Just ) ]


buildTransferSucceedInput : TransferSucceedInputRequiredFields -> TransferSucceedInput
buildTransferSucceedInput required =
    { from = required.from, symbol = required.symbol, to = required.to }


type alias TransferSucceedInputRequiredFields =
    { from : String
    , symbol : String
    , to : String
    }


{-| Type for the TransferSucceedInput input object.
-}
type alias TransferSucceedInput =
    { from : String
    , symbol : String
    , to : String
    }


{-| Encode a TransferSucceedInput into a value that can be used as an argument.
-}
encodeTransferSucceedInput : TransferSucceedInput -> Value
encodeTransferSucceedInput input =
    Encode.maybeObject
        [ ( "from", Encode.string input.from |> Just ), ( "symbol", Encode.string input.symbol |> Just ), ( "to", Encode.string input.to |> Just ) ]


buildUnreadNotificationsSubscriptionInput : UnreadNotificationsSubscriptionInputRequiredFields -> UnreadNotificationsSubscriptionInput
buildUnreadNotificationsSubscriptionInput required =
    { account = required.account }


type alias UnreadNotificationsSubscriptionInputRequiredFields =
    { account : String }


{-| Type for the UnreadNotificationsSubscriptionInput input object.
-}
type alias UnreadNotificationsSubscriptionInput =
    { account : String }


{-| Encode a UnreadNotificationsSubscriptionInput into a value that can be used as an argument.
-}
encodeUnreadNotificationsSubscriptionInput : UnreadNotificationsSubscriptionInput -> Value
encodeUnreadNotificationsSubscriptionInput input =
    Encode.maybeObject
        [ ( "account", Encode.string input.account |> Just ) ]
