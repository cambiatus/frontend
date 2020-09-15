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


buildAddressUpdateInput : AddressUpdateInputRequiredFields -> (AddressUpdateInputOptionalFields -> AddressUpdateInputOptionalFields) -> AddressUpdateInput
buildAddressUpdateInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { number = Absent }
    in
    { accountId = required.accountId, cityId = required.cityId, countryId = required.countryId, neighborhoodId = required.neighborhoodId, number = optionals.number, stateId = required.stateId, street = required.street, zip = required.zip }


type alias AddressUpdateInputRequiredFields =
    { accountId : String
    , cityId : Cambiatus.ScalarCodecs.Id
    , countryId : Cambiatus.ScalarCodecs.Id
    , neighborhoodId : Cambiatus.ScalarCodecs.Id
    , stateId : Cambiatus.ScalarCodecs.Id
    , street : String
    , zip : String
    }


type alias AddressUpdateInputOptionalFields =
    { number : OptionalArgument String }


{-| Type for the AddressUpdateInput input object.
-}
type alias AddressUpdateInput =
    { accountId : String
    , cityId : Cambiatus.ScalarCodecs.Id
    , countryId : Cambiatus.ScalarCodecs.Id
    , neighborhoodId : Cambiatus.ScalarCodecs.Id
    , number : OptionalArgument String
    , stateId : Cambiatus.ScalarCodecs.Id
    , street : String
    , zip : String
    }


{-| Encode a AddressUpdateInput into a value that can be used as an argument.
-}
encodeAddressUpdateInput : AddressUpdateInput -> Value
encodeAddressUpdateInput input =
    Encode.maybeObject
        [ ( "accountId", Encode.string input.accountId |> Just ), ( "cityId", (Cambiatus.ScalarCodecs.codecs |> Cambiatus.Scalar.unwrapEncoder .codecId) input.cityId |> Just ), ( "countryId", (Cambiatus.ScalarCodecs.codecs |> Cambiatus.Scalar.unwrapEncoder .codecId) input.countryId |> Just ), ( "neighborhoodId", (Cambiatus.ScalarCodecs.codecs |> Cambiatus.Scalar.unwrapEncoder .codecId) input.neighborhoodId |> Just ), ( "number", Encode.string |> Encode.optional input.number ), ( "stateId", (Cambiatus.ScalarCodecs.codecs |> Cambiatus.Scalar.unwrapEncoder .codecId) input.stateId |> Just ), ( "street", Encode.string input.street |> Just ), ( "zip", Encode.string input.zip |> Just ) ]


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


buildClaimAnalysisHistoryFilter : (ClaimAnalysisHistoryFilterOptionalFields -> ClaimAnalysisHistoryFilterOptionalFields) -> ClaimAnalysisHistoryFilter
buildClaimAnalysisHistoryFilter fillOptionals =
    let
        optionals =
            fillOptionals
                { claimer = Absent, status = Absent }
    in
    { claimer = optionals.claimer, status = optionals.status }


type alias ClaimAnalysisHistoryFilterOptionalFields =
    { claimer : OptionalArgument String
    , status : OptionalArgument String
    }


{-| Type for the ClaimAnalysisHistoryFilter input object.
-}
type alias ClaimAnalysisHistoryFilter =
    { claimer : OptionalArgument String
    , status : OptionalArgument String
    }


{-| Encode a ClaimAnalysisHistoryFilter into a value that can be used as an argument.
-}
encodeClaimAnalysisHistoryFilter : ClaimAnalysisHistoryFilter -> Value
encodeClaimAnalysisHistoryFilter input =
    Encode.maybeObject
        [ ( "claimer", Encode.string |> Encode.optional input.claimer ), ( "status", Encode.string |> Encode.optional input.status ) ]


buildClaimAnalysisHistoryInput : ClaimAnalysisHistoryInputRequiredFields -> (ClaimAnalysisHistoryInputOptionalFields -> ClaimAnalysisHistoryInputOptionalFields) -> ClaimAnalysisHistoryInput
buildClaimAnalysisHistoryInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { filter = Absent }
    in
    { account = required.account, filter = optionals.filter, symbol = required.symbol }


type alias ClaimAnalysisHistoryInputRequiredFields =
    { account : String
    , symbol : String
    }


type alias ClaimAnalysisHistoryInputOptionalFields =
    { filter : OptionalArgument ClaimAnalysisHistoryFilter }


{-| Type for the ClaimAnalysisHistoryInput input object.
-}
type alias ClaimAnalysisHistoryInput =
    { account : String
    , filter : OptionalArgument ClaimAnalysisHistoryFilter
    , symbol : String
    }


{-| Encode a ClaimAnalysisHistoryInput into a value that can be used as an argument.
-}
encodeClaimAnalysisHistoryInput : ClaimAnalysisHistoryInput -> Value
encodeClaimAnalysisHistoryInput input =
    Encode.maybeObject
        [ ( "account", Encode.string input.account |> Just ), ( "filter", encodeClaimAnalysisHistoryFilter |> Encode.optional input.filter ), ( "symbol", Encode.string input.symbol |> Just ) ]


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


buildClaimsAnalysisInput : ClaimsAnalysisInputRequiredFields -> ClaimsAnalysisInput
buildClaimsAnalysisInput required =
    { account = required.account, symbol = required.symbol }


type alias ClaimsAnalysisInputRequiredFields =
    { account : String
    , symbol : String
    }


{-| Type for the ClaimsAnalysisInput input object.
-}
type alias ClaimsAnalysisInput =
    { account : String
    , symbol : String
    }


{-| Encode a ClaimsAnalysisInput into a value that can be used as an argument.
-}
encodeClaimsAnalysisInput : ClaimsAnalysisInput -> Value
encodeClaimsAnalysisInput input =
    Encode.maybeObject
        [ ( "account", Encode.string input.account |> Just ), ( "symbol", Encode.string input.symbol |> Just ) ]


buildCountryInput : CountryInputRequiredFields -> CountryInput
buildCountryInput required =
    { name = required.name }


type alias CountryInputRequiredFields =
    { name : String }


{-| Type for the CountryInput input object.
-}
type alias CountryInput =
    { name : String }


{-| Encode a CountryInput into a value that can be used as an argument.
-}
encodeCountryInput : CountryInput -> Value
encodeCountryInput input =
    Encode.maybeObject
        [ ( "name", Encode.string input.name |> Just ) ]


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


buildKycDataUpdateInput : KycDataUpdateInputRequiredFields -> KycDataUpdateInput
buildKycDataUpdateInput required =
    { accountId = required.accountId, countryId = required.countryId, document = required.document, documentType = required.documentType, phone = required.phone, userType = required.userType }


type alias KycDataUpdateInputRequiredFields =
    { accountId : String
    , countryId : Cambiatus.ScalarCodecs.Id
    , document : String
    , documentType : String
    , phone : String
    , userType : String
    }


{-| Type for the KycDataUpdateInput input object.
-}
type alias KycDataUpdateInput =
    { accountId : String
    , countryId : Cambiatus.ScalarCodecs.Id
    , document : String
    , documentType : String
    , phone : String
    , userType : String
    }


{-| Encode a KycDataUpdateInput into a value that can be used as an argument.
-}
encodeKycDataUpdateInput : KycDataUpdateInput -> Value
encodeKycDataUpdateInput input =
    Encode.maybeObject
        [ ( "accountId", Encode.string input.accountId |> Just ), ( "countryId", (Cambiatus.ScalarCodecs.codecs |> Cambiatus.Scalar.unwrapEncoder .codecId) input.countryId |> Just ), ( "document", Encode.string input.document |> Just ), ( "documentType", Encode.string input.documentType |> Just ), ( "phone", Encode.string input.phone |> Just ), ( "userType", Encode.string input.userType |> Just ) ]


buildKycDeletionInput : KycDeletionInputRequiredFields -> KycDeletionInput
buildKycDeletionInput required =
    { accountId = required.accountId }


type alias KycDeletionInputRequiredFields =
    { accountId : String }


{-| Type for the KycDeletionInput input object.
-}
type alias KycDeletionInput =
    { accountId : String }


{-| Encode a KycDeletionInput into a value that can be used as an argument.
-}
encodeKycDeletionInput : KycDeletionInput -> Value
encodeKycDeletionInput input =
    Encode.maybeObject
        [ ( "accountId", Encode.string input.accountId |> Just ) ]


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
                { account = Absent, all = Absent, communities = Absent, communityId = Absent }
    in
    { account = optionals.account, all = optionals.all, communities = optionals.communities, communityId = optionals.communityId }


type alias SalesInputOptionalFields =
    { account : OptionalArgument String
    , all : OptionalArgument String
    , communities : OptionalArgument String
    , communityId : OptionalArgument String
    }


{-| Type for the SalesInput input object.
-}
type alias SalesInput =
    { account : OptionalArgument String
    , all : OptionalArgument String
    , communities : OptionalArgument String
    , communityId : OptionalArgument String
    }


{-| Encode a SalesInput into a value that can be used as an argument.
-}
encodeSalesInput : SalesInput -> Value
encodeSalesInput input =
    Encode.maybeObject
        [ ( "account", Encode.string |> Encode.optional input.account ), ( "all", Encode.string |> Encode.optional input.all ), ( "communities", Encode.string |> Encode.optional input.communities ), ( "communityId", Encode.string |> Encode.optional input.communityId ) ]


buildSignUpInput : SignUpInputRequiredFields -> (SignUpInputOptionalFields -> SignUpInputOptionalFields) -> SignUpInput
buildSignUpInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { invitationId = Absent }
    in
    { account = required.account, email = required.email, invitationId = optionals.invitationId, name = required.name, publicKey = required.publicKey }


type alias SignUpInputRequiredFields =
    { account : String
    , email : String
    , name : String
    , publicKey : String
    }


type alias SignUpInputOptionalFields =
    { invitationId : OptionalArgument String }


{-| Type for the SignUpInput input object.
-}
type alias SignUpInput =
    { account : String
    , email : String
    , invitationId : OptionalArgument String
    , name : String
    , publicKey : String
    }


{-| Encode a SignUpInput into a value that can be used as an argument.
-}
encodeSignUpInput : SignUpInput -> Value
encodeSignUpInput input =
    Encode.maybeObject
        [ ( "account", Encode.string input.account |> Just ), ( "email", Encode.string input.email |> Just ), ( "invitationId", Encode.string |> Encode.optional input.invitationId ), ( "name", Encode.string input.name |> Just ), ( "publicKey", Encode.string input.publicKey |> Just ) ]


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
