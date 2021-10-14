module Eos.Permission exposing
    ( PermissionType(..), Authorization, Permission, Permissions
    , decoder, encodeAuthorization, encodePermissionType
    , default, list, parent, toString
    )

{-| This module helps when dealing with Eos Permissions. In EOS, we can have
arbitary names for permissions. However, in our app, we only use the default
ones, `Owner` and `Active`, so we can have type-safe permissions.


## Main Types

@docs PermissionType, Authorization, Permission, Permissions


## JSON helpers

@docs decoder, encodeAuthorization, encodePermissionType


## Helper functions

@docs default, list, parent, toString

-}

import Eos.Account
import Json.Decode as Decode
import Json.Decode.Pipeline
import Json.Encode as Encode
import List.Extra



-- TYPES


{-| The standard permission levels are `Active` and `Owner`. `Active` is a child
of `Owner`, which means `Owner` has more "power" than `Active`. For example, you
can change the `Active` permission's auth data by signing as `Owner`, but you
can't change the `Owner` permission's auth data by signing as `Active`.
-}
type PermissionType
    = RootPermission
    | Owner
    | Active


{-| This defines the key to use to sign a transaction. All accounts are
identified by an `Eos.Account.Name` (the `actor`). Every account may have
multiple (hierarchical) levels of `Permission`, each one associated with a key
pair (on multisig accounts, there may be multiple key pairs per permission level)
-}
type alias Authorization =
    { actor : Eos.Account.Name
    , permission : PermissionType
    }


{-| Every account has a set of `Permissions`. Since we only use the `active` and
`owner` permissions, we only have these fields.
-}
type alias Permissions =
    { active : Permission, owner : Permission }


{-| A `Permission` defines how many votes a proposal needs to be approved, and
who can vote. Each `Authorization` can have a different weight (> 0), and as
soon as the sum of all the weights are bigger than or equal to the `threshold`,
the proposal can be executed
-}
type alias Permission =
    { threshold : Int
    , accounts : List { authorization : Authorization, weight : Int }
    }



-- JSON


{-| Decode the result of the `get_account` call
-}
decoder : Decode.Decoder Permissions
decoder =
    Decode.list internalDecoder
        |> Decode.andThen
            (\permissions ->
                Maybe.map2 (\active owner -> { active = active, owner = owner })
                    (List.Extra.findMap
                        (\permission ->
                            if permission.permission == Active then
                                Just { threshold = permission.threshold, accounts = permission.accounts }

                            else
                                Nothing
                        )
                        permissions
                    )
                    (List.Extra.findMap
                        (\permission ->
                            if permission.permission == Owner then
                                Just { threshold = permission.threshold, accounts = permission.accounts }

                            else
                                Nothing
                        )
                        permissions
                    )
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "Expecting both `Active` and `Owner` permissions")
            )


{-| Encode an `Authorization`
-}
encodeAuthorization : Authorization -> Encode.Value
encodeAuthorization authorization =
    Encode.object
        [ ( "actor", Eos.Account.encodeName authorization.actor )
        , ( "permission", encodePermissionType authorization.permission )
        ]


{-| Encode a `PermissionType`
-}
encodePermissionType : PermissionType -> Encode.Value
encodePermissionType =
    toString >> Encode.string



-- INTERNAL JSON


authorizationDecoder : Decode.Decoder Authorization
authorizationDecoder =
    Decode.succeed Authorization
        |> Json.Decode.Pipeline.required "actor" Eos.Account.nameDecoder
        |> Json.Decode.Pipeline.required "permission" permissionTypeDecoder


permissionTypeDecoder : Decode.Decoder PermissionType
permissionTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\permissionString ->
                case permissionString of
                    "owner" ->
                        Decode.succeed Owner

                    "active" ->
                        Decode.succeed Active

                    _ ->
                        Decode.fail "Expecting  `owner` or `active`"
            )


type alias InternalPermissions =
    { permission : PermissionType
    , threshold : Int
    , accounts : List { authorization : Authorization, weight : Int }
    }


internalDecoder : Decode.Decoder InternalPermissions
internalDecoder =
    Decode.succeed InternalPermissions
        |> Json.Decode.Pipeline.required "perm_name" permissionTypeDecoder
        |> Json.Decode.Pipeline.requiredAt [ "required_auth", "threshold" ] Decode.int
        |> Json.Decode.Pipeline.requiredAt [ "required_auth", "accounts" ]
            (Decode.list
                (Decode.succeed
                    (\authorization weight ->
                        { authorization = authorization
                        , weight = weight
                        }
                    )
                    |> Json.Decode.Pipeline.required "permission" authorizationDecoder
                    |> Json.Decode.Pipeline.required "weight" Decode.int
                )
            )



-- HELPER FUNCTIONS


{-| All the permissions a user can use to sign a transaction.

Note that `RootPermission` isn't included, as it can't be used to sign a
transaction. `RootPermission` only exists to represent the parent of `Owner`

-}
list : List PermissionType
list =
    [ Owner, Active ]


{-| The permission to use when we need a default one
-}
default : PermissionType
default =
    Active


{-| Turn a permission into a string
-}
toString : PermissionType -> String
toString permission =
    case permission of
        RootPermission ->
            ""

        Owner ->
            "owner"

        Active ->
            "active"


{-| `Permissions` in EOS are organized hierarchically. Meaning that every
permission level has another permission as its parent. The only exception is
`Owner`, which doesn't have a parent. However, we represent `Owner`'s parent as
`RootPermission`. In EOS, `RootPermission` is just the empty string.
-}
parent : PermissionType -> PermissionType
parent permission =
    case permission of
        RootPermission ->
            RootPermission

        Owner ->
            RootPermission

        Active ->
            Owner
