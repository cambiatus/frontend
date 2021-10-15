module Api.Eos exposing
    ( query, querySingleItem, queryWithList, Account(..)
    , TokenTable(..), MultiSigTable(..)
    , transact, Action(..)
    , CommunityAction(..), MultiSigAction(..), EosAction(..)
    , getAccount
    )

{-| This is a module to help interacting with EOS, our blockchain. There are two
main operations: [`query`](#query) and [`transact`](#transact).
An EOS transaction is equivalent to a GraphQL mutation, while query is the same
for both of them.

You can view more information on blockchain stuff using [the block explorer](https://local.bloks.io/account/cambiatus.cm?nodeUrl=https%3A%2F%2Fstaging.cambiatus.io&coreSymbol=SYS&systemDomain=eosio).


## Querying

@docs query, querySingleItem, queryWithList, Account


### Tables

@docs TokenTable, MultiSigTable


## Transacting

@docs transact, Authorization, Permission, defaultPermission, Action


### Actions

@docs CommunityAction, MultiSigAction, EosAction


## Helpers

@ docs listPermissions, permissionToString

-}

import Eos
import Eos.Account
import Eos.Permission
import Http
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Ports
import Session.Shared exposing (Shared)
import Time
import Url.Builder



-- TRANSACT


{-| This is how we write to EOS. Because we use `eosjs` to manage signature, we
have a `eosTransaction` port, which signs and sends transactions. This is why
this function returns a `JavascriptOutModel`.

Since transactions need to be signed, we need the private key to be loaded. That
is done in JS, and the simplest way to do it is to add a call to
`LoggedIn.withAuthentication` on your `UpdateResult` pipeline. If the private
key isn't loaded, EOS will return an error.

-}
transact : Shared -> Eos.Permission.Authorization -> msg -> Action -> Ports.JavascriptOutModel msg
transact shared authorization toMsg action =
    { responseAddress = toMsg
    , responseData = Encode.null
    , data =
        Encode.object
            [ ( "name", Encode.string "eosTransaction" )
            , ( "actions", Encode.list (encodeAction shared authorization) [ action ] )
            ]
    }


{-| Since every account has a different set of actions, and every action needs
different arguments, the `Action` type offers us a type-safe way of describing
an action and it's arguments all at the same time, preventing impossible states.

The `Action` type declares the contract that executes the action, and each
action has another type in their constructor, which defines the available
actions in that contract, and their respective arguments.

We actually send transaction to the blockchain, but a transaction is just a list
of actions.

-}
type Action
    = CommunityAction CommunityAction
    | MultiSigAction MultiSigAction
    | EosAction EosAction


{-| All of the available actions related to communities (on the `cambiatus.cm`
contract)

  - `CreateObjective`: create a new objective on the community represented by
    `communitySymbol`, providing the `objectiveDescription`

-}
type CommunityAction
    = CreateObjective
        { communitySymbol : Eos.Symbol
        , objectiveDescription : String
        , communityAdmin : Eos.Account.Name
        }


{-| All of the available actions related to multisig (on the `eosio.msig`
contract)

  - `Propose`: propose a new transaction. The `proposalName` must be under 13
    characters long.
  - `Approve`: cast an approval vote on a proposal. Proposals are scoped by the
    proposer's name and the proposal's name. An approval vote may be removed by
    `Unapproving` the vote.
  - `Unapprove`: remove an approval vote from a proposal. You can't vote "no" to
    a proposal, you can only vote "yes" or remove your "yes" vote
  - `Cancel`: cancels a proposal. Can only be called after the proposal has
    expired.

-}
type MultiSigAction
    = Propose
        { proposer : Eos.Account.Name
        , proposalName : String
        , requestedVotes : List Eos.Permission.Authorization
        , expiration : Time.Posix
        , actions : List ( Action, Eos.Permission.Authorization )
        }
    | Approve
        { proposer : Eos.Account.Name
        , proposalName : String
        }
    | Unapprove
        { proposer : Eos.Account.Name
        , proposalName : String
        }
    | Execute
        { proposer : Eos.Account.Name
        , proposalName : String
        }
    | Cancel
        { proposer : Eos.Account.Name
        , proposalName : String
        }


{-| All of the available actions related to core eos (on the `eosio` contract)

  - `UpdateAuth`: configure an account's permissions. Usually used to set it as
    a multisig account.

-}
type EosAction
    = UpdateAuth
        { targetAccount : Eos.Account.Name
        , targetPermission : Eos.Permission.PermissionType
        , threshold : Int
        , accounts :
            List
                { account : Eos.Account.Name
                , permission : Eos.Permission.PermissionType
                , weight : Int
                }
        }



-- QUERY


{-| This is how we read from EOS. EOS queries need a few arguments to locate the
correct data:

  - account/contract: This is the account that defines the table. The most
    common ones for us are `cambiatus.cm` and `cambiatus.tk`.
  - table: The actual table in that account.
  - scope: This is like an index. We need it so we can say who the data belongs
    to.

As an example, if you wanted to query for multisig proposals, you would query
for the table `proposal` from the `eosio.msig` account, providing the name of
the user who proposed it as the scope.

Since we're getting data from a table, we always get a list of values. If you
want only one, consider using [`querySingleItem`](#querySingleItem).

Since all information on the blockchain is public, you don't need to be logged
in to perform a query.

-}
query : Shared -> (Result Http.Error (List a) -> msg) -> Decode.Decoder a -> Account -> Cmd msg
query shared toMsg decoder query_ =
    queryInternal shared
        query_
        100
        (Decode.field "rows" (Decode.list decoder))
        toMsg


{-| Same as [`query`](#query), but only retrieves the first row on the table.
-}
querySingleItem : Shared -> (Result Http.Error a -> msg) -> Decode.Decoder a -> Account -> Cmd msg
querySingleItem shared toMsg decoder query_ =
    queryInternal shared
        query_
        1
        (Decode.field "rows" (Decode.index 0 decoder))
        toMsg


{-| Same as [`query`](#query), but you have access to the list decoder. Useful
if you want to map or filter the results. Also retrieves more entries than the
regular `query`.
-}
queryWithList : Shared -> (Result Http.Error a -> msg) -> Decode.Decoder a -> Account -> Cmd msg
queryWithList shared toMsg decoder query_ =
    queryInternal shared
        query_
        1000
        (Decode.field "rows" decoder)
        toMsg


{-| Since every account has a different set of tables, and every table needs a
different kind of scope, the `Account` type offers us a type-safe way of
declaring account, table and scope all at the same time, preventing impossible
states.

The `Account` type declares the account, and each account has another type in
their constructor, which defines the available tables. Each constructor in that
table type has an argument which is the type we're expected to send as a scope.

-}
type Account
    = Token TokenTable
    | MultiSig MultiSigTable


{-| All of the available tables for the `Token` account.

  - `Accounts`: Takes in a username as scope. Returns data about their balance
    in all communities they're a member of.
  - `Stat`: Takes in the community symbol. Returns data about the token, like
    `maxSupply` and `minBalance`.
  - `ExpiryOpts`: The scope is the token account itself. Returns data
    related to the expiration period of a token.

-}
type TokenTable
    = Accounts Eos.Account.Name
    | Stat Eos.Symbol
    | ExpiryOpts


{-| All of the available tables for the `MultiSig` account.

  - `Proposal`: takes in the name of the user who proposed the transaction.
    Returns all proposals submitted by the user
  - `Approvals2`: takes in the name of the user who proposed the transaction.
    Returns all pending approvals and provided approvals

-}
type MultiSigTable
    = Proposal Eos.Account.Name
    | Approvals2 Eos.Account.Name



-- SPECIAL QUERIES


getAccount : Shared -> Eos.Account.Name -> (Result Http.Error Eos.Permission.Permissions -> msg) -> Cmd msg
getAccount shared accountName toMsg =
    Http.post
        { url = blockchainUrl shared [ "chain", "get_account" ] []
        , body = Http.jsonBody (Encode.object [ ( "account_name", Eos.Account.encodeName accountName ) ])
        , expect = Http.expectJson toMsg (Decode.field "permissions" Eos.Permission.decoder)
        }



-- INTERNAL HELPERS


blockchainUrl : Shared -> List String -> List Url.Builder.QueryParameter -> String
blockchainUrl { endpoints } paths queryParams =
    Url.Builder.crossOrigin endpoints.eosio
        ("v1" :: paths)
        queryParams



-- INTERNAL QUERY HELPERS


queryInternal : Shared -> Account -> Int -> Decode.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
queryInternal shared query_ limit decoder toMsg =
    Http.post
        { url = blockchainUrl shared [ "chain", "get_table_rows" ] []
        , body = encodeQuery shared query_ limit |> Http.jsonBody
        , expect = Http.expectJson toMsg decoder
        }


encodeQuery : Shared -> Account -> Int -> Encode.Value
encodeQuery shared query_ limit =
    let
        { table, scope } =
            queryTableAndScope shared query_
    in
    encodeTableInfo
        { contract = queryContract shared query_
        , scope = scope
        , table = table
        , limit = limit
        }


queryContract : Shared -> Account -> String
queryContract shared query_ =
    case query_ of
        Token _ ->
            shared.contracts.token

        MultiSig _ ->
            "eosio.msig"


queryTableAndScope : Shared -> Account -> { table : String, scope : String }
queryTableAndScope shared query_ =
    case query_ of
        Token (Accounts accountName) ->
            { table = "accounts", scope = Eos.Account.nameToString accountName }

        Token (Stat symbol) ->
            { table = "stat", scope = Eos.symbolToSymbolCodeString symbol }

        Token ExpiryOpts ->
            { table = "expiryopts", scope = shared.contracts.token }

        MultiSig (Proposal accountName) ->
            { table = "proposal", scope = Eos.Account.nameToString accountName }

        MultiSig (Approvals2 accountName) ->
            { table = "approvals2", scope = Eos.Account.nameToString accountName }


encodeTableInfo : { contract : String, scope : String, table : String, limit : Int } -> Encode.Value
encodeTableInfo tableInfo =
    Encode.object
        [ ( "code", Encode.string tableInfo.contract )
        , ( "scope", Encode.string tableInfo.scope )
        , ( "table", Encode.string tableInfo.table )
        , ( "limit", Encode.int tableInfo.limit )
        , ( "json", Encode.bool True )
        ]



-- INTERNAL TRANSACT HELPERS


encodedEmptyList : Encode.Value
encodedEmptyList =
    Encode.list (\_ -> Encode.null) []


encodeAction : Shared -> Eos.Permission.Authorization -> Action -> Encode.Value
encodeAction shared authorization action =
    let
        actionData_ =
            actionData shared authorization action
    in
    Encode.object
        [ ( "account", Encode.string actionData_.contract )
        , ( "name", Encode.string actionData_.actionName )
        , ( "authorization", Encode.list Eos.Permission.encodeAuthorization [ authorization ] )
        , ( "data", actionData_.encodedData )
        ]


actionData : Shared -> Eos.Permission.Authorization -> Action -> { contract : String, actionName : String, encodedData : Encode.Value }
actionData shared authorization action =
    case action of
        CommunityAction ((CreateObjective _) as communityAction) ->
            let
                actionName =
                    "newobjective"
            in
            { contract = shared.contracts.community
            , actionName = actionName
            , encodedData = encodeCommunityAction communityAction
            }

        MultiSigAction multisigAction ->
            let
                actionName =
                    case multisigAction of
                        Propose _ ->
                            "propose"

                        Approve _ ->
                            "approve"

                        Unapprove _ ->
                            "unapprove"

                        Execute _ ->
                            "exec"

                        Cancel _ ->
                            "cancel"
            in
            { contract = "eosio.msig"
            , actionName = actionName
            , encodedData = encodeMultisigAction shared authorization multisigAction
            }

        EosAction ((UpdateAuth _) as eosAction) ->
            let
                actionName =
                    "updateauth"
            in
            { contract = "eosio"
            , actionName = actionName
            , encodedData = encodeEosAction eosAction
            }


encodeCommunityAction : CommunityAction -> Encode.Value
encodeCommunityAction communityAction =
    case communityAction of
        CreateObjective objective ->
            Encode.object
                [ ( "cmm_asset"
                  , Eos.encodeAsset { amount = 0, symbol = objective.communitySymbol }
                  )
                , ( "description", Encode.string objective.objectiveDescription )
                , ( "creator", Eos.Account.encodeName objective.communityAdmin )
                ]


encodeMultisigAction : Shared -> Eos.Permission.Authorization -> MultiSigAction -> Encode.Value
encodeMultisigAction shared authorization multisigAction =
    case multisigAction of
        Propose proposal ->
            Encode.object
                [ ( "proposer", Eos.Account.encodeName proposal.proposer )
                , ( "proposal_name", Encode.string proposal.proposalName )
                , ( "requested", Encode.list Eos.Permission.encodeAuthorization proposal.requestedVotes )
                , ( "trx"
                  , Encode.object
                        [ ( "expiration"
                          , proposal.expiration
                                |> Iso8601.fromTime
                                |> String.toList
                                -- Eos doesn't support milliseconds, so we remove that information
                                |> List.Extra.dropWhileRight (\c -> c /= '.')
                                |> List.Extra.dropWhileRight (not << Char.isDigit)
                                |> String.fromList
                                |> Encode.string
                          )
                        , ( "ref_block_num", Encode.int 0 )
                        , ( "ref_block_prefix", Encode.int 0 )
                        , ( "max_net_usage_words", Encode.int 0 )
                        , ( "max_cpu_usage_ms", Encode.int 0 )
                        , ( "delay_sec", Encode.int 0 )
                        , ( "context_free_actions", encodedEmptyList )
                        , ( "actions"
                          , Encode.list
                                (\( action, authorization_ ) -> encodeAction shared authorization_ action)
                                proposal.actions
                          )
                        , ( "transaction_extensions", encodedEmptyList )
                        ]
                  )
                ]

        Approve approval ->
            Encode.object
                [ ( "proposer", Eos.Account.encodeName approval.proposer )
                , ( "proposal_name", Encode.string approval.proposalName )
                , ( "level", Eos.Permission.encodeAuthorization authorization )
                ]

        Unapprove unapproval ->
            Encode.object
                [ ( "proposer", Eos.Account.encodeName unapproval.proposer )
                , ( "proposal_name", Encode.string unapproval.proposalName )
                , ( "level", Eos.Permission.encodeAuthorization authorization )
                ]

        Execute execution ->
            Encode.object
                [ ( "proposer", Eos.Account.encodeName execution.proposer )
                , ( "proposal_name", Encode.string execution.proposalName )
                , ( "executer", Eos.Account.encodeName authorization.actor )
                ]

        Cancel cancel ->
            Encode.object
                [ ( "proposer", Eos.Account.encodeName cancel.proposer )
                , ( "proposal_name", Encode.string cancel.proposalName )
                , ( "canceler", Eos.Account.encodeName authorization.actor )
                ]


encodeEosAction : EosAction -> Encode.Value
encodeEosAction (UpdateAuth updateAuth) =
    let
        encodeAccount account =
            Encode.object
                [ ( "permission"
                  , Eos.Permission.encodeAuthorization
                        { actor = account.account
                        , permission = account.permission
                        }
                  )
                , ( "weight", Encode.int account.weight )
                ]
    in
    Encode.object
        [ ( "account", Eos.Account.encodeName updateAuth.targetAccount )
        , ( "permission", Eos.Permission.encodePermissionType updateAuth.targetPermission )
        , ( "parent", Eos.Permission.encodePermissionType (Eos.Permission.parent updateAuth.targetPermission) )
        , ( "auth"
          , Encode.object
                [ ( "threshold", Encode.int updateAuth.threshold )
                , ( "keys", encodedEmptyList )
                , ( "waits", encodedEmptyList )
                , ( "accounts"
                  , updateAuth.accounts
                        -- EOS demands that these accounts are in alphabetical
                        -- order. Otherwise, the transaction fails
                        |> List.sortBy (\account -> Eos.Account.nameToString account.account)
                        |> Encode.list encodeAccount
                  )
                ]
          )
        ]
