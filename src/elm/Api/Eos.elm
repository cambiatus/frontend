module Api.Eos exposing
    ( query, querySingleItem, queryWithList, Account(..)
    , TokenTable(..), MultiSigTable(..)
    , transact, Authorization, Permission(..), Action(..)
    , CommunityAction(..), MultiSigAction(..)
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

@docs transact, Authorization, Permission, Action


### Actions

@docs CommunityAction, MultiSigAction

-}

import Eos
import Eos.Account
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
transact : Shared -> Authorization -> msg -> Action -> Ports.JavascriptOutModel msg
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

-}
type MultiSigAction
    = Propose
        { proposer : Eos.Account.Name
        , proposalName : String
        , requestedVotes : List Authorization
        , expiration : Time.Posix
        , actions : List ( Action, Authorization )
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


{-| This defines the key to use to sign a transaction. All accounts are
identified by an `Eos.Account.Name` (the `actor`). Every account may have
multiple (hierarchical) levels of `Permission`, each one associated with a key
pair (on multisig accounts, there may be multiple key pairs per permission level)
-}
type alias Authorization =
    { actor : Eos.Account.Name
    , permission : Permission
    }


{-| The standard permission levels are `Active` and `Owner`. `Active` is a child
of `Owner`, which means `Owner` has more "power" than `Active`. For example, you
can change the `Active` permission's auth data by signing as `Owner`, but you
can't change the `Owner` permission's auth data by signing as `Active`.
-}
type Permission
    = Active
    | Owner



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

  - `Proposal`: takes in the name of the user who proposed the transaction

-}
type MultiSigTable
    = Proposal Eos.Account.Name



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


encodeAuthorization : Authorization -> Encode.Value
encodeAuthorization authorization =
    let
        permissionName =
            case authorization.permission of
                Active ->
                    "active"

                Owner ->
                    "owner"
    in
    Encode.object
        [ ( "actor", Eos.Account.encodeName authorization.actor )
        , ( "permission", Encode.string permissionName )
        ]


encodedEmptyList : Encode.Value
encodedEmptyList =
    Encode.list (\_ -> Encode.null) []


encodeAction : Shared -> Authorization -> Action -> Encode.Value
encodeAction shared authorization action =
    let
        actionData_ =
            actionData shared authorization action
    in
    Encode.object
        [ ( "account", Encode.string actionData_.contract )
        , ( "name", Encode.string actionData_.actionName )
        , ( "authorization", Encode.list encodeAuthorization [ authorization ] )
        , ( "data", actionData_.encodedData )
        ]


actionData : Shared -> Authorization -> Action -> { contract : String, actionName : String, encodedData : Encode.Value }
actionData shared authorization action =
    case action of
        CommunityAction communityAction ->
            let
                actionName =
                    case communityAction of
                        CreateObjective _ ->
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
            in
            { contract = "eosio.msig"
            , actionName = actionName
            , encodedData = encodeMultisigAction shared authorization multisigAction
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


encodeMultisigAction : Shared -> Authorization -> MultiSigAction -> Encode.Value
encodeMultisigAction shared authorization multisigAction =
    case multisigAction of
        Propose proposal ->
            Encode.object
                [ ( "proposer", Eos.Account.encodeName proposal.proposer )
                , ( "proposal_name", Encode.string proposal.proposalName )
                , ( "requested", Encode.list encodeAuthorization proposal.requestedVotes )
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
                , ( "level", encodeAuthorization authorization )
                ]

        Unapprove unapproval ->
            Encode.object
                [ ( "proposer", Eos.Account.encodeName unapproval.proposer )
                , ( "proposal_name", Encode.string unapproval.proposalName )
                , ( "level", encodeAuthorization authorization )
                ]

        Execute execution ->
            Encode.object
                [ ( "proposer", Eos.Account.encodeName execution.proposer )
                , ( "proposal_name", Encode.string execution.proposalName )
                , ( "executer", Eos.Account.encodeName authorization.actor )
                ]
