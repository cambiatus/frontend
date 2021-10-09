module Api.Eos exposing
    ( query, querySingleItem, queryWithList, Account(..)
    , TokenTable(..), MultiSigTable(..)
    , transact
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

@docs transact

-}

import Eos
import Eos.Account
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Session.Shared exposing (Shared)
import Url.Builder



-- TRANSACT


{-| This is how we write to EOS
-}
transact : a -> Cmd msg
transact loggedIn =
    Cmd.none



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
