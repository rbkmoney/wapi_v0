-module(wapi_bouncer_context).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-type fragment() :: bouncer_client:context_fragment().
-type acc() :: bouncer_context_helpers:context_fragment().

-type fragments() :: {acc(), _ExternalFragments :: #{_ID => fragment()}}.

-export_type([fragment/0]).
-export_type([acc/0]).
-export_type([fragments/0]).

-type prototypes() :: [
    {operation, prototype_operation()}
    | {wallet, prototype_wallet()}
].

-type prototype_operation() :: #{
    id => swag_server_wallet:operation_id(),
    party => maybe_entity_id(),
    identity => maybe_entity_id(),
    wallet => maybe_entity_id(),
    withdrawal => maybe_entity_id(),
    deposit => maybe_entity_id(),
    w2w_transfer => maybe_entity_id(),
    source => maybe_entity_id(),
    destination => maybe_entity_id(),
    report => maybe_entity_id(),
    file => maybe_entity_id(),
    webhook => maybe_entity_id()
}.

-type prototype_wallet() :: #{
    wallet => [wallet_entity()]
}.

-type wallet_entity() ::
    {identity, identity_data()}
    | {wallet, wallet_data()}
    | {withdrawal, withdrawal_data()}
    | {deposit, deposit_data()}
    | {w2w_transfer, w2w_transfer_data()}
    | {source, source_data()}
    | {destination, destination_data()}
    | {webhook, webhook_data()}
    | {report, report_data()}.

-type wallet_entity_type() ::
    identity
    | wallet
    | withdrawal
    | deposit
    | w2w_transfer
    | source
    | destination
    | webhook
    | webhook_filter
    | report
    | report_file.

-type identity_data() :: #{
    id => entity_id()
}.

-type wallet_data() :: #{
    id => entity_id(),
    party => entity_id(),
    cash => cash()
}.

-type withdrawal_data() :: #{
    id => entity_id(),
    party => entity_id()
}.

-type deposit_data() :: #{
    id => entity_id(),
    party => entity_id()
}.

-type w2w_transfer_data() :: #{
    id => entity_id(),
    party => entity_id()
}.

-type source_data() :: #{
    id => entity_id(),
    party => entity_id()
}.

-type destination_data() :: #{
    id => entity_id(),
    party => entity_id()
}.

-type webhook_data() :: #{
    id => entity_id(),
    identity => entity_id(),
    wallet => entity_id()
}.

-type report_data() :: #{
    id => entity_id(),
    identity => entity_id(),
    files => [entity_id()]
}.

-type entity_id() :: binary().
-type maybe_entity_id() :: entity_id() | undefined.
-type cash() :: #{amount := binary(), currency := binary()}.

-export_type([prototypes/0]).
-export_type([prototype_operation/0]).
-export_type([prototype_wallet/0]).
-export_type([wallet_entity_type/0]).

-export([new/0]).
-export([build/2]).
-export([build_wallet_entity/2]).
-export([build_wallet_entity/3]).

%%

-spec new() -> fragments().
new() ->
    {mk_base_fragment(), #{}}.

mk_base_fragment() ->
    bouncer_context_helpers:make_env_fragment(#{
        now => genlib_rfc3339:format(genlib_time:unow(), second),
        deployment => #{id => genlib_app:env(wapi, deployment, undefined)}
    }).

-spec build(prototypes(), fragments()) -> fragments().
build(Prototypes, {Acc0, External}) ->
    Acc1 = lists:foldl(fun({T, Params}, Acc) -> build(T, Params, Acc) end, Acc0, Prototypes),
    {Acc1, External}.

build(operation, Params = #{id := OperationID}, Acc) ->
    Acc#bctx_v1_ContextFragment{
        wapi = #bctx_v1_ContextWalletAPI{
            op = #bctx_v1_WalletAPIOperation{
                id = operation_id_to_binary(OperationID),
                party = maybe_with(party, Params),
                identity = maybe_with(identity, Params),
                wallet = maybe_with(wallet, Params),
                withdrawal = maybe_with(withdrawal, Params),
                deposit = maybe_with(deposit, Params),
                w2w_transfer = maybe_with(w2w_transfer, Params),
                source = maybe_with(source, Params),
                destination = maybe_with(destination, Params),
                report = maybe_with(report, Params),
                file = maybe_with(file, Params),
                webhook = maybe_with(webhook, Params)
            },
            grants = maybe_with(grants, Params, fun build_grants/1)
        }
    };
build(wallet, Params, Acc) when is_list(Params) ->
    Acc#bctx_v1_ContextFragment{
        wallet = build_set(lists:map(fun build_entity_ctx/1, Params))
    }.

-spec build_wallet_entity(wallet_entity_type(), map()) -> wallet_entity().
build_wallet_entity(Type, Data) ->
    build_wallet_entity(Type, Data, undefined).

-spec build_wallet_entity(wallet_entity_type(), map() | undefined, entity_id() | undefined) -> wallet_entity().
build_wallet_entity(identity, #{<<"id">> := ID}, PartyID) ->
    {identity, #{id => ID, party => PartyID}};
build_wallet_entity(wallet, #{<<"id">> := ID}, PartyID) ->
    {wallet, #{id => ID, party => PartyID}};
build_wallet_entity(withdrawal, #{<<"id">> := ID}, PartyID) ->
    {withdrawal, #{id => ID, party => PartyID}};
build_wallet_entity(deposit, #{<<"id">> := ID, <<"wallet">> := WalletID}, _) ->
    {deposit, #{id => ID, wallet => WalletID}};
build_wallet_entity(w2w_transfer, #{<<"id">> := ID}, PartyID) ->
    {w2w_transfer, #{id => ID, party => PartyID}};
build_wallet_entity(source, #{<<"id">> := ID}, PartyID) ->
    {source, #{id => ID, party => PartyID}};
build_wallet_entity(destination, #{<<"id">> := ID}, PartyID) ->
    {destination, #{id => ID, party => PartyID}};
build_wallet_entity(webhook, Webhook = #{<<"id">> := ID, <<"identityID">> := Identity}, _) ->
    Scope = maybe_with(<<"scope">>, Webhook),
    WalletID = maybe_with(<<"walletID">>, Scope),
    {webhook, #{id => ID, identity => Identity, wallet => WalletID}};
build_wallet_entity(report, #{<<"id">> := ID, <<"files">> := Files}, IdentityID) ->
    {report, #{id => ID, identity => IdentityID, files => lists:map(fun(#{<<"id">> := FileID}) -> FileID end, Files)}};
build_wallet_entity(report_file, #{<<"id">> := ID}, _) ->
    {report_file, #{id => ID}};
build_wallet_entity(_, undefined, _) ->
    undefined.

%%

build_entity_ctx({identity, Data}) ->
    #bctx_v1_Entity{
        id = maybe_with(id, Data),
        type = <<"Identity">>,
        party = maybe_with(party, Data)
    };
build_entity_ctx({wallet, Data}) ->
    #bctx_v1_Entity{
        id = maybe_with(id, Data),
        type = <<"Wallet">>,
        party = maybe_with(party, Data),
        wallet = #bctx_v1_WalletAttrs{
            body = maybe_with(cash, Data, fun build_cash/1)
        }
    };
build_entity_ctx({withdrawal, Data}) ->
    #bctx_v1_Entity{
        id = maybe_with(id, Data),
        type = <<"Withdrawal">>,
        party = maybe_with(party, Data)
    };
build_entity_ctx({deposit, Data}) ->
    #bctx_v1_Entity{
        id = maybe_with(id, Data),
        type = <<"Deposit">>,
        wallet = #bctx_v1_WalletAttrs{
            wallet = maybe_with(wallet, Data)
        }
    };
build_entity_ctx({w2w_transfer, Data}) ->
    #bctx_v1_Entity{
        id = maybe_with(id, Data),
        type = <<"W2WTransfer">>,
        party = maybe_with(party, Data)
    };
build_entity_ctx({source, Data}) ->
    #bctx_v1_Entity{
        id = maybe_with(id, Data),
        type = <<"Source">>,
        party = maybe_with(party, Data)
    };
build_entity_ctx({destination, Data}) ->
    #bctx_v1_Entity{
        id = maybe_with(id, Data),
        type = <<"Destination">>,
        party = maybe_with(party, Data)
    };
build_entity_ctx({webhook, Data}) ->
    #bctx_v1_Entity{
        id = maybe_with(id, Data),
        type = <<"WalletWebhook">>,
        wallet = #bctx_v1_WalletAttrs{
            identity = maybe_with(identity, Data),
            wallet = maybe_with(wallet, Data)
        }
    };
build_entity_ctx({report, Data}) ->
    #bctx_v1_Entity{
        id = maybe_with(id, Data),
        type = <<"WalletReport">>,
        wallet = #bctx_v1_WalletAttrs{
            identity = maybe_with(identity, Data),
            report = maybe_with(files, Data, fun build_report_attrs/1)
        }
    }.

%%

maybe_with(Name, Params) ->
    maybe_with(Name, Params, fun(V) -> V end).

maybe_with(Name, Params, Then) ->
    case maps:get(Name, Params, undefined) of
        V when V /= undefined ->
            Then(V);
        undefined ->
            undefined
    end.

operation_id_to_binary(V) ->
    erlang:atom_to_binary(V, utf8).

build_grants(Grants) when is_list(Grants) ->
    build_set(lists:map(fun build_grant/1, Grants)).

build_grant(Grant) ->
    #bctx_v1_WalletGrant{
        wallet = maybe_with(wallet, Grant),
        destination = maybe_with(destination, Grant),
        body = maybe_with(body, Grant, fun build_cash/1),
        created_at = maybe_with(created_at, Grant),
        expires_on = maybe_with(expires_on, Grant)
    }.

build_cash(Cash) ->
    #bctx_v1_Cash{
        amount = maybe_with(amount, Cash),
        currency = maybe_with(currency, Cash)
    }.

build_set(L) when is_list(L) ->
    ordsets:from_list(L).

build_report_attrs(Attrs) when is_list(Attrs) ->
    #bctx_v1_WalletReportAttrs{
        files = build_set(Attrs)
    }.
