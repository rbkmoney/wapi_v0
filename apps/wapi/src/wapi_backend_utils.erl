-module(wapi_backend_utils).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_w2w_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").

-define(EXTERNAL_ID, <<"externalID">>).
-define(CTX_NS, <<"com.rbkmoney.wapi">>).
-define(BENDER_DOMAIN, <<"wapi">>).

%% Context
-type context() :: #{namespace() => md()}.
-type namespace() :: binary().
%% as stolen from `machinery_msgpack`
-type md() ::
    nil
    | boolean()
    | integer()
    | float()
    %% string
    | binary()
    %% binary
    | {binary, binary()}
    | [md()]
    | #{md() => md()}.

-type handler_context() :: wapi_handler:context().
-type id() :: binary().
-type hash() :: integer().
-type params() :: map().
-type gen_type() ::
    identity
    | identity_challenge
    | wallet
    | destination
    | withdrawal
    | w2w_transfer.
-type entity_type() ::
    identity
    | wallet
    | destination
    | withdrawal
    | w2w_transfer.
-type entity_state() ::
    ff_proto_identity_thrift:'IdentityState'()
    | ff_proto_wallet_thrift:'WalletState'()
    | ff_proto_destination_thrift:'DestinationState'()
    | ff_proto_w2w_transfer_thrift:'W2WTransferState'()
    | ff_proto_withdrawal_thrift:'WithdrawalState'().

-export([gen_id/3]).
-export([gen_id/4]).
-export([make_ctx/2]).
-export([add_to_ctx/2]).
-export([add_to_ctx/3]).
-export([get_from_ctx/2]).
-export([get_idempotent_key/3]).
-export([issue_grant_token/3]).
-export([create_params_hash/1]).
-export([decode_resource/1]).
-export([tokenize_resource/1]).
-export([get_entity_owner/2]).

%% Pipeline

-spec get_idempotent_key(gen_type(), id(), id() | undefined) -> binary().
get_idempotent_key(Type, PartyID, ExternalID) ->
    bender_client:get_idempotent_key(?BENDER_DOMAIN, Type, PartyID, ExternalID).

-spec gen_id(gen_type(), params(), handler_context()) -> {ok, id()} | {error, {external_id_conflict, id()}}.
gen_id(Type, Params, Context) ->
    ExternalID = maps:get(?EXTERNAL_ID, Params, undefined),
    Hash = create_params_hash(Params),
    gen_id(Type, ExternalID, Hash, Context).

-spec gen_id(gen_type(), id() | undefined, hash(), handler_context()) ->
    {ok, id()} | {error, {external_id_conflict, id()}}.
gen_id(Type, ExternalID, Hash, Context) ->
    PartyID = wapi_handler_utils:get_owner(Context),
    IdempotentKey = bender_client:get_idempotent_key(?BENDER_DOMAIN, Type, PartyID, ExternalID),
    gen_id_by_type(Type, IdempotentKey, Hash, Context).

%@TODO: Bring back later
%gen_id_by_type(withdrawal = Type, IdempotentKey, Hash, Context) ->
%    gen_snowflake_id(Type, IdempotentKey, Hash, Context);
gen_id_by_type(Type, IdempotentKey, Hash, Context) ->
    gen_sequence_id(Type, IdempotentKey, Hash, Context).

%@TODO: Bring back later
%gen_snowflake_id(_Type, IdempotentKey, Hash, #{woody_context := WoodyCtx}) ->
%    bender_client:gen_snowflake(IdempotentKey, Hash, WoodyCtx).
gen_sequence_id(Type, IdempotentKey, Hash, #{woody_context := WoodyCtx}) ->
    BinType = atom_to_binary(Type, utf8),
    case bender_client:gen_sequence(IdempotentKey, BinType, Hash, WoodyCtx) of
        % No need for IntegerID at this project so far
        {ok, {ID, _IntegerID}} ->
            {ok, ID};
        {error, {external_id_conflict, {ID, _IntegerID}}} ->
            {error, {external_id_conflict, ID}}
    end.

-spec make_ctx(params(), handler_context()) -> context().
make_ctx(Params, Context) ->
    #{
        ?CTX_NS => genlib_map:compact(#{
            <<"owner">> => wapi_handler_utils:get_owner(Context),
            <<"metadata">> => maps:get(<<"metadata">>, Params, undefined)
        })
    }.

-spec add_to_ctx({md(), md() | undefined} | list() | map(), context()) -> context().
add_to_ctx({Key, Value}, Context) ->
    add_to_ctx(Key, Value, Context);
add_to_ctx(Map, Context = #{?CTX_NS := Ctx}) when is_map(Map) ->
    Context#{?CTX_NS => maps:merge(Ctx, Map)};
add_to_ctx(KVList, Context) when is_list(KVList) ->
    lists:foldl(
        fun({K, V}, Ctx) -> add_to_ctx(K, V, Ctx) end,
        Context,
        KVList
    ).

-spec add_to_ctx(md(), md() | undefined, context()) -> context().
add_to_ctx(_Key, undefined, Context) ->
    Context;
add_to_ctx(Key, Value, Context = #{?CTX_NS := Ctx}) ->
    Context#{?CTX_NS => Ctx#{Key => Value}}.

-spec get_from_ctx(md(), context()) -> md().
get_from_ctx(Key, #{?CTX_NS := Ctx}) ->
    maps:get(Key, Ctx, undefined).

-spec issue_grant_token(_, binary(), handler_context()) -> {ok, binary()} | {error, expired}.
issue_grant_token(TokenSpec, Expiration, Context) ->
    case get_expiration_deadline(Expiration) of
        {ok, Deadline} ->
            {ok, wapi_tokens_legacy:issue_access_token(wapi_handler_utils:get_owner(Context), TokenSpec, Deadline)};
        Error = {error, _} ->
            Error
    end.

get_expiration_deadline(Expiration) ->
    Deadline = genlib_rfc3339:parse(Expiration, second),
    case genlib_time:unow() - Deadline < 0 of
        true ->
            {ok, Deadline};
        false ->
            {error, expired}
    end.

-spec create_params_hash(term()) -> integer().
create_params_hash(Value) ->
    erlang:phash2(Value).

-spec decode_resource(binary()) ->
    {ok, wapi_crypto:resource()} | {error, unrecognized} | {error, lechiffre:decoding_error()}.
decode_resource(Token) ->
    case wapi_crypto:decrypt_resource_token(Token) of
        {ok, {Resource, Deadline}} ->
            case wapi_utils:deadline_is_reached(Deadline) of
                true ->
                    {error, expired};
                _ ->
                    {ok, Resource}
            end;
        unrecognized ->
            {error, unrecognized};
        {error, Error} ->
            {error, Error}
    end.

-spec tokenize_resource(wapi_crypto:resource() | term()) -> integer().
tokenize_resource({bank_card, BankCard}) ->
    Map = genlib_map:compact(#{
        token => BankCard#'BankCard'.token,
        bin => BankCard#'BankCard'.bin,
        masked_pan => BankCard#'BankCard'.masked_pan,
        cardholder_name => BankCard#'BankCard'.cardholder_name,
        %% ExpDate is optional in swag_wallets 'StoreBankCard'. But some adapters waiting exp_date.
        %% Add error, somethink like BankCardReject.exp_date_required
        exp_date =>
            case BankCard#'BankCard'.exp_date of
                undefined -> undefined;
                #'BankCardExpDate'{month = Month, year = Year} -> {Month, Year}
            end
    }),
    create_params_hash(Map);
tokenize_resource(Value) ->
    create_params_hash(Value).

-spec get_entity_owner(entity_type(), entity_state()) -> {ok, id()}.
get_entity_owner(Type, State) ->
    {ok, get_context_owner(get_context_from_state(Type, State))}.

get_context_from_state(identity, #idnt_IdentityState{context = Context}) ->
    Context;
get_context_from_state(wallet, #wlt_WalletState{context = Context}) ->
    Context;
get_context_from_state(destination, #dst_DestinationState{context = Context}) ->
    Context;
get_context_from_state(w2w_transfer, #w2w_transfer_W2WTransferState{context = Context}) ->
    Context;
get_context_from_state(withdrawal, #wthd_WithdrawalState{context = Context}) ->
    Context.

get_context_owner(ContextThrift) ->
    Context = wapi_codec:unmarshal(context, ContextThrift),
    wapi_backend_utils:get_from_ctx(<<"owner">>, Context).
