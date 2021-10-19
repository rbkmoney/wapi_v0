-module(wapi_access_backend).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_w2w_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").

-export([get_resource_owner/2]).
-export([check_resource_by_id/3]).

-type id() :: binary().
-type resource_type() ::
    identity
    | wallet
    | destination
    | withdrawal
    | w2w_transfer.

-type handler_context() :: wapi_handler:context().
-type data() ::
    ff_proto_identity_thrift:'IdentityState'()
    | ff_proto_wallet_thrift:'WalletState'().

-define(CTX_NS, <<"com.rbkmoney.wapi">>).

%% Pipeline

-spec get_resource_owner(resource_type(), data()) -> {ok, id()}.
get_resource_owner(Resource, Data) ->
    {ok, get_owner(get_context_from_state(Resource, Data))}.

-spec check_resource_by_id(resource_type(), id(), handler_context()) -> ok | {error, notfound}.
check_resource_by_id(Resource, ID, HandlerContext) ->
    case get_context_by_id(Resource, ID, HandlerContext) of
        {error, notfound} = Error ->
            Error;
        _ ->
            ok
    end.

%%
%% Internal
%%

get_context_by_id(identity, IdentityID, WoodyCtx) ->
    Request = {fistful_identity, 'GetContext', {IdentityID}},
    case wapi_handler_utils:service_call(Request, WoodyCtx) of
        {ok, Context} ->
            Context;
        {exception, #fistful_IdentityNotFound{}} ->
            {error, notfound}
    end;
get_context_by_id(wallet, WalletID, WoodyCtx) ->
    Request = {fistful_wallet, 'GetContext', {WalletID}},
    case wapi_handler_utils:service_call(Request, WoodyCtx) of
        {ok, Context} ->
            Context;
        {exception, #fistful_WalletNotFound{}} ->
            {error, notfound}
    end;
get_context_by_id(destination, DestinationID, WoodyCtx) ->
    Request = {fistful_destination, 'GetContext', {DestinationID}},
    case wapi_handler_utils:service_call(Request, WoodyCtx) of
        {ok, Context} ->
            Context;
        {exception, #fistful_DestinationNotFound{}} ->
            {error, notfound}
    end;
get_context_by_id(w2w_transfer, W2WTransferID, WoodyCtx) ->
    Request = {fistful_w2w_transfer, 'GetContext', {W2WTransferID}},
    case wapi_handler_utils:service_call(Request, WoodyCtx) of
        {ok, Context} ->
            Context;
        {exception, #fistful_W2WNotFound{}} ->
            {error, notfound}
    end;
get_context_by_id(withdrawal, WithdrawalID, WoodyCtx) ->
    Request = {fistful_withdrawal, 'GetContext', {WithdrawalID}},
    case wapi_handler_utils:service_call(Request, WoodyCtx) of
        {ok, Context} ->
            Context;
        {exception, #fistful_WithdrawalNotFound{}} ->
            {error, notfound}
    end.

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

get_owner(ContextThrift) ->
    Context = wapi_codec:unmarshal(context, ContextThrift),
    wapi_backend_utils:get_from_ctx(<<"owner">>, Context).
