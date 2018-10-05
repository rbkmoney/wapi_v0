%%%
%%% Transfer machine
%%%

-module(ff_transfer_machine).

%% API

-type id()        :: machinery:id().
-type ns()        :: machinery:namespace().
-type ctx()       :: ff_ctx:ctx().
-type transfer(T) :: ff_transfer:transfer(T).
-type account()   :: ff_account:account().
-type event(T)    :: T.
-type events(T)   :: [{integer(), ff_machine:timestamped_event(event(T))}].

%% Behaviour definition

-type st(T) :: ff_machine:st(transfer(T)).

-export_type([id/0]).
-export_type([ns/0]).
-export_type([st/1]).
-export_type([event/1]).
-export_type([events/1]).

-callback process_transfer(transfer(_)) ->
    {ok, [event(_)] | poll} |
    {error, _Reason}.

-callback process_call(_CallArgs, transfer(_)) ->
    {ok, [event(_)] | poll} |
    {error, _Reason}.

-optional_callbacks([process_call/2]).

%% API

-export([create/4]).
-export([get/2]).
-export([events/3]).

%% Accessors

-export([transfer/1]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-type params() :: #{
    handler     := ff_transfer:handler(),
    source      := account(),
    destination := account(),
    body        := ff_transaction:body(),
    params      := ff_transfer:params()
}.

-spec create(ns(), id(), params(), ctx()) ->
    ok |
    {error,
        _TransferError |
        exists
    }.

create(NS, ID,
    #{handler := Handler, source := Source, destination := Destination, body := Body, params := Params},
Ctx)
->
    do(fun () ->
        Events = unwrap(ff_transfer:create(Handler, ID, Source, Destination, Body, Params)),
        unwrap(machinery:start(NS, ID, {Events, Ctx}, backend(NS)))
    end).

-spec get(ns(), id()) ->
    {ok, st(_)}      |
    {error, notfound}.

get(NS, ID) ->
    ff_machine:get(ff_transfer, NS, ID).

-spec events(ns(), id(), machinery:range()) ->
    {ok, events(_)} |
    {error, notfound}.

events(NS, ID, Range) ->
    do(fun () ->
        #{history := History} = unwrap(machinery:get(NS, ID, Range, backend(NS))),
        [{EventID, TsEv} || {EventID, _, TsEv} <- History]
    end).

backend(NS) ->
    fistful:backend(NS).

%% Accessors

-spec transfer(st(T)) ->
    transfer(T).

transfer(St) ->
    ff_machine:model(St).

%% Machinery

-type machine()      :: ff_machine:machine(event(_)).
-type result()       :: ff_machine:result(event(_)).
-type handler_opts() :: machinery:handler_opts(_).

-spec init({[event(_)], ctx()}, machine(), _, handler_opts()) ->
    result().

init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events    => ff_machine:emit_events(Events),
        action    => continue,
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

process_timeout(Machine, _, _Opts) ->
    St = ff_machine:collapse(ff_transfer, Machine),
    Transfer = transfer(St),
    process_result((ff_transfer:handler(Transfer)):process_transfer(Transfer), St).

-spec process_call(_CallArgs, machine(), _, handler_opts()) ->
    {ok, result()}.

process_call(CallArgs, Machine, _, _Opts) ->
    St = ff_machine:collapse(ff_transfer, Machine),
    Transfer = transfer(St),
    {ok, process_result((ff_transfer:handler(Transfer)):process_call(CallArgs, Transfer), St)}.

process_result({ok, poll}, St) ->
    #{
        action => set_poll_timer(St)
    };
process_result({ok, Events}, _St) ->
    #{
        events => ff_machine:emit_events(Events),
        action => continue
    };
process_result({error, Reason}, _St) ->
    #{
        events => emit_failure(Reason)
    }.

set_poll_timer(St) ->
    Now = machinery_time:now(),
    Timeout = erlang:max(1, machinery_time:interval(Now, ff_machine:updated(St)) div 1000),
    {set_timer, {timeout, Timeout}}.

emit_failure(Reason) ->
    ff_machine:emit_event({status_changed, {failed, Reason}}).