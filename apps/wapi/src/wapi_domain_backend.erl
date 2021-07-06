-module(wapi_domain_backend).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-type response_data() :: wapi_handler:response_data().

-export([get_currency/1]).

%%

-type id() :: binary().

-export([object/1]).
-export([object/2]).

-type object_data() :: any().
-type object_ref() :: dmsl_domain_thrift:'Reference'().

%% Pipeline

-import(wapi_pipeline, [do/1, unwrap/1]).

%%

-spec get_currency(id()) -> {ok, response_data()} | {error, notfound}.
get_currency(ID) ->
    do(fun() ->
        Currency = unwrap(
            object(
                latest,
                {currency, #domain_CurrencyRef{symbolic_code = ID}}
            )
        ),
        #{
            <<"id">> => genlib_string:to_upper(genlib:to_binary(ID)),
            <<"name">> => Currency#domain_Currency.name,
            <<"numericCode">> => genlib:to_binary(Currency#domain_Currency.numeric_code),
            <<"exponent">> => Currency#domain_Currency.exponent
        }
    end).

%%
%% Internal
%%

-spec object(object_ref()) -> {ok, object_data()} | {error, notfound}.
object(ObjectRef) ->
    object(latest, ObjectRef).

-spec object(dmt_client:version(), object_ref()) -> {ok, object_data()} | {error, notfound}.
object(Ref, {Type, ObjectRef}) ->
    try dmt_client:checkout_object(Ref, {Type, ObjectRef}) of
        {Type, {_RecordName, ObjectRef, ObjectData}} ->
            {ok, ObjectData}
    catch
        #'ObjectNotFound'{} ->
            {error, notfound}
    end.
