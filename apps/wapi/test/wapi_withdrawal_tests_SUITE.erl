-module(wapi_withdrawal_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([
    create/1,
    get/1,
    get_by_external_id/1,
    create_quote/1,
    get_events/1,
    get_event/1
]).

% common-api is used since it is the domain used in production RN
% TODO: change to wallet-api (or just omit since it is the default one) when new tokens will be a thing
-define(DOMAIN, <<"common-api">>).
-define(badresp(Code), {error, {invalid_response_code, Code}}).
-define(emptyresp(Code), {error, {Code, #{}}}).

-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].
-type group_name()      :: atom().

-behaviour(supervisor).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() ->
    [test_case_name()].
all() ->
    [
        {group, base}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {base, [],
            [
                create,
                get,
                get_by_external_id,
                create_quote,
                get_events,
                get_event
            ]
        }
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    wapi_ct_helper:init_suite(?MODULE, C).

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    _ = wapi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    _ = [application:stop(App) || App <- ?config(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(Group, Config) when Group =:= base ->
    ok = wapi_context:save(wapi_context:create(#{
        party_client => party_client:create_client(),
        woody_context => woody_context:new(<<"init_per_group/", (atom_to_binary(Group, utf8))/binary>>)
    })),
    Party = genlib:bsuuid(),
    {ok, Token} = wapi_ct_helper:issue_token(Party, [{[party], write}], unlimited, ?DOMAIN),
    Config1 = [{party, Party} | Config],
    [{context, wapi_ct_helper:get_context(Token)} | Config1];
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) ->
    _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(Name, C) ->
    C1 = wapi_ct_helper:makeup_cfg([wapi_ct_helper:test_case_name(Name), wapi_ct_helper:woody_ctx()], C),
    ok = wapi_context:save(C1),
    [{test_sup, wapi_ct_helper:start_mocked_service_sup(?MODULE)} | C1].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    ok = wapi_context:cleanup(),
    wapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec create(config()) ->
    _.
create(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {bender_thrift, fun('GenerateID', _) -> {ok, ?GENERATE_ID_RESULT} end},
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('Create', _) -> {ok, ?WITHDRAWAL(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:create_withdrawal/3,
        #{
            body => genlib_map:compact(#{
                <<"wallet">> => ?STRING,
                <<"destination">> => ?STRING,
                <<"body">> => #{
                    <<"amount">> => 100,
                    <<"currency">> => <<"RUB">>
                }
        })},
        wapi_ct_helper:cfg(context, C)
    ).

-spec get(config()) ->
    _.
get(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_withdrawal, fun('Get', _) -> {ok, ?WITHDRAWAL(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:get_withdrawal/3,
        #{
            binding => #{
                <<"withdrawalID">> => ?STRING
            }
        },
    wapi_ct_helper:cfg(context, C)
).

-spec get_by_external_id(config()) ->
    _.
get_by_external_id(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {bender_thrift, fun('GetInternalID', _) -> {ok, ?GET_INTERNAL_ID_RESULT} end},
        {fistful_withdrawal, fun('Get', _) -> {ok, ?WITHDRAWAL(PartyID)} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:get_withdrawal_by_external_id/3,
        #{
            binding => #{
                <<"externalID">> => ?STRING
            }
        },
    wapi_ct_helper:cfg(context, C)
).

-spec create_quote(config()) ->
    _.
create_quote(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('GetQuote', _) -> {ok, ?WITHDRAWAL_QUOTE} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:create_quote/3,
        #{
            body => genlib_map:compact(#{
                <<"walletID">> => ?STRING,
                <<"destinationID">> => ?STRING,
                <<"currencyFrom">> => <<"RUB">>,
                <<"currencyTo">> => <<"USD">>,
                <<"cash">> => #{
                    <<"amount">> => 100,
                    <<"currency">> => <<"RUB">>
                }
        })},
        wapi_ct_helper:cfg(context, C)
    ).

-spec get_events(config()) ->
    _.
get_events(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_withdrawal,
            fun
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
                ('GetEvents', [_, #'EventRange'{limit = 0}]) -> {ok, []};
                ('GetEvents', _) -> {ok, [?WITHDRAWAL_EVENT(?WITHDRAWAL_STATUS_CHANGE)]}
            end
        }
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:poll_withdrawal_events/3,
        #{
            binding => #{
                <<"withdrawalID">> => ?STRING
            },
            qs_val => #{
                <<"limit">> => 10
            }
        },
    wapi_ct_helper:cfg(context, C)
).

-spec get_event(config()) ->
    _.
get_event(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_withdrawal,
            fun
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
                ('GetEvents', [_, #'EventRange'{limit = 0}]) -> {ok, []};
                ('GetEvents', _) -> {ok, [?WITHDRAWAL_EVENT(?WITHDRAWAL_STATUS_CHANGE)]}
            end
        }
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:get_withdrawal_events/3,
        #{
            binding => #{
                <<"withdrawalID">> => ?STRING,
                <<"eventID">> => ?INTEGER
            }
        },
    wapi_ct_helper:cfg(context, C)
).

-spec create_quote(config()) ->
    _.
create_quote(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_wallet, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_destination, fun('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)} end},
        {fistful_withdrawal, fun('GetQuote', _) -> {ok, ?WITHDRAWAL_QUOTE} end}
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:create_quote/3,
        #{
            body => genlib_map:compact(#{
                <<"walletID">> => ?STRING,
                <<"destinationID">> => ?STRING,
                <<"currencyFrom">> => <<"RUB">>,
                <<"currencyTo">> => <<"USD">>,
                <<"cash">> => #{
                    <<"amount">> => 100,
                    <<"currency">> => <<"RUB">>
                }
        })},
        ct_helper:cfg(context, C)
    ).

-spec get_events(config()) ->
    _.
get_events(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_withdrawal,
            fun
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
                ('GetEvents', [_, #'EventRange'{limit = 0}]) -> {ok, []};
                ('GetEvents', _) -> {ok, [?WITHDRAWAL_EVENT(?WITHDRAWAL_STATUS_CHANGE)]}
            end
        }
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:poll_withdrawal_events/3,
        #{
            binding => #{
                <<"withdrawalID">> => ?STRING
            },
            qs_val => #{
                <<"limit">> => 10
            }
        },
    ct_helper:cfg(context, C)
).

-spec get_event(config()) ->
    _.
get_event(C) ->
    PartyID = ?config(party, C),
    wapi_ct_helper:mock_services([
        {fistful_withdrawal,
            fun
                ('GetContext', _) -> {ok, ?DEFAULT_CONTEXT(PartyID)};
                ('GetEvents', [_, #'EventRange'{limit = 0}]) -> {ok, []};
                ('GetEvents', _) -> {ok, [?WITHDRAWAL_EVENT(?WITHDRAWAL_STATUS_CHANGE)]}
            end
        }
    ], C),
    {ok, _} = call_api(
        fun swag_client_wallet_withdrawals_api:get_withdrawal_events/3,
        #{
            binding => #{
                <<"withdrawalID">> => ?STRING,
                <<"eventID">> => ?INTEGER
            }
        },
    ct_helper:cfg(context, C)
).

%%

-spec call_api(function(), map(), wapi_client_lib:context()) ->
    {ok, term()} | {error, term()}.
call_api(F, Params, Context) ->
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = F(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).
