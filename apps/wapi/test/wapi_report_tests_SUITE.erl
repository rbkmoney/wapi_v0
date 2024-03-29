-module(wapi_report_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").
-include_lib("fistful_reporter_proto/include/ff_reporter_reports_thrift.hrl").
-include_lib("file_storage_proto/include/fs_file_storage_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("jose/include/jose_jwk.hrl").
-include_lib("wapi_wallet_dummy_data.hrl").
-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("wapi_bouncer_data.hrl").

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
    create_report_ok_test/1,
    get_report_ok_test/1,
    get_reports_ok_test/1,
    reports_with_wrong_identity_ok_test/1,
    download_file_ok_test/1
]).

% common-api is used since it is the domain used in production RN
% TODO: change to wallet-api (or just omit since it is the default one) when new tokens will be a thing
-define(DOMAIN, <<"common-api">>).
-define(badresp(Code), {error, {invalid_response_code, Code}}).
-define(emptyresp(Code), {error, {Code, #{}}}).

-type test_case_name() :: atom().
-type config() :: [{atom(), any()}].
-type group_name() :: atom().

-behaviour(supervisor).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() -> [{group, test_case_name()}].
all() ->
    [
        {group, base}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {base, [], [
            create_report_ok_test,
            get_report_ok_test,
            get_reports_ok_test,
            reports_with_wrong_identity_ok_test,
            download_file_ok_test
        ]}
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

-spec init_per_group(group_name(), config()) -> config().
init_per_group(Group, Config) when Group =:= base ->
    ok = wapi_context:save(
        wapi_context:create(#{
            woody_context => woody_context:new(<<"init_per_group/", (atom_to_binary(Group, utf8))/binary>>)
        })
    ),
    Party = genlib:bsuuid(),
    {ok, Token} = wapi_ct_helper:issue_token(Party, [{[party], write}], unlimited, ?DOMAIN),
    Config1 = [{party, Party} | Config],
    [{context, wapi_ct_helper:get_context(Token)} | Config1];
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    C1 = wapi_ct_helper:makeup_cfg([wapi_ct_helper:test_case_name(Name), wapi_ct_helper:woody_ctx()], C),
    ok = wapi_context:save(C1),
    [{test_sup, wapi_ct_helper:start_mocked_service_sup(?MODULE)} | C1].

-spec end_per_testcase(test_case_name(), config()) -> config().
end_per_testcase(_Name, C) ->
    ok = wapi_context:cleanup(),
    _ = wapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests
-spec create_report_ok_test(config()) -> _.
create_report_ok_test(C) ->
    PartyID = ?config(party, C),
    _ = wapi_ct_helper_bouncer:mock_assert_identity_op_ctx(<<"CreateReport">>, ?STRING, PartyID, C),
    _ = wapi_ct_helper:mock_services(
        [
            {fistful_report, fun
                ('GenerateReport', _) -> {ok, ?REPORT_ID};
                ('GetReport', _) -> {ok, ?REPORT}
            end},
            {fistful_identity, fun('Get', _) -> {ok, ?IDENTITY(PartyID)} end}
        ],
        C
    ),
    {ok, _} = call_api(
        fun swag_client_wallet_reports_api:create_report/3,
        #{
            binding => #{
                <<"identityID">> => ?STRING
            },
            body => #{
                <<"reportType">> => <<"withdrawalRegistry">>,
                <<"fromTime">> => ?TIMESTAMP,
                <<"toTime">> => ?TIMESTAMP
            }
        },
        wapi_ct_helper:cfg(context, C)
    ).

-spec get_report_ok_test(config()) -> _.
get_report_ok_test(C) ->
    PartyID = ?config(party, C),
    _ = wapi_ct_helper_bouncer:mock_assert_generic_op_ctx(
        [
            {report, genlib:to_binary(?INTEGER), #{identity => ?STRING, files => [?STRING, ?STRING, ?STRING]}},
            {identity, ?STRING, PartyID}
        ],
        ?CTX_WAPI(#bctx_v1_WalletAPIOperation{
            id = <<"GetReport">>,
            identity = ?STRING,
            report = genlib:to_binary(?INTEGER)
        }),
        C
    ),
    _ = wapi_ct_helper:mock_services(
        [
            {fistful_report, fun('GetReport', _) -> {ok, ?REPORT} end},
            {fistful_identity, fun('Get', _) -> {ok, ?IDENTITY(PartyID)} end}
        ],
        C
    ),
    {ok, _} = call_api(
        fun swag_client_wallet_reports_api:get_report/3,
        #{
            binding => #{
                <<"identityID">> => ?STRING,
                <<"reportID">> => ?INTEGER
            }
        },
        wapi_ct_helper:cfg(context, C)
    ).

-spec get_reports_ok_test(config()) -> _.
get_reports_ok_test(C) ->
    PartyID = ?config(party, C),
    _ = wapi_ct_helper_bouncer:mock_assert_identity_op_ctx(<<"GetReports">>, ?STRING, PartyID, C),
    _ = wapi_ct_helper:mock_services(
        [
            {fistful_report, fun('GetReports', _) ->
                {ok, [
                    ?REPORT_EXT(pending, []),
                    ?REPORT_EXT(created, undefined),
                    ?REPORT_WITH_STATUS(canceled)
                ]}
            end},
            {fistful_identity, fun('Get', _) -> {ok, ?IDENTITY(PartyID)} end}
        ],
        C
    ),
    {ok, _} = call_api(
        fun swag_client_wallet_reports_api:get_reports/3,
        #{
            binding => #{
                <<"identityID">> => ?STRING
            },
            qs_val => #{
                <<"fromTime">> => ?TIMESTAMP,
                <<"toTime">> => ?TIMESTAMP,
                <<"type">> => <<"withdrawalRegistry">>
            }
        },
        wapi_ct_helper:cfg(context, C)
    ).

-spec reports_with_wrong_identity_ok_test(config()) -> _.
reports_with_wrong_identity_ok_test(C) ->
    IdentityID = <<"WrongIdentity">>,
    _ = wapi_ct_helper_bouncer:mock_arbiter(_ = wapi_ct_helper_bouncer:judge_always_forbidden(), C),
    _ = wapi_ct_helper:mock_services(
        [
            {fistful_report, fun
                ('GenerateReport', _) -> {ok, ?REPORT_ID};
                ('GetReport', _) -> {ok, ?REPORT};
                ('GetReports', _) -> {ok, [?REPORT, ?REPORT, ?REPORT]}
            end},
            {fistful_identity, fun('Get', _) -> {throwing, #fistful_IdentityNotFound{}} end}
        ],
        C
    ),
    ?emptyresp(401) = call_api(
        fun swag_client_wallet_reports_api:create_report/3,
        #{
            binding => #{
                <<"identityID">> => IdentityID
            },
            body => #{
                <<"reportType">> => <<"withdrawalRegistry">>,
                <<"fromTime">> => ?TIMESTAMP,
                <<"toTime">> => ?TIMESTAMP
            }
        },
        wapi_ct_helper:cfg(context, C)
    ),
    ?emptyresp(401) = call_api(
        fun swag_client_wallet_reports_api:get_report/3,
        #{
            binding => #{
                <<"identityID">> => IdentityID,
                <<"reportID">> => ?INTEGER
            }
        },
        wapi_ct_helper:cfg(context, C)
    ),
    ?emptyresp(401) = call_api(
        fun swag_client_wallet_reports_api:get_reports/3,
        #{
            binding => #{
                <<"identityID">> => IdentityID
            },
            qs_val => #{
                <<"fromTime">> => ?TIMESTAMP,
                <<"toTime">> => ?TIMESTAMP,
                <<"type">> => <<"withdrawalRegistry">>
            }
        },
        wapi_ct_helper:cfg(context, C)
    ).

-spec download_file_ok_test(config()) -> _.
download_file_ok_test(C) ->
    _ = wapi_ct_helper_bouncer:mock_assert_op_ctx(<<"DownloadFile">>, C),
    _ = wapi_ct_helper:mock_services([{file_storage, fun('GenerateDownloadUrl', _) -> {ok, ?STRING} end}], C),
    {ok, _} = call_api(
        fun swag_client_wallet_downloads_api:download_file/3,
        #{
            binding => #{
                <<"fileID">> => ?STRING
            },
            qs_val => #{
                <<"expiresAt">> => ?TIMESTAMP
            }
        },
        wapi_ct_helper:cfg(context, C)
    ).

%%

-spec call_api(function(), map(), wapi_client_lib:context()) -> {ok, term()} | {error, term()}.
call_api(F, Params, Context) ->
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = F(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).
