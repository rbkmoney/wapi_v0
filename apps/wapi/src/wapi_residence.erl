-module(wapi_residence).

-type response_data() :: wapi_handler:response_data().

-export([get/1]).

-type id() :: dmsl_domain_thrift:'Residence'().
-type residence() :: #{
    id := id(),
    name := binary(),
    flag => binary()
}.

-export_type([id/0]).
-export_type([residence/0]).

%%

-spec get(id()) -> response_data().
get(ID) ->
    get_residence(marshal(residence, ID)).

get_residence(ID = 'rus') ->
    {ok, #{
        <<"id">> => genlib_string:to_upper(genlib:to_binary(ID)),
        <<"name">> => <<"Российская федерация"/utf8>>,
        <<"flag">> => <<"🇷🇺"/utf8>>
    }};
get_residence(_) ->
    {error, notfound}.

%% Marshaling

marshal(residence, V) ->
    try
        erlang:binary_to_existing_atom(genlib_string:to_lower(V), latin1)
    catch
        error:badarg ->
            % TODO
            %  - Essentially this is incorrect, we should reply with 400 instead
            undefined
    end.
