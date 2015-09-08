-module(strava_repr_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).

-export([test_to_athlete/1]).

all() ->
    [ test_to_athlete ].

test_to_athlete(Config) ->
    {JSON, Term} = load_data(Config, <<"athlete">>),
    Term = strava_repr:to_athlete(JSON).

load_data(Config, Name) ->
    DataDir = ?config(data_dir, Config),
    JSONPath = filename:join(DataDir, <<Name/bytes, ".json">>),
    TermPath = filename:join(DataDir, <<Name/bytes, ".erl">>),
    ct:pal("Loading '~s' files:~n"
           "~s~n~s", [Name, JSONPath, TermPath]),
    {ok, JSON} = file:read_file(JSONPath),
    {ok, [Term]} = file:consult(TermPath),
    {jsx:decode(JSON, [return_maps]), Term}.
