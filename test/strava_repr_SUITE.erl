-module(strava_repr_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).

-export([test_to_activity_2/1,
         test_to_activity_3/1,
         test_to_activity_comment/1,
         test_to_activity_lap/1,
         test_to_athlete/1,
         test_to_club/1]).

all() ->
    [ test_to_activity_2,
      test_to_activity_3,
      test_to_activity_comment,
      test_to_activity_lap,
      test_to_athlete,
      test_to_club ].

-define(test_call(Name, FunName),
        {JSON, ExpectedTerm} = load_data(Config, Name),
        Term = strava_repr:FunName(JSON),
        ct:pal("~p", [ExpectedTerm]),
        ct:pal("~p", [Term]),
        ExpectedTerm = Term).

test_to_activity_2(Config) ->
    ?test_call(<<"activity-2">>, to_activity).

test_to_activity_3(Config) ->
    ?test_call(<<"activity-3">>, to_activity).

test_to_activity_comment(Config) ->
    ?test_call(<<"activity_comment">>, to_activity_comment).

test_to_activity_lap(Config) ->
    ?test_call(<<"activity_lap">>, to_activity_lap).

test_to_athlete(Config) ->
    ?test_call(<<"athlete">>, to_athlete).

test_to_club(Config) ->
    ?test_call(<<"club">>, to_club).

load_data(Config, Name) ->
    DataDir = ?config(data_dir, Config),
    JSONPath = filename:join(DataDir, <<Name/bytes, ".json">>),
    TermPath = filename:join(DataDir, <<Name/bytes, ".erl">>),
    ct:pal("Loading '~s' files:~n"
           "~s~n~s", [Name, JSONPath, TermPath]),
    {ok, JSON} = file:read_file(JSONPath),
    {ok, [Term]} = file:consult(TermPath),
    {jsx:decode(JSON, [return_maps]), Term}.
