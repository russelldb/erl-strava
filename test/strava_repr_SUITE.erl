-module(strava_repr_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).

-export([test_to_activity_2/1,
         test_to_activity_3/1,
         test_to_activity_comment/1,
         test_to_activity_lap/1,
         test_to_activity_photo/1,
         test_to_athlete/1,
         test_to_athlete_2/1,
         test_to_athlete_stats/1,
         test_to_club/1,
         test_to_club_announcement/1,
         test_to_club_group_event/1,
         test_to_gear_bike/1,
         test_to_stream/1]).

all() ->
    [ test_to_activity_2,
      test_to_activity_3,
      test_to_activity_comment,
      test_to_activity_lap,
      test_to_activity_photo,
      test_to_athlete,
      test_to_athlete_2,
      test_to_athlete_stats,
      test_to_club,
      test_to_gear_bike ].

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

test_to_activity_photo(Config) ->
    ?test_call(<<"activity_photo">>, to_activity_photo).

test_to_athlete(Config) ->
    ?test_call(<<"athlete">>, to_athlete).

test_to_athlete_2(Config) ->
    ?test_call(<<"athlete-2">>, to_athlete).

test_to_athlete_stats(Config) ->
    ?test_call(<<"athlete_stats">>, to_athlete_stats).

test_to_club(Config) ->
    ?test_call(<<"club">>, to_club).

test_to_club_announcement(Config) ->
    ?test_call(<<"club_announcement">>, to_club_announcement).

test_to_club_group_event(Config) ->
    ?test_call(<<"club_group_event">>, to_club_group_event).

test_to_gear_bike(Config) ->
    ?test_call(<<"gear-bike">>, to_gear).

test_to_stream(Config) ->
    ?test_call(<<"stream">>, to_stream).

load_data(Config, Name) ->
    DataDir = ?config(data_dir, Config),
    JSONPath = filename:join(DataDir, <<Name/bytes, ".json">>),
    TermPath = filename:join(DataDir, <<Name/bytes, ".erl">>),
    ct:pal("Loading '~s' files:~n"
           "~s~n~s", [Name, JSONPath, TermPath]),
    {ok, JSON} = file:read_file(JSONPath),
    {ok, [Term]} = file:consult(TermPath),
    {jsx:decode(JSON, [return_maps]), Term}.
