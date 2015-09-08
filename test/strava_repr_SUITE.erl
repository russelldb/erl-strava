-module(strava_repr_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1]).

-export([test_to_athlete/1]).

all() ->
    [ test_to_athlete ].

init_per_suite(Config) ->
    PrivDir = ?config(data_dir, Config),
    What = [ athlete ],
    lists:foldl(
      fun(Name, Ans) ->
              NameBin = atom_to_binary(Name, latin1),
              JSONPath = filename:join(PrivDir, <<NameBin/bytes, ".json">>),
              TermPath = filename:join(PrivDir, <<NameBin/bytes, ".erl">>),
              ct:pal("Loading ~s", [JSONPath]),
              {ok, JSON} = file:read_file(JSONPath),
              ct:pal("Loading ~s", [TermPath]),
              {ok, [OutTerm]} = file:consult(TermPath),
              InTerm = jsx:decode(JSON, [return_maps]),
              [{Name, {InTerm, OutTerm}} | Ans]
      end, Config, What).

test_to_athlete(Config) ->
    {In, Out} = ?config(athlete, Config),
    Out = strava_repr:to_athlete(In).
