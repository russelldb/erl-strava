-module(strava_gear_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [ read_detailed ].

init_per_suite(Config) ->
    httpc_mock:init_per_suite(Config).

end_per_suite(Config) ->
    httpc_mock:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    httpc_mock:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    httpc_mock:end_per_testcase(TestCase, Config).

%%--------------------------------------------------------------------

read_detailed(_Config) ->
    {ok, #{id := <<"b105763">>,
           primary := false,
           name := <<"Cannondale TT">>,
           distance := 476612.8,
           resource_state := detailed,
           brand_name := <<"Cannondale">>,
           model_name := <<"Slice">>,
           frame_type := time_trial,
           description := <<"Best bike EVER!!">>}} =
        strava_gear:gear(<<"xyzzy">>, <<"b105763">>).
