-module(strava_club_SUITE).

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
    {ok, #{id := 1,
           resource_state := detailed,
           name := <<"Team Strava Cycling">>,
           profile_medium := <<"http://pics.com/clubs/1/medium.jpg">>,
           profile := <<"http://pics.com/clubs/1/large.jpg">>,
           description := <<"From the people who brought you strava.com">>,
           club_type := company,
           sport_type := cycling,
           city := <<"San Francisco">>,
           state := <<"California">>,
           country := <<"United States">>,
           private := false,
           member_count := 71,
           membership := member,
           admin := true,
           owner := false,
           following_count := 1}} =
        strava_club:club(<<"xyzzy">>, 1).
