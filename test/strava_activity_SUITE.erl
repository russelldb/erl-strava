-module(strava_activity_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [ read_athletes ].

init_per_suite(Config) ->
    httpc_mock:init_per_suite(Config).

end_per_suite(Config) ->
    httpc_mock:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    httpc_mock:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    httpc_mock:end_per_testcase(TestCase, Config).

%%--------------------------------------------------------------------

read_athletes(_Config) ->
    {ok, [ #{id := 8529483,
             resource_state := summary,
             external_id := <<"2013-08-23-17-04-12.fit">>,
             upload_id := 84130503,
             athlete := #{id := 227615,
                          resource_state := meta},
             name := <<"08/23/2013 Oakland, CA">>,
             distance := 32486.1,
             moving_time := 5241,
             elapsed_time := 5427,
             total_elevation_gain := 566.0,
             type := ride,
             start_date := {{2013,08,24}, {00,04,12}},
             start_date_local := {{2013,08,23}, {17,04,12}},
             timezone := <<"(GMT-08:00) America/Los_Angeles">>,
             start_latlng := {37.793551, -122.2686},
             end_latlng := {37.792836, -122.268287},
             location_city := <<"Oakland">>,
             location_state := <<"CA">>,
             location_country := <<"United States">>,
             achievement_count := 8,
             kudos_count := 0,
             comment_count := 0,
             athlete_count := 1,
             photo_count := 0,
             total_photo_count := 0,
             map := #{id := <<"a77175935">>,
                      summary_polyline := <<"cetewLja@zYcG">>,
                      resource_state := summary},
             trainer := false,
             commute := false,
             manual := false,
             private := false,
             flagged := false,
             average_speed := 3.4,
             max_speed := 4.514,
             average_watts := 163.6,
             weighted_average_watts := 200,
             kilojoules := 857.6,
             device_watts := true,
             has_heartrate := true,
             average_heartrate := 138.8,
             max_heartrate := 179.0} ]} =
        strava_activity:athletes(<<"xyzzy">>, 1, 1).
