-module(httpc_mock).

-compile(export_all).

init_per_suite(Config) ->
    meck:new(httpc),
    Config.

end_per_suite(_Config) ->
    meck:unload(httpc).

init_per_testcase(_TestCase, Config) ->
    meck:expect(httpc, request,
                fun(Method,
                    {URL, [{"Authorization", "Bearer xyzzy"}]},
                    _HTTPOpts, _Opts, strava) ->
                        Body = httpc_request(Method, URL),
                        ct:pal("~p ~p", [Method, URL]),
                        {ok, {200, Body}}
                end),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:delete(httpc, request, 5).

%%--------------------------------------------------------------------

httpc_request(get, "https://www.strava.com/api/v3/athlete") ->
    <<"
{
  \"id\": 227615,
  \"resource_state\": 3,
  \"firstname\": \"John\",
  \"lastname\": \"Applestrava\",
  \"profile_medium\": \"http://pics.com/227615/medium.jpg\",
  \"profile\": \"http://pics.com/227615/large.jpg\",
  \"city\": \"San Francisco\",
  \"state\": \"California\",
  \"country\": \"United States\",
  \"sex\": \"M\",
  \"friend\": null,
  \"follower\": null,
  \"premium\": true,
  \"created_at\": \"2008-01-01T17:44:00Z\",
  \"updated_at\": \"2013-09-04T20:00:50Z\",
  \"follower_count\": 273,
  \"friend_count\": 19,
  \"mutual_friend_count\": 0,
  \"athlete_type\": 0,
  \"date_preference\": \"%m/%d/%Y\",
  \"measurement_preference\": \"feet\",
  \"email\": \"john@applestrava.com\",
  \"ftp\": 280,
  \"weight\": 68.7,
  \"clubs\": [
    {
      \"id\": 1,
      \"resource_state\": 2,
      \"name\": \"Team Strava Cycling\",
      \"profile_medium\": \"http://pics.com/clubs/1/medium.jpg\",
      \"profile\": \"http://pics.com/clubs/1/large.jpg\"
    }
  ],
  \"bikes\": [
    {
      \"id\": \"b105763\",
      \"primary\": false,
      \"name\": \"Cannondale TT\",
      \"distance\": 476612.9,
      \"resource_state\": 2
    },
    {
      \"id\": \"b105762\",
      \"primary\": true,
      \"name\": \"Masi\",
      \"distance\": 9000578.2,
      \"resource_state\": 2
    }
  ],
  \"shoes\": [
    {
      \"id\": \"g1\",
      \"primary\": true,
      \"name\": \"Running Shoes\",
      \"distance\": 67492.9,
      \"resource_state\": 2
    }
  ]
}
">>;

httpc_request(get, "https://www.strava.com/api/v3/athletes/227615") ->
    <<"
{
  \"id\": 227615,
  \"resource_state\": 2,
  \"firstname\": \"John\",
  \"lastname\": \"Applestrava\",
  \"profile_medium\": \"http://pics.com/227615/medium.jpg\",
  \"profile\": \"http://pics.com/227615/large.jpg\",
  \"city\": \"San Francisco\",
  \"state\": \"CA\",
  \"country\": \"United States\",
  \"sex\": \"M\",
  \"friend\": null,
  \"follower\": \"accepted\",
  \"premium\": true,
  \"created_at\": \"2011-03-19T21:59:57Z\",
  \"updated_at\": \"2013-09-05T16:46:54Z\"
}
">>;

httpc_request(get, "https://www.strava.com/api/v3/athletes/227615/stats") ->
    <<"
{
  \"biggest_ride_distance\": 205481.0,
  \"biggest_climb_elevation_gain\": 1224.0,
  \"recent_ride_totals\": {
    \"count\": 30,
    \"distance\": 1037410.8046875,
    \"moving_time\": 165675,
    \"elapsed_time\": 181337,
    \"elevation_gain\": 13624.902572631836,
    \"achievement_count\": 346
  },
  \"recent_run_totals\": {
    \"count\": 6,
    \"distance\": 83126.9013671875,
    \"moving_time\": 24731,
    \"elapsed_time\": 25764,
    \"elevation_gain\": 1564.9624166488647,
    \"achievement_count\": 41
  },
  \"ytd_ride_totals\": {
    \"count\": 45,
    \"distance\": 1586190,
    \"moving_time\": 257281,
    \"elapsed_time\": 285315,
    \"elevation_gain\": 22430
  },
  \"ytd_run_totals\": {
    \"count\": 6,
    \"distance\": 83127,
    \"moving_time\": 24731,
    \"elapsed_time\": 25764,
    \"elevation_gain\": 1565
  },
  \"all_ride_totals\": {
    \"count\": 765,
    \"distance\": 42918079,
    \"moving_time\": 6386345,
    \"elapsed_time\": 7228437,
    \"elevation_gain\": 550886
  },
  \"all_run_totals\": {
    \"count\": 75,
    \"distance\": 916433,
    \"moving_time\": 249542,
    \"elapsed_time\": 261434,
    \"elevation_gain\": 7520
  }
}
">>;

httpc_request(get, "https://www.strava.com/api/v3/gear/b105763") ->
    <<"
{
  \"id\": \"b105763\",
  \"primary\": false,
  \"name\": \"Cannondale TT\",
  \"distance\": 476612.8,
  \"resource_state\": 3,
  \"brand_name\": \"Cannondale\",
  \"model_name\": \"Slice\",
  \"frame_type\": 4,
  \"description\": \"Best bike EVER!!\"
}
">>.
