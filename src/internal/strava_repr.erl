-module(strava_repr).

%% From representation functions
-export([to_activity/1, to_activity_comment/1, to_activity_lap/1,
         to_activity_photo/1, to_activity_zones/1, to_athlete/1,
         to_athlete_stats/1, to_club/1, to_club_announcement/1,
         to_club_group_event/1, to_error/1, to_gear/1, to_route/1,
         to_segment/1, to_segment_climb_category/1,
         to_segment_effort/1, to_segment_leaderboard/1, to_stream/1,
         to_upload_status/1]).

%% To representation functions
-export([from_activity_type/1, from_athlete/1, from_athlete_sex/1,
         from_datetime/1, from_segment_climb_category/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type field() :: atom()
               | {atom(), binary()}
               | {atom(), fun()}
               | {atom(), {list, fun()}}
               | {atom(), binary(), {list, fun()}}
               | {atom(), binary(), fun()}.
-type fields() :: [field()].

%%%===================================================================
%%% From representation functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_activity(map()) -> strava_activity:t().

to_activity(Map) ->
    transform(
      Map,
      [ achievement_count,
        athlete_count,
        average_cadence,
        average_heartrate,
        average_speed,
        average_temp,
        average_watts,
        calories,
        comment_count,
        commute,
        description,
        device_name,
        device_watts,
        distance,
        elapsed_time,
        elev_high,
        elev_low,
        embed_token,
        external_id,
        flagged,
        gear_id,
        has_heartrate,
        has_kudoed,
        id,
        kilojoules,
        kudos_count,
        location_city,
        location_country,
        location_state,
        manual,
        max_heartrate,
        max_speed,
        max_watts,
        moving_time,
        name,
        photo_count,
        private,
        suffer_score,
        timezone,
        total_elevation_gain,
        total_photo_count,
        trainer,
        upload_id,
        weighted_average_watts,
        best_efforts,                           % TODO
        splits_metric,                          % TODO
        splits_standard,                        % TODO
        {athlete, fun to_athlete/1},
        {end_latlng, fun to_latlng/1},
        {gear, fun to_gear/1},
        {map, fun to_map/1},
        {photos, fun to_activity_photos_summary/1},
        {resource_state, fun to_resource_state/1},
        {segment_efforts, {list, fun to_segment_effort/1}},
        {start_date, fun to_datetime/1},
        {start_date_local, fun to_datetime/1},
        {start_latlng, fun to_latlng/1},
        {type, fun to_activity_type/1},
        {workout_type, fun(0) -> default;
                          (1) -> race;
                          (2) -> long_run;
                          (3) -> intervals end} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_activity_comment(map()) -> strava_activity:comment().

to_activity_comment(Map) ->
    transform(
      Map,
      [ activity_id,
        id,
        text,
        {athlete, fun to_athlete/1},
        {created_at, fun to_datetime/1},
        {resource_state, fun to_resource_state/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_activity_lap(map()) -> strava_activity:lap().

to_activity_lap(Map) ->
    transform(
      Map,
      [ average_cadence,
        average_heartrate,
        average_speed,
        average_watts,
        device_watts,
        distance,
        elapsed_time,
        end_index,
        id,
        lap_index,
        max_heartrate,
        max_speed,
        moving_time,
        name,
        start_index,
        total_elevation_gain,
        {activity, fun to_activity/1},
        {athlete, fun to_athlete/1},
        {resource_state, fun to_resource_state/1},
        {start_date, fun to_datetime/1},
        {start_date_local, fun to_datetime/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_activity_photo(map()) -> strava_activity:photo().

to_activity_photo(Map) ->
    transform(
      Map,
      [ activity_id,
        caption,
        id,
        ref,
        type,
        uid,
        unique_id,
        {created_at, fun to_datetime/1},
        {location, fun to_latlng/1},
        {resource_state, fun to_resource_state/1},
        {source, fun(1) -> strava;
                    (2) -> instagram end},
        {uploaded_at, fun to_datetime/1},
        {urls, fun(Urls) ->
                       maps:fold(
                         fun(K, V, Ans) ->
                                 Ans#{binary_to_integer(K) => V}
                         end, _Ans = #{}, Urls)
               end} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_activity_photos_summary(map()) -> map().

to_activity_photos_summary(Map) ->
    transform(
      Map,
      [ count,
        {primary, fun to_activity_photo/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_activity_type(binary()) -> strava_activity:type().

to_activity_type(<<"AlpineSki">>) -> alpine_ski;
to_activity_type(<<"BackcountrySki">>) -> backcountry_ski;
to_activity_type(<<"Canoeing">>) -> canoeing;
to_activity_type(<<"Crossfit">>) -> crossfit;
to_activity_type(<<"EBikeRide">>) -> ebike_ride;
to_activity_type(<<"Elliptical">>) -> elliptical;
to_activity_type(<<"Hike">>) -> hike;
to_activity_type(<<"IceSkate">>) -> ice_skate;
to_activity_type(<<"InlineSkate">>) -> inline_skate;
to_activity_type(<<"Kayaking">>) -> kayaking;
to_activity_type(<<"Kitesurf">>) -> kitesurf;
to_activity_type(<<"NordicSki">>) -> nordic_ski;
to_activity_type(<<"Ride">>) -> ride;
to_activity_type(<<"RockClimbing">>) -> rock_climbing;
to_activity_type(<<"RollerSki">>) -> roller_ski;
to_activity_type(<<"Rowing">>) -> rowing;
to_activity_type(<<"Run">>) -> run;
to_activity_type(<<"Snowboard">>) -> snowboard;
to_activity_type(<<"Snowshoe">>) -> snowshoe;
to_activity_type(<<"StairStepper">>) -> stair_stepper;
to_activity_type(<<"StandUpPaddling">>) -> stand_up_paddling;
to_activity_type(<<"Surfing">>) -> surfing;
to_activity_type(<<"Swim">>) -> swim;
to_activity_type(<<"VirtualRide">>) -> virtual_ride;
to_activity_type(<<"Walk">>) -> walk;
to_activity_type(<<"WeightTraining">>) -> weight_training;
to_activity_type(<<"Windsurf">>) -> windsurf;
to_activity_type(<<"Workout">>) -> workout;
to_activity_type(<<"Yoga">>) -> yoga.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_activity_zones(map()) -> strava_activity:zones().

to_activity_zones(Map) ->
    transform(
      Map,
      [ custom_zones,
        max,
        points,
        score,
        sensor_based,
        {distribution_buckets, {list, fun to_activity_zones_bucket/1}},
        {resource_state, fun to_resource_state/1},
        {type, fun(<<"heartrate">>) -> heartrate;
                  (<<"power">>) -> power end} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_activity_zones_bucket(map()) -> strava_activity:zones_bucket().

to_activity_zones_bucket(Map) ->
    transform(Map, [max, min, time]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_athlete(map()) -> strava_athlete:t().

to_athlete(Map) ->
    transform(
      Map,
      [ city,
        country,
        date_preference,
        email,
        firstname,
        follower_count,
        friend_count,
        ftp,
        id,
        lastname,
        mutual_friend_count,
        premium,
        profile,
        profile_medium,
        state,
        weight,
        {athlete_type, fun to_athlete_type/1},
        {bikes, {list, fun to_gear/1}},
        {clubs, {list, fun to_club/1}},
        {created_at, fun to_datetime/1},
        {follower, fun to_athlete_friend_status/1},
        {friend, fun to_athlete_friend_status/1},
        {measurement_preference, fun(<<"feet">>) -> feet;
                                    (<<"meters">>) -> meters end},
        {resource_state, fun to_resource_state/1},
        {sex, fun to_athlete_sex/1},
        {shoes, {list, fun to_gear/1}},
        {updated_at, fun to_datetime/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_athlete_friend_status(binary()) -> strava_athlete:friend_status().

to_athlete_friend_status(Text)
  when Text =:= <<"pending">>;
       Text =:= <<"accepted">>;
       Text =:= <<"blocked">> ->
    binary_to_atom(Text, latin1).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_athlete_sex(binary()) -> strava_athlete:sex().

to_athlete_sex(<<"F">>) -> female;
to_athlete_sex(<<"M">>) -> male.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_athlete_stats(map()) -> strava_athlete:stats().

to_athlete_stats(Map) ->
    transform(
      Map,
      [ biggest_climb_elevation_gain,
        biggest_ride_distance,
        {all_ride_totals, fun to_athlete_totals/1},
        {all_run_totals, fun to_athlete_totals/1},
        {all_swim_totals, fun to_athlete_totals/1},
        {recent_ride_totals, fun to_athlete_totals/1},
        {recent_run_totals, fun to_athlete_totals/1},
        {recent_swim_totals, fun to_athlete_totals/1},
        {ytd_ride_totals, fun to_athlete_totals/1},
        {ytd_run_totals, fun to_athlete_totals/1},
        {ytd_swim_totals, fun to_athlete_totals/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_athlete_totals(map()) -> strava_athlete:totals().

to_athlete_totals(Map) ->
    transform(
      Map,
      [ achievement_count,
        count,
        distance,
        elapsed_time,
        elevation_gain,
        moving_time ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_athlete_type(integer()) -> strava_athlete:type().

to_athlete_type(0) -> cyclist;
to_athlete_type(1) -> runner.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_club(map()) -> strava_club:t().

to_club(Map) ->
    transform(
      Map,
      [ admin,
        city,
        country,
        description,
        following_count,
        id,
        member_count,
        name,
        owner,
        private,
        profile,
        profile_medium,
        state,
        {club_type, fun to_club_type/1},
        %% TODO: handle unknown membership
        {membership, fun(<<"member">>) -> member;
                        (<<"pending">>) -> pending end},
        {resource_state, fun to_resource_state/1},
        {sport_type, fun to_club_sport_type/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_club_announcement(map()) -> strava_club:announcement().

to_club_announcement(Map) ->
    transform(
      Map,
      [ club_id,
        id,
        message,
        {athlete, fun to_athlete/1},
        {created_at, fun to_datetime/1},
        {resource_state, fun to_resource_state/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_club_group_event(map()) -> strava_club:group_event().

to_club_group_event(Map) ->
    transform(
      Map,
      [ address,
        club_id,
        description,
        id,
        private,
        route_id,
        title,
        woman_only,
        skill_levels,                                       % TODO?
        terrain,                                            % TODO?
        {activity_type, fun to_activity_type/1},
        {created_at, fun to_datetime/1},
        {organizing_athlete, fun to_athlete/1},
        {resource_state, fun to_resource_state/1},
        {upcoming_occurrences, {list, fun to_datetime/1}} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_club_sport_type(binary()) -> strava_club:sport_type().

to_club_sport_type(Text)
  when Text =:= <<"cycling">>;
       Text =:= <<"running">>;
       Text =:= <<"triathlon">>;
       Text =:= <<"other">> ->
    binary_to_atom(Text, latin1).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_club_type(binary()) -> strava_club:type().

to_club_type(Text)
  when Text =:= <<"casual_club">>;
       Text =:= <<"racing_team">>;
       Text =:= <<"shop">>;
       Text =:= <<"company">>;
       Text =:= <<"other">> ->
    binary_to_atom(Text, latin1).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_datetime(binary()) -> calendar:datetime().

to_datetime(Text) ->
    {ok, Ans} = iso8601:parse_datetime(Text), Ans.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_error(map()) -> strava:error().

to_error(Map) ->
    transform(
      Map,
      [ message,
        {errors, {list, fun(Map1) ->
                                transform(Map1,
                                          [ resource, field, code ])
                        end}} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_gear(map()) -> strava_gear:t().

to_gear(Map) ->
    transform(
      Map,
      [ brand_name,
        description,
        distance,
        id,
        model_name,
        name,
        primary,
        {frame_type, fun to_gear_frame_type/1},
        {resource_state, fun to_resource_state/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_gear_frame_type(integer()) -> strava_gear:frame_type().

to_gear_frame_type(1) -> mtb;
to_gear_frame_type(2) -> cross;
to_gear_frame_type(3) -> road;
to_gear_frame_type(4) -> time_trial.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_latlng([number()]) -> strava:latlng().

to_latlng([Lat, Lon]) -> {Lat, Lon}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_map(map()) -> map().

to_map(Map) ->
    transform(
      Map,
      [ id,
        polyline,
        summary_polyline,
        {resource_state, fun to_resource_state/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_resource_state(integer()) -> strava:resource_state().

to_resource_state(1) -> meta;
to_resource_state(2) -> summary;
to_resource_state(3) -> detailed.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_route(map()) -> strava_route:t().

to_route(Map) ->
    transform(
      Map,
      [ description,
        distance,
        elevation_gain,
        id,
        name,
        private,
        starred,
        timestamp,
        {athlete, fun to_athlete/1},
        {map, fun to_map/1},
        {resource_state, fun to_resource_state/1},
        {segments, {list, fun to_segment/1}},
        {sub_type, fun(1) -> road;
                      (2) -> mtb;
                      (3) -> cx;
                      (4) -> trail;
                      (5) -> mixed end},
        {type, fun(1) -> ride;
                  (2) -> run end} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_segment_effort(map()) -> strava_segment_effort:t().

to_segment_effort(Map) ->
    transform(
      Map,
      [ average_cadence,
        average_heartrate,
        average_watts,
        device_watts,
        distance,
        elapsed_time,
        end_index,
        hidden,
        id,
        kom_rank,
        max_heartrate,
        moving_time,
        name,
        pr_rank,
        start_index,
        {activity, fun to_activity/1},
        {athlete, fun to_athlete/1},
        {resource_state, fun to_resource_state/1},
        {segment, fun to_segment/1},
        {start_date, fun to_datetime/1},
        {start_date_local, fun to_datetime/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_segment(map()) -> strava_segment:t().

to_segment(Map) ->
    transform(
      Map,
      [ athlete_count,
        average_grade,
        city,
        country,
        distance,
        effort_count,
        elevation_high,
        elevation_low,
        hazardous,
        id,
        maximum_grade,
        name,
        private,
        star_count,
        starred,
        state,
        total_elevation_gain,
        {activity_type, fun to_segment_activity_type/1},
        {climb_category, fun to_segment_climb_category/1},
        {created_at, fun to_datetime/1},
        {end_latlng, fun to_latlng/1},
        {map, fun to_map/1},
        {resource_state, fun to_resource_state/1},
        {start_latlng, fun to_latlng/1},
        {updated_at, fun to_datetime/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_segment_activity_type(binary()) -> strava_segment:activity_type().

to_segment_activity_type(<<"Ride">>) -> ride;
to_segment_activity_type(<<"Run">>) -> run.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_segment_climb_category(integer()) -> strava_segment:climb_category().

to_segment_climb_category(0) -> undefined;
to_segment_climb_category(1) -> 4;
to_segment_climb_category(2) -> 3;
to_segment_climb_category(3) -> 2;
to_segment_climb_category(4) -> 1;
to_segment_climb_category(5) -> hc.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_segment_leaderboard(map()) -> strava_segment:leaderboard().

to_segment_leaderboard(Map) ->
    transform(
      Map,
      [ entry_count,
        {entries, {list, fun to_segment_leaderboard_entry/1}} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_segment_leaderboard_entry(map()) -> map().

to_segment_leaderboard_entry(Map) ->
    transform(
      Map,
      [ activity_id,
        athlete_id,
        athlete_name,
        athlete_profile,
        average_hr,
        average_watts,
        distance,
        effort_id,
        elapsed_time,
        moving_time,
        rank,
        {athlete_gender, fun to_athlete_sex/1},
        {start_date, fun to_datetime/1},
        {start_date_local, fun to_datetime/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_stream(map()) -> strava_stream:t().

to_stream(Map) ->
    transform(
      Map,
      [ original_size,
        {data, {list, fun to_stream_data/1}},
        {resolution, fun(<<"low">>) -> low;
                        (<<"medium">>) -> medium;
                        (<<"high">>) -> high end},
        {series_type, fun(<<"time">>) -> time;
                         (<<"distance">>) -> distance end},
        {type, fun to_stream_type/1} ]
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_stream_data(boolean() | number() | [number()]) ->
                            boolean() | number() | strava:latlng().

to_stream_data([Lat, Lon])
  when is_number(Lat), is_number(Lon) ->
    {Lat, Lon};

to_stream_data(Term)
  when is_boolean(Term); is_number(Term) ->
    Term.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_stream_type(binary()) -> strava_stream:type().

to_stream_type(Term)
  when Term =:= <<"time">>;
       Term =:= <<"latlng">>;
       Term =:= <<"distance">>;
       Term =:= <<"altitude">>;
       Term =:= <<"velocity_smooth">>;
       Term =:= <<"heartrate">>;
       Term =:= <<"cadence">>;
       Term =:= <<"watts">>;
       Term =:= <<"temp">>;
       Term =:= <<"moving">>;
       Term =:= <<"grade_smooth">> ->
    binary_to_atom(Term, latin1).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_upload_status(map()) -> strava_upload:status().

to_upload_status(Map) ->
    transform(
      Map,
      [ activity_id,
        error,
        external_id,
        id,
        status ]
     ).

%%%===================================================================
%%% To representation functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_activity_type(strava_activity:type()) -> binary().

from_activity_type(alpine_ski) -> <<"AlpineSki">>;
from_activity_type(backcountry_ski) -> <<"BackcountrySki">>;
from_activity_type(canoeing) -> <<"Canoeing">>;
from_activity_type(crossfit) -> <<"Crossfit">>;
from_activity_type(ebike_ride) -> <<"EBikeRide">>;
from_activity_type(elliptical) -> <<"Elliptical">>;
from_activity_type(hike) -> <<"Hike">>;
from_activity_type(ice_skate) -> <<"IceSkate">>;
from_activity_type(inline_skate) -> <<"InlineSkate">>;
from_activity_type(kayaking) -> <<"Kayaking">>;
from_activity_type(kitesurf) -> <<"Kitesurf">>;
from_activity_type(nordic_ski) -> <<"NordicSki">>;
from_activity_type(ride) -> <<"Ride">>;
from_activity_type(rock_climbing) -> <<"RockClimbing">>;
from_activity_type(roller_ski) -> <<"RollerSki">>;
from_activity_type(rowing) -> <<"Rowing">>;
from_activity_type(run) -> <<"Run">>;
from_activity_type(snowboard) -> <<"Snowboard">>;
from_activity_type(snowshoe) -> <<"Snowshoe">>;
from_activity_type(stair_stepper) -> <<"StairStepper">>;
from_activity_type(stand_up_paddling) -> <<"StandUpPaddling">>;
from_activity_type(surfing) -> <<"Surfing">>;
from_activity_type(swim) -> <<"Swim">>;
from_activity_type(virtual_ride) -> <<"VirtualRide">>;
from_activity_type(walk) -> <<"Walk">>;
from_activity_type(weight_training) -> <<"WeightTraining">>;
from_activity_type(windsurf) -> <<"Windsurf">>;
from_activity_type(workout) -> <<"Workout">>;
from_activity_type(yoga) -> <<"Yoga">>.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_athlete(strava_athlete:t()) -> map().

from_athlete(Map) ->
    maps:fold(
      fun(K, V, Ans)
            when K =:= city;
                 K =:= state;
                 K =:= country;
                 K =:= weight ->
              Ans#{atom_to_binary(K, latin1) => V};
         (sex, Term, Ans) -> Ans#{sex => from_athlete_sex(Term)};
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_athlete_sex(strava_athlete:sex()) -> binary().

from_athlete_sex(male) -> <<"M">>;
from_athlete_sex(female) -> <<"F">>.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_datetime(calendar:datetime()) -> binary().

from_datetime(DateTime) ->
    iolist_to_binary(iso8601:format_datetime(DateTime)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_segment_climb_category(strava_segment:climb_category()) -> integer().

from_segment_climb_category(undefined) -> 0;
from_segment_climb_category(4) -> 1;
from_segment_climb_category(3) -> 2;
from_segment_climb_category(2) -> 3;
from_segment_climb_category(1) -> 4;
from_segment_climb_category(hc) -> 5.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec transform(map(), fields()) -> map().

transform(Map, Fields) ->
    IdFun =
        fun(Term) -> Term end,
    UniformFields =
        lists:map(
          fun(Field = {_Name, _ExternName, _Fun}) -> Field;
             ({Name, ExternName}) when is_binary(ExternName) ->
                  {Name, ExternName, IdFun};
             ({Name, Fun}) when is_function(Fun); is_tuple(Fun) ->
                  {Name, atom_to_binary(Name, latin1), Fun};
             (Name) -> {Name, atom_to_binary(Name, latin1), IdFun}
          end, Fields),
    lists:foldl(
      fun({Name, ExternName, Fun}, Ans) ->
              case maps:get(ExternName, Map, null) of
                  null -> Ans;
                  Value -> Ans#{Name =>
                                    case Fun of
                                        {list, Fun1} -> lists:map(Fun1, Value);
                                        Fun1 -> Fun1(Value)
                                    end}
              end
      end, _Ans = #{}, UniformFields).
