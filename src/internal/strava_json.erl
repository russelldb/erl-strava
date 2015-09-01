-module(strava_json).

%% Parse JSON
-export([decode/1]).

%% From JSON map functions
-export([to_activity/1, to_athlete/1, to_athlete_stats/1, to_club/1,
         to_gear/1, to_segment/1, to_segment_effort/1, to_stream/1]).

%% To JSON functions
-export([from_athlete/1]).

%%%===================================================================
%%% Parse JSON
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> term().

decode(Text) -> jsx:decode(Text, [return_maps]).

%%%===================================================================
%%% From JSON map functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_activity(map()) -> strava_activity:t().

to_activity(Map) ->
    maps:fold(
      fun(K, V, Ans)
            when K =:= <<"id">>;
                 K =:= <<"external_id">>;
                 K =:= <<"upload_id">>;
                 K =:= <<"name">>;
                 K =:= <<"description">>;
                 K =:= <<"distance">>;
                 K =:= <<"moving_time">>;
                 K =:= <<"elapsed_time">>;
                 K =:= <<"total_elevation_gain">>;
                 K =:= <<"timezone">>;
                 K =:= <<"location_city">>;
                 K =:= <<"location_state">>;
                 K =:= <<"location_country">>;
                 K =:= <<"achievement_count">>;
                 K =:= <<"kudos_count">>;
                 K =:= <<"comment_count">>;
                 K =:= <<"athlete_count">>;
                 K =:= <<"photo_count">>;
                 K =:= <<"total_photo_count">>;
                 K =:= <<"trainer">>;
                 K =:= <<"commute">>;
                 K =:= <<"manual">>;
                 K =:= <<"private">>;
                 K =:= <<"flagged">>;
                 K =:= <<"gear_id">>;
                 K =:= <<"average_speed">>;
                 K =:= <<"max_speed">>;
                 K =:= <<"average_cadence">>;
                 K =:= <<"average_temp">>;
                 K =:= <<"average_watts">>;
                 K =:= <<"weighted_average_watts">>;
                 K =:= <<"kilojoules">>;
                 K =:= <<"device_watts">>;
                 K =:= <<"average_heartrate">>;
                 K =:= <<"max_heartrate">>;
                 K =:= <<"calories">>;
                 K =:= <<"has_kudoed">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         (<<"resource_state">>, Int, Ans) -> Ans#{resource_state => to_resource_state(Int)};
         (<<"athlete">>, Term, Ans) -> Ans#{athlete => to_athlete(Term)};
         (<<"type">>, Str, Ans) -> Ans#{type => Str}; % TODO
         (<<"start_date">>, Str, Ans) -> Ans#{start_date => to_datetime(Str)};
         (<<"start_date_local">>, Str, Ans) -> Ans#{start_date_local => to_datetime(Str)};
         (<<"start_latlng">>, List, Ans) -> Ans#{start_latlng => List}; % TODO
         (<<"end_latlng">>, List, Ans) -> Ans#{end_latlng => List}; % TODO
         (<<"photos">>, List, Ans) -> Ans#{photos => List}; % TODO
         (<<"map">>, Term, Ans) -> Ans#{map => Term};         % TODO
         (<<"workout_type">>, Int, Ans) -> Ans#{workout_type => Int}; % TODO
         (<<"gear">>, Term, Ans) -> Ans#{gear => to_gear(Term)};
         (<<"segment_efforts">>, List, Ans) -> Ans#{segment_efforts => lists:map(fun to_segment_effort/1, List)};
         (<<"splits_metric">>, _Term, Ans) -> Ans;
         (<<"splits_standard">>, _Term, Ans) -> Ans;
         (<<"best_efforts">>, _Term, Ans) -> Ans;
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_athlete(map()) -> strava_athlete:t().

to_athlete(Map) ->
    maps:fold(
      fun(K, V, Ans)
            when K =:= <<"id">>;
                 K =:= <<"firstname">>;
                 K =:= <<"lastname">>;
                 K =:= <<"profile_medium">>;
                 K =:= <<"profile">>;
                 K =:= <<"city">>;
                 K =:= <<"state">>;
                 K =:= <<"country">>;
                 K =:= <<"premium">>;
                 K =:= <<"follower_count">>;
                 K =:= <<"friend_count">>;
                 K =:= <<"mutual_friend_count">>;
                 K =:= <<"date_preference">>;
                 K =:= <<"email">>;
                 K =:= <<"ftp">>;
                 K =:= <<"weight">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         (<<"resource_state">>, Int, Ans) -> Ans#{resource_state => to_resource_state(Int)};
         (<<"sex">>, Str, Ans) -> Ans#{sex => Str}; % TODO
         (<<"friend">>, Str, Ans) -> Ans#{friend => Str}; % TODO
         (<<"follower">>, Str, Ans) -> Ans#{follower => Str}; % TODO
         (<<"created_at">>, Str, Ans) -> Ans#{created_at => to_datetime(Str)};
         (<<"updated_at">>, Str, Ans) -> Ans#{updated_at => to_datetime(Str)};
         (<<"athlete_type">>, Int, Ans) -> Ans#{athlete_type => Int}; % TODO
         (<<"measurement_preference">>, Str, Ans) -> Ans#{measurement_preference => Str}; % TODO
         (<<"clubs">>, List, Ans) -> Ans#{clubs => lists:map(fun to_club/1, List)};
         (<<"bikes">>, List, Ans) -> Ans#{bikes => lists:map(fun to_gear/1, List)};
         (<<"shoes">>, List, Ans) -> Ans#{shoes => lists:map(fun to_gear/1, List)};
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_athlete_stats(map()) -> strava_athlete:stats().

to_athlete_stats(Map) ->
    maps:fold(
      fun(K, V, Ans)
            when K =:= <<"biggest_ride_distance">>;
                 K =:= <<"biggest_climb_elevation_gain">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         (K, V, Ans)
            when K =:= <<"recent_ride_totals">>;
                 K =:= <<"recent_run_totals">>;
                 K =:= <<"ytd_ride_totals">>;
                 K =:= <<"ytd_run_totals">>;
                 K =:= <<"all_ride_totals">>;
                 K =:= <<"all_run_totals">> ->
              Ans#{binary_to_atom(K, latin1) => to_athlete_totals(V)};
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_athlete_totals(map()) -> strava_athlete:totals().

to_athlete_totals(Map) ->
    maps:fold(
      fun(K, V, Ans)
            when K =:= <<"achievement_count">>;
                 K =:= <<"count">>;
                 K =:= <<"distance">>;
                 K =:= <<"elapsed_time">>;
                 K =:= <<"elevation_gain">>;
                 K =:= <<"moving_time">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_club(map()) -> strava_club:t().

to_club(Map) ->
    maps:fold(
      fun(K, V, Ans)
            when K =:= <<"id">>;
                 K =:= <<"name">>;
                 K =:= <<"profile_medium">>;
                 K =:= <<"profile">>;
                 K =:= <<"description">>;
                 K =:= <<"city">>;
                 K =:= <<"state">>;
                 K =:= <<"country">>;
                 K =:= <<"private">>;
                 K =:= <<"member_count">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         (<<"resource_state">>, Int, Ans) -> Ans#{resource_state => to_resource_state(Int)};
         (<<"club_type">>, Str, Ans) -> Ans#{club_type => Str}; % TODO
         (<<"sport_type">>, Str, Ans) -> Ans#{sport_type => Str}; % TODO
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

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
-spec to_gear(map()) -> strava_gear:t().

to_gear(Map) ->
    maps:fold(
      fun(K, V, Ans)
         when K =:= <<"id">>;
              K =:= <<"primary">>;
              K =:= <<"name">>;
              K =:= <<"distance">>;
              K =:= <<"brand_name">>;
              K =:= <<"model_name">>;
              K =:= <<"description">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         (<<"resource_state">>, Int, Ans) -> Ans#{resource_state => to_resource_state(Int)};
         (<<"frame_type">>, Int, Ans) -> Ans#{frame_type => to_gear_frame_type(Int)};
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

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
-spec to_resource_state(integer()) -> strava:resource_state().

to_resource_state(1) -> meta;
to_resource_state(2) -> summary;
to_resource_state(3) -> detailed.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_segment_effort(map()) -> strava_segment_effort:t().

to_segment_effort(Map) ->
    maps:fold(
      fun(K, V, Ans)
            when K =:= <<"id">>;
                 K =:= <<"name">>;
                 K =:= <<"elapsed_time">>;
                 K =:= <<"moving_time">>;
                 K =:= <<"distance">>;
                 K =:= <<"start_index">>;
                 K =:= <<"end_index">>;
                 K =:= <<"average_cadence">>;
                 K =:= <<"average_watts">>;
                 K =:= <<"device_watts">>;
                 K =:= <<"average_heartrate">>;
                 K =:= <<"max_heartrate">>;
                 K =:= <<"kom_rank">>;
                 K =:= <<"pr_rank">>;
                 K =:= <<"hidden">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         (<<"resource_state">>, Int, Ans) -> Ans#{resource_state => to_resource_state(Int)};
         (<<"activity">>, Term, Ans) -> Ans#{activity => to_activity(Term)};
         (<<"athlete">>, Term, Ans) -> Ans#{athlete => to_athlete(Term)};
         (<<"start_date">>, Str, Ans) -> Ans#{start_date => to_datetime(Str)};
         (<<"start_date_local">>, Str, Ans) -> Ans#{start_date_local => to_datetime(Str)};
         (<<"segment">>, Term, Ans) -> Ans#{segment => to_segment(Term)};
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_segment(map()) -> strava_segment:t().

to_segment(Map) ->
    maps:fold(
      fun(K, V, Ans)
            when K =:= <<"id">>;
                 K =:= <<"name">>;
                 K =:= <<"distance">>;
                 K =:= <<"average_grade">>;
                 K =:= <<"maximum_grade">>;
                 K =:= <<"elevation_high">>;
                 K =:= <<"elevation_low">>;
                 K =:= <<"city">>;
                 K =:= <<"state">>;
                 K =:= <<"country">>;
                 K =:= <<"private">>;
                 K =:= <<"starred">>;
                 K =:= <<"total_elevation_gain">>;
                 K =:= <<"effort_count">>;
                 K =:= <<"athlete_count">>;
                 K =:= <<"hazardous">>;
                 K =:= <<"star_count">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         (<<"resource_state">>, Int, Ans) -> Ans#{resource_state => to_resource_state(Int)};
         (<<"activity_type">>, Str, Ans) -> Ans#{activity_type => Str}; % TODO
         (<<"start_latlng">>, List, Ans) -> Ans#{start_latlng => List}; % TODO
         (<<"end_latlng">>, List, Ans) -> Ans#{end_latlng => List}; % TODO
         (<<"climb_category">>, Int, Ans) -> Ans#{climb_category => Int}; % TODO
         (<<"created_at">>, Str, Ans) -> Ans#{created_at => to_datetime(Str)};
         (<<"updated_at">>, Str, Ans) -> Ans#{updated_at => to_datetime(Str)};
         (<<"map">>, Term, Ans) -> Ans#{map => Term};             % TODO
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_stream(map()) -> strava_stream:t().

to_stream(Map) ->
    maps:fold(
      fun(<<"type">>, Str, Ans) -> Ans#{type => Str}; % TODO
         (<<"data">>, List, Ans) -> Ans#{data => List}; % TODO
         (<<"series_type">>, Str, Ans) -> Ans#{series_type => Str}; % TODO
         (<<"original_size">>, Int, Ans) -> Ans#{original_size => Int};
         (<<"resolution">>, Str, Ans) -> Ans#{resolution => Str}; % TODO
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

%%%===================================================================
%%% To JSON functions
%%%===================================================================

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
                 K =:= sex;
                 K =:= weight ->
              Ans#{atom_to_binary(K, latin1) => V};
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).
