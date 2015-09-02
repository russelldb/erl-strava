-module(strava_json).

%% Parse JSON
-export([decode/1]).

%% From JSON map functions
-export([to_activity/1, to_athlete/1, to_athlete_stats/1, to_club/1,
         to_club_announcement/1, to_club_group_event/1, to_gear/1,
         to_segment/1, to_segment_effort/1, to_stream/1]).

%% To JSON functions
-export([from_activity_type/1, from_athlete/1, from_datetime/1]).

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
      fun(_K, _V = null, Ans) -> Ans;           % Ignore null fields
         (K, V, Ans)
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
         (<<"type">>, Str, Ans) -> Ans#{type => to_activity_type(Str)};
         (<<"start_date">>, Str, Ans) -> Ans#{start_date => to_datetime(Str)};
         (<<"start_date_local">>, Str, Ans) -> Ans#{start_date_local => to_datetime(Str)};
         (<<"start_latlng">>, List, Ans) -> Ans#{start_latlng => to_position(List)};
         (<<"end_latlng">>, List, Ans) -> Ans#{end_latlng => to_position(List)};
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
-spec to_activity_type(binary()) -> strava_activity:type().

to_activity_type(<<"AlpineSki">>) -> alpine_ski;
to_activity_type(<<"BackcountrySki">>) -> backcountry_ski;
to_activity_type(<<"Canoeing">>) -> canoeing;
to_activity_type(<<"CrossCountrySkiing">>) -> cross_country_skiing;
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
-spec to_athlete(map()) -> strava_athlete:t().

to_athlete(Map) ->
    maps:fold(
      fun(_K, _V = null, Ans) -> Ans;           % Ignore null fields
         (K, V, Ans)
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
         (<<"sex">>, Str, Ans) -> Ans#{sex => to_athlete_sex(Str)};
         (<<"friend">>, Str, Ans) -> Ans#{friend => to_athlete_friend_status(Str)};
         (<<"follower">>, Str, Ans) -> Ans#{follower => to_athlete_friend_status(Str)};
         (<<"created_at">>, Str, Ans) -> Ans#{created_at => to_datetime(Str)};
         (<<"updated_at">>, Str, Ans) -> Ans#{updated_at => to_datetime(Str)};
         (<<"athlete_type">>, Int, Ans) -> Ans#{athlete_type => to_athlete_type(Int)};
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
    maps:fold(
      fun(_K, _V = null, Ans) -> Ans;           % Ignore null fields
         (K, V, Ans)
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
      fun(_K, _V = null, Ans) -> Ans;           % Ignore null fields
         (K, V, Ans)
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
-spec to_athlete_type(integer()) -> strava_athlete:type().

to_athlete_type(0) -> cyclist;
to_athlete_type(1) -> runner.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_club(map()) -> strava_club:t().

to_club(Map) ->
    maps:fold(
      fun(_K, _V = null, Ans) -> Ans;           % Ignore null fields
         (K, V, Ans)
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
         (<<"club_type">>, Str, Ans) -> Ans#{club_type => to_club_type(Str)};
         (<<"sport_type">>, Str, Ans) -> Ans#{sport_type => to_club_sport_type(Str)};
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_club_announcement(map()) -> strava_club:announcement().

to_club_announcement(Map) ->
    maps:fold(
      fun(_K, _V = null, Ans) -> Ans;           % Ignore null fields
         (K, V, Ans)
            when K =:= <<"id">>;
                 K =:= <<"club_id">>;
                 K =:= <<"message">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         (<<"athlete">>, Term, Ans) -> Ans#{athlete => to_athlete(Term)};
         (<<"resource_state">>, Int, Ans) -> Ans#{resource_state => to_resource_state(Int)};
         (<<"created_at">>, Str, Ans) -> Ans#{created_at => to_datetime(Str)};
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_club_group_event(map()) -> strava_club:group_event().

to_club_group_event(Map) ->
    maps:fold(
      fun(_K, _V = null, Ans) -> Ans;           % Ignore null fields
         (K, V, Ans)
            when K =:= <<"id">>;
                 K =:= <<"title">>;
                 K =:= <<"description">>;
                 K =:= <<"club_id">>;
                 K =:= <<"route_id">>;
                 K =:= <<"woman_only">>;
                 K =:= <<"private">>;
                 K =:= <<"address">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         (<<"resource_state">>, Int, Ans) -> Ans#{resource_state => to_resource_state(Int)};
         (<<"organizing_athlete">>, Term, Ans) -> Ans#{organizing_athlete => to_athlete(Term)};
         (<<"activity_type">>, Str, Ans) -> Ans#{activity_type => Str}; % TODO
         (<<"created_at">>, Str, Ans) -> Ans#{created_at => to_datetime(Str)};
         (<<"skill_levels">>, Int, Ans) -> Ans#{skill_levels => Int}; % TODO
         (<<"terrain">>, Int, Ans) -> Ans#{terrain => Int}; % TODO
         (<<"upcoming_occurrences">>, List, Ans) -> Ans#{upcoming_occurrences => lists:map(fun to_datetime/1, List)};
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

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
-spec to_gear(map()) -> strava_gear:t().

to_gear(Map) ->
    maps:fold(
      fun(_K, _V = null, Ans) -> Ans;           % Ignore null fields
         (K, V, Ans)
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
-spec to_position([number()]) -> strava:position().

to_position([Lat, Lon]) -> {Lat, Lon}.

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
      fun(_K, _V = null, Ans) -> Ans;           % Ignore null fields
         (K, V, Ans)
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
      fun(_K, _V = null, Ans) -> Ans;           % Ignore null fields
         (K, V, Ans)
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
         (<<"start_latlng">>, List, Ans) -> Ans#{start_latlng => to_position(List)};
         (<<"end_latlng">>, List, Ans) -> Ans#{end_latlng => to_position(List)};
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
      fun(_K, _V = null, Ans) -> Ans;                   % Ignore null fields
         (<<"type">>, Str, Ans) -> Ans#{type => Str}; % TODO
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
-spec from_activity_type(strava_activity:type()) -> binary().

from_activity_type(alpine_ski) -> <<"AlpineSki">>;
from_activity_type(backcountry_ski) -> <<"BackcountrySki">>;
from_activity_type(canoeing) -> <<"Canoeing">>;
from_activity_type(cross_country_skiing) -> <<"CrossCountrySkiing">>;
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
                 K =:= sex;
                 K =:= weight ->
              Ans#{atom_to_binary(K, latin1) => V};
         (_K, _V, Ans) -> Ans
      end, _Ans = #{}, Map).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_datetime(calendar:datetime()) -> binary().

from_datetime(DateTime) ->
    iolist_to_binary(iso8601:format_datetime(DateTime)).
