-module(strava_json).

%% To/from JSON map functions
-export([to_activity/1, to_athlete/1, to_club/1, to_gear/1,
         to_segment/1, to_segment_effort/1]).

%%%===================================================================
%%% To/from JSON map functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_activity(map()) -> strava_activity:t().

to_activity(Map) ->
    maps:fold(
      fun({<<"id">>, Int}, Ans) -> Ans#{id => Int};
         ({<<"resource_state">>, Int}, Ans) -> Ans#{resource_state => Int}; % TODO
         ({<<"external_id">>, Str}, Ans) -> Ans#{external_id => Str};
         ({<<"upload_id">>, Int}, Ans) -> Ans#{upload_id => Int};
         ({<<"athlete">>, Term}, Ans) -> Ans#{athlete => to_athlete(Term)};
         ({<<"name">>, Str}, Ans) -> Ans#{name => Str};
         ({<<"description">>, Str}, Ans) -> Ans#{description => Str};
         ({<<"distance">>, Num}, Ans) -> Ans#{distance => Num};
         ({<<"moving_time">>, Int}, Ans) -> Ans#{moving_time => Int};
         ({<<"elapsed_time">>, Int}, Ans) -> Ans#{elapsed_time => Int};
         ({<<"total_elevation_gain">>, Num}, Ans) -> Ans#{total_elevation_gain => Num};
         ({<<"type">>, Str}, Ans) -> Ans#{type => Str}; % TODO
         ({<<"start_date">>, Str}, Ans) -> Ans#{start_date => Str}; % TODO
         ({<<"start_date_local">>, Str}, Ans) -> Ans#{start_date_local => Str}; % TODO
         ({<<"timezone">>, Str}, Ans) -> Ans#{timezone => Str};
         ({<<"start_latlng">>, List}, Ans) -> Ans#{start_latlng => List}; % TODO
         ({<<"end_latlng">>, List}, Ans) -> Ans#{end_latlng => List}; % TODO
         ({<<"location_city">>, Str}, Ans) -> Ans#{location_city => Str};
         ({<<"location_state">>, Str}, Ans) -> Ans#{location_state => Str};
         ({<<"location_country">>, Str}, Ans) -> Ans#{location_country => Str};
         ({<<"achievement_count">>, Int}, Ans) -> Ans#{achievement_count => Int};
         ({<<"kudos_count">>, Int}, Ans) -> Ans#{kudos_count => Int};
         ({<<"comment_count">>, Int}, Ans) -> Ans#{comment_count => Int};
         ({<<"athlete_count">>, Int}, Ans) -> Ans#{athlete_count => Int};
         ({<<"photo_count">>, Int}, Ans) -> Ans#{photo_count => Int};
         ({<<"total_photo_count">>, Int}, Ans) -> Ans#{total_photo_count => Int};
         ({<<"photos">>, List}, Ans) -> Ans#{photos => List}; % TODO
         ({<<"map">>, Term}, Ans) -> Ans#{map => Term};         % TODO
         ({<<"trainer">>, Bool}, Ans) -> Ans#{trainer => Bool};
         ({<<"commute">>, Bool}, Ans) -> Ans#{commute => Bool};
         ({<<"manual">>, Bool}, Ans) -> Ans#{manual => Bool};
         ({<<"private">>, Bool}, Ans) -> Ans#{private => Bool};
         ({<<"flagged">>, Bool}, Ans) -> Ans#{flagged => Bool};
         ({<<"workout_type">>, Int}, Ans) -> Ans#{workout_type => Int}; % TODO
         ({<<"gear_id">>, Int}, Ans) -> Ans#{gear_id => Int};
         ({<<"gear">>, Term}, Ans) -> Ans#{gear => to_gear(Term)}; % TODO
         ({<<"average_speed">>, Num}, Ans) -> Ans#{average_speed => Num};
         ({<<"max_speed">>, Num}, Ans) -> Ans#{max_speed => Num};
         ({<<"average_cadence">>, Num}, Ans) -> Ans#{average_cadence => Num};
         ({<<"average_temp">>, Num}, Ans) -> Ans#{average_temp => Num};
         ({<<"average_watts">>, Num}, Ans) -> Ans#{average_watts => Num};
         ({<<"weighted_average_watts">>, Num}, Ans) -> Ans#{weighted_average_watts => Num};
         ({<<"kilojoules">>, Num}, Ans) -> Ans#{kilojoules => Num};
         ({<<"device_watts">>, Bool}, Ans) -> Ans#{device_watts => Bool};
         ({<<"average_heartrate">>, Num}, Ans) -> Ans#{average_heartrate => Num};
         ({<<"max_heartrate">>, Int}, Ans) -> Ans#{max_heartrate => Int};
         ({<<"calories">>, Num}, Ans) -> Ans#{calories => Num};
         ({<<"has_kudoed">>, Bool}, Ans) -> Ans#{has_kudoed => Bool};
         ({<<"segment_efforts">>, List}, Ans) -> Ans#{segment_efforts => lists:map(fun to_segment_effort/1, List)};
         ({<<"splits_metric">>, _Term}, Ans) -> Ans;
         ({<<"splits_standard">>, _Term}, Ans) -> Ans;
         ({<<"best_efforts">>, _Term}, Ans) -> Ans;
         ({_K, _V}, Ans) -> Ans
      end, _Ans = #{}, Map).

%%%===================================================================
%%% To/from JSON map functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_athlete(map()) -> strava_athlete:t().

to_athlete(Map) ->
    maps:fold(
      fun({K, V}, Ans)
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
         ({<<"resource_state">>, Int}, Ans) -> Ans#{resource_state => Int}; % TODO
         ({<<"sex">>, Str}, Ans) -> Ans#{sex => Str}; % TODO
         ({<<"friend">>, Str}, Ans) -> Ans#{friend => Str}; % TODO
         ({<<"follower">>, Str}, Ans) -> Ans#{follower => Str}; % TODO
         ({<<"created_at">>, Str}, Ans) -> Ans#{created_at => Str}; % TODO
         ({<<"updated_at">>, Str}, Ans) -> Ans#{updated_at => Str}; % TODO
         ({<<"athlete_type">>, Int}, Ans) -> Ans#{athlete_type => Int}; % TODO
         ({<<"measurement_preference">>, Str}, Ans) -> Ans#{measurement_preference => Str}; % TODO
         ({<<"clubs">>, List}, Ans) -> Ans#{clubs => lists:map(fun to_club/1, List)};
         ({<<"bikes">>, List}, Ans) -> Ans#{bikes => lists:map(fun to_gear/1, List)};
         ({<<"shoes">>, List}, Ans) -> Ans#{shoes => lists:map(fun to_gear/1, List)};
         ({_K, _V}, Ans) -> Ans
      end, _Ans = #{}, Map).

%%%===================================================================
%%% To/from JSON map functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_club(map()) -> strava_club:t().

to_club(Map) ->
    maps:fold(
      fun({K, V}, Ans)
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
         ({<<"resource_state">>, Int}, Ans) -> Ans#{resource_state => Int}; % TODO
         ({<<"club_type">>, Str}, Ans) -> Ans#{club_type => Str}; % TODO
         ({<<"sport_type">>, Str}, Ans) -> Ans#{sport_type => Str}; % TODO
         ({_K, _V}, Ans) -> Ans
      end, _Ans = #{}, Map).

%%%===================================================================
%%% To/from JSON functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_gear(map()) -> strava_gear:t().

to_gear(Map) ->
    maps:fold(
      fun({K, V}, Ans)
         when K =:= <<"id">>;
              K =:= <<"primary">>;
              K =:= <<"name">>;
              K =:= <<"distance">>;
              K =:= <<"brand_name">>;
              K =:= <<"model_name">>;
              K =:= <<"description">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         ({<<"resource_state">>, Int}, Ans) -> Ans#{resource_state => Int}; % TODO
         ({<<"frame_type">>, Int}, Ans) -> Ans#{frame_type => Int}; % TODO
         ({_K, _V}, Ans) -> Ans
      end, _Ans = #{}, Map).

%%%===================================================================
%%% To/from JSON functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_segment_effort(map()) -> strava_segment_effort:t().

to_segment_effort(Map) ->
    maps:fold(
      fun({K, V}, Ans)
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
         ({<<"resource_state">>, Int}, Ans) -> Ans#{resource_state => Int}; % TODO
         ({<<"activity">>, Term}, Ans) -> Ans#{activity => to_activity(Term)};
         ({<<"athlete">>, Term}, Ans) -> Ans#{athlete => to_athlete(Term)};
         ({<<"start_date">>, Str}, Ans) -> Ans#{start_date => Str}; % TODO
         ({<<"start_date_local">>, Str}, Ans) -> Ans#{start_date_local => Str}; % TODO
         ({<<"segment">>, Term}, Ans) -> Ans#{segment => to_segment(Term)};
         ({_K, _V}, Ans) -> Ans
      end, _Ans = #{}, Map).

%%%===================================================================
%%% To/from JSON functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_segment(map()) -> strava_segment:t().

to_segment(Map) ->
    maps:fold(
      fun({K, V}, Ans)
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
         ({<<"resource_state">>, Int}, Ans) -> Ans#{resource_state => Int}; % TODO
         ({<<"activity_type">>, Str}, Ans) -> Ans#{activity_type => Str}; % TODO
         ({<<"start_latlng">>, List}, Ans) -> Ans#{start_latlng => List}; % TODO
         ({<<"end_latlng">>, List}, Ans) -> Ans#{end_latlng => List}; % TODO
         ({<<"climb_category">>, Int}, Ans) -> Ans#{climb_category => Int}; % TODO
         ({<<"created_at">>, Str}, Ans) -> Ans#{created_at => Str}; % TODO
         ({<<"updated_at">>, Str}, Ans) -> Ans#{updated_at => Str}; % TODO
         ({<<"map">>, Term}, Ans) -> Ans#{map => Term};             % TODO
         ({_K, _V}, Ans) -> Ans
      end, _Ans = #{}, Map).
