-module(strava_activity).

%% Types
-export_type([activity/0, comment/0, lap/0, photo/0, t/0, zone/0]).

%% Activities functions
-export([activity/2, athletes/1, athletes/3, athletes_after/2,
         athletes_before/2, create/2, delete/2, friends/1, friends/3,
         friends_before/2, laps/2, laps/4, related/2, related/4,
         update/2, zones/2]).

%% Activity comments functions
-export([comments/2, comments/4]).

%% Activity kudos functions
-export([kudoers/2, kudoers/4]).

%% Activity photos functions
-export([photos/2, photos/4]).

%% To/from JSON functions
-export([to_activity/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type activity() :: map().
-type comment() :: map().
-type lap() :: map().
-type photo() :: map().
-type zone() :: map().

-type t() :: activity().

%%%===================================================================
%%% Activities functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an activity.
%% @end
%%--------------------------------------------------------------------
-spec activity(strava:auth_token(), integer()) -> t().

activity(_Token, _Id) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava:auth_token()) -> [t()].

athletes(Token) ->
    athletes(Token, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

athletes(Token, Page, PerPage) ->
    athletes(Token, _Args = #{page     => Page,
                              per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes_after(strava:auth_token(), integer()) -> [t()].

athletes_after(Token, Time) ->
    athletes(Token, _Args = #{'after' => Time}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes_before(strava:auth_token(), integer()) -> [t()].

athletes_before(Token, Time) ->
    athletes(Token, _Args = #{before => Time}).

%%--------------------------------------------------------------------
%% @doc
%% Create an activity.
%% @end
%%--------------------------------------------------------------------
-spec create(strava:auth_token(), t()) -> t().

create(_Token, _Activity) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% Delete an activity.
%% @end
%%--------------------------------------------------------------------
-spec delete(strava:auth_token(), integer()) -> ok.

delete(_Token, _Id) ->
    %% TODO
    ok.

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token()) -> [t()].

friends(Token) ->
    friends(Token, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

friends(Token, Page, PerPage) ->
    friends(Token, _Args = #{page     => Page,
                             per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities.
%% @end
%%--------------------------------------------------------------------
-spec friends_before(strava:auth_token(), integer()) -> [t()].

friends_before(Token, Time) ->
    friends(Token, _Args = #{before => Time}).

%%--------------------------------------------------------------------
%% @doc
%% List activity laps.
%% @end
%%--------------------------------------------------------------------
-spec laps(strava:auth_token(), integer()) -> [lap()].

laps(Token, Id) ->
    laps(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List activity laps.
%% @end
%%--------------------------------------------------------------------
-spec laps(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [lap()].

laps(Token, Id, Page, PerPage) ->
    laps(Token, Id, _Args = #{page     => Page,
                              per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List related activities.
%% @end
%%--------------------------------------------------------------------
-spec related(strava:auth_token(), integer()) -> [t()].

related(Token, Id) ->
    related(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List related activities.
%% @end
%%--------------------------------------------------------------------
-spec related(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].

related(Token, Id, Page, PerPage) ->
    related(Token, Id, _Args = #{page     => Page,
                                 per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Update an activity.
%% @end
%%--------------------------------------------------------------------
-spec update(strava:auth_token(), t()) -> t().

update(_Token, _Activity) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% List activity zones.
%% @end
%%--------------------------------------------------------------------
-spec zones(strava:auth_token(), integer()) -> [zone()].

zones(_Token, _Id) ->
    %% TODO
    [].

%%%===================================================================
%%% Activity comments functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List activity comments.
%% @end
%%--------------------------------------------------------------------
-spec comments(strava:auth_token(), integer()) -> [comment()].

comments(Token, Id) ->
    comments(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List activity comments.
%% @end
%%--------------------------------------------------------------------
-spec comments(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [comment()].

comments(Token, Id, Page, PerPage) ->
    comments(Token, Id, _Args = #{page     => Page,
                                  per_page => PerPage}).

%%%===================================================================
%%% Activity kudos functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List activity kudoers.
%% @end
%%--------------------------------------------------------------------
-spec kudoers(strava:auth_token(), integer()) -> [strava_athlete:t()].

kudoers(Token, Id) ->
    kudoers(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List activity kudoers.
%% @end
%%--------------------------------------------------------------------
-spec kudoers(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [strava_athlete:t()].

kudoers(Token, Id, Page, PerPage) ->
    kudoers(Token, Id, _Args = #{page     => Page,
                                 per_page => PerPage}).

%%%===================================================================
%%% Activity photos functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List photos.
%% @end
%%--------------------------------------------------------------------
-spec photos(strava:auth_token(), integer()) -> [photo()].

photos(Token, Id) ->
    photos(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List photos.
%% @end
%%--------------------------------------------------------------------
-spec photos(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [photo()].

photos(Token, Id, Page, PerPage) ->
    photos(Token, Id, _Args = #{page     => Page,
                                per_page => PerPage}).

%%%===================================================================
%%% To/from JSON map functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_activity(map()) -> t().

to_activity(Map) ->
    maps:foldl(
      fun({<<"id">>, Int}, Ans) -> Ans#{id => Int};
         ({<<"resource_state">>, Int}, Ans) -> Ans#{resource_state => Int}; % TODO
         ({<<"external_id">>, Str}, Ans) -> Ans#{external_id => Str};
         ({<<"upload_id">>, Int}, Ans) -> Ans#{upload_id => Int};
         ({<<"athlete">>, Term}, Ans) -> Ans#{athlete => strava_athlete:to_athlete(Term)};
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
         ({<<"gear">>, Term}, Ans) -> Ans#{gear => strava_gear:to_gear(Term)}; % TODO
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
         ({<<"segment_efforts">>, List}, Ans) ->
              Ans#{segment_efforts => lists:map(fun strava_segment_effort:to_segment_effort/1, List)};
         ({<<"splits_metric">>, _Term}, Ans) -> Ans;
         ({<<"splits_standard">>, _Term}, Ans) -> Ans;
         ({<<"best_efforts">>, _Term}, Ans) -> Ans;
         ({_K, _V}, Ans) -> Ans
      end, _Ans = #{}, Map).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava:auth_token(), map()) -> [t()].

athletes(_Token, _Args) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token(), map()) -> [t()].

friends(_Token, _Args) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List activity laps.
%% @end
%%--------------------------------------------------------------------
-spec laps(strava:auth_token(), integer(), map()) -> [lap()].

laps(_Token, _Id, _Args) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List related activities.
%% @end
%%--------------------------------------------------------------------
-spec related(strava:auth_token(), integer(), map()) -> [t()].

related(_Token, _Id, _Args) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List activity comments.
%% @end
%%--------------------------------------------------------------------
-spec comments(strava:auth_token(), integer(), map()) -> [comment()].

comments(_Token, _Id, _Args) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List activity kudoers.
%% @end
%%--------------------------------------------------------------------
-spec kudoers(strava:auth_token(), integer(), map()) -> [strava_athlete:t()].

kudoers(_Token, _Id, _Args) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List photos.
%% @end
%%--------------------------------------------------------------------
-spec photos(strava:auth_token(), integer(), map()) -> [photo()].

photos(_Token, _Id, _Args) ->
    %% TODO
    [].
