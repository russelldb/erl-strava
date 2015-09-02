-module(strava_activity).

%% Types
-export_type([activity/0, comment/0, lap/0, photo/0,
              t/0, type/0, zones/0]).

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

%%%===================================================================
%%% Types
%%%===================================================================

-type activity() :: map().
-type type() :: alpine_ski
              | backcountry_ski
              | canoeing
              | cross_country_skiing
              | crossfit
              | ebike_ride
              | elliptical
              | hike
              | ice_skate
              | inline_skate
              | kayaking
              | kitesurf
              | nordic_ski
              | ride
              | rock_climbing
              | roller_ski
              | rowing
              | run
              | snowboard
              | snowshoe
              | stair_stepper
              | stand_up_paddling
              | surfing
              | swim
              | virtual_ride
              | walk
              | weight_training
              | windsurf
              | workout
              | yoga.
-type comment() :: map().
-type lap() :: map().
-type photo() :: map().
-type zones() :: map().

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

activity(Token, Id) ->
    case strava_api:read(Token, [<<"activities">>, Id], _Opts = #{}) of
        {ok, JSON} -> strava_json:to_activity(JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava:auth_token()) -> [t()].

athletes(Token) ->
    athletes_args(Token, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

athletes(Token, Page, PerPage) ->
    athletes_args(Token, _Args = #{page     => Page,
                                   per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes_after(strava:auth_token(), integer()) -> [t()].

athletes_after(Token, Time) ->
    athletes_args(Token, _Args = #{'after' => Time}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes_before(strava:auth_token(), integer()) -> [t()].

athletes_before(Token, Time) ->
    athletes_args(Token, _Args = #{before => Time}).

%%--------------------------------------------------------------------
%% @doc
%% Create an activity.
%% @end
%%--------------------------------------------------------------------
-spec create(strava:auth_token(), t()) -> t().

create(Token, Activity) ->
    Content =
        maps:fold(
          fun(K, V, Ans)
                when K =:= name;
                     K =:= elapsed_time;
                     K =:= description;
                     K =:= distance ->
                  Ans#{atom_to_binary(K, latin1) => V};
             (K, V, Ans)
                when K =:= private;
                     K =:= trainer;
                     K =:= commute ->
                  Ans#{atom_to_binary(K, latin1) => case V of
                                                        true -> 1;
                                                        false -> 0
                                                    end};
             (type, Type, Ans) ->
                  Ans#{<<"type">> => strava_json:from_activity_type(Type)};
             (start_date_local, Date, Ans) ->
                  Ans#{<<"start_date_local">> => strava_json:from_datetime(Date)};
             (_K, _V, Ans) -> Ans
          end, _Ans = #{}, Activity),
    case strava_api:create(Token, [<<"activities">>], Content) of
        {ok, JSON} -> strava_json:to_activity(JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Delete an activity.
%% @end
%%--------------------------------------------------------------------
-spec delete(strava:auth_token(), integer()) -> ok.

delete(Token, Id) ->
    case strava_api:delete(Token, [<<"activities">>, Id]) of
        ok -> ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token()) -> [t()].

friends(Token) ->
    friends_args(Token, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

friends(Token, Page, PerPage) ->
    friends_args(Token, _Args = #{page     => Page,
                                  per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities.
%% @end
%%--------------------------------------------------------------------
-spec friends_before(strava:auth_token(), integer()) -> [t()].

friends_before(Token, Time) ->
    friends_args(Token, _Args = #{before => Time}).

%%--------------------------------------------------------------------
%% @doc
%% List activity laps.
%% @end
%%--------------------------------------------------------------------
-spec laps(strava:auth_token(), integer()) -> [lap()].

laps(Token, Id) ->
    laps_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List activity laps.
%% @end
%%--------------------------------------------------------------------
-spec laps(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [lap()].

laps(Token, Id, Page, PerPage) ->
    laps_args(Token, Id, _Args = #{page     => Page,
                                   per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List related activities.
%% @end
%%--------------------------------------------------------------------
-spec related(strava:auth_token(), integer()) -> [t()].

related(Token, Id) ->
    related_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List related activities.
%% @end
%%--------------------------------------------------------------------
-spec related(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].

related(Token, Id, Page, PerPage) ->
    related_args(Token, Id, _Args = #{page     => Page,
                                      per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Update an activity.
%% @end
%%--------------------------------------------------------------------
-spec update(strava:auth_token(), t()) -> t().

update(Token, Activity) ->
    #{id := Id} = Activity,
    Content =
        maps:fold(
          fun(K, V, Ans)
                when K =:= name;
                     K =:= private;
                     K =:= commute;
                     K =:= trainer;
                     K =:= description ->
                  Ans#{atom_to_binary(K, latin1) => V};
             (type, Type, Ans) -> Ans#{<<"type">> => strava_json:from_activity_type(Type)};
             (gear_id, undefined, Ans) -> Ans#{<<"gear_id">> => <<"none">>};
             (gear_id, Int, Ans) -> Ans#{<<"gear_id">> => Int};
             (_K, _V, Ans) -> Ans
          end, _Ans = #{}, Activity),
    case strava_api:update(Token, [<<"activities">>, Id], Content) of
        {ok, JSON} -> strava_json:to_activity(JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List activity zones.
%% @end
%%--------------------------------------------------------------------
-spec zones(strava:auth_token(), integer()) -> [zones()].

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
    comments_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List activity comments.
%% @end
%%--------------------------------------------------------------------
-spec comments(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [comment()].

comments(Token, Id, Page, PerPage) ->
    comments_args(Token, Id, _Args = #{page     => Page,
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
    kudoers_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List activity kudoers.
%% @end
%%--------------------------------------------------------------------
-spec kudoers(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [strava_athlete:t()].

kudoers(Token, Id, Page, PerPage) ->
    kudoers_args(Token, Id, _Args = #{page     => Page,
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
    photos_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List photos.
%% @end
%%--------------------------------------------------------------------
-spec photos(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [photo()].

photos(Token, Id, Page, PerPage) ->
    photos_args(Token, Id, _Args = #{page     => Page,
                                     per_page => PerPage}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes_args(strava:auth_token(), map()) -> [t()].

athletes_args(Token, Args) ->
    case strava_api:read(Token, [<<"athlete">>, <<"activities">>], Args) of
        {ok, JSON} -> lists:map(fun strava_json:to_activity/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities.
%% @end
%%--------------------------------------------------------------------
-spec friends_args(strava:auth_token(), map()) -> [t()].

friends_args(Token, Args) ->
    case strava_api:read(Token, [<<"activities">>, <<"following">>], Args) of
        {ok, JSON} -> lists:map(fun strava_json:to_activity/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List activity laps.
%% @end
%%--------------------------------------------------------------------
-spec laps_args(strava:auth_token(), integer(), map()) -> [lap()].

laps_args(Token, Id, Args) ->
    case strava_api:read(Token, [<<"activities">>, Id, <<"laps">>], Args) of
        {ok, JSON} -> lists:map(fun strava_json:to_activity_lap/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List related activities.
%% @end
%%--------------------------------------------------------------------
-spec related_args(strava:auth_token(), integer(), map()) -> [t()].

related_args(Token, Id, Args) ->
    case strava_api:read(Token, [<<"activities">>, Id, <<"related">>], Args) of
        {ok, JSON} -> lists:map(fun strava_json:to_activity/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List activity comments.
%% @end
%%--------------------------------------------------------------------
-spec comments_args(strava:auth_token(), integer(), map()) -> [comment()].

comments_args(Token, Id, Args) ->
    case strava_api:read(Token, [<<"activities">>, Id, <<"comments">>], Args) of
        {ok, JSON} -> lists:map(fun strava_json:to_activity_comment/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List activity kudoers.
%% @end
%%--------------------------------------------------------------------
-spec kudoers_args(strava:auth_token(), integer(), map()) -> [strava_athlete:t()].

kudoers_args(Token, Id, Args) ->
    case strava_api:read(Token, [<<"activities">>, Id, <<"kudos">>], Args) of
        {ok, JSON} -> lists:map(fun strava_json:to_athlete/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List photos.
%% @end
%%--------------------------------------------------------------------
-spec photos_args(strava:auth_token(), integer(), map()) -> [photo()].

photos_args(Token, Id, Args) ->
    Args1 = Args#{photo_sources => true},
    case strava_api:read(Token, [<<"activities">>, Id, <<"photos">>], Args1) of
        {ok, JSON} -> lists:map(fun strava_json:to_activity_photo/1, JSON)
    end.
