%%%-------------------------------------------------------------------
%%% @doc
%%% Types and functions related to Strava activities.
%%% @reference http://strava.github.io/api/v3/activities/.
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(strava_activity).

%% Types
-export_type([activity/0, comment/0, lap/0, photo/0,
              t/0, type/0, zones/0, zones_bucket/0]).

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
-type zones_bucket() :: map().

-type t() :: activity().

%%%===================================================================
%%% Activities functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an activity. Returns a detailed representation if the
%% activity is owned by the requesting athlete. Returns a summary
%% representation for all other requests.
%% @end
%%--------------------------------------------------------------------
-spec activity(strava_auth:token(), integer()) ->
                      {ok, t()} | strava:error().

activity(Token, Id) ->
    strava_api:convert(
      strava_api:read(Token, [<<"activities">>, Id]),
      fun strava_repr:to_activity/1
     ).

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities. Returns a list of activities for the
%% authenticated user. Activities are in summary representations,
%% sorted newest first.
%%
%% @see athletes/3
%% @see athletes_after/2
%% @see athletes_before/2
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava_auth:token()) -> {ok, [t()]} | strava:error().

athletes(Token) ->
    athletes_args(Token, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities. Variant of `athlete/1' function with
%% pagination.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava_auth:token(), pos_integer(), pos_integer()) ->
                      {ok, [t()]} | strava:error().

athletes(Token, Page, PerPage) ->
    athletes_args(Token, _Args = #{page     => Page,
                                   per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities. Only activities whose `start_date' is
%% after the specified POSIX timestamp will be returned. Activities
%% will be sorted oldest first.
%% @end
%%--------------------------------------------------------------------
-spec athletes_after(strava_auth:token(), integer()) ->
                            {ok, [t()]} | strava:error().

athletes_after(Token, Time) ->
    athletes_args(Token, _Args = #{'after' => Time}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities. Only activities whose `start_date' is
%% before the specified POSIX timestamp will be returned.
%% @end
%%--------------------------------------------------------------------
-spec athletes_before(strava_auth:token(), integer()) ->
                             {ok, [t()]} | strava:error().

athletes_before(Token, Time) ->
    athletes_args(Token, _Args = #{before => Time}).

%%--------------------------------------------------------------------
%% @doc
%% Create an activity. This function is for creating manually entered
%% activities. For the list of relevant fields of `Activity' see
%% Strava documentation. `Token' must have at least `write' scope.
%% @end
%%--------------------------------------------------------------------
-spec create(strava_auth:token(), t()) -> {ok, t()} | strava:error().

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
                  Ans#{<<"type">> => strava_repr:from_activity_type(Type)};
             (start_date_local, Date, Ans) ->
                  Ans#{<<"start_date_local">> => strava_repr:from_datetime(Date)};
             (_K, _V, Ans) -> Ans
          end, _Ans = #{}, Activity),
    strava_api:convert(
      strava_api:create(Token, [<<"activities">>], Content),
      fun strava_repr:to_activity/1
     ).

%%--------------------------------------------------------------------
%% @doc
%% Delete an activity. The provided auth `Token' must of at least
%% `write' scope.
%% @end
%%--------------------------------------------------------------------
-spec delete(strava_auth:token(), integer()) -> ok | strava:error().

delete(Token, Id) ->
    strava_api:convert(
      strava_api:delete(Token, [<<"activities">>, Id])
     ).

%%--------------------------------------------------------------------
%% @doc
%% List friends' activities. List the recent activities performed by
%% the current athlete and those they are following.
%%
%% @see friends/3
%% @see friends_before/2
%% @end
%%--------------------------------------------------------------------
-spec friends(strava_auth:token()) -> {ok, [t()]} | strava:error().

friends(Token) ->
    friends_args(Token, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava_auth:token(), pos_integer(), pos_integer()) ->
                     {ok, [t()]} | strava:error().

friends(Token, Page, PerPage) ->
    friends_args(Token, _Args = #{page     => Page,
                                  per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities. Only activities whose `start_date' is
%% before the specified POSIX timestamp will be returned.
%% @end
%%--------------------------------------------------------------------
-spec friends_before(strava_auth:token(), integer()) ->
                            {ok, [t()]} | strava:error().

friends_before(Token, Time) ->
    friends_args(Token, _Args = #{before => Time}).

%%--------------------------------------------------------------------
%% @doc
%% List activity laps. Will return all laps for an activity.
%%
%% @see laps/4
%% @end
%%--------------------------------------------------------------------
-spec laps(strava_auth:token(), integer()) ->
                  {ok, [lap()]} | strava:error().

laps(Token, Id) ->
    laps_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List activity laps. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec laps(strava_auth:token(), integer(), pos_integer(), pos_integer()) ->
                  {ok, [lap()]} | strava:error().

laps(Token, Id, Page, PerPage) ->
    laps_args(Token, Id, _Args = #{page     => Page,
                                   per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List related activities. Returns the activities that were matched
%% as "with this group".
%%
%% @see related/4
%% @end
%%--------------------------------------------------------------------
-spec related(strava_auth:token(), integer()) ->
                     {ok, [t()]} | strava:error().

related(Token, Id) ->
    related_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List related activities. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec related(strava_auth:token(), integer(), pos_integer(), pos_integer()) ->
                     {ok, [t()]} | strava:error().

related(Token, Id, Page, PerPage) ->
    related_args(Token, Id, _Args = #{page     => Page,
                                      per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Update an activity. The auth `Token' must have at least `write'
%% scope. For the list of relevant fields of `Activity' see Strava
%% documentation.
%% @end
%%--------------------------------------------------------------------
-spec update(strava_auth:token(), t()) -> {ok, t()} | strava:error().

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
             (type, Type, Ans) -> Ans#{<<"type">> => strava_repr:from_activity_type(Type)};
             (gear_id, undefined, Ans) -> Ans#{<<"gear_id">> => <<"none">>};
             (gear_id, Int, Ans) -> Ans#{<<"gear_id">> => Int};
             (_K, _V, Ans) -> Ans
          end, _Ans = #{}, Activity),
    strava_api:convert(
      strava_api:update(Token, [<<"activities">>, Id], Content),
      fun strava_repr:to_activity/1
     ).

%%--------------------------------------------------------------------
%% @doc
%% List activity zones. Heartrate and power zone distribution of the
%% activity. The `Token' must be from a premium user.
%% @end
%%--------------------------------------------------------------------
-spec zones(strava_auth:token(), integer()) ->
                   {ok, [zones()]} | strava:error().

zones(Token, Id) ->
    strava_api:convert(
      strava_api:read(Token, [<<"activities">>, Id, <<"zones">>]),
      {list, fun strava_repr:to_activity_zones/1}
     ).

%%%===================================================================
%%% Activity comments functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List activity comments.
%%
%% @see comments/4
%% @end
%%--------------------------------------------------------------------
-spec comments(strava_auth:token(), integer()) ->
                      {ok, [comment()]} | strava:error().

comments(Token, Id) ->
    comments_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List activity comments. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec comments(strava_auth:token(), integer(), pos_integer(), pos_integer()) ->
                      {ok, [comment()]} | strava:error().

comments(Token, Id, Page, PerPage) ->
    comments_args(Token, Id, _Args = #{page     => Page,
                                       per_page => PerPage}).

%%%===================================================================
%%% Activity kudos functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List activity kudoers.
%%
%% @see kudoers/4
%% @end
%%--------------------------------------------------------------------
-spec kudoers(strava_auth:token(), integer()) ->
                     {ok, [strava_athlete:t()]} | strava:error().

kudoers(Token, Id) ->
    kudoers_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List activity kudoers. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec kudoers(strava_auth:token(), integer(), pos_integer(), pos_integer()) ->
                     {ok, [strava_athlete:t()]} | strava:error().

kudoers(Token, Id, Page, PerPage) ->
    kudoers_args(Token, Id, _Args = #{page     => Page,
                                      per_page => PerPage}).

%%%===================================================================
%%% Activity photos functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List photos.
%%
%% @see photos/4
%% @end
%%--------------------------------------------------------------------
-spec photos(strava_auth:token(), integer()) ->
                    {ok, [photo()]} | strava:error().

photos(Token, Id) ->
    photos_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List photos. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec photos(strava_auth:token(), integer(), pos_integer(), pos_integer()) ->
                    {ok, [photo()]} | strava:error().

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
-spec athletes_args(strava_auth:token(), map()) ->
                           {ok, [t()]} | strava:error().

athletes_args(Token, Args) ->
    strava_api:convert(
      strava_api:read(Token, [<<"athlete">>, <<"activities">>], Args),
      {list, fun strava_repr:to_activity/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% List friends' activities.
%% @end
%%--------------------------------------------------------------------
-spec friends_args(strava_auth:token(), map()) ->
                          {ok, [t()]} | strava:error().

friends_args(Token, Args) ->
    strava_api:convert(
      strava_api:read(Token, [<<"activities">>, <<"following">>], Args),
      {list, fun strava_repr:to_activity/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% List activity laps.
%% @end
%%--------------------------------------------------------------------
-spec laps_args(strava_auth:token(), integer(), map()) ->
                       {ok, [lap()]} | strava:error().

laps_args(Token, Id, Args) ->
    strava_api:convert(
      strava_api:read(Token, [<<"activities">>, Id, <<"laps">>], Args),
      {list, fun strava_repr:to_activity_lap/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% List related activities.
%% @end
%%--------------------------------------------------------------------
-spec related_args(strava_auth:token(), integer(), map()) ->
                          {ok, [t()]} | strava:error().

related_args(Token, Id, Args) ->
    strava_api:convert(
      strava_api:read(Token, [<<"activities">>, Id, <<"related">>], Args),
      {list, fun strava_repr:to_activity/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% List activity comments.
%% @end
%%--------------------------------------------------------------------
-spec comments_args(strava_auth:token(), integer(), map()) ->
                           {ok, [comment()]} | strava:error().

comments_args(Token, Id, Args) ->
    strava_api:convert(
      strava_api:read(Token, [<<"activities">>, Id, <<"comments">>], Args),
      {list, fun strava_repr:to_activity_comment/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% List activity kudoers.
%% @end
%%--------------------------------------------------------------------
-spec kudoers_args(strava_auth:token(), integer(), map()) ->
                          {ok, [strava_athlete:t()]} | strava:error().

kudoers_args(Token, Id, Args) ->
    strava_api:convert(
      strava_api:read(Token, [<<"activities">>, Id, <<"kudos">>], Args),
      {list, fun strava_repr:to_athlete/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% List photos.
%% @end
%%--------------------------------------------------------------------
-spec photos_args(strava_auth:token(), integer(), map()) ->
                         {ok, [photo()]} | strava:error().

photos_args(Token, Id, Args) ->
    Args1 = Args#{photo_sources => true},
    strava_api:convert(
      strava_api:read(Token, [<<"activities">>, Id, <<"photos">>], Args1),
      {list, fun strava_repr:to_activity_photo/1}
     ).
