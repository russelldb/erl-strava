%%%-------------------------------------------------------------------
%%% @doc Types and functions related to Strava clubs.
%%% @reference http://strava.github.io/api/v3/clubs/
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(strava_club).

%% Types
-export_type([announcement/0, club/0, group_event/0, sport_type/0,
              t/0, type/0]).

%% Clubs functions
-export([activities/2, activities/4, activities_before/3,
         announcements/2, athletes/1, club/2, group_events/2, join/2,
         leave/2, members/2, members/4]).

%%%===================================================================
%%% Types
%%%===================================================================

-type announcement() :: map().
-type club() :: map().
-type group_event() :: map().
-type sport_type() :: cycling
                    | running
                    | triathlon
                    | other.
-type type() :: casual_club
              | racing_team
              | shop
              | company
              | other.

-type t() :: club().

%%%===================================================================
%%% Clubs functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List club activities. Retrieve the recent activities performed by
%% members of a specific club. The authenticated athlete must be a
%% member of the club.
%%
%% @see activities/4
%% @see activities_before/3
%% @end
%%--------------------------------------------------------------------
-spec activities(strava_auth:token(), integer()) ->
                        {ok, [strava_activity:t()]} | strava:error().

activities(Token, Id) ->
    activities_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List club activities. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec activities(strava_auth:token(), integer(), pos_integer(), pos_integer()) ->
                        {ok, [strava_activity:t()]} | strava:error().

activities(Token, Id, Page, PerPage) ->
    activities_args(Token, Id, _Args = #{page     => Page,
                                         per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List club activities. Only activities whose `start_date' is
%% before the specified POSIX timestamp will be returned.
%% @end
%%--------------------------------------------------------------------
-spec activities_before(strava_auth:token(), integer(), integer()) ->
                               {ok, [strava_activity:t()]} | strava:error().

activities_before(Token, Id, Time) ->
    activities_args(Token, Id, _Args = #{before => Time}).

%%--------------------------------------------------------------------
%% @doc
%% List club announcements. Announcements are posts sent by club
%% admins or owners to the members of a club. Only members of private
%% clubs can access their announcements.
%% @end
%%--------------------------------------------------------------------
-spec announcements(strava_auth:token(), integer()) ->
                           {ok, [announcement()]} | strava:error().

announcements(Token, Id) ->
    strava_api:convert(
      strava_api:read(Token, [<<"clubs">>, Id, <<"announcements">>]),
      {list, fun strava_repr:to_club_announcement/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% List athlete clubs. Fetch an array of clubs that the currently
%% authenticated athlete is a member of.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava_auth:token()) -> {ok, [t()]} | strava:error().

athletes(Token) ->
    strava_api:convert(
      strava_api:read(Token, [<<"athlete">>, <<"clubs">>]),
      {list, fun strava_repr:to_club/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve a club. Retrieve details about a specific club. The club
%% must be public or the current athlete must be a member.
%% @end
%%--------------------------------------------------------------------
-spec club(strava_auth:token(), integer()) -> {ok, t()} | strava:error().

club(Token, Id) ->
    strava_api:convert(
      strava_api:read(Token, [<<"clubs">>, Id]),
      fun strava_repr:to_club/1
     ).

%%--------------------------------------------------------------------
%% @doc
%% List club group events. Group events are optionally recurring
%% events for club members. Only club members can access private club
%% events.
%% @end
%%--------------------------------------------------------------------
-spec group_events(strava_auth:token(), integer()) ->
                          {ok, [group_event()]} | strava:error().

group_events(Token, Id) ->
    strava_api:convert(
      strava_api:read(Token, [<<"clubs">>, Id, <<"group_events">>]),
      {list, fun strava_repr:to_club_group_event/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% Join a club. The auth `Token' must have at least `write' scope.
%% @end
%%--------------------------------------------------------------------
-spec join(strava_auth:token(), integer()) -> ok | strava:error().

%% TODO: parse response, return as a result
join(Token, Id) ->
    strava_api:convert(
      strava_api:create(Token, [<<"clubs">>, Id, <<"join">>], #{})
     ).

%%--------------------------------------------------------------------
%% @doc
%% Leave a club. The auth `Token' must have at least `write' scope.
%% @end
%%--------------------------------------------------------------------
-spec leave(strava_auth:token(), integer()) -> ok | strava:error().

leave(Token, Id) ->
    strava_api:convert(
      strava_api:create(Token, [<<"clubs">>, Id, <<"leave">>], #{})
     ).

%%--------------------------------------------------------------------
%% @doc
%% List club members. Retrieve summary information about members of a
%% specific club.
%% @end
%%--------------------------------------------------------------------
-spec members(strava_auth:token(), integer()) ->
                     {ok, [strava_athlete:t()]} | strava:error().

members(Token, Id) ->
    members_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List club members. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec members(strava_auth:token(), integer(), pos_integer(), pos_integer()) ->
                     {ok, [strava_athlete:t()]} | strava:error().

members(Token, Id, Page, PerPage) ->
    members_args(Token, Id, _Args = #{page     => Page,
                                      per_page => PerPage}).

%% TODO: functions to retrieve admins

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List club activities.
%% @end
%%--------------------------------------------------------------------
-spec activities_args(strava_auth:token(), integer(), map()) ->
                             {ok, [strava_activity:t()]} | strava:error().

activities_args(Token, Id, Args) ->
    strava_api:convert(
      strava_api:read(Token, [<<"clubs">>, Id, <<"activities">>], Args),
      {list, fun strava_repr:to_activity/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% List club members.
%% @end
%%--------------------------------------------------------------------
-spec members_args(strava_auth:token(), integer(), map()) ->
                          {ok, [strava_athlete:t()]} | strava:error().

members_args(Token, Id, Args) ->
    strava_api:convert(
      strava_api:read(Token, [<<"clubs">>, Id, <<"members">>], Args),
      {list, fun strava_repr:to_athlete/1}
     ).
