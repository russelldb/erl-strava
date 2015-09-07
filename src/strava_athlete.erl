%%%-------------------------------------------------------------------
%%% @doc
%%% Types and functions related to Strava athletes.
%%% @reference http://strava.github.io/api/v3/athlete/
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(strava_athlete).

%% Types
-export_type([athlete/0, friend_status/0, sex/0, stats/0, t/0,
              totals/0, type/0]).

%% Athletes API functions
-export([athlete/1, athlete/2, koms/2, koms/4, stats/2,
         update/2]).

%% Friends and followers API functions
-export([both_following/2, both_following/4, followers/1, followers/2,
         followers/3, followers/4, friends/1, friends/2, friends/3,
         friends/4]).

%%%===================================================================
%%% Types
%%%===================================================================

-type athlete() :: map().
-type friend_status() :: pending | accepted | blocked.
-type sex() :: male | female.
-type stats() :: map().
-type totals() :: map().
-type type() :: cyclist | runner.

-type t() :: athlete().

%%%===================================================================
%%% Athletes API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve current athlete. Information about the currently
%% authenticated athlete.
%% @end
%%--------------------------------------------------------------------
-spec athlete(strava_auth:token()) -> t().

athlete(Token) ->
    athlete(Token, _Id = undefined).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve another athlete. Information about any athlete on Strava.
%% @end
%%--------------------------------------------------------------------
-spec athlete(strava_auth:token(), integer()) -> t().

athlete(Token, Id) ->
    Path = case Id of
               undefined -> [<<"athlete">>];
               _SomeId -> [<<"athletes">>, Id]
           end,
    case strava_api:read(Token, Path) of
        {ok, JSON} -> strava_repr:to_athlete(JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List athlete K/QOMs/CRs. Returns an array of segment efforts
%% representing overall KOMs/QOMs and course records held by the given
%% athlete. Results are sorted by date, newest first.
%%
%% @see koms/4
%% @end
%%--------------------------------------------------------------------
-spec koms(strava_auth:token(), integer()) -> [strava_segment_effort:t()].

koms(Token, Id) ->
    koms_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete K/QOMs/CRs. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec koms(strava_auth:token(), integer(), pos_integer(), pos_integer()) ->
                  [strava_segment_effort:t()].

koms(Token, Id, Page, PerPage) ->
    koms_args(Token, Id, _Args = #{page     => Page,
                                   per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Totals and stats. Returns recent (last 4 weeks), year to date and
%% all time stats for a given athlete. Only available for the
%% authenticated athlete. This is the recommended function when
%% polling for athlete upload events.
%% @end
%%--------------------------------------------------------------------
-spec stats(strava_auth:token(), integer()) -> stats().

stats(Token, Id) ->
    case strava_api:read(Token, [<<"athletes">>, Id, <<"stats">>]) of
        {ok, JSON} -> strava_repr:to_athlete_stats(JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Update current athlete. The auth `Token' must have at least `write'
%% scope. For the list of relevant fields of the `Athlete' see Strava
%% documentation.
%% @end
%%--------------------------------------------------------------------
-spec update(strava_auth:token(), t()) -> t().

update(Token, Athlete) ->
    Content = strava_repr:from_athlete(Athlete),
    case strava_api:update(Token, [<<"athlete">>], Content) of
        {ok, JSON} -> strava_repr:to_athlete(JSON)
    end.

%%%===================================================================
%%% Friends and followers API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List current athlete friends.
%%
%% @see friends/3
%% @end
%%--------------------------------------------------------------------
-spec friends(strava_auth:token()) -> [t()].

friends(Token) ->
    friends_args(Token, _Id = undefined, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List current athlete friends. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava_auth:token(), pos_integer(), pos_integer()) -> [t()].

friends(Token, Page, PerPage) ->
    friends_args(Token, _Id = undefined, _Args = #{page     => Page,
                                                   per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List current athlete followers.
%%
%% @see followers/3
%% @end
%%--------------------------------------------------------------------
-spec followers(strava_auth:token()) -> [t()].

followers(Token) ->
    followers_args(Token, _Id = undefined, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List current athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava_auth:token(), pos_integer(), pos_integer()) -> [t()].

followers(Token, Page, PerPage) ->
    followers_args(Token, _Id = undefined, _Args = #{page     => Page,
                                                     per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List any athlete friends. If the indicated athlete has blocked the
%% authenticated athlete, the result will be an empty array.
%%
%% @see friends/4
%% @end
%%--------------------------------------------------------------------
-spec friends(strava_auth:token(), integer()) -> [t()].

friends(Token, Id) ->
    friends_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List any athlete friends. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava_auth:token(), integer(), pos_integer(), pos_integer()) -> [t()].

friends(Token, Id, Page, PerPage) ->
    friends_args(Token, Id, _Args = #{page     => Page,
                                      per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List any athlete followers. If the indicated athlete has blocked
%% the authenticated athlete, the result will be an empty array.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava_auth:token(), integer()) -> [t()].

followers(Token, Id) ->
    followers_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List any athlete followers. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava_auth:token(), integer(), pos_integer(), pos_integer()) -> [t()].

followers(Token, Id, Page, PerPage) ->
    followers_args(Token, Id, _Args = #{page     => Page,
                                        per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List both following. Retrieve the athletes who both the
%% authenticated user and the indicated athlete are following.
%%
%% @see both_following/4
%% @end
%%--------------------------------------------------------------------
-spec both_following(strava_auth:token(), integer()) -> [t()].

both_following(Token, Id) ->
    both_following_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List both following. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec both_following(strava_auth:token(), integer(), pos_integer(), pos_integer()) -> [t()].

both_following(Token, Id, Page, PerPage) ->
    both_following_args(Token, Id, _Args = #{page     => Page,
                                             per_page => PerPage}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List athlete K/QOMs/CRs.
%% @end
%%--------------------------------------------------------------------
-spec koms_args(strava_auth:token(), integer(), map()) -> [strava_segment_effort:t()].

koms_args(Token, Id, Args) ->
    case strava_api:read(Token, [<<"athletes">>, Id, <<"koms">>], Args) of
        {ok, JSON} -> lists:map(fun strava_repr:to_segment_effort/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends_args(strava_auth:token(), integer(), map()) -> [t()].

friends_args(Token, Id, Args) ->
    Path = case Id of
               undefined -> [<<"athlete">>, <<"friends">>];
               _SomeId -> [<<"athletes">>, Id, <<"friends">>]
           end,
    case strava_api:read(Token, Path, Args) of
        {ok, JSON} -> lists:map(fun strava_repr:to_athlete/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers_args(strava_auth:token(), integer(), map()) -> [t()].

followers_args(Token, Id, Args) ->
    Path = case Id of
               undefined -> [<<"athlete">>, <<"followers">>];
               _SomeId -> [<<"athletes">>, Id, <<"followers">>]
           end,
    case strava_api:read(Token, Path, Args) of
        {ok, JSON} -> lists:map(fun strava_repr:to_athlete/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List both following.
%% @end
%%--------------------------------------------------------------------
-spec both_following_args(strava_auth:token(), integer(), map()) -> [t()].

both_following_args(Token, Id, Args) ->
    Path = [<<"athletes">>, Id, <<"both-following">>],
    case strava_api:read(Token, Path, Args) of
        {ok, JSON} -> lists:map(fun strava_repr:to_athlete/1, JSON)
    end.
