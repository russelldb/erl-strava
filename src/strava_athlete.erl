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
%% Retrieve current athlete.
%% @end
%%--------------------------------------------------------------------
-spec athlete(strava_auth:token()) -> t().

athlete(Token) ->
    athlete(Token, _Id = undefined).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve another athlete.
%% @end
%%--------------------------------------------------------------------
-spec athlete(strava_auth:token(), integer()) -> t().

athlete(Token, Id) ->
    Path = case Id of
               undefined -> [<<"athlete">>];
               _SomeId -> [<<"athletes">>, Id]
           end,
    case strava_api:read(Token, Path) of
        {ok, JSON} -> strava_json:to_athlete(JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List athlete K/QOMs/CRs.
%% @end
%%--------------------------------------------------------------------
-spec koms(strava_auth:token(), integer()) -> [strava_segment_effort:t()].

koms(Token, Id) ->
    koms_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete K/QOMs/CRs.
%% @end
%%--------------------------------------------------------------------
-spec koms(strava_auth:token(), integer(), pos_integer(), pos_integer()) ->
                  [strava_segment_effort:t()].

koms(Token, Id, Page, PerPage) ->
    koms_args(Token, Id, _Args = #{page     => Page,
                                   per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Totals and stats.
%% @end
%%--------------------------------------------------------------------
-spec stats(strava_auth:token(), integer()) -> stats().

stats(Token, Id) ->
    case strava_api:read(Token, [<<"athletes">>, Id, <<"stats">>]) of
        {ok, JSON} -> strava_json:to_athlete_stats(JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Update current athlete.
%% @end
%%--------------------------------------------------------------------
-spec update(strava_auth:token(), t()) -> t().

update(Token, Athlete) ->
    Content = strava_json:from_athlete(Athlete),
    case strava_api:update(Token, [<<"athlete">>], Content) of
        {ok, JSON} -> strava_json:to_athlete(JSON)
    end.

%%%===================================================================
%%% Friends and followers API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List current athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava_auth:token()) -> [t()].

friends(Token) ->
    friends_args(Token, _Id = undefined, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List current athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava_auth:token(), pos_integer(), pos_integer()) -> [t()].

friends(Token, Page, PerPage) ->
    friends_args(Token, _Id = undefined, _Args = #{page     => Page,
                                                   per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List current athlete followers.
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
%% List athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava_auth:token(), integer()) -> [t()].

friends(Token, Id) ->
    friends_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava_auth:token(), integer(), pos_integer(), pos_integer()) -> [t()].

friends(Token, Id, Page, PerPage) ->
    friends_args(Token, Id, _Args = #{page     => Page,
                                      per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava_auth:token(), integer()) -> [t()].

followers(Token, Id) ->
    followers_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava_auth:token(), integer(), pos_integer(), pos_integer()) -> [t()].

followers(Token, Id, Page, PerPage) ->
    followers_args(Token, Id, _Args = #{page     => Page,
                                        per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List both following.
%% @end
%%--------------------------------------------------------------------
-spec both_following(strava_auth:token(), integer()) -> [t()].

both_following(Token, Id) ->
    both_following_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List both following.
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
        {ok, JSON} -> lists:map(fun strava_json:to_segment_effort/1, JSON)
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
        {ok, JSON} -> lists:map(fun strava_json:to_athlete/1, JSON)
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
        {ok, JSON} -> lists:map(fun strava_json:to_athlete/1, JSON)
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
        {ok, JSON} -> lists:map(fun strava_json:to_athlete/1, JSON)
    end.
