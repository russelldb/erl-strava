-module(strava_athlete).

%% Types
-export_type([athlete/0, stats/0, t/0]).

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
-type stats() :: map().

-type t() :: athlete().

%%%===================================================================
%%% Athletes API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve current athlete.
%% @end
%%--------------------------------------------------------------------
-spec athlete(strava:auth_token()) -> t().

athlete(_Token) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% Retrieve another athlete.
%% @end
%%--------------------------------------------------------------------
-spec athlete(strava:auth_token(), integer()) -> t().

athlete(_Token, _Id) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% List athlete K/QOMs/CRs.
%% @end
%%--------------------------------------------------------------------
-spec koms(strava:auth_token(), integer()) -> [strava_segment_effort:t()].

koms(Token, Id) ->
    koms_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete K/QOMs/CRs.
%% @end
%%--------------------------------------------------------------------
-spec koms(strava:auth_token(), integer(), pos_integer(), pos_integer()) ->
                  [strava_segment_effort:t()].

koms(Token, Id, Page, PerPage) ->
    koms_args(Token, Id, _Args = #{page     => Page,
                                   per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Totals and stats.
%% @end
%%--------------------------------------------------------------------
-spec stats(strava:auth_token(), integer()) -> stats().

stats(_Token, _Id) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% Update current athlete.
%% @end
%%--------------------------------------------------------------------
-spec update(strava:auth_token(), t()) -> t().

update(_Token, _Athlete) ->
    %% TODO
    #{}.

%%%===================================================================
%%% Friends and followers API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List current athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token()) -> [t()].

friends(Token) ->
    friends_args(Token, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List current athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

friends(Token, Page, PerPage) ->
    friends_args(Token, _Args = #{page     => Page,
                                  per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List current athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava:auth_token()) -> [t()].

followers(Token) ->
    followers_args(Token, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List current athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

followers(Token, Page, PerPage) ->
    followers_args(Token, _Args = #{page     => Page,
                                    per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token(), integer()) -> [t()].

friends(Token, Id) ->
    friends_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].

friends(Token, Id, Page, PerPage) ->
    friends_args(Token, Id, _Args = #{page     => Page,
                                      per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava:auth_token(), integer()) -> [t()].

followers(Token, Id) ->
    followers_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].

followers(Token, Id, Page, PerPage) ->
    followers_args(Token, Id, _Args = #{page     => Page,
                                        per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List both following.
%% @end
%%--------------------------------------------------------------------
-spec both_following(strava:auth_token(), integer()) -> [t()].

both_following(Token, Id) ->
    both_following_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List both following.
%% @end
%%--------------------------------------------------------------------
-spec both_following(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].

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
-spec koms_args(strava:auth_token(), integer(), map()) -> [strava_segment_effort:t()].

koms_args(_Token, _Id, _Args) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List current athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends_args(strava:auth_token(), map()) -> [t()].

friends_args(_Token, _Args) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends_args(strava:auth_token(), integer(), map()) -> [t()].

friends_args(_Token, _Id, _Args) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List current athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers_args(strava:auth_token(), map()) -> [t()].

followers_args(_Token, _Args) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers_args(strava:auth_token(), integer(), map()) -> [t()].

followers_args(_Token, _Id, _Args) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List both following.
%% @end
%%--------------------------------------------------------------------
-spec both_following_args(strava:auth_token(), integer(), map()) -> [t()].

both_following_args(_Token, _Id, _Args) ->
    %% TODO
    [].
