-module(strava_athlete).

%% Types
-export_type([athlete/0, stats/0, t/0]).

%% Athletes API functions
-export([athlete/1, athlete/2, koms/2, koms/4, stats/2,
         update/2]).

%% Friends and followers API functions
-export([followers/1, followers/2, followers/3, followers/4,
         friends/1, friends/2, friends/3, friends/4]).

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

%%--------------------------------------------------------------------
%% @doc
%% Retrieve another athlete.
%% @end
%%--------------------------------------------------------------------
-spec athlete(strava:auth_token(), integer()) -> t().

%%--------------------------------------------------------------------
%% @doc
%% List athlete K/QOMs/CRs.
%% @end
%%--------------------------------------------------------------------
-spec koms(strava:auth_token(), integer()) -> map().
-spec koms(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> map().

%%--------------------------------------------------------------------
%% @doc
%% Totals and stats.
%% @end
%%--------------------------------------------------------------------
-spec stats(strava:auth_token(), integer()) -> stats().

%%--------------------------------------------------------------------
%% @doc
%% Update current athlete.
%% @end
%%--------------------------------------------------------------------
-spec update(strava:auth_token(), t()) -> ok.

%%%===================================================================
%%% Friends and followers API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List current athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token()) -> [t()].
-spec friends(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

%%--------------------------------------------------------------------
%% @doc
%% List current athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava:auth_token()) -> [t()].
-spec followers(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

%%--------------------------------------------------------------------
%% @doc
%% List athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token(), integer()) -> [t()].
-spec friends(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].

%%--------------------------------------------------------------------
%% @doc
%% List athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava:auth_token(), integer()) -> [t()].
-spec followers(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].

%%--------------------------------------------------------------------
%% @doc
%% List both following.
%% @end
%%--------------------------------------------------------------------
-spec both_following(strava:auth_token(), integer()) -> [t()].
-spec both_following(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].
