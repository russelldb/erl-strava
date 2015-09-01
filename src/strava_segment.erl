-module(strava_segment).

%% Types
-export_type([leaderboard/0, segment/0, t/0]).

%% Segment functions
-export([efforts/2, efforts/3, efforts/4, efforts/5, explore/3,
         explore/4, leaderboard/2, leaderboard/3, leaderboard/4,
         leaderboard/5, segment/2, starred/1, starred/3]).

%%%===================================================================
%%% Types
%%%===================================================================

-type leaderboard() :: map().
-type segment() :: map().

-type t() :: segment().

%%%===================================================================
%%% Segment functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an array of segment efforts.
%% @end
%%--------------------------------------------------------------------
-spec efforts(strava:auth_token(), integer()) -> [strava_segment_effort:t()].
-spec efforts(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [strava_segment_effort:t()].
-spec efforts(strava:auth_token(), integer(), map()) -> [strava_segment_effort:t()].
-spec efforts(strava:auth_token(), integer(), map(), pos_integer(), pos_integer()) -> [strava_segment_effort:t()].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec explore(strava:auth_token(), strava:geo_point(), strava:geo_point()) -> [t()].
-spec explore(strava:auth_token(), strava:geo_point(), strava:geo_point(), map()) -> [t()].

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava:auth_token(), integer()) -> leaderboard().
-spec leaderboard(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> leaderboard().
-spec leaderboard(strava:auth_token(), integer(), map()) -> leaderboard().
-spec leaderboard(strava:auth_token(), integer(), map(), pos_integer(), pos_integer()) -> leaderboard().

%%--------------------------------------------------------------------
%% @doc
%% Retrieve a segment.
%% @end
%%--------------------------------------------------------------------
-spec segment(strava:auth_token(), integer()) -> t().

%%--------------------------------------------------------------------
%% @doc
%% List starred segments.
%% @end
%%--------------------------------------------------------------------
-spec starred(strava:auth_token()) -> [t()].
-spec starred(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].
