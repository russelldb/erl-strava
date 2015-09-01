-module(strava_stream).

%% Types
-export_type([stream/0, stream_type/0, t/0]).

%% Streams functions
-export([activity/3, activity/4, effort/3, effort/4, segment/3,
         segment/4]).

%%%===================================================================
%%% Types
%%%===================================================================

-type stream() :: map().
-type stream_type() :: altitude
                     | cadence
                     | distance
                     | grade
                     | heart_rate
                     | moving
                     | position
                     | power
                     | temperature
                     | time
                     | velocity.

-type t() :: stream().

%%%===================================================================
%%% Streams functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec activity(strava:auth_token(), integer(), [stream_type()]) -> [t()].
-spec activity(strava:auth_token(), integer(), [stream_type()], map()) -> [t()].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec effort(strava:auth_token(), integer(), [stream_type()]) -> [t()].
-spec effort(strava:auth_token(), integer(), [stream_type()], map()) -> [t()].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec segment(strava:auth_token(), integer(), [stream_type()]) -> [t()].
-spec segment(strava:auth_token(), integer(), [stream_type()], map()) -> [t()].
