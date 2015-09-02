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
%% Retrieve activity streams.
%% @end
%%--------------------------------------------------------------------
-spec activity(strava_auth:token(), integer(), [stream_type()]) -> [t()].

activity(Token, Id, Types) ->
    activity(Token, Id, Types, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve activity streams.
%% @end
%%--------------------------------------------------------------------
-spec activity(strava_auth:token(), integer(), [stream_type()], map()) -> [t()].

activity(_Token, _Id, _Types, _Options) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Retrieve effort streams.
%% @end
%%--------------------------------------------------------------------
-spec effort(strava_auth:token(), integer(), [stream_type()]) -> [t()].

effort(Token, Id, Types) ->
    effort(Token, Id, Types, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve effort streams.
%% @end
%%--------------------------------------------------------------------
-spec effort(strava_auth:token(), integer(), [stream_type()], map()) -> [t()].

effort(_Token, _Id, _Types, _Options) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Retrieve segment streams.
%% @end
%%--------------------------------------------------------------------
-spec segment(strava_auth:token(), integer(), [stream_type()]) -> [t()].

segment(Token, Id, Types) ->
    segment(Token, Id, Types, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve segment streams.
%% @end
%%--------------------------------------------------------------------
-spec segment(strava_auth:token(), integer(), [stream_type()], map()) -> [t()].

segment(_Token, _Id, _Types, _Options) ->
    %% TODO
    [].
