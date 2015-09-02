-module(strava_stream).

%% Types
-export_type([stream/0, t/0, type/0]).

%% Streams functions
-export([activity/3, activity/4, effort/3, effort/4, segment/3,
         segment/4]).

%%%===================================================================
%%% Types
%%%===================================================================

-type stream() :: map().
-type type() :: altitude
              | cadence
              | distance
              | grade_smooth
              | heartrate
              | latlng
              | moving
              | temp
              | time
              | velocity_smooth
              | watts.

-type t() :: stream().

%%%===================================================================
%%% Streams functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve activity streams.
%% @end
%%--------------------------------------------------------------------
-spec activity(strava_auth:token(), integer(), [type()]) -> [t()].

activity(Token, Id, Types) ->
    activity(Token, Id, Types, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve activity streams.
%% @end
%%--------------------------------------------------------------------
-spec activity(strava_auth:token(), integer(), [type()], map()) -> [t()].

activity(_Token, _Id, _Types, _Options) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Retrieve effort streams.
%% @end
%%--------------------------------------------------------------------
-spec effort(strava_auth:token(), integer(), [type()]) -> [t()].

effort(Token, Id, Types) ->
    effort(Token, Id, Types, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve effort streams.
%% @end
%%--------------------------------------------------------------------
-spec effort(strava_auth:token(), integer(), [type()], map()) -> [t()].

effort(_Token, _Id, _Types, _Options) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Retrieve segment streams.
%% @end
%%--------------------------------------------------------------------
-spec segment(strava_auth:token(), integer(), [type()]) -> [t()].

segment(Token, Id, Types) ->
    segment(Token, Id, Types, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve segment streams.
%% @end
%%--------------------------------------------------------------------
-spec segment(strava_auth:token(), integer(), [type()], map()) -> [t()].

segment(_Token, _Id, _Types, _Options) ->
    %% TODO
    [].
