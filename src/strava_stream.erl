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

activity(Token, Id, Types, Options) ->
    streams(Token, [<<"activities">>, Id, <<"streams">>], Types, Options).

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

effort(Token, Id, Types, Options) ->
    streams(Token, [<<"segment_efforts">>, Id, <<"streams">>], Types, Options).

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

segment(Token, Id, Types, Options) ->
    streams(Token, [<<"segments">>, Id, <<"streams">>], Types, Options).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve segment streams.
%% @end
%%--------------------------------------------------------------------
-spec streams(strava_auth:token(), strava_api:path(), [type()], map()) ->
                     [t()].

streams(Token, Path, Types, Options) ->
    Path1 = Path ++
        [string:join(lists:map(fun strava_util:to_string/1, Types), ",")],
    Options1 =
        maps:fold(
          fun(K, V, Ans)
                when K =:= resolution,
                     V =:= low orelse
                     V =:= medium orelse
                     V =:= high ->
                  Ans#{K => V};
             (K, V, Ans)
                when K =:= series_type,
                     V =:= time orelse
                     V =:= distance ->
                  Ans#{K => V};
             (_K, _V, Ans) -> Ans
          end, _Ans = #{}, Options),
    case strava_api:read(Token, Path1, Options1) of
        {ok, JSON} -> lists:map(fun strava_repr:to_stream/1, JSON)
    end.
