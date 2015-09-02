-module(strava_segment_effort).

%% Types
-export_type([segment_effort/0, t/0]).

%% Segment efforts functions
-export([segment_effort/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type segment_effort() :: map().

-type t() :: segment_effort().

%%%===================================================================
%%% Segment efforts functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec segment_effort(strava_auth:token(), integer()) -> t().

segment_effort(Token, Id) ->
    case strava_api:read(Token, [<<"segment_efforts">>, Id], _Args = #{}) of
        {ok, JSON} -> strava_json:to_segment_effort(JSON)
    end.
