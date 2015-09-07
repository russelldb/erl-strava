-module(strava_gear).

%% Types
-export_type([frame_type/0, gear/0, t/0]).

%% Gear functions
-export([gear/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type frame_type() :: mtb | cross | road | time_trial.
-type gear() :: map().

-type t() :: gear().

%%%===================================================================
%%% Gear functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve gear.
%% @end
%%--------------------------------------------------------------------
-spec gear(strava_auth:token(), binary()) -> t().

gear(Token, Id) ->
    case strava_api:read(Token, [<<"gear">>, Id]) of
        {ok, JSON} -> strava_repr:to_gear(JSON)
    end.
