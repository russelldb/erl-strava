%%%-------------------------------------------------------------------
%%% @doc
%%% Types and functions related to Strava gear. Gear represents
%%% equipment used during an activity.
%%% @reference http://strava.github.io/api/v3/gear/
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
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
%% Retrieve gear. The requesting athlete must own the gear. At this
%% time it is not possible to view just anyone’s gear type and usage.
%% @end
%%--------------------------------------------------------------------
-spec gear(strava_auth:token(), binary()) -> {ok, t()} | strava:error().

gear(Token, Id) ->
    strava_api:convert(
      strava_api:read(Token, [<<"gear">>, Id]),
      fun strava_repr:to_gear/1
     ).
