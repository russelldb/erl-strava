%%%-------------------------------------------------------------------
%%% @doc
%%% Types and functions related to Strava segment efforts. A segment
%%% effort represents an athlete’s attempt at a segment. It can also
%%% be thought of as a portion of a ride that covers a segment.
%%% @reference http://strava.github.io/api/v3/efforts/
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
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
%% Retrieve a segment effort. The effort must be public or it must
%% correspond to the current athlete.
%% @end
%%--------------------------------------------------------------------
-spec segment_effort(strava_auth:token(), integer()) ->
                            {ok, t()} | strava:error().

segment_effort(Token, Id) ->
    strava_api:convert(
      strava_api:read(Token, [<<"segment_efforts">>, Id]),
      fun strava_repr:to_segment_effort/1
     ).
