%%%-------------------------------------------------------------------
%%% @doc
%%% ETag tracking versions of some functions from strava_athlete
%%% module.
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(strava_athlete_etag).

%% Athletes API functions
-export([stats/3]).

%%%===================================================================
%%% Athletes API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% ETag tracking version of totals and stats function.
%%
%% @see strava_athlete:stats/2
%% @end
%%--------------------------------------------------------------------
-spec stats(strava_auth:token(), strava_api:etag() | undefined,
            integer()) ->
                   {ok, strava_api:etag(), strava_athlete:stats() | undefined}
                       | strava:error().

stats(Token, ETag, Id) ->
    strava_api:convert_etag(
      strava_api:read_etag(Token, ETag, [<<"athletes">>, Id, <<"stats">>]),
      fun strava_repr:to_athlete_stats/1
     ).
