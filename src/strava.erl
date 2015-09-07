%%%-------------------------------------------------------------------
%%% @doc
%%% Module with common types and application related functions.
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(strava).

%% Types
-export_type([latlng/0, resource_state/0]).

%% API
-export([start/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type latlng() :: {number(), number()}.
-type resource_state() :: meta | summary | detailed.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------

-spec start() -> ok.

start() ->
    case application:ensure_all_started(strava) of
        {ok, _Apps} -> ok
    end,
    case inets:start(httpc, [{profile, strava}]) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.
