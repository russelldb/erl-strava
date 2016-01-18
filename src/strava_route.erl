%%%-------------------------------------------------------------------
%%% @doc
%%% Types and functions related to Strava routes.
%%% @reference http://strava.github.io/api/v3/routes/
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(strava_route).

%% Types
-export_type([route/0, sub_type/0, t/0, type/0]).

%% Route API functions
-export([athletes/2, route/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type route() :: map().
-type sub_type() :: road | mtb | cx | trail | mixed.
-type type() :: ride | run.

-type t() :: route().

%%%===================================================================
%%% Route API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Lists a specific athlete's routes. Private routes will only be
%% included if the authenticating user is viewing their own routes and
%% the token has `view_private` permissions.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava_auth:token(), integer()) -> {ok, [t()]} | strava:error().

athletes(Token, AthleteId) ->
    %% TODO
    {ok, []}.

%%--------------------------------------------------------------------
%% @doc
%% Retrieve details about a route. Private routes can only be accessed
%% if owned by the authenticating user and the token has
%% `view_private` permissions.
%% @end
%%--------------------------------------------------------------------
-spec route(strava_auth:token(), integer()) -> {ok, t()} | strava:error().

route(Token, Id) ->
    %% TODO
    {ok, #{}}.
