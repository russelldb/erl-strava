-module(strava_api).

%% Types
-export_type([content/0, options/0, path/0]).

%% API
-export([create/3, delete/2, read/3, update/3]).

%%%===================================================================
%%% Types
%%%===================================================================

-type content() :: map() | [map()].
-type options() :: map().
-type path() :: [atom() | integer() | iodata()].

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% POST application/x-www-form-urlencoded
%% POST multipart/form-data
%% @end
%%--------------------------------------------------------------------
-spec create(strava:auth_token(), path(), content()) -> {ok, content()} | {error, pos_integer()}.

create(_Token, _Path, _Content) ->
    %% TODO
    {error, -1}.

%%--------------------------------------------------------------------
%% @doc
%% DELETE
%% @end
%%--------------------------------------------------------------------
-spec delete(strava:auth_token(), path()) -> ok | {error, pos_integer()}.

delete(_Token, _Path) ->
    %% TODO
    {error, -1}.

%%--------------------------------------------------------------------
%% @doc
%% GET application/json
%% @end
%%--------------------------------------------------------------------
-spec read(strava:auth_token(), path(), options()) -> {ok, content()} | {error, pos_integer()}.

read(_Token, _Path, _Options) ->
    %% TODO
    {error, -1}.

%%--------------------------------------------------------------------
%% @doc
%% PUT application/x-www-form-urlencoded
%% @end
%%--------------------------------------------------------------------
-spec update(strava:auth_token(), path(), content()) -> {ok, content()} | {error, pos_integer()}.

update(_Token, _Path, _Content) ->
    %% TODO
    {error, -1}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
