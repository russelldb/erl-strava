-module(strava_upload).

%% Types
-export_type([data_type/0, status/0]).

%% Upload functions
-export([file/3, file/4, status/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type data_type() :: fit
                   | fit_gz
                   | gpx
                   | gpx_gz
                   | tcx
                   | tcx_gz.
-type status() :: map().

%%%===================================================================
%%% Upload functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Upload an activity.
%% @end
%%--------------------------------------------------------------------
-spec file(strava:auth_token(), data_type(), file:filename_all()) -> status().

file(Token, DataType, FileName) ->
    file(Token, DataType, FileName, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Upload an activity.
%% @end
%%--------------------------------------------------------------------
-spec file(strava:auth_token(), data_type(), file:filename_all(), map()) -> status().

file(_Token, _DataType, _FileName, _Options) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% Check upload status.
%% @end
%%--------------------------------------------------------------------
-spec status(strava:auth_token(), integer()) -> status().

status(_Token, _Id) ->
    %% TODO
    #{}.
