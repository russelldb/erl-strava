-module(strava_upload).

%% Types
-export_type([data_type/0, status/0]).

%% Upload functions
-export([file/3, file/4, status/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type data_type() :: fit
                   | gpx
                   | tcx
                   | 'fit.gz'
                   | 'gpx.gz'
                   | 'tcx.gz'.
-type status() :: map().

%%%===================================================================
%%% Upload functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Upload an activity.
%% @end
%%--------------------------------------------------------------------
-spec file(strava_auth:token(), data_type(), file:filename_all()) -> status().

file(Token, DataType, FileName) ->
    file(Token, DataType, FileName, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Upload an activity.
%% @end
%%--------------------------------------------------------------------
-spec file(strava_auth:token(), data_type(), file:filename_all(), map()) -> status().

file(_Token, _DataType, _FileName, _Options) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% Check upload status.
%% @end
%%--------------------------------------------------------------------
-spec status(strava_auth:token(), integer()) -> status().

status(Token, Id) ->
    case strava_api:read(Token, [<<"uploads">>, Id], _Opts = #{}) of
        {ok, JSON} -> strava_json:to_upload_status(JSON)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec content_type(data_type()) -> binary().

content_type(FileType)
  when FileType =:= fit; FileType =:= 'fit.gz' ->
    <<"application/vnd.ant.fit">>;

content_type(FileType)
  when FileType =:= tcx; FileType =:= 'tcx.gz' ->
    <<"application/vnd.garmin.tcx+xml">>;

content_type(FileType)
  when FileType =:= gpx; FileType =:= 'gpx.gz' ->
    <<"application/gpx+xml">>.
