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

file(Token, FileType, FileName) ->
    file(Token, FileType, FileName, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Upload an activity.
%% @end
%%--------------------------------------------------------------------
-spec file(strava_auth:token(), data_type(), file:filename_all(), map()) -> status().

file(Token, FileType, FileName, Options) ->
    Options1 =
        maps:fold(
          fun(K, V, Ans)
                when K =:= activity_type;
                     K =:= name;
                     K =:= description;
                     K =:= external_id ->
                  Ans#{K => V};
             (K, V, Ans)
                when K =:= private;
                     K =:= trainer;
                     K =:= commute ->
                  Ans#{K => case V of
                                true -> 1;
                                false -> 0
                            end};
             (_K, _V, Ans) -> Ans
          end, _Ans = #{}, Options),
    Form = [{data_type, FileType},
            case FileType of
                _ when FileType =:= fit;
                       FileType =:= tcx;
                       FileType =:= gpx ->
                    {file, content_type(FileType), FileName};
                _ when FileType =:= 'fit.gz';
                       FileType =:= 'tcx.gz';
                       FileType =:= 'gpx.gz' ->
                    {file, content_type(FileType), <<"gzip">>, FileName}
            end | maps:to_list(Options1)],
    case strava_api:create(Token, [<<"uploads">>], Form) of
        {ok, JSON} -> strava_json:to_upload_status(JSON)
    end.

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
