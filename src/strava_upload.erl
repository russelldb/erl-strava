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
%% @end
%%--------------------------------------------------------------------
-spec file(strava:auth_token(), data_type(), file:filename_all()) -> status().
-spec file(strava:auth_token(), data_type(), file:filename_all(), map()) -> status().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec status(strava:auth_token(), integer()) -> status().
