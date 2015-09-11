%%%-------------------------------------------------------------------
%%% @doc
%%% ETag tracking versions of some functions from strava_activity
%%% module.
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(strava_activity_etag).

%% Activities functions
-export([athletes/4, athletes_after/3, athletes_before/3,
         friends/4, friends_before/3]).

%% Activity comments and kudos functions
-export([comments/5, kudoers/5]).

%%%===================================================================
%%% Activities functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @see strava_activity:athletes/3
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava_auth:token(), strava_api:etag() | undefined,
               pos_integer(), pos_integer()) ->
                      {ok, strava_api:etag(), [strava_activity:t()] | undefined}
                          | strava:error().

athletes(Token, ETag, Page, PerPage) ->
    athletes_opts(Token, ETag, _Opts = #{page     => Page,
                                         per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% @see strava_activity:athletes_after/2
%% @end
%%--------------------------------------------------------------------
-spec athletes_after(strava_auth:token(), strava_api:etag() | undefined,
                     integer()) ->
                            {ok, strava_api:etag(), [strava_activity:t()] | undefined}
                                | strava:error().

athletes_after(Token, ETag, Time) ->
    athletes_opts(Token, ETag, _Opts = #{'after' => Time}).

%%--------------------------------------------------------------------
%% @doc
%% @see strava_activity:athletes_before/2
%% @end
%%--------------------------------------------------------------------
-spec athletes_before(strava_auth:token(), strava_api:etag() | undefined,
                      integer()) ->
                             {ok, strava_api:etag(), [strava_activity:t()] | undefined}
                                 | strava:error().

athletes_before(Token, ETag, Time) ->
    athletes_opts(Token, ETag, _Opts = #{before => Time}).

%%--------------------------------------------------------------------
%% @doc
%% @see strava_activity:friends/3
%% @end
%%--------------------------------------------------------------------
-spec friends(strava_auth:token(), strava_api:etag() | undefined,
              pos_integer(), pos_integer()) ->
                     {ok, strava_api:etag(), [strava_activity:t()] | undefined}
                         | strava:error().

friends(Token, ETag, Page, PerPage) ->
    friends_opts(Token, ETag, _Opts = #{page     => Page,
                                        per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% @see strava_activity:friends_before/2
%% @end
%%--------------------------------------------------------------------
-spec friends_before(strava_auth:token(), strava_api:etag() | undefined,
                     integer()) ->
                            {ok, strava_api:etag(), [strava_activity:t()] | undefined}
                                | strava:error().

friends_before(Token, ETag, Time) ->
    friends_opts(Token, ETag, _Opts = #{before => Time}).

%%%===================================================================
%%% Activity comments and kudos functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @see strava_activity:comments/4
%% @end
%%--------------------------------------------------------------------
-spec comments(strava_auth:token(), strava_api:etag() | undefined,
               integer(), pos_integer(), pos_integer()) ->
                      {ok, strava_api:etag(), [strava_activity:comment()] | undefined}
                          | strava:error().

comments(Token, ETag, Id, Page, PerPage) ->
    comments_opts(Token, ETag, Id, _Opts = #{page     => Page,
                                             per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% @see strava_activity:kudoers/4
%% @end
%%--------------------------------------------------------------------
-spec kudoers(strava_auth:token(), strava_api:etag() | undefined,
              integer(), pos_integer(), pos_integer()) ->
                     {ok, strava_api:etag(), [strava_athlete:t()] | undefined}
                         | strava:error().

kudoers(Token, ETag, Id, Page, PerPage) ->
    kudoers_opts(Token, ETag, Id, _Opts = #{page     => Page,
                                            per_page => PerPage}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec athletes_opts(strava_auth:token(), strava_api:etag() | undefined,
                    map()) ->
                           {ok, strava_api:etag(), [strava_activity:t()] | undefined}
                               | strava:error().

athletes_opts(Token, ETag, Opts) ->
    strava_api:convert_etag(
      strava_api:read_etag(Token, ETag, [<<"athlete">>, <<"activities">>], Opts),
      {list, fun strava_repr:to_activity/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec friends_opts(strava_auth:token(), strava_api:etag() | undefined,
                   map()) ->
                          {ok, strava_api:etag(), [strava_activity:t()] | undefined}
                              | strava:error().

friends_opts(Token, ETag, Opts) ->
    strava_api:convert_etag(
      strava_api:read_etag(Token, ETag, [<<"activities">>, <<"following">>], Opts),
      {list, fun strava_repr:to_activity/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec comments_opts(strava_auth:token(), strava_api:etag() | undefined,
                    integer(), map()) ->
                           {ok, strava_api:etag(), [strava_activity:comment()] | undefined}
                               | strava:error().

comments_opts(Token, ETag, Id, Opts) ->
    strava_api:convert_etag(
      strava_api:read_etag(Token, ETag, [<<"activities">>, Id, <<"comments">>], Opts),
      {list, fun strava_repr:to_activity_comment/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec kudoers_opts(strava_auth:token(), strava_api:etag() | undefined,
                   integer(), map()) ->
                          {ok, strava_api:etag(), [strava_athlete:t()] | undefined}
                              | strava:error().

kudoers_opts(Token, ETag, Id, Opts) ->
    strava_api:convert_etag(
      strava_api:read_etag(Token, ETag, [<<"activities">>, Id, <<"kudos">>], Opts),
      {list, fun strava_repr:to_athlete/1}
     ).
