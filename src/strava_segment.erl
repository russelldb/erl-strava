%%%-------------------------------------------------------------------
%%% @doc
%%% Types and functions related to Strava segments.
%%% @reference http://strava.github.io/api/v3/segments/
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(strava_segment).

%% Types
-export_type([activity_type/0, climb_category/0, leaderboard/0,
              segment/0, t/0]).

%% Segment functions
-export([efforts/2, efforts/3, efforts/4, efforts/5, explore/3,
         explore/4, leaderboard/2, leaderboard/3, leaderboard/4,
         leaderboard/5, segment/2, starred/1, starred/3]).

%%%===================================================================
%%% Types
%%%===================================================================

-type activity_type() :: ride | run.
-type climb_category() :: undefined | 4 | 3 | 2 | 1 | hc.
-type leaderboard() :: map().
-type segment() :: map().

-type t() :: segment().

%%%===================================================================
%%% Segment functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an array of segment efforts. All efforts for the segment
%% will be returned.
%%
%% @see efforts/3
%% @see efforts/4
%% @see efforts/5
%% @end
%%--------------------------------------------------------------------
-spec efforts(strava_auth:token(), integer()) ->
                     {ok, [strava_segment_effort:t()]} | strava:error().

efforts(Token, Id) ->
    efforts_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an array of segment efforts. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec efforts(strava_auth:token(), integer(), pos_integer(), pos_integer()) ->
                     {ok, [strava_segment_effort:t()]} | strava:error().

efforts(Token, Id, Page, PerPage) ->
    efforts_args(Token, Id, _Args = #{page     => Page,
                                      per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve a filtered array of segment efforts. For filter options
%% see Strava documentation.
%% @end
%%--------------------------------------------------------------------
-spec efforts(strava_auth:token(), integer(), map()) ->
                     {ok, [strava_segment_effort:t()]} | strava:error().

efforts(Token, Id, Filter) ->
    efforts_args(Token, Id, _Args = Filter).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve a filtered array of segment efforts. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec efforts(strava_auth:token(), integer(), map(), pos_integer(), pos_integer()) ->
                     {ok, [strava_segment_effort:t()]} | strava:error().

efforts(Token, Id, Filter, Page, PerPage) ->
    efforts_args(Token, Id, _Args = Filter#{page     => Page,
                                            per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Segment explorer. Find popular segments within a given
%% area. Returns an array of up to 10 segment objects.
%%
%% @see explore/4
%% @end
%%--------------------------------------------------------------------
-spec explore(strava_auth:token(), strava:latlng(), strava:latlng()) ->
                     {ok, [t()]} | strava:error().

explore(Token, SW, NE) ->
    explore(Token, SW, NE, _Filter = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Segment explorer. For filter options see Strava documentation.
%% @end
%%--------------------------------------------------------------------
-spec explore(strava_auth:token(), strava:latlng(), strava:latlng(), map()) ->
                     {ok, [t()]} | strava:error().

explore(Token, {SWLat, SWLon}, {NELat, NELon}, Filter) ->
    Args0 = maps:fold(
              fun(K, V, Ans)
                    when K =:= min_cat;
                         K =:= max_cat ->
                      Ans#{K => strava_repr:from_segment_climb_category(V)};
                 (activity_type, ride, Ans) -> Ans#{activity_type => <<"riding">>};
                 (activity_type, run, Ans) -> Ans#{activity_type => <<"running">>};
                 (_K, _V, Ans) -> Ans
              end, _Ans = #{}, Filter),
    Args1 = Args0#{bounds => iolist_to_binary(
                               io_lib:format("~f,~f,~f,~f",
                                             [SWLat, SWLon, NELat, NELon])
                              )},
    strava_api:convert(
      strava_api:read(Token, [<<"segments">>, <<"explore">>], Args1),
      {list, fun strava_repr:to_segment/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards. Leaderboards represent the ranking of
%% athletes on specific segments.
%%
%% @see leaderboard/3
%% @see leaderboard/4
%% @see leaderboard/5
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava_auth:token(), integer()) ->
                         {ok, leaderboard()} | strava:error().

leaderboard(Token, Id) ->
    leaderboard_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava_auth:token(), integer(), pos_integer(), pos_integer()) ->
                         {ok, leaderboard()} | strava:error().

leaderboard(Token, Id, Page, PerPage) ->
    leaderboard_args(Token, Id, _Args = #{page =>     Page,
                                          per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Filtered segment leaderboards. For filter options see Strava
%% documentation.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava_auth:token(), integer(), map()) ->
                         {ok, leaderboard()} | strava:error().

leaderboard(Token, Id, Filter) ->
    leaderboard_args(Token, Id, _Args = Filter).

%%--------------------------------------------------------------------
%% @doc
%% Filtered segment leaderboards. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava_auth:token(), integer(), map(), pos_integer(), pos_integer()) ->
                         {ok, leaderboard()} | strava:error().

leaderboard(Token, Id, Filter, Page, PerPage) ->
    leaderboard_args(Token, Id, _Args = Filter#{page     => Page,
                                                per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve a segment. Returns a detailed representation of the
%% segment.
%% @end
%%--------------------------------------------------------------------
-spec segment(strava_auth:token(), integer()) ->
                     {ok, t()} | strava:error().

segment(Token, Id) ->
    strava_api:convert(
      strava_api:read(Token, [<<"segments">>, Id]),
      fun strava_repr:to_segment/1
     ).

%%--------------------------------------------------------------------
%% @doc
%% List starred segments. Returns segments starred by the
%% authenticated user.
%% @end
%%--------------------------------------------------------------------
-spec starred(strava_auth:token()) ->
                     {ok, [t()]} | strava:error().

starred(Token) ->
    starred_args(Token, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List starred segments. With pagination.
%% @end
%%--------------------------------------------------------------------
-spec starred(strava_auth:token(), pos_integer(), pos_integer()) ->
                     {ok, [t()]} | strava:error().

starred(Token, Page, PerPage) ->
    starred_args(Token, _Args = #{page     => Page,
                                  per_page => PerPage}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an array of segment efforts.
%% @end
%%--------------------------------------------------------------------
-spec efforts_args(strava_auth:token(), integer(), map()) ->
                          {ok, [strava_segment_effort:t()]} | strava:error().

efforts_args(Token, Id, Args) ->
    Args1 = maps:fold(
              fun(K, V, Ans)
                    when K =:= athlete_id;
                         K =:= page;
                         K =:= per_page ->
                      Ans#{K => V};
                 (K, V, Ans)
                    when K =:= start_date_local;
                         K =:= end_date_local ->
                      Ans#{K => strava_repr:from_datetime(V)};
                 (_K, _V, Ans) -> Ans
              end, _Ans = #{}, Args),
    strava_api:convert(
      strava_api:read(Token, [<<"segments">>, Id, <<"all_efforts">>], Args1),
      {list, fun strava_repr:to_segment_effort/1}
     ).

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard_args(strava_auth:token(), integer(), map()) ->
                              {ok, leaderboard()} | strava:error().

leaderboard_args(Token, Id, Args) ->
    Args1 = maps:fold(
              fun(K, V, Ans)
                    when K =:= page;
                         K =:= per_page;
                         K =:= context_entries;
                         K =:= club_id;
                         K =:= following ->
                      Ans#{K => V};
                 (gender, Term, Ans) -> Ans#{gender => strava_repr:from_athlete_sex(Term)};
                 (age_group, Term, Ans) -> Ans#{age_group => Term}; % TODO?
                 (weight_class, Term, Ans) -> Ans#{weight_class => Term}; % TODO?
                 (_K, _V, Ans) -> Ans
              end, _Ans = #{}, Args),
    strava_api:convert(
      strava_api:read(Token, [<<"segments">>, Id, <<"leaderboard">>], Args1),
      fun strava_repr:to_segment_leaderboard/1
     ).

%%--------------------------------------------------------------------
%% @doc
%% List starred segments.
%% @end
%%--------------------------------------------------------------------
-spec starred_args(strava_auth:token(), map()) ->
                          {ok, [t()]} | strava:error().

starred_args(Token, Args) ->
    Args1 = maps:filter(fun(K, _V) ->
                                K =:= page orelse K =:= per_page
                        end, Args),
    strava_api:convert(
      strava_api:read(Token, [<<"segments">>, <<"starred">>], Args1),
      {list, fun strava_repr:to_segment/1}
     ).
