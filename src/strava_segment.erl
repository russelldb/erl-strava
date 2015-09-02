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
%% Retrieve an array of segment efforts.
%% @end
%%--------------------------------------------------------------------
-spec efforts(strava_auth:token(), integer()) -> [strava_segment_effort:t()].

efforts(Token, Id) ->
    efforts_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an array of segment efforts.
%% @end
%%--------------------------------------------------------------------
-spec efforts(strava_auth:token(), integer(), pos_integer(), pos_integer()) -> [strava_segment_effort:t()].

efforts(Token, Id, Page, PerPage) ->
    efforts_args(Token, Id, _Args = #{page     => Page,
                                      per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an array of segment efforts.
%% @end
%%--------------------------------------------------------------------
-spec efforts(strava_auth:token(), integer(), map()) -> [strava_segment_effort:t()].

efforts(Token, Id, Filter) ->
    efforts_args(Token, Id, _Args = Filter).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an array of segment efforts.
%% @end
%%--------------------------------------------------------------------
-spec efforts(strava_auth:token(), integer(), map(), pos_integer(), pos_integer()) -> [strava_segment_effort:t()].

efforts(Token, Id, Filter, Page, PerPage) ->
    efforts_args(Token, Id, _Args = Filter#{page     => Page,
                                            per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Segment explorer.
%% @end
%%--------------------------------------------------------------------
-spec explore(strava_auth:token(), strava:latlng(), strava:latlng()) -> [t()].

explore(Token, SW, NE) ->
    explore(Token, SW, NE, _Filter = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Segment explorer.
%% @end
%%--------------------------------------------------------------------
-spec explore(strava_auth:token(), strava:latlng(), strava:latlng(), map()) -> [t()].

explore(Token, {SWLat, SWLon}, {NELat, NELon}, Filter) ->
    Args0 = maps:fold(
              fun(K, V, Ans)
                    when K =:= min_cat;
                         K =:= max_cat ->
                      Ans#{K => strava_json:from_segment_climb_category(V)};
                 (activity_type, ride, Ans) -> Ans#{activity_type => <<"riding">>};
                 (activity_type, run, Ans) -> Ans#{activity_type => <<"running">>};
                 (_K, _V, Ans) -> Ans
              end, _Ans = #{}, Filter),
    Args1 = Args0#{bounds => iolist_to_binary(
                               io_lib:format("~f,~f,~f,~f",
                                             [SWLat, SWLon, NELat, NELon])
                              )},
    case strava_api:read(Token, [<<"segments">>, <<"explore">>], Args1) of
        {ok, JSON} -> lists:map(fun strava_json:to_segment/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava_auth:token(), integer()) -> leaderboard().

leaderboard(Token, Id) ->
    leaderboard_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava_auth:token(), integer(), pos_integer(), pos_integer()) -> leaderboard().

leaderboard(Token, Id, Page, PerPage) ->
    leaderboard_args(Token, Id, _Args = #{page =>     Page,
                                          per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava_auth:token(), integer(), map()) -> leaderboard().

leaderboard(Token, Id, Filter) ->
    leaderboard_args(Token, Id, _Args = Filter).

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava_auth:token(), integer(), map(), pos_integer(), pos_integer()) -> leaderboard().

leaderboard(Token, Id, Filter, Page, PerPage) ->
    leaderboard_args(Token, Id, _Args = Filter#{page     => Page,
                                                per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve a segment.
%% @end
%%--------------------------------------------------------------------
-spec segment(strava_auth:token(), integer()) -> t().

segment(Token, Id) ->
    case strava_api:read(Token, [<<"segments">>, Id], _Args = #{}) of
        {ok, JSON} -> strava_json:to_segment(JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List starred segments.
%% @end
%%--------------------------------------------------------------------
-spec starred(strava_auth:token()) -> [t()].

starred(Token) ->
    starred_args(Token, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List starred segments.
%% @end
%%--------------------------------------------------------------------
-spec starred(strava_auth:token(), pos_integer(), pos_integer()) -> [t()].

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
                          [strava_segment_effort:t()].

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
                      Ans#{K => strava_json:from_datetime(V)};
                 (_K, _V, Ans) -> Ans
              end, _Ans = #{}, Args),
    case strava_api:read(Token, [<<"segments">>, Id, <<"all_efforts">>], Args1) of
        {ok, JSON} -> lists:map(fun strava_json:to_segment_effort/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard_args(strava_auth:token(), integer(), map()) -> leaderboard().

leaderboard_args(Token, Id, Args) ->
    Args1 = maps:fold(
              fun(K, V, Ans)
                    when K =:= page;
                         K =:= per_page;
                         K =:= context_entries;
                         K =:= club_id;
                         K =:= following ->
                      Ans#{K => V};
                 (gender, Term, Ans) -> Ans#{gender => strava_json:from_athlete_sex(Term)};
                 (age_group, Term, Ans) -> Ans#{age_group => Term}; % TODO?
                 (weight_class, Term, Ans) -> Ans#{weight_class => Term}; % TODO?
                 (_K, _V, Ans) -> Ans
              end, _Ans = #{}, Args),
    case strava_api:read(Token, [<<"segments">>, Id, <<"leaderboard">>], Args1) of
        {ok, JSON} -> strava_json:to_segment_leaderboard(JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List starred segments.
%% @end
%%--------------------------------------------------------------------
-spec starred_args(strava_auth:token(), map()) -> [t()].

starred_args(Token, Args) ->
    Args1 = maps:filter(fun(K, _V) ->
                                K =:= page orelse K =:= per_page
                        end, Args),
    case strava_api:read(Token, [<<"segments">>, <<"starred">>], Args1) of
        {ok, JSON} -> lists:map(fun strava_json:to_segment/1, JSON)
    end.
