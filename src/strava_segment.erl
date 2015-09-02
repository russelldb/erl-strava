-module(strava_segment).

%% Types
-export_type([leaderboard/0, segment/0, t/0]).

%% Segment functions
-export([efforts/2, efforts/3, efforts/4, efforts/5, explore/3,
         explore/4, leaderboard/2, leaderboard/3, leaderboard/4,
         leaderboard/5, segment/2, starred/1, starred/3]).

%%%===================================================================
%%% Types
%%%===================================================================

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
-spec explore(strava_auth:token(), strava:position(), strava:position()) -> [t()].

explore(Token, SW, NE) ->
    explore(Token, SW, NE, _Filter = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Segment explorer.
%% @end
%%--------------------------------------------------------------------
-spec explore(strava_auth:token(), strava:position(), strava:position(), map()) -> [t()].

explore(_Token, _SW, _NE, _Filter) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava_auth:token(), integer()) -> leaderboard().

leaderboard(Token, Id) ->
    leaderboard(Token, Id, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava_auth:token(), integer(), pos_integer(), pos_integer()) -> leaderboard().

leaderboard(Token, Id, Page, PerPage) ->
    leaderboard(Token, Id, _Filter = #{}, Page, PerPage).

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava_auth:token(), integer(), map()) -> leaderboard().

leaderboard(Token, Id, Filter) ->
    leaderboard(Token, Id, Filter, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava_auth:token(), integer(), map(), pos_integer(), pos_integer()) -> leaderboard().

leaderboard(_Token, _Id, _Filter, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Retrieve a segment.
%% @end
%%--------------------------------------------------------------------
-spec segment(strava_auth:token(), integer()) -> t().

segment(_Token, _Id) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% List starred segments.
%% @end
%%--------------------------------------------------------------------
-spec starred(strava_auth:token()) -> [t()].

starred(Token) ->
    starred(Token, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List starred segments.
%% @end
%%--------------------------------------------------------------------
-spec starred(strava_auth:token(), pos_integer(), pos_integer()) -> [t()].

starred(_Token, _Page, _PerPage) ->
    %% TODO
    [].

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
                    when K =:= id;
                         K =:= athlete_id;
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
