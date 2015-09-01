-module(strava_segment).

%% Types
-export_type([leaderboard/0, segment/0, t/0]).

%% Segment functions
-export([efforts/2, efforts/3, efforts/4, efforts/5, explore/3,
         explore/4, leaderboard/2, leaderboard/3, leaderboard/4,
         leaderboard/5, segment/2, starred/1, starred/3]).

%% To/from JSON functions
-export([to_segment/1]).

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
-spec efforts(strava:auth_token(), integer()) -> [strava_segment_effort:t()].

efforts(Token, Id) ->
    efforts(Token, Id, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an array of segment efforts.
%% @end
%%--------------------------------------------------------------------
-spec efforts(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [strava_segment_effort:t()].

efforts(_Token, _Id, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an array of segment efforts.
%% @end
%%--------------------------------------------------------------------
-spec efforts(strava:auth_token(), integer(), map()) -> [strava_segment_effort:t()].

efforts(Token, Id, Filter) ->
    efforts(Token, Id, Filter, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an array of segment efforts.
%% @end
%%--------------------------------------------------------------------
-spec efforts(strava:auth_token(), integer(), map(), pos_integer(), pos_integer()) -> [strava_segment_effort:t()].

efforts(_Token, _Id, _Filter, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Segment explorer.
%% @end
%%--------------------------------------------------------------------
-spec explore(strava:auth_token(), strava:geo_point(), strava:geo_point()) -> [t()].

explore(Token, SW, NE) ->
    explore(Token, SW, NE, _Filter = #{}).

%%--------------------------------------------------------------------
%% @doc
%% Segment explorer.
%% @end
%%--------------------------------------------------------------------
-spec explore(strava:auth_token(), strava:geo_point(), strava:geo_point(), map()) -> [t()].

explore(_Token, _SW, _NE, _Filter) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava:auth_token(), integer()) -> leaderboard().

leaderboard(Token, Id) ->
    leaderboard(Token, Id, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> leaderboard().

leaderboard(Token, Id, Page, PerPage) ->
    leaderboard(Token, Id, _Filter = #{}, Page, PerPage).

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava:auth_token(), integer(), map()) -> leaderboard().

leaderboard(Token, Id, Filter) ->
    leaderboard(Token, Id, Filter, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% Segment leaderboards.
%% @end
%%--------------------------------------------------------------------
-spec leaderboard(strava:auth_token(), integer(), map(), pos_integer(), pos_integer()) -> leaderboard().

leaderboard(_Token, _Id, _Filter, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Retrieve a segment.
%% @end
%%--------------------------------------------------------------------
-spec segment(strava:auth_token(), integer()) -> t().

segment(_Token, _Id) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% List starred segments.
%% @end
%%--------------------------------------------------------------------
-spec starred(strava:auth_token()) -> [t()].

starred(Token) ->
    starred(Token, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List starred segments.
%% @end
%%--------------------------------------------------------------------
-spec starred(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

starred(_Token, _Page, _PerPage) ->
    %% TODO
    [].

%%%===================================================================
%%% To/from JSON functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_segment(map()) -> t().

to_segment(Map) ->
    maps:fold(
      fun({K, V}, Ans)
            when K =:= <<"id">>;
                 K =:= <<"name">>;
                 K =:= <<"distance">>;
                 K =:= <<"average_grade">>;
                 K =:= <<"maximum_grade">>;
                 K =:= <<"elevation_high">>;
                 K =:= <<"elevation_low">>;
                 K =:= <<"city">>;
                 K =:= <<"state">>;
                 K =:= <<"country">>;
                 K =:= <<"private">>;
                 K =:= <<"starred">>;
                 K =:= <<"total_elevation_gain">>;
                 K =:= <<"effort_count">>;
                 K =:= <<"athlete_count">>;
                 K =:= <<"hazardous">>;
                 K =:= <<"star_count">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         ({<<"resource_state">>, Int}, Ans) -> Ans#{resource_state => Int}; % TODO
         ({<<"activity_type">>, Str}, Ans) -> Ans#{activity_type => Str}; % TODO
         ({<<"start_latlng">>, List}, Ans) -> Ans#{start_latlng => List}; % TODO
         ({<<"end_latlng">>, List}, Ans) -> Ans#{end_latlng => List}; % TODO
         ({<<"climb_category">>, Int}, Ans) -> Ans#{climb_category => Int}; % TODO
         ({<<"created_at">>, Str}, Ans) -> Ans#{created_at => Str}; % TODO
         ({<<"updated_at">>, Str}, Ans) -> Ans#{updated_at => Str}; % TODO
         ({<<"map">>, Term}, Ans) -> Ans#{map => Term};             % TODO
         ({_K, _V}, Ans) -> Ans
      end, _Ans = #{}, Map).
