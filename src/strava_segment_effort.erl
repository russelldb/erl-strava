-module(strava_segment_effort).

%% Types
-export_type([segment_effort/0, t/0]).

%% Segment efforts functions
-export([segment_effort/2]).

%% To/from JSON functions
-export([to_segment_effort/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type segment_effort() :: map().

-type t() :: segment_effort().

%%%===================================================================
%%% Segment efforts functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec segment_effort(strava:auth_token(), integer()) -> t().

segment_effort(_Token, _Id) ->
    %% TODO
    #{}.

%%%===================================================================
%%% To/from JSON functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_segment_effort(map()) -> t().

to_segment_effort(Map) ->
    maps:fold(
      fun({K, V}, Ans)
            when K =:= <<"id">>;
                 K =:= <<"name">>;
                 K =:= <<"elapsed_time">>;
                 K =:= <<"moving_time">>;
                 K =:= <<"distance">>;
                 K =:= <<"start_index">>;
                 K =:= <<"end_index">>;
                 K =:= <<"average_cadence">>;
                 K =:= <<"average_watts">>;
                 K =:= <<"device_watts">>;
                 K =:= <<"average_heartrate">>;
                 K =:= <<"max_heartrate">>;
                 K =:= <<"kom_rank">>;
                 K =:= <<"pr_rank">>;
                 K =:= <<"hidden">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         ({<<"resource_state">>, Int}, Ans) -> Ans#{resource_state => Int}; % TODO
         ({<<"activity">>, Term}, Ans) -> Ans#{activity => strava_activity:to_activity(Term)};
         ({<<"athlete">>, Term}, Ans) -> Ans#{athlete => strava_athlete:to_athlete(Term)};
         ({<<"start_date">>, Str}, Ans) -> Ans#{start_date => Str}; % TODO
         ({<<"start_date_local">>, Str}, Ans) -> Ans#{start_date_local => Str}; % TODO
         ({<<"segment">>, Term}, Ans) -> Ans#{segment => strava_segment:to_segment(Term)};
         ({_K, _V}, Ans) -> Ans
      end, _Ans = #{}, Map).
