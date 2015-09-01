-module(strava_gear).

%% Types
-export_type([gear/0, t/0]).

%% Gear functions
-export([gear/2]).

%% To/from JSON functions
-export([to_gear/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type gear() :: map().

-type t() :: gear().

%%%===================================================================
%%% Gear functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve gear.
%% @end
%%--------------------------------------------------------------------
-spec gear(strava:auth_token(), binary()) -> t().

gear(_Token, _Id) ->
    %% TODO
    #{}.

%%%===================================================================
%%% To/from JSON functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_gear(map()) -> t().

to_gear(Map) ->
    maps:fold(
      fun({K, V}, Ans)
         when K =:= <<"id">>;
              K =:= <<"primary">>;
              K =:= <<"name">>;
              K =:= <<"distance">>;
              K =:= <<"brand_name">>;
              K =:= <<"model_name">>;
              K =:= <<"description">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         ({<<"resource_state">>, Int}, Ans) -> Ans#{resource_state => Int}; % TODO
         ({<<"frame_type">>, Int}, Ans) -> Ans#{frame_type => Int}; % TODO
         ({_K, _V}, Ans) -> Ans
      end, _Ans = #{}, Map).
