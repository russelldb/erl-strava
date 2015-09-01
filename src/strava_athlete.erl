-module(strava_athlete).

%% Types
-export_type([athlete/0, stats/0, t/0]).

%% Athletes API functions
-export([athlete/1, athlete/2, koms/2, koms/4, stats/2,
         update/2]).

%% Friends and followers API functions
-export([both_following/2, both_following/4, followers/1, followers/2,
         followers/3, followers/4, friends/1, friends/2, friends/3,
         friends/4]).

%% To/from JSON functions
-export([to_athlete/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type athlete() :: map().
-type stats() :: map().

-type t() :: athlete().

%%%===================================================================
%%% Athletes API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve current athlete.
%% @end
%%--------------------------------------------------------------------
-spec athlete(strava:auth_token()) -> t().

athlete(_Token) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% Retrieve another athlete.
%% @end
%%--------------------------------------------------------------------
-spec athlete(strava:auth_token(), integer()) -> t().

athlete(_Token, _Id) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% List athlete K/QOMs/CRs.
%% @end
%%--------------------------------------------------------------------
-spec koms(strava:auth_token(), integer()) -> [strava_segment_effort:t()].

koms(Token, Id) ->
    koms(Token, Id, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List athlete K/QOMs/CRs.
%% @end
%%--------------------------------------------------------------------
-spec koms(strava:auth_token(), integer(), pos_integer(), pos_integer()) ->
                  [strava_segment_effort:t()].

koms(_Token, _Id, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Totals and stats.
%% @end
%%--------------------------------------------------------------------
-spec stats(strava:auth_token(), integer()) -> stats().

stats(_Token, _Id) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% Update current athlete.
%% @end
%%--------------------------------------------------------------------
-spec update(strava:auth_token(), t()) -> t().

update(_Token, _Athlete) ->
    %% TODO
    #{}.

%%%===================================================================
%%% Friends and followers API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List current athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token()) -> [t()].

friends(Token) ->
    friends(Token, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List current athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

friends(_Token, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List current athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava:auth_token()) -> [t()].

followers(Token) ->
    followers(Token, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List current athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

followers(_Token, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token(), integer()) -> [t()].

friends(Token, Id) ->
    friends(Token, Id, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List athlete friends.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].

friends(_Token, _Id, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava:auth_token(), integer()) -> [t()].

followers(Token, Id) ->
    followers(Token, Id, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List athlete followers.
%% @end
%%--------------------------------------------------------------------
-spec followers(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].

followers(_Token, _Id, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List both following.
%% @end
%%--------------------------------------------------------------------
-spec both_following(strava:auth_token(), integer()) -> [t()].

both_following(Token, Id) ->
    both_following(Token, Id, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List both following.
%% @end
%%--------------------------------------------------------------------
-spec both_following(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].

both_following(_Token, _Id, _Page, _PerPage) ->
    %% TODO
    [].

%%%===================================================================
%%% To/from JSON map functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_athlete(map()) -> t().

to_athlete(Map) ->
    maps:fold(
      fun({K, V}, Ans)
            when K =:= <<"id">>;
                 K =:= <<"firstname">>;
                 K =:= <<"lastname">>;
                 K =:= <<"profile_medium">>;
                 K =:= <<"profile">>;
                 K =:= <<"city">>;
                 K =:= <<"state">>;
                 K =:= <<"country">>;
                 K =:= <<"premium">>;
                 K =:= <<"follower_count">>;
                 K =:= <<"friend_count">>;
                 K =:= <<"mutual_friend_count">>;
                 K =:= <<"date_preference">>;
                 K =:= <<"email">>;
                 K =:= <<"ftp">>;
                 K =:= <<"weight">> ->
              Ans#{binary_to_atom(K, latin1) => V};
         ({<<"resource_state">>, Int}, Ans) -> Ans#{resource_state => Int}; % TODO
         ({<<"sex">>, Str}, Ans) -> Ans#{sex => Str}; % TODO
         ({<<"friend">>, Str}, Ans) -> Ans#{friend => Str}; % TODO
         ({<<"follower">>, Str}, Ans) -> Ans#{follower => Str}; % TODO
         ({<<"created_at">>, Str}, Ans) -> Ans#{created_at => Str}; % TODO
         ({<<"updated_at">>, Str}, Ans) -> Ans#{updated_at => Str}; % TODO
         ({<<"athlete_type">>, Int}, Ans) -> Ans#{athlete_type => Int}; % TODO
         ({<<"measurement_preference">>, Str}, Ans) -> Ans#{measurement_preference => Str}; % TODO
         ({<<"clubs">>, List}, Ans) -> Ans#{clubs => lists:map(fun strava_club:to_club/1, List)};
         ({<<"bikes">>, List}, Ans) -> Ans#{bikes => lists:map(fun strava_gear:to_gear/1, List)};
         ({<<"shoes">>, List}, Ans) -> Ans#{shoes => lists:map(fun strava_gear:to_gear/1, List)};
         ({_K, _V}, Ans) -> Ans
      end, _Ans = #{}, Map).
