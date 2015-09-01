-module(strava_club).

%% Types
-export_type([announcement/0, club/0, group_event/0, sport_type/0,
              t/0, type/0]).

%% Clubs functions
-export([activities/2, activities/4, activities_before/3,
         announcements/2, athletes/1, club/2, group_events/2, join/2,
         leave/2, members/2, members/4]).

%%%===================================================================
%%% Types
%%%===================================================================

-type announcement() :: map().
-type club() :: map().
-type group_event() :: map().
-type sport_type() :: cycling
                    | running
                    | triathlon
                    | other.
-type type() :: casual_club
              | racing_team
              | shop
              | company
              | other.

-type t() :: club().

%%%===================================================================
%%% Clubs functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List club activities.
%% @end
%%--------------------------------------------------------------------
-spec activities(strava:auth_token(), integer()) -> [strava_activity:t()].

activities(Token, Id) ->
    activities_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List club activities.
%% @end
%%--------------------------------------------------------------------
-spec activities(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [strava_activity:t()].

activities(Token, Id, Page, PerPage) ->
    activities_args(Token, Id, _Args = #{page     => Page,
                                         per_page => PerPage}).

%%--------------------------------------------------------------------
%% @doc
%% List club activities.
%% @end
%%--------------------------------------------------------------------
-spec activities_before(strava:auth_token(), integer(), integer()) -> [strava_activity:t()].

activities_before(Token, Id, Time) ->
    activities_args(Token, Id, _Args = #{before => Time}).

%%--------------------------------------------------------------------
%% @doc
%% List club announcements.
%% @end
%%--------------------------------------------------------------------
-spec announcements(strava:auth_token(), integer()) -> [announcement()].

announcements(Token, Id) ->
    case strava_api:read(Token, [<<"clubs">>, Id, <<"announcements">>], _Opts = #{}) of
        {ok, JSON} -> lists:map(fun strava_json:to_club_announcement/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List athlete clubs.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava:auth_token()) -> [t()].

athletes(Token) ->
    case strava_api:read(Token, [<<"athlete">>, <<"clubs">>], _Opts = #{}) of
        {ok, JSON} -> lists:map(fun strava_json:to_club/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Retrieve a club.
%% @end
%%--------------------------------------------------------------------
-spec club(strava:auth_token(), integer()) -> t().

club(Token, Id) ->
    case strava_api:read(Token, [<<"clubs">>, Id], _Opts = #{}) of
        {ok, JSON} -> strava_json:to_club(JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List club group events.
%% @end
%%--------------------------------------------------------------------
-spec group_events(strava:auth_token(), integer()) -> [group_event()].

group_events(_Token, _Id) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Join a club.
%% @end
%%--------------------------------------------------------------------
-spec join(strava:auth_token(), integer()) -> ok.

join(Token, Id) ->
    case strava_api:create(Token, [<<"clubs">>, Id, <<"join">>], #{}) of
        {ok, _JSON} -> ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Leave a club.
%% @end
%%--------------------------------------------------------------------
-spec leave(strava:auth_token(), integer()) -> ok.

leave(Token, Id) ->
    case strava_api:create(Token, [<<"clubs">>, Id, <<"leave">>], #{}) of
        {ok, _JSON} -> ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% List club members.
%% @end
%%--------------------------------------------------------------------
-spec members(strava:auth_token(), integer()) -> [strava_athlete:t()].

members(Token, Id) ->
    members_args(Token, Id, _Args = #{}).

%%--------------------------------------------------------------------
%% @doc
%% List club members.
%% @end
%%--------------------------------------------------------------------
-spec members(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [strava_athlete:t()].

members(Token, Id, Page, PerPage) ->
    members_args(Token, Id, _Args = #{page     => Page,
                                      per_page => PerPage}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List club activities.
%% @end
%%--------------------------------------------------------------------
-spec activities_args(strava:auth_token(), integer(), map()) ->
                             [strava_activity:t()].

activities_args(Token, Id, Args) ->
    case strava_api:read(Token, [<<"clubs">>, Id, <<"activities">>], Args) of
        {ok, JSON} -> lists:map(fun strava_json:to_activity/1, JSON)
    end.

%%--------------------------------------------------------------------
%% @doc
%% List club members.
%% @end
%%--------------------------------------------------------------------
-spec members_args(strava:auth_token(), integer(), map()) ->
                          [strava_athlete:t()].

members_args(Token, Id, Args) ->
    case strava_api:read(Token, [<<"clubs">>, Id, <<"members">>], Args) of
        {ok, JSON} -> lists:map(fun strava_json:to_athlete/1, JSON)
    end.
