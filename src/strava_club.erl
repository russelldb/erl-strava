-module(strava_club).

%% Types
-export_type([announcement/0, club/0, group_event/0, t/0]).

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
-spec activities(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [strava_activity:t()].
-spec activities_before(strava:auth_token(), integer(), integer()) -> [strava_activity:t()].

%%--------------------------------------------------------------------
%% @doc
%% List club announcements.
%% @end
%%--------------------------------------------------------------------
-spec announcements(strava:auth_token(), integer()) -> [announcement()].

%%--------------------------------------------------------------------
%% @doc
%% List athlete clubs.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava:auth_token()) -> [t()].

%%--------------------------------------------------------------------
%% @doc
%% Retrieve a club.
%% @end
%%--------------------------------------------------------------------
-spec club(strava:auth_token(), integer()) -> t().

%%--------------------------------------------------------------------
%% @doc
%% List club group events.
%% @end
%%--------------------------------------------------------------------
-spec group_events(strava:auth_token(), integer()) -> [group_event()].

%%--------------------------------------------------------------------
%% @doc
%% Join a club.
%% @end
%%--------------------------------------------------------------------
-spec join(strava:auth_token(), integer()) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Leave a club.
%% @end
%%--------------------------------------------------------------------
-spec leave(strava:auth_token(), integer()) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% List club members.
%% @end
%%--------------------------------------------------------------------
-spec members(strava:auth_token(), integer()) -> [strava_athlete:t()].
-spec members(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [strava_athlete:t()].
