-module(strava_activity).

%% Types
-export_type([activity/0, comment/0, lap/0, photo/0, t/0, zone/0]).

%% Activities functions
-export([activity/2, athletes/1, athletes/3, athletes_after/2,
         athletes_before/2, create/2, delete/2, friends/1, friends/3,
         friends_before/2, laps/2, laps/4, related/2, related/4,
         update/2, zones/2]).

%% Activity comments functions
-export([comments/2, comments/4]).

%% Activity kudos functions
-export([kudoers/2, kudoers/4]).

%% Activity photos functions
-export([photos/2, photos/4]).

%%%===================================================================
%%% Types
%%%===================================================================

-type activity() :: map().
-type comment() :: map().
-type lap() :: map().
-type photo() :: map().
-type zone() :: map().

-type t() :: activity().

%%%===================================================================
%%% Activities functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve an activity.
%% @end
%%--------------------------------------------------------------------
-spec activity(strava:auth_token(), integer()) -> t().

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava:auth_token()) -> [t()].
-spec athletes(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].
-spec athletes_after(strava:auth_token(), integer()) -> [t()].
-spec athletes_before(strava:auth_token(), integer()) -> [t()].

%%--------------------------------------------------------------------
%% @doc
%% Create an activity.
%% @end
%%--------------------------------------------------------------------
-spec create(strava:auth_token(), t()) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Delete an activity.
%% @end
%%--------------------------------------------------------------------
-spec delete(strava:auth_token(), integer()) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token()) -> [t()].
-spec friends(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].
-spec friends_before(strava:auth_token(), integer()) -> [t()].

%%--------------------------------------------------------------------
%% @doc
%% List activity laps.
%% @end
%%--------------------------------------------------------------------
-spec laps(strava:auth_token(), integer()) -> [lap()].
-spec laps(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [lap()].

%%--------------------------------------------------------------------
%% @doc
%% List related activities.
%% @end
%%--------------------------------------------------------------------
-spec related(strava:auth_token(), integer()) -> [t()].
-spec related(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].

%%--------------------------------------------------------------------
%% @doc
%% Update an activity.
%% @end
%%--------------------------------------------------------------------
-spec update(strava:auth_token(), t()) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% List activity zones.
%% @end
%%--------------------------------------------------------------------
-spec zones(strava:auth_token(), integer()) -> [zone()].

%%%===================================================================
%%% Activity comments functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List activity comments.
%% @end
%%--------------------------------------------------------------------
-spec comments(strava:auth_token(), integer()) -> [comment()].
-spec comments(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [comment()].

%%%===================================================================
%%% Activity kudos functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List activity kudoers.
%% @end
%%--------------------------------------------------------------------
-spec kudoers(strava:auth_token(), integer()) -> [strava_athlete:t()].
-spec kudoers(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [strava_athlete:t()].

%%%===================================================================
%%% Activity photos functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List photos.
%% @end
%%--------------------------------------------------------------------
-spec photos(strava:auth_token(), integer()) -> [photo()].
-spec photos(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [photo()].
