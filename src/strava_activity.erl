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

activity(_Token, _Id) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava:auth_token()) -> [t()].

athletes(Token) ->
    athletes(Token, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

athletes(_Token, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes_after(strava:auth_token(), integer()) -> [t()].

athletes_after(_Token, _Time) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List athlete activities.
%% @end
%%--------------------------------------------------------------------
-spec athletes_before(strava:auth_token(), integer()) -> [t()].

athletes_before(_Token, _Time) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Create an activity.
%% @end
%%--------------------------------------------------------------------
-spec create(strava:auth_token(), t()) -> t().

create(_Token, _Activity) ->
    %% TODO
    #{}.

%%--------------------------------------------------------------------
%% @doc
%% Delete an activity.
%% @end
%%--------------------------------------------------------------------
-spec delete(strava:auth_token(), integer()) -> ok.

delete(_Token, _Id) ->
    %% TODO
    ok.

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token()) -> [t()].

friends(Token) ->
    friends(Token, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities.
%% @end
%%--------------------------------------------------------------------
-spec friends(strava:auth_token(), pos_integer(), pos_integer()) -> [t()].

friends(_Token, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List friends’ activities.
%% @end
%%--------------------------------------------------------------------
-spec friends_before(strava:auth_token(), integer()) -> [t()].

friends_before(_Token, _Time) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List activity laps.
%% @end
%%--------------------------------------------------------------------
-spec laps(strava:auth_token(), integer()) -> [lap()].

laps(Token, Id) ->
    laps(Token, Id, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List activity laps.
%% @end
%%--------------------------------------------------------------------
-spec laps(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [lap()].

laps(_Token, _Id, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% List related activities.
%% @end
%%--------------------------------------------------------------------
-spec related(strava:auth_token(), integer()) -> [t()].

related(Token, Id) ->
    related(Token, Id, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List related activities.
%% @end
%%--------------------------------------------------------------------
-spec related(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [t()].

related(_Token, _Id, _Page, _PerPage) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% Update an activity.
%% @end
%%--------------------------------------------------------------------
-spec update(strava:auth_token(), t()) -> ok.

update(_Token, _Activity) ->
    %% TODO
    ok.

%%--------------------------------------------------------------------
%% @doc
%% List activity zones.
%% @end
%%--------------------------------------------------------------------
-spec zones(strava:auth_token(), integer()) -> [zone()].

zones(_Token, _Id) ->
    %% TODO
    [].

%%%===================================================================
%%% Activity comments functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List activity comments.
%% @end
%%--------------------------------------------------------------------
-spec comments(strava:auth_token(), integer()) -> [comment()].

comments(Token, Id) ->
    comments(Token, Id, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List activity comments.
%% @end
%%--------------------------------------------------------------------
-spec comments(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [comment()].

comments(_Token, _Id, _Page, _PerPage) ->
    %% TODO
    [].

%%%===================================================================
%%% Activity kudos functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List activity kudoers.
%% @end
%%--------------------------------------------------------------------
-spec kudoers(strava:auth_token(), integer()) -> [strava_athlete:t()].

kudoers(Token, Id) ->
    kudoers(Token, Id, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List activity kudoers.
%% @end
%%--------------------------------------------------------------------
-spec kudoers(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [strava_athlete:t()].

kudoers(_Token, _Id, _Page, _PerPage) ->
    %% TODO
    [].

%%%===================================================================
%%% Activity photos functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List photos.
%% @end
%%--------------------------------------------------------------------
-spec photos(strava:auth_token(), integer()) -> [photo()].

photos(Token, Id) ->
    photos(Token, Id, _Page = undefined, _PerPage = undefined).

%%--------------------------------------------------------------------
%% @doc
%% List photos.
%% @end
%%--------------------------------------------------------------------
-spec photos(strava:auth_token(), integer(), pos_integer(), pos_integer()) -> [photo()].

photos(_Token, _Id, _Page, _PerPage) ->
    %% TODO
    [].
