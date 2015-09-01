-module(strava).

%% Types
-export_type([auth_token/0, position/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type auth_token() :: binary().
-type position() :: {number(), number()}.
