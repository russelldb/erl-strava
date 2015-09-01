-module(strava).

%% Types
-export_type([auth_token/0, position/0, resource_state/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type auth_token() :: binary().
-type position() :: {number(), number()}.
-type resource_state() :: meta | summary | detailed.
