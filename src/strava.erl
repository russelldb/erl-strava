-module(strava).

%% Types
-export_type([position/0, resource_state/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type position() :: {number(), number()}.
-type resource_state() :: meta | summary | detailed.
