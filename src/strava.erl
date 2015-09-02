-module(strava).

%% Types
-export_type([latlng/0, resource_state/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type latlng() :: {number(), number()}.
-type resource_state() :: meta | summary | detailed.
