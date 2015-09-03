-module(strava_auth).

%% Types
-export_type([approval_prompt/0, scope/0, token/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type approval_prompt() :: auto | force.
-type scope() :: public | write | view_private | view_private_write.
-type token() :: binary().
