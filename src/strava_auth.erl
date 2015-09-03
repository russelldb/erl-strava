-module(strava_auth).

%% Types
-export_type([approval_prompt/0, scope/0, token/0]).

%% API
-export([authorize_url/2, authorize_url/3, authorize_url/5,
         deauthorize/1, token/3]).

%%%===================================================================
%%% Types
%%%===================================================================

-type approval_prompt() :: auto | force.
-type scope() :: public | write | view_private | view_private_write.
-type token() :: binary().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize_url(integer(), binary()) -> binary().

authorize_url(ClientId, RedirectUri) ->
    authorize_url_opts(ClientId, RedirectUri, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize_url(integer(), binary(), scope()) -> binary().

authorize_url(ClientId, RedirectUri, Scope) ->
    authorize_url_opts(ClientId, RedirectUri,
                       _Options = #{scope => Scope}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize_url(integer(), binary(), scope(), approval_prompt(),
                    binary()) -> binary().

authorize_url(ClientId, RedirectUri, Scope, ApprovalPrompt, State) ->
    authorize_url_opts(ClientId, RedirectUri,
                       _Options = #{scope           => Scope,
                                    approval_prompt => ApprovalPrompt,
                                    state           => State}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec deauthorize(token()) -> ok.

deauthorize(_Token) ->
    %% TODO
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec token(integer(), binary(), binary()) -> {token(), strava_athlete:t()}.

token(_ClientId, _ClientSecret, _Code) ->
    %% TODO
    {<<>>, #{}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize_url_opts(integer(), binary(), map()) -> binary().

authorize_url_opts(_ClientId, _RedirectUri, _Options) ->
    %% TODO
    <<"https://www.strava.com/oauth/authorize">>.
