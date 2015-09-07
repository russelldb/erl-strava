%%%-------------------------------------------------------------------
%%% @doc
%%% Types and functions related to Strava authentication process.
%%% @reference http://strava.github.io/api/v3/oauth/
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
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
%% URL to request access on behalf of a user. Redirect the user to
%% Strava's authorization page. No values will be provided for
%% `Scope', `ApprovalPrompt' and `State' parameters.
%% @see authorize_url/3
%% @see authorize_url/5
%% @end
%%--------------------------------------------------------------------
-spec authorize_url(integer(), binary()) -> binary().

authorize_url(ClientId, RedirectUri) ->
    authorize_url_opts(ClientId, RedirectUri, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% URL to request access on behalf of a user. No values will be
%% provided for `ApprovalPrompt' and `State' parameters.
%% @end
%%--------------------------------------------------------------------
-spec authorize_url(integer(), binary(), scope()) -> binary().

authorize_url(ClientId, RedirectUri, Scope) ->
    authorize_url_opts(ClientId, RedirectUri,
                       _Options = #{scope => Scope}).

%%--------------------------------------------------------------------
%% @doc
%% URL to request access on behalf of a user.
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
%% Allows an application to revoke its access to an athlete's
%% data. This will invalidate all access token, including the `Token'.
%% @end
%%--------------------------------------------------------------------
-spec deauthorize(token()) -> ok.

deauthorize(Token) ->
    case strava_http:request(
           _Method = post,
           _Headers = [{<<"Authorization">>, [<<"Bearer ">>, Token]}],
           _URL = url(<<"deauthorize">>),
           _Query = #{},
           _ContentType = <<>>,
           _Body = <<>>
          )
    of
        {ok, _Status, _ResBody} -> ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Completing the token exchange. If the user accepts the request to
%% share access to their Strava data, Strava will redirect back to
%% redirect_uri with the authorization code. The application must now
%% exchange the temporary authorization code for an access token,
%% using its client ID and client secret.
%% @end
%%--------------------------------------------------------------------
-spec token(integer(), binary(), binary()) -> {token(), strava_athlete:t()}.

token(ClientId, ClientSecret, Code) ->
    case strava_http:request(
           _Method = post,
           _Headers = [],
           _URL = url(<<"token">>),
           _Query = #{},
           _ContentType = <<"application/x-www-form-urlencoded">>,
           _Body = strava_http:qs(#{client_id     => ClientId,
                                    client_secret => ClientSecret,
                                    code          => Code})
          )
    of
        {ok, _Status, ResBody} ->
            #{<<"access_token">> := Token, <<"athlete">> := Athlete} =
                jsx:decode(ResBody, [return_maps]),
            {Token, strava_repr:to_athlete(Athlete)}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize_url_opts(integer(), binary(), map()) -> binary().

authorize_url_opts(ClientId, RedirectUri, Options) ->
    Options1 = maps:fold(
                 fun(approval_prompt, Atom, Ans) ->
                         Ans#{approval_prompt =>
                                  case Atom of
                                      auto -> auto;
                                      force -> force
                                  end};
                    (scope, Atom, Ans) ->
                         Ans#{scope =>
                                  case Atom of
                                      public -> public;
                                      write -> write;
                                      view_private -> view_private;
                                      view_private_write -> <<"view_private,write">>
                                  end};
                    (state, Str, Ans) -> Ans#{state => Str};
                    (_K, _V, Ans) -> Ans
                 end, _Ans = #{}, Options),
    Options2 = Options1#{client_id     => ClientId,
                         redirect_uri  => RedirectUri,
                         response_type => code},
    iolist_to_binary(url(<<"authorize">>, Options2)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec url(iodata()) -> strava_http:url().

url(Suffix) ->
    url(Suffix, _Query = #{}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec url(iodata(), strava_http:query()) -> strava_http:url().

url(Suffix, Query) ->
    [<<"https://www.strava.com/oauth/">>, Suffix,
     strava_http:qs(Query, $?)].
