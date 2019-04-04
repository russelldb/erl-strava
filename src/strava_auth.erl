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
         deauthorize/1, token/3, token/4, migrate/3]).

%%%===================================================================
%%% Types
%%%===================================================================

-type approval_prompt() :: auto | force.
-type scope() :: public | write | view_private | view_private_write.
-type token() :: binary() | v2_token().
-type v2_token() :: #{version := v2,
                      refresh := binary(),
                      access := binary(),
                      expires_at := pos_integer(),
                      expires_in := pos_integer()
                     }.

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
-spec deauthorize(token()) -> ok | strava:error().

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
        {ok, _ResBody} -> ok;
        {error, ResBody} ->
            strava_repr:to_error(strava_json:decode(ResBody))
    end.

%%--------------------------------------------------------------------
%% @doc
%% Completing the token exchange. If the user accepts the request to
%% share access to their Strava data, Strava will redirect back to
%% redirect_uri with the authorization code. The application must now
%% exchange the temporary authorization code for an access token,
%% using its client ID and client secret.
%% @see token/4
%% @end
%%--------------------------------------------------------------------
-spec token(integer(), binary(), binary()) ->
                   {ok, token(), strava_athlete:t()} | strava:error().

token(ClientId, ClientSecret, Code) ->
    token(ClientId, ClientSecret, Code, _Migrate=false).

%%--------------------------------------------------------------------
%% @doc

%% Completing the token exchange. If the user accepts the request to
%% share access to their Strava data, Strava will redirect back to
%% redirect_uri with the authorization code. The application must now
%% exchange the temporary authorization code for an access token,
%% using its client ID and client secret. IF the token returned by
%% Strava is a ForeverToken, and `Migrate` is `true` then the token
%% will be migrated to the new refresh token/short-lived access tokens

%% @end
%%--------------------------------------------------------------------
-spec token(integer(), binary(), binary(), boolean()) ->
                   {ok, token(), strava_athlete:t()} | strava:error().

token(ClientId, ClientSecret, Code, Migrate) ->
    case strava_http:request(
           _Method = post,
           _Headers = [],
           _URL = url(<<"token">>),
           _Query = #{},
           _ContentType = <<"application/x-www-form-urlencoded">>,
           _Body = strava_http:qs(#{client_id     => ClientId,
                                    client_secret => ClientSecret,
                                    code          => Code,
                                    grant_type    => <<"authorization_code">>})
          )
    of
        {ok, ResBody} ->
            Decoded = strava_json:decode(ResBody),
            Token = token_from_response(Decoded),
            #{<<"athlete">> := Athlete0} = Decoded,
            Athlete = strava_repr:to_athlete(Athlete0),
            case (Migrate andalso v1_token(Token)) of
                false ->
                    {ok, Token, Athlete};
                true ->
                    migrate_new(ClientId, ClientSecret, Token, Athlete)
            end;
        {error, ResBody} ->
            strava_repr:to_error(strava_json:decode(ResBody))
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
-spec migrate(integer(), binary(), binary()) ->
                   {ok, token()} | strava:error().

migrate(ClientId, ClientSecret, ForeverToken) ->
    case strava_http:request(
           _Method = post,
           _Headers = [],
           _URL = url(<<"token">>),
           _Query = #{},
           _ContentType = <<"application/x-www-form-urlencoded">>,
           _Body = strava_http:qs(#{client_id     => ClientId,
                                    client_secret => ClientSecret,
                                    refresh_token => ForeverToken,
                                    grant_type    => <<"refresh_token">>})
          )
    of
        {ok, ResBody} ->
            Decoded = strava_json:decode(ResBody),
            Token = token_from_response(Decoded),
            {ok, Token};
        {error, ResBody} ->
            strava_repr:to_error(strava_json:decode(ResBody))
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
     strava_http:qs(Query, <<"?">>)].

%% decide on the token version type, returning backwards compatible
%% plain binary for v1, or a map for refresh etc for v2
-spec token_from_response(map()) -> token().
token_from_response(TokenMap) ->
    Token =
        case maps:is_key(<<"refresh_token">>, TokenMap) of
            true ->
                %% This is a 'v2' map
            #{<<"refresh_token">> := RefreshToken,
              <<"access_token">> := AccessToken,
              <<"expires_in">> := ExpiresIn,
              <<"expires_at">> := ExpiresAt} = TokenMap,
            #{version => v2,
              refresh => RefreshToken,
              access => AccessToken,
              expires_at => ExpiresAt,
              expires_in => ExpiresIn};
            false ->
                %% this is an old forever token
                #{<<"access_token">> := ForeverToken} = TokenMap,
                ForeverToken
        end,
    Token.

-spec v1_token(token()) -> boolean().
v1_token(Token) when is_binary(Token) ->
    true;
v1_token(Token) when is_map(Token) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% Migrate a newly acquired `ForeverToken' for `Athlete' to a v2
%% refresh token. If migration fails, an error is logegd and the
%% forever token returned
%% @end
%%--------------------------------------------------------------------
migrate_new(ClientId, ClientSecret, ForeverToken, Athlete) ->
    case migrate(ClientId, ClientSecret, ForeverToken) of
        {ok, Token} ->
            {ok, Token, Athlete};
        Error ->
            lager:error("Error migrating new athlete ~p ~p ~p",
                        [Error, ForeverToken, Athlete]),
            {ok, ForeverToken, Athlete}
    end.
