%%%-------------------------------------------------------------------
%%% @doc
%%% Types and functions related to Strava authentication tokens.
%%% @reference http://strava.github.io/api/v3/oauth/
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(strava_token).

-export([
         token_from_response/1,
         refresh_token_from_response/1,
         is_forever_token/1,
         get_refresh_token/1,
         get_access_token/1,
         get_access_expires_in/1,
         get_access_expires_at/1,
         needs_refresh/1
        ]).

-opaque token() ::  forever_token() | refresh_token().
-opaque forever_token() :: v1_token() | legacy_token().
-opaque refresh_token() :: v2_token().
-type legacy_token() :: binary().
-type v1_token() :: #{version := v1,
                      forever_token := binary()
                     }.

-type v2_token() :: #{version := v2,
                      refresh := binary(),
                      access := binary(),
                      expires_at := pos_integer(),
                      expires_in := pos_integer()
                     }.
-type t() :: token().

-export_type([t/0, token/0, forever_token/0, refresh_token/0]).


-spec refresh_token_from_response(map()) -> refresh_token().
refresh_token_from_response(TokenMap) ->
    %% v2 or crash
    #{<<"refresh_token">> := RefreshToken,
      <<"access_token">> := AccessToken,
      <<"expires_in">> := ExpiresIn,
      <<"expires_at">> := ExpiresAt} = TokenMap,
    #{version => v2,
      refresh => RefreshToken,
      access => AccessToken,
      expires_at => ExpiresAt,
      expires_in => ExpiresIn}.

%% decide on the token version type, returning v1 forever token, or a
%% map for refresh etc for v2
-spec token_from_response(map()) -> token().
token_from_response(TokenMap) ->
    Token =
        case maps:is_key(<<"refresh_token">>, TokenMap) of
            true ->
                %% This is a 'v2' map
                refresh_token_from_response(TokenMap);
            false ->
                %% this is an old forever token
                #{<<"access_token">> := ForeverToken} = TokenMap,
                #{version => v1,
                  forever_token => ForeverToken}
        end,
    Token.

-spec is_forever_token(token()) -> boolean().
is_forever_token(Token) when is_binary(Token) ->
    true;
is_forever_token(#{version := v1})  ->
    true;
is_forever_token(_) ->
    false.

-spec get_refresh_token(token()) -> binary().
get_refresh_token(Token) when is_binary(Token) ->
    %% assume a legacy forever token
    Token;
get_refresh_token(#{version := v1, forever_token := Token}) ->
    Token;
get_refresh_token(#{version := v2, refresh := Token}) ->
    Token.

-spec get_access_token(token()) -> binary().
get_access_token(Token) when is_binary(Token) ->
    %% assume a legacy forever token
    Token;
get_access_token(#{version := v1, forever_token := Token}) ->
    Token;
get_access_token(#{version := v2, access := Token}) ->
    Token.

-spec get_access_expires_in(token()) -> pos_integer().
get_access_expires_in(Token) when is_binary(Token) ->
    %% assume a legacy forever token
    0;
get_access_expires_in(#{version := v1}) ->
    0;
get_access_expires_in(#{version := v2, expires_in := Seconds}) ->
    Seconds.

-spec get_access_expires_at(token()) -> pos_integer().
get_access_expires_at(Token) when is_binary(Token) ->
    %% assume a legacy forever token
    os:system_time(seconds);
get_access_expires_at(#{version := v1}) ->
    os:system_time(seconds);
get_access_expires_at(#{version := v2, expires_at := TS}) ->
    TS.

%% @doc same as get_access_expires_at - now < 3600
-spec needs_refresh(token()) -> boolean().
needs_refresh(#{version := v1}) ->
    %% NOTE: refresh == migrate for forever token, and we should
    %% migrate all remaining forever tokens
    true;
needs_refresh(#{version := v2, expires_at := TS}) ->
    %% 3600 == an hour in seconds
    (TS - os:system_time(seconds)) < 3600.

