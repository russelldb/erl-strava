-module(strava_api).

%% Types
-export_type([path/0]).

%% API
-export([create/3, delete/2, read/2, read/3, update/3]).

%%%===================================================================
%%% Types
%%%===================================================================

-type path() :: [atom() | integer() | iodata()].

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% POST application/x-www-form-urlencoded
%% POST multipart/form-data
%% @end
%%--------------------------------------------------------------------
-spec create(strava_auth:token(), path(),
             strava_http:query() | strava_multipart:t()) ->
                    {ok, map()} | {error, map()}.

create(Token, Path, Content) ->
    {ContentType, Body} =
        case Content of
            Query when is_map(Query) ->
                {"application/x-www-form-urlencoded", strava_http:qs(Query)};
            Form when is_list(Form) ->
                strava_multipart:form_data(Form)
        end,
    {Status, _ResHeaders, ResBody} =
        strava_http:request(
          _Method = post,
          _Headers = headers(Token),
          _URL = url(Path),
          _Options = #{},
          ContentType,
          Body
         ),
    {strava_http:status_atom(Status), jsx:decode(ResBody, [return_maps])}.

%%--------------------------------------------------------------------
%% @doc
%% DELETE
%% @end
%%--------------------------------------------------------------------
-spec delete(strava_auth:token(), path()) -> ok | {error, map()}.

delete(Token, Path) ->
    {Status, _ResHeaders, ResBody} =
        strava_http:request(
          _Method = delete,
          _Headers = headers(Token),
          _URL = url(Path),
          _Options = #{},
          _ContentType = <<>>,
          _Body = <<>>
         ),
    case strava_http:status_atom(Status) of
        ok -> ok;
        error -> {error, jsx:decode(ResBody, [return_maps])}
    end.

%%--------------------------------------------------------------------
%% @doc
%% GET application/json
%% @end
%%--------------------------------------------------------------------
-spec read(strava_auth:token(), path()) -> {ok, map()} | {error, map()}.

read(Token, Path) ->
    read(Token, Path, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% GET application/json
%% @end
%%--------------------------------------------------------------------
-spec read(strava_auth:token(), path(), strava_http:query()) ->
                  {ok, map()} | {error, map()}.

read(Token, Path, Options) ->
    {Status, _ResHeaders, ResBody} =
        strava_http:request(
          _Method = get,
          _Headers = headers(Token),
          _URL = url(Path),
          Options,
          _ContentType = <<>>,
          _Body = <<>>
         ),
    {strava_http:status_atom(Status), jsx:decode(ResBody, [return_maps])}.

%%--------------------------------------------------------------------
%% @doc
%% PUT application/x-www-form-urlencoded
%% @end
%%--------------------------------------------------------------------
-spec update(strava_auth:token(), path(), strava_http:query()) ->
                    {ok, map()} | {error, map()}.

update(Token, Path, Content) ->
    {Status, _ResHeaders, ResBody} =
        strava_http:request(
          _Method = put,
          _Headers = headers(Token),
          _URL = url(Path),
          _Options = #{},
          _ContentType = <<"application/x-www-form-urlencoded">>,
          _Body = strava_http:qs(Content)
         ),
    {strava_http:status_atom(Status), jsx:decode(ResBody, [return_maps])}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec headers(strava_auth:token()) -> strava_http:headers().

headers(Token) ->
    [{<<"Authorization">>, [<<"Bearer ">>, Token]}].

-spec headers(strava_auth:token(), binary()) -> strava_http:headers().

headers(Token, ETag) ->
    [{<<"If-None-Match">>, ETag} | headers(Token)].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec url(path()) -> strava_http:url().

url(Path) ->
    [ <<"https://www.strava.com/api/v3/">>,
      lists:map(fun(Elem) ->
                        [strava_util:to_binary(Elem), <<"/">>]
                end, Path) ].
