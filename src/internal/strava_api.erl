-module(strava_api).

%% Types
-export_type([etag/0, path/0]).

%% API
-export([create/3, delete/2, read/2, read/3, update/3]).

%% API
-export([read_etag/3, read_etag/4]).

%% API
-export([convert/1, convert/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type convert_fun() :: fun() | {list, fun()}.
-type etag() :: binary().
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
    {strava_http:status_atom(Status), strava_json:decode(ResBody)}.

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
        error -> {error, strava_json:decode(ResBody)}
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
    {strava_http:status_atom(Status), strava_json:decode(ResBody)}.

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
    {strava_http:status_atom(Status), strava_json:decode(ResBody)}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec read_etag(strava_auth:token(), etag() | undefined, path()) ->
                       {ok, etag(), map() | undefined} |
                       {error, map()}.

read_etag(Token, ETag, Path) ->
    read_etag(Token, ETag, Path, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec read_etag(strava_auth:token(), path(), strava_http:query(),
                etag() | undefined) ->
                       {ok, etag(), map() | undefined} |
                       {error, map()}.

read_etag(Token, ETag, Path, Options) ->
    {Status, ResHeaders, ResBody} =
        strava_http:request(
          _Method = get,
          _Headers = headers(Token, ETag),
          _URL = url(Path),
          Options,
          _ContentType = <<>>,
          _Body = <<>>
         ),
    case Status of
        _ when Status >= 200, Status =< 299 ->
            ETag1 = proplists:get_value("etag", ResHeaders),
            {ok, ETag1, strava_json:decode(ResBody)};
        304 ->
            {ok, ETag, undefined};
        _ ->
            {error, strava_json:decode(ResBody)}
    end.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Convert result of a CRUD function from JSON representation to
%% application's.
%% @end
%%--------------------------------------------------------------------
-spec convert(ok | {error, map()}) -> ok | strava:error().

convert(ok) ->
    ok;

convert({ok, _JSON}) ->
    ok;

convert({error, JSON}) ->
    strava_repr:to_error(JSON).

%%--------------------------------------------------------------------
%% @doc
%% Convert result of a CRUD function from JSON representation to
%% application's.
%% @end
%%--------------------------------------------------------------------
-spec convert({ok, map()} | {error, map()}, convert_fun()) ->
                     {ok, term()} | strava:error().

convert({ok, JSON}, {list, Fun}) ->
    {ok, lists:map(Fun, JSON)};

convert({ok, JSON}, Fun) ->
    {ok, Fun(JSON)};

convert({error, JSON}, _Fun) ->
    strava_repr:to_error(JSON).

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

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec headers(strava_auth:token(), binary() | undefined) -> strava_http:headers().

headers(Token, _ETag = undefined) ->
    headers(Token);

headers(Token, ETag) ->
    [{<<"If-None-Match">>, ETag} | headers(Token)].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec url(path()) -> strava_http:url().

url(Path) ->
    [ <<"https://www.strava.com/api/v3">>,
      lists:map(fun(Elem) ->
                        [<<"/">>, strava_util:to_binary(Elem)]
                end, Path) ].
