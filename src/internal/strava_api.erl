-module(strava_api).

%% Types
-export_type([path/0]).

%% API
-export([create/3, delete/2, read/2, read/3, update/3]).

%% API
-export([convert/1, convert/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type convert_fun() :: fun() | {list, fun()}.
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
    Options = #{},
    case request(post, Token, Path, Options, ContentType, Body) of
        {Ans, ResBody} -> {Ans, strava_json:decode(ResBody)}
    end.

%%--------------------------------------------------------------------
%% @doc
%% DELETE
%% @end
%%--------------------------------------------------------------------
-spec delete(strava_auth:token(), path()) -> ok | {error, map()}.

delete(Token, Path) ->
    Options = #{},
    ContentType = <<>>,
    Body = <<>>,
    case request(delete, Token, Path, Options, ContentType, Body) of
        {ok, _ResBody} -> ok;
        {error, ResBody} -> {error, strava_json:decode(ResBody)}
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
    ContentType = <<>>,
    Body = <<>>,
    case request(get, Token, Path, Options, ContentType, Body) of
        {Ans, ResBody} -> {Ans, strava_json:decode(ResBody)}
    end.

%%--------------------------------------------------------------------
%% @doc
%% PUT application/x-www-form-urlencoded
%% @end
%%--------------------------------------------------------------------
-spec update(strava_auth:token(), path(), strava_http:query()) ->
                    {ok, map()} | {error, map()}.

update(Token, Path, Content) ->
    Options = #{},
    ContentType = "application/x-www-form-urlencoded",
    Body = strava_http:qs(Content),
    case request(put, Token, Path, Options, ContentType, Body) of
        {Ans, ResBody} -> {Ans, strava_json:decode(ResBody)}
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
-spec request(strava_http:method(), strava_auth:token(), path(),
              strava_http:query(), strava_http:content_type(),
              strava_http:body()) ->
                     {ok, binary()} | {error, binary()}.

request(Method, Token, Path, Options, ContentType, Body) ->
    URL = url(Path),
    Headers = [{<<"Authorization">>, [<<"Bearer ">>, Token]}],
    strava_http:request(Method, Headers, URL, Options,
                        ContentType, Body).

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
