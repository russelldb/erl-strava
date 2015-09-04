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
                    {ok, map()} | {error, pos_integer()}.

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
        {ok, ResBody} -> {ok, strava_json:decode(ResBody)};
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% DELETE
%% @end
%%--------------------------------------------------------------------
-spec delete(strava_auth:token(), path()) -> ok | {error, pos_integer()}.

delete(Token, Path) ->
    Options = #{},
    ContentType = <<>>,
    Body = <<>>,
    case request(delete, Token, Path, Options, ContentType, Body) of
        {ok, _ResBody} -> ok;
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% GET application/json
%% @end
%%--------------------------------------------------------------------
-spec read(strava_auth:token(), path()) -> {ok, map()} | {error, pos_integer()}.

read(Token, Path) ->
    read(Token, Path, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% GET application/json
%% @end
%%--------------------------------------------------------------------
-spec read(strava_auth:token(), path(), strava_http:query()) ->
                  {ok, map()} | {error, pos_integer()}.

read(Token, Path, Options) ->
    ContentType = <<>>,
    Body = <<>>,
    case request(get, Token, Path, Options, ContentType, Body) of
        {ok, ResBody} -> {ok, strava_json:decode(ResBody)};
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% PUT application/x-www-form-urlencoded
%% @end
%%--------------------------------------------------------------------
-spec update(strava_auth:token(), path(), strava_http:query()) ->
                    {ok, map()} | {error, pos_integer()}.

update(Token, Path, Content) ->
    Options = #{},
    ContentType = "application/x-www-form-urlencoded",
    Body = strava_http:qs(Content),
    case request(put, Token, Path, Options, ContentType, Body) of
        {ok, ResBody} -> {ok, strava_json:decode(ResBody)};
        Error -> Error
    end.

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
                     {ok, binary()} | {error, pos_integer()}.

request(Method, Token, Path, Options, ContentType, Body) ->
    URL = url(Path),
    Headers = [{<<"Authorization">>, [<<"Bearer ">>, Token]}],
    case strava_http:request(Method, Headers, URL, Options,
                             ContentType, Body) of
        {ok, _Status, ResBody} -> {ok, ResBody};
        {error, Status, _ResBody} -> {error, Status}
    end.

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
