-module(strava_api).

%% Types
-export_type([content/0, options/0, path/0]).

%% API
-export([create/3, delete/2, read/2, read/3, update/3]).

%% API
-export([qs/1, qs/2]).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type content() :: map().
-type options() :: map().
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
-spec create(strava_auth:token(), path(), content() | strava_multipart:t()) ->
                    {ok, content()} | {error, pos_integer()}.

create(Token, Path, Content) ->
    {ContentType, Body} =
        case Content of
            _ when is_map(Content) ->
                {"application/x-www-form-urlencoded", qs(Content, <<>>)};
            _ when is_list(Content) ->
                strava_multipart:form_data(Content)
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
    ContentType = "",
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
-spec read(strava_auth:token(), path()) -> {ok, content()} | {error, pos_integer()}.

read(Token, Path) ->
    read(Token, Path, _Options = #{}).

%%--------------------------------------------------------------------
%% @doc
%% GET application/json
%% @end
%%--------------------------------------------------------------------
-spec read(strava_auth:token(), path(), options()) -> {ok, content()} | {error, pos_integer()}.

read(Token, Path, Options) ->
    ContentType = "",
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
-spec update(strava_auth:token(), path(), content()) -> {ok, content()} | {error, pos_integer()}.

update(Token, Path, Content) ->
    Options = #{},
    ContentType = "application/x-www-form-urlencoded",
    Body = qs(Content, <<>>),
    case request(put, Token, Path, Options, ContentType, Body) of
        {ok, ResBody} -> {ok, strava_json:decode(ResBody)};
        Error -> Error
    end.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec qs(options()) -> binary().

qs(Options) ->
    qs(Options, _Prefix = <<>>).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec qs(options(), binary()) -> binary().

qs(Options, Prefix) ->
    {_Prefix1, Ans} =
        maps:fold(
          fun(K, V, {Sep, Ans}) ->
                  {$&, [Ans, Sep,
                        http_uri:encode(strava_util:to_string(K)),
                        $=,
                        http_uri:encode(strava_util:to_string(V))]}
          end, {Prefix, []}, Options),
    iolist_to_binary(Ans).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec request(httpc:method(), strava_auth:token(), path(), options(),
              httpc:content_type(), httpc:body()) ->
                     {ok, binary()} | {error, pos_integer()}.

request(Method, Token, Path, Options, ContentType, Body) ->
    URL = strava_util:to_string([url(Path), qs(Options, $?)]),
    Headers = [{"Authorization", strava_util:to_string([<<"Bearer ">>, Token])}],
    Request = case Method of
                  _ when Method =:= delete;
                         Method =:= get ->
                      {URL, Headers};
                  _ when Method =:= post;
                         Method =:= put ->
                      {URL, Headers, ContentType, Body}
              end,
    ?debugVal({URL, Headers, ContentType, Body}),
    case httpc:request(Method, Request, _HTTPOptions = [],
                       _Options = [{body_format, binary},
                                   {full_result, false}],
                       strava)
    of
        {ok, {Status, ResBody}}
          when Status >= 200, Status =< 299 ->
            ?debugVal({Status, ResBody}),
            {ok, ResBody};
        {ok, {Status, ResBody}} ->
            ?debugVal({Status, ResBody}),
            {error, Status}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec url(path()) -> binary().

url(Path) ->
    iolist_to_binary(
      [ <<"https://www.strava.com/api/v3/">>,
        lists:map(fun(Elem) ->
                          [strava_util:to_binary(Elem), <<"/">>]
                  end, Path) ]
     ).
