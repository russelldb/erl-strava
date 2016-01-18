-module(strava_http).

%% Types
-export_type([body/0, content_type/0, headers/0, method/0, query/0,
              url/0]).

%% API
-export([qs/1, qs/2, request/4, request/6]).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type body() :: iodata().
-type content_type() :: iodata().
-type headers() :: [{iodata(), iodata()}].
-type method() :: delete | get | post | put.
-type query() :: map().
-type url() :: iodata().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec qs(query()) -> iodata().

qs(Query) ->
    qs(Query, _Prefix = <<>>).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec qs(query(), iodata()) -> iodata().

qs(Query, Prefix) ->
    {_Prefix1, Ans} =
        maps:fold(
          fun(K, V, {Sep, Ans}) ->
                  {$&, [Ans, Sep,
                        http_uri:encode(strava_util:to_string(K)),
                        $=,
                        http_uri:encode(strava_util:to_string(V))]}
          end, {Prefix, []}, Query),
    Ans.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec request(method(), headers(), url(), query()) ->
                     {ok, binary()} | {error, binary()}.

request(Method, Headers, URL, Query) ->
    request(Method, Headers, URL, Query,
            _ContentType = "", _Body = <<>>).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec request(method(), headers(), url(), query(), content_type(),
              body()) -> {ok, binary()} | {error, binary()}.

request(Method, Headers, URL, Query, ContentType, Body) ->
    URL1 = strava_util:to_string([URL, qs(Query, $?)]),
    Headers1 = lists:map(fun({K, V}) ->
                                 {strava_util:to_string(K),
                                  strava_util:to_string(V)}
                         end, Headers),
    Request = case strava_util:to_string(ContentType) of
                  _None = "" -> {URL1, Headers1};
                  ContentType1 -> {URL1, Headers1, ContentType1,
                                   strava_util:to_binary(Body)}
              end,
    case httpc:request(Method, Request, _HTTPOptions = [],
                       _Options = [{body_format, binary},
                                   {full_result, false}],
                       strava) of
        {ok, {Status, ResBody}}
          when Status >= 200, Status =< 299 ->
            {ok, ResBody};
        {ok, {_Status, ResBody}} ->
            {error, ResBody}
    end.
