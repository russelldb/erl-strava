-module(strava_multipart).

%% Types
-export_type([multipart/0, part_data/0, part_name/0, t/0]).

%% API
-export([form_data/1, form_data/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type part_name() :: atom() | iodata().

-type part_data() :: atom() | integer() | iodata()
                   | {_Type :: iodata(), file:filename_all()}
                   | {_Type :: iodata(), _Encoding :: iodata(), file:filename_all()}.

-type multipart() :: [{part_name(), part_data()}].

-type t() :: multipart().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec form_data(multipart()) -> {iodata(), httpc:body()}.

form_data(Form) ->
    form_data(Form, random_boundary()).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec form_data(multipart(), iodata()) -> {iodata(), httpc:body()}.

form_data(Form, Boundary) ->
    {[<<"multipart/form-data; boundary=\"">>, Boundary, <<"\"">>],
     [lists:map(
        fun({Name, {ContentType, ContentEncoding, FileName}}) ->
                {ok, FileData} = file:read_file(FileName),
                part(Boundary, headers(Name, ContentType,
                                       ContentEncoding, FileName), FileData);
           ({Name, {ContentType, FileName}}) ->
                {ok, FileData} = file:read_file(FileName),
                part(Boundary, headers(Name, ContentType, FileName), FileData);
           ({Name, Body}) ->
                part(Boundary, headers(Name),
                     [strava_util:to_binary(Body), <<"\r\n">>])
        end, Form),
      <<"--">>, Boundary, <<"--\r\n">>]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Part of a multipart message.
%% @end
%%--------------------------------------------------------------------
-spec part(iodata(), iodata(), iodata()) -> iodata().

part(Boundary, Headers, Content) ->
    [<<"--">>, Boundary, <<"\r\n">>,
     Headers, <<"\r\n">>, Content].

%%--------------------------------------------------------------------
%% @doc
%% Content disposition header for form field `Name'.
%% @end
%%--------------------------------------------------------------------
-spec content_disposition(part_name()) -> iodata().

content_disposition(Name) ->
    [<<"Content-Disposition: form-data; name=\"">>,
     strava_util:to_binary(Name), <<"\"">>].

%%--------------------------------------------------------------------
%% @doc
%% Content disposition header for file form field `Name'. File has
%% name `FileName'.
%% @end
%%--------------------------------------------------------------------
-spec content_disposition(part_name(), file:filename_all()) -> iodata().

content_disposition(Name, FileName) ->
    [content_disposition(Name), <<"; filename=\"">>,
     filename:basename(FileName), <<"\"">>].

%%--------------------------------------------------------------------
%% @doc
%% Part headers for form field `Name'.
%% @end
%%--------------------------------------------------------------------
-spec headers(part_name()) -> iodata().

headers(Name) ->
    [content_disposition(Name), <<"\r\n">>].

%%--------------------------------------------------------------------
%% @doc
%% Part headers for file form field `Name'.
%% @end
%%--------------------------------------------------------------------
-spec headers(part_name(), iodata(), file:filename_all()) -> iodata().

headers(Name, ContentType, FileName) ->
    [content_disposition(Name, FileName), <<"\r\n">>,
     <<"Content-Type: ">>, ContentType, <<"\r\n">>].

%%--------------------------------------------------------------------
%% @doc
%% Part headers for encoded file form field `Name'.
%% @end
%%--------------------------------------------------------------------
-spec headers(part_name(), iodata(), iodata(), file:filename_all()) -> iodata().

headers(Name, ContentType, ContentEncoding, FileName) ->
    [headers(Name, ContentType, FileName),
     <<"Content-Encoding: ">>, ContentEncoding, <<"\r\n">>].

%%--------------------------------------------------------------------
%% @doc
%% Random multipart boundary.
%% @end
%%--------------------------------------------------------------------
-spec random_boundary() -> iodata().

random_boundary() ->
    base64:encode(crypto:rand_bytes(32)).
