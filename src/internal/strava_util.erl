-module(strava_util).

%% API
-export([to_binary/1, to_string/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_binary(atom() | integer() | iodata()) -> binary().

to_binary(Term) when is_atom(Term) -> atom_to_binary(Term, latin1);

to_binary(Term) when is_integer(Term) -> integer_to_binary(Term);

to_binary(Term) -> iolist_to_binary(Term).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_string(atom() | integer() | iodata()) -> string().

to_string(Term) when is_atom(Term) -> atom_to_list(Term);

to_string(Term) when is_binary(Term) -> binary_to_list(Term);

to_string(Term) when is_integer(Term) -> integer_to_list(Term);

to_string(Term) -> binary_to_list(iolist_to_binary(Term)).
