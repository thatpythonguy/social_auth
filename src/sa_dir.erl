-module(sa_dir).

-export([
         init/0,
         insert/3,
         delete/1,
         delete/2,
         lookup/2
        ]).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

insert(Key, Pid) ->
    ets:insert(?TABLE_ID, {Key, Pid}).

lookup(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
        [{Key, Pid}] -> {ok, Pid};
        []           -> {error, not_found}
    end.

reindex(OldKey, NewKey) ->
    case lookup(OldKey) of
        {ok, Pid} ->
            delete(OldKey),
            insert(Key, Pid);
        {error, _} ->
            insert(Key, Pid)
    end.
            

delete(Pid) ->
    ets:match_delete(?TABLE_ID, {'_', Pid}).

delete(Key) ->
    ets:match_delete(?TABLE_ID, {Key, '_'}).

to_list(undefined) -> [];
to_list(<<>>) -> [];
to_list(L) when is_list(L) -> L;
to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(F) when is_float(F) -> float_to_list(F).

