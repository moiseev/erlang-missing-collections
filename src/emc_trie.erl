-module(emc_trie).

-export([
    from_list/1,
    search/3
]).

from_list(XS) ->
    Table = ets:new(
        ?MODULE,
        % ordered_set has a nice property of being able to find the next key
        % even for the keys that have never been inserted, which works nicely
        % for the prefix search.
        [ordered_set, protected, {read_concurrency, true}]
    ),
    [ets:insert(Table, {X}) || X <- XS],
    Table.

search(Prefix, Trie, Limit) when is_binary(Prefix) ->
    search(Prefix, Trie, ets:next(Trie, Prefix), Limit, []).

search(_Prefix, _Trie, _Item, 0, Acc) ->
    lists:reverse(Acc);
search(_Prefix, _Trie, '$end_of_table', _Limit, Acc) ->
    lists:reverse(Acc);
search(Prefix, Trie, Item, Limit, Acc) ->
    case binary:match(Item, Prefix) of
        {0, _} ->
            search(Prefix, Trie, ets:next(Trie, Item), Limit - 1, [Item | Acc]);
        _NoMatchOrNotAPrefix ->
            lists:reverse(Acc)
    end.
