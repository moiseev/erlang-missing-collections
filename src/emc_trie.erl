-module(emc_trie).

-export([
    from_list/1,
    search/3
]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

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

words_test() ->
    Words = get_the_words(),

    Trie = from_list(Words),
    ?assertEqual([<<"aardvark">>, <<"aardwolf">>], search(<<"aar">>, Trie, 3)),
    ?assertEqual([], search(<<"aar">>, Trie, 0)),
    ?assertEqual([], search(<<"zz">>, Trie, 5)).

get_the_words() ->
    Candidates = ["/usr/share/dict/words", "/usr/dict/words"],
    Contents = [Bin || Path <- Candidates, {ok, Bin} <- [file:read_file(Path)]],
    case Contents of
        [Head | _] ->
            string:split(Head, "\n", all);
        _ ->
            % Did not locate the words file
            [<<"aa">>, <<"aardvark">>, <<"aardwolf">>, <<"armageddon">>]
    end.
