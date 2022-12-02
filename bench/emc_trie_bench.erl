%% @doc Helper functions to benchmark the trie implementation
-module(emc_trie_bench).

-export([
    init/0,
    hit/1,
    miss/1
]).

-define(MISS_TERM, <<"notaword">>).
-define(HIT_TERM, <<"yol">>).

init() ->
    {ok, Bin} = file:read_file("/usr/share/dict/words"),
    Words = string:split(Bin, "\n", all),
    SortedWords = lists:sort(Words),
    Trie = emc_trie:from_list(Words),
    persistent_term:put({?MODULE, sorted_words}, SortedWords),
    persistent_term:put({?MODULE, trie}, Trie).

miss(trie) ->
    Trie = persistent_term:get({?MODULE, trie}),
    emc_trie:search(?MISS_TERM, Trie, 1);
miss(naive) ->
    Words = persistent_term:get({?MODULE, sorted_words}),
    lists:member(?MISS_TERM, Words).

hit(trie) ->
    Trie = persistent_term:get({?MODULE, trie}),
    emc_trie:search(?HIT_TERM, Trie, 1);
hit(naive) ->
    Words = persistent_term:get({?MODULE, sorted_words}),
    find_in_words(?HIT_TERM, Words, 1, []).

find_in_words(_Prefix, [], _, Acc) ->
    Acc;
find_in_words(_Prefix, _, 0, Acc) ->
    Acc;
find_in_words(Prefix, [H | T], Limit, Acc) ->
    case binary:match(H, Prefix) of
        {0, _} ->
            find_in_words(Prefix, T, Limit - 1, [H | Acc]);
        _ ->
            find_in_words(Prefix, T, Limit, Acc)
    end.
