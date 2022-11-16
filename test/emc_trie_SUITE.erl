-module(emc_trie_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

% Test cases
-export([
    words_test/1,
    % This one is here for convenience. Just append your test cases above it.
    always_pass/1
]).

all() ->
    [
        words_test,
        always_pass
    ].

init_per_suite(Config) ->
    Words = get_the_words(),
    [{words, Words} | Config].

end_per_suite(_Config) ->
    ok.

% Test cases

always_pass(_Config) ->
    ok.

words_test(Config) ->
    Words = proplists:get_value(words, Config),

    Trie = emc_trie:from_list(Words),
    ?assertEqual([<<"aardvark">>, <<"aardwolf">>], emc_trie:search(<<"aar">>, Trie, 3)),
    ?assertEqual([], emc_trie:search(<<"aar">>, Trie, 0)),
    ?assertEqual([], emc_trie:search(<<"zz">>, Trie, 5)).

% Implementation details

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
