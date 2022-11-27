-module(emc_zipper_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0
]).

% Test cases
-export([
    current_fails_on_empty/1,
    remove_fails_on_empty/1,
    basic_moves/1
]).

all() ->
    [
        current_fails_on_empty,
        remove_fails_on_empty,
        basic_moves
    ].

% Test cases

current_fails_on_empty(_Config) ->
    ?assertError(badarg, emc_zipper:current(emc_zipper:new())).

remove_fails_on_empty(_Config) ->
    ?assertError(badarg, emc_zipper:remove(emc_zipper:new())).

basic_moves(_Config) ->
    Zipper0 = emc_zipper:from_list([a, b, c]),
    % Should be focused on the a element now
    ?assertEqual({1, a}, emc_zipper:current(Zipper0)),
    Zipper1 = emc_zipper:move_right(Zipper0),
    ?assertEqual({2, b}, emc_zipper:current(Zipper1)),
    Zipper2 = emc_zipper:move_right(Zipper1),
    ?assertEqual({3, c}, emc_zipper:current(Zipper2)),
    % Moving past the right end
    Zipper3 = emc_zipper:move_right(Zipper2),
    ?assertEqual({3, c}, emc_zipper:current(Zipper3)),

    Zipper4 = emc_zipper:move_left(Zipper3),
    ?assertEqual({2, b}, emc_zipper:current(Zipper4)),
    Zipper5 = emc_zipper:move_left(Zipper4),
    ?assertEqual({1, a}, emc_zipper:current(Zipper5)),
    % Moving past the left end
    Zipper6 = emc_zipper:move_left(Zipper5),
    ?assertEqual({1, a}, emc_zipper:current(Zipper6)),
    ok.
