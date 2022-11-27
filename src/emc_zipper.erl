-module(emc_zipper).

-export([
    new/0,
    new/1,
    from_list/1,
    from_list/2,
    to_list/1,
    current/1,
    size/1,
    move_right/1,
    move_left/1,
    insert/2,
    remove/1
]).

-export([
    format_error/2
]).

% lax - moving past either end of the collection is
% impossible, the focus stays at the first/last element.
% strict - moving past either end results in an error.
% ring - moving past either end wraps around.
-type mode() :: lax | strict | ring.
-define(DEFAULT_MODE, lax).

-record(zipper, {
    left,
    left_len,
    right,
    right_len,
    mode :: mode()
}).

-define(EMPTY_ERROR_INFO(Pos), {error_info, #{cause => #{Pos => "Empty zipper"}}}).

-opaque t() :: #zipper{}.
-export_type([t/0]).

new() ->
    from_list([]).

new(Mode) ->
    from_list([], Mode).

from_list(Xs) ->
    from_list(Xs, ?DEFAULT_MODE).

from_list(Xs, Mode) ->
    #zipper{
        left = [],
        left_len = 0,
        right = Xs,
        right_len = length(Xs),
        mode = Mode
    }.

to_list(#zipper{left = Left, right = Right}) ->
    lists:reverse(Left, Right).

current(#zipper{right = []} = Zipper) ->
    error(badarg, [Zipper], [?EMPTY_ERROR_INFO(1)]);
current(#zipper{left_len = LeftLen, right = [H | _]}) ->
    {LeftLen + 1, H}.

size(#zipper{left_len = LL, right_len = RL}) ->
    LL + RL.

move_right(#zipper{right = []} = Zipper) ->
    Zipper;
move_right(#zipper{right = [_]} = Zipper) ->
    Zipper;
move_right(#zipper{left = L, left_len = LL, right = [RH | RT], right_len = RL}) ->
    #zipper{
        left = [RH | L],
        left_len = LL + 1,
        right = RT,
        right_len = RL - 1
    }.

move_left(#zipper{left = []} = Zipper) ->
    Zipper;
move_left(#zipper{left = [LH | LT], left_len = LL, right = R, right_len = RL}) ->
    #zipper{
        left = LT,
        left_len = LL - 1,
        right = [LH | R],
        right_len = RL + 1
    }.

insert(What, #zipper{right = R} = Zipper) ->
    Zipper#zipper{right = [What | R]}.

remove(#zipper{right = []} = Zipper) ->
    error(badarg, [Zipper], [?EMPTY_ERROR_INFO(1)]);
remove(#zipper{right = [Last]} = Zipper) ->
    NewZipper = Zipper#zipper{right = []},
    {Last, move_left(NewZipper)};
remove(#zipper{right = [RH | RT]} = Zipper) ->
    {RH, Zipper#zipper{right = RT}}.

format_error(badarg, [{_M, _F, _A, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info),
    maps:get(cause, ErrorInfo).
