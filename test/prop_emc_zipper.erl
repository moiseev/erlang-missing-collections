-module(prop_emc_zipper).

-include_lib("proper/include/proper.hrl").

-export([
    % Only need to export the /1 functions.
    % Properties (/0) functions are exported automatically.
    prop_from_list_and_back/1,
    prop_size_equals_list_length/1,
    prop_moves_dont_affect_size/1,
    prop_inserts_from_list_equivalency/1,
    prop_insert_remove_noop/1
]).

prop_from_list_and_back(doc) ->
    "to_list . from_list results in the original list. Always.".

prop_from_list_and_back() ->
    ?FORALL(
        Xs,
        list(),
        Xs =:= emc_zipper:to_list(emc_zipper:from_list(Xs))
    ).

prop_size_equals_list_length(doc) ->
    "size of the zipper is always the same as the size of the original list".

prop_size_equals_list_length() ->
    ?FORALL(
        Xs,
        list(),
        length(Xs) =:= emc_zipper:size(emc_zipper:from_list(Xs))
    ).

prop_moves_dont_affect_size(doc) ->
    "Moving the focus should not affect the size of the zipper".

prop_moves_dont_affect_size() ->
    ?FORALL(
        {Zipper, Moves},
        {zipper_gen(), list(move_gen())},
        begin
            ZipperAfterMoves = apply_moves(Zipper, Moves),
            emc_zipper:size(ZipperAfterMoves) =:= emc_zipper:size(Zipper)
        end
    ).

prop_inserts_from_list_equivalency(doc) ->
    "An empty zipper following a bunch of inserts shold be the same"
    " as the one built from the list of the same elements".

prop_inserts_from_list_equivalency() ->
    ?FORALL(
        Xs,
        list(),
        begin
            FromInserts = lists:foldl(
                fun(X, Z) -> emc_zipper:insert(X, Z) end,
                emc_zipper:new(),
                Xs
            ),
            FromList = emc_zipper:from_list(Xs),
            lists:reverse(emc_zipper:to_list(FromInserts)) =:=
                emc_zipper:to_list(FromList)
        end
    ).

prop_insert_remove_noop(doc) ->
    "Inserting an element at some random place in the zipper "
    "ans then removing it should result in the original list".

prop_insert_remove_noop() ->
    ?FORALL(
        {Xs, Moves},
        {list(), list(move_gen())},
        begin
            Zipper0 = emc_zipper:from_list(Xs),
            Zipper1 = apply_moves(Zipper0, Moves),
            Ref = make_ref(),
            Zipper2 = emc_zipper:insert(Ref, Zipper1),
            {Removed, Zipper3} = emc_zipper:remove(Zipper2),
            Removed =:= Ref andalso
                emc_zipper:to_list(Zipper3) =:= Xs
        end
    ).

zipper_gen() ->
    ?LET(Xs, list(), emc_zipper:from_list(Xs)).

move_gen() ->
    ?LET(
        MoveRight,
        boolean(),
        case MoveRight of
            true ->
                move_right;
            _ ->
                move_left
        end
    ).

apply_moves(Zipper, Moves) ->
    lists:foldl(
        fun(Move, Z) -> emc_zipper:Move(Z) end,
        Zipper,
        Moves
    ).
