-module(sudoku).
-export([fetch_good_sudoku/0, fetch_bad_sudoku/0, sum45/1, create_column/1, create_tail_grid/1]).

%% For quick & dirty purposes, a Sudoku grid is represented as a list of lists.
fetch_good_sudoku() ->
         [
            [3, 9, 1, 2, 8, 6, 5, 7, 4],
            [4, 8, 7, 3, 5, 9, 1, 2, 6],
            [6, 5, 2, 7, 1, 4, 8, 3, 9],
            [8, 7, 5, 4, 3, 1, 6, 9, 2],
            [2, 1, 3, 9, 6, 7, 4, 8, 5],
            [9, 6, 4, 5, 2, 8, 7, 1, 3],
            [1, 4, 9, 6, 7, 3, 2, 5, 8],
            [5, 3, 8, 1, 4, 2, 9, 6, 7],
            [7, 2, 6, 8, 9, 5, 3, 4, 1]
        ].

fetch_bad_sudoku() ->
        [
            [3, 9, 1, 2, 8, 6, 5, 7, 4],
            [3, 9, 1, 2, 8, 6, 5, 7, 4],
            [3, 9, 1, 2, 8, 6, 5, 7, 4],
            [3, 9, 1, 2, 8, 6, 5, 7, 4],
            [4, 8, 7, 3, 5, 9, 1, 2, 6],
            [4, 8, 7, 3, 5, 9, 1, 2, 6],
            [4, 8, 7, 3, 5, 9, 1, 2, 6],
            [4, 8, 7, 3, 5, 9, 1, 2, 6],
            [6, 5, 7, 3, 2, 1, 9, 8, 6]
        ].

-spec sum45(Line::list(integer())) -> boolean().
sum45(Line) ->
    lists:sum(Line) =:= 45.

-spec create_column(Grid::list()) -> list(integer()).
create_column(Grid) ->
    [ hd(X) || X <- Grid ].

-spec create_tail_grid(Grid::list()) -> list(list()).
create_tail_grid(Grid) ->
    [ tl(X) || X <- Grid ].
