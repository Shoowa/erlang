-module(sudoku).
-export([fetch_good_sudoku/0, fetch_bad_sudoku/0, sum45/1, create_column/1, create_tail_grid/1, nine_numbers/1, check_columns/1, check_rows/1]).

-type grid() :: list(list(integer())).

%% For quick & dirty purposes, a Sudoku grid is represented as a list of listed integers. No functions check the size of a grid. The aforementioned type definition of 'grid()' matches this specification.
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

%% Recursively inspect a list for digits 1 through 9. Return a TRUE or FALSE value.
-spec nine_numbers(Line :: list(integer())) -> boolean().
nine_numbers(Line) ->
    nine_numbers(9, Line).

-spec nine_numbers(N :: integer(), Line :: list()) -> boolean().
nine_numbers(0, Line) ->
    true;
nine_numbers(N, Line) when N > 0 ->
    case lists:member(N, Line) of
        true ->
            nine_numbers(N-1, Line);
        false ->
            io:format("Failure! The list lacks nine unique digits~n"),
            false
    end.

%% The sum of 1 through 9 is 45. Return TRUE or FALSE if a list, ideally composed of digits 1 through 9, adds up to 45.
-spec sum45(Line :: list(integer())) -> boolean().
sum45(Line) ->
    lists:sum(Line) =:= 45.

%% Peels the head off each list in a Sudoku grid composed of nine lists, then produces a new list from those nine heads.
-spec create_column(Grid :: grid()) -> list(integer()).
create_column(Grid) ->
    [ hd(X) || X <- Grid ].

%% Peels the heads of nine lists off, then returns the tails of all nine lists.
-spec create_tail_grid(Grid :: grid()) -> list(list()).
create_tail_grid(Grid) ->
    [ tl(X) || X <- Grid ]. 

%% Recursively inspect each column of the grid for digits 1 through 9. Return TRUE and continue inspecting for the ultimate truth, or cease and return FALSE upon discovering a column doesn't contain all digits between 1 and 9.
-spec check_columns(Grid :: grid()) -> boolean().
check_columns([[],[],[],[],[],[],[],[],[]]) ->
    true;
check_columns(Grid) ->
    Column = create_column(Grid),
    case nine_numbers(Column) of
        true ->
            LesserGrid = create_tail_grid(Grid),
            check_columns(LesserGrid);
        false ->
            io:format("Failure! The column lacks nine unique digits.~n"),
            false
    end.

%% Check each row of a grid for the digits 1 through 9. If any row lacks all nine digits, return FALSE.
-spec check_rows(Grid :: grid()) -> boolean().
check_rows(Grid) ->
    Outcome = [nine_numbers(X) || X <- Grid],
    case lists:member(false, Outcome) of
        true ->
            false;
        false ->
            true
    end.
