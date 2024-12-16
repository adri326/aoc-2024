read_maze(Maze) :-
    open("./input/day16.txt", read, Stream),
    parse_maze(Stream, Maze),
    close(Stream),
    !.

read_example_maze(Maze) :-
    open("./input/day16-example.txt", read, Stream),
    parse_maze(Stream, Maze),
    close(Stream),
    !.

solve_part1(Maze, Cost) :-
    starting_pos(XPos, YPos, Maze),
    ending_pos(XEnd, YEnd, Maze),
    fill_grid_with(Maze, Closed, [0, 0, 0, 0]),
    solve([[XPos, YPos, 0, 0, []]], Maze, XEnd, YEnd, Closed, Cost, _).

solve_part2(Maze, GoodCount) :-
    starting_pos(XPos, YPos, Maze),
    ending_pos(XEnd, YEnd, Maze),
    fill_grid_with(Maze, Closed, [0, 0, 0, 0]),
    solve([[XPos, YPos, 0, 0, []]], Maze, XEnd, YEnd, Closed, _, Explored),
    fill_grid_with(Maze, Zeroes, 0),
    fill_grid_at([[XEnd, YEnd] | Explored], Zeroes, ExploredGrid),
    grid_count_ones(ExploredGrid, GoodCount).

% === PARSING ===

parse_line(10, _, []) :- !.
parse_line(-1, _, []) :- !.
parse_line(end_of_file, _, []) :- !.
parse_line(X, Stream, [1 | Rest]) :-
    X is "#",
    get_code(Stream, NextChar),
    parse_line(NextChar, Stream, Rest).
parse_line(X, Stream, [0 | Rest]) :-
    X is ".",
    get_code(Stream, NextChar),
    parse_line(NextChar, Stream, Rest).
parse_line(X, Stream, [2 | Rest]) :-
    X is "S",
    get_code(Stream, NextChar),
    parse_line(NextChar, Stream, Rest).
parse_line(X, Stream, [3 | Rest]) :-
    X is "E",
    get_code(Stream, NextChar),
    parse_line(NextChar, Stream, Rest).

parse_maze(Stream, []) :- at_end_of_stream(Stream).
parse_maze(Stream, [Line | Rest]) :- \+ at_end_of_stream(Stream),
    get_code(Stream, FirstChar),
    parse_line(FirstChar, Stream, Line),
    parse_maze(Stream, Rest).

print_maze([]) :- !.
print_maze([Line | Rest]) :-
    line_to_string(Line, Str),
    format(Str),
    format("\n"),
    print_maze(Rest),
    !.
line_to_string([], []) :- !.
line_to_string([0 | Rest], [Out | OutRest]) :-
    Out is ".",
    line_to_string(Rest, OutRest),
    !.
line_to_string([1 | Rest], [Out | OutRest]) :-
    Out is "#",
    line_to_string(Rest, OutRest),
    !.
line_to_string([2 | Rest], [Out | OutRest]) :-
    Out is "S",
    line_to_string(Rest, OutRest),
    !.
line_to_string([3 | Rest], [Out | OutRest]) :-
    Out is "E",
    line_to_string(Rest, OutRest),
    !.
line_to_string([_ | Rest], [Out | OutRest]) :-
    Out is "?",
    line_to_string(Rest, OutRest).

% === GRID MANIPULATION ===

grid_get(X, Y, Grid, Out) :-
    nth0(Y, Grid, Line),
    nth0(X, Line, Out).

grid3_get(X, Y, Z, Grid, Out) :-
    nth0(Y, Grid, Line),
    nth0(X, Line, Vec),
    nth0(Z, Vec, Out).

replace_nth0(Index, List, Value, Out) :-
    nth0(Index, List, _, Transfer),
    nth0(Index, Out, Value, Transfer).

grid_set(X, Y, Grid, Value, Out) :-
    nth0(Y, Grid, Line),
    replace_nth0(X, Line, Value, NewLine),
    replace_nth0(Y, Grid, NewLine, Out).

grid3_set(X, Y, Z, Grid, Value, Out) :-
    nth0(Y, Grid, Line),
    nth0(X, Line, Vec),
    replace_nth0(Z, Vec, Value, NewVec),
    replace_nth0(X, Line, NewVec, NewLine),
    replace_nth0(Y, Grid, NewLine, Out).

fill_grid_with([], [], _).
fill_grid_with([Line | Rest], [Zeroes | ZeroRest], Value) :-
    fill_line_with(Line, Zeroes, Value),
    fill_grid_with(Rest, ZeroRest, Value).

fill_line_with([], [], _).
fill_line_with([_ | Rest], [Value | Zeroes], Value) :- fill_line_with(Rest, Zeroes, Value).

fill_grid_at([], Grid, Grid).
fill_grid_at([[X, Y] | Rest], Grid, Out) :-
    fill_grid_at(Rest, Grid, Tmp),
    grid_set(X, Y, Tmp, 1, Out).

grid_count_ones([], 0).
grid_count_ones([Line | Rest], Count) :-
    grid_count_ones(Rest, RestCount),
    line_count_ones(Line, LineCount),
    Count is LineCount + RestCount.

line_count_ones([], 0).
line_count_ones([1 | Rest], Count) :-
    line_count_ones(Rest, RestCount),
    Count is RestCount + 1,
    !.
line_count_ones([_ | Rest], Count) :-
    line_count_ones(Rest, Count).

starting_pos(X, Y, Maze) :-
    grid_get(X, Y, Maze, 2),
    !.

ending_pos(X, Y, Maze) :-
    grid_get(X, Y, Maze, 3),
    !.

% === MOVEMENT ===

% get_dir(Dir, X, Y) or get_dir(Dir, XIn, Yin, XOut, YOut)
get_dir(0, 1, 0).
get_dir(1, 0, 1).
get_dir(2, -1, 0).
get_dir(3, 0, -1).

get_dir(Dir, XIn, YIn, XOut, YOut) :-
    get_dir(Dir, XDelta, YDelta),
    XOut is XIn + XDelta,
    YOut is YIn + YDelta.

% Moving forward:
can_move(
    [XPos, YPos, Dir, Cost, Explored],
    Maze,
    Closed,
    [X2, Y2, Dir, NewCost, [[XPos, YPos] | Explored]]
) :-
    get_dir(Dir, XPos, YPos, X2, Y2),
    grid3_get(X2, Y2, Dir, Closed, 0),
    (grid_get(X2, Y2, Maze, 0) ; grid_get(X2, Y2, Maze, 3)),
    NewCost is Cost + 1.

% Turning right:
can_move([XPos, YPos, Dir, Cost, Explored], Maze, Closed, [XPos, YPos, NewDir, NewCost, Explored]) :-
    NewDir is (Dir + 1) mod 4,
    get_dir(NewDir, XPos, YPos, X2, Y2),
    grid3_get(X2, Y2, NewDir, Closed, 0),
    (grid_get(X2, Y2, Maze, 0) ; grid_get(X2, Y2, Maze, 3)),
    NewCost is Cost + 1000.

% Turning left:
can_move([XPos, YPos, Dir, Cost, Explored], Maze, Closed, [XPos, YPos, NewDir, NewCost, Explored]) :-
    NewDir is (Dir + 3) mod 4,
    get_dir(NewDir, XPos, YPos, X2, Y2),
    grid3_get(X2, Y2, NewDir, Closed, 0),
    (grid_get(X2, Y2, Maze, 0) ; grid_get(X2, Y2, Maze, 3)),
    NewCost is Cost + 1000.

% `can_move_in(List, Maze, Closed, Move)`:
% True if `element(Current, List)` and `can_move(Current, Maze, Closed, Move)`.
can_move_in([Move | _], Maze, Closed, Out) :- can_move(Move, Maze, Closed, Out).
can_move_in([_ | Rest], Maze, Closed, Out) :- can_move_in(Rest, Maze, Closed, Out).

filter_closed(_, [], [], []) :- !.
filter_closed([X, Y, D], [[X, Y, D | ElemRest] | Rest], FilteredRest, [[X, Y, D | ElemRest] | ExcludedRest]) :-
    filter_closed([X, Y, D], Rest, FilteredRest, ExcludedRest),
    !.

filter_closed(Cond, [Current | Rest], [Current | FilteredRest], Excluded) :-
    filter_closed(Cond, Rest, FilteredRest, Excluded).

insert_sorted(Elem, [], [Elem]) :- !.
insert_sorted([X, Y, D, Cost | ElemRest], [Current | Rest], [[X, Y, D, Cost | ElemRest], Current | Rest]) :-
    nth0(3, Current, Cost2),
    Cost < Cost2,
    !.
insert_sorted(Elem, [Current | Rest], [Current | Inserted]) :- insert_sorted(Elem, Rest, Inserted).

merge_sorted([], List, List).
merge_sorted([Elem | Rest], List, ListOut) :-
    insert_sorted(Elem, List, ListTmp),
    merge_sorted(Rest, ListTmp, ListOut).

combine_moves([], _, []).
combine_moves([Current | Rest], ExcludedMoves, [Combined | CombinedRest]) :-
    combine_moves(Rest, ExcludedMoves, CombinedRest),
    combine_move(Current, ExcludedMoves, Combined).
combine_move(Move, [], Move).
combine_move([X, Y, D, Cost, Path], [[X, Y, D, Cost, ExcludedPath] | Rest], [X, Y, D, Cost, MergedPath]) :-
    combine_move([X, Y, D, Cost, Path], Rest, [X, Y, D, Cost, PremergedPath]),
    append(ExcludedPath, PremergedPath, MergedPath_List),
    % Deduplicate the values in MergedPath:
    list_to_set(MergedPath_List, MergedPath),
    !.
combine_move(Move, [_ | Rest], MergedMove) :-
    combine_move(Move, Rest, MergedMove).

% === PATH FINDING ===

solve([[XEnd, YEnd, _, Cost, Explored] | _], _Maze, XEnd, YEnd, _Closed, Cost, Explored) :- !.
solve([Current | Opens], Maze, XEnd, YEnd, Closed, Cost, Explored) :-
    Current = [XPos, YPos, Dir | _],
    % Find the next moves:
    findall(T, can_move(Current, Maze, Closed, T), Moves),

    % Add (XPos, YPos, Dir) to Closed and remove the other entries in Opens:
    grid3_set(XPos, YPos, Dir, Closed, 1, NewClosed),
    filter_closed([XPos, YPos, Dir], Opens, OpensFiltered, Excluded),

    % Combine merging paths:
    findall(T, can_move_in(Excluded, Maze, Closed, T), ExcludedMoves),
    combine_moves(Moves, ExcludedMoves, CombinedMoves),

    % Add the moves to Opens, keeping the ordering intact
    merge_sorted(CombinedMoves, OpensFiltered, NextMoves),

    % La récursion~
    solve(NextMoves, Maze, XEnd, YEnd, NewClosed, Cost, Explored),
    !.
