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

starting_pos(X, Y, Maze) :-
    grid_get(X, Y, Maze, 2),
    !.

ending_pos(X, Y, Maze) :-
    grid_get(X, Y, Maze, 3),
    !.

% ===

% get_dir(Dir, X, Y)
get_dir(0, 1, 0).
get_dir(1, 0, 1).
get_dir(2, -1, 0).
get_dir(3, 0, -1).

get_dir(Dir, XIn, YIn, XOut, YOut) :-
    get_dir(Dir, XDelta, YDelta),
    XOut is XIn + XDelta,
    YOut is YIn + YDelta.

solve_part1(Maze, Cost) :-
    starting_pos(XPos, YPos, Maze),
    ending_pos(XEnd, YEnd, Maze),
    fill_grid_with(Maze, Closed, [0, 0, 0, 0]),
    solve([[XPos, YPos, 0, 0]], Maze, XEnd, YEnd, Closed, Cost).

fill_grid_with([], [], _).
fill_grid_with([Line | Rest], [Zeroes | ZeroRest], Value) :-
    fill_line_with(Line, Zeroes, Value),
    fill_grid_with(Rest, ZeroRest, Value).
fill_line_with([], [], _).
fill_line_with([_ | Rest], [Value | Zeroes], Value) :- fill_line_with(Rest, Zeroes, Value).

% Go forward:
can_move([XPos, YPos, Dir, Cost], Maze, Closed, [X2, Y2, Dir, NewCost]) :-
    get_dir(Dir, XPos, YPos, X2, Y2),
    grid3_get(X2, Y2, Dir, Closed, 0),
    (grid_get(X2, Y2, Maze, 0) ; grid_get(X2, Y2, Maze, 3)),
    NewCost is Cost + 1.

% Turn right:
can_move([XPos, YPos, Dir, Cost], Maze, Closed, [XPos, YPos, NewDir, NewCost]) :-
    NewDir is (Dir + 1) mod 4,
    get_dir(NewDir, XPos, YPos, X2, Y2),
    grid3_get(X2, Y2, NewDir, Closed, 0),
    (grid_get(X2, Y2, Maze, 0) ; grid_get(X2, Y2, Maze, 3)),
    NewCost is Cost + 1000.

% Turn left:
can_move([XPos, YPos, Dir, Cost], Maze, Closed, [XPos, YPos, NewDir, NewCost]) :-
    NewDir is (Dir + 3) mod 4,
    get_dir(NewDir, XPos, YPos, X2, Y2),
    grid3_get(X2, Y2, NewDir, Closed, 0),
    (grid_get(X2, Y2, Maze, 0) ; grid_get(X2, Y2, Maze, 3)),
    NewCost is Cost + 1000.

filter_closed(_, [], []) :- !.
filter_closed([X, Y, D], [[X, Y, D | _] | Rest], Rest) :- !.
filter_closed(Cond, [Current | Rest], [Current | FilteredRest]) :- filter_closed(Cond, Rest, FilteredRest).

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

solve([[XEnd, YEnd, _, Cost] | _], _Maze, XEnd, YEnd, _Closed, Cost) :- !.
solve([Current | Opens], Maze, XEnd, YEnd, Closed, Cost) :-
    Current = [XPos, YPos, Dir | _],
    % Find the next moves:
    findall(T, can_move(Current, Maze, Closed, T), Moves),

    % Add (XPos, YPos, Dir) to Closed and remove the other entries in Opens:
    grid3_set(XPos, YPos, Dir, Closed, 1, NewClosed),
    filter_closed([XPos, YPos, Dir], Opens, OpensFiltered),

    merge_sorted(Moves, OpensFiltered, NextMoves),

    % For debugging:
    % length(NextMoves, Len),
    % debug(foo, "~d", [Len]),

    solve(NextMoves, Maze, XEnd, YEnd, NewClosed, Cost).
