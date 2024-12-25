:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- [library(dcg/basics)].

part1(Path, Count) :-
    read_problem(Path, Keys, Locks),
    findall(Pair, (
        Pair = [X, Y],
        member(X, Keys),
        member(Y, Locks),
        fits(X, Y)
    ), Pairs),
    length(Pairs, Count).

fits([], []).
fits([Key | KeyRest], [Lock | LockRest]) :-
    fits(KeyRest, LockRest),
    Key >= Lock.

% === Parsing ===

read_problem(Path, Keys, Locks) :-
    phrase_from_file(lines(Lines), Path),
    group_by([], Lines, Groups),
    parse_groups(parse_key, 46, Groups, Keys),
    parse_groups(parse_lock, 35, Groups, Locks),
    !.

parse_groups(_, _, [], []).
parse_groups(Parser, FirstChar, [Group | Rest], [Out | RestOut]) :-
    Group = [[FirstChar | _] | _],
    transpose(Group, Columns),
    call(Parser, Columns, Out),
    parse_groups(Parser, FirstChar, Rest, RestOut).
parse_groups(Parser, FirstChar, [[[F | _] | _] | Rest], RestOut) :-
    F \= FirstChar,
    parse_groups(Parser, FirstChar, Rest, RestOut).


parse_key([], []).
parse_key([Column | Rest], [N | Key]) :-
    parse_key(Rest, Key),
    (nth0(N, Column, 35), !).

parse_lock([], []).
parse_lock([Column | Rest], [N | Key]) :-
    parse_lock(Rest, Key),
    (nth0(N, Column, 46), !).

group_by(_, [], [[]]).
group_by(X, [X | Rest], [[] | GroupedRest]) :-
    group_by(X, Rest, GroupedRest).
group_by(X, [Y | Rest], [[Y | Group] | GroupedRest]) :-
    Y \= X,
    group_by(X, Rest, [Group | GroupedRest]).

lines([]) --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

line([]) --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).
