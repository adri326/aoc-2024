:- use_module(library(pio)).
:- use_module(library(ugraphs)).
:- use_module(library(lists)).
:- [library(dcg/basics)].

part1(Path, OutNum) :-
    read_problem(Path, Init, Gates),
    execute(Init, Gates, OutPairs),
    matching_values("z", Gates, ZNames),
    get_values(ZNames, OutPairs, ZValues),
    !,
    little_endian(ZValues, OutNum).

little_endian([], 0).
little_endian([Value | Rest], OutNum) :-
    little_endian(Rest, RestNum),
    OutNum is RestNum * 2 + Value.

matching_values(Prefix, Gates, Values) :-
    gate_graph(Gates, Graph),
    vertices(Graph, AllValues),
    matching_values_(Prefix, AllValues, UnsortedValues),
    msort(UnsortedValues, Values).

matching_values_(_, [], []).
matching_values_(Prefix, [Name | Rest], [Name | RestOut]) :-
    atom_concat(Prefix, _, Name),
    matching_values_(Prefix, Rest, RestOut).
matching_values_(Prefix, [Name | Rest], RestOut) :-
    \+ atom_concat(Prefix, _, Name),
    matching_values_(Prefix, Rest, RestOut).

get_values([], _, []).
get_values([Name | RestName], Pairs, [Value | RestValues]) :-
    member([Name, Value], Pairs),
    get_values(RestName, Pairs, RestValues).

part2(Path, Out) :-
    read_problem(Path, _, Gates),
    matching_values(x, Gates, Xs),
    matching_values(y, Gates, Ys),
    matching_values(z, Gates, Zs),
    solve_errors(Gates, Xs, Ys, Zs, 4, Pairs),
    swap_gates(Pairs, Gates, Swapped),
    find_error(Swapped, Xs, Ys, Zs, []),
    !,
    append(Pairs, Joined),
    msort(Joined, Out).

% === Parsing ===

lines([]) --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

line([]) --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).

read_problem(Path, Values, Gates) :-
    phrase_from_file(lines(Lines), Path),
    parse_lines(Lines, Values, Gates),
    !.

parse_lines([[] | InstructionLines], [], Gates) :-
    parse_lines(InstructionLines, Gates).
parse_lines([Line | Rest], [Value | RestValues], Gates) :-
    \+ Line = "",
    phrase(initial_value(Name, V), Line),
    Value = [Name, V],
    parse_lines(Rest, RestValues, Gates).

parse_lines([], []).
parse_lines([Line | Rest], [CurrentGate | RestGates]) :-
    phrase(gate(CurrentGate), Line),
    parse_lines(Rest, RestGates).

is_alphanumeric(X) :- X >= 0'a, X =< 0'z.
is_alphanumeric(X) :- X >= 0'0, X =< 0'9.

var_name(Name) --> var_name_(NameCodes), {atom_codes(Name, NameCodes)}.
var_name_([X | Rest]) --> [X], {is_alphanumeric(X)}, var_name_(Rest).
var_name_([]) --> {true}.
initial_value(Name, Value) --> var_name(Name), ": ", integer(Value).
gate(and(Lhs,Rhs,Res)) --> var_name(Lhs), " AND ", var_name(Rhs), " -> ", var_name(Res).
gate(or(Lhs,Rhs,Res))  --> var_name(Lhs), " OR ",  var_name(Rhs), " -> ", var_name(Res).
gate(xor(Lhs,Rhs,Res)) --> var_name(Lhs), " XOR ", var_name(Rhs), " -> ", var_name(Res).

% === Gate operations ===

split_gate(and(Lhs, Rhs, Out), and, Lhs, Rhs, Out).
split_gate(or(Lhs, Rhs, Out), or, Lhs, Rhs, Out).
split_gate(xor(Lhs, Rhs, Out), xor, Lhs, Rhs, Out).

gate_graph([], []).
gate_graph([Gate | Rest], Graph) :-
    gate_graph(Rest, RestGraph),
    split_gate(Gate, _, Lhs, Rhs, Out),
    add_edges(RestGraph, [Lhs-Out, Rhs-Out], Graph).

find_gate(GateOut, [Current | _], Current) :-
    split_gate(Current, _, _, _, GateOut).
find_gate(GateOut, [_ | Rest], Found) :-
    find_gate(GateOut, Rest, Found).

% === Execution ===

execute(ValuesIn, Gates, ValuesOut) :-
    gate_graph(Gates, GateGraph),
    top_sort(GateGraph, ExecutionOrder),
    execute_(ExecutionOrder, Gates, ValuesIn, ValuesOut).

execute_([], _, Values, Values).
execute_([OutName | Rest], Gates, ValuesIn, ValuesOut) :-
    find_gate(OutName, Gates, Gate),
    \+ member([OutName, _], ValuesIn),
    execute_gate(Gate, ValuesIn, OutValue),
    execute_(Rest, Gates, [OutValue | ValuesIn], ValuesOut).
execute_([OutName | Rest], Gates, ValuesIn, ValuesOut) :-
    member([OutName, _], ValuesIn),
    \+ find_gate(OutName, Gates, _),
    execute_(Rest, Gates, ValuesIn, ValuesOut).

execute_gate(and(Lhs, Rhs, OutName), ValuesIn, [OutName, Output]) :-
    member([Lhs, LhsValue], ValuesIn),
    member([Rhs, RhsValue], ValuesIn),
    Output is LhsValue /\ RhsValue.

execute_gate(or(Lhs, Rhs, OutName), ValuesIn, [OutName, Output]) :-
    member([Lhs, LhsValue], ValuesIn),
    member([Rhs, RhsValue], ValuesIn),
    Output is LhsValue \/ RhsValue.

execute_gate(xor(Lhs, Rhs, OutName), ValuesIn, [OutName, Output]) :-
    member([Lhs, LhsValue], ValuesIn),
    member([Rhs, RhsValue], ValuesIn),
    Output is LhsValue xor RhsValue.

% === Swapping ===

swap_gates([], Gates, Gates).
swap_gates([[Fst, Snd] | Rest], Gates, OutGates) :-
    swap_gates(Rest, Gates, TmpGates),

    find_gate(Fst, Gates, FstGate),
    find_gate(Snd, Gates, SndGate),
    split_gate(FstGate, FstOp, FstLhs, FstRhs, Fst),
    split_gate(SndGate, SndOp, SndLhs, SndRhs, Snd),

    split_gate(NewFstGate, FstOp, FstLhs, FstRhs, Snd),
    split_gate(NewSndGate, SndOp, SndLhs, SndRhs, Fst),

    select(FstGate, TmpGates, NewFstGate, TmpGates2),
    select(SndGate, TmpGates2, NewSndGate, OutGates).

% === Error correction ===

solve_errors(Gates, Xs, Ys, Zs, 0, []) :-
    find_error(Gates, Xs, Ys, Zs, []),
    !.

solve_errors(Gates, Xs, Ys, Zs, N, [Pair | Rest]) :-
    N > 0,
    N1 is N - 1,
    find_error(Gates, Xs, Ys, Zs, Pair),
    swap_gates([Pair], Gates, Swapped),
    solve_errors(Swapped, Xs, Ys, Zs, N1, Rest).

gate_member(Gate, Gates) :- member(Gate, Gates).
gate_member(Gate, Gates) :-
    split_gate(Gate, Op, Lhs, Rhs, Out),
    split_gate(Swapped, Op, Rhs, Lhs, Out),
    member(Swapped, Gates).

match_half_adder(Gates, X, Y, CarryOut, SumOut) :-
    gate_member(and(X, Y, CarryOut), Gates),
    gate_member(xor(X, Y, SumOut), Gates).

match_full_adder(Gates, X, Y, CarryIn, CarryOut, Z) :-
    match_half_adder(Gates, X, Y, Carry, Sum),
    gate_member(xor(Sum, CarryIn, Z), Gates),
    gate_member(and(CarryIn, Sum, CarrySem), Gates),
    gate_member(or(CarrySem, Carry, CarryOut), Gates).

find_error(Gates, [X | Xs], [Y | Ys], [Z | Zs], Error) :-
    match_half_adder(Gates, X, Y, COut, Z),
    find_error(Gates, COut, Xs, Ys, Zs, Error).

find_error(_, _, [], [], _, []).
find_error(Gates, CIn, [X | Xs], [Y | Ys], [Z | Zs], Error) :-
    match_full_adder(Gates, X, Y, CIn, COut, Z),
    find_error(Gates, COut, Xs, Ys, Zs, Error).

find_error(Gates, CIn, [X | _], [Y | _], [Z | _], [Fst, Snd]) :-
    \+ match_full_adder(Gates, X, Y, CIn, _, Z),
    match_half_adder(Gates, X, Y, PC, PS),
    gate_member(xor(_, CIn, PZ), Gates),
    gate_member(and(PCIn, _, PCSem), Gates),
    (gate_member(or(PCSem, _, PCOut), Gates); gate_member(or(PC, _, PCOut), Gates)),
    Involved = [PC, PS, PZ, PCIn, PCSem, PCOut],
    nth0(FstIndex, Involved, Fst),
    nth0(SndIndex, Involved, Snd),
    FstIndex < SndIndex,
    swap_gates([[Fst, Snd]], Gates, Swapped),
    match_full_adder(Swapped, X, Y, CIn, COut, Z),
    gate_member(xor(COut, _, _), Swapped).
