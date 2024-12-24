:- use_module(library(pio)).
:- use_module(library(ugraphs)).
:- [library(dcg/basics)].

part1(Path, OutNum) :-
    read_problem(Path, Init, Gates),
    execute(Init, Gates, OutPairs),
    get_z_pairs(OutPairs, ZPairs),
    !,
    msort(ZPairs, ZPairsSorted),
    accumulate_zpairs(ZPairsSorted, OutNum).

accumulate_zpairs([], 0).
accumulate_zpairs([[_, Value] | Rest], OutNum) :-
    accumulate_zpairs(Rest, RestNum),
    OutNum is RestNum * 2 + Value.

get_z_pairs([], []).
get_z_pairs([[Name, Value] | Rest], [[Name, Value] | RestOut]) :-
    atom_concat("z", _, Name),
    get_z_pairs(Rest, RestOut).
get_z_pairs([[Name, _] | Rest], RestOut) :-
    \+ atom_concat("z", _, Name),
    get_z_pairs(Rest, RestOut).

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

split_gate(and(Lhs, Rhs, Out), Lhs, Rhs, Out).
split_gate(or(Lhs, Rhs, Out), Lhs, Rhs, Out).
split_gate(xor(Lhs, Rhs, Out), Lhs, Rhs, Out).

gate_graph([], []).
gate_graph([Gate | Rest], Graph) :-
    gate_graph(Rest, RestGraph),
    split_gate(Gate, Lhs, Rhs, Out),
    add_edges(RestGraph, [Lhs-Out, Rhs-Out], Graph).

find_gate(GateOut, [Current | Rest], Found) :-
    split_gate(Current, _, _, CurrentOut),
    (
        GateOut = CurrentOut, Found = Current;
        \+ GateOut = CurrentOut, find_gate(GateOut, Rest, Found)
    ).

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
