:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- [library(dcg/basics)].

lines([]) --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

% eos([], []).

line([]) --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).

register(Name, I) --> "Register ", Name, ": ", integer(I).

integer_list([I | Rest]) --> integer(I), ",", integer_list(Rest), !.
integer_list([I]) --> integer(I).

program(Instructions) --> "Program: ", integer_list(Instructions).

read_problem([AInit, BInit, CInit, 0], Instructions) :-
    phrase_from_file(lines(Lines), "./input/day17-example.txt"),
    nth0(0, Lines, ALine),
    nth0(1, Lines, BLine),
    nth0(2, Lines, CLine),
    nth0(4, Lines, InstrLine),
    phrase(register("A", AInit), ALine),
    phrase(register("B", BInit), BLine),
    phrase(register("C", CInit), CLine),
    phrase(program(Instructions), InstrLine).

fold_instructions([], []).
fold_instructions([A, B | Rest], [[A, B] | FRest]) :-
    fold_instructions(Rest, FRest).

% === EXECUTION ===

execute([A, B, C, Pc], Instructions, OutputIn, OutputOut) :-
    nth0(Pc, Instructions, Instr),
    % debug(foo, "~d ~d ~d ~d", [A, B, C, Pc]),
    execute_single(Instr, [A, B, C, Pc], NextState, OutputIn, OutputTmp),
    execute(NextState, Instructions, OutputTmp, OutputOut).

execute(Regs, Instructions, Output, Output) :-
    Regs = [_, _, _, Pc],
    \+ nth0(Pc, Instructions, _),
    % debug(foo, "~d ~d ~d ~d", Regs),
    true.

print_output(Output) :-
    reverse(Output, OutputRev),
    print_output_(OutputRev).
print_output_([X]) :-
    format("~d", X),
    !.
print_output_([X | Rest]) :-
    format("~d,", X),
    print_output_(Rest),
    !.

read_combo(Combo, _Regs, Combo) :-
    Combo < 4.

read_combo(4, [A, _B, _C], A).
read_combo(5, [_A, B, _C], B).
read_combo(6, [_A, _B, C], C).

% adv(Combo): A <- A >> *Combo
execute_single([0, Combo], [A, B, C, Pc], NextState, Output, Output) :-
    read_combo(Combo, [A, B, C], Value),
    NextPc is Pc + 1,
    Shifted #= A >> Value,
    NextState = [Shifted, B, C, NextPc].

% bdv(Combo): B <- A >> *Combo
execute_single([6, Combo], [A, B, C, Pc], NextState, Output, Output) :-
    read_combo(Combo, [A, B, C], Value),
    NextPc is Pc + 1,
    Shifted #= A >> Value,
    NextState = [A, Shifted, C, NextPc].

% cdv(Combo): C <- A >> *Combo
execute_single([7, Combo], [A, B, C, Pc], NextState, Output, Output) :-
    read_combo(Combo, [A, B, C], Value),
    NextPc is Pc + 1,
    Shifted #= A >> Value,
    NextState = [A, B, Shifted, NextPc].

% bxl(Literal): B <- B xor Literal
execute_single([1, Literal], [A, B, C, Pc], NextState, Output, Output) :-
    NextPc is Pc + 1,
    Xored #= B xor Literal,
    NextState = [A, Xored, C, NextPc].

% bst(Combo): B <- *Combo
execute_single([2, Combo], [A, B, C, Pc], NextState, Output, Output) :-
    read_combo(Combo, [A, B, C], Value),
    NextPc is Pc + 1,
    Mod #= Value mod 8,
    NextState = [A, Mod, C, NextPc].

% jnz(Addr): if A > 0 then (Pc <- Addr) end
execute_single([3, _Addr], [0, B, C, Pc], [0, B, C, NextPc], Output, Output) :-
    NextPc is Pc + 1.
execute_single([3, Addr], [A, B, C, _Pc], [A, B, C, Addr], Output, Output) :-
    A #> 0.

% bxc(_): B <- B xor C
execute_single([4, _], [A, B, C, Pc], NextState, Output, Output) :-
    NextPc is Pc + 1,
    Mod #= B xor C,
    NextState = [A, Mod, C, NextPc].

% out(Combo): output ::= *Combo
execute_single([5, Combo], [A, B, C, Pc], [A, B, C, NextPc], Output, [Mod | Output]) :-
    read_combo(Combo, [A, B, C], Value),
    NextPc is Pc + 1,
    Mod #= Value mod 8.

% to3(0, [0, 0, 0]).
% to3(1, [1, 0, 0]).
% to3(2, [0, 1, 0]).
% to3(3, [1, 1, 0]).
% to3(4, [0, 0, 1]).
% to3(5, [0, 1, 1]).
% to3(6, [1, 0, 1]).
% to3(7, [1, 1, 1]).

% xor3(A, X, B) :-
%     to3(X, Y),
%     xor3_(A, Y, B).
% xor3_(A, [], A).
% xor3_(A, [0 | Rest], B) :-
%     A2 #= A // 2,
%     xor3_(A2, Rest, B2),
%     B #= B2 * 2 + (A mod 2).
% xor3_(A, [1 | Rest], B) :-
%     A2 #= A // 2,
%     xor3_(A2, Rest, B2),
%     B #= B2 * 2 + (1 - (A mod 2)).
