:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- [library(dcg/basics)].

% Fuel is a value to limit the recursion depth during the backward search. A value of 1000 is good enough.
solve_part1(Out) :- solve_part1(1000, Out).
solve_part1(Fuel, Out) :-
    read_problem(Regs, I),
    fold_instructions(I, IFolded),
    execute(Regs, Fuel, IFolded, OutRev, _),
    print_output(OutRev),
    reverse(OutRev, Out),
    !.

solve_part2(A) :- solve_part2(1000, 10000000000000000, A).

% `Bound` is the maximum value that registers may have during execution.
% A value of 1000000000000000 should be good enough
solve_part2(Fuel, Bound, A) :-
    read_problem([_A, B, C, Pc], I),
    fold_instructions(I, I2),
    reverse(I, IInv),
    execute([A, B, C, Pc], Fuel, I2, IInv, Residuals),
    Residuals ins 0..Bound,
    % Kindly ask clpfd to find solutions. It focuses on the values with the smallest
    % domains first.
    labeling([ff], Residuals),
    % Check each solution
    execute([A, B, C, Pc], Fuel, I2, IInv, _).

% === PARSING ===

lines([]) --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

line([]) --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).

register(Name, I) --> "Register ", Name, ": ", integer(I).

integer_list([I | Rest]) --> integer(I), ",", integer_list(Rest), !.
integer_list([I]) --> integer(I).

program(Instructions) --> "Program: ", integer_list(Instructions).

read_problem([AInit, BInit, CInit, 0], Instructions) :-
    read_problem("./input/day17.txt", [AInit, BInit, CInit, 0], Instructions).

read_problem(Path, [AInit, BInit, CInit, 0], Instructions) :-
    phrase_from_file(lines(Lines), Path),
    nth0(0, Lines, ALine),
    nth0(1, Lines, BLine),
    nth0(2, Lines, CLine),
    nth0(4, Lines, InstrLine),
    phrase(register("A", AInit), ALine),
    phrase(register("B", BInit), BLine),
    phrase(register("C", CInit), CLine),
    phrase(program(Instructions), InstrLine).

% === TRANSLATION ===

% Groups instructions into [operation, operand] pairs
fold_instructions([], []).
fold_instructions([A, B | Rest], [[A, B] | FRest]) :-
    fold_instructions(Rest, FRest).

% Prints the output in the format requested by AoC
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

% === EXECUTION ===

% Executes instructions until the fuel runs out or the end of the program is reached.
% If the fuel runs out, then the program will output -1.
%
% ResidualsOut is populated with the temporary values of registers that get overwritten,
% for use in part 2.
execute(Regs, Fuel, Instructions, OutputOut, ResidualsOut) :-
    execute(Regs, Fuel, Instructions, [], OutputOut, [], ResidualsOut).

execute([A, B, C, Pc], Fuel, Instructions, OutputIn, OutputOut, ResidualsIn, ResidualsOut) :-
    Fuel > 0,
    nth0(Pc, Instructions, Instr),
    execute_single(Instr, [A, B, C, Pc], NextState, OutputIn, OutputTmp, ResidualsIn, ResidualsTmp),
    NextFuel is Fuel - 1,
    execute(NextState, NextFuel, Instructions, OutputTmp, OutputOut, ResidualsTmp, ResidualsOut).

execute(Regs, _Fuel, Instructions, Output, Output, Residuals, Residuals) :-
    Regs = [_, _, _, Pc],
    \+ nth0(Pc, Instructions, _),
    true.

execute(_, 0, _, Output, [-1 | Output], Residuals, Residuals).

read_combo(Combo, _Regs, Combo) :- Combo < 4.
read_combo(4, [A, _B, _C], A).
read_combo(5, [_A, B, _C], B).
read_combo(6, [_A, _B, C], C).

% adv(Combo): A <- A >> *Combo
execute_single(
    [0, Combo],
    [A, B, C, Pc],
    NextState,
    Output,
    Output,
    Residuals,
    [A | Residuals]
) :-
    read_combo(Combo, [A, B, C], Value),
    NextPc is Pc + 1,
    Shifted #= A >> Value,
    NextState = [Shifted, B, C, NextPc].

% bdv(Combo): B <- A >> *Combo
execute_single(
    [6, Combo],
    [A, B, C, Pc],
    NextState,
    Output,
    Output,
    Residuals,
    [B | Residuals]
) :-
    read_combo(Combo, [A, B, C], Value),
    NextPc is Pc + 1,
    Shifted #= A >> Value,
    NextState = [A, Shifted, C, NextPc].

% cdv(Combo): C <- A >> *Combo
execute_single(
    [7, Combo],
    [A, B, C, Pc],
    NextState,
    Output,
    Output,
    Residuals,
    [C | Residuals]
) :-
    read_combo(Combo, [A, B, C], Value),
    NextPc is Pc + 1,
    Shifted #= A >> Value,
    NextState = [A, B, Shifted, NextPc].

% bxl(Literal): B <- B xor Literal
execute_single([1, Literal], [A, B, C, Pc], NextState, Output, Output, R, R) :-
    NextPc is Pc + 1,
    Xored #= B xor Literal,
    NextState = [A, Xored, C, NextPc].

% bst(Combo): B <- *Combo
execute_single(
    [2, Combo],
    [A, B, C, Pc],
    NextState,
    Output,
    Output,
    Residuals,
    [B | Residuals]
) :-
    read_combo(Combo, [A, B, C], Value),
    NextPc is Pc + 1,
    Mod #= Value mod 8,
    NextState = [A, Mod, C, NextPc].

% jnz(Addr): if A > 0 then (Pc <- Addr) end
execute_single([3, _Addr], [0, B, C, Pc], [0, B, C, NextPc], Output, Output, R, R) :-
    NextPc is Pc + 1.
execute_single([3, Addr], [A, B, C, _Pc], [A, B, C, Addr], Output, Output, R, R) :-
    A #> 0.

% bxc(_): B <- B xor C
execute_single([4, _], [A, B, C, Pc], NextState, Output, Output, R, R) :-
    NextPc is Pc + 1,
    Mod #= B xor C,
    NextState = [A, Mod, C, NextPc].

% out(Combo): output ::= *Combo
execute_single([5, Combo], [A, B, C, Pc], [A, B, C, NextPc], Output, [Mod | Output], R, R) :-
    read_combo(Combo, [A, B, C], Value),
    NextPc is Pc + 1,
    Mod #= Value mod 8.
