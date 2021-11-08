:- module sorting.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int, string, solutions.

:- pred minMax(int, int, int, int).
:- mode minMax(in, in, out, out) is nondet.
:- mode minMax(out, out, in, in) is nondet.
minMax(X, Y, Min, Max) :-
  X < Y, Min = X, Max = Y ;
  X >= Y, Max = X, Min = Y.

:- pred smallest(list(int), int, list(int)).
:- mode smallest(in, out, out) is nondet.
:- mode smallest(out, in, in) is nondet.
smallest([X], X, []).
smallest([H|T], S, L) :-
  L = [Max|L0],
  minMax(H, S0, S, Max),
  smallest(T, S0, L0).

:- pred sorto(list(int), list(int)).
:- mode sorto(in, out) is nondet.
:- mode sorto(out, in) is nondet.
sorto([], []).
sorto(Xs, Sorted) :-
  Sorted = [Min | SortedRest],
  smallest(Xs, Min, Rest),
  sorto(Rest, SortedRest).

:- pred permutations(list(int), list(int)).
:- mode permutations(in, out) is nondet.
permutations(Xs, Perm) :-
  sorto(Xs, Sorted),
  sorto(Perm, Sorted).

main(!IO) :-
  printSorted([1,2,3], !IO),
  printSorted([1,3,2], !IO),
  printSorted([3,2,1], !IO),
  printSorted([1,1,1], !IO),
  printSorted([9,8,7,6,5,4,3,2,1], !IO),
  printPerms([1,2,3], !IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Printers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred printEachList(string::in, list(list(int))::in, io::di, io::uo) is det.
printEachList(_, [], !IO).
printEachList(Name, [H|T], !IO) :-
  io.format( "%s:\t%s\n"
           , [s(Name), s(listToString(H))]
           , !IO),
  printEachList(Name, T, !IO).

% % This implementation leads to a mode error: there are two modes
% % which match the sorto(Xs) call.
% :- pred printSorted(list(int)::in, io::di, io::uo) is det.
% printSorted(Xs, !IO) :-
%   io.format("\nList:\t%s\n", [s(listToString(Xs))], !IO),
%   solutions(sorto(Xs), AllSolutions),
%   printEachList("Sorted", AllSolutions, !IO).

% % We can replace sorto(Xs) call to the call of the predicate
% % with only one mode sortoInOut(Xs).
% :- pred sortoInOut(list(int), list(int)).
% :- mode sortoInOut(in, out) is nondet.
% sortoInOut(Xs, Sorted) :- sorto(Xs, Sorted).

% :- pred printSorted(list(int)::in, io::di, io::uo) is det.
% printSorted(Xs, !IO) :-
%   io.format("\nList:\t%s\n", [s(listToString(Xs))], !IO),
%   solutions(sortoInOut(Xs), AllSolutions), % This fixes things
%   printEachList("Sorted", AllSolutions, !IO).

% This one seems to be the least awkward one.
% It specifies the suitable mode within a partial application.
:- pred printSorted(list(int)::in, io::di, io::uo) is det.
printSorted(Xs, !IO) :-
  io.format("\nList:\t%s\n", [s(listToString(Xs))], !IO),
  solutions( (pred(Sorted::out) is nondet :- sorto(Xs, Sorted))
           , AllSolutions),
  printEachList("Sorted", AllSolutions, !IO).


:- pred printPerms(list(int)::in, io::di, io::uo) is det.
printPerms(Xs, !IO) :-
  io.format("\nList:\t%s\n", [s(listToString(Xs))], !IO),
  solutions(permutations(Xs), AllSolutions),
  printEachList("Permutation", AllSolutions, !IO).

:- func listToString(list(int)) = string.
listToString(Xs) = join_list(", ", map(int_to_string,Xs)).
