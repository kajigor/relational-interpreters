:- module prop.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module char, list, bool, int, string, solutions.

:- type myBool ---> true; false.

:- type prop ---> conj(prop, prop)
                ; disj(prop, prop)
                ; lit(myBool).

:- func propToString(prop) = string.
:- func boolToString(myBool) = string.

:- pred myAnd(myBool, myBool, myBool).
:- mode myAnd(in, in, out) is det.
:- mode myAnd(out, out, in) is multi.

:- pred myOr(myBool, myBool, myBool).
:- mode myOr(in, in, out) is det.
:- mode myOr(out, out, in) is multi.
%:- pred evaluate(prop::in, bool::out) is det.
%:- pred evaluate(prop::out, bool::in) is multi.
:- pred evaluate(prop, myBool).
:- mode evaluate(out, in) is multi.

boolToString(true) = "true".
boolToString(false) = "false".

propToString(lit(B)) = boolToString(B).
propToString(conj(X, Y)) = S :-
  Sx = propToString(X),
  Sy = propToString(Y),
  S = "conj(" ++ Sx ++ ", " ++ Sy ++ ")".
propToString(disj(X, Y)) = S :-
  Sx = propToString(X),
  Sy = propToString(Y),
  S = "disj(" ++ Sx ++ ", " ++ Sy ++ ")".

myAnd(true, true, true).
myAnd(false, true, false).
myAnd(true, false, false).
myAnd(false, false, false).

myOr(false, false, false).
myOr(false, true, true).
myOr(true, false, true).
myOr(true, true, true).

evaluate(lit(true), true).
evaluate(lit(false), false).
evaluate(conj(X, Y), R) :-
  evaluate(X, Rx),
  evaluate(Y, Ry),
  myAnd(Rx, Ry, R).
evaluate(disj(X, Y), R) :-
  evaluate(X, Rx),
  evaluate(Y, Ry),
  myOr(Rx, Ry, R).

:- pred evaluatesToTrue(prop::out) is multi.
evaluatesToTrue(X) :- evaluate(X, true).

:- pred tiny(int, prop, bool, io, io).
:- mode tiny(in, in, out, di, uo) is det.
tiny(Limit, Prop, More, !IO) :-
  More = (if size(Prop) = Limit then no else yes),
  io.print_line(propToString(Prop), !IO).

:- func size(prop) = int.
size(lit(_)) = 1.
size(disj(X,Y)) = S :-
  Sx = size(X),
  Sy = size(Y),
  S = Sx + Sy.
size(conj(X,Y)) = S :-
  Sx = size(X),
  Sy = size(Y),
  S = Sx + Sy.


main(!IO) :-
 % Prop = conj(lit(true), disj(lit(false), lit(false))),
 % propToString(Prop, Str),

 % evaluate(Prop, Res),
 % boolToString(Res, StrRes),

 % io.format("Prop:\t%s\n", [s(Str)], !IO),
 % io.format("Result:\t%s\n", [s(StrRes)], !IO),
  do_while(evaluatesToTrue, tiny(5), !IO).
  % evaluate(Prop1, true),
  % propToString(Prop1, StrProp),
  % io.format("EvaluateTotrue:\n%s\n", [s(stringify_list(map(propToString, Results)))], !IO).






% :- func stringify_list(list(string)) = string.

% stringify_list(L) = format("[%s]", [s(stringify_list_1(L))]).

% :- func stringify_list_1(list(string)) = string.

% stringify_list_1([]) = "".
% stringify_list_1([Last]) = format("\"%s\"", [s(Last)]).
% stringify_list_1([First, Second | T]) = S :-
%   FirstString = format("\"%s\", ", [s(First)]),
%   RestString = stringify_list_1([Second | T]),
%   append(FirstString, RestString, S).
