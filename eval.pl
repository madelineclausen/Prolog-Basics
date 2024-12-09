:- use_module(library(prolog_coverage)).

eval(E, E) :- integer(E).
eval(E1 + E2, V) :- eval(E1, V1), eval(E2, V2), V is V1 + V2.
eval(E1 - E2, V) :- eval(E1, V1), eval(E2, V2), V is V1 - V2.
eval(E1 * E2, V) :- eval(E1, V1), eval(E2, V2), V is V1 * V2.
eval(E1 / E2, V) :- eval(E1, V1), eval(E2, V2), V2 =\= 0, V is V1 / V2.
eval(E1 ^ E2, V) :- eval(E1, V1), eval(E2, V2), V is V1 ^ V2.

simplify(E, E) :- atom(E); number(E).
simplify(E * 0, 0).
simplify(0 * E, 0).
simplify(0 * 0, 0).
simplify(E * 1, E).
simplify(1 * E, E).
simplify(E / 1, E).
simplify(E / E, 1) :- \+ number(E), E \= 0.
simplify(0 / E, 0) :- \+ number(E), E \= 0.
simplify(E / 0, 0) :- \+ number(E), E \= 0.
simplify(E + 0, E).
simplify(0 + E, E).
simplify(E - 0, E).
simplify(E + S, R) :- simplify(E, E2), simplify(S, S2), (number(E2), number(S2) -> R is E2 + S2; (S2 = 1 -> R = E2; R = E2 + S2)).
simplify(E * S, R) :- simplify(E, E2), simplify(S, S2), (number(E2), number(S2) -> R is E2 * S2; (S2 = 1 -> R = E2; R = E2 * S2)).
simplify(E - S, R) :- simplify(E, E2), simplify(S, S2), (number(E2), number(S2) -> R is E2 - S2; (S2 == 0 -> R = E2; R = E2 - S2)).
simplify(E / S, R) :- simplify(E, E2), simplify(S, S2), (number(E2), number(S2), S2 =\= 0 -> R is E2 / S2; (S2 = 1 -> R = E2; R = E2 / S2)).

deriv(E, Y) :- deriv_helper(E, Y).
deriv_helper(E, D) :- simplify(E, D).
deriv_helper(E, 0) :- number(E), !.
deriv_helper(E, 1) :- var(E), !.
deriv_helper(-E, -1).
deriv_helper(E^0, 0).
deriv_helper(E^1, 1).
deriv_helper(E^2, 2*E).
deriv_helper(C*E, C) :- number(C).
deriv_helper(E*C, C) :- number(C).
deriv_helper(E^N, D) :- Y is N - 1, (N > 0 -> D = N*E^Y ; Y1 is -1 * N, D = N/E^Y1).
deriv_helper(E1 * E^N, D) :- D1 is E1 * N, T is N-1, (T = 1 -> D2 = E ; D2 = E^T), D = D1 * D2.
deriv_helper(E1*E2*E3, D) :- deriv_helper(E1, D1), deriv_helper(E2*E3, D2), simplify(D1 * (E2*E3) + D2 * E1, D).
deriv_helper(E1*E2, D) :- deriv_helper(E1, D1), deriv_helper(E2, D2), simplify(D1*E2+D2*E1, D), !.
deriv_helper(E1+E2, D) :- deriv_helper(E1, D1), deriv_helper(E2, D2), simplify(D1 + D2, D), !.
deriv_helper(E1-E2, D) :- deriv_helper(E1, D1), deriv_helper(E2, D2), simplify(D1 - D2, D), !.
deriv_helper(E1+E2+E3, D) :- deriv_helper(E1, D1), deriv_helper(E2, D2), deriv_helper(E3, D3), simplify(D1+D2+D3, D).
deriv_helper(E1+E2-E3+E4-E5, D) :- deriv_helper(E1, D1), deriv_helper(E2, D2), deriv_helper(E3, D3), deriv_helper(E4, D4), deriv_helper(E5, D5), simplify(D1 + D2 - D3 + D4 - D5, D).
deriv_helper(E1/E2, D) :- deriv_helper(E1, D1), deriv_helper(E2, D2), simplify((D1*E2-E1*D2)/(E2^2), D), !.
deriv_helper(-(E), D) :- deriv_helper((-1)*E, D).


party_seating(L) :- people(People), permutation(People, L), party_seating_helper_list(L), party_seating_helper_pairs(L).
people([_, _, _, _, _, _, _, _, _, _]).
party_seating_helper_pairs([_]).
party_seating_helper_pairs([H1,H2|T]) :- (male(H1), female(H2) ; female(H1), male(H2)), party_seating_helper_pairs([H2|T]).
party_seating_helper_list([_]).
party_seating_helper_list([H1, H2 | T]) :- speaks(H1, L), speaks(H2, L), party_seating_helper_list([H2 | T]).

?- simplify(5+2*6+x, Y).
?- show_coverage.