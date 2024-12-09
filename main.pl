my_length([], 0).
my_length([LHead | LTail], R) :- my_length(LTail, R1), R is R1 + 1.

my_member(A, [A | LTail]).
my_member(A, [LHead | LTail]) :- my_member(A, LTail).

my_append([], L, L).
my_append([LHead | LTail1], L2, [LHead | R]) :- my_append(LTail1, L2, R).

my_reverse(L, R) :- my_reverse_helper(L, [], R).
my_reverse_helper([], R, R).
my_reverse_helper([LHead | LTail], L2, R) :- my_reverse_helper(LTail, [LHead | L2], R).

my_nth(L, 1, L).
my_nth([LHead | LTail], N, R) :- N > 1, N1 is N - 1, my_nth(LTail, N1, R).

my_remove(X, [], []).
my_remove(X, [X | LTail], R) :- my_remove(X, LTail, R).
my_remove(X, [LHead | LTail], [LHead | R]) :- dif(X, LHead), my_remove(X, LTail, R).

my_subst(X, Y, [], []).
my_subst(X, Y, [X | LTail], [Y | R]) :- my_subst(X, Y, LTail, R).
my_subst(X, Y, [LHead | LTail], [LHead | R]) :- dif(X, LHead), my_subst(X, Y, LTail, R).

my_subset(P, [], []).
my_subset(P, [X | LTail], [X | R]) :- call(P, X), !, my_subset(P, LTail, R).
my_subset(P, [X | LTail], R) :- my_subset(P, LTail, R).

my_add(N1, N2, R) :- my_add(N1, N2, 0, R).
my_add([], [], 1, [1]).
my_add(N1, [], C, R) :- my_add_helper(N1, 0, C, R).
my_add([], N2, C, R) :- my_add_helper(N2, 0, C, R).
my_add([N1 | LTail1], [N2 | LTail2], C, [R | RTail]) :- my_sum_calc(N1, N2, C, R, NewC), my_add(LTail1, LTail2, NewC, RTail).
my_sum_calc(N1, N2, CIn, R, COut) :- Total is N1 + N2 + CIn, R is Total mod 10, COut is Total // 10.
my_add_helper([], 0, 0, []).
my_add_helper([], 0, 1, [1]).
my_add_helper([], C, 0, [C]).
my_add_helper([], C, 1, [R]) :- R is C.
my_add_helper([N | LTail], 0, C, [R | RTail]) :- my_sum_calc(N, 0, C, R, NewC), my_add_helper(LTail, 0, NewC, RTail).
my_add_helper([N | LTail], C, 0, [R | RTail]) :- my_sum_calc(N, 0, C, R, NewC), my_add_helper(LTail, 0, NewC, RTail).
my_add_helper([N | LTail], C, 1, [R | RTail]) :- my_sum_calc(N, 0, C, R, NewC), my_add_helper(LTail, 0, NewC, RTail).

my_merge([], L2, L2).
my_merge(L1, [], L1).
my_merge([X | LTail1], [Y | LTail2], [X | RTail]) :- X =< Y, my_merge(LTail1, [Y | LTail2], RTail).
my_merge([X | LTail1], [Y | LTail2], [Y | RTail]) :- X > Y, my_merge([X | LTail1], LTail2, RTail).

my_sublist([], _L2).
my_sublist([X | LTail1], [X | LTail2]) :- my_sublist_helper(LTail1, LTail2).
my_sublist(L, [X | LTail2]) :- my_sublist(L, LTail2).

my_sublist_helper([], L2).
my_sublist_helper([X | LTail1], [X | LTail2]) :- my_sublist_helper(LTail1, LTail2).

my_assoc(K, [], V) :- fail.
my_assoc(K, [K, V | LTail], V).
my_assoc(K, [K2 | LTail], V) :- dif(K, K2), my_assoc(K, LTail, V). 
my_assoc(K, [Head | LTail], V) :- my_assoc(K, LTail, V).

my_replace([], L, []).
my_replace([X | LTail], L, [Y | R]) :- my_replace_helper(X, L, Y), my_replace(LTail, L, R).
my_replace_helper(X, [LHead | LTail], V).
my_replace_helper(X, [LHead | LTail], Y) :- my_replace_helper(X, LTail, Y).

