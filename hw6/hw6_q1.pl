bt(_, []).
bt(Value, Children) :-
    length(Children, K),
    check_binomial_tree(Value, Children, K).

check_binomial_tree(_, [], 0).
check_binomial_tree(Value, [bt(ChildValue, ChildChildren) | RestChildren], K) :-
    K > 0,
    Value < ChildValue,      
    K1 is K - 1,                                
    check_binomial_tree(ChildValue, ChildChildren, K1), 
    check_binomial_tree(Value, RestChildren, K1).  


merge_bt(bt(Value1, Children1), bt(Value2, Children2), bt(Value1, [bt(Value2, Children2) | Children1])) :-
    Value1 =< Value2.
merge_bt(bt(Value1, Children1), bt(Value2, Children2), bt(Value2, [bt(Value1, Children1) | Children2])) :-
    Value1 > Value2.




% Add a binomial tree T into a binomial heap L, resulting in a new heap N.
add_bt(T, L, N) :- 
    reverse(L, LR),         % Reverse the input list to start processing from the least significant tree.
             % Reverse the output list for the same reason.
    dag(T, S1),             % Determine the order of the tree T.
    do1(LR, T, S1, N).      % Perform the merge operation.

% Determine the order (degree) of a binomial tree.
dag(empty, 0).              % An empty tree has order 0.
dag(bt(_, Children), S1) :- 
    length(Children, S1).   % The order of the tree is the number of children.

% Perform the merge operation.
do1([TREE | Rest], T, S, [TREE | NRest]) :- 
    S > 0,                    % Continue if S (order) is greater than 0.
    S1 is S - 1,              % Decrement the order.
    do1(Rest, T, S1, NRest).   % Recursive call with decremented order.

do1([empty | Rest], T, 0, [T | Rest]):-!.  % Place the tree T in the first empty slot (order 0).

do1([T1 | Rest], T, 0, [empty | NRest]) :- 
    merge_bt(T1, T, T2),                % Merge two trees of the same order.
    do1(Rest, T2, 0, NRest).             % Recursive call with the merged tree.

do1([], T, 0, [T]):-!.                      % If the list is empty and order is 0, start a new list with T.

do1([], T, S1, [empty | NRest]) :-       % If the list is empty and order is not 0, add an empty node.
    S2 is S1 - 1,                       % Decrement the order.
    do1([], T, S2, NRest).               % Recursive call with decremented order.

add(R, S, X) :- add_bt(bt(R, []), S, X).



fetch_tree_aux(_, [], _, []).
fetch_tree_aux(X, [bt(X, S)], Sons, []) :- Sons = S.
fetch_tree_aux(X, [bt(X, S)| D_s], Sons, [D_new| D_new_s]) :-
    D_s \= [],
    D_new = empty,
    Sons = S,
    fetch_tree_aux(X, D_s, Sons, D_new_s).

fetch_tree_aux(X, [bt(R,S)|Ds], Sons, [D_new|D_new_s]) :- 
    R \= X,
    D_new = bt(R,S),
    fetch_tree_aux(X, Ds, Sons, D_new_s).

fetch_tree_aux(X, [empty|Ds], Sons, [D_new|D_new_s]) :- 
    D_new = empty,
    fetch_tree_aux(X, Ds, Sons, D_new_s).

% If the stack got only empty entries, the Tree_idx is -1.
find_last_tree_idx([], _, -1) :- !.
find_last_tree_idx([], _, Tree_idx, Tree_idx).
find_last_tree_idx([empty|Ds], Idx, Temp_tree_idx, Tree_idx) :-
    Next_idx is Idx + 1,
    find_last_tree_idx(Ds, Next_idx, Temp_tree_idx, Tree_idx).
find_last_tree_idx([bt(_, _)|Ds], Idx, _, Tree_idx) :-
    New_temp_idx = Idx,
    Next_idx is Idx + 1,
    find_last_tree_idx(Ds, Next_idx, New_temp_idx, Tree_idx).

remove_trailing_empties_aux([], _, _, []).
remove_trailing_empties_aux(_, Idx, Last_tree_idx, []) :- Idx > Last_tree_idx.
remove_trailing_empties_aux([D_in|D_in_s], Idx, Last_tree_idx, [D_out| D_out_s]) :-
    Idx =< Last_tree_idx,
    D_out = D_in,
    Next_idx is Idx + 1,
    remove_trailing_empties_aux(D_in_s, Next_idx, Last_tree_idx, D_out_s).
    
remove_trailing_empties(S_in, S_out) :-
    find_last_tree_idx(S_in, 0, -1, Last_tree_idx),
	remove_trailing_empties_aux(S_in, 0, Last_tree_idx, S_out),
    !.

fetch_tree(X, S_in, Sons, S_out) :-
    fetch_tree_aux(X, S_in, Sons, S_temp),
    remove_trailing_empties(S_temp, S_out).

add_sons([], S_in, S_out) :- S_out = S_in.
add_sons([Son|Sons], S_in, S_out) :-
    add_bt(Son, S_in, S_temp),
    add_sons(Sons, S_temp, S_out).


list_all_roots(L, []) :- L = [].
list_all_roots(L, [empty|Ds]) :- list_all_roots(L, Ds).
list_all_roots([L|Ls], [bt(R, _)|Ds]) :- L=R, list_all_roots(Ls, Ds).

fetch_min(Min_val, S_full, S_new) :-
    list_all_roots(L, S_full),
    min_list(L, Min_val),
    fetch_tree(Min_val, S_full, Min_Sons, S_without_min_tree),
    add_sons(Min_Sons, S_without_min_tree, S_new).


sort_me(L,X):-to_heap(L,H),to_list(H,X).

to_heap([],[empty]). 
to_heap([Head|R],H) :- to_heap(R,H1),add(Head,H1,H).

to_list([],[]).
to_list(H,[Head|Rest]) :- fetch_min(Head,H,NH),to_list(NH,Rest).

