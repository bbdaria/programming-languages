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



