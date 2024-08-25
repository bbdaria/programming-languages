pythagorean(X, Y, Z) :-
    number(X),
    number(Y),
    number(Z),
    X*X + Y*Y =:= Z*Z.



is_divisible(N, Divisor) :-
    0 is N mod Divisor.

has_divisors(N, Divisor) :-
    Divisor * Divisor =< N,
    (   is_divisible(N, Divisor)
    ;   NextDivisor is Divisor + 1,
        has_divisors(N, NextDivisor)
    ).

prime(2).
prime(N) :-
    N > 2,
    \+ has_divisors(N, 2). 



goldbach(X, Y, Z) :-              
    between(2, Z, X),        
    Z1 is Z - X,             
    prime(X),                
    prime(Z1),               
    Y = Z1.                  

