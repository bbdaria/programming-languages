exception Empty

datatype ('a, 'b) heterolist = NIL |
::: of ('a * ('b, 'a) heterolist);
infixr 5 :::;


fun build4 (x, one, y, two) = x:::one:::y:::two:::NIL;

fun unzip NIL = (nil, nil)
| unzip (a ::: (b ::: rest)) =
    let 
        val (aList, bList) = unzip(rest)
    in
        (a :: aList, b :: bList)
    end
| unzip (a ::: NIL) = (a :: nil, nil)

fun zip (nil, nil) = NIL
| zip (a :: restA, b :: restB) = a ::: b ::: zip(restA, restB)
| zip (a :: rest, nil) = raise Empty
| zip (nil, b :: rest) = raise Empty