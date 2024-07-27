exception Empty;
exception Undefined;

datatype Atom =
   NIL | SYMBOL of string;

datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);

fun initEnv () = fn
    (x: string) => (raise Undefined):SExp;


fun define x y z = fn (a: string) => if x = a then z else y x;

fun emptyNestedEnv () = [] : (string -> SExp) list;

fun pushEnv x y = x::y;

fun popEnv (x::y) = y | popEnv [] = raise Empty;

fun topEnv (x::y) = x | topEnv [] = raise Empty;

fun defineNested (s: string) ([]: (string -> 'a) list) (z: 'a) = raise Empty   | defineNested (s: string) (list: (string -> 'a) list) (z: 'a) =
    let
        val top = topEnv(list)
        val newTop = define s top z
        val rest = popEnv(list)
    in
        pushEnv newTop rest
    end;

fun find (msg: string) ([]: (string -> 'a) list) = raise Undefined
  | find (msg: string) (env::rest) =
    (env msg) handle Undefined => find msg rest;
