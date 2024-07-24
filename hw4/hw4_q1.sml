exception Empty;
exception Undefined;

datatype Atom =
   NIL | SYMBOL of string;

datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);

fun initEnv () = fn
    (x: string) => raise Undefined
  | (_) => Atom NIL

