use "hw4_q1.sml";

(* TODO, REPLACE WITH PATH TO YOUR PARSER *)
use "../hw3_q3.sml"

exception LispError;

(* Helper function - feel free to delete *)
fun first (x, _) = x;

local
    fun tokenize x = 
        String.tokens (fn c: char => c = #" ") 
            (String.translate (fn #"(" => "( " | #")" => " )" | c => str c) x);

    (* Helper functions - feel free to delete *)
    (* ====================================== *)
    fun is_digit c = c >= #"0" andalso c <= #"9";

    fun is_number str =
        let
            fun check [] = true
            | check (c::cs) = is_digit c andalso check cs
            
            val chars = String.explode str
        in
            if List.null chars then false else check chars
        end;
        
    fun char_to_int c = ord(c) - ord(#"0")

    fun string_to_int str =
        let
            fun convert [] acc = acc
            | convert (c::cs) acc = convert cs (10 * acc + char_to_int c)
        in
            convert (String.explode str) 0
        end;

    fun sexp_to_int sexp =
        case sexp of
            ATOM (SYMBOL s) => string_to_int s
          | _ => raise LispError;
    (* ====================================== *)

in
    fun eval str env =
    let
        fun real_eval (ATOM (SYMBOL string_exp)) env =
        if is_number string_exp then ATOM (SYMBOL string_exp)
        else find string_exp env
      | real_eval (ATOM NIL) env = ATOM NIL
      | real_eval (CONS (ATOM (SYMBOL "quote"), CONS (x, ATOM NIL))) env = x
      | real_eval (CONS (ATOM (SYMBOL "car"), CONS (x, _))) env = 
        (case real_eval list env of
             CONS (x, _) => x (*fix return value*)
           | _ => raise LispError)
      | real_eval (CONS (ATOM (SYMBOL "cdr"), CONS (_, y))) env = 
        (case real_eval list env of
             CONS (_, y) => y (*fix return value*)
           | _ => raise LispError)
      | real_eval (CONS (ATOM(SYMBOL "CONS"),x,y)) env= CONS( )

      | real_eval _ _ = raise LispError;
    in 
        real_eval (parse tokenize str) env
end;
