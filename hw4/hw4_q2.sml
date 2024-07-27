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
    fun eval string_exp env =
        case sexp of
            ATOM => if string_exp = NIL then NIL else if is_number string_exp then sexp_to_int string_exp else
    fun eval NIL env = NIL
    | eval ATOM (SYMBOL string_exp) env = if is_number string_exp then sexp_to_int string_exp else 
    | eval ATOM (SYMBOL string_exp) env = 
end;