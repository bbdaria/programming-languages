use "hw4_q1.sml";
use "./hw4_parser.sml";

exception LispError;
Control.Print.printLength := 100;
Control.Print.printDepth := 100;


(* Helper function - feel free to delete *)
fun first (x, _) = x;
fun tokenize x = 
        String.tokens (fn c: char => c = #" ") 
            (String.translate (fn #"(" => "( " | #")" => " )" | c => str c) x);

fun print_atom (SYMBOL s) = s
  | print_atom NIL = "nil"

fun ends_with_nil (CONS (car, cdr)) = ends_with_nil cdr
  | ends_with_nil (ATOM NIL) = true
  | ends_with_nil _ = false

fun sexp_to_string (ATOM a) = print_atom a
  | sexp_to_string (CONS (car, cdr)) = "(" ^ print_cons (car, cdr) ^ ")"

and print_cons (car, ATOM NIL) = sexp_to_string car
  | print_cons (car, CONS (cadr, cddr)) = (case ends_with_nil (CONS (cadr, cddr)) of
      true => sexp_to_string car ^ " " ^ print_cons (cadr, cddr)
    | false => sexp_to_string car ^ " . " ^ sexp_to_string (CONS (cadr, cddr)))
  | print_cons (car, cdr) = sexp_to_string car ^ " . " ^ sexp_to_string cdr;


local
    (*fun tokenize x = 
        String.tokens (fn c: char => c = #" ") 
            (String.translate (fn #"(" => "( " | #")" => " )" | c => str c) x);*)

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

    fun is_atom (ATOM NIL) = true
        | is_atom (ATOM (SYMBOL s)) = true
        | is_atom _ = false;

    fun math_lisp (operation, arg1, arg2, env) =
        let
            val num1 =  if is_number arg1 then
                            string_to_int arg1
                        else
                            sexp_to_int (find arg1 env)
            val num2 = if is_number arg2 then
                            string_to_int arg2
                        else
                            sexp_to_int (find arg2 env)
            val res = 
                case operation of
                    "+" => num1 + num2
                    | "-" => num1 - num2
                    | "*" => num1 * num2
                    | "/" => num1 div num2
                    | "mod" => num1 mod num2
                    | _ => raise LispError
            val res_str = Int.toString res
        in
            (ATOM (SYMBOL res_str))
        end;

    fun comp_lisp (comp_op, arg1, arg2, env) =
        let
            val num1 =  if is_number arg1 then
                            string_to_int arg1
                        else
                            sexp_to_int (find arg1 env)
            val num2 = if is_number arg2 then
                            string_to_int arg2
                        else
                            sexp_to_int (find arg2 env)
            val res = 
                case comp_op of
                    "=" => num1 = num2
                    | "/=" => num1 <> num2
                    | "<" => num1 < num2
                    | ">" => num1 > num2
                    | _ => raise LispError
        in
            if res then
                (ATOM (SYMBOL "t"))
            else
                (ATOM (SYMBOL "nil"))
        end;

        
    fun eval_aux (expr, env) = 
    let
        fun eval_expr (CONS (ATOM (SYMBOL "cons"), CONS (expr1, CONS (expr2, ATOM NIL))), env) = 
            let
                val (val1, new_env1) = eval_aux (expr1, env)
                val (val2, new_env2) = eval_aux (expr2, new_env1)
                val result = CONS (val1, val2)
            in
                (result, new_env2)
            end
          | eval_expr (CONS (ATOM (SYMBOL "car"), CONS(sub_expr, ATOM NIL)), env) =
            let
                val (eval_res, new_env) = eval_aux (sub_expr, env)
            in
                (case eval_res of
                    CONS (head, _) => (head, new_env)
                  | _ => (ATOM (SYMBOL "lisp-error"), new_env))
            end
          | eval_expr (CONS (ATOM (SYMBOL "cdr"), CONS (sub_expr, ATOM NIL)), env) =
            let
                val (eval_res, new_env) = eval_aux (sub_expr, env)
            in
                (case eval_res of
                    CONS (_, tail) => (tail, new_env)
                  | _ => (ATOM (SYMBOL "lisp-error"), new_env))
            end
          | eval_expr (CONS (ATOM (SYMBOL "quote"), CONS (sub_expr, ATOM NIL)), env) = (sub_expr, env)
          | eval_expr (CONS (ATOM (SYMBOL "atom"), CONS (sub_expr, ATOM NIL)), env) =
            let
                val (eval_res, new_env) = eval_aux (sub_expr, env)
            in
                (case eval_res of
                    ATOM _ => (ATOM (SYMBOL "t"), new_env)
                  | _ => (ATOM NIL, new_env))
            end
          | eval_expr (CONS (ATOM (SYMBOL "null"), CONS (sub_expr, ATOM NIL)), env) =
            let
                val (eval_res, new_env) = eval_aux (sub_expr, env)
            in
                (case eval_res of
                    ATOM NIL => (ATOM (SYMBOL "t"), new_env)
                  | _ => (ATOM NIL, new_env))
            end
          | eval_expr (CONS (ATOM (SYMBOL "eq"), CONS (expr1, CONS(expr2, ATOM NIL))), env) =
            let
                val (val1, new_env1) = eval_aux (expr1, env)
                val (val2, new_env2) = eval_aux (expr2, new_env1)
            in
                if val1 = val2 andalso is_atom val1 andalso is_atom val2 then
                    (ATOM (SYMBOL "t"), new_env2)
                else
                    (ATOM NIL, new_env2)
            end
          | eval_expr (CONS (ATOM (SYMBOL "cond"), conditions), env) =
            let
                fun eval_condition (CONS (condition, CONS (body, ATOM NIL)), env) =
                    let
                        val (cond_result, new_env) = eval_aux (condition, env)
                    in
                        (cond_result, body, new_env)
                    end
                  | eval_condition _ = raise LispError

                fun eval_conditions (CONS (condition, rest), env) =
                    let
                        val (cond_result, body, new_env) = eval_condition (condition, env)
                    in
                        if cond_result <> ATOM NIL then
                            eval_aux (body, new_env)
                        else
                            eval_conditions (rest, new_env)
                    end
                  | eval_conditions (ATOM NIL, env) = (ATOM NIL, env)
            in
                eval_conditions (conditions, env)
            end
        | eval_expr (CONS (ATOM (SYMBOL "+"), CONS(ATOM (SYMBOL arg1), CONS(ATOM (SYMBOL arg2), ATOM NIL))), env) = (math_lisp ("+", arg1, arg2, env), env)
        | eval_expr (CONS (ATOM (SYMBOL "-"), CONS(ATOM (SYMBOL arg1), CONS(ATOM (SYMBOL arg2), ATOM NIL))), env) = (math_lisp ("-", arg1, arg2, env), env)
        | eval_expr (CONS (ATOM (SYMBOL "*"), CONS(ATOM (SYMBOL arg1), CONS(ATOM (SYMBOL arg2), ATOM NIL))), env) = (math_lisp ("*", arg1, arg2, env), env)
        | eval_expr (CONS (ATOM (SYMBOL "/"), CONS(ATOM (SYMBOL arg1), CONS(ATOM (SYMBOL arg2), ATOM NIL))), env) = (math_lisp ("/", arg1, arg2, env), env)
        | eval_expr (CONS (ATOM (SYMBOL "mod"), CONS(ATOM (SYMBOL arg1), CONS(ATOM (SYMBOL arg2), ATOM NIL))), env) = (math_lisp ("mod", arg1, arg2, env), env)
        | eval_expr (CONS (ATOM (SYMBOL "="), CONS(ATOM (SYMBOL arg1), CONS(ATOM (SYMBOL arg2), ATOM NIL))), env) = (comp_lisp ("=", arg1, arg2, env), env)
        | eval_expr (CONS (ATOM (SYMBOL "/="), CONS(ATOM (SYMBOL arg1), CONS(ATOM (SYMBOL arg2), ATOM NIL))), env) = (comp_lisp ("/=", arg1, arg2, env), env)
        | eval_expr (CONS (ATOM (SYMBOL "<"), CONS(ATOM (SYMBOL arg1), CONS(ATOM (SYMBOL arg2), ATOM NIL))), env) = (comp_lisp ("<", arg1, arg2, env), env)
        | eval_expr (CONS (ATOM (SYMBOL ">"), CONS(ATOM (SYMBOL arg1), CONS(ATOM (SYMBOL arg2), ATOM NIL))), env) = (comp_lisp (">", arg1, arg2, env), env)
          | eval_aux (CONS (CONS (ATOM (SYMBOL "lambda"), CONS (params, CONS (exp, ATOM NIL))), values), env_stack) =
            let
                fun init_params ((CONS (ATOM (SYMBOL param), rest_params)), (CONS (value, rest_values)), env, env_stack) =
                    let
                        val eval_val = first (eval_aux (value, env_stack))
                        val updated_env = define param env eval_val
                    in
                        init_params (rest_params, rest_values, updated_env, env_stack)
                    end
                    | init_params ((ATOM NIL), (ATOM NIL), env, env_stack) = env
                    | init_params (_, _, _, _) = raise LispError

                val env_init = initEnv()
                val env = init_params (params, values, env_init, env_stack)
                val updated_env_stack = pushEnv env env_stack
                val sexp = sexp_to_string exp
                val evaluated_exp = first (eval_aux (exp, updated_env_stack))
            in
                (evaluated_exp, env_stack)
            end
          | eval_expr (CONS (ATOM (SYMBOL sym), ATOM NIL), env) = eval_expr (ATOM (SYMBOL sym), env)
          | eval_expr (ATOM (SYMBOL sym), env) =
            (case sym of
                s => if is_number s orelse s = "nil" then (ATOM (SYMBOL s), env)
                     else (find s env, env) handle Undefined => (ATOM (SYMBOL "lisp-error"), env))
          | eval_expr _ = (ATOM (SYMBOL "lisp-error"), env)
    in
        eval_expr (expr, env)
    end;

    (* ====================================== *)

in
    fun eval string_exp env =
        let
            val sexp = parse (tokenize string_exp)
        in
            eval_aux (sexp, env) handle LispError => (ATOM (SYMBOL "lisp-error"), env)
        end
end;
