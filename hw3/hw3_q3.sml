datatype Atom =
   NIL | SYMBOL of string;

datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);

fun tokenize x =
   String.tokens (fn c => c = #" ")
       (String.translate (fn #"(" => "( " | #")" => " )" | c => str c) x);


fun parse tokens =
    let
       
        fun parseExprs [] = (ATOM NIL, [])
          | parseExprs (")"::rest) = (ATOM NIL, rest)
          | parseExprs ("("::rest) = parseList rest
          | parseExprs (tok::rest) = (ATOM (SYMBOL tok), rest)
        
        and parseList tokens =
            let
                fun parseItems [] = (ATOM NIL, [])
                  | parseItems (")"::rest) = (ATOM NIL, rest)
                  | parseItems toks =
                        let
                            val parsedHead = parseExprs toks
                            val head = #1 parsedHead
                            val remainingTokens1 = #2 parsedHead
                            val parsedTail = parseItems remainingTokens1
                            val tail = #1 parsedTail
                            val remainingTokens2 = #2 parsedTail
                        in
                            (CONS(head, tail), remainingTokens2)
                        end
            in
                parseItems tokens
            end
        
        val (result, _) = parseExprs tokens
    in
        result
    end;
