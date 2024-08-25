use "q1_def.sml";

fun toMatrix DNil (_, _) = []
  | toMatrix _ (0, 0) = []
  | toMatrix _ (0, j) = []
  | toMatrix _ (i, 0) = []
  | toMatrix (DCons (value, nextX, nextY)) (i, j) =
    let
      fun insideMatrixCol DNil _ = []
        | insideMatrixCol (DCons (v, _, nextY)) (x, 1) = [v]
        | insideMatrixCol (DCons (v, _, nextY)) (x, y) = v :: insideMatrixCol (nextY ()) (x, y - 1)

      fun insideMatrixRow DNil _ = []
        | insideMatrixRow (DCons (v, nextX, nextY)) (0, j) = [insideMatrixCol (DCons (v, nextX, nextY)) (0, j)]
        | insideMatrixRow (DCons (v, nextX, nextY)) (x, j) = 
            let 
              val col = insideMatrixCol (DCons (v, nextX, nextY)) (0, j)
              val rest = insideMatrixRow (nextX ()) (x - 1, j)
            in 
              col :: rest
            end
    in
      insideMatrixRow (DCons (value, nextX, nextY)) (i - 1, j)
    end;

fun Q () = 
  let
    fun next (n, d) = DCons((n, d), fn () => next (n, d+1), fn () => next (n+1, d))
  in
    next (1, 1)
  end;


fun merge [] ys = ys
  | merge xs [] = xs
  | merge (x::xs) (y::ys) = x :: y :: merge xs ys;


local
    fun traverse [] [] = Nil
    | traverse [] next_diag = traverse next_diag []
    | traverse (dseq :: ns) [] =
        let
        in
        case dseq () of
            DCons (head, xf, yf) => Cons (head, fn () => traverse ns [yf, xf])
            | DNil => traverse ns []
        end
    | traverse (dseq :: ns) next_diag =
        let
        in
        case dseq () of
            DCons (head, xf, yf) => Cons (head, fn () => traverse ns (next_diag @ [xf]))
            | DNil => traverse ns next_diag
        end
in
    fun diags DNil = Nil
      | diags dseq = traverse [fn () => dseq] []
end;