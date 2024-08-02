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

fun diags DNil = Nil
  | diags (DCons ((x, y), nextX, nextY)) =
      let
        fun traverse_diag (1, y) = Cons ((1, y), fn () => diags (nextY ()))
          | traverse_diag (x, y) = Cons ((x, y), fn () => traverse_diag (x-1, y+1))
      in
        traverse_diag (x, y)
      end;