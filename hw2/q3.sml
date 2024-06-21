type regexp = string;


fun isMatch  (s1: regexp) (s2: string) : bool =
    let
        val let_arr1 = String.explode s2
        val let_arr2 = String.explode s1

        fun matchLists([], []) = true
          | matchLists(_, []) = false
          | matchLists([], c2::cs2) =
              if c2 = #"*" then matchLists([], cs2) else false
          | matchLists(cs1, cs2) =
              let
                  val c1 = hd cs1
                  val c2 = hd cs2
                  val cs1_tail = tl cs1
                  val cs2_tail = tl cs2
              in
                  if c2 = #"*" then matchLists(cs1, cs2_tail) 
                  orelse (not (null cs1) 
                  andalso matchLists(cs1_tail, cs2))
                  else if not (null cs1) andalso c1 = c2 then
                      matchLists(cs1_tail, cs2_tail)
                  else
                      false
              end
    in
        matchLists(let_arr1, let_arr2)
    end;
