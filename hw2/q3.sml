fun cutLeadingSequence (ch: char) (charList: char list) : char list =
    let
        fun removeLeadingChars [] = []
          | removeLeadingChars (x::xs) =
              if x = ch then removeLeadingChars xs
              else x::xs
    in
        removeLeadingChars charList
    end


type regexp = string

fun isMatch (s1: regexp) (s2: string) : bool =
    let
        val let_arr1 = String.explode s1
        val let_arr2 = String.explode s2

        fun matchLists([], []) = true
          | matchLists(_, []) = false
          | matchLists([], c2::cs2) =
              if not (null cs2) andalso hd cs2 = #"*" then matchLists([], tl cs2) else false
          | matchLists(c1::cs1, c2::cs2) =
              if not (null cs2) andalso hd cs2 = #"*" then
                  matchLists(cutLeadingSequence(c2) (c1::cs1), tl cs2)
              else if c2 = #"*" then
                  matchLists(c1::cs1, cs2)
              else if c1 = c2 then
                  matchLists(cs1, cs2)
              else
                  false
    in
        matchLists(let_arr2, let_arr1)
    end;
