
fun to_binary 0 = []
  | to_binary n =
      let
        fun aux 0 acc = acc
          | aux n acc = aux (n div 2) (acc @ [n mod 2])
      in
        aux n []
      end;

fun binaryToDecimal binaryList =
    let
        fun helper [] _ acc = acc
          | helper (x::xs) power acc = helper xs (power * 2) (acc + x * power)
    in
        helper binaryList 1 0
    end;


fun encode [] = []
  | encode listToCode = 
    let 
        fun reverse 0 = 1
          | reverse a = 0
        
        fun getNumber [] = (0, 0)
          | getNumber (0 :: rest) =
              let val (zeros, ones) = getNumber rest
              in (zeros + 1, ones) end
          | getNumber (a :: rest) =
              let val (zeros, ones) = getNumber rest
              in (zeros, ones + 1) end
        
        fun helper ([], (zeros, ones)) _ = []
          | helper (first :: rest, (zeros, ones)) currIndex =
              let
                val reversed = reverse first
                val newVector = reversed :: rest
                val (newZeros, newOnes) =
                  if reversed = 0 then (zeros + 1, ones - 1)
                  else (zeros - 1, ones + 1)
              in
                if newZeros = newOnes then newVector @ to_binary (currIndex + 1)
                else reversed :: helper (rest, (newZeros, newOnes)) (currIndex + 1)
              end
        
    in
        helper (listToCode, getNumber listToCode) 0
    end;


fun decode ([], _) = []
  | decode ((first :: rest), length) = 
    let
        fun reverse 0 = 1
          | reverse a = 0
        
        fun getList ([], _) = []
          | getList (first :: rest, 0) = []
          | getList (first :: rest, a) = first :: getList (rest, a - 1)

        fun getRest ([], _) = []
          | getRest (first :: rest, 0) = first :: rest
          | getRest (first :: rest, a) = getRest (rest, a - 1)
        
        fun foldReverse ([], _) = []
          | foldReverse (first :: rest, 0) = first :: rest
          | foldReverse (first :: rest, a) = reverse first :: foldReverse (rest, a - 1)
        
        val original = first :: getList (rest, length - 1)
        val restOfGiven = getRest (first :: rest, length)
        val indexToStop = binaryToDecimal restOfGiven
    in
        foldReverse (original, indexToStop)
    end;

