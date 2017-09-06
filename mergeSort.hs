--mergeSort.hs

halve seqx = let a = div (length seqx) 2
             in splitAt a seqx
                 
seqmerge acc seqa seqb 
                        |null seqa && null seqb = reverse acc
                        |null seqa = seqmerge (head seqb : acc) seqa (tail seqb)
                        |null seqb = seqmerge (head seqa : acc) (tail seqa) seqb
                        |head seqa <= head seqb = seqmerge (head seqa : acc) (tail seqa) seqb
                        |head seqa >= head seqb = seqmerge (head seqb : acc) seqa (tail seqb)

mergeSort seqx
              |null seqx = seqx
              |length seqx == 1 = seqx
              |otherwise = seqmerge [] (mergeSort (fst (halve seqx))) (((mergeSort (snd (halve seqx)))))
