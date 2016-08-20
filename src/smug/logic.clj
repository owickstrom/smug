(ns smug.logic
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(defne mapo
  "Map the relation `f` over every value in the sequence `s`, resulting 
   in the sequence `o`. The mapping function takes two arguments: a 
   variable in the sequence, and the corresponding variable in the output
    sequence."
  [f s o]
  ([ _ [] [] ])
  ([ _ [s-head . s-tail] [o-head . o-tail] ]
   (f s-head o-head)
   (mapo f s-tail o-tail)))

(defne foldo
  "Fold the logical sequence `s` with relation `f` using `a` as an initial
   accumulator and `o` as the output variable. The function `f` takes the
   accumulator, a value in the sequence, and an output accumulator variable.
   When the sequence is empty the initial accumulator is unified with the
   output variable."
  [f s a o]
  ([ _ [] _ _ ]
   (== a o))
  ([ _ [ h . t ] _ _ ]
   (fresh [a']
     (f a h a')
     (foldo f t a' o))))

(defn flatteno
  "A relation where `nested` is a sequence of nested sequences, such that
   the sequence `flat` directly contains all the nested elements of `nested`."
  [nested flat]
  (foldo appendo nested [] flat))

