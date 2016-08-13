(ns smug.music
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

;;; LOGIC

(defne foldo
  "Fold the logical sequence 's' with relation 'f' using 'a' as an initial
   accumulator and 'o' as the output variable. The function 'f' takes the
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

(defn everyo
  "Apply the relation 'f' on every value in the logical sequence 's'."
  [f s]
  (fresh [a o]
    (foldo (fn [_ x _] (f x)) s a o)))

(defn lengtho
  [s l]
  (foldo #(fd/+ %1 1 %3) s 0 l))

(defn pitcho [p]
  (fd/in p (fd/interval 1 7)))

(defn note-valueo [v]
  (fd/in v (fd/domain 1 2 4 8 16)))

(defne noteo [note]
  ([ [p v] ]
   (pitcho p)
   (note-valueo v)))

(defn noteso [notes]
  (everyo noteo notes))

(defn notes-total-valueo [notes total]
  (let [f (fne [a n o]
               ([ _ [p v] _ ]
                (note-valueo v)
                (fd/+ a v o)))]
    (foldo f notes 0 total)))

(defn groupo [notes duration]
  (all
    (matche [notes]
            ([ [[_ v]] ]
             (fd/>= v 4)
             (fd/== v duration))
            ([ [[_ 1] [_ 1] [_ 1] [_ 1]] ]
             (fd/== duration 4))
            ([ [[_ 1] [_ 2] [_ 1]] ]
             (fd/== duration 4))
            ([ [[_ 2] [_ 1] [_ 1]] ]
             (fd/== duration 4))
            ([ [[_ 1] [_ 1] [_ 2]] ]
             (fd/== duration 4))
            ([ [[_ 2] [_ 2]] ]
             (fd/== duration 4))
            ([ [[_ 2] [_ 4] [_ 2]] ]
             (fd/== duration 8)))
    (everyo noteo notes)
    (note-valueo duration)))

(defn groupso [groups duration]
  (let [f (fn [a group o]
            (fresh [group-total]
              (groupo group group-total)
              (fd/+ a group-total o)))]
    (foldo f groups 0 duration)))

(defn bar-noteso [bar notes]
  (foldo appendo bar [] notes))

(defne scaleo [direction notes]
  ([ _ [] ])
  ([ _ [[p _]] ] (pitcho p))
  ([ _ [[p1 _] . [p2 v2] . ns]]
   (pitcho p1)
   (pitcho p2)
   (conde
    [(== direction :asc)
     (fd/+ p1 1 p2)]
    [(== direction :desc)
     (fd/- p1 1 p2)])
   (fresh [tail]
     (conso [p2 v2] ns tail)
     (scaleo direction tail))))

(defne min-intervalo [min notes]
  ([ _ [] ])
  ([ _ [[p _]] ] (pitcho p))
  ([ _ [[p1 _] . [p2 v2] . ns] ]
   (pitcho p1)
   (pitcho p2)
   (fresh [diff tail]
     (fd/>= diff min)
     (conde
      [(fd/+ p1 diff p2)]
      [(fd/- p1 diff p2)])
     (conso [p2 v2] ns tail)
     (min-intervalo min tail))))

(defn baro [bar]
  (fresh [dir notes]
    (groupso bar 16)
    (bar-noteso bar notes)
    (scaleo dir notes)))

;;; CONVERSION

(defn ->pitch [p]
  (nth [:c :d :e :f :g :a :b] (- p 1)))

(defn ->note-value [d]
  (/ d 16))

(defn ->note [[p d]]
  [(->pitch p)
   (->note-value d)])

(defn flatten-groups [groups]
  (map #(apply concat %1) groups))

(defn ->bar [bar]
  (map ->note bar))

;;; INTERFACE

(defn generate-score [n]
  (let [groups (run n [q]
                 (baro q))
        bars (flatten-groups groups)]
    {:bars (map ->bar bars)}))
