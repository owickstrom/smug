(ns smug.music
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

;;; LOGIC

(defn pitcho [p]
  (fd/in p (fd/interval 1 7)))

(defn durationo [d]
  (fd/in d (fd/domain 1 2 4 8 16 32)))

(defne noteo [note]
  ([ [p d] ]
   (pitcho p)
   (durationo d)))

(defne noteso [notes]
  ([ [] ])
  ([ [n . ns] ]
   (noteo n)
   (noteso ns)))

(defne sum-durationo [notes sum]
  ([ [] _ ]
   (fd/in sum (fd/domain 0)))
  ([ [[p d] . ns] _ ]
   (fresh (s)
     (durationo d)
     (fd/+ d s sum)
     (sum-durationo ns s))))

(defne rising-fromo [notes pitch]
  ([ [] _ ])
  ([ [[p _] . ns] _ ]
   (pitcho p)
   (fd/> p pitch)
   (rising-fromo ns p)))

(defn baro [notes]
  (fresh []
    (sum-durationo notes 16)
    (noteso notes)
    ;;(rising-fromo notes 0)
    ))

;;; CONVERSION

(defn ->pitch [p]
  (nth [:c :d :e :f :g :a :b] (- p 1)))

(defn ->duration [d]
  (/ d 16))

(defn ->note [[p d]]
  [(->pitch p)
   (->duration d)])

(defn ->bar [bar]
  (map ->note bar))

;;; INTERFACE

(defn generate-score [n]
  (let [bars (run n [q] (baro q))]
    {:bars (map ->bar bars)}))
