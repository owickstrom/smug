(ns smug.music
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

;;; LOGIC

(defn pitcho [p]
  (fd/in p (fd/interval 1 7)))

(defn note-valueo [v]
  (fd/in v (fd/domain 1 2 4 8 16)))

(defne noteo [note]
  ([ [p v] ]
   (pitcho p)
   (note-valueo v)))

(defne noteso [notes]
  ([ [] ])
  ([ [n . ns] ]
   (noteo n)
   (noteso ns)))

(defne notes-total-valueo [notes total]
  ([ [] _ ]
   (fd/== total 0))
  ([ [[p v] . ns] _ ]
   (fresh (s)
     (note-valueo v)
     (fd/+ v s total)
     (notes-total-valueo ns s))))

(defn baro [notes]
  (fresh []
    (noteso notes)
    (notes-total-valueo notes 16)))

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
  (let [bars (run n [q]
               (baro q))]
    {:bars (map ->bar bars)}))
