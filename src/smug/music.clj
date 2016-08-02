(ns smug.music
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

;;; LOGIC

(defn pitcho [p]
  (fd/in p (fd/interval 1 7)))

(defn durationo [d]
  (fd/in d (fd/domain 1 2 4 8 16 32)))

(defn noteo [note]
  (matche [note]
   ([ [p d] ]
    (pitcho p)
    (durationo d))))

(defn noteso [notes]
  (matche
   [notes]
   ([ [] ])
   ([ [n . ns] ]
    (noteo n)
    (noteso ns))))

(defn sum-durationo [notes sum]
  (matche
   [notes]
   ([ [] ]
    (fd/in sum (fd/domain 0)))
   ([ [[p d] . ns] ]
    (fresh (s)
      (durationo d)
      (fd/+ d s sum)
      (sum-durationo ns s)))))

(defn rising-fromo [notes pitch]
  (matche
   [notes]
   ([ [] ])
   ([ [[p _] . ns] ]
    (pitcho p)
    (fd/> p pitch)
    (rising-fromo ns p))))

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
  (/ 1 d))

(defn ->note [[p d]]
  [(->pitch p)
   (->duration d)])

(defn ->bar [bar]
  (map ->note bar))

;;; INTERFACE

(defn gen-music [n]
  (let [bars (run n [q] (baro q))]
    (map ->bar bars)))
