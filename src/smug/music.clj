(ns smug.music
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

;;; LOGIC

(defne pitcho [p difficulty]
  ([ _ :easy ]
   (fd/in p (fd/interval 1 7)))
  ([ _ _ ]
   (fd/in p (fd/interval 1 7)))
  )

(defne durationo [d difficulty]
  ([ _ :easy ]
   (fd/in d (fd/domain 4 8 16)))
  ([ _ :medium]
   (fd/in d (fd/domain 2 4 8)))
  ([ _ :hard]
   (fd/in d (fd/domain 1 2))))

(defne noteo [note difficulty]
  ([ [p d] _ ]
   (pitcho p difficulty)
   (durationo d difficulty)))

(defne noteso [notes difficulty]
  ([ [] _ ])
  ([ [n . ns] _ ]
   (noteo n difficulty)
   (noteso ns difficulty)))

(defne notes-durationo [notes difficulty sum]
  ([ [] _ _ ]
   (fd/== sum 0))
  ([ [[p d] . ns] _ _ ]
   (fresh (s)
     (durationo d difficulty)
     (fd/+ d s sum)
     (notes-durationo ns difficulty s))))

(defne all-longero [notes minimum difficulty]
  ([ [] _ _ ])
  ([ [[p d] . ns] _ _ ]
   (durationo d difficulty)
   (fd/>= d minimum)
   (all-longero ns minimum difficulty)))

(defn groupo [notes difficulty]
  (fresh []
    (noteso notes difficulty)
    (notes-durationo notes difficulty 4)
    (matche [notes]
            ([ [[_ 1] [_ 1] [_ 1] [_ 1]] ])
            ([ [[_ 1] [_ 2] [_ 1]] ])
            ([ [[_ 2] [_ 1] [_ 1]] ])
            ([ [[_ 1] [_ 1] [_ 2]]])
            ([ [[_ 2] [_ 2]]])
            ([ _ ]
             (all-longero notes 4 difficulty)))))

(defne groupso [groups difficulty duration]
  ([ [] _ _ ]
   (fd/== duration 0))
  ([ [g . gs] _ _ ]
   (fresh (s)
     (groupo g difficulty)
     (fd/+ 4 s duration)
     (groupso gs difficulty s))))

(defn baro [notes difficulty]
  (fresh []
    (groupso notes difficulty 16)))

;;; CONVERSION

(defn ->pitch [p]
  (nth [:c :d :e :f :g :a :b
        :C :D :E :F :G :A :B] (- p 1)))

(defn ->duration [d]
  (/ d 16))

(defn ->note [[p d]]
  [(->pitch p)
   (->duration d)])

(defn ->bar [bar]
  (map ->note bar))

(defn flatten-groups [groups]
  (map #(apply concat %1) groups))

;;; INTERFACE

(defn generate-score
  ([n]
   (generate-score n :hard))
  ([n difficulty]
   (let [groups (run n [q]
                  (fresh [d]
                    (baro q d)
                    (== d difficulty)))]
     {:bars (map ->bar (flatten-groups groups))})))
