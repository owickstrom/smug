(ns smug.music
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

;;; LOGIC

(defne pitcho [p difficulty]
  ([ _ :easy ]
   (fd/in p (fd/interval 1 7)))
  ([ _ _ ]
   (fd/in p (fd/interval 1 14))))

(defne durationo [d difficulty]
  ([ _ :easy ]
   (fd/in d (fd/domain 4 8 16)))
  ([ _ :medium]
   (fd/in d (fd/domain 2 4 8)))
  ([ _ :hard]
   (fd/in d (fd/domain 1 2 4))))

(defne noteo [note difficulty]
  ([ [p d] _ ]
   (pitcho p difficulty)
   (durationo d difficulty)))

(defne noteso [notes difficulty]
  ([ [] _ ])
  ([ [n . ns] _ ]
   (noteo n difficulty)
   (noteso ns difficulty)))

(defne sum-durationo [notes difficulty sum]
  ([ [] _ _ ]
   (fd/in sum (fd/domain 0)))
  ([ [[p d] . ns] _ _ ]
   (fresh (s)
     (durationo d difficulty)
     (fd/+ d s sum)
     (sum-durationo ns difficulty s))))

(defne rising-fromo [notes pitch difficulty]
  ([ [] _ _ ])
  ([ [[p _] . ns] _ _ ]
   (pitcho p difficulty)
   (fd/> p pitch)
   (rising-fromo ns p difficulty)))

(defn baro [notes difficulty]
  (fresh []
    (sum-durationo notes difficulty 16)
    (noteso notes difficulty)
    ;;(rising-fromo notes 0 difficulty)
    ))

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

;;; INTERFACE

(defn generate-score
  ([n]
   (generate-score n :easy))
  ([n difficulty]
   (let [bars (run n [q]
                (fresh [d]
                  (baro q d)
                  (== d difficulty)))]
     {:bars (map ->bar bars)})))
