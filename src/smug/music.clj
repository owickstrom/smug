(ns smug.music
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]
            [smug.logic :refer :all]))

;;; LOGIC

(defn pitcho [p]
  (fd/in p (fd/interval 1 7)))

(defn note-valueo [v]
  (fd/in v (fd/domain 1 2 4 8 ;16
                      )))

(defne noteo [note]
  ([ [p v] ]
   (pitcho p)
   (note-valueo v)))

(defn noteso [notes]
  (everyg noteo notes))

(defn notes-total-valueo [notes total]
  (let [f (fne [a n o]
               ([ _ [p v] _ ]
                (note-valueo v)
                (fd/+ a v o)))]
    (foldo f notes 0 total)))

(defne with-intervalo [min max notes]
  ([ _ _ [] ])
  ([ _ _ [[p _]] ] (pitcho p))
  ([ _ _ [[p1 _] . [p2 v2] . ns] ]
   (pitcho p1)
   (pitcho p2)
   (fresh [diff tail]
     (fd/in diff (fd/interval min max))
     (conde
      [(fd/+ p1 diff p2)]
      [(fd/- p1 diff p2)])
     (conso [p2 v2] ns tail)
     (with-intervalo min max tail))))

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
   (everyg noteo notes)
   (note-valueo duration)))

(defn groupso [groups duration]
  (let [f (fn [a group o]
            (fresh [group-total]
              (groupo group group-total)
              (fd/+ a group-total o)))]
    (all
     (foldo f groups 0 duration)
     (distincto groups)
     )))

(defn group-note-valueso [group note-values]
  (mapo (fn [n o]
          (fresh [p]
            (== n [p o])))
        group
        note-values))

(defn groups-note-valueso [groups note-values]
  (mapo group-note-valueso groups note-values))

(defn baro [bar]
  (fresh [note-values]
    (groupso bar 16)))

(defn bars-noteso [bars notes]
  (fresh [bars-notes]
    (mapo flatteno bars bars-notes)
    (flatteno bars-notes notes)))

(defn phraseo [phrase]
  (let [bars (repeatedly 2 lvar)]
    (all
     (distincto bars)
     (everyg baro bars)
     (== phrase bars))))

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

(defn ->score [bars]
  (let [without-groups (flatten-groups bars)]
    {:bars (map ->bar without-groups)}))

;;; INTERFACE

(defn generate-score [n]
  (let [phrases (run n [q]
                 (phraseo q))]
    (->score (apply concat phrases))))
