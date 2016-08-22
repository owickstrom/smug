;;; Copyright (c) 2016 Oskar Wickström
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns smug.music
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]
            [smug.logic :refer :all]))

;;; LOGIC

(defn pitcho
  "Constrain `p` to be a pitch, i.e. an integer in the range 0-7,
   where 0 is a 'rest', and 1-7 are the notes C-B."
  [p]
  (fd/in p (fd/interval 0 7)))

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

(defn barso [count bars]
  (let [bs (repeatedly count lvar)]
    (all
     (everyg baro bs)
     (== bars bs))))

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

(defne phraseo [notes]
  ([ [[p1 4] [p2 4] [p3 4] [p4 4] [p5 4] [0 4]] ]
   (fd/!= 0 p1)
   (fd/!= 0 p2)
   (fd/!= 0 p3)
   (fd/!= 0 p4)
   (fd/!= 0 p5)
   (distincto [p1 p2 p3 p4 p5]))
  ([ [[p1 4] [p2 8] [p3 2] [p4 2] [p5 4] [0 4]] ]
   (fd/!= 0 p1)
   (fd/!= 0 p2)
   (fd/!= 0 p3)
   (fd/!= 0 p4)
   (fd/!= 0 p5)
   (distincto [p1 p2 p3 p4 p5])))

;;; CONVERSION

(defn ->pitch [p]
  (nth [:r :c :d :e :f :g :a :b] p))

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
  {:bars (map ->bar bars)})

;;; INTERFACE

(defn generate-score [n]
  (let [bars (run n [notes]
               (fresh [p1 p2 bars]
                 (phraseo p1)
                 (phraseo p2)
                 (everyg noteo p1)
                 (everyg noteo p2)
                 (appendo p1 p2 notes)
                 (notes-total-valueo notes (* 16 3))
                 (barso 3 bars)
                 (bars-noteso bars notes)))]
    (->score bars)))
