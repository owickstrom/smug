;;; Copyright (c) 2016 Oskar Wickström
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns smug.render.model)

(defn- note->element [[pitch duration]]
  [:note pitch duration])

(defn- bar->elements [bar]
  (map note->element bar))

(defn- bars->lines
  "Splits the bars of a score into lines of bars
  to create a more readable layout."
  [bars-per-line bars]
  (if (< (count bars) bars-per-line)
    ;; A single line.
    [bars]
    ;; Multiple lines. Use an empty vector as pad for
    ;; the last partition, otherwise it is discarded.
    (partition
     bars-per-line
     bars-per-line
     []
     bars)))

(defn- line->elements [line]
  (apply
   concat
   (interpose [[:bar-line :single]]
              (map bar->elements line))))

(defn- lines->elements [lines]
  (apply
   concat
   (interpose [[:end-of-staff-line]]
              (map line->elements lines))))

(defn score->elements
  "Converts the score to the abstract rendering model."
  [score]
  (let [lines (bars->lines 8 (:bars score))
        line-elements (lines->elements lines)]
    (concat
     line-elements
     [[:bar-line :end]])))
