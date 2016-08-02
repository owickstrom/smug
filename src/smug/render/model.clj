(ns smug.render.model)

(defn- note->element [[pitch duration]]
  [:note pitch duration])

(defn bar->elements [bar]
  (map note->element bar))

(defn line->elements [line]
  (apply
   concat
   (interpose [[:bar-line :single]]
              (map bar->elements line))))

(defn lines->elements [lines]
  (apply
   concat
   (interpose [[:end-of-staff-line]]
              (map line->elements lines))))

(defn score->elements [score]
  (let [lines (partition 8 (:bars score))
        line-elements (lines->elements lines)]
    (concat
     line-elements
     [[:bar-line :end]])))

