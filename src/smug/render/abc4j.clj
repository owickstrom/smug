(ns smug.render.abc4j
  (:require [smug.render.model :refer :all])
  (:import [abc.notation
            BarLine
            EndOfStaffLine
            Note
            Tune]
           [abc.ui.swing JScoreComponent]
           [java.awt Color]
           [javax.swing
            JFrame
            JScrollPane]))

(defmulti render-element (fn [[element-type]] element-type))

(defmethod render-element :note
  [[_ pitch duration]]
  (let [note-pitch (condp = pitch
                     :c Note/C
                     :d Note/D
                     :e Note/E
                     :f Note/F
                     :g Note/G
                     :a Note/A
                     :b Note/B)
        note-duration (condp = duration
                        1    Note/WHOLE
                        1/2  Note/HALF
                        1/4  Note/QUARTER
                        1/8  Note/EIGHTH
                        1/16 Note/SIXTEENTH
                        1/32 Note/THIRTY_SECOND
                        1/64 Note/SIXTY_FOURTH)
        note (Note. note-pitch)]
    (doto (Note. note-pitch)
      (.setStrictDuration note-duration))))

(defmethod render-element :bar-line
  [[_ line-type]]
  (let [line-type (condp = line-type
                    :end    BarLine/END
                    :single BarLine/SIMPLE)]
    (BarLine. line-type)))

(defmethod render-element :end-of-staff-line
  [_]
  (EndOfStaffLine.))

(defn score->tune [score]
  (let [tune (Tune.)
        music (.getMusic tune)]
    (doseq [element (score->elements score)]
      (.addElement music (render-element element)))
    tune))

(defn create-score-component [score]
  (doto (JScoreComponent.)
    (.setSize (float 60))
    (.setTune (score->tune score))
    (.setJustification true)))

(defn render-to [score-component output]
  (.writeScoreTo score-component output))

(defn render-in-window [score-component]
  (let [frame (JFrame. "Generated Score")
        scroll-pane (JScrollPane. score-component)]
    (.setSize frame 640 480)
    (.setContentPane frame scroll-pane)
    (.setVisible frame true)))
