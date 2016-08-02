(ns smug.render
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

(defn pitch->abc-note [pitch]
  (Note.
   (condp = pitch
     :c Note/C
     :d Note/D
     :e Note/E
     :f Note/F
     :g Note/G
     :a Note/A
     :b Note/B)))

(defn duration->strict [duration]
  (condp = duration
    1    Note/WHOLE
    1/2  Note/HALF
    1/4  Note/QUARTER
    1/8  Note/EIGHTH
    1/16 Note/SIXTEENTH
    1/32 Note/THIRTY_SECOND
    1/64 Note/SIXTY_FOURTH))

(defn ->abc-note [[pitch duration]]
  (let [note (pitch->abc-note pitch)]
    (.setStrictDuration note (duration->strict duration))
    note))

(defn bar->elements [bar]
  (map ->abc-note bar))

(defn line->elements [line]
  (apply
   concat
   (interpose [(BarLine.)]
              (map bar->elements line))))

(defn lines->elements [lines]
  (apply
   concat
   (interpose [(EndOfStaffLine.)]
              (map line->elements lines))))

(defn score->elements [score]
  (let [line-break (EndOfStaffLine.)
        lines (partition 8 (:bars score))
           line-elements (lines->elements lines)]
    (concat
     line-elements
     [(BarLine. BarLine/END)])))

(defn score->tune [score]
  (let [tune (Tune.)
        music (.getMusic tune)]
    (doseq [element (score->elements score)]
      (.addElement music element))
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
