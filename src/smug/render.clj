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

(defn score->tune [{:keys [bars]}]
  (let [tune (Tune.)
        music (.getMusic tune)]
    (doseq [line (partition 4 bars)]
      (doseq [bar line]
        (doseq [note bar]
          (.addElement music (->abc-note note)))
        (.addElement music (BarLine.)))
      (.addElement music (EndOfStaffLine.)))
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
