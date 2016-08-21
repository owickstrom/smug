;;; Copyright (c) 2016 Oskar Wickström
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns smug.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.util.io :refer [piped-input-stream]]
            [ring.middleware.file :refer [wrap-file]]
            [ring.middleware.params :refer [wrap-params]]
            [smug.music :refer [generate-score]]
            [smug.render.lilypond :refer [render-svg-to]]))

(defn- home-view []
  "<h1>SMUG &mdash; The Sheet Music Generator</h1>
   <form method=\"POST\" action=\"/score\">
      <label>
      Number of phrases
      <input name=\"phrase-count\" type=\"number\" value=\"32\" min=\"4\" max=\"128\" step=\"4\" />
      </label>
      <p>
      <button type=\"submit\">Generate Score</button>
      </p>
   </form>")

(defn- render-score-pages [output-dir phrase-count-str]
  (if-let [phrase-count (Integer/parseInt phrase-count-str)]
    (let [score (generate-score phrase-count)
          out-files (render-svg-to
                     score
                     (java.io.File/createTempFile
                      "score"
                      ".svg"
                      (java.io.File. output-dir)))
          pages (map #(str "<div><img src=\"" (.getName %) "\" /></div>")
                     out-files)]
      (clojure.string/join pages))
    {:status 400
     :body "Bad phrase count value."}))


(defn- app-routes [output-dir]
  (routes
   (GET "/" []
        (home-view))
   (POST "/score" [phrase-count]
         (render-score-pages output-dir phrase-count))
   (route/not-found "Not Found")))

(defonce server (atom nil))

(defn init-app [output-dir]
  (.mkdir (java.io.File. output-dir))
  (-> (app-routes output-dir)
      (wrap-file output-dir)
      wrap-params))

(defn stop-server! []
  (when @server
    (.stop @server)))

(defn start-server!
  ([] (start-server! {:port 3000
                      :join? false
                      :output-dir "generated"}))
  ([{:keys [port output-dir] :as options}]
   (stop-server!)
   (println "Starting server on port" port)
   (reset! server (run-jetty
                   (init-app output-dir)
                   options))))

(defn -main []
  (let [port (or (some-> (System/getenv "PORT")
                         Integer/parseInt)
                 3000)
        output-dir (or (System/getenv "OUTPUT_DIR") "generated")]
    (start-server! {:port port
                    :join? true
                    :output-dir output-dir})))
