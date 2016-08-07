(ns smug.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.util.io :refer [piped-input-stream]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [smug.music :refer [generate-score]]
            [smug.render.lilypond :refer [render-svg-to]]))

(defn render-score []
  (let [score (generate-score 32)
        out (render-svg-to
             score
             (java.io.File/createTempFile "score" ".svg"))]
    {:headers {"Content-Type" "image/svg+xml"}
     :body out}))


(defroutes app-routes
  (GET "/" [] "<img src=\"/score.svg\" />")
  (GET "/score.svg" [] (render-score))
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes site-defaults))

(defonce server (atom nil))

(defn stop-server! []
  (when @server
    (.stop @server)))

(defn start-server!
  ([] (start-server! {:port 3000
                      :join? false}))
  ([options]
   (stop-server!)
   (reset! server (run-jetty app options))))

(defn main- []
  (let [port (if-let [port (System/getenv "PORT")]
               (Integer/parseInt port)
               3000)]
    (start-server! {:port port
                    :join? true})))
