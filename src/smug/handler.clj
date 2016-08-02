(ns smug.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.util.io :refer [piped-input-stream]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [smug.music :refer [gen-music]]
            [smug.render :refer [create-score-component render-to]]))

(defn render-score []
  {:headers {"Content-Type" "image/png"}
   :body (piped-input-stream
          (fn [output-stream]
            (render-to (create-score-component (gen-music 32))
                       output-stream)))})

(defroutes app-routes
  (GET "/" [] "<h1>Generated Score</h1><img src=\"/score.png\" />")
  (GET "/score.png" [] (render-score))
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
