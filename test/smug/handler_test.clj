(ns smug.handler-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [smug.handler :refer :all]))

(defn- init-test-app []
  (init-app (.getAbsolutePath (java.io.File/createTempFile "smug-test" ""))))

(deftest test-app
    (testing "not-found route"
    (let [response ((init-test-app) (mock/request :get "/invalid"))]
      (is (= (:status response) 404)))))
