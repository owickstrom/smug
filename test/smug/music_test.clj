(ns smug.music-test
  (:require [clojure.test :refer :all]
            [smug.music :refer :all]))

(deftest test-->score

  (testing "empty score"
    (is (= (->score [])
           {:bars []})))
  
  (testing "score of one bar with three groups"
    (is (= (->score [[[[1 1] [4 2] [3 1]]
                      [[2 4]]
                      [[3 8]]]])
           {:bars [[[:c 1/16]
                    [:f 1/8]
                    [:e 1/16]
                    [:d 1/4]
                    [:e 1/2]]]}))))
