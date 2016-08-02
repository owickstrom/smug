(ns smug.render.model-test
  (:require [clojure.test :refer :all]
            [smug.render.model :refer :all]))

(deftest test-model
  (testing "creates renderable elements for two bars"
    (let [bars [[[:c 1/2]
                 [:d 1/4]
                 [:d 1/4]]
                [[:e 1/4]
                 [:f 1/2]
                 [:g 1/4]]]
          elements (score->elements {:bars bars})]
      (is (= elements [[:note :c 1/2]
                       [:note :d 1/4]
                       [:note :d 1/4]
                       [:bar-line :single]
                       [:note :e 1/4]
                       [:note :f 1/2]
                       [:note :g 1/4]
                       [:bar-line :end]]))))

  (testing "creates renderable elements for 10 bars"
    (let [bars [[[:c 1]]
                [[:d 1]]
                [[:e 1]]
                [[:f 1]]
                [[:g 1]]
                [[:c 1]]
                [[:d 1]]
                [[:e 1]]
                [[:f 1]]
                [[:g 1]]]
          elements (score->elements {:bars bars})]
      (is (= elements [
                       [:note :c 1]
                       [:bar-line :single]
                       [:note :d 1]
                       [:bar-line :single]
                       [:note :e 1]
                       [:bar-line :single]
                       [:note :f 1]
                       [:bar-line :single]
                       [:note :g 1]
                       [:bar-line :single]
                       [:note :c 1]
                       [:bar-line :single]
                       [:note :d 1]
                       [:bar-line :single]
                       [:note :e 1]
                       
                       [:end-of-staff-line]
                       
                       [:note :f 1]
                       [:bar-line :single]
                       [:note :g 1]
                       [:bar-line :end]])))))
