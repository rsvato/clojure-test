(ns clojure-test.core-test
  (:require [clojure.test :refer :all]
            [clojure-test.core :refer :all]))

(deftest encoded-equal
  (testing "Encode"
    (is (= "v" (encode "a" "v")))))
(deftest decoded-equal
  (testing "Decode")
  (is (= "a" (decode "v" "v")))
  )
