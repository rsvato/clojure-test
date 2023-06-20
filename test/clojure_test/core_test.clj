(ns clojure-test.core-test
  (:require [clojure.test :refer :all]
            [clojure-test.core :refer :all]))

(def secret "ulysses")
(def phrase "odyssey left ilium at eleven")

(deftest encoded-equal
  (testing "Encode"
    (is (= "v" (carroll-encode "a" "v")))))

(deftest decoded-equal
  (testing "Decode")
  (is (= "a" (carroll-decode "v" "v")))
  )

(deftest alphabetic-encode-decode
  (testing "Aphabetic"
    (is (= phrase (carroll-decode secret (carroll-encode secret phrase))))))

(deftest autokey-encode-decode
  (testing "Autokey"
    (is
     (= phrase
        (autokey-decode secret
                        (autokey-encode secret phrase))
        )
     )
    )
)
