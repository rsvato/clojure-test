(ns clojure-test.core
  (:use [clojure.string :only [index-of join]])
)

; https://en.wikipedia.org/wiki/The_Alphabet_Cipher

(def alphabet "abcdefghijklmnopqrstuvwxyz ")

(defn line
  [letter]
  (let [pos (index-of alphabet letter)]
       (let [tail (subs alphabet pos) head (subs alphabet 0 pos)]
         (str tail head)
       ))
)

(defn expand-key
  [key length]
  (loop [k key]
    (if (> (count k) length)
      (subs k 0 length)
      (recur (str k k))))
)

(defn full-key
  [key length]
  (if (> (count key) length)
    (subs key 0 length)
    (expand-key key length))
)

(defn align
  [key message]
  (full-key key (count message))
)

(defn align-incorporate
  [key message]
  (let [full (str key message)] full
       (let [result (subs full 0 (count message))] result
       ))
)

(defn encode-letter
  [vec]
  (let [specimen (line "a") subst (line (second vec))]
    (let [initial (index-of specimen (first vec))]
      (let [result (nth subst initial)] result
    )))
)

(defn decode-letter
  [vec]
  (let [subst (line "a") specimen (line (second vec))]
    (let [initial (index-of specimen (first vec))]
      (let [result (nth subst initial)] result
    )))
)

(defn process [key message func]
  (let [lines (map vector message key)]
    (let [result (map func lines)] (join result))
    )
)

(defn decode [key message]
  (let [k (align key message)]
    (process k message decode-letter))
)

(defn encode [key message]
  (let [k (align key message)]
    (process k message encode-letter))
)

