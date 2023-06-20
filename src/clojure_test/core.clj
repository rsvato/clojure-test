(ns clojure-test.core
  (:use [clojure.string :only [index-of join]])
)

; https://en.wikipedia.org/wiki/The_Alphabet_Cipher
; https://en.wikipedia.org/wiki/Autokey_cipher

(def alphabet "abcdefghijklmnopqrstuvwxyz ")
(def alphabet-size (count alphabet))

(defn line
  [letter]
  (let [pos (index-of alphabet letter)]
       (let [tail (subs alphabet pos) head (subs alphabet 0 pos)]
         (str tail head)
       ))
)

(defn letter-at
  [pos]
  (nth alphabet pos))

(defn rot
  [letter cnt fun]
  (let [
        pos (index-of alphabet letter)
        new-pos (fun pos cnt)
        ]
        (mod (fun pos cnt) alphabet-size))
)

(defn rot-to-base
  [letter new-base]
  (let [
        base-pos (index-of alphabet new-base)
        ]
    (letter-at (rot letter base-pos +))
  )
)

(defn unrot-to-base
  [letter new-base]
  (let [
        base-pos (index-of alphabet new-base)
        ]
    (letter-at (rot letter base-pos -))
  )
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

(defn encoder
  [letter orig-alphabet subst-alphabet]
  (let
      [initial (index-of orig-alphabet letter)
       result (nth subst-alphabet initial)]
    result)
)

(defn encode-letter
  [vec]
  (let [specimen (line "a") subst (line (second vec))]
    (encoder (first vec) specimen subst)
    )
)

(defn rot-vector
  [vec]
  (rot-to-base (first vec) (second vec))
)

(defn unrot-vector
  [vec]
  (unrot-to-base (first vec) (second vec))
)

(defn decode-letter
  [vec]
  (let [subst (line "a") specimen (line (second vec))]
    (encoder (first vec) specimen subst)
  )
)

(defn process [key message func]
  (let [lines (map vector message key)]
    (let [result (map func lines)] (join result))
    )
)

(defn carroll-decode [key message]
  (let [k (align key message)]
    (process k message unrot-vector))
)

(defn carroll-encode [key message]
  (let [k (align key message)]
    (process k message rot-vector))
)

(defn autokey-encode [agreed-key message]
  (let [k (align-incorporate agreed-key message)]
    (process k message rot-vector))
)

(defn chunks [message key-size]
  ; can be replaced with partition-all
  (let [limit (count message)]
    (loop [start 0 result []]
      (let [tentavive-end (+ start key-size)]
        (if (> tentavive-end limit)
          (if (= start limit)
            result
            (conj result (subs message start limit)))
          (let [res (conj result (subs message start tentavive-end))]
            (recur (+ start key-size) res))
          )
        )
      )
    )
  )

(defn autokey-decode [agreed-key message]
  (let [
        k (conj nil agreed-key)
        c (chunks message (count agreed-key))
        ]
    (loop [chunks c keys k deciphered []]
      (if (= 0 (count chunks))
        (join deciphered)
        (let [
              key (first keys)
              new-key (process key (first chunks) unrot-vector)
              ]
          (recur (rest chunks) (conj keys new-key) (conj deciphered new-key))
          )
      )
)))


