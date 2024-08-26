#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input03.txt"))

(defn raw->input [raw]
  (str/split-lines raw))

(defn btoi [s]
  (Long/parseLong s 2))

(defn first-answer [input]
  (let [tm (apply map str input)
        flip-bits (fn [bits]
                    (->> bits
                         (replace {\0 \1, \1 \0})
                         (apply str)))
        most-common (fn [bits]
                      (->> bits
                           frequencies
                           (apply max-key val)
                           first))
        gamma-rate (reduce (fn [acc v] (str acc (most-common v))) "" tm)
        epsilon-rate (flip-bits gamma-rate)]
    (* (btoi gamma-rate) (btoi epsilon-rate))))

(defn get-rating [ns op]
  (let
      [bitcol (fn [ns n] (map #(nth % n) ns))
       select-bit (fn [bits]
                    (let [fqs (frequencies bits)]
                      (if (op (fqs \1) (fqs \0)) \1 \0)))
       select-bit-at (comp select-bit bitcol)
       filter-on-bit (fn [ns bitpos]
                       (let [bit (select-bit-at ns bitpos)]
                         (filter #(= (nth % bitpos) bit) ns)))]
    (loop [ns ns
           bitpos 0]
      (if (= (count ns) 1)
        (first ns)
        (recur (filter-on-bit ns bitpos) (inc bitpos))))))

(defn second-answer [input]
  (let
      [o-rating (get-rating input >=)
       c-rating (get-rating input <)]
    (* (btoi o-rating) (btoi c-rating ))))

(->> raw
     raw->input
     second-answer)
