#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input02.txt"))

(defn raw->input [raw]
  (let [parse-command (fn [s]
                        (let [[dir n] (str/split s #" ")]
                          [(keyword dir) (read-string n)]))]
    (->> raw
         (str/split-lines)
         (map parse-command))))

(defn first-answer [input]
  (let [nav (fn [[h d] [dir n]]
              (case dir
                :forward [(+ h n) d]
                :down [h (+ d n)]
                :up [h (- d n)]))]
    (->> (reduce nav [0 0] input)
         (reduce *))))

(defn second-answer [input]
  (let [nav (fn [[h d a] [dir n]]
              (case dir
                :forward [(+ h n) (+ d (* a n)) a]
                :down [h d (+ a n)]
                :up [h d (- a n)]))]
    (->> (reduce nav [0 0 0] input)
         (apply (fn [h d _] (* h d))))))

(->> raw
     raw->input
     second-answer)
