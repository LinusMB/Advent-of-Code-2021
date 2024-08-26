#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input01.txt"))

(defn raw->input [raw]
  (read-string (str "[" raw "]")))

(defn count-inc [v]
  (reduce + (map #(if (> %2 %1) 1 0) v (next v))))

(defn first-answer [input]
  (count-inc input))

(defn second-answer [input]
  (count-inc (map + input (next input) (nnext input))))

(->> raw
     raw->input
     second-answer)
