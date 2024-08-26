#!/usr/bin/env bb

(def raw (slurp "input06.txt"))

(defn raw->input [raw]
  (read-string (str "[" raw "]")))

(def f
  (memoize (fn [s d]
             (if (>= s d)
               1
               (+ (f 6 (- d (inc s)))
                  (f 8 (- d (inc s))))))))

(defn first-answer [input]
  (reduce + (map #(f % 80) input)))

(defn second-answer [input]
  (reduce + (map #(f % 256) input)))

(->> raw
     raw->input
     second-answer)
