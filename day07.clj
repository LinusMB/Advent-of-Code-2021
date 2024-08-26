#!/usr/bin/env bb

(def raw (slurp "input07.txt"))

(defn raw->input [raw]
  (read-string (str "[" raw "]")))

(defn round [n]
  (->> n
       double
       Math/round))

(defn round-down [n]
  (->> n
       long))

(defn round-up [n]
  (->> n
       long
       inc))

(defn median [xs]
  (let [xs (sort xs)
        n (count xs)
        mid-idx (quot n 2)
        mid (nth xs mid-idx)]
    (cond
      (odd? n) mid
      (even? n) (/ (+ mid (nth xs (dec mid-idx))) 2))))

(defn mean [xs]
  (/ (reduce + xs) (count xs)))

(defn first-answer [input]
  (let [fuel-cost (fn [xs pos]
                    (->> xs
                         (map #(Math/abs (- % pos)))
                         (reduce +)))]
    (fuel-cost input (median input))))

(defn second-answer [input]
  (let [fuel-cost (fn [xs pos]
                    (let [cost (fn [x pos]
                                 (let [d (Math/abs (- x pos))]
                                   (/ (* d (inc d)) 2)))]
                      (->> xs
                           (map #(cost % pos))
                           (reduce +))))]
    (min
     (fuel-cost input (round-down (mean input)))
     (fuel-cost input (round-up (mean input))))))

(->> raw
     raw->input
     second-answer)
