#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input05.txt"))

(defrecord Point [x y])

(defn raw->input [raw]
  (map
   (fn
     [[_ x1 y1 x2 y2]]
     [(->Point (read-string x1) (read-string y1))
      (->Point (read-string x2) (read-string y2))])
   (re-seq #"(\d+),(\d+) -> (\d+),(\d+)" raw)))

(defn segments [{x1 :x y1 :y} {x2 :x y2 :y}]
  (let [prange (fn [a b]
                 (concat
                  (range a b (if (< a b) 1 -1))
                  (list b)))
        xs (prange x1 x2)
        ys (prange y1 y2)]
    (if (= (count xs) (count ys))
      (map ->Point xs ys)
      (for [x xs
            y ys]
        (->Point x y)))))

(defn filter-hor-ver [lines]
  (filter
   (fn [[{x1 :x y1 :y} {x2 :x y2 :y}]] (or (= x1 x2) (= y1 y2)))
   lines))

(defn first-answer [input]
  (->> input
       filter-hor-ver
       (reduce (fn [a c] (into a (apply segments c))) [])
       frequencies
       (filter #(>= (val %) 2))
       (count)))

(defn second-answer [input]
  (->> input
       (reduce (fn [a c] (into a (apply segments c))) [])
       frequencies
       (filter #(>= (val %) 2))
       count))

(->> raw
     raw->input
     second-answer)
