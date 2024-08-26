#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input20.txt"))

(defn raw->input [raw]
  (let [[algo image] (str/split raw #"\n\n")]
    [(str/replace algo #"\n" "")
     (str/split-lines image)]))

(defn btoi [s]
  (Long/parseLong s 2))

(def square->pixel
  (memoize
   (fn [square algo]
     (->> square
          (map {\. \0 \# \1})
          (apply str)
          btoi
          (get algo)))))

(defn output-pixel [[row col] [image sen] algo]
  (-> (for [r [(dec row) row (inc row)]
            c [(dec col) col (inc col)]]
        (get-in image [r c] sen))
      (square->pixel algo)))

(defn step [algo [image sen]]
  (let [nrows (count image)
        ncols (count (first image))]
    [(->> (for [r (range -1 (inc nrows))
                c (range -1 (inc ncols))]
            (output-pixel [r c] [image sen] algo))
          (partition (+ 2 ncols))
          (mapv #(apply str %)))
     (get algo ({\. 0 \# 511} sen))]))

(defn n-times [f x n]
  (loop [x x, n (dec n)]
    (let [fx (f x)]
      (if (zero? n) fx (recur fx (dec n))))))

(defn count-lit [image]
  (->> image
       (apply str)
       (filter #{\#})
       count))

(defn first-answer [[algo image]]
  (count-lit (n-times #(step algo %) [image \.] 2)))

(defn second-answer [[algo image]]
  (count-lit (n-times #(step algo %) [image \.] 50)))

(->> raw
     raw->input
     second-answer)
