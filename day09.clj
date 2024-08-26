#!/usr/bin/env bb

(require '[clojure.string :as str]
         '[clojure.set :as set])

(def raw (slurp "input09.txt"))

(defn raw->input [raw]
  (let [ncols (str/index-of raw "\n")
        arr (->> raw
                 (str/join ",")
                 (#(read-string (str "[" % "]"))))]
    [ncols arr]))

(defn adjacent-idx [i size ncols]
  (let [nrows (/ size ncols)
        row (quot i ncols)
        col (rem i ncols)
        left (when (not= col 0) (dec i))
        right (when (not= col (dec ncols)) (inc i))
        up (when (not= row 0) (- i ncols))
        down (when (not= row (dec nrows)) (+ i ncols))]
    (remove nil? [left, right, up, down])))

(defn adjacent [i arr ncols]
  (map arr (adjacent-idx i (count arr) ncols)))

(defn low-point? [point adjacent]
  (every? (partial < point) adjacent))

(defn low-points [arr ncols]
  (filter #(low-point? (arr %) (adjacent % arr ncols)) (range (count arr))))

(defn basin
  ([i arr ncols] (basin i arr ncols #{}))
  ([i arr ncols acc]
   (if (or (= 9 (arr i)) (contains? acc i))
     acc
     (let [nbs (adjacent-idx i (count arr) ncols)]
       (reduce (fn [acc nxt] (set/union acc (basin nxt arr ncols acc))) (conj acc i) nbs)))))

(defn first-answer [[ncols arr]]
  (->> (low-points arr ncols)
       (map (comp inc arr))
       (apply +)))

(defn second-answer [[ncols arr]]
  (->> (low-points arr ncols)
       (map #((comp count basin) % arr ncols))
       sort
       (take-last 3)
       (apply *)))

(->> raw
     raw->input
     second-answer)
