#!/usr/bin/env clojure

(require '[clojure.string :as str]
         '[clojure.data.priority-map :refer [priority-map]])

(def raw (slurp "input15.txt"))

(defn raw->input [raw]
  (->> raw
       str/split-lines
       (mapv #(read-string (str "[" (str/join "," %) "]")))))

(defn adjacent [grid [row col]]
  (let [nrows (count grid)
        ncols (count (first grid))]
    (->> [(when (not= row 0) [(dec row) col])
          (when (not= col (dec ncols)) [row (inc col)])
          (when (not= row (dec nrows)) [(inc row) col])
          (when (not= col 0) [row (dec col)])]
         (remove nil?))))

(defn relax [grid pq scanned [cur v]]
  (let [nbs (remove scanned (adjacent grid cur))
        upd (fn [pq k]
              (let [old (get pq k)
                    new? (+ v (get-in grid k))]
                (if (and old (>= new? old)) pq
                    (assoc pq k new?))))]
    (reduce upd pq nbs)))

(defn scan [pq scanned]
  (let [[n v] (peek pq)]
    [[n v]
     (pop pq)
     (conj scanned n)]))

(defn shortest-path [grid start end]
  (loop [pq (priority-map start 0)
         scanned #{}]
    (let [[[cur v] pq scanned] (scan pq scanned)]
      (if (= cur end) v
          (recur (relax grid pq scanned [cur v]) scanned)))))

(defn solve [grid]
  (let [nrows (count grid)
        ncols (count (first grid))
        start [0 0]
        end [(dec nrows) (dec ncols)]]
    (shortest-path grid start end)))

(defn enlarge-grid [grid]
  (let [inc-n (fn [n i] (inc (mod (+ (dec n) i) 9)))
        nrows (count grid)
        ncols (count (first grid))]
    (->> (for [r (range (* 5 nrows))
               c (range (* 5 ncols))
               :let [fac (+ (quot r nrows) (quot c ncols))]
               :let [v (let [l (get-in grid [(rem r nrows) (rem c ncols)])]
                         (inc-n l fac))]]
           v)
         (partition (* 5 ncols))
         (mapv vec))))

(defn first-answer [grid]
  (solve grid))

(defn second-answer [grid]
  (solve (enlarge-grid grid)))

(-> raw
    raw->input
    second-answer)
