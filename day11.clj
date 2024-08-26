#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input11.txt"))

(defn raw->input [raw]
  (->> raw
       str/split-lines
       (mapv #(read-string (str "[" (str/join "," %) "]")))))

(defn stepinc [i]
  (case i
    (9 :flash) :flash
    :flashed :flashed
    (inc i)))

(defn step-update [grid] (mapv (partial mapv stepinc) grid))

(defn adjacent [row col nrows ncols]
  (assert (and (< row nrows) (< col ncols)))
  (let [clean (partial filter identity)
        vert (clean [(when (not= row 0) (dec row))
                     row
                     (when (not= row (dec nrows)) (inc row))])
        horz (clean [(when (not= col 0) (dec col))
                     col
                     (when (not= col (dec ncols)) (inc col))])]
    (for [r vert
          c horz
          :when (not= [r c] [row col])] [r c])))

(defn flash-update [grid]
  (let [grid-new (atom grid)
        nrows (count @grid-new)
        ncols (count (first @grid-new))]
    (while (some #(.contains % :flash) @grid-new)
      (doseq [r (range nrows)
              c (range ncols)
              :when (= :flash (get-in @grid-new [r c]))]
        (swap! grid-new assoc-in [r c] :flashed)
        (doseq [adj (adjacent r c nrows ncols)]
          (swap! grid-new update-in adj stepinc))))
    @grid-new))

(defn reset-grid [grid]
  (mapv (fn [row] (mapv #(if (= % :flashed) 0 %) row)) grid))

(defn step [grid]
  (->> grid
       step-update
       flash-update
       reset-grid))

(defn count-flashes [grid]
  (->> grid
       flatten
       (filter zero?)
       count))

(defn count-flashes-n [grid n]
  (->> grid
       (iterate step)
       next
       (take n)
       (reduce (fn [acc next] (+ acc (count-flashes next))) 0)))

(defn first-answer [grid]
  (count-flashes-n grid 100))

(defn second-answer [grid]
  (let [n (count (flatten grid))]
    (->> grid
         (iterate step)
         next
         (take-while #(not= n (count-flashes %)))
         count
         inc)))

(-> raw
    raw->input
    second-answer)
