#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input17.txt"))

(defn raw->input [raw]
  (let [[_ xmin xmax ymin ymax]
        (->> raw
             str/trim-newline
             (re-matches #"target area: x=(\-?\d+)..(\-?\d+), y=(\-?\d+)..(\-?\d+)"))]
    {:xmin (read-string xmin)
     :xmax (read-string xmax)
     :ymin (read-string ymin)
     :ymax (read-string ymax)})) 

(defn all-vels [{xmin :xmin xmax :xmax ymin :ymin ymax :ymax}]
  (let [step (fn [[pos vel n]] [(+ pos vel) (dec vel) (inc n)])
        xsm (reduce (fn [m x]
                      (loop [[pos vel n] [0 x 0]
                             m m]
                        (if (or (> pos xmax) (neg? vel)) m
                            (if (>= pos xmin)
                              (recur (step [pos vel n])
                                     (update-in m (if (zero? vel)
                                                    [:gt n]
                                                    [n])
                                                (fnil conj []) x))
                              (recur (step [pos vel n]) m)))))
                    {} (range (inc xmax)))
        ysm (reduce (fn [m y]
                      (loop [[pos vel n] [0 y 0]
                             m m]
                        (if (< pos ymin) m
                            (if (<= pos ymax)
                              (recur (step [pos vel n]) (update m n (fnil conj []) y))
                              (recur (step [pos vel n]) m)))))
                    {} (range ymin (abs ymin)))]
    (->> (for [yk (keys ysm)
               yv (ysm yk)
               xv (concat (xsm yk) (flatten (keep (fn [[k v]] (when (<= k yk) v)) (xsm :gt))))]
           [xv yv])
         distinct)))

(defn first-answer [area]
  (let [gauss (fn [n] (/ (* n (+ n 1)) 2))]
    (->> area
         all-vels
         (map second)
         (apply max)
         gauss)))

(defn second-answer [area]
  (->> area
       all-vels
       count))

(->> raw
     raw->input
     second-answer)
