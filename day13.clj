#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input13.txt"))

(def s-map (comp set map))

(defn raw->input [raw]
  (let [[dots instructions] (str/split raw #"\n\n")
        parse-dot (fn [s]
                    (let [[_ x y] (re-matches #"(\d+),(\d+)" s)]
                      [(Long/parseLong x) (Long/parseLong y)]))
        parse-instruction (fn [s]
                            (let [[_ axs val] (re-matches #"fold along (\w)=(\d+)" s)]
                              [(keyword axs) (Long/parseLong val)]))]
    [(->> dots
          str/split-lines
          (s-map parse-dot))
     (->> instructions
          str/split-lines
          (map parse-instruction))]))

(defn fold [dots [axs val]]
  (let [to (fn [n]
             (if (< n val) n
                 (- (* val 2) n)))]
    (case axs
      :x (s-map (fn [[x y]] [(to x) y]) dots)
      :y (s-map (fn [[x y]] [x (to y)]) dots))))

(defn do-folds [dots instructions]
  (->> instructions
       (reduce fold dots)))

(defn repr [dots]
  (let [max-x (apply max (map first dots))
        max-y (apply max (map second dots))]
    (->> (for [y (range (inc max-y))
               x (range (inc max-x))
               :let [c (if (dots [x y]) \# \.)]
               :let [eol (when (= x max-x) \newline)]]
           (str c eol))
         (apply str))))

(defn first-answer [[dots instructions]]
  (count (fold dots (first instructions))))

(defn second-answer [[dots instructions]]
  (->> instructions
       (do-folds dots)
       repr
       println))

(->> raw
     raw->input
     second-answer)
