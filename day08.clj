#!/usr/bin/env bb

(require '[clojure.string :as str]
         '[clojure.set :as set])

(def raw (slurp "input08.txt"))

(defn raw->input [raw]
  (->> (str/split-lines raw)
       (map (fn [line]
              (-> line
                  (str/replace #"^(.*)\|(.*)$" "[[$1][$2]]")
                  (str/replace #"(\w+)" "\"$1\"")
                  (read-string))))))

(defn create-num-map [seg-counts]
  (let
      [num-map (atom {})]
    (swap! num-map
           merge {8 (first (seg-counts 7))
                  7 (first (seg-counts 3))
                  4 (first (seg-counts 4))
                  1 (first (seg-counts 2))})
    (swap! num-map
           assoc 9 (set/union
                    (set/union (@num-map 4) (@num-map 7))
                    (apply set/intersection (seg-counts 5))))
    (swap! num-map
           assoc 0 (set/difference
                    (@num-map 8)
                    (set/intersection (@num-map 4)
                                      (apply set/intersection (seg-counts 5)))))
    (swap! num-map
           assoc 3 (set/union
                    (apply set/intersection (seg-counts 5))
                    (@num-map 1)))
    (swap! num-map
           assoc 6 (set/union
                    (apply set/intersection (seg-counts 6))
                    (set/difference (@num-map 8) (@num-map 0))
                    (set/difference (@num-map 8) (@num-map 9))))
    (swap! num-map
           assoc 5 (set/difference
                    (@num-map 9)
                    (set/difference (@num-map 8) (@num-map 6))))
    (swap! num-map
           assoc 2 (set/union
                    (set/difference (@num-map 3) (@num-map 1))
                    (set/difference (@num-map 8) (@num-map 6))
                    (set/difference (@num-map 8) (@num-map 9))))
    (set/map-invert @num-map)))


(defn first-answer [input]
  (->> input
       (map (fn [line]
              (->> (second line)
                   (filter #(.contains [2 4 3 7] (count %))))))
       flatten
       count))

(defn second-answer [input]
  (->> input
       (map (fn [line]
              (let [num-map (->> (first line)
                                 (map set)
                                 (group-by count)
                                 create-num-map)
                    output (map set (second line))]
                (reduce (fn [acc c] (str acc (num-map c))) "" output))))
       (map #(Integer/parseInt %))
       (reduce +)))

(->> raw
     raw->input
     second-answer)
