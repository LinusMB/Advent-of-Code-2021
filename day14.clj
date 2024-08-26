#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input14.txt"))

(defn raw->input [raw]
  (let [[template rules] (str/split raw #"\n\n")
        parse-template (fn [s] (mapv (comp keyword str) s))
        parse-rule (fn [s]
                     (let [[_ from to] (re-matches #"(\w+) -> (\w)" s)]
                       {(map (comp keyword str) from) (keyword to)}))]
    [(parse-template template)
     (->> rules
          str/split-lines
          (map parse-rule)
          (reduce merge))]))

(defn step [[pairs els] rules]
  (->> pairs
       keys
       (reduce (fn [[p e] [left right]]
                 (let [insert (rules [left right])
                       times (pairs [left right])]
                   [(-> p
                        (update [left insert] (fnil + 0) times)
                        (update [insert right] (fnil + 0) times))
                    (-> e
                        (update insert (fnil + 0) times))]))
               [{} els])))

(defn solve [template rules num-steps]
  (let [pairs (->> template
                   (partition 2 1)
                   frequencies)
        els (frequencies template)
        freqs (->> [pairs els]
                   (iterate #(step % rules))
                   (#(nth % num-steps))
                   second)
        [_ mc] (apply max-key val freqs)
        [_ lc] (apply min-key val freqs)]
    (- mc lc)))

(defn first-answer [[template rules]]
  (solve template rules 10))

(defn second-answer [[template rules]]
  (solve template rules 40))

(->> raw
     raw->input
     second-answer)
