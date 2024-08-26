#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input12.txt"))

(defn parse-edge [s]
  (let [[_ n1 n2] (re-matches #"([a-zA-Z]+)-([a-zA-Z]+)" s)]
    [(keyword n1) (keyword n2)]))

(defn raw->input [raw]
  (->> raw
       str/split-lines
       (map parse-edge)))

(defn get-node-map [edges]
  (let [edge->map (fn [[n1 n2]] [{n1 [n2]} {n2 [n1]}])]
    (reduce
        (fn [acc next]
          (reduce (partial merge-with into) acc (edge->map next))) {} edges)))

(defn no-revisit? [node]
  (every? #(Character/isLowerCase %) (name node)))

(defn count-paths [node-map visited?]
  (let [cp (memoize
            (fn [f from visited]
              (if (= from :end) 1
                  (let [visited (if (no-revisit? from)
                                  (update visited from (fnil inc 0))
                                  visited)]
                    (->> (for [n (node-map from)
                               :when (not (visited? n visited))]
                           (f f n visited))
                         (reduce +))))))
        m-cp (partial cp cp)]
    (m-cp :start {})))

(defn first-answer [edges]
  (-> edges
      get-node-map
      (count-paths (fn [node visited] (visited node)))))

(defn second-answer [edges]
  (-> edges
      get-node-map
      (count-paths (fn [node visited]
                     (when (visited node)
                       (or (contains? #{:start :end} node) (some #(> % 1) (vals visited))))))))

(->> raw
     raw->input
     first-answer)
