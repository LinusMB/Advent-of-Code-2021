(require '[clojure.string :as str])

(def raw (slurp "input25.txt"))

(defn raw->input [raw]
  (->> raw
       str/split-lines
       (map str/trim)
       (mapv vec)))

(defn move [grid face]
  (let [num-rows (count grid)
        num-cols (count (first grid))
        check (fn [[r c]]
                (case face
                  \> [r (mod (inc c) num-cols)]
                  \v [(mod (inc r) num-rows) c]))]
    (->> (for [r (range num-rows)
               c (range num-cols)
               :when (= (get-in grid [r c]) face)
               :let [nxt (check [r c])]
               :when (= (get-in grid nxt) \.)] [[r c] nxt])
         (reduce
           (fn [grid [prev nxt]]
             (-> grid
                 (assoc-in prev \.)
                 (assoc-in nxt face)))
           grid))))

(defn step [grid]
  (-> grid
      (move \>)
      (move \v)))

(defn fp [f x]
  (loop [x x
         i 1]
    (let [fx (f x)]
      (if (= fx x) [x i]
        (recur fx (inc i))))))

(defn solve [grid]
  (->> grid
       (fp step)
       second))

(-> raw
    raw->input
    solve)
