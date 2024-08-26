#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input21.txt"))

(defn raw->input [raw]
  (let [[player-a player-b] (str/split raw #"\n")
        [_ pos-a] (re-matches #"Player 1 starting position: (\d+)" player-a)
        [_ pos-b] (re-matches #"Player 2 starting position: (\d+)" player-b)]
    [(read-string pos-a) (read-string pos-b)]))

(defn add-in-ring [start end a b]
  (inc (mod (+ (- a start) b) end)))

(def add-in-1-10 (partial add-in-ring 1 10))

(defn deterministic-die []
  (->> (range 1 101)
       cycle
       (partition 3)
       (map (partial reduce +))))

(defn first-answer [[pos-a pos-b]]
  (let [die (deterministic-die)
        positions (fn [pos die]
                    (rest (reductions (fn [pos roll] (add-in-1-10 pos roll)) pos die)))
        poss (interleave (positions pos-a (take-nth 2 die))
                         (positions pos-b (take-nth 2 (rest die))))]
    (loop [score-a 0
           score-b 0
           n 0]
      (if (>= score-b 1000) (* score-a (* n 3))
          (recur score-b (+ score-a (nth poss n)) (inc n))))))

(def count-wins 
  (let [fact (frequencies
              (for [x (range 1 4)
                    y (range 1 4)
                    z (range 1 4)]
                (+ x y z)))
        possible (keys fact)]
    (fn
      ([pos] (count-wins [] 0 pos 1))
      ([rolls score pos npaths]
       (if (>= score 21)
         {(count rolls) npaths}
         (reduce
          (fn [acc v]
            (merge-with + acc
             (let [rolls (conj rolls v)
                   pos (add-in-1-10 pos v)
                   score (+ score pos)]
               (count-wins rolls score pos (* npaths (fact v))))))
          {} possible))))))

(defn nperms [num wmap]
  (let [fact (* 3 3 3)]
    (reduce (fn [np n] (- (* np fact) (get wmap n 0))) 1 (range 1 (inc num)))))

(defn second-answer [[pos-a pos-b]]
  (let [a-wmap (count-wins pos-a)
        b-wmap (count-wins pos-b)
        a-wins (reduce-kv
                (fn [np k v]
                  (+ np (* v (nperms (dec k) b-wmap))))
                0 a-wmap)
        b-wins (reduce-kv
                (fn [np k v]
                  (+ np (* v (nperms k a-wmap))))
                0 b-wmap)]
    (max a-wins b-wins)))

(->> raw
     raw->input
     first-answer)
