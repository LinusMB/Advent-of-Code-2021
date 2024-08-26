#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input04.txt"))

(defn raw->input [raw]
  (let [[numbers & boards] (str/split raw #"\n\n")]
    [(read-string (str "[" numbers "]"))
     (map #(read-string (str "[" % "]")) boards)]))

(defn init-boards [boards]
  (let [num->map (fn [n] {:num n :marked false})
        board->num (fn [board] (map num->map board))]
    (map board->num boards)))

(defn mark-board [board num]
  (map #(if (= num (:num %)) (assoc % :marked true) %) board))

(defn board-wins? [board]
  (boolean (or (some #(every? :marked %) (partition 5 board))
               (some #(every? :marked %) (apply map vector (partition 5 board))))))

(defn sum-unmarked [board]
  (->> board
       (remove :marked)
       (map :num)
       (reduce +)))

(defn first-answer [[numbers boards]]
  (loop [boards (init-boards boards)
         numbers numbers]
    (let [drawn (first numbers)
          boards (map #(mark-board % drawn) boards)
          {winning-boards true, remaining false} (group-by board-wins? boards)]
      (if (not-empty winning-boards)
        (* (sum-unmarked (first winning-boards)) drawn)
        (recur remaining (next numbers))))))

(defn second-answer [[numbers boards]]
  (loop [boards (init-boards boards)
         numbers numbers]
    (let [drawn (first numbers)
          boards (map #(mark-board % drawn) boards)
          {winning-boards true, remaining false} (group-by board-wins? boards)]
      (if (empty? remaining)
        (* (sum-unmarked (first winning-boards)) drawn)
        (recur remaining (next numbers))))))

(->> raw
     raw->input
     second-answer)
