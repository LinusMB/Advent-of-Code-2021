#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input10.txt"))

(def raw->input str/split-lines)

(def get-closing {\( \), \[ \], \< \>, \{ \}})
(def closing? (partial contains? #{\) \] \> \}}))
(def opening? (partial contains? #{\( \[ \< \{}))
(def syntax-score {\) 3, \] 57, \} 1197, \> 25137})
(def completion-score {\) 1, \] 2, \} 3, \> 4})

(defn report
  ([string] (report '() string))
  ([stack [ch & rest]]
   (cond
     (nil? ch) stack
     (opening? ch) (report (conj stack ch) rest)
     (closing? ch) (if (= ch (get-closing (first stack)))
                     (report (next stack) rest)
                     ch))))

(defn first-answer [lines]
  (->> lines
       (map report)
       (filter closing?)
       (map syntax-score)
       (reduce +)))

(defn second-answer [lines]
  (->> lines
       (map report)
       (remove closing?)
       (map (fn [stack]
              (->> stack
                   (map get-closing)
                   (reduce (fn [acc next] (+ (completion-score next) (* acc 5))) 0))))
       sort
       ((fn [ss] (nth ss (quot (count ss) 2))))))

(->> raw
     raw->input
     second-answer)
