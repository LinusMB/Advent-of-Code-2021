#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input24.txt"))

(defn parse-instruction [line]
  (let [[_ inst varl varl-or-num] (re-matches #"(\w+) ([wxyz])(?:$| (-?\d+|[wxyz]))" line)]
    (cond-> [(keyword inst)
             (keyword varl)]
      (not-empty varl-or-num) (conj (if (number? (read-string varl-or-num))
                                      (read-string varl-or-num)
                                      (keyword varl-or-num))))))

(defn raw->input [raw]
  (->> raw
       (str/split-lines)
       (map str/trim)
       (map parse-instruction)))

(defn chop-first [[v & vs]]
  [v (vec vs)])

(defn execute [[op dst src :as inst] state input]
  (let [src-value (fn [src]
                    (cond
                      (number? src) src
                      (keyword? src) (state src)))]
    (case op
      :inp (let [[n input] (chop-first input)]
             [(assoc state dst n) input])
      :add [(update state dst (fn [n] (+ n (src-value src)))) input]
      :mul [(update state dst (fn [n] (* n (src-value src)))) input]
      :div [(update state dst (fn [n] (quot n (src-value src)))) input]
      :mod [(update state dst (fn [n] (mod n (src-value src)))) input]
      :eql [(update state dst (fn [n] ({true 1 false 0} (= n (src-value src))))) input])))

(defn program [instructions input]
  (let
    [state {:w 0 :x 0 :y 0 :z 0}]
    (reduce
      (fn [[state input] inst]
        (execute inst state input))
      [state input]
      instructions)))

;; assume that all other values (not captured by t, a, b) are constant accross problem inputs
(defn parse-instb [[[_ _]
                    [_ _ _]
                    [_ _ _]
                    [_ _ _]
                    [_ _ t]
                    [_ _ a]
                    [_ _ _]
                    [_ _ _]
                    [_ _ _]
                    [_ _ _]
                    [_ _ _]
                    [_ _ _]
                    [_ _ _]
                    [_ _ _]
                    [_ _ _]
                    [_ _ b]
                    [_ _ _]
                    [_ _ _]]]
  [({1 :type-1 26 :type-2} t) a b])

(defn quot-mod [a b]
  [(quot a b) (mod a b)])

(defn prev-z 
  ([instbs input-range]
   (->> input-range
        (some (fn [i] (prev-z i 0 instbs input-range)))))
  ([inp next-z [instb & instbs] input-range]
   (let [collect-inps (fn [prev-z?]
                        (->> input-range
                             (some (fn [i] (prev-z i prev-z? instbs input-range)))
                             ((fn [pi] (and pi (into pi [inp]))))))
         [typ a b] (parse-instb instb)]
     (if (empty? instbs)
       (when (= next-z (+ inp b)) [inp])
       (case typ
         :type-1
         (let [[prev-z? m] (quot-mod (- next-z (+ inp b)) 26)]
           (when (zero? m) (collect-inps prev-z?)))
         :type-2
         (or
           (let [prev-z? (- next-z (+ inp b))]
             (when (and (pos? prev-z?)
                        (not (= inp (+ (mod prev-z? 26) a))))
               (collect-inps prev-z?)))
           (->> (range (* next-z 26) (* (inc next-z) 26))
                (some (fn [prev-z?]
                        (when (= inp (+ (mod prev-z? 26) a))
                          (collect-inps prev-z?)))))))))))

(def prev-z (memoize prev-z))

(defn first-answer [instructions]
  (let [instbs (->> instructions
                    (partition 18)
                    reverse)
        input-range (range 9 0 -1)]
    (prev-z instbs input-range)))

(defn second-answer [instructions]
  (let [instbs (->> instructions
                    (partition 18)
                    reverse)
        input-range (range 1 10)]
    (prev-z instbs input-range)))

(->> raw
     raw->input
     second-answer)
