#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input16.txt"))

(defn raw->input [raw]
  (str/trim-newline raw))

(defn btoi [s]
  (Long/parseLong s 2))

(defn hex->binary [h]
  (let [digit (fn [d]
                (let [b (-> (str d)
                            (Integer/parseInt 16)
                            (Integer/toBinaryString))]
                  (str/replace (format "%4s" b) #" " "0")))]
    (reduce (fn [acc next]
              (str acc (digit next))) "" h)))

(defn chop-at [s idx]
  [(subs s 0 idx) (subs s idx)])

(defn parse-operator-type-1 [data packet]
  (let [[len data] (chop-at data 11)
        packet (assoc packet :len len)]
    (loop [data data
           num (btoi len)
           packet packet]
      (let [[left subpkt] (parse-packet data)
            packet (update packet :payload (fnil conj []) subpkt)
            num (dec num)]
        (if (< num 1)
          [left packet]
          (recur left num packet))))))

(defn parse-operator-type-0 [data packet]
  (let [[len data] (chop-at data 15)
        packet (assoc packet :len len)]
    (loop [data data
           bits (btoi len)
           packet packet]
      (let [[left subpkt] (parse-packet data)
            packet (update packet :payload (fnil conj []) subpkt)
            read (- (count data) (count left))
            bits (- bits read)]
        (if (< bits 1)
          [left packet]
          (recur left bits packet))))))

(defn parse-operator [data packet]
  (let [[len-id data] (chop-at data 1)
        packet (assoc packet :len-id len-id)]
    (case len-id
      "0" (parse-operator-type-0 data packet)
      "1" (parse-operator-type-1 data packet))))

(defn parse-literal [data packet]
  (loop [data data
         result ""]
    (let [[group data] (chop-at data 5)
          [f num] (chop-at group 1)
          result (str result num)]
      (if (= f "0")
        [data (assoc packet :payload result)]
        (recur data result)))))

(defn parse-packet [data]
   (let [[version data] (chop-at data 3)
         [id data] (chop-at data 3)
         packet {:version version :id id}]
     (case id
       "100" (parse-literal data packet)
       (parse-operator data packet))))

(defn sum-verns [packet]
  (->> packet
       (tree-seq #(not= (:id %) "100") :payload)
       (keep :version)
       (map btoi)
       (reduce +)))

(defn eval-packet [packet]
  (case (btoi (:id packet))
    4 (btoi (:payload packet))
    0 (apply + (map eval-packet (:payload packet)))
    1 (apply * (map eval-packet (:payload packet)))
    2 (apply min (map eval-packet (:payload packet)))
    3 (apply max (map eval-packet (:payload packet)))
    5 (apply #(if (> %1 %2) 1 0) (map eval-packet (:payload packet)))
    6 (apply #(if (< %1 %2) 1 0) (map eval-packet (:payload packet)))
    7 (apply #(if (= %1 %2) 1 0) (map eval-packet (:payload packet)))))

(defn first-answer [hex]
  (-> hex
      hex->binary
      parse-packet
      second
      sum-verns))

(defn second-answer [hex]
  (-> hex
      hex->binary
      parse-packet
      second
      eval-packet))

(-> raw
    raw->input
    second-answer)
