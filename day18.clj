#!/usr/bin/env bb

(def raw (slurp "input18.txt"))

(defn raw->input [raw]
  (read-string (str "[" raw "]")))

(defn nearest [x y n ks]
  (if-let [parent (loop [ks ks]
                    (cond
                      (empty? ks) nil
                      (= x (peek ks)) (recur (pop ks))
                      :else (conj (pop ks) x)))]
    (loop [ks parent]
      (if (coll? (get-in n ks)) (recur (conj ks y)) ks))))

(def nearest-right (partial nearest 1 0))
(def nearest-left (partial nearest 0 1))

(defn update-in? [m ks & args]
  (if (nil? ks) m
      (apply update-in m ks args)))

(defn explode [n ks]
  (let [lks (conj ks 0)
        rks (conj ks 1)
        left-nb (nearest-left n lks)
        right-nb (nearest-right n rks)]
    (-> n
        (update-in? left-nb + (get-in n lks))
        (update-in? right-nb + (get-in n rks))
        (assoc-in ks 0))))

(defn split [n ks]
  (let [x (/ (get-in n ks) 2)
        round-down #(long (Math/floor %))
        round-up #(long (Math/ceil %))]
    (assoc-in n ks [(round-down x) (round-up x)])))

(defn simple-pair? [n] (and (coll? n) (every? number? n)))

(defn leaf? [n] (or (number? n) (simple-pair? n)))

(defn explode? [n ks]
  (and (simple-pair? (get-in n ks)) (>= (count ks) 4)))

(defn split? [n ks]
  (let [v (get-in n ks)]
    (and (number? v) (>= v 10))))

(defn key-seqs [v]
  (->> (tree-seq
        (fn [is] (coll? (get-in v is)))
        (fn [is]
          (map-indexed (fn [i _] (conj is i)) (get-in v is)))
        [])
       (filter #(leaf? (get-in v %)))))

(defn fp [f x]
  (let [fx (f x)]
    (if (= fx x) x
        (recur f fx))))

(defn add [a b]
  (fp (fn [n]
        (let [kss (key-seqs n)]
          (if-let [ks (some (fn [ks] (when (explode? n ks) ks)) kss)]
            (explode n ks)
            (if-let [ks (some (fn [ks] (when (split? n ks) ks)) kss)]
              (split n ks)
              n))))
      (vector a b)))

(defn magnitude [n]
  (if (number? n) n
      (let [[l r] n]
        (+ (* 3 (magnitude l)) (* 2 (magnitude r))))))

(defn first-answer [ns]
  (->> ns
       (reduce add)
       magnitude))

(defn second-answer [ns]
  (->> (for [a ns
             b ns]
         (magnitude (add a b)))
       (apply max)))

(->> raw
     raw->input
     second-answer)
