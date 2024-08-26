#!/usr/bin/env clojure

(require '[clojure.string :as str])
(import Jama.Matrix)

(def raw (slurp "input19.txt"))

(defn raw->input [raw]
  (let [parse-scanner
        (fn [s]
          (->> (str/replace-first s #"^.*?\n" "")
               str/split-lines
               (mapv #(read-string (str "[" % "]")))))]
    (->> (str/split raw #"\n\n")
         (map parse-scanner))))

(defn double-array-2d [arr]
  (into-array (map double-array arr)))

(defn solve-les [lhs rhs nvar]
  (let [A (Matrix. (double-array-2d lhs))
        B (Matrix. (double-array rhs) (count lhs))
        ans (.solve A B)]
    (map #(Math/round (.get ans % 0)) (range nvar))))

(defn get-transform-matrix [beacs-a beacs-b]
  (let [lhs (mapv #(conj % 1) beacs-b)
        nvar 4]
    [(solve-les lhs (mapv #(get % 0) beacs-a) nvar)
     (solve-les lhs (mapv #(get % 1) beacs-a) nvar)
     (solve-les lhs (mapv #(get % 2) beacs-a) nvar)
     [0 0 0 1]]))

(defn get-relative [m beac]
  (let [dim (count beac)
        A (Matrix. (double-array-2d m))
        B (Matrix. (double-array (conj beac 1)) (inc dim))
        ans (.times A B)]
    (mapv #(Math/round (.get ans % 0)) (range dim))))

(defn bag [& args]
  (frequencies args))

(def distance-map
  (memoize
   (fn [scanner]
     (let [sz (count scanner)]
       (reduce
        (fn [m [[x1 y1 z1 :as a] [x2 y2 z2 :as b]]]
          (let [dst (bag (abs (- x2 x1)) (abs (- y2 y1)) (abs (- z2 z1)))]
            (-> m
                (update a update dst (fnil conj []) b)
                (update b update dst (fnil conj []) a)))) {}
        (for [i (range sz)
              j (range (inc i) sz)] [(scanner i) (scanner j)]))))))

(defn keys-intxn [m1 m2]
  (keys (select-keys m1 (keys m2))))

(defn overlap [scanner-a scanner-b]
  (let [dm-a (distance-map scanner-a)
        dm-b (distance-map scanner-b)]
    (letfn [(match? [a b n [beacs-a beacs-b :as beacs]]
              (let [dss-a (dm-a a)
                    dss-b (dm-b b)
                    ks (keys-intxn dss-a dss-b)]
                (cond
                  (<= n 1) beacs
                  (empty? ks) nil
                  :else
                  (->> (for [k ks
                             a (remove #(.contains beacs-a %) (dss-a k))
                             b (remove #(.contains beacs-b %) (dss-b k))] [a b])
                       (some
                        (fn [[a b]]
                          (match? a b (dec n)
                                  [(conj beacs-a a) (conj beacs-b b)])))))))]
      (->> (for [a scanner-a
                 b scanner-b] [a b])
           (some
            (fn [[a b]]
              (match? a b 12 [[a][b]])))))))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn collect-beacs-and-locs [scanners]
  (loop [beacs (into #{} (first scanners))
         locs #{[0 0 0]}
         active (conj (queue) (first scanners))
         unsearched (rest scanners)]
    (if (empty? unsearched)
      [beacs locs]
      (let [a (peek active)
            active (pop active)
            [beacs locs active unsearched]
            (reduce
             (fn [[beacs locs active unsearched] b]
               (if-let [[beacs-a beacs-b] (overlap a b)]
                 (let [[[_ _ _ x]
                        [_ _ _ y]
                        [_ _ _ z] :as m] (get-transform-matrix beacs-a beacs-b)
                       b-rel (mapv #(get-relative m %) b)]
                   [(into beacs b-rel) (conj locs [x y z]) (conj active b-rel) unsearched])
                 [beacs locs active (conj unsearched b)]))
             [beacs locs active (queue)]
             unsearched)]
        (recur beacs locs active unsearched)))))

(defn first-answer [scanners]
  (let [[beacs] (collect-beacs-and-locs scanners)]
    (count beacs)))

(defn second-answer [scanners]
  (let [[_ locs] (collect-beacs-and-locs scanners)]
    (apply max (for [[x1 y1 z1] locs
                     [x2 y2 z2] locs]
                 (+ (abs (- x2 x1)) (abs (- y2 y1)) (abs (- z2 z1)))))))

(->> raw
     raw->input
     first-answer)
