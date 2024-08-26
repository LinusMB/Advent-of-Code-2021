#!/usr/bin/env bb

(require '[clojure.string :as str])

(def raw (slurp "input23.txt"))


(def amph-cost {:A 1 :B 10 :C 100 :D 1000})

(def amph-cols {:A 2 :B 4 :C 6 :D 8})

(def hallway-locs [[0 0] [0 1] [0 3] [0 5] [0 7] [0 9] [0 10]])

(defn in-right-room? [bur [r c :as loc]]
  (let [amph (get-in bur loc)]
    (and
      (pos? r)
      (= (amph-cols amph) c))))

(defn same-room? [[_ c1] [_ c2]]
  (= c1 c2))

(defn in-hallway? [[r _]]
  (zero? r))

(defn in-room? [[r c]]
  (and
    (pos? r)
    (#{2 4 6 8} c)))

(defn move-up [[r c]]
  [(dec r) c])

(defn move-down [[r c]]
  [(inc r) c])

(defn move-left [[r c]]
  [r (dec c)])

(defn move-right [[r c]]
  [r (inc c)])

(defn above? [[r1 _] [r2 _]]
  (< r1 r2))

(defn below? [[r1 _] [r2 _]]
  (> r1 r2))

(defn to-left? [[_ c1] [_ c2]]
  (< c1 c2))

(defn to-right? [[_ c1] [_ c2]]
  (> c1 c2))

(defn occupied? [bur loc]
  (not= (get-in bur loc) :0))

(defn next-move-on-path [from to]
  (cond
    (= from to) to
    (and (not (same-room? from to)) (in-room? from)) (move-up from)
    (and (same-room? from to) (above? from to)) (move-down from)
    (and (same-room? from to) (below? from to)) (move-up from)
    (and (to-left? from to) (in-hallway? from)) (move-right from)
    (and (to-right? from to) (in-hallway? from)) (move-left from)))

(defn move-no-obstacle? [bur from to]
  (loop [cur from]
    (if (= cur to) true
      (let [next-cur (next-move-on-path cur to)]
        (if (occupied? bur next-cur) false
          (recur next-cur))))))

(defn amph-room-last [amph bur]
  [(dec (count bur)) (amph-cols amph)])

(defn amph-goal-loc [bur loc]
  (let [amph (get-in bur loc)
        start (amph-room-last amph bur)]
    (loop [cur start]
      (assert (not (in-hallway? cur)) "unreachable")
      (if (or (= cur loc)
              (not= (get-in bur cur) amph))
        cur
        (recur (move-up cur))))))

(defn amph-set? [bur loc]
  (= (amph-goal-loc bur loc) loc))

(defn amph-waits-for [bur loc]
  (let [to (amph-goal-loc bur loc)]
    (loop [waits-for []
           cur loc]
      (if (= cur to) waits-for
        (let [next-cur (next-move-on-path cur to)
              waits-for (if (occupied? bur next-cur) (conj waits-for next-cur) waits-for)]
          (recur waits-for next-cur))))))

(defn amph-locs [bur]
  (let [num-rows (count bur)
        num-cols (count (first bur))]
    (for [r (range num-rows)
          c (range num-cols)
          :let [v (get-in bur [r c])]
          :when (not= v :0)]
      [r c])))

(defn amph-locs-not-set [bur]
  (->> bur
       amph-locs
       (remove (partial amph-set? bur))))

(defn num-moves [from to]
  (loop [cur from
         n 0]
    (if (= cur to) n
      (recur (next-move-on-path cur to) (inc n)))))

(defn calculate-cost [amph from to]
  (* (num-moves from to) (amph-cost amph)))

(defn set-amph [bur from to]
  (let [amph (get-in bur from)]
    [(-> bur
         (assoc-in from :0)
         (assoc-in to amph))
     (calculate-cost amph from to)]))

(defn safe+ [a b]
  (cond
    (= a Long/MAX_VALUE) Long/MAX_VALUE
    (= b Long/MAX_VALUE) Long/MAX_VALUE
    :else (+ a b)))

(defn all-next-burs [bur]
  (let [amph-locs (amph-locs-not-set bur)]
    (or
      (seq (keep
             (fn [loc]
               (let [to-loc (amph-goal-loc bur loc)]
                 (when (move-no-obstacle? bur loc to-loc)
                   (set-amph bur loc to-loc))))
             amph-locs))
      (let [wait-for (reduce (fn [acc loc] (into acc (amph-waits-for bur loc))) #{} amph-locs)]
        (mapcat
          (fn [loc]
            (let [to-locs (if (in-room? loc)
                            (filter (partial move-no-obstacle? bur loc) hallway-locs)
                            [])]
              (map (partial set-amph bur loc) to-locs)))
          wait-for)))))

(defn min-cost-solve [bur]
  (if (finished? bur) 0
    (reduce
      (fn [acc [bur cost]]
        (min acc (safe+ cost (min-cost-solve bur))))
      Long/MAX_VALUE
      (all-next-burs bur))))

(def min-cost-solve (memoize min-cost-solve))

(defn first-answer []
  (defn raw->input [raw]
    (let [levels (str/split-lines raw)
          [_ ra1 rb1 rc1 rd1] (mapv keyword (re-matches #".*(\w)#(\w)#(\w)#(\w).*" (get levels 2)))
          [_ ra2 rb2 rc2 rd2] (mapv keyword (re-matches #".*(\w)#(\w)#(\w)#(\w).*" (get levels 3)))]
      [[:0 :0 :0  :0 :0  :0 :0  :0 :0  :0 :0]
       [:0 :0 ra1 :0 rb1 :0 rc1 :0 rd1 :0 :0]
       [:0 :0 ra2 :0 rb2 :0 rc2 :0 rd2 :0 :0]]))

  (defn finished? [bur]
    (= bur
       [[:0 :0 :0 :0 :0 :0 :0 :0 :0 :0 :0]
        [:0 :0 :A :0 :B :0 :C :0 :D :0 :0]
        [:0 :0 :A :0 :B :0 :C :0 :D :0 :0]]))

  (let [bur (raw->input raw)]
    (min-cost-solve bur)))

(defn second-answer []
  (defn raw->input [raw]
    (let [levels (str/split-lines raw)
          [_ ra1 rb1 rc1 rd1] (mapv keyword (re-matches #".*(\w)#(\w)#(\w)#(\w).*" (get levels 2)))
          [_ ra2 rb2 rc2 rd2] (mapv keyword (re-matches #".*(\w)#(\w)#(\w)#(\w).*" (get levels 3)))]
      [[:0 :0 :0  :0 :0  :0 :0  :0 :0  :0 :0]
       [:0 :0 ra1 :0 rb1 :0 rc1 :0 rd1 :0 :0]
       [:0 :0 :D  :0 :C  :0 :B  :0 :A  :0 :0]
       [:0 :0 :D  :0 :B  :0 :A  :0 :C  :0 :0]
       [:0 :0 ra2 :0 rb2 :0 rc2 :0 rd2 :0 :0]]))

  (defn finished? [bur]
    (= bur
       [[:0 :0 :0 :0 :0 :0 :0 :0 :0 :0 :0]
        [:0 :0 :A :0 :B :0 :C :0 :D :0 :0]
        [:0 :0 :A :0 :B :0 :C :0 :D :0 :0]
        [:0 :0 :A :0 :B :0 :C :0 :D :0 :0]
        [:0 :0 :A :0 :B :0 :C :0 :D :0 :0]]))
  (let [bur (raw->input raw)]
    (min-cost-solve bur)))

(second-answer)
