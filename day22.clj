(require '[clojure.string :as str])

(def raw (slurp "input22.txt"))

(defn raw->input [raw]
  (let [parse-step
        (fn [s]
          (let [[_ op & nbs] (re-matches #"(\w+) x=(\-?\d+)..(\-?\d+),y=(\-?\d+)..(\-?\d+),z=(\-?\d+)..(\-?\d+)" (str/trim s))]
            (into [({"on" 1 "off" -1} op)] (map read-string nbs))))]
    (->> raw
         str/split-lines
         (mapv parse-step))))

(defn overlap? [[_ ax1 ax2 ay1 ay2 az1 az2] [_ bx1 bx2 by1 by2 bz1 bz2]]
  (every?
    (fn [[a1 a2 b1 b2]]
      (<= (- (max a2 b2) (min a1 b1))
          (+ (- a2 a1) (- b2 b1))))
    [[ax1 ax2 bx1 bx2] [ay1 ay2 by1 by2] [az1 az2 bz1 bz2]]))

(defn intxn [cb-a cb-b]
  (mapv (fn [f a b] (f a b))
        [(fn [op _] (* -1 op)) max min max min max min]
        cb-a cb-b))

(defn add-cuboid [clist [op :as cb]]
  (let [to-add
        (reduce
          (fn [to-add v]
            (if (overlap? v cb)
              (conj to-add (intxn v cb))
              to-add))
          ({1 [cb] -1 []} op) clist)]
    (into clist to-add)))

(defn count-cubes [clist]
  (reduce
    (fn [cnt [op x1 x2 y1 y2 z1 z2]]
      (+ cnt (* op (* (inc (- x2 x1)) (inc (- y2 y1)) (inc (- z2 z1))))))
    0 clist))

(defn solve [steps]
  (->> steps
       (reduce add-cuboid [])
       count-cubes))

(defn first-answer [steps]
  (let [filter-50
        (fn [[_ & nbs]]
          (every? #(<= (Math/abs %) 50) nbs))]
    (solve (filter filter-50 steps))))

(def second-answer solve)

(->> raw
     raw->input
     first-answer)

