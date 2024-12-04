(ns aoc2024clj.d01.main)

(require '[clojure.string :as str])

(def input (slurp "./src/aoc2024clj/d01/input.txt"))

(defn read-pair [line]
  (let [parts (str/split line #"   ")]
    {:first (first parts)
     :second (last parts)}))

(defn read-input [input]
  (let [pairs (map read-pair (str/split-lines input))
        first-list (map #(Integer/parseInt (:first %)) pairs)
        second-list (map #(Integer/parseInt (:second %)) pairs)]
    {:first first-list
     :second second-list}))

(def processed-input (read-input input))

(defn distance [x y]
  (abs (- x y)))

(defn find-min [coll]
  (let [min-val (apply min coll)]
    {:min min-val
     :idx (.indexOf coll min-val)}))

(defn remove-nth [n coll]
  (keep-indexed #(when (not= %1 n) %2) coll))

(defn total-distance [fst snd acc]
  (if (or (empty? fst) (empty? snd))
    acc

    (let [fst-min (find-min fst)
          snd-min (find-min snd)
          new-fst (remove-nth (:idx fst-min) fst)
          new-snd (remove-nth (:idx snd-min) snd)
          dst (distance (:min fst-min) (:min snd-min))]
      (total-distance new-fst
                      new-snd
                      (+ acc dst)))))

(total-distance (:first processed-input) (:second processed-input) 0)

(defn similarity-score [v snd]
  (let [cnt (count (filter #(= v %) snd))]
    (* v cnt)))

(similarity-score 3 (:second processed-input))

(defn total-similarity-score [fst snd]
  (reduce + (map #(similarity-score % snd) fst)))

(total-similarity-score (:first processed-input) (:second processed-input))

