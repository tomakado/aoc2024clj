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

; (defn find-min [coll]
;   (some ))

(defn total-distance [fst snd acc]
  (if (or (empty? fst) (empty? snd))
    acc

    (let [fst-min (apply min fst)
        snd-min (apply min snd)
        new-fst (remove #(= fst-min %) fst)
        new-snd (remove #(= snd-min %) snd)
        dst (distance fst-min snd-min)]
      (println dst)
      (total-distance new-fst
                      new-snd 
                      (+ acc dst)))))

(total-distance (:first processed-input) (:second processed-input) 0)
