(ns aoc2024clj.d03.main)

(require '[clojure.string :as str])

(def input (slurp "./src/aoc2024clj/d03/input.txt"))

(def matches (re-seq #"(don't|do|mul\([0-9]{1,3},[0-9]{1,3}\))" input))

(def ops (map first matches))

(defn mul-enabled? [mul-enabled op]
  (cond
    (str/starts-with? op "don't") false
    (str/starts-with? op "do") true
    :else mul-enabled))

(defn mul? [op]
  (str/starts-with? op "mul"))

(defn mul-args [op]
  (map #(Integer/parseInt %) (re-seq #"[0-9]{1,3}" op)))

(defn calc-mul-op [op]
  (let [args (mul-args op)
        a (first args)
        b (second args)]
    (* a b)))

(defn calc-1 [ops]
  (reduce #(+ %1 (calc-mul-op %2)) 0 ops))

(defn calc-2 [ops acc mul-enabled]
  (if (empty? ops)
    acc
    (let [op (first ops)
          enabled (mul-enabled? mul-enabled op)
          op-res (if (and (mul? op) enabled)
                   (calc-mul-op op)
                   0)]

      (calc-2 (rest ops) (+ acc op-res) enabled))))

(calc-1 (filter mul? ops))
(calc-2 ops 0 true)
