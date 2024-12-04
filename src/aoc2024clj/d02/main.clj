(ns aoc2024clj.d02.main)

(require '[clojure.string :as str])

(def input (slurp "./src/aoc2024clj/d02/input.txt"))

(defn read-report [line]
  (vec (map #(Integer/parseInt %) (str/split line #" "))))

(defn read-reports [input]
  (vec (map #(read-report %) (str/split input #"\n"))))

(def reports (read-reports input))

(defn adj-safe? [a b]
  (let [diff (abs (- a b))]
    (and (>= diff 1) (<= diff 3))))

(defn increasing? [coll]
  (apply < coll))

(defn decreasing? [coll]
  (apply > coll))

(defn mono? [coll]
  (or (decreasing? coll)
      (increasing? coll)))

(defn adj-safe-report? [report]
  (let [a (first report)
        b (second report)]

    (if (adj-safe? a b)

      (if (> (count report) 2)
        (adj-safe-report? (rest report))
        true)

      false)))

(defn safe? [report]
  (and (adj-safe-report? report)
       (mono? report)))

(defn remove-nth [n coll]
  (keep-indexed #(when (not= %1 n) %2) coll))

(defn almost-safe? [report]
  (if (safe? report)
    true

    (let [cut-reports (map
                       #(remove-nth % report)
                       (range (count report)))]
      (some safe? cut-reports))))

(range (count (first reports)))

(defn num-safe-reports [reports]
  (count (filter almost-safe? reports)))

(num-safe-reports reports)
