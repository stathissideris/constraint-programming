(ns or-tools-clj.core
  "Core types and constructors for OR-Tools constraint programming."
  (:require [clojure.math.combinatorics :as combo]))

;; Records

(defrecord Range [min max])

(defrecord EnumRange [values])

;; Constructors

(defn RANGE
  "Creates a Range record with min and max bounds.

   Example:
   (RANGE 1 10)  ; => #or-tools-clj.core.Range{:min 1, :max 10}"
  ([max]
   (->Range 0 max))
  ([min max]
   (->Range min max)))

(defn ENUM
  "Creates an Enum record holding arbitrary values.

   Example:
   (ENUM 1 3 5 7 9)  ; => #or-tools-clj.core.Enum{:values [1 3 5 7 9]}"
  [& values]
  (->EnumRange values))

;; Expansion

(defn- expand-value
  "Converts a Range or Enum to its sequence of values."
  [x]
  (cond
    (instance? Range x)
    (range (:min x) (:max x))

    (instance? EnumRange x)
    (:values x)

    :else
    [x]))

(defn expand
  "Expands a map containing RANGE or ENUM records into a sequence
   of maps with all combinations.

   Examples:
   (expand {:a (RANGE 0 3)})
   ;; => ({:a 0} {:a 1} {:a 2})

   (expand {:a (RANGE 0 2) :b (ENUM :x :y)})
   ;; => ({:a 0 :b :x} {:a 0 :b :y} {:a 1 :b :x} {:a 1 :b :y})"
  [m]
  (let [ks            (keys m)
        vs            (vals m)
        expanded-vals (map expand-value vs)]
    (for [combo (apply combo/cartesian-product expanded-vals)]
      (zipmap ks combo))))
