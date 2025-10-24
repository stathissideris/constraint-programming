(ns or-tools-clj.domain
  "Functions for constructing and inspecting OR-Tools Domain objects.

   Domains represent sets of integers as unions of closed intervals.
   Intervals can be represented as:
   - Vectors: [min max] for closed intervals
   - Maps: {:min min :max max} for closed intervals

   See: https://or-tools.github.io/docs/javadoc/com/google/ortools/util/Domain.html"
  (:import [com.google.ortools.util Domain])
  (:refer-clojure :exclude [complement]))

;; Constructors

(defn empty-domain
  "Creates an empty domain."
  []
  (Domain.))

(defn singleton
  "Creates a domain containing a single value."
  [value]
  (Domain. (long value)))

(defn interval
  "Creates a domain from a single interval [left, right].
   Returns empty domain if left > right.

   Accepts:
   - Two arguments: (interval 1 10)
   - Vector: (interval [1 10])
   - Map: (interval {:min 1 :max 10})"
  ([left right]
   (Domain. (long left) (long right)))
  ([interval-spec]
   (cond
     (vector? interval-spec)
     (let [[left right] interval-spec]
       (interval left right))

     (map? interval-spec)
     (let [{:keys [min max]} interval-spec]
       (interval min max))

     :else
     (throw (IllegalArgumentException.
             (str "Invalid interval specification: " interval-spec))))))

(defn from-values
  "Creates a domain from a sequence of integer values.
   Values don't need to be sorted."
  [values]
  (Domain/fromValues (long-array values)))

(defn from-intervals
  "Creates a domain from a sequence of intervals.

   Each interval can be:
   - A vector: [min max]
   - A map: {:min min :max max}

   Example:
   (from-intervals [[1 5] [10 15] [20 25]])
   (from-intervals [{:min 1 :max 5} {:min 10 :max 15}])"
  [intervals]
  (let [normalized (map (fn [interval]
                          (cond
                            (vector? interval) interval
                            (map? interval) [(:min interval) (:max interval)]
                            :else (throw (IllegalArgumentException.
                                          (str "Invalid interval format: " interval)))))
                        intervals)
        array (into-array (map long-array normalized))]
    (Domain/fromIntervals array)))

(defn from-flat-intervals
  "Creates a domain from a flattened array of interval bounds.
   The array should contain alternating min/max values: [min1 max1 min2 max2 ...]

   Example:
   (from-flat-intervals [1 5 10 15 20 25])"
  [flat-intervals]
  (Domain/fromFlatIntervals (long-array flat-intervals)))

(defn all-values
  "Creates a domain containing all Int64 values."
  []
  (Domain/allValues))

;; Query/Inspection Functions

(defn empty?
  "Returns true if the domain is empty."
  [^Domain domain]
  (.isEmpty domain))

(defn size
  "Returns the number of elements in the domain.
   Note: Capped at Long/MAX_VALUE for very large domains."
  [^Domain domain]
  (.size domain))

(defn min-value
  "Returns the minimum value in the domain.
   Requires non-empty domain."
  [^Domain domain]
  (.min domain))

(defn max-value
  "Returns the maximum value in the domain.
   Requires non-empty domain."
  [^Domain domain]
  (.max domain))

(defn contains?
  "Returns true if the domain contains the given value."
  [^Domain domain value]
  (.contains domain (long value)))

(defn flattened-intervals
  "Returns a vector of flattened interval bounds [min1 max1 min2 max2 ...].
   Each pair represents one closed interval."
  [^Domain domain]
  (vec (.flattenedIntervals domain)))

(defn intervals
  "Returns a sequence of intervals as vectors [min max].
   Each vector represents one closed interval in the domain."
  [^Domain domain]
  (let [flat (.flattenedIntervals domain)]
    (partition 2 flat)))

(defn intervals-as-maps
  "Returns a sequence of intervals as maps {:min min :max max}.
   Each map represents one closed interval in the domain."
  [^Domain domain]
  (map (fn [[min max]] {:min min :max max})
       (intervals domain)))

;; Set Operations

(defn complement
  "Returns the complement of the domain (Int64 ∖ domain)."
  [^Domain domain]
  (.complement domain))

(defn negation
  "Returns the negation of the domain: {x | ∃e ∈ domain, x = -e}."
  [^Domain domain]
  (.negation domain))

(defn intersection
  "Returns the intersection of two domains."
  [^Domain domain1 ^Domain domain2]
  (.intersectionWith domain1 domain2))

(defn union
  "Returns the union of two domains."
  [^Domain domain1 ^Domain domain2]
  (.unionWith domain1 domain2))

(defn addition
  "Returns the Minkowski sum of two domains: {a + b | a ∈ domain1, b ∈ domain2}."
  [^Domain domain1 ^Domain domain2]
  (.additionWith domain1 domain2))

;; Data-Driven Construction

(defn create
  "Creates a domain from a data structure (map or value).

   Supported formats:

   1. Empty domain:
      {:type :empty}

   2. Singleton:
      {:type :singleton :value 5}
      5  ; shorthand for singleton

   3. Single interval:
      {:type :interval :min 1 :max 10}
      {:type :interval :interval [1 10]}
      [1 10]  ; shorthand for interval

   4. Multiple intervals:
      {:type :intervals :intervals [[1 5] [10 15]]}
      {:type :intervals :intervals [{:min 1 :max 5} {:min 10 :max 15}]}

   5. From values:
      {:type :values :values [1 3 5 7 9]}

   6. All values:
      {:type :all}

   Examples:
   (create 42)                                    ; singleton
   (create [1 10])                               ; interval [1,10]
   (create {:type :singleton :value 5})          ; explicit singleton
   (create {:type :interval :min 1 :max 10})    ; explicit interval
   (create {:type :intervals
            :intervals [[1 5] [10 15] [20 25]]}) ; multiple intervals
   (create {:type :values :values [2 4 6 8]})   ; from values"
  [spec]
  (cond
    ;; Number shorthand - singleton
    (number? spec)
    (singleton spec)

    ;; Vector shorthand - interval
    (and (vector? spec) (= 2 (count spec)))
    (interval spec)

    ;; Map with :type key
    (and (map? spec) (:type spec))
    (case (:type spec)
      :empty (empty-domain)
      :singleton (singleton (:value spec))
      :interval (if (:interval spec)
                  (interval (:interval spec))
                  (interval (:min spec) (:max spec)))
      :intervals (from-intervals (:intervals spec))
      :values (from-values (:values spec))
      :all (all-values)
      (throw (IllegalArgumentException.
              (str "Unknown domain type: " (:type spec)))))

    ;; Default: treat as singleton
    :else
    (throw (IllegalArgumentException.
            (str "Invalid domain specification: " spec)))))

;; Utility

(defn domain->string
  "Returns a compact string representation of the domain.
   Format: [1,4][6][10,20]"
  [^Domain domain]
  (.toString domain))
