(ns or-tools-clj.schedule
  (:require [or-tools-clj.core :as ort :refer [RANGE ENUM]]
            [or-tools-clj.domain :as domain])
  (:import [com.google.ortools Loader]
           [com.google.ortools.sat CpModel CpSolver CpSolverStatus IntVar
            CpSolverSolutionCallback LinearExpr LinearExprBuilder Literal]))

;; https://developers.google.com/optimization/cp/cp_solver#java
;; https://developers.google.com/optimization/assignment/assignment_example#java

(defn init! []
  (Loader/loadNativeLibraries))

(defn add-int-var! [model min max name]
  (.newIntVar model (long min) (long max) name))

(defn add-different! [model var1 var2]
  (.addDifferent model var1 var2))

(defn solve [model]
  (let [solver (CpSolver.)]
    (.solve solver model)
    solver))

(def job
  {:model       (CpModel.)
   :vars        [{:int {:name "x" :lb 0 :ub 2}}
                 {:int {:name "y" :lb 0 :ub 2}}
                 {:int {:name "z" :lb 0 :ub 2}}]
   :constraints [{:type :different :left [:var "x"] :right [:var "y"]}]})

;; Domains
;; https://or-tools.github.io/docs/javadoc/com/google/ortools/util/Domain.html

;; They represent all values, or an interval, or several intervals, or sparse numbers

(comment
  (def model (CpModel.))

  (def x (add-int-var! model 0 2 "x"))
  (def y (add-int-var! model 0 2 "y"))
  (def z (add-int-var! model 0 2 "z"))

  (add-different! model x y)
  (add-different! model x z)
  (add-different! model y z)

  (def solver (solve model))

  (.value solver x)
  (.value solver y)
  (.value solver z)

;;;

  (def model (CpModel.))

  (def x (add-int-var! model 0 2 "x"))
  (def y (add-int-var! model 0 2 "y"))
  (def z (add-int-var! model 0 2 "z"))

  (add-different! model x y)
  (add-different! model x z)
  (add-different! model y z)

  (def solver (solve model))

  (.value solver x)
  (.value solver y)
  (.value solver z)



  )

;; working on example https://developers.google.com/optimization/scheduling/employee_scheduling

(comment
  (init!)

  (def nurses [:a :b :c :d])
  (def days (range 3))
  (def shifts (range 3))

  (def model (CpModel.))

  (def var-keys (for [n nurses
                      d days
                      s shifts]
                  [n d s]))

  (def vars (zipmap var-keys (map #(.newBoolVar model (pr-str %)) var-keys)))

  ;; Each shift is assigned to a single nurse per day
  (doseq [d days
          s shifts]
    (.addExactlyOne model
                    (map vars
                         (for [n nurses] [n d s]))))

  ;; Each nurse works at most one shift per day.
  (doseq [n nurses
          d days]
    (.addAtMostOne model
                   (map vars
                        (for [s shifts] [n d s]))))

  ;; there are nine shifts over the three-day period, we can assign two shifts
  ;; to each of the four nurses. After that there will be one shift left over,
  ;; which can be assigned to any nurse

  (doseq [n nurses]
    (let [shifts-worked (LinearExpr/newBuilder)]
      (doseq [d days
              s shifts]
        (.add shifts-worked (vars [n d s])))
      (.addLinearConstraint model shifts-worked 2 3)))

  (do
    (def solver (CpSolver.))
    (-> solver .getParameters (.setLinearizationLevel 0))
    (-> solver .getParameters (.setEnumerateAllSolutions true)))

  (def solution-count (atom 0))

  (.solve
   solver
   model
   (proxy [CpSolverSolutionCallback] []
     (onSolutionCallback []
       (println "Solution" @solution-count)
       (doseq [d days]
         (println "Day" d)
         (doseq [n nurses]
           (doseq [s shifts]
             (when (proxy-super booleanValue (vars [n d s]))
               (println "Nurse" n "works shift" s)))))
       (swap! solution-count inc)
       (when (>= 5 @solution-count)
         (proxy-super stopSearch)))))

  (-> model .model str print)
  )
