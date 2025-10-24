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
  (def model (CpModel.))

  (def var-keys
    (for [n (range 4)
          d (range 3)
          s (range 3)]
      {:nurse n :d d :shift s}))

  (def vars (zipmap var-keys (map #(.newBoolVar model (pr-str %)) var-keys)))

  (.addExactlyOne model (for []))

  (-> model .model str print) ;; seems to be the only way to inspect
  )

;; addded some stuff to the core namespace to help me out

(comment
  (init!)

  (def model (CpModel.))

  (def var-keys (ort/expand {:nurse (ENUM :a :b :c :d)
                             :day   (RANGE 3)
                             :shift (RANGE 3)}))

  (def vars (zipmap var-keys (map #(.newBoolVar model (pr-str %)) var-keys)))

  )
