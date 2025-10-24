(ns schedule.schedule
  (:require [rolling-stones.core :as sat :refer [!]]
            [clojure.pprint :as pp]))

;; Requirements
;; - Omades den spane
;; - Handover happens with both teams and both get paid
;; - Grey boxes mean that the ship is off shore and you can't swap teams
;;   - 7 days steps
;; - Team some time off in the week after you return from off shore, so avoid
;;   a swap then
;; - Range of time off 65 to 75, but if some team gets 65, next year give them 75
;; - Summer period: 20/5 until 15/9
;;   - All teams must have at least 20 days of time off
;; - Holiday period: Don't make the same team work for consecutive years on the
;;   same holiday
;;   - 1-15 Aug
;;   - Pasxa
;;   - Xmas: 20 Dec - 5 Jan
;; - Keep group changes to 15-16 times to reduce double pay in handovers
;; - No handovers on weekends


(def teams [:a :b :c :d :e])
(def team-combos (set (for [a teams
                            b teams
                            :when (and (not= a b))]
                        #{a b})))
(def residents (concat teams team-combos))

(def placements [:kikonas :evros :elmina :orfeas :leave])

(def assignment
  {:team      :a
   :placement :evros
   :date      "2024-11-23"
   :off-shore true})

(def assignment
  {:team          :a
   :outgoing-team :e
   :placement     :evros
   :date          "2024-11-23"})

(defn one-team-per-placement-per-date [dates]
  (for [d dates
        p placements]
    (sat/exactly 1
                 (for [t teams] ;; you want residents really
                   {:date d :placement p :team t}))))

(defn team-in-only-one-place-at-once [dates]
  (for [d dates
        t teams] ;; you want residents really
    (sat/exactly 1 (for [p placements]
                     {:date d :placement p :team t}))))

(defn min-leave-per-team [dates min-leave]
  (for [t teams]
    (sat/at-least min-leave
                  (for [d dates]
                    {:date d :placement :leave :team t}))))

(defn solve [& {:keys [dates min-leave]}]
  (->> (concat
        (one-team-per-placement-per-date dates)
        (team-in-only-one-place-at-once dates)
        (min-leave-per-team dates min-leave))
       sat/solve-symbolic-cnf
       (filter sat/positive?)
       (sort-by (juxt :date :placement))))

(defn render [solution]
  (->> solution
       (group-by :date)
       vals
       (map (fn [data]
              (merge {:date (:date (first data))}
                     (zipmap (map :placement data) (map :team data)))))
       (sort-by :date)
       (pp/print-table (cons :date placements))))

(defn stats [solution]
  (->> solution
       (filter #(= :leave (:placement %)))
       (map :team)
       frequencies))

(comment
  (def solution (solve :dates (range 10) :min-leave 1))
  (render solution)

  (def solution (solve :dates (range 365) :min-leave 20))
  (render solution)

  (solve :dates (range 10) :min-leave 3))
