(ns sat.core
  (:require [rolling-stones.core :as sat :refer [!]]
            [clojure.string :as str]))

(def rows (range 9))
(def cols (range 9))
(def values (map inc (range 9)))


(defn possible-square-values
  "All the possible values in a square"
  [r c]
  (for [x values]
    {:r r :c c :x x}))


(defn one-number-per-square []
  (for [r rows
        c cols]
    (sat/exactly 1 (possible-square-values r c))))


(defn each-number-once-per-row []
  (apply
   concat
   (for [row rows]
     (for [x values]
       (sat/exactly
        1
        (for [c cols]
          {:r row :c c :x x}))))))


(defn each-number-once-per-column []
  (apply
   concat
   (for [col cols]
     (for [x values]
       (sat/exactly
        1
        (for [r rows]
          {:r r :c col :x x}))))))


(defn box-coords [d-row d-col]
  (apply
   concat
   (for [r (range 3)]
     (for [c (range 3)]
       {:r (+ r d-row) :c (+ c d-col)}))))


(defn each-number-once-per-box []
  (apply
   concat
   (for [x values]
     (for [d-row (range 0 8 3)
           d-col (range 0 8 3)]
       (sat/exactly
        1
        (for [{:keys [r c]} (box-coords d-row d-col)]
          {:r r :c c :x x}))))))


(defn render [board]
  (let [lookup (zipmap (map (juxt :r :c) board) board)
        board  (for [r rows
                     c cols]
                 {:r r :c c :x (:x (get lookup [r c]))})
        rows   (for [row (partition-by :r board)]
                 (->> (map #(or (:x %) ".") row)
                      (partition 3)
                      (map (partial str/join " "))
                      (str/join " | ")))
        rows   (map (partial str " ") rows)]
    (doall (map println (take 3 rows)))
    (println "-------+-------+-------")
    (doall (map println (->> rows (drop 3) (take 3))))
    (println "-------+-------+-------")
    (doall (map println (take-last 3 rows)))
    nil))


(defn solve [known]
  (filter
   sat/positive?
   (sat/solve-symbolic-cnf
    (concat (one-number-per-square)
            (each-number-once-per-row)
            (each-number-once-per-column)
            (each-number-once-per-box)
            (map vector known)))))

(def puzzle1
  [{:r 0 :c 1 :x 6}
   {:r 2 :c 0 :x 4}

   {:r 0 :c 5 :x 7}
   {:r 2 :c 5 :x 9}

   {:r 1 :c 6 :x 8}
   {:r 0 :c 7 :x 3}
   {:r 1 :c 8 :x 2}

   {:r 5 :c 0 :x 1}
   {:r 4 :c 1 :x 3}
   {:r 4 :c 2 :x 5}

   {:r 3 :c 3 :x 3}
   {:r 4 :c 3 :x 4}
   {:r 4 :c 5 :x 8}
   {:r 5 :c 5 :x 6}

   {:r 4 :c 6 :x 7}
   {:r 4 :c 7 :x 9}
   {:r 3 :c 8 :x 1}

   {:r 7 :c 0 :x 8}
   {:r 8 :c 1 :x 2}
   {:r 7 :c 2 :x 3}

   {:r 6 :c 3 :x 2}
   {:r 8 :c 3 :x 7}

   {:r 6 :c 8 :x 7}
   {:r 8 :c 7 :x 6}])

(def puzzle2 ;;"evil" from websudoku.com
  [{:r 0 :c 1 :x 4}
   {:r 1 :c 0 :x 7}
   {:r 2 :c 1 :x 9}
   {:r 2 :c 2 :x 3}

   {:r 0 :c 4 :x 8}

   {:r 1 :c 6 :x 2}
   {:r 0 :c 7 :x 7}
   {:r 1 :c 8 :x 9}

   {:r 4 :c 0 :x 4}
   {:r 4 :c 2 :x 1}
   {:r 5 :c 2 :x 6}

   {:r 3 :c 3 :x 9}
   {:r 3 :c 4 :x 2}
   {:r 5 :c 4 :x 7}
   {:r 5 :c 5 :x 5}

   {:r 3 :c 6 :x 6}
   {:r 4 :c 6 :x 9}
   {:r 4 :c 8 :x 5}

   {:r 7 :c 0 :x 6}
   {:r 8 :c 1 :x 8}
   {:r 7 :c 2 :x 4}

   {:r 8 :c 4 :x 9}

   {:r 6 :c 6 :x 4}
   {:r 6 :c 7 :x 3}
   {:r 7 :c 8 :x 8}
   {:r 8 :c 7 :x 6}])


 ;; this is from the PDF here, "very hard": http://www.sudokuessentials.com/x-wing.html
(def puzzle3
  [{:r 0 :c 1 :x 3}
   {:r 2 :c 0 :x 8}

   {:r 0 :c 3 :x 4}
   {:r 0 :c 4 :x 8}
   {:r 1 :c 4 :x 2}
   {:r 1 :c 5 :x 7}
   {:r 2 :c 3 :x 3}

   {:r 0 :c 6 :x 6}
   {:r 0 :c 8 :x 9}

   {:r 3 :c 1 :x 1}
   {:r 3 :c 2 :x 9}
   {:r 4 :c 0 :x 7}
   {:r 4 :c 1 :x 8}

   {:r 4 :c 5 :x 2}
   {:r 5 :c 5 :x 4}

   {:r 4 :c 7 :x 9}
   {:r 4 :c 8 :x 3}
   {:r 5 :c 6 :x 8}
   {:r 5 :c 7 :x 7}

   {:r 8 :c 0 :x 9}
   {:r 8 :c 2 :x 2}

   {:r 7 :c 3 :x 1}
   {:r 7 :c 4 :x 3}
   {:r 8 :c 4 :x 4}
   {:r 8 :c 5 :x 8}
   {:r 6 :c 5 :x 5}

   {:r 8 :c 7 :x 1}
   {:r 6 :c 8 :x 6}])

(comment
  (render puzzle1)
  (render (solve puzzle1))

  (render puzzle2)
  (render (solve puzzle2))

  (render puzzle3)
  (render (solve puzzle3)))
