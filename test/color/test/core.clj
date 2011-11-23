(ns color.test.core
  (:use [color.core])
  (:use [clojure.test]))

;;;; Tets utils

(defn partition-doubling
  "Partition a collection into groups of 1, 2, 4, 8... etc."
  ([coll]
     (partition-doubling coll 1))
  ([coll group1]
     (cond
      (empty? coll) []
      (= (count coll) 1) [coll]
      :else (cons (take group1 coll)
                  (partition-doubling (drop group1 coll)
                                      (* 2 group1))))))

(deftest partdouble
  (is (= (partition-doubling []) []))
  (is (= (partition-doubling [1]) [[1]]))
  (is (= (map set (partition-doubling (range 1 16)))
         [#{1} #{2 3} #{4 5 6 7} #{8 9 10 11 12 13 14 15}])))

;;;; Tests

(deftest utils
  (is (= (spread 0)
         []))
  (is (= (map (comp int #(* % 2)) (spread 1))
         [0 1]))
  (let [spread4int (map (comp int #(* % 16)) (spread 4))]
    (is (= (first spread4int) 0))
    (is (= (map set (partition-doubling (rest spread4int)))
           [#{8} #{4 12} #{2 6 10 14} #{1 3 5 7 9 11 13 15}]))))
