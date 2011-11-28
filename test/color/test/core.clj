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

(deftest util-spread
  (is (= (spread 0)
         []))
  (is (= (map (comp int #(* % 2)) (spread 1))
         [0 1]))
  (let [spread4int (map (comp int #(* % 16)) (spread 4))]
    (is (= (first spread4int) 0))
    (is (= (map set (partition-doubling (rest spread4int)))
           [#{8} #{4 12} #{2 6 10 14} #{1 3 5 7 9 11 13 15}]))))

(deftest util-bound
  (is (= (bound 0 1 500) 1))
  (is (= (bound 0 1 0.5) 0.5))
  (is (= (bound 0 1 -50) 0)))

(deftest conversions
  ;; extrema
  (is (= (lch->rgb 0 0 42.17) (map double [0 0 0])))
  (is (= (lch->rgb 1 0 42.17) (map double [1 1 1])))
  ;; FIXME: This is just a reasonable observed value, not a theoretical result
  (are [h r g b] (= (map (comp int (partial * 255))
                         (lch->rgb 0.5 1 h))
                    [r g b])
       0/3 255  51  51
       1/3   0 232   0
       2/3  99  99 255))

