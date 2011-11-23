(ns color.test.core
  (:use [color.core])
  (:use [clojure.test]))

(deftest utils
  (is [0 1] (spread 1)))
