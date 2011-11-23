(ns color.core)


(defn spread [n]
  "For an exponent n, produce a permutation of [0, 2^n - 1] such that
successive values are approximately maximally separate."
  (if (= n 1)
    [0 1]
    (let [co1 (spread (dec n))
          h1 (map #(* 2 %) co1)
          h2 (map inc h1)]
      (concat h1 h2))))
