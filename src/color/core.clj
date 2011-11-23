(ns color.core)


(defn spread [n]
  "For an exponent n, produce a permutation of numbers from the cyclic range
 [0, 1) such that successive values are approximately maximally separate.
 Specifically, this produces a concatenated sequence of groups [0], [1/2],
 [1/4, 3/4], [1/8, 3/8, 5/8, 7/8], etc. for a total collection of 2^n numbers."
  (case n
        0 []
        1 [0 0.5]
        (let [h1 (spread (dec n))
              shift (Math/pow 2 (- n))
              h2 (map (partial + shift) h1)]
          (concat h1 h2))))

