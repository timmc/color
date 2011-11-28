(ns color.core
  (:gen-class))

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

(defn bound
  "Restrict the value x to the closed range [lo,hi]."
  [lo hi x]
  (if (< x lo)
    lo
    (if (> x hi)
      hi
      x)))

;; TODO: This seems to be wrong...
(defn lch->rgb
  "Given luma and chroma in [0,1] and hue [0,1), return [r g b] each in [0,1].
Luma and chroma will be bounded to their range, while hue will be modulo'd."
  [l c h]
  (let [l (bound 0 1 l)
        c (bound 0 1 c)
        h (mod h 1)
        h' (* h 6)
        x (* c (- 1 (Math/abs ^Double (dec (mod h' 2)))))
        [r1 g1 b1] (cond ;;TODO: case when hue is undefined
                    (< h' 1) [c x 0]
                    (< h' 2) [x c 0]
                    (< h' 3) [0 c x]
                    (< h' 4) [0 x c]
                    (< h' 5) [x 0 c]
                    :else [c 0 x])
        m (- l (* 0.30 r1) (* 0.59 g1) (* 0.11 b1))]
    [(bound 0 1 (+ m r1))
     (bound 0 1 (+ m g1))
     (bound 0 1 (+ m b1))]))

(defn -main
  "Given luma, chroma, and a number of steps (as strings), generate HTML page
with a color test area."
  [& args]
  (let [l (Double/parseDouble (nth args 0))
        c (Double/parseDouble (nth args 1))
        num (Integer/parseInt (nth args 2) 10)]
    (doseq [h (range 0 1 (double (/ num)))]
      (let [[r g b] (map #(int (* 255 %)) (lch->rgb l c h))]
        (println (format "<div style='width: 100px; height: 10px; background-color: rgb(%d,%d,%d);'></div>" r g b))))))
