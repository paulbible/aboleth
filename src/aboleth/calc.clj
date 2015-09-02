(ns aboleth.calc
  (:require [incanter.core :as icore] 
            [incanter.zoo :as izoo]
            [incanter.stats :as istats]))


;;;;;;;;;;;;;;;;;;;; Utilities
;;
(defn which
  "a function to mimic the r which function,
   index of values that evaluation to true give function f"
  [vals f]
  (let [true-list (map f vals)
        pairs (partition 2 (interleave vals (range (count vals))))
        temp-index (map #(apply (fn [val idx]
                                  (if (f val) idx)) %) pairs)]
      (filter #(not (nil? %)) temp-index)))

;;
(defn list->func [vals]
  "Takes a list of values and returns a function, f(i) = val[i]"
  (fn [i]
    (nth (vec vals) i)))

;;;;;;;;;;;;;;;;;;;; Summary values
;;
(defn mean [vals]
  (istats/mean vals))

;;
(defn sd
  "standard deviation of the vals"
  [vals]
  (istats/sd vals))

;;
(defn var
  "returns the variance of the vals"
  [vals]
  (istats/variance vals))

;;;;;;;;;;;;;;;;;;;; other operations
;;
(defn pow
  "returns the values raised to the nth power"
  [vals n]
  (icore/pow vals n))

;;
(defn minus
  [v1 v2]
  (map #(apply - (reverse %))
       (partition 2 (interleave v1 v2))))


;;;;;;;;;;;;;;;;;;;; smoothing
;;
(defn sma
  "a function to calculate the simpel moving aveage of a list of values"
  [period vals]
  (izoo/roll-mean period vals))


;;;;;;;;;;;;;;;;;;;; slopes and derivatives
;;
(defn slope
  "simple first deivative, t1 - t0 to n-1"
  [vals]
  (map #(apply - (reverse %))
       (partition 2 1 vals)))

(defn nth-derivative 
  "get the nth splot of the values, t1 - t0"
  [vals n]
  (loop [v-list vals
         dn n]
    (if (zero? dn)
      v-list
      (recur (slope v-list) (dec dn)))))

;;
(defn cross-points 
  "calculates the cross over points, where the derivative crosses 0"
  [vals]
  (slope (map #(if (> 0 %) 1 0) (slope vals))))
