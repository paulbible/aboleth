(ns aboleth.calc
  (:require [incanter.core :as icore] 
            [incanter.zoo :as izoo]
            [incanter.stats :as istats])
  (:import [org.opencv.core
             Core CvType Scalar Mat Size Point Rect]))

;;;;;;;;;;;;;;;;;;;; Utilities
(defn which
  "a function to mimic the r 'which' function,
   returns the index of values where the predicate evaluates to true"
  [vals pred]
  (let [true-list (map pred vals)
        temp-index (map (fn [val idx] (if val idx))
                        true-list
                        (range (count vals)))
        res (filter #(not (nil? %)) temp-index)]
      (if (empty? res)
        nil
        res)))

(defn first-where
  "finds the index of the first value where the predicate is true"
  [vals pred]
  (count (take-while #(not (pred %))
                     vals)))

(defn last-where
  "finds the index of the last value where the predicate is true"
  [vals f]
  (let [idx (count (take-while #(not (f %))
                               (reverse vals)))]
    (- (count vals) idx)))

(defn which-min
  "finds the index of the lowest value in a list"
  [vals]
  (let [min-val (apply min vals)]
    (first-where vals #(= % min-val))))

(defn list->func [vals]
  "Takes a list of values and returns a function, f(i) = val[i]"
  (fn [i]
    (nth (vec vals) i)))

;;;;;;;;;;;;;;;;;;;; Summary values
(defn mean [vals]
  (istats/mean vals))

(defn sd
  "standard deviation of the vals"
  [vals]
  (istats/sd vals))

(defn var
  "returns the variance of the vals"
  [vals]
  (istats/variance vals))

;;;;;;;;;;;;;;;;;;;; other operations
(defn pow
  "returns the values raised to the nth power"
  [vals n]
  (icore/pow vals n))

(defn minus
  [v1 v2]
  (map #(apply - (reverse %))
       (partition 2 (interleave v1 v2))))


;;;;;;;;;;;;;;;;;;;; smoothing
(defn sma
  "a function to calculate the simple moving aveage of a list of values"
  [period vals]
  (izoo/roll-mean period vals))

(defn smv
  "a function to calculate the simple moving variance of a list of values"
  [period vals]
  (izoo/roll-apply istats/variance period vals))


;;;;;;;;;;;;;;;;;;;; slopes and derivatives
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

(defn cross-points 
  "calculates the cross over points, where the derivative crosses 0"
  [vals]
  (slope (map #(if (> 0 %) 1 0) (slope vals))))





