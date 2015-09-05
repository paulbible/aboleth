(ns aboleth.vis
  (:require [incanter.core :as icore]
            [incanter.charts :as icharts]
            [incanter.zoo :as izoo]
            [incanter.optimize :as iopt]
            [aboleth.cv :as cv]
            [aboleth.calc :as calc])
  (import aboleth.ImageProcessor aboleth.ImageViewer))

;;
(defn view 
  "view an incanter plot (JChart)"
  [chart]
  (icore/view chart))

(defn view-chart
  "view an incanter plot (JChart)"
  [chart]
  (icore/view chart))

;;
(defn view-image
  "view an incanter plot (JChart)"
  [mat]
  (let [viewer (aboleth.ImageViewer.)]
    (doto viewer  
      (.show mat "Image"))))

;;
(defn tile-images
  "take multiple"
  [imgs])

;;
(defn row-mean-plot
  "return an incanter plot of the list of row means"
  ([img]
    (let [n (.rows img)
          x (range n)
          y (cv/row-means img n)]
      (icharts/xy-plot x y)))
  ([img n]
    (let [x (range n)
          y (cv/row-means img n)]
      (icharts/xy-plot x y)))
  ([img s e]
    (let [x (range s e)
          y (cv/row-means img s e)]
      (icharts/xy-plot x y))))

;;
(defn col-mean-plot 
  "return an incanter plot of the list of col means"
  ([img]
    (let [n (.cols img)
          x (range n)
          y (cv/col-means img n)]
      (icharts/xy-plot x y)))
  ([img n]
    (let [x (range n)
          y (cv/col-means img n)]
      (icharts/xy-plot x y)))
  ([img s e]
    (let [x (range s e)
          y (cv/col-means img s e)]
      (icharts/xy-plot x y))))

;;
(defn signal-plot
  "Plot a signal, a list of values"
  [vals]
  (let [x (range (count vals))]
      (icharts/xy-plot x vals)))

;;
(defn add-signal
  "adds a plot of the signal to the given chart"
  [chart vals]
  (icharts/add-function
      chart
      (calc/list->func vals)
      0
      (- (count vals) 2)))


(defn hist
  [vals]
  (icharts/histogram vals))



(comment
  "proved to be slower than partiiton map apply"
  (defn calc/which [vals f]
    (loop [vlist vals
           func f
           carry '()
           n 0]
      (if (empty? vlist)
        (reverse carry)
        (recur (rest vlist)
               f 
               (if (f (first vlist))
                 (conj carry n)
                 carry)
               (inc n))))))




