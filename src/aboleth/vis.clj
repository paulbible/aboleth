(ns aboleth.vis
  (:require [incanter.core :as icore]
            [incanter.charts :as icharts]
            [incanter.zoo :as izoo]
            [incanter.optimize :as iopt]
            [aboleth.cv :as cv]
            [aboleth.calc :as calc])
  (import aboleth.ImageProcessor
          aboleth.ImageViewer
          java.awt.Font
          java.awt.Font
          java.awt.image.BufferedImage
          java.lang.String
          java.lang.Character))

(defn view 
  "view an incanter plot (JChart)"
  [chart]
  (icore/view chart))

(defn view-chart
  "view an incanter plot (JChart)"
  [chart]
  (icore/view chart))

(defn view-image
  "view an incanter plot (JChart)"
  [mat]
  (let [viewer (aboleth.ImageViewer.)]
    (doto viewer  
      (.show mat "Image"))))

(defn draw-text
  "draw text on the buffered image (java side)
   accepts instance of BufferedImage
   call cv/mat->buffered-image first"
  ([^BufferedImage image text x y point-size]
    (let [buffered-image (cv/mat->buffered-image image)
          gfx            (doto (.getGraphics buffered-image)
                           (.setFont (Font. "Arial Black" Font/BOLD point-size))
                           (.drawString text x y))]
      buffered-image))
  ([image text x y]
    (draw-text image text x y 20)))

(defn unicode-range->string
  [s e]
  (let [r  (range s e)
        text (clojure.string/join (map #(Character/toString  (char %)) r))]
    text))


(defn view-tiles
  "View the tiles in a composit image"
  [images]
  (let [viewer (aboleth.ImageViewer.)]
    (doto viewer  
      (.show (cv/tile-images images) "Image"))))

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

(defn signal-plot
  "Plot a signal, a list of values"
  [vals]
  (let [x (range (count vals))]
      (icharts/xy-plot x vals)))

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




