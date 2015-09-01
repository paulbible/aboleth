(ns aboleth.core
  (:require [aboleth.vis :as vis]
            [aboleth.cv :as cv]
            [aboleth.calc :as calc]))


;;;;;;;;;;;;;;;;;;;; images
;; def file resources
(def page-8-fname "resources/data/page_8.jpg")
(def page-9-fname "resources/data/page_9.jpg")

;; read in a fresh image
(def img-p8
  (cv/imread page-8-fname))

(def img-p9
  (cv/imread page-9-fname))

(defn t-clip-to-text-area 
  [img]
  (cv/imwrite "resources/data/clip-to-text2.png"
              (cv/clip-to-text-area-2 img)))

(def a-mask 
  (cv/get-letter-mask "a" 2 2))

;; convert color image to gray scale
(def img-gray
  (cv/col->gray img-p8))

;;write the gray image
(cv/imwrite 
  "resources/data/gray.png"
  (cv/canny img-gray 200))

(cv/imwrite 
  "resources/data/test.png"
  (-> (cv/blur (cv/col->gray img-p9))
    (cv/laplace-proc)
    (cv/threshold 90)))  

(cv/imwrite 
  "resources/data/test-8.png" 
  (cv/clip-to-text-area-2 img-p8))

(cv/imwrite 
  "resources/data/test-9.png"
  (cv/clip-to-text-area-2 img-p9))

(def trim-8 
  (cv/imread "resources/data/test-8.png"))

(def trim-9 
  (cv/imread "resources/data/test-9.png"))

(def use-gray
  (cv/col->gray trim-8))

(cv/imwrite 
  "resources/data/test.png"
  (cv/laplace-proc use-gray))


(def a-scores
  (for [x (range (- (.cols use-gray) (.cols a-mask)))
        y (range (- (.rows use-gray) (.cols a-mask)))]
    (cv/score (cv/matched-region use-gray a-mask x y) a-mask)))

;;
(def top-a
  (take 20
        (calc/which a-scores #(> % 124))))

(let [x (int (/ (second top-a)
           (.cols use-gray)))
      y (mod (second top-a)
             (.cols use-gray))]
  (cv/imwrite
     "resources/data/target.png"
     (cv/matched-region use-gray a-mask x y)))


(def top-a-loc
  (into [] (map #(cv/index->xy use-gray %)
                   top-a)))


(cv/imwrite
     "resources/data/target.png"
     (let [p (nth top-a-loc 19)
           x (first p)
           y (second p)]
     (cv/matched-region use-gray a-mask x y)))





(comment


;;;;;;;;;;;;;;;;;;;; pre-process image, lapalce 2 rounds
;; apply laplace filter
(def img-laplace
  (-> img-gray
    (cv/blur)
    (cv/laplace-proc)
    (cv/blur)
    (cv/laplace-proc)))

;;write image with a lapace filter
(cv/imwrite
  "resources/data/laplace.png" img-laplace)

(vis/view
  (-> (vis/signal-plot col-means)
    (vis/add-signal sma-5-all-c)))

(def row-indexes
  (let [ d2 (calc/slope 
              (calc/cross-points (calc/nth-derivative sma-5-all 2)))]
    (calc/which d2 #(= % 2))))

;;(cv/draw-h-lines! img-laplace rows-indexes)


(cv/imwrite 
  "resources/data/lines.png" img-laplace)

(cv/imwrite 
  "resources/data/lines.png"
    (loop [img img-laplace
           ys row-indexes
           n (count ys)]
      (if (zero? n)
        img
        (recur (cv/draw-h-line img (first ys)) (rest ys) (dec n)))))

(cv/imwrite 
  "resources/data/lines.png"
    (cv/draw-h-lines img-laplace row-indexes))  

(cv/imwrite 
  "resources/data/lines.png"
  (let [pair (cv/clip-ends sma-5-all 0.33)]
    (cv/draw-h-lines img-laplace pair)))

(cv/imwrite 
  "resources/data/lines.png"
  (let [tmp (.clone img-laplace)]
    (do
      (cv/draw-h-lines! tmp row-indexes)
      tmp)))
            





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Letter mask stuff

;;make an 'a' mask
(def a-mask 
  (cv/get-letter-mask "a" 2 2))
;;write the mask
(cv/imwrite "resources/data/letter.png" a-mask)

;;get the target at 500 500 using mask
(def target
  (cv/matched-region img-gray a-mask 500 500))

;;write the mask value
(cv/imwrite "resources/data/target.png" 
            (cv/matched-region 
              img-gray a-mask 500 500))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def img-top
  (cv/sub-image img-laplace 0 0 (.cols img-laplace) 300))

(cv/imwrite "resources/data/top.png"
            img-top)

);;comment block
