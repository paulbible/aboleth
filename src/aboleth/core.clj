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
    (cv/laplace)
    (cv/threshold 90)))  

(def trim-8 
  (cv/imread "resources/data/test-8.png"))

(def trim-9 
  (cv/imread "resources/data/test-9.png"))

(def use-gray
  (cv/col->gray trim-8))

(cv/imwrite 
  "resources/data/test.png"
  (cv/laplace use-gray))


;;create an image mask for a letter
(def a-mask 
  (cv/get-letter-mask "a" 2 2))
  
(vis/view-image a-mask)

(def letter-masks
  (map #(cv/get-letter-mask (str %) 2 2) 
       (seq "abcdefghijklmnopqrstuvwxyz")))
       
(vis/view-image (second letter-masks)) 


;;;;;;;;;;;;;; Tests

(defn t-clip-to-text-area 
  [img]
  (cv/imwrite "resources/data/clip-to-text2.png"
              (cv/clip-to-text-area-2 img)))

(defn t-image-append
  []
  (let [i1 img-p8
        i2 img-p9]
    (cv/image-append i1 i2)))

(defn t-tile-images
  []
  (let [s1 (cv/sub-image img-p8 0   0   100 100)
        s2 (cv/sub-image img-p8 100 100 200 200)
        s3 (cv/sub-image img-p8 200 200 300 300)
        s4 (cv/sub-image img-p8 300 300 400 400)]
    (cv/tile-images (list s1 s2 s3 s4))))

(defn t-tile-images-2
  []
  (let [s1 (cv/sub-image img-p8 0   0   100 100)
        s2 (cv/sub-image img-p8 100 100 200 200)
        s3 (cv/sub-image img-p8 200 200 300 300)
        s4 (cv/sub-image img-p8 300 300 400 400)
        s5 (cv/sub-image img-p8 400 400 500 500)
        s6 (cv/sub-image img-p8 500 500 600 600)]
    (cv/tile-images (list s1 s2 s3 s4 s5 s6))))

(defn t-tile-images-3
  []
  (let [s1 (cv/sub-image img-p8 0   0   100 100)
        s2 (cv/sub-image img-p8 100 100 200 200)
        s3 (cv/sub-image img-p8 200 200 300 300)
        s4 (cv/sub-image img-p8 300 300 400 400)
        s5 (cv/sub-image img-p8 400 400 500 500)
        s6 (cv/sub-image img-p8 500 500 600 600)
        s7 (cv/sub-image img-p8 300 300 400 400) 
        s8 (cv/sub-image img-p8 400 400 500 500)
        s9 (cv/sub-image img-p8 500 500 600 600)]
    (cv/tile-images (list s1 s2 s3 s4 s5 s6 s7 s8 s9))))


(defn t-random-image
  []
  (let [mask (cv/sub-image img-p8 0 0 100 100)
        s1   (cv/random-sub-image img-p8 mask)
        s2   (cv/random-sub-image img-p8 mask)
        s3   (cv/random-sub-image img-p8 mask)]
    (cv/tile-images (list mask s1 s2 s3))))

(defn t-random-image-2
  []
  (let [mask (cv/sub-image img-p8 0 0 100 100)]
    (cv/tile-images 
      (conj 
        (take 20 (repeatedly #(cv/random-sub-image img-p8 mask)))
        mask))))

(defn t-random-image-3
  []
  (let [mask (cv/sub-image img-p8 0 0 100 100)]
    (cv/tile-images 
      (cv/n-random-sub-images (cv/laplace img-p8)
                              mask 100))))

(def imgs
  (cv/n-random-sub-images img-p8 a-mask 100))

(defn t-k-means
  [k]
  (let [labels (cv/cluster-images
                 (cv/n-random-sub-images img-p8 a-mask 1000)
                 k)]
    labels))

(defn t-k-means-2
  [k]
  (let [post-image    (cv/laplace (cv/blur (cv/blur img-p8)))
        sorted-images (cv/cluster-images
                        (cv/n-random-sub-images post-image a-mask 1000)
                        k)]
    sorted-images))

(defn t-svm
  [k]
  (let [post-image    (cv/laplace (cv/blur (cv/blur img-p8)))
        samples       (cv/n-random-sub-images post-image a-mask 1000)  
        sorted-images (cv/cluster-images samples k)
        svm           (cv/svm samples sorted-images)]
    svm))


;;;;;;;;;;;;;;;; Commented out
(comment
  
(cv/imwrite 
  "resources/data/test-8.png" 
  (cv/clip-to-text-area-2 img-p8))

(cv/imwrite 
  "resources/data/test-9.png"
  (cv/clip-to-text-area-2 img-p9))


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

;;;;;;;;;;;;;;;;;;;; pre-process image, lapalce 2 rounds
;; apply laplace filter
(def img-laplace
  (-> img-gray
    (cv/blur)
    (cv/laplace)
    (cv/blur)
    (cv/laplace)))

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
