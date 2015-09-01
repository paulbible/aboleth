(ns aboleth.cv
  (:require [aboleth.calc :as calc])
  (:import  [org.opencv.core
             Core CvType Scalar Mat Size Point Rect]
            [org.opencv.imgcodecs Imgcodecs]
            [org.opencv.imgproc Imgproc]))



;;;;;;;;;;;;;;;;;;;; Utility
;;
(defn imread
  "returns a cv Mat of the image at fname"
  [fname] 
  (Imgcodecs/imread fname))

;;
(defn imwrite
  "writes the image Mat to fname"
  [fname img]
  (Imgcodecs/imwrite fname img))

;;
(defn col->gray 
  "Convenient color to gray convertions for cv Mat"
  [src]
  (let [dst (.clone src)]
    (do 
      (Imgproc/cvtColor src dst Imgproc/COLOR_RGB2GRAY)
      dst)))


;;;;;;;;;;;;;;;;;;;; Processing / Filters
;;
(defn laplace-proc
  "apply the lapace filter the src image"
  [src]
  (let [dst (.clone src)]
    (do
      (Imgproc/Laplacian 
        src dst 
        CvType/CV_8U 3 1 0 Core/BORDER_DEFAULT)
      dst)))

;;
(defn blur
  "Apply a guassian blur to the image kernel 5 "
  [img]
  (let [dst (.clone img)]
    (do
      ;;sigmax sigmay
      (Imgproc/GaussianBlur img dst (Size. 5 5) 2 2)
      dst)))

;;
(defn canny
  "Apply a guassian blur to the image kernel 5 "
  ([img]
    (let [dst (.clone img)]
      (do
        ;;sigmax sigmay
        (Imgproc/GaussianBlur dst dst (Size. 5 5) 2 2)
        (Imgproc/Canny dst dst 3 3)
        dst)))
  ([img t1]
    (let [dst (.clone img)]
      (do
        ;;sigmax sigmay
        (Imgproc/GaussianBlur dst dst (Size. 5 5) 2 2)
        (Imgproc/Canny dst dst t1 (* 2 t1))
        dst))))


;;
(defn threshold
  "filter the image using a threshold"
  [img thresh]
  (let [dst (.clone img)]
    (do
      (Imgproc/threshold dst dst thresh 255.0 Imgproc/THRESH_BINARY)
      dst)))
  

;;;;;;;;;;;;;;;;;;;; Drawing
;;
(defn draw-line
  "draw a line between p1 and p2"
  [img p1 p2]
  (let [dst (.clone img)]
    (do
      (Imgproc/line dst p1 p2 (Scalar. 255 0 0) 1)
      dst)))

;;
(defn draw-line!
  "draw a line on the image as a side effect, modify in place"
  [img p1 p2]
  (Imgproc/line img p1 p2 (Scalar. 255 0 0) 1))

;;
(defn draw-h-line 
  "Draw a horiozntal line returns an image with the line drawn"
  [img y]
  (draw-line img 
             (Point. 0 y)
             (Point. (.cols img) y)))

;;
(defn draw-h-line!
  "draw a horizonalt line on the img as a side effect, modify in place"
  [img y]
  (draw-h-line img y))

(defn draw-h-lines!
  "Draw horizontal lines at a list of rows as a side effect, modify in place"
  [img row-indexes]
  (map #(draw-h-line! img %) row-indexes))

(defn draw-h-lines
  "Draw horizontal lines at a list of rows"
  [img ys]
  (loop [img-tmp img
         ys-tmp  ys
         n       (count ys)]
    (if (zero? n)
      img-tmp
      (recur (draw-h-line img-tmp (first ys-tmp)) (rest ys-tmp) (dec n)))))


;;;;;;;;;;;;;;;;;;;; Mat Cnversions
;;
(defn mat->float
  "convert an image Mat to float representation (not 0 - 255)"
  [src]
  (let [dst (.clone src)]
    (do
      (.convertTo src dst CvType/CV_64FC1)
      dst)))

;;
(defn norm-255
  "Convert to float and devide by 255, represent as percentage"
  [src]
  (let [dst (.clone src)]
    (do
      (Core/divide (mat->float src) (Scalar. 255.0) dst)
      dst)))


;;;;;;;;;;;;;;;;;;;; Letter mask, subregion, score
;;
(defn get-letter-mask
  "get a letter mask for the given string"
  [letter scale thickness]
  (let [baseline (int-array 1)
        size     (Imgproc/getTextSize 
                   letter Core/FONT_HERSHEY_PLAIN
                   scale thickness baseline)
        mat      (Mat. size CvType/CV_8U (Scalar. 0.0))]
    (do
      (Imgproc/putText 
        mat letter 
        (Point. 0 (.rows mat)) Core/FONT_HERSHEY_PLAIN scale (Scalar. 255 255 255) thickness)
      mat)))

;;
(defn matched-region 
  "grab the sub section of the source image at the point x y"
  [src mask x y]
  (let [rect (Rect. (Point. x y) (.size mask))]
    (Mat. src rect)))

;;
(defn score
  "score the mask against the src image"
  [src mask]
  (let [temp (.clone mask)
        norm-src (mat->float (norm-255 src))
        norm-mask (mat->float (norm-255 mask))]
    (do 
      (Core/multiply norm-src norm-mask temp)
      (aget (.val (Core/sumElems temp)) 0))))

;;
(defn img-get 
  "get the values of a point in the image"
  [img x y]
  (let [col-vals (.get img x y)]
    (map #(aget col-vals  %) 
         (range (alength col-vals)))))

;;
(defn sub-image
  "get the sub image defined by the image and rect"
  ([image rect]
    (Mat. image rect))
  ([image p1x p1y p2x p2y]
    (Mat. image (Rect. (Point. p1x p1y) (Point. p2x p2y)))))


(defn index->xy
  [img n]
  (let [x (int (/ n (.cols img)))
        y (mod n (.cols img))]
    (list x y)))



;;;;;;;;;;;;;;;;;;;; Row and Column means
;;
(defn img-mean
  [img]
  (Core/mean img))


(defn row-mean
  "get the mean of a row of pixels in the image"
  [src nrow]
  (let [row-slice (.row src nrow)]
    (calc/mean
      (pmap #(aget (.get row-slice 0 %) 0) 
            (range (.cols row-slice))))))

;;
(defn col-mean
  "get the mean of a col of pixels in the image"
  [src ncol]
  (let [col-slice (.col src ncol)]
    (calc/mean
      (pmap #(aget (.get col-slice % 0) 0)
            (range (.cols col-slice))))))

;;
(defn row-means
  "Calculate a list of row means
   from 0 to nrows or a range"
  ([img start end]
    (pmap #(row-mean img %) 
          (range start end)))
  ([img nrows]
    (pmap #(row-mean img %) 
          (range nrows)))
  ([img]
    (row-means img 0 (.rows img))))

;;
(defn col-means
  "Calculate a list of col means
   from 0 to ncols or a range"
  ([img ncols]
    (pmap #(col-mean img %) 
          (range ncols)))
  ([img start end]
    (pmap #(col-mean img %) 
          (range start end)))
  ([img]
    (col-means img 0 (.cols img))))


;;;;;;;;;;;;;;;;;;;; pre-processing
;;
(defn min-in
  "find the min value within the given percentile"
  [vals percent]
  (apply 
    min 
    (take (int (* percent (count vals))) vals)))

;;
(defn clip-index
  "find the index of the min within the percentile"
  [vals percent]
  (let [target (min-in vals 0.33)]
    (first (calc/which vals #(= target %)))))

;;
(defn clip-index-rev
  "find the index of the min within the percentile of a reversed list"
  [vals percent]
  (let [rev-vals (reverse vals)
        target (min-in rev-vals  0.33)]
    (- (count vals)
      (first (calc/which rev-vals #(= target %))))))

;;
(defn clip-ends
  "get the top and bottom mins for clipping"
  [vals percent]
  (let [t-row (clip-index     vals 0.33)
        b-row (clip-index-rev vals 0.33)]
    (list t-row b-row)))

;;
(defn trim-tb
  "takes and image and trips it at the min points on the top and bottom"
  [img vals percent]
  (let [pair (clip-ends vals 0.33)
        t-row (first pair)
        b-row (last pair)]
    (sub-image img 0           t-row 
                   (.cols img) b-row)))

;;
(defn trim-lr
  "takes and image and trips it at the min points on the top and bottom"
  [img vals percent]
  (let [pair (clip-ends vals 0.33)
        l-col (first pair)
        r-col (last pair)]
      (sub-image img l-col 0 
                     r-col (.rows img))))

;;
(defn clip-to-text-area
  [img]
  (let [gray (col->gray (.clone img))
        r-means (calc/sma 5 (row-means gray))
        c-means (calc/sma 5 (col-means gray))
        tb-pair (clip-ends r-means 0.33)
        lr-pair (clip-ends c-means 0.33)]
    (sub-image img 
               (first lr-pair) (first tb-pair)
               (last lr-pair)  (last tb-pair))))

(defn clip-to-text-area-2
  [img]
  (let [gray (col->gray (.clone img))
        laplace (laplace-proc (blur (threshold gray 100)))
        r-means (calc/sma 5 (row-means laplace))
        c-means (calc/sma 5 (col-means gray))
        tb-pair (clip-ends r-means 0.33)
        lr-pair (clip-ends c-means 0.33)]
    (sub-image img 
               (first lr-pair) (first tb-pair)
               (last lr-pair)  (last tb-pair))))


;; End of file