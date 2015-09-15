(ns aboleth.cv
  (:require [aboleth.calc :as calc])
  (:import  [org.opencv.core
             Core CvType Scalar Mat Size Point Rect TermCriteria]
            [org.opencv.imgcodecs Imgcodecs]
            [org.opencv.imgproc Imgproc]
            [org.opencv.ml SVM StatModel Ml]))


;;;;;;;;;;;;;;;;;;;; Utility
(defn imread
  "returns a cv Mat of the image at fname"
  [fname] 
  (Imgcodecs/imread fname))

(defn imwrite
  "writes the image Mat to fname"
  [fname img]
  (Imgcodecs/imwrite fname img))

(defn col->gray 
  "Convenient color to gray convertions for cv Mat"
  [src]
  (let [dst (.clone src)]
      (if (= 1 (.channels src))
        dst
        (do
          (Imgproc/cvtColor src dst Imgproc/COLOR_RGB2GRAY)
          dst))))

;;;;;;;;;;;;;;;;;;;; Mat Cnversions
(defn mat->float
  "convert an image Mat to float representation (0 - 255 int to float)"
  [src]
  (let [dst (.clone src)]
    (do
      (.convertTo src dst CvType/CV_32F)
      dst)))

(defn mat->scalar
  "convert an image Mat to scalar representation (0 - 255 int to float)"
  [src]
  (let [dst (.clone src)]
    (do
      (.convertTo src dst CvType/CV_32S)
      dst)))

(defn norm-255
  "Convert to float and devide by 255, represent as percentage"
  [src]
  (let [dst (.clone src)]
    (do
      (Core/divide (mat->float src) (Scalar. 255.0) dst)
      dst)))

;; imgae operations
(defn img-get 
  "get the values of a point in the image"
  [img x y]
  (let [col-vals (.get img x y)]
    (map #(aget col-vals  %) 
         (range (alength col-vals)))))

(defn sub-image
  "get the sub image defined by the image and rect"
  ([image rect]
    (Mat. image rect))
  ([image p1x p1y p2x p2y]
    (Mat. image (Rect. (Point. p1x p1y) (Point. p2x p2y)))))

(defn sub-image-at
  [image mask pos]
  (sub-image image
             (Rect. (Point. (:x pos) (:y pos)) (.size mask))))

(defn sum
  [image]
  (aget (.val (Core/sumElems image)) 0))

(defn area
  [image]
  (* (.width image) (.height image)))

(defn size->map
  [image]
  image)

(defn matched-region 
  "grab the sub section of the source image at the point x y"
  [src mask x y]
  (let [rect (Rect. (Point. x y) (.size mask))]
    (Mat. src rect)))

(defn index->xy
  "take a pixel index and translates it to an (x,y) coordinate"
  [img n]
  (let [x (int (/ n (.cols img)))
        y (mod n (.cols img))]
    (list x y)))

(defn copy-to 
  "capy the tile to the image and return it"
  [img tile-image x y]
  (do
    (.copyTo tile-image 
      (Mat. img (Rect. (Point. x y) (.size tile-image))))
    img))

(defn image-append
  "append two images into a new one"
  [img1 img2]
  (let [r1 (.rows img1)
        c1 (.cols img1)
        r2 (.rows img2)
        c2 (.cols img2)
        mat (Mat. r1 (+ c1 c2) (.type img1))]
    (do
      (.copyTo img1  (Mat. mat (Rect. (Point. 0 0)
                                      (.size img1))))
      (.copyTo img2  (Mat. mat (Rect. (Point. c1 0)
                                      (.size img2))))
      mat)))

(defn image-c-bind
  "append two images into a new one"
  [img1 img2]
  (let [r1 (.rows img1)
        c1 (.cols img1)
        r2 (.rows img2)
        c2 (.cols img2)
        mat (Mat. r1 (+ c1 c2) (.type img1))]
    (do
      (.copyTo img1  (Mat. mat (Rect. (Point. 0 0)
                                      (.size img1))))
      (.copyTo img2  (Mat. mat (Rect. (Point. c1 0)
                                      (.size img2))))
      mat)))

(defn image-r-bind
  "append two images into a new one"
  [img1 img2]
  (let [r1 (.rows img1)
        c1 (.cols img1)
        r2 (.rows img2)
        c2 (.cols img2)
        mat (Mat. (+ r1 r2) c1 (.type img1))]
    (do
      (.copyTo img1  (Mat. mat (Rect. (Point. 0 0)
                                      (.size img1))))
      (.copyTo img2  (Mat. mat (Rect. (Point. 0 r1)
                                      (.size img2))))
      mat)))

(defn max-size
  "returns a size that can contain the largest mat size (w, h)"
  [images]
  (let [ws (map #(.cols %) images)
        hs (map #(.rows %) images)]
    (Mat. (Size. (reduce max ws)
                 (reduce max hs))
          (.type (first images)))))

(defn calc-tile-mat-size
  "calculate the size of the image by the rows 
   and columns width and height, golden ratio ~= 0.618"
  [n w h]
  (let [c     (inc (int (Math/sqrt n)))
        r     (if (integer? (/ n c)) (/ n c) (inc (int (/ n c))))
        nrows (* r h)
        ncols (* c w)]
    {:rows nrows 
     :cols ncols}))

(defn tile-position-func
  "Creates a function to translate the index to the position"
  [n w h]
  (let [c     (inc (int (Math/sqrt n)))
        r     (if (integer? (/ n c)) (/ n c) (inc (int (/ n c))))
        nrows (* r h)
        ncols (* c w)]
    (fn [x]
      (let [temp-c (mod x c)
            temp-r (int (/ x c))]
      (-> (assoc {} :x (* temp-c w))
        (assoc :y (* temp-r h)))))))
      
(defn tile-images
  "Tile many smaller images onto a larger image,
   assumes they are the same size
  "
  [imgs]
  (let [n (count imgs)
        img-vec (vec (reverse imgs))
        img1 (max-size imgs)
        w (.cols img1)
        h (.rows img1)
        dims (calc-tile-mat-size n w h)
        mat (Mat. (:rows dims) (:cols dims) (.type img1) (Scalar. 0 0 0))
        pos-trans (tile-position-func n w h)]
      (loop [dst mat
             carry imgs
             n 0]
        (if (empty? carry)
          dst
          (recur (copy-to dst (first carry)
                          (:x (pos-trans n))
                          (:y (pos-trans n)))
                 (rest carry)
                 (inc n))))))

(defn random-tile-pos
  "draw a random sub-image, the same size as the mask"
  [image mask]
  (let [w (.cols image)
        h (.rows image)
        rand-x (rand-int (- (.cols image) (.cols mask)))
        rand-y (rand-int (- (.rows image) (.rows mask)))]
    (-> (assoc {} :x rand-x)
      (assoc :y rand-y))))



(defn random-sub-image
  "draw a random sub-image, the same size as the mask"
  [image mask]
  (let [w (.cols image)
        h (.rows image)
        rand-x (rand-int (- (.cols image) (.cols mask)))
        rand-y (rand-int (- (.rows image) (.rows mask)))]
    (sub-image image (Rect. (Point. rand-x rand-y) (.size mask)))))



(defn n-random-sub-images
  "Grabs n random sub images, the same size as mask"
  [image mask n]
  (take n (repeatedly #(random-sub-image image mask))))

(defn dump
  "calls the opencv dump function to pring a matrix"
  [image]
  (.dump image))

(defn image->row-vec
  "convert an image Mat to  1xn row vector Mat"
  [image]
  (let [dst (.clone image)]
    (.reshape dst (.channels dst) 1)))

(defn images->row-mat
  "converts a set of images to 1xn row vectors of n pixels"
  [images]
  (let [img (first images)
        num (count images)
        vec1 (image->row-vec img)
        n-pxls (.cols vec1)
        mat (Mat. num n-pxls (.type img))]
    (loop [imgs images
           n 0]
      (if (empty? imgs)
        mat
        (recur (do
                 (copy-to mat (image->row-vec (first imgs)) 0 n)
                 (rest imgs))
               (inc n))))))

;;;;;;;;;;;;;;;;;;;;; Machine Learning
(defn label-at 
  "get the label value at the fiven location"
  [labels n]
  (int (aget (.get labels n 0) 0)))

(defn labels->list
  [labels]
  (map #(label-at labels %)
       (range (.rows labels))))

(defn kmeans-call
  [data labels k iters]
  (Core/kmeans 
       data
       k
       labels
       (TermCriteria. )
       iters 
       Core/KMEANS_PP_CENTERS))

(defn kmeans
  "cluster a set of images using k means, 
   returns an nx1 column vector of labels"
  [images k]
  (let [data (mat->float (images->row-mat images))
        labels (Mat. (count images) 1 
                     (.type (first images)))]
    (do
      (kmeans-call data labels k 100)
      labels)))

(defn reorder-by-labels
  [images labels]
  (let [lbl-list (labels->list labels)
        pairs (partition 
                2 
                (interleave images lbl-list))
        sorted (sort-by 
                 :cluster 
                 (map #(-> (assoc {} :mat (first %))
                         (assoc :cluster (last %)))
                      pairs))]
    (map :mat sorted)))

(defn cluster-images
  "cluster a set of images using k means"
  [images k]
  (reorder-by-labels 
    images 
    (kmeans images k)))

(defn mat->stat-format
  "convert an image Mat to the right format for svm"
  [src]
  (let [dst (.clone src)]
    (do
      (.convertTo src dst CvType/CV_32F)
      dst)))

(defn cluster->color
  [cluster]
  (cond
    (= 0 cluster) (Scalar.   0     0    0)
    (= 1 cluster) (Scalar. 255     0    0)
    (= 2 cluster) (Scalar.   0   255    0)
    (= 3 cluster) (Scalar.   0     0  255)
    (= 4 cluster) (Scalar. 255     0  255)))

(defn mark-cluster
  [image mask pos k]
  (let [dst (.clone image)]
    (copy-to dst 
             (Mat. (.size mask) (.type image) (cluster->color k)) 
             (:x pos) (:y pos))))

(defn mark-clusters-2
  "Draw horizontal lines at a list of rows"
  [img mask points labels]
  (loop [img-tmp img
         ps-tmp  points
         lab-tmp labels]
    (if (empty? ps-tmp)
      img-tmp
      (recur (mark-cluster img-tmp mask (first ps-tmp) (int (first lab-tmp)))
             (rest ps-tmp)
             (rest lab-tmp)))))

(defn mark-clusters
  "Draw horizontal lines at a list of rows"
  [img mask points labels]
  (let [dst (.clone img)]
    (loop [img-tmp dst
           ps-tmp  points
           lab-tmp labels]
      (if (empty? ps-tmp)
        dst
        (recur (do
                 (copy-to 
                   dst 
                   (Mat. (.size mask) (.type dst) (cluster->color (first lab-tmp)))
                   (:x (first ps-tmp)) 
                   (:y (first ps-tmp)))
                 dst)
               (rest ps-tmp)
               (rest lab-tmp))))))


(defn gabor
  "Run a gobor filter over the image"
  [image]
  (let [dst (col->gray (.clone image))]
    (do
      (Imgproc/filter2D dst dst -1
        (Imgproc/getGaborKernel (Size. 9 9) 1 (* 3.14 0.75) 1 1))
      dst)))




(defn svm
  "Train an svm classifier using samples and labels"
  [training-set labels]
  (let [data-rows (images->row-mat training-set)
        data      (mat->stat-format (col->gray data-rows))
        svm-model (doto (SVM/create )
                    (.setType SVM/C_SVC)
                    (.setKernel SVM/LINEAR))]
    (do
      (.train svm-model data Ml/ROW_SAMPLE labels)
      svm-model)))

(defn svm-predict-labels
  [model test-set]
  (let [data-rows (images->row-mat test-set)
        data      (mat->stat-format (col->gray data-rows))
        labels    (Mat. (count test-set) 1 
                        (.type (first test-set)))]
  (do
    (.predict model data labels Ml/ROW_SAMPLE)
    labels)))

(defn svm-predict-image
  [model image]
  (let [data-rows (image->row-vec image)
        data      (mat->stat-format data-rows)]
      (int (.predict model data))))

(defn svm-predict
  "Train an svm classifier using samples and labels"
  [model test-set]
  (let [labels (svm-predict-labels model test-set)]
    (reorder-by-labels test-set labels)))

;;;;;;;;;;;;;;;;;;;; Processing / Filters
(defn laplace
  "apply the lapace filter the src image"
  [src]
  (let [dst (.clone src)]
    (do
      (Imgproc/Laplacian 
        src dst 
        CvType/CV_8U 3 1 0 Core/BORDER_DEFAULT)
      dst)))

(defn blur
  "Apply a guassian blur to the image kernel 5 "
  [img]
  (let [dst (.clone img)]
    (do
      ;;sigmax sigmay
      (Imgproc/GaussianBlur img dst (Size. 5 5) 2 2)
      dst)))

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

(defn threshold
  "filter the image using a threshold"
  [img thresh]
  (let [dst (.clone img)]
    (do
      (Imgproc/threshold dst dst thresh 255.0 Imgproc/THRESH_BINARY)
      dst)))


;;;;;;;;;;;;;;;;;;;; Drawing
(defn color
  [col-str]
  (cond
    (= "red" col-str)   (Scalar. 0     0 255)
    (= "green" col-str) (Scalar. 0   255   0)
    (= "blue" col-str)  (Scalar. 255   0   0)
    (= "black" col-str) (Scalar. 0     0   0)
    (= "white" col-str) (Scalar. 255 255 255)
    :else (Scalar. 0     0   0)))

(defn draw-line
  "draw a line between p1 and p2"
  [img p1 p2]
  (let [dst (.clone img)]
    (do
      (Imgproc/line dst p1 p2 (Scalar. 255 0 0) 1)
      dst)))

(defn point
  [x y]
  (Point. x y))

(defn draw-line!
  "draw a line on the image as a side effect, modify in place"
  ([img p1 p2]
    (Imgproc/line img p1 p2 (Scalar. 0 0 0) 1))
  ([img p1 p2 col]
    (Imgproc/line img p1 p2 (color col) 1)))

(defn draw-h-line 
  "Draw a horiozntal line returns an image with the line drawn"
  [img y]
  (draw-line img 
             (Point. 0 y)
             (Point. (.cols img) y)))

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
      (recur (draw-h-line img-tmp (first ys-tmp))
             (rest ys-tmp)
             (dec n)))))


(defn draw-rect
  "draw a rectangle on the image"
  ([img x1 y1 x2 y2]
    (do
      (draw-line! img (Point. x1 y1) (Point. x2 y1)) ;; top
      (draw-line! img (Point. x1 y1) (Point. x1 y2)) ;; left
      (draw-line! img (Point. x2 y1) (Point. x2 y2)) ;; right
      (draw-line! img (Point. x1 y2) (Point. x2 y2)) ;; bottom
      img))
  ([img x1 y1 x2 y2 col]
    (do
      (draw-line! img (Point. x1 y1) (Point. x2 y1) col) ;; top
      (draw-line! img (Point. x1 y1) (Point. x1 y2) col) ;; left
      (draw-line! img (Point. x2 y1) (Point. x2 y2) col) ;; right
      (draw-line! img (Point. x1 y2) (Point. x2 y2) col) ;; bottom
      img)))


;;;;;;;;;;;;;;;;;;;; Letter mask, subregion, score
(defn get-letter-mask-2
  "get a letter mask for the given string"
  [letter scale thickness]
  (let [baseline (int-array 1)
        size     (Imgproc/getTextSize 
                   letter Core/FONT_HERSHEY_PLAIN
                   scale thickness baseline)
        mat      (Mat. size CvType/CV_8U (Scalar. 0.0))]
    (do
      (println (aget baseline 0))
      (Imgproc/putText 
        mat letter 
        (Point. 0
                (.height size)) 
        Core/FONT_HERSHEY_PLAIN scale (Scalar. 255 255 255) thickness)
      mat)))

(defn t-letter-mask
  []
  (let [baseline (int-array 1)
        letter "g"
        size (Imgproc/getTextSize 
                   letter Core/FONT_HERSHEY_PLAIN
                   2 2 baseline)
        mat (Mat. (Size. (.width size) (+ (.height size) (aget baseline 0))) CvType/CV_8U)
        ;;mat (Mat. (Size. 30 30) CvType/CV_8U)
        txt-orig  (Point. (/ (- (.cols mat) (.width size)) 2)
                          (/ (+ (.rows mat) (.height size)) 2))]
    (do
      (Imgproc/putText
        mat letter 
        txt-orig
        Core/FONT_HERSHEY_PLAIN 2 (Scalar. 255 255 255) 2)
      mat)))

(defn get-letter-mask
  "get a letter mask for the given string"
  [letter scale thickness]
  (let [baseline (int-array 1)
        size (Imgproc/getTextSize 
                   letter Core/FONT_HERSHEY_PLAIN
                   scale thickness baseline)
        mat (Mat. (Size. (.width size) 
                         (+ (.height size) (aget baseline 0)))
                  CvType/CV_8U
                  (Scalar. 0 0 0))
        ;;mat (Mat. (Size. 30 30) CvType/CV_8U)
        txt-orig  (Point. (/ (- (.cols mat) (.width size)) 2)
                          (/ (+ (.rows mat) (.height size)) 2))]
    (do
      (Imgproc/putText
        mat letter 
        txt-orig
        Core/FONT_HERSHEY_PLAIN scale (Scalar. 255 255 255) thickness)
      mat)))

(defn score
  "score the mask against the src image"
  [src mask]
  (let [temp (.clone mask)
        norm-src (mat->float (norm-255 src))
        norm-mask (mat->float (norm-255 mask))]
    (do 
      (Core/multiply norm-src norm-mask temp)
      (aget (.val (Core/sumElems temp)) 0))))


;;;;;;;;;;;;;;;;;;;; Row and Column means
(defn img-mean
  "Calculate the mean value of the image"
  [img]
  (Core/mean img))

;;
(defn row-mean
  "get the mean of a row of pixels in the image"
  [src nrow]
  (let [row-slice (.row src nrow)]
    (calc/mean
      (pmap #(aget (.get row-slice 0 %) 0) 
            (range (.cols row-slice))))))

(defn col-mean
  "get the mean of a col of pixels in the image"
  [src ncol]
  (let [col-slice (.col src ncol)]
    (calc/mean
      (pmap #(aget (.get col-slice % 0) 0)
            (range (.cols col-slice))))))

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
(defn min-in
  "find the min value within the given percentile"
  [vals percent]
  (apply 
    min 
    (take (int (* percent (count vals))) vals)))

(defn clip-index
  "find the index of the min within the percentile"
  [vals percent]
  (let [target (min-in vals 0.33)]
    (first (calc/which vals #(= target %)))))

(defn clip-index-rev
  "find the index of the min within the percentile of a reversed list"
  [vals percent]
  (let [rev-vals (reverse vals)
        target (min-in rev-vals  0.33)]
    (- (count vals)
      (first (calc/which rev-vals #(= target %))))))

(defn clip-ends
  "get the top and bottom mins for clipping"
  [vals percent]
  (let [t-row (clip-index     vals 0.33)
        b-row (clip-index-rev vals 0.33)]
    (list t-row b-row)))

(defn trim-tb
  "takes and image and trips it at the min points on the top and bottom"
  [img vals percent]
  (let [pair (clip-ends vals 0.33)
        t-row (first pair)
        b-row (last pair)]
    (sub-image img 0           t-row 
                   (.cols img) b-row)))

(defn trim-lr
  "takes and image and trips it at the min points on the top and bottom"
  [img vals percent]
  (let [pair (clip-ends vals 0.33)
        l-col (first pair)
        r-col (last pair)]
      (sub-image img l-col 0 
                     r-col (.rows img))))

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
        laplace (laplace (blur (threshold gray 100)))
        r-means (calc/sma 5 (row-means laplace))
        c-means (calc/sma 5 (col-means gray))
        tb-pair (clip-ends r-means 0.33)
        lr-pair (clip-ends c-means 0.33)]
    (sub-image img 
               (first lr-pair) (first tb-pair)
               (last lr-pair)  (last tb-pair))))


 ;; End of file