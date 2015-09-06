# aboleth

A small project combining Clojure with OpenCV

I'm experimenting in the aboelth.core namespace. I want to move more to the other namespaces:

* cv	OpenCV code
* vis	Code to view images and visualize signals
* calc	math operations on signals

Ultimate Goal: Read text from the scan.

# Getting Started
## Usage

`cd aboleth`

`lein repl`

```clojure
(use 'aboleth.core)

(in-ns `aboleth.core)

;;view an image
(vis/view-image img-p8)

;;convert from color to gray scale
(vis/view-image (cv/col->gray img-p8))

;;Apply multiple filters sequentially
(vis/view-image 
  (-> (cv/blur img-p8)
    (cv/blur)
    (cv/laplace)))
    
;;create an image mask for a letter
(def a-mask 
  (cv/get-letter-mask "a" 2 2))
  
(vis/view-image a-mask)

;;generate letter masks
(def letter-masks
  (map #(cv/get-letter-mask (str %) 2 2) 
       (seq "abcdefghijklmnopqrstuvwxyz0123456789()[]~!@#$%^&*_+-=")))
       
(vis/view-image 
  (cv/tile-images letter-masks))

;;get random samples of size mask
(def samples
  (cv/n-random-sub-images img-p8 a-mask 500))
  
(vis/view-image
  (cv/tile-images
    samples))

;;cluster samples using k means
(vis/view-image
  (cv/tile-images
    (cv/cluster-images samples 5)))
    
    
```
## Installation

Follow [OpenCV with for Clojure][opencv] to get started.

Install OpenCV, [cmake][cmake] is needed. Make sure you have [ant][ant] installed.
Once OpenCV is installed, install the java bindings. The link above details how to package the native libs as java jars.

You will end up with two jars that need to be included opencv-300.jar and opencv-native-300.jar and libopencv_java300.dylib in the project folder.

Add these jars as dependencies.

```clojure
(defproject aboleth "0.1.0-SNAPSHOT"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [opencv/opencv "3.0.0"]
                 [opencv/opencv-native "3.0.0"]
                 [incanter "1.5.6"]]
  :java-source-paths ["src/aboleth"]
  :injections [(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)])
```

Two Classes from image viewing were taken from [JavaOpenCVBook][jviewer]. ImageProcessor and ImageViewer.

The program also uses [incanter][ican].

# Links
* [OpenCV Java Docs](http://docs.opencv.org/java/)
* [OpenCV Docs](http://docs.opencv.org/modules/refman.html)
* [Incanter API](http://liebke.github.io/incanter/)
* [Machine Learning tutorial for OpenCV](http://bytefish.de/blog/machine_learning_opencv/)

[opencv]: http://docs.opencv.org/doc/tutorials/introduction/clojure_dev_intro/clojure_dev_intro.html
[cmake]: http://www.cmake.org/
[ant]: http://ant.apache.org/
[lrepo]: https://github.com/kumarshantanu/lein-localrepo
[ican]: http://incanter.org/
[jviewer]: https://github.com/JavaOpenCVBook/code

## License

Copyright © 2015 

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
