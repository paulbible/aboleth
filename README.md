# aboleth

A small project combining Clojure with OpenCV

# Getting Started

Follow [OpenCV with for Clojure][opencv] to get started.

Install OpenCV, [cmake][cmake] is needed. Make sure you have [ant][ant] installed.
Once OpenCV is installed, install the java bindings. The link above details how to package the native libs as java jars.

You will end up with two jars that need to be included opencv-300.jar and opencv-native-300.jar and libopencv_java247.dylib in the project folder.

Add these jars as dependencies.

```clojure
(defproject simple-sample "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [opencv/opencv "2.4.7"] ; added line
                 [opencv/opencv-native "2.4.7"]]) ;added line
```


# Notes

I'm experimenting in the aboelth.core namespace. I want to move more to the other namespaces:

* cv
* vis
* calc

 

[opencv]: http://docs.opencv.org/doc/tutorials/introduction/clojure_dev_intro/clojure_dev_intro.html
[cmake]: http://www.cmake.org/
[ant]: http://ant.apache.org/
[lrepo]: https://github.com/kumarshantanu/lein-localrepo
## Usage


## License

Copyright © 2015 TODO

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
