# aboleth

A small project combining Clojure with OpenCV

# Getting Started

Follow [OpenCV with for Clojure][opencv] to get started.

Install OpenCV by downloading [cmake][cmake]. Make sure you have [ant][ant] installed.

I think both static and dynamic libraries are needed. You will end up with two jars:

opencv-300.jar and libopencv_java300.dylib

Install [localrepo][lrepo] and include opencv-300.jar

`
(defproject simple-sample "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [opencv/opencv "2.4.7"] ; added line
                 [opencv/opencv-native "2.4.7"]]) ;added line
`


 

[opencv]: http://docs.opencv.org/doc/tutorials/introduction/clojure_dev_intro/clojure_dev_intro.html
[cmake]: http://www.cmake.org/
[ant]: http://ant.apache.org/
[lrepo]: https://github.com/kumarshantanu/lein-localrepo
## Usage


## License

Copyright © 2015 TODO

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
