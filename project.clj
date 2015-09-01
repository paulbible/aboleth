(defproject aboleth "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [opencv/opencv "3.0.0"]
                 [opencv/opencv-native "3.0.0"]
                 [incanter "1.5.6"]]
  :java-source-paths ["src/aboleth"]
  :injections [(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)])
