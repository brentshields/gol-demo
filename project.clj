(defproject gol-demo "0.1.0-SNAPSHOT"
  :description "Conway's Game of Life demo in the browser"
  :url "https://github.com/brentshields/gol-demo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2173"]
                 [org.clojure/core.match "0.2.1"]
                 [funken "0.1.0-SNAPSHOT"]]

  :plugins [[lein-cljsbuild "1.0.2"]]

  :cljsbuild {
    :builds [
      {:source-paths ["src"]
       :compiler {:output-to "gol/demo.js"
                  :output-dir "gol"
                  :optimizations :advanced
                  :pretty-print false}}]})
