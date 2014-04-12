(ns gol.demo
  (:require [rxatom.core :as rx]
            [cljs.reader :as reader]))

(def grid (rx/rxatom {[0 0] true
                      [1 0] false
                      [0 1] false
                      [1 1] true}))

(def grid-observer(rx/observe grid #(js/alert (str "Grid updated:" %))))

(defn flip-cell [coord-string]
  (js/alert (str "calling with" coord-string))
  (let [cell (rx/rxlens-key grid (reader/read-string coord-string))]
    (swap! cell not)))

(defn step [] (rx/commit-frame! grid))
