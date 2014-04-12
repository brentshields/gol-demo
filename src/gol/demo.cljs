(ns gol.demo
  (:require [rxatom.core :as rx]
            [cljs.reader :as reader]))

(def grid-x 2)
(def grid-y 2)
(def cell-size 25)
(defn cell-color [set?] (if set? "black" "none"))

(def grid-indices (for [y (range grid-y) x (range grid-x)]
                    [x y]))

(def grid (rx/rxatom (zipmap grid-indices (repeat false))))

(def cells
  (zipmap grid-indices (map #(rx/rxlens-key grid %) grid-indices)))


(def history (atom '()))

(defn step-back []
  (when-not (empty? @history)
    (reset! grid (first @history))
    (swap! history next)
    (rx/commit-frame! grid)))


(defn flip-cell [coord-string]
  (let [idx (reader/read-string coord-string)]
    (swap! (cells idx) not)))

(defn step []
  (swap! history conj @grid)
  (rx/commit-frame! grid))

(defn observe-cell [idx]
  (rx/observe (cells idx)
              #(-> js/document
                   (.getElementById (pr-str idx))
                   (.setAttribute "fill" (cell-color %)))))

(def observers (doall (map observe-cell grid-indices)))
