(ns gol.demo
  (:require [rxatom.core :as rx]
            [cljs.reader :as reader]))

(def grid-x 100)
(def grid-y 100)
(def cell-size 10)
(defn cell-color [set?] (if set? "black" "none"))

(def grid-indices (for [y (range grid-y) x (range grid-x)]
                    [x y]))


(defn cell-svg [[x y :as idx]]
  (str "<rect id="
       "\"" (pr-str idx) "\""
       " x=" (* cell-size x)
       " y=" (* cell-size y)
       " width=" cell-size
       " height=" cell-size
       " stroke=black fill=none stroke-width=2px pointer-events=fill"
       " onclick='gol.demo.flip_cell(this.id); gol.demo.step()'/>"))

(defn grid-height []
  (* grid-y cell-size))

(defn grid-width []
  (* grid-x cell-size))

(defn grid-svg []
  (apply str (map cell-svg grid-indices)))


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
