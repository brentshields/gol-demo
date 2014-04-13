(ns gol.demo
  (:require [rxatom.core :as rx]
            [cljs.reader :as reader]))

;; Geometry

(def ^:export grid-x 50)
(def ^:export grid-y 50)
(def ^:export cell-size 15)

(def cell-indices (range (* grid-x grid-y)))

(defn cell-coord [idx] [(mod idx grid-x) (quot idx grid-x)])

;; Rendering

(defn cell-color [alive?] (if alive? "blue" "none"))

(defn render-callback [idx]
  #(-> js/document
       (.getElementById (str idx))
       (.setAttribute "fill" (cell-color %))))

(defn cell-svg [idx]
  (let [[x y] (cell-coord idx)]
    (str "<rect id="
         "\"" (str idx) "\""
         " x=" (* cell-size x)
         " y=" (* cell-size y)
         " width=" cell-size
         " height=" cell-size
         " fill=none pointer-events=fill"
         " onclick='gol.demo.flip_cell(this.id);'/>")))

(defn ^:export grid-svg []
  (apply str (map cell-svg cell-indices)))

;; Application State

(def grid (rx/rxatom (zipmap cell-indices (repeat false))))

(def cells (zipmap cell-indices (map #(rx/rxlens-key grid %) cell-indices)))

(def deltas (atom {}))

(def history (atom []))

;; Game of Life Simulation

(defn neighbors [idx]
  (let [[x y] (cell-coord idx)]
    (for [dx [-1 0 1]
          dy [-1 0 1]
          :let [[nx ny] [(mod (+ x dx) grid-x) (mod (+ y dy) grid-y)]]
          :when (not= [nx ny] [x y])]
      (+ nx (* grid-x ny)))))

(defn alive-next-gen? [cell-alive? n0 n1 n2 n3 n4 n5 n6 n7]
  (let [living-neighbors (count (filter identity [n0 n1 n2 n3 n4 n5 n6 n7]))]
    (if cell-alive?
      (#{2 3} living-neighbors)
      (= 3 living-neighbors))))

(defn next-gen-cell [idx]
  (let [[c n0 n1 n2 n3 n4 n5 n6 n7]
        (->> idx neighbors (cons idx) (map cells) vec)]
    (rx/rxfn c n0 n1 n2 n3 n4 n5 n6 n7 alive-next-gen?)))

;; Actions

(defn- update-grid []
  (swap! history conj @grid)
  (rx/commit-frame! grid))

(defn ^:export flip-cell [idx-string]
  (let [idx (reader/read-string idx-string)]
    (swap! (cells idx) not)
    (update-grid)))

(defn ^:export step-back []
  (when-not (empty? @history)
    (reset! grid (peek @history))
    (swap! history pop)
    (rx/commit-frame! grid)))

(defn ^:export step-forward []
  (swap! grid conj @deltas)
  (swap! deltas empty)
  (update-grid))

;; Observers

(def cell-renderers
  (doall (map #(rx/observe (cells %) (render-callback %)) cell-indices)))

(def delta-generators
  (let [callback #(fn [alive?] (swap! deltas assoc % alive?))]
    (doall (map #(rx/observe (next-gen-cell %) (callback %)) cell-indices))))
