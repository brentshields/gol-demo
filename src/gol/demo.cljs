(ns gol.demo
  (:require [rxatom.core :as rx]
            [cljs.reader :as reader]))

(def ^:export grid-x 50)
(def ^:export grid-y 50)
(def ^:export cell-size 15)

(defn cell-color [set?] (if set? "blue" "none"))

(def grid-indices (for [y (range grid-y) x (range grid-x)]
                    [x y]))

(defn cell-svg [[x y :as idx]]
  (str "<rect id="
       "\"" (pr-str idx) "\""
       " x=" (* cell-size x)
       " y=" (* cell-size y)
       " width=" cell-size
       " height=" cell-size
       " fill=none pointer-events=fill"
       " onclick='gol.demo.flip_cell(this.id);'/>"))

(defn ^:export grid-svg []
  (apply str (map cell-svg grid-indices)))


(def grid (rx/rxatom (zipmap grid-indices (repeat false))))

(def cells (zipmap grid-indices (map #(rx/rxlens-key grid %) grid-indices)))

(defn observe-cell-display [idx]
  (rx/observe (cells idx)
              #(-> js/document
                   (.getElementById (pr-str idx))
                   (.setAttribute "fill" (cell-color %)))))

(def cell-renderers (doall (map observe-cell-display grid-indices)))

(def gen-counter (rx/rxatom 0))

(defn neighbors [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :let [neighbor [(mod (+ x dx) grid-x) (mod (+ y dy) grid-y)]]
        :when (not= neighbor [x y])]
    neighbor))

(defn alive-next-gen? [cell-alive? n0 n1 n2 n3 n4 n5 n6 n7]
  (let [alive-neighbors (count (filter identity [n0 n1 n2 n3 n4 n5 n6 n7]))]
    (if cell-alive?
      (#{2 3} alive-neighbors)
      (= 3 alive-neighbors))))

(defn next-gen-cell [idx]
  (let [[c n0 n1 n2 n3 n4 n5 n6 n7]
        (->> idx neighbors (cons idx) (map cells) vec)]
    (rx/rxfn c n0 n1 n2 n3 n4 n5 n6 n7 alive-next-gen?)))

(def history (atom '()))

(defn update-grid []
  (swap! history conj @grid)
  (rx/commit-frame! grid))

(defn ^:export step-back []
  (when-not (empty? @history)
    (reset! grid (first @history))
    (swap! history next)
    (rx/commit-frame! grid)))

(defn ^:export flip-cell [coord-string]
  (let [idx (reader/read-string coord-string)]
    (swap! (cells idx) not)
    (update-grid)))

(defn ^:export next-gen []
  (swap! gen-counter inc)
  (rx/commit-frame! gen-counter)
  (update-grid))

(defn gen-updater [idx]
  (let [cell (cells idx)
        last-gen (atom 0)]
    (rx/observe (rx/rxfn cell (next-gen-cell idx) gen-counter vector)
                (fn [[cur next-gen gen-count]]
                   (when (and (> gen-count @last-gen)
                              (not= cur next-gen))
                     (reset! cell next-gen))
                   (reset! last-gen gen-count)))))

(def next-gen-updaters (doall (map gen-updater grid-indices)))
