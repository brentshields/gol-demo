(ns gol.demo
  (:require [rxatom.core :as rx]
            [cljs.core.match])
  (:require-macros [cljs.core.match.macros :refer [match]]))

;; Geometry

(def ^:export grid-x 50)
(def ^:export grid-y 50)
(def ^:export cell-size 15)

(def cell-indices (range (* grid-x grid-y)))

(defn cell-coord [idx] [(mod idx grid-x) (quot idx grid-x)])

;; Rendering

(def cell-color {:alive "blue" :dead "none"})

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
         " onclick='gol.demo.flip_cell(parseInt(this.id));'/>")))

(defn ^:export grid-svg []
  (apply str (map cell-svg cell-indices)))

;; Application State

(def grid
  (rx/rxatom (conj (zipmap cell-indices (repeat :dead)) {:generation 0})))

(def cells (zipmap cell-indices (map #(rx/rxlens-key grid %) cell-indices)))

(def generation (rx/rxlens-key grid :generation))

(def deltas (atom {}))

(def history (atom []))

;; Game of Life Simulation

(def flip {:alive :dead :dead :alive})

(defn neighbors [idx]
  (let [[x y] (cell-coord idx)]
    (for [dx [-1 0 1]
          dy [-1 0 1]
          :let [[nx ny] [(mod (+ x dx) grid-x) (mod (+ y dy) grid-y)]]
          :when (not= [nx ny] [x y])]
      (+ nx (* grid-x ny)))))

(defn next-gen-val [center n0 n1 n2 n3 n4 n5 n6 n7]
  (let [living-neighbors (count (filter #{:alive} [n0 n1 n2 n3 n4 n5 n6 n7]))]
    (match [center living-neighbors]
      [:alive 2] :alive
      [:alive 3] :alive
      [:dead  3] :alive
      :else :dead)))

(defn cell-transition [idx]
  (let [neighbor-cells (vec (map cells (neighbors idx)))
        make-pair (fn [c n0 n1 n2 n3 n4 n5 n6 n7]
                    [c (next-gen-val c n0 n1 n2 n3 n4 n5 n6 n7)])]
    (apply rx/rxfn (cells idx) (conj neighbor-cells make-pair))))

;; Actions

(defn- push-history-and-update []
  (swap! history conj @grid)
  (rx/commit-frame! grid))

(defn ^:export flip-cell [idx]
  (swap! (cells idx) flip)
  (push-history-and-update))

(defn ^:export step-back []
  (when-not (empty? @history)
    (reset! grid (peek @history))
    (swap! history pop)
    (rx/commit-frame! grid)))

(defn ^:export step-forward []
  (swap! generation inc)
  (rx/commit-frame! grid))

;; Observers

(def cell-renderers
  (doall (map #(rx/observe (cells %) (render-callback %)) cell-indices)))

(def delta-generators
  (let [callback #(fn [[cur next]]
                    (if (= cur next)
                      (swap! deltas dissoc %)
                      (swap! deltas assoc % next)))]
    (doall (map #(rx/observe (cell-transition %) (callback %)) cell-indices))))

(def generation-transition
  (let [last-gen (atom 0)
        apply-deltas (fn [gen]
                       (when (> gen @last-gen)
                         (swap! grid conj @deltas)
                         (swap! deltas empty)
                         (push-history-and-update))
                       (reset! last-gen gen))]
    (rx/observe generation apply-deltas)))
