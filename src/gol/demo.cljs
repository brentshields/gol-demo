(ns gol.demo
  (:require [funken.core :as fnk] [cljs.core.match])
  (:require-macros [cljs.core.match.macros :refer [match]]))

;; Geometry

(def ^:export grid-x 120)
(def ^:export grid-y 70)
(def ^:export cell-size 10)

(def cell-indices (range (* grid-x grid-y)))

(defn cell-coord [idx] [(mod idx grid-x) (quot idx grid-x)])

;; Application State

(def grid (fnk/state (zipmap cell-indices (repeat :dead))))

(def cells (zipmap cell-indices (map #(fnk/key-lens grid %) cell-indices)))

(def deltas (atom {}))

(def history (fnk/state []))

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
    (apply fnk/view (cells idx) (conj neighbor-cells make-pair))))

;; Actions

(defn- push-history-and-update []
  (swap! history conj @grid)
  (fnk/commit! grid history))

(defn ^:export flip-cell [idx]
  (swap! (cells idx) flip)
  (push-history-and-update))

(defn ^:export step-back []
  (when-not (empty? @history)
    (reset! grid (peek @history))
    (swap! history pop)
    (fnk/commit! grid history)))

(defn ^:export step-forward []
  (when-not (empty? @deltas)
    (swap! grid conj @deltas)
    (swap! deltas empty)
    (push-history-and-update)))

;; setup delta tracking
(let [callback #(fn [_ _ _ [cur next]]
                  (if (= cur next)
                    (swap! deltas dissoc %)
                    (swap! deltas assoc % next)))]
  (dorun (map #(add-watch (cell-transition %) ::deltas (callback %))
                          cell-indices)))

;; Rendering

(def cell-color {:alive "blue" :dead "gainsboro"})

(defn element-by-id [str-id] (.getElementById js/document str-id))

(defn cell-svg [idx]
  (let [[x y] (cell-coord idx)]
    (str "<rect id=\"" (str idx) "\""
         " x=" (* cell-size x) " y=" (* cell-size y)
         " width=" cell-size " height=" cell-size
         " fill=" (cell-color :dead)
         " onclick='gol.demo.flip_cell(parseInt(this.id));'/>")))

(defn ^:export grid-svg [] (apply str (map cell-svg cell-indices)))

(defn render-callback [idx]
  #(.setAttribute (element-by-id (str idx)) "fill" (cell-color %4)))

;; render changes in cell values
(dorun (map #(add-watch (cells %) ::renderer (render-callback %)) cell-indices))

;; connect the generation counter
(add-watch (fnk/view history count)
           ::history-counter
           #(set! (.-textContent (element-by-id "gencount"))
                  (str "Generation: " %4)))
