(ns kilppari-reagent.turtle
  (:require [kilppari-reagent.state :as state]
            [reagent.core :as reagent]))

;; Helpers
(defn deg->rad [deg]
  (* deg (/ js/Math.PI 180)))

(defn add-angle [a b]
  ;; TODO: check for negative angles?
  (-> (+ a b) (mod 360)))

(defn pen-down?
  "Is the pen down at a given step index?"
  [step]
  (->> (get-in @state/app-state [:turtle :script :instructions])
       (take step)                  ;; example:
       (filter #(= :pen (first %))) ;;=> ([:pen :up] [:pen :down])
       last                         ;;=> [:pen :down]
       second                       ;;=> :down
       (not= :up)))                 ;; we do it like this in case the list is
                                    ;; empty.. (not nil) => true

(def start-coords [100 100])
(def start-angle 0)

(def default-turtle-script
  {:instructions
   [[:move 50]
    [:turn-right 45]
    [:move 100]
    [:pen :up]
    [:move 75]
    [:turn-left 45]
    [:pen :down]
    [:move 50]]
   :index 0})

(def initial-turtle
  {:script default-turtle-script
   :coords nil ;; reset-position! sets these 3
   :angle nil
   :line nil
   :playing false
   :playing-id nil})

(defn reset-position! []
  (swap! state/app-state assoc-in [:turtle :coords] start-coords)
  (swap! state/app-state assoc-in [:turtle :line] [{:coords start-coords :step 0}])
  (swap! state/app-state assoc-in [:turtle :angle] start-angle))

(defn init-turtle! []
  (swap! state/app-state assoc :turtle initial-turtle)
  (reset-position!)
  (let [img (js/Image.)]
    (aset img "onload" (fn [] (swap! state/app-state assoc :turtle-img img)))
    (aset img "src" "/img/turtle.png")))

;; Drawing on the canvas

(def turtle-size 32)

(defn canvas-ctx []
  (get @state/app-state :canvas-ctx))

(defn clear-screen []
  (let [ctx (canvas-ctx)]
    (aset ctx "fillStyle" "white")
    (.fillRect ctx 0 0 600 600)))

(defn draw-turtle-img
  "Draws a picture to denote the turtle"
  []
  (let [ctx (canvas-ctx)
        [x y] (get-in @state/app-state [:turtle :coords])
        cx (- x (/ turtle-size 2))
        cy (- y (/ turtle-size 2))
        ;; add 90 to angle because turtle img is pointing up
        ;; but 0 degrees means to the right in canvas
        angledeg (add-angle 90 (get-in @state/app-state [:turtle :angle]))
        anglerad (deg->rad angledeg)
        img (get-in @state/app-state [:turtle-img])]
    (when img
      (.translate ctx x y)
      (.rotate ctx anglerad)
      (.translate ctx (- x) (- y))
      (.drawImage ctx img cx cy turtle-size turtle-size)
      (.setTransform ctx 1 0 0 1 0 0)))) ;; reset transformation to id matrix

(defn draw-line [[x1 y1] [x2 y2]]
  (let [ctx (canvas-ctx)]
    (.beginPath ctx)
    (.moveTo ctx x1 y1)
    (.lineTo ctx x2 y2)
    (.stroke ctx)))

(defn draw-turtle-line
  "Draws the line up to current line of the script"
  []
  (let [turtle (:turtle @state/app-state)
        line-segs (partition 2 1 (:line turtle))]
    (doseq [l line-segs]
      (let [pen-down (pen-down? (-> l second :step))
            c1 (-> l first :coords)
            c2 (-> l second :coords)]
        (when pen-down (draw-line c1 c2))))))

(defn move-turtle! [distance step-n]
  (let [anglerad (deg->rad (get-in @state/app-state [:turtle :angle]))
        [x y] (get-in @state/app-state [:turtle :coords])
        dx (-> anglerad js/Math.cos (* distance))
        dy (-> anglerad js/Math.sin (* distance))]
    (swap! state/app-state assoc-in [:turtle :coords] [(+ x dx) (+ y dy)])
    (swap! state/app-state update-in [:turtle :line] #(conj % {:coords [(+ x dx) (+ y dy)]
                                                               :step step-n}))))

(defn turn-right! [angle]
  (swap! state/app-state update-in [:turtle :angle] #(add-angle % angle)))

(defn turn-left! [angle]
  (swap! state/app-state update-in [:turtle :angle] #(add-angle % (- angle))))

(defn update-turtle!
  "Updates current location based on script"
  []
  (let [turtle (:turtle @state/app-state)
        script (get-in turtle [:script :instructions])
        steps (get-in turtle [:script :index])]
    (reset-position!)
    (clear-screen)
    (doseq [step-n (range steps)]
      (let [[instr data] (nth script step-n)]
        (case instr
          :move (move-turtle! data step-n)
          :turn-right (turn-right! data)
          :turn-left (turn-left! data)
          :pen nil))) ;; pen instructions are handled by (pen-down? step-n)
    (draw-turtle-line)
    (draw-turtle-img)))

(defn turtle-step! []
  (let [turtle (:turtle @state/app-state)
        max-index (count (get-in turtle [:script :instructions]))
        new-index (inc (get-in turtle [:script :index]))]
    (when (>= max-index new-index)
      (swap! state/app-state assoc-in [:turtle :script :index] new-index)
      (update-turtle!))))

(defn turtle-step-back! []
  (let [new-index (dec (get-in @state/app-state [:turtle :script :index]))]
    (when (>= new-index 0)
      (swap! state/app-state assoc-in [:turtle :script :index] new-index)
      (update-turtle!))))

(defn back-to-start! []
  (swap! state/app-state assoc-in [:turtle :script :index] 0)
  (update-turtle!))

(defn go-to-end! []
  (let [max-index (count (get-in @state/app-state [:turtle :script :instructions]))]
    (swap! state/app-state assoc-in [:turtle :script :index] max-index)
    (update-turtle!)))

(defn play-turtle! []
  (let [is-playing (get-in @state/app-state [:turtle :playing])]
    (if is-playing
      (let [interval-id (get-in @state/app-state [:turtle :playing-id])]
        (js/clearInterval interval-id)
        (swap! state/app-state assoc-in [:turtle :playing-id] nil))
      (let [interval-id (js/setInterval turtle-step! 1000)]
        (swap! state/app-state assoc-in [:turtle :playing-id] interval-id)))

    (swap! state/app-state update-in [:turtle :playing] not)))
