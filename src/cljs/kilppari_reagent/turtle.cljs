(ns kilppari-reagent.turtle
  (:require [kilppari-reagent.state :as state]
            [reagent.core :as reagent]))

;; Helpers

(def start-coords [100 100])
(def start-angle 0)

(def default-turtle-script
  [[:move 50]
   [:turn-right 45]
   [:move 100]
   [:turn-right 90]
   [:move 75]
   [:turn-right 45]
   [:move 50]])

(def initial-turtle
  {:coords start-coords
   :angle start-angle
   :line [start-coords]
   :script default-turtle-script
   :script-index 0
   :playing false
   :playing-id nil})

(defn init-turtle! []
  (swap! state/app-state assoc :turtle initial-turtle))

(defn reset-position! []
  (swap! state/app-state assoc-in [:turtle :coords] start-coords)
  (swap! state/app-state assoc-in [:turtle :line] [start-coords])
  (swap! state/app-state assoc-in [:turtle :angle] start-angle))

(defn active-turtle []
  (get @state/app-state :turtle))

;; Drawing on the canvas

(def turtle-size 20)

(defn canvas-ctx []
  (get @state/app-state :canvas-ctx))

(defn clear-screen []
  (let [ctx (canvas-ctx)]
    (aset ctx "fillStyle" "white")
    (.fillRect ctx 0 0 600 600)))

(defn draw-turtle-rect
  "Draws a rectangle to denote the turtle"
  []
  (let [ctx (canvas-ctx)
        [centerx centery] (get-in @state/app-state [:turtle :coords])
        x (- centerx (/ turtle-size 2))
        y (- centery (/ turtle-size 2))]
    (aset ctx "fillStyle" "black")
    (.fillRect ctx x y turtle-size turtle-size)))

(defn draw-line [[[x1 y1] [x2 y2]]]
  (let [ctx (canvas-ctx)]
    (.beginPath ctx)
    (.moveTo ctx x1 y1)
    (.lineTo ctx x2 y2)
    (.stroke ctx)))

(defn draw-turtle-line
  "Draws the line up to current line of the script"
  []
  (let [turtle (active-turtle)
        line-segs (partition 2 1 (:line turtle))]
    (doseq [l line-segs]
      (draw-line l))))

(defn deg->rad [deg]
  (* deg (/ js/Math.PI 180)))

(defn move-turtle! [distance]
  (let [anglerad (deg->rad (get-in @state/app-state [:turtle :angle]))
        [x y] (get-in @state/app-state [:turtle :coords])
        dx (-> anglerad js/Math.cos (* distance))
        dy (-> anglerad js/Math.sin (* distance))]
    (swap! state/app-state assoc-in [:turtle :coords] [(+ x dx) (+ y dy)])
    (swap! state/app-state update-in [:turtle :line] #(conj % [(+ x dx) (+ y dy)]))))

(defn add-angle [a b]
  (-> (+ a b) (mod 360)))

(defn turn-right! [angle]
  (swap! state/app-state update-in [:turtle :angle] #(add-angle % angle)))

(defn update-turtle!
  "Updates current location based on script"
  []
  (let [turtle (active-turtle)
        script (:script turtle)
        steps (:script-index turtle)]
    (reset-position!)
    (clear-screen)
    (doseq [instruction (take steps script)]
      (case (first instruction)
        :move (move-turtle! (second instruction))
        :turn-right (turn-right! (second instruction))))
    (draw-turtle-line)
    (draw-turtle-rect)))

(defn turtle-step! []
  (let [turtle (active-turtle)
        max-index (count (:script turtle))
        new-index (inc (:script-index turtle))]
    (when (>= max-index new-index)
      (swap! state/app-state assoc-in [:turtle :script-index] new-index)
      (update-turtle!))))

(defn turtle-step-back! []
  (let [turtle (active-turtle)
        new-index (dec (:script-index turtle))]
    (when (>= new-index 0)
      (swap! state/app-state assoc-in [:turtle :script-index] new-index)
      (update-turtle!))))

(defn back-to-start! []
  (swap! state/app-state assoc-in [:turtle :script-index] 0)
  (update-turtle!))

(defn go-to-end! []
  (let [max-index (count (get-in @state/app-state [:turtle :script]))]
    (swap! state/app-state assoc-in [:turtle :script-index] max-index)
    (update-turtle!)))

(defn start-playing-turtle! []
  (let [start-playing (get-in @state/app-state [:turtle :playing])]
    (if start-playing
      (let [interval-id (js/setInterval turtle-step! 1000)]
        (swap! state/app-state assoc-in [:turtle :playing-id] interval-id))
      (let [interval-id (get-in @state/app-state [:turtle :playing-id])]
        (js/clearInterval interval-id)
        (swap! state/app-state assoc-in [:turtle :playing-id] nil)))))

(defn play-turtle! []
  (swap! state/app-state update-in [:turtle :playing] not)
  (start-playing-turtle!))
