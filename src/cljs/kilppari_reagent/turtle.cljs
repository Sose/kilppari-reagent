(ns kilppari-reagent.turtle
  (:require [kilppari-reagent.state :refer [app-state]]))

;; Helpers
(defn deg->rad [deg]
  (* deg (/ js/Math.PI 180)))

(defn add-angle [a b]
  ;; TODO: check for negative angles?
  (-> (+ a b) (mod 360)))

(defn pen-down?
  "Is the pen down at a given step index?"
  [step]
  ;; TODO: this should work inside repeat statements
  (->> (get-in @app-state [:turtle :script])
       (take step)
       (filter #(= :pen (first %)))
       #(get-in % [:args :value])
       (not= :up)))

(def start-coords [100 100])
(def start-angle 0)

(def default-turtle-script
  [])

(def initial-turtle
  {:script default-turtle-script
   :script-index 0
   :script-defs {}
   :coords nil ;; reset-position! sets these 3
   :angle nil
   :line nil
   :playing false
   :playing-id nil})

(defn reset-position! []
  (swap! app-state assoc-in [:turtle :coords] start-coords)
  (swap! app-state assoc-in [:turtle :line] [{:coords start-coords :step 0}])
  (swap! app-state assoc-in [:turtle :angle] start-angle))

(defn init-turtle! []
  (swap! app-state assoc :turtle initial-turtle)
  (reset-position!)
  (let [img (js/Image.)]
    (aset img "onload" (fn [] (swap! app-state assoc :turtle-img img)))
    (aset img "src" "/img/turtle.png")))

;; Drawing on the canvas

(def turtle-size 32)

(defn canvas-ctx []
  (get @app-state :canvas-ctx))

(defn clear-screen []
  (let [ctx (canvas-ctx)]
    (aset ctx "fillStyle" "white")
    (.fillRect ctx 0 0 600 600))) ;; TODO: determine size of the canvas

(defn draw-turtle-img
  "Draws a picture to denote the turtle"
  []
  (let [ctx (canvas-ctx)
        [x y] (get-in @app-state [:turtle :coords])
        cx (- x (/ turtle-size 2))
        cy (- y (/ turtle-size 2))
        ;; add 90 to angle because turtle img is pointing up
        ;; but 0 degrees means to the right in canvas
        angledeg (add-angle 90 (get-in @app-state [:turtle :angle]))
        anglerad (deg->rad angledeg)
        img (get-in @app-state [:turtle-img])]
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
  (let [turtle (:turtle @app-state)
        line-segs (partition 2 1 (:line turtle))]
    (doseq [l line-segs]
      (let [pen-down (pen-down? (-> l second :step))
            c1 (-> l first :coords)
            c2 (-> l second :coords)]
        (when pen-down (draw-line c1 c2))))))

(defn move-turtle! [distance step-n]
  (let [anglerad (deg->rad (get-in @app-state [:turtle :angle]))
        [x y] (get-in @app-state [:turtle :coords])
        dx (-> anglerad js/Math.cos (* distance))
        dy (-> anglerad js/Math.sin (* distance))]
    (swap! app-state assoc-in [:turtle :coords] [(+ x dx) (+ y dy)])
    (swap! app-state update-in [:turtle :line] #(conj % {:coords [(+ x dx) (+ y dy)]
                                                         :step step-n}))))

(defn turn-right! [angle]
  (swap! app-state update-in [:turtle :angle] #(add-angle % angle)))

(defn turn-left! [angle]
  (swap! app-state update-in [:turtle :angle] #(add-angle % (- angle))))

(declare do-script!)

(defn turtle-repeat! [[times instructions] fn-args inside-fn-name]
  (doseq [_ (range times)]
    (do-script! instructions (count instructions) fn-args inside-fn-name)))

(defn turtle-call!
  [fn-name arg-value]
  (let [fn-def (get-in @app-state [:turtle :script-defs (keyword fn-name)])
        args (get fn-def :args)
        instructions (get fn-def :instructions)]
    (do-script! instructions (count instructions) [args arg-value] (keyword fn-name))))

(defn prepare-function! [fn-name {:keys [args instructions]}]
  (swap! app-state assoc-in [:turtle :script-defs (keyword fn-name)] {:args args :instructions instructions})
  nil) ;; explicitly return nil just in case?

(defn prepare-let! [var-name value]
  (swap! app-state assoc-in [:turtle :var-defs (keyword var-name)] value)
  nil)

(defn prepare-script!
  "Plucks function definitions out of the script and defines those fns"
  [script]
  (into []
        (filter
         some?
         (for [i (range (count script))]
           (let [[instr args] (nth script i)
                 a (:args args)
                 fn-name (:value (first a))
                 data (:value (second a))]
             (case instr
               :function (prepare-function! fn-name data)
               :let (prepare-let! fn-name data)
               (nth script i)))))))

(defn get-var-by-name [var-name fn-args inside-fn-name]
  (if (and fn-args (= var-name (first fn-args)))
    (second fn-args)
    (get-in @app-state [:turtle :var-defs var-name])))

(defn get-val-or-var [{:keys [type value]} fn-args inside-fn-name]
  (case type
    :value value
    :variable (get-var-by-name value fn-args inside-fn-name)
    (js/console.log "error in get-val-or-var." type value)))

(defn do-script!
  ([script until-step]
   (do-script! script until-step nil nil))
  ([script until-step fn-args inside-fn-name]
   (doseq [step-n (range until-step)]
     (let [[instr args] (nth script step-n)
           data (get-in args [:args])]
       (case instr
         :move (move-turtle! (get-val-or-var data fn-args inside-fn-name) step-n)
         :turn-right (turn-right! (get-val-or-var data fn-args inside-fn-name))
         :turn-left (turn-left! (get-val-or-var data fn-args inside-fn-name))
         :repeat (turtle-repeat! [(get-val-or-var (first data) fn-args inside-fn-name)
                                  (:value (second data))]
                                 fn-args, inside-fn-name)
         :function (js/console.log "ERROR: function in do-script!")
         :call (turtle-call! (:value (first data)) (:value (second data)))
         :pen nil))))) ;; pen instructions are handled by (pen-down? step-n)

(defn update-turtle!
  "Runs a turtle until the turtle's script-index and draws the image"
  []
  (let [turtle (:turtle @app-state)
        script (get-in turtle [:script])
        steps (get-in turtle [:script-index])]
    (reset-position!)
    (clear-screen)
    (do-script! script steps)
    (draw-turtle-line)
    (draw-turtle-img)))

(defn turtle-step!
  "Advances the script by one major step.
  TODO: add support for sub-scripts like 'repeat'"
  []
  (let [turtle (:turtle @app-state)
        max-index (count (get-in turtle [:script]))
        new-index (inc (get-in turtle [:script-index]))]
    (when (>= max-index new-index)
      (swap! app-state assoc-in [:turtle :script-index] new-index)
      (update-turtle!))))

(defn turtle-step-back! []
  (let [new-index (dec (get-in @app-state [:turtle :script-index]))]
    (when (>= new-index 0)
      (swap! app-state assoc-in [:turtle :script-index] new-index)
      (update-turtle!))))

(defn back-to-start! []
  (swap! app-state assoc-in [:turtle :script-index] 0)
  (update-turtle!))

(defn go-to-end! []
  (let [max-index (count (get-in @app-state [:turtle :script]))]
    (swap! app-state assoc-in [:turtle :script-index] max-index)
    (update-turtle!)))

(defn play-turtle!
  "Uses Javascripts setInterval to animate a turtle every 1 second.
  Calling play-turtle! while the turtle is already playing, stops it."
  []
  (let [is-playing (get-in @app-state [:turtle :playing])]
    (if is-playing
      (let [interval-id (get-in @app-state [:turtle :playing-id])]
        (js/clearInterval interval-id)
        (swap! app-state assoc-in [:turtle :playing-id] nil))
      (let [interval-id (js/setInterval turtle-step! 1000)]
        (swap! app-state assoc-in [:turtle :playing-id] interval-id)))

    (swap! app-state update-in [:turtle :playing] not)))

(defn set-script!
  "Prepare a script by removing fn definitions and then set it as active"
  [script]
  (let [prepared (prepare-script! script)]
    (swap! app-state assoc-in [:turtle :script] prepared)
    (swap! app-state assoc-in [:turtle :script-index] 0)))
