(ns kilppari-reagent.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]
   [kilppari-reagent.state :refer [app-state]]
   [kilppari-reagent.turtle :as turtle]
   [kilppari-reagent.state :as state]))


;; -------------------------
;; Routes


(def router
  (reitit/router
   [["/" :index]
    ["/turtle" :turtle]
    ["/about" :about]]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

;; -------------------------
;; Page components

(defn home-page []
  (fn []
    [:span.main
     [:h1 "Welcome to kilppari-reagent"]]))

(defn about-page []
  (fn []
    [:span.main
     [:h1 "About kilppari-reagent"]
     [:p "Made with Clojure and Clojurescript and Reagent to practice."]
     [:p "Original turtle vector graphic is from "
      [:a
       {:href "https://www.vecteezy.com/free-vector/nature"}
       "Nature Vectors by Vecteezy"]]
     [:img {:id "turtle-pic" :src "/img/turtle.png" :alt "turtle"}]]))

(defn canvas-element []
  (reagent/create-class
   {:component-did-mount
    (fn [this]
      (let [canvas (rdom/dom-node this)
            canvas-ctx (.getContext canvas "2d")]

        ;;(init-drawing-state!) ; doing this in core for now
        (swap! app-state assoc :canvas-ctx canvas-ctx)
        ;; (.addEventListener canvas "mousemove" mouse-move-handler)
        ;; (.addEventListener canvas "mouseup" mouse-up-handler)
        ;; (.addEventListener canvas "mousedown" mouse-down-handler
        (turtle/update-turtle!)))

    :reagent-render
    (fn []
      [:canvas {:width 400 :height 300 :id "piirtely"}])}))

(declare script-view)

(defn script-item [instr data cur-i view-i show-active?]
  (let [class (if (and show-active? (= cur-i view-i)) "active" "step")]
    (case instr
      :repeat [:li
               {:class class :key cur-i}
               [:div
                [:div (str ":repeat " (first data))]
                [:div (script-view (second data) false)]]]

      [:li {:class class :key cur-i} (str instr " " (first data))])))

(defn script-view [script main-script?]
  (let [active-index (:index script)
        instructions (:instructions script)]
    [:ul
     (for [i (range (count instructions))]
       (let [[instr & data] (nth instructions i)]
         (script-item instr data active-index i main-script?)))
     [:li (when (>= active-index (count instructions)) {:key "end" :class "active"}) "end"]]))

(defn turtle-page []
  (fn []
    [:span.main
     [:h1 "Turtle"]

     [:div.container
      [:div.row
       [:div.col [canvas-element]]
       [:div.col [script-view (get-in @app-state [:turtle :script]) true]]]]

     [:div.turtle-buttons
      [:button {:on-click turtle/back-to-start!} "<<"]
      [:button {:on-click turtle/turtle-step-back!} "<-"]
      [:button {:on-click turtle/play-turtle!}
       (if (get-in @app-state [:turtle :playing]) "| |" "|>")]
      [:button {:on-click turtle/turtle-step!} "->"]
      [:button {:on-click turtle/go-to-end!} ">>"]]]))

;; -------------------------
;; Translate routes -> page components


(defn page-for [route]
  (case route
    :index #'home-page
    :about #'about-page
    :turtle #'turtle-page))


;; -------------------------
;; Page mounting component


(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      [:div
       [:header
        [:p
         [:a {:href (path-for :index)} "Home"] " | "
         [:a {:href (path-for :about)} "About"] " | "
         [:a {:href (path-for :turtle)} "Turtle"]]]
       [page]
       [:footer
        [:p "Kilppari nyymi 2021"]]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (rdom/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (let [match (reitit/match-by-path router path)
            current-page (:name (:data  match))
            route-params (:path-params match)]
        (reagent/after-render clerk/after-render!)
        (session/put! :route {:current-page (page-for current-page)
                              :route-params route-params})
        (clerk/navigate-page! path)))

    :path-exists?
    (fn [path]
      (boolean (reitit/match-by-path router path)))})
  (accountant/dispatch-current!)
  (turtle/init-turtle!)
  (mount-root))
