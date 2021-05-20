(ns kilppari-reagent.core
  (:require
   [clojure.pprint :refer [pprint]]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]
   [ajax.core :as ajax :refer [GET]]
   [kilppari-reagent.state :refer [app-state]]
   [kilppari-reagent.turtle :as turtle]
   [kilppari-reagent.state :as state]
   [kilppari-reagent.parsing :as parsing]))

;; Load a default script with AJAX
(defn get-default-script! []
  (GET "/example.turtle"
    {:handler (fn [data]
                (swap! app-state assoc :default-script data))
     :error-handler (fn [err]
                      (js/alert "error loading default script")
                      (js/console.log err))}))


;; -------------------------
;; Routes


(def router
  (reitit/router
   [["/" :index]
    ["/turtle" :turtle]
    ["/about" :about]
    ["/edit" :edit]]))

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

(defn script-item [instr data active-i view-i show-active?]
  (let [class (if (and show-active? (= active-i view-i)) "active" "step")]
    (case instr
      :repeat [:li.list-group-item
               {:class class :key view-i}
               [:div
                [:div (str ":repeat " (first data))]
                [:div (script-view (second data) nil false)]]]

      :function [:li.list-group-item
                 {:key (second data)}
                 [:div
                  [:div (str ":fn " (first data))]
                  [:div (script-view (second data) nil false)]]]

      [:li.list-group-item {:class class :key view-i} (str instr " " (first data))])))

(defn script-view [instructions active-index show-active?]
  [:ul.list-group
   (for [i (range (count instructions))]
     (let [[instr & data] (nth instructions i)]
       (script-item instr data active-index i show-active?)))
   [:li.list-group-item {:key "end"} "end"]])

(defn turtle-page []
  (fn []
    [:span.main
     [:h1 "Turtle"]

     [:div.container
      [:div.row
       [:div.col [canvas-element]]
       [:div.col [script-view
                  (get-in @app-state [:turtle :script])
                  (get-in @app-state [:turtle :script-index])
                  true]]]]

     [:div.turtle-buttons
      [:button {:on-click turtle/back-to-start!} "<<"]
      [:button {:on-click turtle/turtle-step-back!} "<-"]
      [:button {:on-click turtle/play-turtle!}
       (if (get-in @app-state [:turtle :playing]) "| |" "|>")]
      [:button {:on-click turtle/turtle-step!} "->"]
      [:button {:on-click turtle/go-to-end!} ">>"]]]))

(defn set-script! [script]
  (let [prepared (turtle/prepare-script! script)]
    (swap! app-state assoc-in [:turtle :script] prepared)
    (swap! app-state assoc-in [:turtle :script-index] 0)))

(defn edit-page []
  (let [default-script (get @app-state :default-script)
        txt (reagent/atom default-script)]
    (if-not default-script
      [:span.main
       [:h1 "Loading"]]
      (fn []
        (let [parsed (parsing/parse-turtle @txt)]
          [:span.main
           [:h1 "Editor"]
           [:div.row
            [:div.col [:textarea {:rows 15
                                  :cols 30
                                  :value @txt
                                  :on-change #(reset! txt (-> % .-target .-value))}]]
            (if parsed
              [:div.col [script-view parsed 0 false]]
              [:div.col "No parse :("])]
           [:div.editor-buttons
            [:button {:on-click #(set-script! parsed)} "Set as active"]
            [:button {:on-click #(reset! txt default-script)} "Reset"]]])))))

;; -------------------------
;; Translate routes -> page components


(defn page-for [route]
  (case route
    :index #'home-page
    :about #'about-page
    :turtle #'turtle-page
    :edit #'edit-page))


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
         [:a {:href (path-for :turtle)} "Turtle"] " | "
         [:a {:href (path-for :edit)} "Edit"]]]
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
  (get-default-script!)
  (turtle/init-turtle!)
  (mount-root))
