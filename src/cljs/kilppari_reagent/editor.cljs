(ns kilppari-reagent.editor
  (:require [reagent.core :as reagent]
            [kilppari-reagent.state :refer [app-state]]
            [kilppari-reagent.turtle :refer [set-script!]]
            [kilppari-reagent.scriptview :refer [script-view]]
            [kilppari-reagent.parsing :as parsing]))

(defonce txt (reagent/atom "")) ;;TODO: move this to app-state?

(defn text-editor [txt-atom]
  [:div.editor-wrapper
   [:textarea.editor
    {:rows 15
     :cols 30
     :value @txt-atom
     :on-change #(reset! txt-atom (-> % .-target .-value))}]])

(defn edit-page []
  (let [default-script (get @app-state :default-script)]
    ;; if the editor is empty, set it to default script on loading the editor
    (when (and default-script (empty? @txt)) (reset! txt default-script))
    (if-not default-script
      [:span.main
       [:h1 "Loading"]]
      (fn []
        (let [[parse-status parsed] (parsing/parse-turtle @txt)]
          [:span.main
           [:h1 "Editor"]
           [:div.row
            [:div.col [text-editor txt]]
            (if (= :success parse-status)
              [:div.col [script-view parsed 0 false]]
              [:div.col [:div parsed]])]
           [:div.editor-buttons
            [:button {:on-click #(set-script! parsed)} "Set as active"]
            [:button {:on-click #(reset! txt default-script)} "Reset"]]])))))
