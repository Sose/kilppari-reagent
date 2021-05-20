(ns kilppari-reagent.editor
  (:require [reagent.core :as reagent]
            [kilppari-reagent.state :refer [app-state]]
            [kilppari-reagent.turtle :refer [set-script!]]
            [kilppari-reagent.scriptview :refer [script-view]]
            [kilppari-reagent.parsing :as parsing]))

(defonce txt (reagent/atom "")) ;;TODO: move this to app-state?

(defn edit-page []
  (let [default-script (get @app-state :default-script)]
    (when (empty? @txt) (reset! txt default-script))
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