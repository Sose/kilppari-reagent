(ns kilppari-reagent.scriptview)

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
                 {:key (first data)}
                 [:div
                  [:div (str ":fn " (first data))]
                  [:div (script-view (:instructions (second data)) nil false)]]]

      [:li.list-group-item {:class class :key view-i} (str instr " " data)])))

(defn script-view [instructions active-index show-active?]
  [:ul.list-group
   (for [i (range (count instructions))]
     (let [[instr & data] (nth instructions i)]
       (script-item instr data active-index i show-active?)))
   [:li.list-group-item {:key "end"} "end"]])
