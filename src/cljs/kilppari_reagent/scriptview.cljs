(ns kilppari-reagent.scriptview)

(declare script-view)

(defn script-item [instr args active-i view-i show-active?]
  (let [class (if (and show-active? (= active-i view-i)) "active" "step")
        data (get-in args [:args])]
    (case instr
      :repeat [:li.list-group-item
               {:class class :key view-i}
               [:div
                [:div (str ":repeat " (:value (first data)))]
                [:div (script-view (:value (second data)) nil false)]]]

      :function [:li.list-group-item
                 {:key (first data)}
                 [:div
                  [:div (str ":fn " (:value (first data)))]
                  [:div (script-view (get-in (second data) [:value :instructions]) nil false)]]]

      :let [:li.list-group-item
            {:key (-> data first :value)}
            (str "let " (-> data first :value) " = " (-> data second :value))]

      [:li.list-group-item {:class class :key view-i} (str instr " " (:value data))])))

(defn script-view [instructions active-index show-active?]
  [:ul.list-group
   (for [i (range (count instructions))]
     (let [[instr args] (nth instructions i)]
       (script-item instr args active-index i show-active?)))
   [:li.list-group-item {:key "end"} "end"]])
