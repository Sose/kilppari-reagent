(ns kilppari-reagent.prod
  (:require [kilppari-reagent.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
