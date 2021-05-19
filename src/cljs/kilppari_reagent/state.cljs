(ns kilppari-reagent.state
  (:require [reagent.core :as reagent]))

(defonce app-state (reagent/atom {:canvas-context nil}))
