(ns kilppari-reagent.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [kilppari-reagent.core-test]))

(doo-tests 'kilppari-reagent.core-test)
