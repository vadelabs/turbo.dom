(ns com.vadelabs.turbo.dom-card
  (:require
   [devcards.core :as dc]
   [com.vadelabs.turbo.dom :as td])
  (:require-macros
   [devcards.core :refer [defcard]]))

(defn ^:export start
  []
  (dc/start-devcard-ui!)
  (js/console.log "Started..."))

(defn ^:export stop
  []
  (js/console.log "Stopped..."))

(defn restart
  []
  (stop)
  (start))

(defcard Div (td/div "Hello"))
