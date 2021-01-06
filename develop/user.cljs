(ns user
  (:require
   [com.vadelabs.turbo.dom :as td]
   [goog.dom :as gdom]))

(defn start
  []
  (td/attach "Hello" (gdom/getElement "app"))
  (js/console.log "Started..."))

(defn stop
  []
  (td/detach (gdom/getElement "app"))
  (js/console.log "Stopped..."))

(defn restart
  []
  (stop)
  (start))
