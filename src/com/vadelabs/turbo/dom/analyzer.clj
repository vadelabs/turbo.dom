(ns com.vadelabs.turbo.dom.analyzer
  (:require
   [cljs.analyzer :as ana]
   [cljs.analyzer.api :as ana-api]))

(defn infer-input-type
  [env x]
  (ana/infer-tag env (ana-api/no-warn (ana-api/analyze env x))))
