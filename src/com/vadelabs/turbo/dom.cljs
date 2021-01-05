(ns com.vadelabs.turbo.dom
  (:require
   ["react" :as react]
   ["react-dom" :as rdom]
   [com.vadelabs.turbo.dom.helpers :as tdh])
  (:require-macros [com.vadelabs.turbo.dom]))

(defn attach
  [elem node]
  (rdom/render elem node)
  nil)

(defn detach
  [node]
  (rdom/unmountComponentAtNode node))

(defn ^js/React get-react [] react)

(defn element?
  [x]
  ^boolean (.isValidElement (get-react) x))

(defn create-element
  ([tag]
   (create-element tag nil))
  ([tag opts]
   (create-element tag opts nil))
  ([tag opts & children]
   (apply (.createElement (get-react)) tag opts children)))

(defn $
  [type & args]
  (let [type (if (keyword? type)
               (name type)
               type)
        native? (or (keyword? type)
                    (string? type))]
    (cond
      (map? (first args))
      (apply create-element
             type
             (if native?
               (tdh/-native-props (first args))
               (tdh/-props (first args)))
             (rest args))
      (nil? (first args))
      (apply create-element
             type
             nil
             (rest args))
      :else
      (apply create-element
             type
             nil
             args))))
