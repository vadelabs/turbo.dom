(ns com.vadelabs.turbo.dom
  (:refer-clojure :exclude [time map meta])
  (:require
   [com.vadelabs.turbo.dom.helpers :as tdh]))

(declare input textarea option select a abbr address area article aside audio b base bdi
         bdo big blockquote body br button canvas caption cite code col colgroup data datalist
         dd del details dfn dialog div dl dt em embed fieldset figcaption figure footer form
         h1 h2 h3 h4 h5 h6 head header hr html i iframe img ins kbd keygen label legend li link
         main map mark menu menuitem meta meter nav noscript object ol optgroup output p param
         picture pre progress q rp rt ruby s samp script section small source span strong style
         sub summary sup table tbody td tfoot th thead time title tr track u ul var video wbr
         circle clipPath ellipse g line mask path pattern polyline rect svg text defs
         linearGradient polygon radialGradient stop tspan)

(def tags '#{input textarea option select a abbr address area article aside audio b base bdi
             bdo big blockquote body br button canvas caption cite code col colgroup data datalist
             dd del details dfn dialog div dl dt em embed fieldset figcaption figure footer form
             h1 h2 h3 h4 h5 h6 head header hr html i iframe img ins kbd keygen label legend li link
             main map mark menu menuitem meta meter nav noscript object ol optgroup output p param
             picture pre progress q rp rt ruby s samp script section small source span strong style
             sub summary sup table tbody td tfoot th thead time title tr track u ul var video wbr
             circle clipPath ellipse g line mask path pattern polyline rect svg text defs
             linearGradient polygon radialGradient stop tspan})

(defmacro $
  [type & args]
  (let [native? (or (keyword? type)
                    (string? type)
                    (:native (meta type)))
        type (if (keyword? type)
               (name type)
               type)]
    (cond
      (map? (first args))
      `^js/React.Element (.createElement
                          (get-react)
                          ~type
                          ~(if native?
                             `(tdh/native-props ~(first args))
                             `(tdh/props ~(first args)))
                          ~@(rest args))
      (nil? (first args))
      `^js/React.Element (.createElement
                          (get-react)
                          ~type
                          nil
                          ~@(rest args))
      (symbol? (first args))
      `^js/React.Element (.createElement
                          (get-react)
                          ~type
                          ~(if native?
                             `(tdh/native-props ~(first args))
                             `(tdh/props ~(first args)))
                          ~@(rest args))
      :else
      `^js/React.Element (.createElement
                          (get-react)
                          ~type
                          nil
                          ~@args))))

(defn gen-tag-macro
  [tag]
  `(defmacro ~tag ~(tdh/gen-docstring tag)
     {:style/indent :defn}
     [& args#]
     `(com.vadelabs.turbo.dom/$ ~(keyword '~tag) ~@args#)))

(defmacro gen-tag-macros
  []
  `(do
     ~@(for [tag tags]
         (gen-tag-macro tag))))

(gen-tag-macros)
