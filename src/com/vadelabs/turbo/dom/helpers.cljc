(ns com.vadelabs.turbo.dom.helpers
  (:require
   [clojure.string :as str]
   #?@(:cljs [[goog.object :as gobj]]))
  #?(:cljs (:require-macros [com.vadelabs.turbo.dom.helpers])))

(defn gen-docstring
  [tag]
  (str "Returns a React DOM element " tag))

(def special-re #"^(aria-|data-|--).*")

#?(:cljs (def camel-re (js/RegExp "-(\\w)", "g")))

(defn camel-case
  "Returns camel case version of the string, e.g. \"http-equiv\" becomes \"httpEquiv\"."
  [s]
  (if (or (keyword? s)
          (string? s)
          (symbol? s))
    (let [name-str (name s)]
      ; this is hot path so we want to use low-level interop
      #?(:clj  (cond
                 (some? (re-matches special-re name-str)) name-str
                 (= (subs name-str 0 1) "'") (subs name-str 1)
                 :else (str/replace name-str #"-(\w)" #(str/upper-case (second %))))
         :cljs (cond
                 (some? (.match name-str special-re)) name-str
                 (= (.substring name-str 0 1) "'") (.substring name-str 1)
                 :else (.replace name-str camel-re #(.toUpperCase %2)))))

    s))

#_(camel-case "hello-world")

(defn kw->str
  [kw]
  (let [kw-ns (namespace kw)
        kw-name (name kw)]
    (if (nil? kw-ns)
      kw-name
      (str kw-ns "/" kw-name))))

(defn set-obj
  [o k v]
  #?(:clj (conj o k v)
     :cljs (doto o (gobj/set k v))))

#?(:cljs (defn ->js [x]
           (clj->js x :keyword-fn (comp camel-case name))))

(defn primitive-obj
  ([] #?(:clj '[cljs.core/js-obj]
         :cljs #js {}))
  ([m]
   #?(:clj (if (map? m)
             (primitive-obj m (primitive-obj))
             `(primitive-obj ~m))
      :cljs (primitive-obj m (primitive-obj))))
  ([m o]
   (if (seq m)
     (recur (rest m)
            (let [entry (first m)]
              (set-obj o
                       (camel-case (kw->str (key entry)))
                       #?(:clj `(->js ~(val entry))
                          :cljs (->js (val entry))))))
     #?(:clj (list* o)
        :cljs o))))

(defn into-js-array
  [aseq]
  #?(:clj (list* (into '[cljs.core/array] aseq))
     :cljs (into-array aseq)))

#?(:cljs
   (defn merge-obj [obj1 obj2]
     (if (nil? obj2)
       obj1
       (doto obj1
         (gobj/extend obj2)))))

(defn seq-to-class
  [klass]
  (->> klass
       (remove nil?)
       (map str)
       (str/join " ")))

#?(:clj
   (defn unquote-class
     [klass]
     (if (sequential? klass)
       (seq-to-class klass)
       (str klass))))

#?(:clj
   (defn normalize-class
     [klass]
     (cond
       (string? klass) klass

       (and (list? klass)
            (= (first klass) 'quote))
       (unquote-class (second class))

       :else
       `(normalize-klass ~klass))))

#?(:cljs
   (defn normalize-class
     [klass]
     (cond
       (string? klass)
       klass

       (sequential? klass)
       (seq-to-class klass)

       :else
       (str klass))))

#?(:cljs
   (defn or-undefined
     [v]
     (if (nil? v)
       js/undefined
       v)))

(defn native-style
  [style]
  (cond
    (map? style)
    (primitive-obj style)

    (vector? style)
    (into-js-array (map #(if (map? %) (primitive-obj %) %) style))

    :else
    #?(:clj `(native-style ~style)
       :cljs style)))

(defn -native-props
  ([m]
   #?(:clj (if (symbol? m)
             `(-native-props ~m (primitive-obj))
             (if-let [spread-sym (cond
                                   (contains? m '&) '&
                                   (contains? m :&) :&)]
               `(merge-obj ~(-native-props (dissoc m spread-sym) (primitive-obj))
                           (-native-props ~(get m spread-sym)))
               (-native-props m (primitive-obj))))
      :cljs (if (map? m)
              (-native-props m (primitive-obj))
              m)))
  ([m o]
   (if (seq m)
     (recur (rest m)
            (let [entry (first m)
                  k (key entry)
                  v (val entry)]
              (case k
                :class (set-obj o "className" (normalize-class v))
                :for (set-obj o "htmlFor" v)
                :style (set-obj o "style" (native-style v))
                :value (set-obj o "value" #?(:clj `(or-undefined ~v)
                                             :cljs (or-undefined v)))
                (set-obj o (camel-case (kw->str k)) v))))
     #?(:clj (list* o)
        :cljs o))))

#_(-native-props {:abcd "fghj" :style 'hello})
#_(-native-props {:style ["hello"]})
#_(-native-props {:foo-bar "baz"})
#_(let [dattrs {:abcd "Hello"}]
    (-native-props dattrs))
#_(let [dattrs (assoc {} :absc "Boom")]
    (-native-props dattrs))

(defmacro native-props
  [m]
  (-native-props m))

(defn -props
  ([m]
   #?(:clj (if-let [spread-sym (cond
                                 (contains? m '&) '&
                                 (contains? m :&) :&)]
             `(merge-obj ~(-props (dissoc m spread-sym) (primitive-obj))
                         (-props ~(get m spread-sym)))
             (-props m (primitive-obj)))
      :cljs (if (map? m)
              (-props m (primitive-obj))
              m)))
  ([m o]
   (if (seq m)
     (recur (rest m)
            (let [entry (first m)]
              (set-obj o (kw->str (key entry)) (val entry))))
     #?(:clj (list* o)
        :cljs o))))

#_(-props {:foo-bar "baz"})

(defmacro props
  [m]
  (-props m))
