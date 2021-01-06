(ns com.vadelabs.turbo.dom-test
  (:require
   [devcards.core :as dc]
   [com.vadelabs.turbo.dom :as td]
   [cljs-bean.core :refer [bean]])
  (:require-macros
   [devcards.core :as dc :refer [deftest]]
   [cljs.test :refer [is testing]]))

(defn ^:export start
  []
  (devcards.core/start-devcard-ui!)
  (js/console.log "Started..."))

(defn ^:export stop
  []
  (js/console.log "Stopped..."))

(defn restart
  []
  (stop)
  (start))

(deftest dom-tests
  (testing "Pass no arguments"
    (let [elem (td/div "Hello")]
      (is (td/element? elem))
      (is (= "div" (unchecked-get elem "type")))
      (is (= {:children "Hello"} (bean (unchecked-get elem "props"))))))
  (testing "Pass a CLJS map"
    (let [elem (td/div {:class "a"} "Hello")]
      (is (td/element? elem))
      (is (= "div" (unchecked-get elem "type")))
      (is (= {:className "a"
              :children "Hello"} (bean (unchecked-get elem "props"))))))
  (testing "Pass nil attrs"
    (let [elem (td/div nil "Hello")]
      (is (td/element? elem))
      (is (= "div" (unchecked-get elem "type")))
      (is (= {:children "Hello"} (bean (unchecked-get elem "props"))))))
  (testing "Pass dynamic attrs"
    (let [attrs {:class "test-class"}
          elem (td/div attrs "Hello")]
      (is (td/element? elem))
      (is (= {:className "test-class"
              :children "Hello"} (bean (unchecked-get elem "props"))))))
  (testing "Pass dynamic attrs with assoc"
    (let [attrs (assoc {} :class "assoc-class")
          elem (td/div attrs "Hello")]
      (is (td/element? elem))
      (is (= {:className "assoc-class"
              :children "Hello"} (bean (unchecked-get elem "props"))))))
  (testing "Nested elements as children (no props)"
    (let [elem (td/div (td/p "Hello"))
          props (unchecked-get elem "props")
          children (unchecked-get props "children")]
      (is (td/element? elem))
      (is (= "div" (unchecked-get elem "type")))
      (is (td/element? children))
      (is (= "p" (unchecked-get children "type")))
      (is (= {:children "Hello"} (bean (unchecked-get children "props")))))))
