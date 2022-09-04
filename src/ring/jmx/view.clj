(ns ring.jmx.view
  (:require [clojure.java.io :as io]
            [ring.jmx.type :refer [type-str form-input render-value]]))

(set! *warn-on-reflection* true)

(defn- header [model]
  [:header
   [:nav
    [:select {:id "domain" :onchange "javasctipt: document.location=this.value"}
     (for [{:keys [domain uri selected]} (:all-domains model)]
       [:option (cond-> {:value uri}
                  selected (assoc :selected ""))
        (str domain)])]
    ;; select name under domain
    (when (some :selected (:domain-names model))
      (when-let [names (:domain-names model)]
        [:select {:onchange "javasctipt: document.location=this.value"}
         (for [name names]
           [:option
            (cond-> {:value (:uri name)}
              (:selected name) (assoc :selected ""))
            ;; ???
            (:canonical-key-property-list-string name)])]))]])


(defn- attributes-table [attributes]
  [:table
   [:thead [:tr [:th "Name"] [:th "Type"] [:th "Value"]]]
   [:tbody
    (for [attribute attributes]
      [:tr
       [:td (:name attribute)]
       [:td [:code (type-str (str (:type attribute)))]]
       [:td (render-value attribute)]])]])


(def style [:style (slurp (clojure.java.io/resource "ring/jmx/style.css"))])
(def script [:script (slurp (clojure.java.io/resource "ring/jmx/script.js"))])

(defn operation-block [operation params {:keys [call-result call-error] :as executed?}]
  [:article (if executed? {:class "active operation"} {:class "operation"})
            [:b (type-str (.getReturnType operation))]
            [:b (.getName operation)]
            (when (not= (.getDescription operation) (.getName operation))
              [:p (.getDescription operation)])
    [:form {:method "post"}
      [:input {:type "hidden" :name "action" :value (.getName operation)}]
      (when (not-empty (.getSignature operation))
        [:table
        (for [p (map (fn [p s] (assoc p :type (.getType s) :name (.getName s)))
                     (or params (repeat {}))
                     (.getSignature operation))]
          (list
            [:tr
              [:td [:code (type-str (:type p))]]
              [:td (:name p)]
              [:td (form-input (assoc p :value (:raw p)))]]
            (when-let [t ^Throwable (:error p)]
              [:tr [:td {:class "error" :colspan 3}
                [:pre [:b (.getSimpleName (class t))]]
                [:pre (.getMessage t)]]])))])
      [:input {:type "submit" :value "Execute"}]
      (when call-result
        [:details {:open "open"}
          [:summary "result " (.getReturnType operation)]
          (render-value {:type (.getReturnType operation) :value call-result})])
      (when call-error
        [:details {:open "open"}
                  [:summary (str call-error)]
                  [:pre (pr-str call-error)]])]])

(defn- page-operations [model]
  (when-let [operations (not-empty (:operations model))]
    [:section
     [:h3 "Operations"]
     (for [op operations]
       [:div (operation-block op nil nil)])]))

(defn- page-attributes [model]
  (when-let [attributes (not-empty (:attributes model))]
    [:section
     [:h3 "Attributes"]
     (attributes-table attributes)]))

(def version (or (some-> (io/resource "jmx-ui-version") slurp)
                 (System/getProperty "ring-jmx.version")
                 (assert false)))

(defn page-footer []
  [:footer
   [:p "ring.jmx version " [:span version]]])

(defn page [model]
  (list [:head
         [:meta {:charset "UTF-8"}]
         style, script]
        [:body
          (header model)
          [:main
            (when (and (:selected-domain model) (not (:selected-name model)))
              [:section
              [:h3 "Domain " (:selected-domain model)]
              [:ul
                (for [name (:domain-names model)]
                  [:li [:a {:href (:uri name)} (:canonical-name name)]])]])

            (page-attributes model)
            (page-operations model)]
          (page-footer)]))
