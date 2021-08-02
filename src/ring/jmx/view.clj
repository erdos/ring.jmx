(ns ring.jmx.view
  (:require [ring.jmx.type :refer [type-str]]))

(defn- hiccup-visitor [tree]
  (cond
    (string? tree) (println tree)
    (number? tree) (println tree)
    (nil? tree)    nil
    (seq? tree)    (run! hiccup-visitor tree)

    (vector? tree)
    (let [tag (first tree)
          opts (if (map? (second tree)) (second tree) {})
          opts-str  (some->> (for [[k v] opts]
                               (if (#{:selected :readonly :checked :disabled} k)
                                 (when v (name k))
                                 (str (name k) "=" \" v \")))
                             (not-empty)
                             (clojure.string/join " " )
                             (str " "))
          body (if (map? (second tree)) (nnext tree) (next tree))]
      (assert (keyword? tag))
      (cond
        (not-empty body)
        (do (print (str "<" (name tag) opts-str ">"))
            (run! hiccup-visitor body)
            (print (str "</" (name tag) ">")))
        (= :br tag) (print "<br>")
        :else       (print (str "<" (name tag) opts-str "/>"))))))

(defn hiccup-str [tree]
  (with-out-str
    (println "<!doctype html>")
    (println "<meta charset=\"UTF-8\">")
    (hiccup-visitor tree)))

(defn- header [model]
  [:header
   [:nav
    [:select {:onchange "javasctipt: document.location=this.value"}
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
            (:canonicalKeyPropertyListString name)])]))]])

(defmulti render-value :type)

(defmethod render-value :default [{:keys [type value]}]
  [:pre (str value)])

(doseq [type ["long" "int" "float" "double" "byte" "short"]]
  (defmethod render-value type [{:keys [value]}]
    [:pre [:i value]]
    ))

(defmethod render-value "boolean" [{:keys [value writable]}]
  (assert (boolean? writable))
  [:input {:type "checkbox" :disabled (not writable) :checked value}])

(defmethod render-value "javax.management.openmbean.CompositeDataSupport"
  [{:keys [value]}]
  [:b "!!!" (pr-str value)]
  [:ol
   (for [v (.values value)]
     [:li (render-value {:value v :type (.getName (class v))})]
     )]
  )

(defmethod render-value "javax.management.openmbean.TabularData"
  [{:keys [value]}]
  [:table
   [:tr [:th "Key"] [:th "Value"]]
   (for [k (.keySet value)]
     [:tr
      [:td (str k)]
      [:td
       (let [x (.get value (into-array k))
             c (.getName (class x))]
         (render-value {:value x :type c}))
       ]])])

(defn- attributes-table [attributes]
  [:table
   [:thead [:tr [:th "Name"] [:th "Type"] [:th "Value"]]]
   [:tbody
    (for [attribute attributes]
      [:tr
       [:td (:name attribute)]
       [:td (type-str (str (:type attribute)))]
       [:td (render-value attribute)]])]])

(def style
  [:style
   "td {background:#eee; padding: 4px}"
   "th {text-align: left; background: #ddd; padding: 4px}"
   "nav {background: darkmagenta; padding: 1em}"
   "footer {background: #ddd; padding:1em; min-height: 50px}"
   "nav > select {margin: 4px}"
   "main {padding:1em}"
   "body {padding:0;margin:0}"
   "article > button {display: none}"
   "article:hover > button {display: block}"

   "body{height:100vh}"
   "body{display:flex; flex-direction:column;}"
   "footer{margin-top:auto;}"
   ])

(defn- view-operation [m]
  [:article
   [:b (type-str (:returnType m))]
   [:b (:name m)]

   [:pre (pr-str m)]
   (when (not= (:description m) (:name m))
     [:p (:description m)])
   [:div]
   [:button "Execute"]
   ])

(defn- page-operations [model]
  (when-let [operations (not-empty (:operations model))]
    [:section
     [:h3 "Operations"]
     (for [m operations]
       (view-operation m))]))

(defn- page-attributes [model]
  (when-let [attributes (not-empty (:attributes model))]
    [:section
     [:h3 "Attributes"]
     (attributes-table attributes)]))

(defmacro ^:private get-version []
  (System/getProperty "ring-jmx.version"))

(defn page-footer []
  [:footer
   [:p "ring.jmx version " [:span (get-version)]]]
  )

(defn page [model]
  [:head
   ; [:meta {:charset "UTF-8"}]
   ]
  [:body
   style
   (header model)
   [:main
    (when (and (:active-domain model) (not (:active-name model)))
      [:section
       [:h3 "Domain " (:active-domain model)]
       [:ul
        (for [name (:domain-names model)]
          [:li [:a {:href (:uri name)} (:canonicalName name)]])]])

    (page-attributes model)
    (page-operations model)]
   (page-footer)
   ])
