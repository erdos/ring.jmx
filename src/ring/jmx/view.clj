(ns ring.jmx.view
  (:require [clojure.string]
            [ring.jmx.type :refer [type-str form-input render-value]]))

(set! *warn-on-reflection* true)

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
       [:td (type-str (str (:type attribute)))]
       [:td (render-value attribute)]])]])

(def style
  [:style
   "td {background:#eee; padding: 4px}"
   "th {text-align: left; background: #ddd; padding: 4px}"
   "nav {background: darkmagenta; padding: 1em}"
   "nav { background: linear-gradient(to top, #f953c6, darkmagenta); }"

   "footer {background: #ddd; padding:1em; min-height: 50px}"
   "nav > select {margin: 4px}"
   "main {padding:1em}"
   "body {padding:0;margin:0}"
   "article > button {display: none}"
   "article:hover > button {display: block}"
   "article.active {background: #fea}"

   "body{height:100vh}"
   "body{display:flex; flex-direction:column;}"
   "footer{margin-top:auto;}"
   ])

(defn- view-operation [m]
  [:article (when (:active? m) {:class "active"})
   [:b (type-str (:returnType m))]
   [:b (:name m)]

   [:pre (pr-str m)]
   (when (not= (:description m) (:name m))
     [:p (:description m)])
   [:div]
   [:form {:method "post" :action (str "?action=" (:name m))}
    (when (not-empty (:signature m))
      [:table
       (for [p (:signature m)]
         [:tr
          [:td (type-str (:type p))]
          [:td (:name p)]
          [:td (form-input p)]])])
    [:input {:type "submit" :value "Execute" :method "POST"}]]
   (when (:active? m)
     [:div
      [:pre (str (:call-result m))]])
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
    (when (and (:selected-domain model) (not (:selected-name model)))
      [:section
       [:h3 "Domain " (:selected-domain model)]
       [:ul
        (for [name (:domain-names model)]
          [:li [:a {:href (:uri name)} (:canonical-name name)]])]])

    (page-attributes model)
    (page-operations model)]
   (page-footer)
   ])
