(ns ring.jmx.view)

(defn- hiccup-visitor [tree]
  (cond
    (string? tree) (println tree)
    (number? tree) (println tree)
    (nil? tree)    nil
    (seq? tree)    (run! hiccup-visitor tree)

    (vector? tree)
    (let [tag (first tree)
          opts (if (map? (second tree)) (second tree) {})
          opts-str  (some->> (for [[k v] opts] (if (= k :selected)
                                                 "selected"
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
   [:nav {:style "background: darkmagenta; padding: 1em"}
    [:select {:onchange "javasctipt: document.location=this.value"}
     (for [{:keys [domain uri selected]} (:all-domains model)]
       [:option (cond-> {:value uri}
                  selected (assoc :selected ""))
        (str domain)])]
    ;; select name under domain
    (when-let [names (:domain-names model)]
      [:select {:onchange "javasctipt: document.location=this.value"}
       (for [name names]
         [:option
          (cond-> {:value (:uri name)}
            (:selected name) (assoc :selected ""))
          ;; ???
          (:canonicalKeyPropertyListString name)])])
    [:noscript ;; so that it can be fetched with wget --recursive
     [:ul
      (for [{:keys [domain uri selected]} (:all-domains model)]
        [:li [:a {:href uri} (str domain)]]
        )]]]])

(defmulti render-value :type)

(defmethod render-value :default [{:keys [type value]}]
  [:pre (str value)])

(doseq [type ["long" "int" "boolean" "float" "double" "byte" "short"]]
  (defmethod render-value type [{:keys [value]}]
    [:pre [:i value]]
    ))

(defn- attributes-table [attributes]
  [:table
   [:thead [:tr [:th "Name"] [:th "Type"] [:th "Value"]]]
   [:tbody
    (for [attribute attributes]
      [:tr
       [:td (:name attribute)]
       [:td (-> (str (:type attribute))
                (.replace "javax.management.openmbean." "")
                (.replace "java.lang." "")
                (.replace "javax.management." "")
                )]
       [:td (render-value attribute)]])]])

(defn page [model]
  [:body {:style "padding: 0; margin: 0"}
   [:style
    "td {background:#eee; padding: 4px}"
    "th {text-align: left; background: #ddd; padding: 4px}"
    "nav > select {margin: 4px}"]
   (header model)
   [:div {:style "padding: 1em"}
    (when-let [attributes (not-empty (:attributes model))]
      [:div
       [:h3 "Attributes"]
       (attributes-table attributes)])]])
