(ns ring.jmx.type
  (:require [clojure.string]))

(set! *warn-on-reflection* true)

(defn type-str [^String type]
  (cond
    (.startsWith type "[L")
    (let [type1 (.substring type 2 (dec (.length type)))]
      (str (type-str type1) "[]")
      )

    ;; https://docs.oracle.com/javase/7/docs/api/java/lang/Class.html#getName()
    (.startsWith type "[")
    (case type
      "[Z" "boolean[]"
      "[B" "byte[]"
      "[C" "char[]"
      "[D" "double[]"
      "[F" "float[]"
      "[I" "int[]"
      "[J" "long[]"
      "[S" "short[]")

    :else
    (-> type
        (.replace "javax.management.openmbean." "")
        (.replace "java.lang." "")
        (.replace "javax.management." ""))))

;; renders a html form for the given type
(defmulti form-input :type)

(doseq [type ["long" "java.lang.String" "float" "double" "int" "byte" "short" "char" "boolean"]]
  (defmethod form-input type [{:keys [name defaultValue]}]
    [:input {:type "text" :name name :value (str defaultValue)}]))

(defmethod form-input :default [_]
  [:pre "Cannot input."])

;; (defmethod form-input "TabularData" [{:keys [name]}])

(defmulti render-value (fn [x] (when (some? (:value x)) (:type x))))

(defmethod render-value nil [_] [:pre "nil"])

(defmethod render-value :default [{:keys [type value]}]
  [:pre (str value)])

(doseq [type ["long" "int" "float" "double" "byte" "short"]]
  (defmethod render-value type [{:keys [value]}]
    [:pre [:i value]]))

(defmethod render-value "boolean" [{:keys [value writable]}]
  (assert (boolean? writable))
  [:input {:type "checkbox" :disabled (not writable) :checked value}])

(defmethod render-value "javax.management.openmbean.CompositeDataSupport"
  [{:keys [value]}]
  ; [:b "!!!" (pr-str value)]
  [:ol
   (for [v (.values value)]
     [:li (render-value {:value v :type (.getName (class v))})])])

(defmethod render-value "javax.management.openmbean.CompositeData" [{:keys [value]}]
  [:table
   (doall
    (for [k (-> value .getCompositeType .keySet)
          :let [v (.get value k)
                t (-> value .getCompositeType (.getType k) (.getClassName))]]
      [:tr
       [:td (str k)]
       [:td [:code (type-str t)]]
       [:td (render-value {:type (type v) :value v})]]
      ))])

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
         (render-value {:value x :type c}))]])])

(defmethod render-value "javax.management.openmbean.TabularDataSupport"
  [{:keys [value]}]
  [:pre (pr-str (bean value))])

(defmethod render-value "[Ljava.lang.String;"
  [{:keys [value]}]
  [:pre (pr-str (vec value))])

(defmethod render-value "[J" [{:keys [value]}] ;; long[]
  [:pre (pr-str (vec value))])


(defmulti parse-value :type)

(defmethod parse-value "boolean" [{:keys [value]}]
  (case value
    ("true" "True" "TRUE") true
    ("false" "False" "FALSE") false))

(defmethod parse-value "long" [{:keys [value]}]
  (Long/parseLong ^String value))

(defmethod parse-value "int" [{:keys [value]}]
  (Integer/parseInt ^String value))

(defmethod parse-value "java.lang.String" [{:keys [value]}] value)
(defmethod parse-value "java.lang.CharSequence" [{:keys [value]}] value)

;; TODO: other mappings as well
;; TODO: enums should use Enum.valueOf for quick lookup



(defn val->str [val]
  (str val)
  (cond (nil? val)              ""
        (-> val class .isArray) (val->str (vec val))
        (instance? javax.management.openmbean.CompositeData val)
        (recur (into {} (for [k (.keySet (.getCompositeType val))]
                          [k (.get val k)])))
        (map? val)      (clojure.string/join "\n" (for [[k v] val] (str k ": " v)))
        :else                   (str val)))
