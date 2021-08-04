(ns ring.jmx.type)

(defn type-str [^String type]
  (cond
    (.startsWith type "[L")
    (let [type1 (.substring type 2 (dec (.length type)))]
      (str (type-str type1) "[]")
      )

    :else
    (-> type
        (.replace "javax.management.openmbean." "")
        (.replace "java.lang." "")
        (.replace "javax.management." ""))))

(defmulti form-input :type)

(doseq [type ["long" "java.lang.String" "float" "double" "int" "byte" "short" "char" "boolean"]]
  (defmethod form-input type [{:keys [name defaultValue]}]
    [:input {:type "text" :name name :value (str defaultValue)}]))



(defmethod form-input :default [_]
  [:pre "Cannot input."])

; (defn form-input [{:keys [type name]}])
