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
