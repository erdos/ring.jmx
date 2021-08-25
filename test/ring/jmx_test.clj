(ns ring.jmx-test
  (:require [clojure.test :refer :all]
            [ring.jmx :refer :all]
            [clojure.java.jmx :as jmx]))

(defonce register-once
  (jmx/register-mbean
   (jmx/create-bean (ref {:string-attribute "a-string"}))
   "my.namespace:name=Value"))

(defn default-handler [request]
  {:status 200
   :body "asdf"
   :headers {"content-type" "text/plain"}}
  )


(def -app (wrap-jmx default-handler))

(def app #'-app)
