(ns ring.jmx-test
  (:require [clojure.test :refer :all]
            [ring.jmx :refer :all]))

(defn default-handler [request]
  {:status 200
   :body "asdf"
   :headers {"content-type" "text/plain"}}
  )


(def -app (wrap-jmx default-handler))

(def app #'-app)
