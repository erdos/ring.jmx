(ns ring.jmx-test
  (:require [clojure.test :refer :all]
            [ring.jmx :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))


(defn default-handler [request]
  {:status 200
   :body "asdf"
   :headers {"content-type" "text/plain"}}
  )


(def -app (wrap-jmx default-handler))

(def app #'-app)
