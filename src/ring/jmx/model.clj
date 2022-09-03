(ns ring.jmx.model
  (:require [clojure.set :refer [rename-keys]]))

(defn jmx-url [options]
  (when-let [options (:jmx-url options)]
    (format "service:jmx:%s://%s:%s/%s"
            (:protocol options "rmi://jndi/rmi")
            (:host options "localhost")
            (:port options "3000")
            (:jndi-path options "jmxrmi"))))

(defn get-connector [options]
  (if-let [url (jmx-url options)]
    (javax.management.remote.JMXConnectorFactory/connect
     (javax.management.remote.JMXServiceURL. url)
     {})
    (java.lang.management.ManagementFactory/getPlatformMBeanServer)))

(defn normal-keys [m]
  (rename-keys m {:canonicalKeyPropertyListString :canonical-key-property-list-string
                  :canonicalName :canonical-name}))

(defn get-all-names [conn]
  (for [x (.queryNames conn (javax.management.ObjectName. "*:*") nil)]
    (-> (bean x) (normal-keys) (assoc :object x))))

(defn get-selected-name [conn domain name]
  (when-let [x (first (.queryNames conn (javax.management.ObjectName. (str domain ":" name)) nil))]
    (-> x (bean) (normal-keys) (assoc :object x))))
