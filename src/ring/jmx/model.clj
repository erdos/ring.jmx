(ns ring.jmx.model)

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
