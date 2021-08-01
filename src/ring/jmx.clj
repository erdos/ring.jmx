(ns ring.jmx
  (:require [ring.jmx.view :as view]))


(defn- handles? [options request]
  (and (.startsWith (str (:uri request)) (str (:prefix options)))
       ((:guard options) request)))

(defn- jmx-url [options]
  (when-let [options (:jmx-url options)]
    (format "service:jmx:%s://%s:%s/%s"
            (:protocol options "rmi://jndi/rmi")
            (:host options "localhost")
            (:port options "3000")
            (:jndi-path options "jmxrmi"))))

(defn- url-encode [s] (java.net.URLEncoder/encode s "UTF-8"))
(defn- url-decode [s] (java.net.URLDecoder/decode s "UTF-8"))

(defn get-connector [options]
  (if-let [url (jmx-url options)]
    (javax.management.remote.JMXConnectorFactory/connect
     (javax.management.remote.JMXServiceURL. url)
     {})
    (java.lang.management.ManagementFactory/getPlatformMBeanServer)))

(defn- val->str [val]
  (str val)
  (cond (nil? val)              ""
        (-> val class .isArray) (val->str (vec val))
        (instance? javax.management.openmbean.CompositeData val)
        (recur (into {} (for [k (.keySet (.getCompositeType val))]
                          [k (.get val k)])))
        (map? val)      (clojure.string/join "\n" (for [[k v] val] (str k ": " v)))
        :else                   (str val)))

(defn request->model [options request]
  (let [conn      (get-connector options)
        all-names (map (fn [x] (assoc (bean x) :object x))
                       (.queryNames conn (javax.management.ObjectName. "*:*") nil))
        [active-domain active-name] ((juxt :active-domain :active-name) request)
        active-name (first (for [name all-names
                                 :when (= (:domain name) active-domain)
                                 :when (= (:canonicalKeyPropertyListString name) active-name)] name))
        mbean-info (some->> active-name :object (.getMBeanInfo conn))]
    {:all-names   all-names
     :all-domains (doall
                   (for [domain (sort (set (keep :domain all-names)))
                         :let [uri (str (:prefix options) domain "/")]]
                     {:domain   domain
                      :selected (= domain active-domain)
                      :uri      uri}))
     :active-domain active-domain
     :active-name   active-name
     :mbean-info    mbean-info
     :attributes    (doall
                     (for [a (some-> mbean-info .getAttributes)]
                       (-> (bean a)
                           (update :descriptor bean)
                           (assoc :object a)
                           (as-> m
                               (try
                                 (if (.isReadable a)
                                   (assoc m :value
                                          (val->str (.getAttribute conn (:object active-name) (.getName a))))
                                   m)
                                 (catch RuntimeException e
                                   (println :! e)
                                        (assoc m :exception e))
                                 (catch UnsupportedOperationException e
                                   (assoc m
                                          :exception e
                                          :supported false))))
                           )))
     :domain-names  (doall
                     (for [name all-names
                           :when (= active-domain (:domain name))
                           :let [uri (str (:prefix options)
                                          (url-encode active-domain)
                                          "/" (url-encode (:canonicalKeyPropertyListString name)))]]
                       (assoc name
                              :uri uri
                              :selected (= active-name name))))
     }))

(defn- request-enrich [options request]
  (let [[active-domain
         active-name]
        (map url-decode (.split (.substring (:uri request) (count (:prefix options))) "/"))]
    (assoc request
           :active-name active-name
           :active-domain active-domain)))

(defn- handle-jmx [options request]
  (let [request (request-enrich options request)
        model   (request->model options request)]
    {:body    (view/hiccup-str (view/page model))
     :headers {"content-type" "text/html"}
     :status  200}))

(def default-options
  {;; uri prefix for jmx ui page
   :prefix "/jmx/"
   ;; an extra predicate to check if the request can be processed. you can add access control here.
   :guard  (fn [request] true)
   })

(defn wrap-jmx
  "Adds a page to the handler for displaying information on JMX endpoints."
  ([handler]
   (wrap-jmx handler {}))
  ([handler options]
   (let [options (merge default-options options)]
     (fn
       ([request]
        (if (handles? options request)
          (handle-jmx options request)
          (handler request)))
       ([request respond raise]
        (if (handles? options request)
          (try (respond (handle-jmx options request))
               (catch Exception e (raise e)))
          (handler respond raise)))))))
