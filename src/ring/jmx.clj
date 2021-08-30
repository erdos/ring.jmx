(ns ring.jmx
  (:require [clojure.string]
            [ring.middleware.params :as ring-params]
            [ring.jmx.type :as type]
            [ring.jmx.view :as view]))


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

(defn- invoke-model [conn op active-name form-params]
  (assert conn)
  (assert (:name op))
  (assert (:object active-name))
  (let [param-types (into-array String (map :type (:signature op)))
        param-values (->> (map (comp form-params :name) (:signature op))
                          (map (fn [type value] (type/parse-value {:type type :value value})) param-types)
                          (into-array Object))]
    (try {:call-result (.invoke conn (:object active-name) (:name op) param-values param-types)}
         (catch Exception e {:call-error e}))))

(defn request->model [options request]
  (let [conn      (get-connector options)
        all-names (map (fn [x] (assoc (bean x) :object x))
                       (.queryNames conn (javax.management.ObjectName. "*:*") nil))
        [active-domain active-name] ((juxt :active-domain :active-name) request)
        active-name (first (for [name all-names
                                 :when (= (:domain name) active-domain)
                                 :when (= (:canonicalKeyPropertyListString name) active-name)] name))

        mbean-info (some->> active-name :object (.getMBeanInfo conn))
        operations (doall (for [m (some-> mbean-info .getOperations)]
                            (-> (bean m)
                                (assoc :object m)
                                (update :descriptor bean)
                                (update :signature (partial mapv bean))
                                (assoc  :active? (= (.getName m) (get-in request [:query-params "action"]))))))
        active-op (first (for [op operations :when (:active? op)]
                           (merge op (invoke-model conn op active-name (:form-params request)))))]
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
     :operations    (for [op operations] (if (:active? op) active-op op))
     :active-operation active-op
     :attributes    (doall
                     (for [a (some-> mbean-info .getAttributes)]
                       (-> (bean a)
                           (update :descriptor bean)
                           (assoc :object a)
                           (as-> m
                                 (try
                                   (if (.isReadable a)
                                     (let [value (.getAttribute conn (:object active-name) (.getName a))]
                                       (assoc m :value value))
                                     m)
                                   (catch RuntimeException e
                                     (println :! e)
                                     (assoc m :exception e))
                                   (catch UnsupportedOperationException e
                                     (assoc m
                                            :exception e
                                            :supported false)))))))
     :domain-names  (sort-by
                     :canonicalName
                     (for [name all-names
                           :when (= active-domain (:domain name))
                           :let [uri (str (:prefix options)
                                          (url-encode active-domain)
                                          "/" (url-encode (:canonicalKeyPropertyListString name)))]]
                       (assoc name
                              :uri uri
                              :selected (= active-name name))))}))

(defn- request-enrich [options request]
  (let [[active-domain
         active-name]
        (map url-decode (.split (.substring (:uri request) (count (:prefix options))) "/"))]
    (assoc request
           :active-name (not-empty active-name)
           :active-domain (not-empty active-domain))))


(defn evaled-model [model request]
  (println :? (pr-str request))
  (if (= :post (:request-method request))
    (do (println "Executed!")
        (println (:active-name model))
        ;; 1. deserialize parameters as objects of correct type
        ;; 1.B. if cannot parse -> return parse errors.
        ;; 2. call expression
        ;; 3. put result into model
        ;; 4. write ui to show evaluation result.
        model)
    model))


(defn- handle-jmx [options request]
  (assert (map? request))
  (let [request (ring-params/assoc-form-params request "UTF-8")
        request (ring-params/assoc-query-params request "UTF-8")
        request (request-enrich options request)
        _       (assert (map? request))
        model   (-> (request->model options request) (evaled-model request))]
    {:body    (view/hiccup-str (view/page model))
     :headers {"content-type" "text/html"}
     :status  200}))


(def default-options
  {;; uri prefix for jmx ui page
   :prefix "/jmx/"
   ;; an extra predicate to check if the request can be processed. you can add access control here.
   :guard  (constantly true)
   })

(defn- wrap-async-handler [options handles? handler]
  (fn
    ([request]
     (if (handles? options request)
       (handle-jmx options request)
       (handler request)))
    ([request respond raise]
     (if (handles? options request)
       (try (respond (handle-jmx options request))
            (catch Exception e (raise e)))
       (handler respond raise)))))

(defn wrap-jmx
  "Adds a page to the handler for displaying information on JMX endpoints."
  ([handler]
   (wrap-jmx handler {}))
  ([handler options]
   (let [options (merge default-options options)]
     (assert (.startsWith (str (:prefix options)) "/"))
     (assert (fn? (:guard options)))
     (wrap-async-handler options handles? handler))))
