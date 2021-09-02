(ns ring.jmx
  (:require [clojure.string]
            [clojure.set :refer [rename-keys]]
            [ring.middleware.params :as ring-params]
            [ring.jmx.model :as model]
            [ring.jmx.type :as type]
            [ring.jmx.view :as view]))

(set! *warn-on-reflection* true)

(defn- handles? [options request]
  (and (.startsWith (str (:uri request)) (str (:prefix options)))
       ((:guard options) request)))

(defn- url-encode [s] (java.net.URLEncoder/encode s "UTF-8"))
(defn- url-decode [s] (java.net.URLDecoder/decode s "UTF-8"))

(defn normal-keys [m]
  (rename-keys m {:canonicalKeyPropertyListString :canonical-key-property-list-string
                  :canonicalName :canonical-name}))

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

(defn- get-all-names [conn]
  (for [x (.queryNames conn (javax.management.ObjectName. "*:*") nil)]
    (-> (bean x) (normal-keys) (assoc :object x))))

(defn request->model [options request]
  (let [conn      (model/get-connector options)
        all-names (get-all-names conn)
        [active-domain active-name] ((juxt :active-domain :active-name) request)
        active-name (first (for [name all-names
                                 :when (= (:domain name) active-domain)
                                 :when (= (:canonical-key-property-list-string name) active-name)] name))

        mbean-info (some->> active-name :object (.getMBeanInfo conn))
        operations (doall (for [m (some-> mbean-info .getOperations)]
                            (-> (bean m)
                                (assoc :object m)
                                (update :descriptor bean)
                                (update :signature (partial mapv bean))
                                (assoc  :active? (= (.getName m) (get-in request [:query-params "action"]))))))]
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
     :operations    (for [op operations]
                      (if (:active? op)
                        (merge op (invoke-model conn op active-name (:form-params request)))
                        op))
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
                                     (assoc m :exception e))
                                   (catch UnsupportedOperationException e
                                     (assoc m
                                            :exception e
                                            :supported false)))))))
     :domain-names  (sort-by
                     :canonical-name
                     (for [name all-names
                           :when (= active-domain (:domain name))
                           :let [uri (str (:prefix options)
                                          (url-encode active-domain)
                                          "/" (url-encode (:canonical-key-property-list-string name)))]]
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


(defn- handle-jmx [options request]
  (assert (map? request))
  (let [request (ring-params/assoc-form-params request "UTF-8")
        request (ring-params/assoc-query-params request "UTF-8")
        request (request-enrich options request)
        model   (request->model options request)]
    {:body    (view/hiccup-str (view/page model))
     :headers {"content-type" "text/html"}
     :status  200}))


(def default-options
  {;; uri prefix for jmx ui page
   :prefix "/jmx/"
   ;; an extra predicate to check if the request can be processed. you can add access control here.
   :guard  (constantly true)})

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
