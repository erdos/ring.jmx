(ns ring.jmx
  (:require [clojure.string]
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

(defn- make-uri
  ([options domain]
   (assert (:prefix options))
   (assert (string? domain))
   (str (:prefix options) (url-encode domain) "/"))
  ([options active-domain name]
   (assert (:prefix options))
   (assert (string? active-domain))
   (assert (map? name))
   (str (:prefix options) (url-encode active-domain) "/" (url-encode (:canonical-key-property-list-string name)))))

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

(defn- assoc-all-names [connection request model]
  (let [all-names (model/get-all-names connection)
        [active-domain active-name] ((juxt :active-domain :active-name) request)
        active-name (first (for [name all-names
                                 :when (= (:domain name) active-domain)
                                 :when (= (:canonical-key-property-list-string name) active-name)] name))

        mbean-info (some->> active-name :object (.getMBeanInfo connection))]
    (assoc model
           :all-names all-names
           :active-name active-name
           :mbean-info mbean-info)))

(defn- assoc-all-domains [options request model]
  (assert (:all-names model))
  (let [all-names (:all-names model)
        [active-domain active-name] ((juxt :active-domain :active-name) request)]
    (assert all-names)
    (assoc model
           :active-domain active-domain
           :all-domains (doall
                         (for [domain (into (sorted-set) (keep :domain) all-names)]
                           {:domain   domain
                            :selected (= domain active-domain)
                            :uri      (make-uri options domain)}))
           :domain-names (sort-by
                          :canonical-name
                          (for [name all-names
                                :when (= active-domain (:domain name))]
                            (assoc name
                                   :uri (make-uri options active-domain name)
                                   :selected (= active-name name)))))))

(defn- assoc-operations [conn request model]
  (assoc model
         :operations
         (for [op (some-> model :mbean-info .getOperations)
               :let [active? (= (.getName op) (get-in request [:query-params "action"]))
                     bab (bean op)]]
           (-> bab
               (assoc :object op)
               (update :descriptor bean)
               (update :signature (partial mapv bean))
               (assoc  :active? active?)
               (cond-> active? (merge (invoke-model conn bab (:active-name model) (:form-params request))))))))

(defn- assoc-attributes [conn model]
  (let [active-name (:active-name model)]
    (assoc model
           :attributes
           (doall
            (for [a (some-> model :mbean-info .getAttributes)]
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
                                   :supported false))))))))))

(defn request->model [options request]
  (let [connection (model/get-connector options)]
    (->> {}
         (assoc-all-names connection request)
         (assoc-all-domains options request)
         (assoc-operations connection request)
         (assoc-attributes connection))))

(defn- request-enrich [options request]
  (let [[active-domain
         active-name]
        (map url-decode (.split (.substring (str (:uri request)) (count (:prefix options))) "/"))]
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
