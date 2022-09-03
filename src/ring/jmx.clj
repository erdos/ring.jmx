(ns ring.jmx
  (:require [clojure.string]
            [ring.middleware.params :as ring-params]
            [ring.middleware.multipart-params :as multipart-params]
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
  ; (.println System/out (pr-str :!!! (vec (:signature op))))
  (let [param-types (into-array String (map :type (:signature op)))
        param-values (->> (map (comp form-params :name) (:signature op))
                          (map (fn [type value]
                                 (.println System/out (str :> (pr-str type) (pr-str value)))
                                 (type/parse-value {:type type :value value}))
                               param-types)
                          (into-array Object))]
    (try {:call-result (.invoke conn (:object active-name) (:name op) param-values param-types)}
         (catch Exception e {:call-error e}))))

(defn- assoc-all-names [connection model]
  (let [all-names (model/get-all-names connection)
        active-name (first (for [name all-names
                                 :when (= (:domain name) (:selected-domain model))
                                 :when (= (:canonical-key-property-list-string name) (:selected-name model))] name))
        mbean-info (some->> active-name :object (.getMBeanInfo connection))]
    (assoc model
           :all-names all-names
           :active-name active-name
           :mbean-info mbean-info)))

(defn- assoc-all-domains [options model]
  (assert (:all-names model))
  (let [all-names (:all-names model)]
    (assert all-names)
    (assoc model
           :all-domains (doall
                         (for [domain (into (sorted-set) (keep :domain) all-names)]
                           {:domain   domain
                            :selected (= domain (:selected-domain model))
                            :uri      (make-uri options domain)}))
           :domain-names (sort-by
                          :canonical-name
                          (for [name all-names
                                :when (= (:selected-domain model) (:domain name))]
                            (assoc name
                                   :uri (make-uri options (:selected-domain model) name)
                                   :selected (= (:selected-name model) (:canonical-key-property-list-string name))))))))

(defn- assoc-operations [conn request model]
  (assoc model
         :operations
         (for [op (some-> model :mbean-info .getOperations)
               :let [active? (= (.getName op) (get-in request [:params "action"]))
                     bab (update (bean op) :signature (partial mapv bean))]]
           (-> bab
               (assoc  :object op)
               (update :descriptor bean)
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

(defn- request->selected [options request]
  (let [[active-domain active-name]
        (map url-decode (.split (.substring (str (:uri request)) (count (:prefix options))) "/"))]
    {:selected-name active-name
     :selected-domain active-domain}))

(defn request->model [options request]
  (let [connection (model/get-connector options)]
    (->> (request->selected options request)
         (assoc-all-names connection)
         (assoc-all-domains options)
         (assoc-operations connection request)
         (assoc-attributes connection))))


;; find action by name
(defn handle-jmx-invoke [options request]
  (println :>>>> (pr-str request))
  (let [request (multipart-params/multipart-params-request request "UTF-8")
        request (ring-params/assoc-query-params request "UTF-8")
        {:keys [selected-domain selected-name]} (request->selected options request)
        conn        (model/get-connector options)
        action-name (get-in request [:params "action"])
        model       (model/get-selected-name conn selected-domain selected-name)
        object-name (javax.management.ObjectName. (str selected-domain ":" selected-name))
        mbean-info  (.getMBeanInfo conn object-name)
        operation   (some #(when (= (.getName %) action-name) %) (.getOperations mbean-info))
        _ (assert operation (str "No operation with " (:params request)))
        params (for [sig (.getSignature operation)
                     :let [typ  (.getType sig)
                           nam  (.getName sig)
                           raw  (get-in request [:params nam])]]
                 (try {:type typ :raw raw :name nam :value (type/parse-value {:type typ :value raw})}
                      (catch Exception e {:type typ :raw raw :name nam :error e})))
        executed (when (not-any? :error params)
                   (try {:call-result (.invoke conn object-name
                                               (.getName operation)
                                               (into-array Object (map :value params))
                                               (into-array String (map :type params)))}
                        (catch Exception e {:call-error e})))]
    {:status (if (contains? executed :call-result) 201 500)
     :headers {"content-type" "text/html"}
     :body (view/hiccup-str (view/operation-block operation params executed))}))

(defn- handle-jmx [options request]
  (assert (map? request))
  (if (= :post (:request-method request))
     (handle-jmx-invoke options request) 
  
  (let [request (ring-params/assoc-query-params request "UTF-8")
        model   (request->model options request)]
    {:body    (view/hiccup-str (view/page model))
     :headers {"content-type" "text/html"}
     :status  200}))
)

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
