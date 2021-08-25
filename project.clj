(defproject dev.erdos/ring-jmx "0.1.0-SNAPSHOT"
  :description "Ring middleware for a JMX UI"
  :url "https://github.com/erdos/ring-jmx"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :profiles {:test {:plugins [[lein-ring "0.12.5"]]
                    :dependencies [[org.clojure/java.jmx "1.0.0"]]
                    :ring {:handler ring.jmx-test/app}}}
  :repl-options {:init-ns ring.jmx})
