(ns ring.jmx.hiccup
  "Minimalist and self-contained Hiccup implementation"
  (:require [clojure.string]))

(set! *warn-on-reflection* true)

(defn- hiccup-visitor [tree]
  (cond
    (string? tree) (println tree)
    (number? tree) (println tree)
    (nil? tree)    nil
    (seq? tree)    (run! hiccup-visitor tree)
    (vector? tree)
    (let [tag (first tree)
          opts (if (map? (second tree)) (second tree) {})
          opts-str  (some->> (for [[k v] opts]
                               (if (#{:selected :readonly :checked :disabled} k)
                                 (when v (name k))
                                 (str (name k) "=" \" v \")))
                             (not-empty)
                             (clojure.string/join " ")
                             (str " "))
          body (if (map? (second tree)) (nnext tree) (next tree))]
      (assert (keyword? tag))
      (cond
        (not-empty body)
        (do (print (str "<" (name tag) opts-str ">"))
            (run! hiccup-visitor body)
            (print (str "</" (name tag) ">")))
        (= :br tag) (print "<br>")
        :else       (print (str "<" (name tag) opts-str "/>"))))))

(defn hiccup-str [tree]
  (with-out-str
    (println "<!doctype html>")
    (println "<meta charset=\"UTF-8\">")
    (hiccup-visitor tree)))