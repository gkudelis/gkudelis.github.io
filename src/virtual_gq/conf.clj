(ns virtual-gq.conf
  (:require [clojure.edn :as edn]))

(defn load-config
  "Given an EDN filename load and return the configuration object"
  [filename]
  (edn/read-string (slurp filename)))

(def blog-config (load-config "blog.conf.edn"))
