(defproject virtual-gq "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [ring  "1.6.1"]
                 [stasis "2.3.0"]
                 [optimus "0.19.3"]
                 [markdown-clj "0.9.99"]]
  :plugins  [[lein-ring  "0.12.0"]]
  :ring  {:handler virtual-gq.core/app}
  :aliases {"build-site" ["run" "-m" "virtual-gq.core/export"]})
