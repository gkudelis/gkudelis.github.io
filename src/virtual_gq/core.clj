(ns virtual-gq.core
  (:require [clojure.string :as s]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [stasis.core :as stasis]
            [markdown.core :refer [md-to-html-string-with-meta]]
            [virtual-gq.conf :as conf]
            [virtual-gq.templates :as t]))

; generic flow stuff

(defn id-print [x] (println x) x)

(defn recomp [& fs] (apply comp (reverse fs)))

(defn map-v [f m]
  (let [tm (transient {})]
    (persistent! (reduce-kv (fn [acc k v] (assoc! acc k (f v))) tm m))))

(defn map-k [f m]
  (let [tm (transient {})]
    (persistent! (reduce-kv (fn [acc k v] (assoc! acc (f k) v)) tm m))))

(defn rename-key [k1 k2 m] (map-k #(if (= % k1) k2 %) m))

; blog-specific stuff

(def map-path map-k)
(def map-content map-v)

(defn html-extension [path] (s/replace path #"\.md$" ".html"))

(defn add-passed-context [m ctx] (assoc m :context ctx))
(defn merge-to-context [f m]
  (assoc m :context (merge (:context m) (f m))))
(defn join-meta-strings [m]
  (assoc m :metadata (map-v (partial s/join " ") (:metadata m))))

(def init-with constantly)
(defn function-monad-bind [f g] (fn [r] (g (f r) r)))
(defn function-monad [& fs] (reduce function-monad-bind (init-with nil) fs))
(defn no-r [f] (fn [x _] (f x)))

(defn convert-date [in-date]
  (let [in-format (java.text.SimpleDateFormat. "yyyy-MM-dd")
        out-format (java.text.SimpleDateFormat. "d MMM, yyyy")]
    (->> in-date
        (.parse in-format)
        (.format out-format))))
(defn convert-date-in [path ctx]
  (if (get-in ctx path)
    (update-in ctx path convert-date)
    ctx))

; figure out how to drop the last argument cleanly - why can't I ignore the
; argument that's passed in at the end, why is this an issue?
(defn pages []
  (->> (stasis/slurp-directory "pages" #"\.md$")
       (map-path html-extension)
       (map-content #(function-monad
                       (init-with %)
                       (no-r md-to-html-string-with-meta)
                       (no-r join-meta-strings)
                       add-passed-context
                       (no-r (partial merge-to-context :metadata))
                       (no-r (partial convert-date-in [:context :published]))
                       (no-r (partial rename-key :html :body))
                       (no-r t/page-template)))))

(defn posts []
  (->> (stasis/slurp-directory "posts" #"\.md$")
       (map-path html-extension)
       (map-content #(function-monad
                       (init-with %)
                       (no-r md-to-html-string-with-meta)
                       (no-r join-meta-strings)
                       add-passed-context
                       (no-r (partial merge-to-context :metadata))
                       (no-r (partial convert-date-in [:context :published]))
                       (no-r (partial rename-key :html :body))
                       (no-r t/post-template)))))

(def other {"/css/default.css" (slurp "public/css/default.css")})

(defn output-pages []
  (stasis/merge-page-sources
    {:pages (pages)
     :posts (posts)
     :other other}))

(def post-list
  (->> (stasis/slurp-directory "posts" #"\.md$")
       (map-path html-extension)
       (map-content #(-> %
                         md-to-html-string-with-meta
                         join-meta-strings))
       (reduce-kv (fn [acc k v] (conj acc (assoc (:metadata v) :url k))) [])
       (filter :published)
       (sort-by :published)
       (reverse)
       (map #(update % :published convert-date))))

(def site-context
  {:conf conf/blog-config
   :posts post-list})

(def app (wrap-content-type (stasis/serve-pages output-pages site-context)))

(defn export []
  (stasis/empty-directory! (:target-dir conf/blog-config))
  (stasis/export-pages (output-pages) (:target-dir conf/blog-config)))
