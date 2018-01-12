(ns virtual-gq.templates
  (:require [hiccup.page :refer [html5]]))

(defn post-list-partial [post-list]
  [:ul.post-list
   (map (fn [{:keys [title url published]}]
          [:li [:a {:href url} title] " - " published])
        post-list)])

(defn base-template
  [{:keys [title post-list] :as context} body]
  (html5
    [:head
     [:meta {:charser "utf-8"}]
     [:meta {:name "viewport"
             :content "width=device-width, initial-scale=1.0"}]
     [:meta {:name "description"
             :content title}]
     [:title "Blog and Notes of Giedrius Kudelis - " title]
     [:link {:rel "stylesheet"
             :href "https://fonts.googleapis.com/css?family=Josefin+Sans|Josefin+Slab:700"}]
     [:link {:rel "stylesheet"
             :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
             :integrity "sha256-MfvZlkHCEqatNoGiOXveE8FIwMzZg4W85qfrfIFBfYc= sha512-dTfge/zgoMYpP7QbHy4gWMEGsbsdZeCXz7irItjcC3sPUFtf0kuFbDz/ixG7ArTxmDjLXDmezHubeNikyKGVyQ=="
             :crossorigin "anonymous"}]
     [:link {:rel "stylesheet"
             :href "/css/default.css"}]]
    [:body
     [:div.container
      [:div.row
       [:div.col-sm-3.col-sm-push-7.col-sm-offset-1
        [:a {:href "/"} [:h1 "virtual.gq"]]
        [:h3 "Posts"]
        (post-list-partial post-list)]
       [:div.col-sm-7.col-sm-pull-3 body]]]]))

(defn page-template
  [{body :body context :context}]
  (base-template context body))

(defn post-template
  [{body :body
    {:keys [title tags published] :as context} :context}]
  (let [post-body [:div.post-body
                   [:h2 title]
                   [:p.info
                    "First published: " published [:br]
                    "Tags: " tags]
                   body]]
    (base-template context post-body)))
