(ns concept20.core
  (:require [clojure.spec :as s])
  (:use [slingshot.slingshot :only [throw+]]))

(defn saas? [ctx]
  (get-in (first ctx) [:sec/claims ::saas]))

(def identities {"michael.stephan@sap.com" {:sec/id "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
                                            :sec/password "123"
                                            :sec/tenant ::sap
                                            :sec/roles #{::product-manager}}})
(def measurement-sink (atom []))
(def config (atom {}))

(defn subscription [ctx]
  (get-in (last ctx) [:sec/claims ::subscription]))

(defn measure [ctx key value]
  (swap! measurement-sink conj [(subscription ctx) key value]))

(defn claim [ctx key value]
  (assoc-in ctx [:sec/claims key] value))

(defn token [{:keys [:sec/username :sec/password] :as credentials} roles]
  {:sec/claims (let [{stored-password :sec/password :as stored-credentials} (get identities username)]
                 (if (= stored-password password)
                   (-> stored-credentials
                       (dissoc :sec/password)
                       (assoc :sec/principal username)
                       (assoc :sec/provider "https://sap-identity-provider"))
                   (throw+ {:type ::not-authenticated :hint "invalid password"})))
   :sec/signature ""})

(defn tenant [ctx]
  (some (fn [ctx]
          (get-in ctx [:sec/claims :sec/tenant]))
        (reverse ctx)))

(defn roles [ctx]
  ; does it really make sense to combine all roles? do we grant too much privileges to the calller?
  (apply clojure.set/union (map (fn [ctx]
                                  (get-in ctx [:sec/claims :sec/roles]))
                                ctx)))

(defn call [{:keys [::service] :as service-description} function ctx & args]
  (apply service function ctx args))

(defn lookup [ctx service]
  (if-let [{:keys [::subscription] :as service-description} (let [rules (get-in @config [(tenant ctx) ::mapping service])]
                                                              (some (fn [rule]
                                                                      (rule ctx)) rules))]
    (fn [function ctx & args]
      (apply call service-description function (conj ctx {:sec/claims {::subscription subscription
                                                                       :sec/signature ""}}) args))
    (throw+ {:type ::service-not-found :hint service})))

(defn authorized? [service function ctx requested-scope]
  (= requested-scope (clojure.set/intersection
                      (apply clojure.set/union (for [role (roles ctx)]
                                                 (get-in @config [(tenant ctx) ::authorization role] #{})))
                      requested-scope)))

(defn product-service [function ctx & args]
  (measure ctx ::api-call 1)
  (condp = function
    :hybris/product-service-get (if (authorized? :hybris/product-service function ctx #{::product-read})
                                  (let [[id] args]
                                    [{:id id
                                      :name "banana"}])
                                  {:type ::not-authorized :hint (str :hybris/product-service "/" function)})
    (throw+ {:type ::function-not-found :hint function})))

(defn saas-ui-browser [credentials]
  (let [ctx [(-> (token credentials #{::product-manager})
                 (claim ::saas true))]
        product-service (lookup ctx :hybris/product-service)]
    (product-service :hybris/product-service-get ctx :sku123)))

(reset! config {::sap {::authorization {::product-manager #{::product-read}}
                       ::mapping {:hybris/product-service [(fn [ctx]
                                                             (when (saas? ctx)
                                                               {::service product-service
                                                                ::subscription ::some-saas-subscription}))
                                                           (fn [ctx]
                                                             {::service product-service
                                                              ::subscription ::some-low-touch-subscription})]}}})

(comment
  (saas-ui-browser {}))

(comment
  (authorized? :hybris/product-service :hybris/product-service-get {:sec/claims {:sec/tenant ::sap :sec/roles #{::product-manager ::sales-representative}}} #{::product-read}))
