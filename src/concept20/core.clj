(ns concept20.core
  (:require [clojure.spec :as s])
  (:use [slingshot.slingshot :only [throw+]]))

(defn saas? [ctx]
  (get-in (first ctx) [:sec/claims :yaas/saas]))

(defn subscription [ctx]
  (get-in (last ctx) [:sec/claims :yaas/subscription]))

(defn measure [yaas ctx key value]
  (swap! yaas update-in [:yaas/measurements] conj [(subscription ctx) key value]))

(defn claim [ctx key value]
  (assoc-in ctx [:sec/claims key] value))

(defn token [yaas {:keys [:sec/username :sec/password] :as credentials} roles]
  {:sec/claims (let [{stored-password :sec/password :as stored-credentials} (get-in @yaas [:yaas/authentication-db username])]
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

(defn call [yaas {:keys [:yaas/service] :as service-description} function ctx & args]
  (apply service yaas function ctx args))

(defn lookup [yaas ctx service]
  (if-let [{:keys [:yaas/subscription] :as service-description} (let [rules (get-in @yaas [:yaas/config-db (tenant ctx) :config/mapping service])]
                                                                  (some (fn [rule]
                                                                          (rule ctx)) rules))]
    (fn [function ctx & args]
      (apply call yaas service-description function (conj ctx {:sec/claims {:yaas/subscription subscription
                                                                            :sec/signature ""}}) args))
    (throw+ {:type ::service-not-found :hint service})))

(defn authorized? [yaas service function ctx requested-scope]
  (= requested-scope (clojure.set/intersection
                      (apply clojure.set/union (for [role (roles ctx)]
                                                 (get-in @yaas [:yaas/config-db (tenant ctx) :config/authorization role] #{})))
                      requested-scope)))

(defn product-service [yaas function ctx & args]
  (measure yaas ctx ::api-call 1)
  (condp = function
    :hybris/product-service-get (if (authorized? yaas :hybris/product-service function ctx #{:scope/product-read})
                                  (let [[id] args]
                                    [{:id id
                                      :name "banana"}])
                                  {:type ::not-authorized :hint (str :hybris/product-service "/" function)})
    (throw+ {:type ::function-not-found :hint function})))

(defn saas-ui-browser [yaas credentials]
  (let [ctx [(-> (token yaas credentials #{:role/product-manager})
                 (claim :yaas/saas true))]
        product-service (lookup yaas ctx :hybris/product-service)]
    (product-service :hybris/product-service-get ctx :sku123)))
