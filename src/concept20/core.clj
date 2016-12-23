(ns concept20.core
  (:require [clojure.spec :as s])
  (:use [slingshot.slingshot :only [throw+]]))

;context helper

(defn saas? [ctx]
  (some (fn [ctx]
          (get-in ctx [:sec/claims :yaas/saas])) ctx))

(defn subscription [ctx]
  (get-in (last ctx) [:sec/claims :yaas/subscription]))

(defn measure [yaas ctx key value]
  (swap! yaas update-in [:yaas/measurements] conj [(subscription ctx) key value]))

(defn claim-saas [ctx]
  (conj ctx {:sec/claims {:yaas/saas true}}))

(defn user-id [ctx]
  (some (fn [ctx]
          (get-in ctx [:sec/claims :sec/id]))
        (reverse ctx)))

(defn tenant [ctx]
  (some (fn [ctx]
          (get-in ctx [:sec/claims :sec/tenant]))
        (reverse ctx)))

(defn roles [ctx]
  ; does it really make sense to combine all roles? do we grant too much privileges to the caller?
  (apply clojure.set/union (map (fn [ctx]
                                  (get-in ctx [:sec/claims :sec/roles]))
                                ctx)))

; platform 

(defn authenticate [yaas {:keys [:sec/username :sec/password] :as credentials}]
  [{:sec/claims (let [{stored-password :sec/password :as stored-credentials} (get-in @yaas [:yaas/authentication-db (dissoc credentials :sec/password)])]
                  (if (= stored-password password)
                    (-> stored-credentials
                        (dissoc :sec/password)
                        (assoc :sec/username username)
                        (assoc :sec/provider "https://sap-identity-provider"))
                    (throw+ {:type ::not-authenticated :hint "invalid password"})))
    :sec/signature ""}])

(defn authorize [yaas roles ctx]
  (let [active-roles (clojure.set/intersection roles (get-in @yaas [:yaas/authorization-db (user-id ctx) :sec/roles]))]
    (if (= roles active-roles)
      (conj ctx {:sec/claims {:sec/roles active-roles}})
      (throw+ {:type ::not-authorized :hint "requested roles not assigned"}))))

(defn authorized? [yaas service function ctx requested-scope]
  (= requested-scope (clojure.set/intersection
                      (apply clojure.set/union (for [role (roles ctx)]
                                                 (get-in @yaas [:yaas/config-db (tenant ctx) :config/authorization role] #{})))
                      requested-scope)))

(defn call [yaas {:keys [:yaas/service] :as service-description} function ctx & args]
  (apply service yaas function ctx args))

(defn lookup [yaas ctx service]
  (if-let [{:keys [:yaas/subscription] :as service-description} (let [filters (get-in @yaas [:yaas/config-db (tenant ctx) :config/discovery service])]
                                                                  (some (fn [{:keys [rule]}]
                                                                          (rule ctx)) filters))]
    (fn [function ctx & args]
      (apply call yaas service-description function (conj ctx {:sec/claims {:yaas/subscription subscription
                                                                            :sec/signature ""}}) args))
    (throw+ {:type ::service-not-found :hint service})))

(defn provision [yaas ctx services {:keys [:subscription/id]}]
  (doall (for [[service-type services] services]
           (doall (for [[type] services]
                    (swap! yaas assoc-in [:yaas/subscriptions (tenant ctx) id :subscription/state service-type type] :provision/done))))))

(defn announce [yaas ctx services subscription-id]
  (doall (for [[id uri] (get services :yaas/services)]
           (swap! yaas update-in [:yaas/config-db (tenant ctx) :config/discovery id] conj {:yaas/subscription subscription-id
                                                                                           :rule (fn [ctx]
                                                                                                   {:yaas/service uri
                                                                                                    :yaas/subscription subscription-id})}))))

(defn subscribe [yaas ctx service-bundle-id]
  (let [service :hybris/platform function :hybris/subscription-service-subscribe
        subscription-id (-> (java.util.UUID/randomUUID) str keyword)
        subscription {:service-bundle/id service-bundle-id
                      :subscription/state {}
                      :subscription/created (System/currentTimeMillis)}]
    (if (authorized? yaas service function ctx #{:scope/subscription-manage})
      (if-let [service-bundle (get-in @yaas [:yaas/service-bundles service-bundle-id])]
        (do
          (swap! yaas assoc-in [:yaas/subscriptions (tenant ctx) subscription-id] subscription)
          (provision yaas ctx service-bundle (assoc subscription :subscription/id subscription-id))
          (announce yaas ctx service-bundle subscription-id))
        (throw+ {:type ::not-found :hint (str "service bundle '" service-bundle-id "'not found")}))
      (throw+ {:type ::not-authorized :hint (str service "/" function)}))))

; top level applications

(defn product-service [yaas function ctx & args]
  (measure yaas ctx ::api-call 1)
  (condp = function
    :hybris/product-service-get (if (authorized? yaas :hybris/product-service function ctx #{:scope/product-read})
                                  (let [[id] args]
                                    [{:id id
                                      :name "banana"}])
                                  (throw+ {:type ::not-authorized :hint (str :hybris/product-service "/" function)}))
    (throw+ {:type ::function-not-found :hint function})))

(defn category-service [yaas function ctx & args])

(defn subscription-ui-browser [yaas credentials service-bundles]
  (let [ctx (->> (authenticate yaas credentials)
                 (authorize yaas #{:role/administrator}))]
    (doall (for [service-bundle service-bundles]
             (subscribe yaas ctx service-bundle)))))

(defn saas-ui-browser [yaas credentials]
  (let [ctx (->> (authenticate yaas credentials)
                 (authorize yaas #{:role/product-manager})
                 (claim-saas))
        product-service (lookup yaas ctx :hybris/product-service)]
    (product-service :hybris/product-service-get ctx :sku123)))
