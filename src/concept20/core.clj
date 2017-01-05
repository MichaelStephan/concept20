(ns concept20.core
  (:require [clojure.spec :as s])
  (:use [slingshot.slingshot :only [throw+]]))

; IDEAs

; * when service A (=originator) calls service B (=callee) originator adds signed nonce (=proof of identity) into ctx
; * ctx elements may be subject to ttl rules, e.g. a ctx element can be configured to only survive 5 hops until it is dropped
; * ctx element may be added to be readable by certain service only (encrypted using public key)
; * on service startup services register themselves among each other and exchange secrets

; TODOs
; * :config/discovery contains map with service identifier used as keys. The key should become another map to allow for all items to be looked up, not only services 


;context helper

(defn saas? [ctx]
  (some (fn [ctx]
          (get-in ctx [:sec/claims :yaas/saas])) ctx))

(defn subscription [ctx]
  (some (fn [ctx]
          (get-in ctx [:sec/claims :yaas/subscription]))
        (reverse ctx)))

(defn claim-saas [ctx]
  (conj ctx {:sec/claims {:yaas/saas true}}))

(defn claim-originator [ctx originator]
  (conj ctx {:sec/claims {:service/originator originator}}))

(defn user-id [ctx]
  (some (fn [ctx]
          (get-in ctx [:sec/claims :sec/id]))
        (reverse ctx)))

(defn tenant [ctx]
  (some (fn [ctx]
          (get-in ctx [:sec/claims :sec/tenant]))
        (reverse ctx)))

(defn roles [ctx]
  ; does it really make sense to combine all roles? do we grant too much privileges to the originator?
  (apply clojure.set/union (map (fn [ctx]
                                  (get-in ctx [:sec/claims :sec/roles]))
                                ctx)))

; platform 

(defn measure [yaas ctx key value]
  (swap! yaas update-in [:yaas/measurements] conj [(subscription ctx) key value]))

(defn load-credentials [yaas credentials]
  (get-in @yaas [:yaas/authentication-db (dissoc credentials :yaas/subscription :sec/password)]))

(defn authenticate [yaas {:keys [:sec/username :sec/password :yaas/subscription] :as credentials}]
  [{:sec/claims (let [{stored-password :sec/password :as stored-credentials} (load-credentials yaas credentials)]
                  (if (= stored-password password)
                    (-> stored-credentials
                        (dissoc :sec/password)
                        (assoc :sec/username username)
                        (assoc :sec/provider "https://sap-identity-provider")
                        (assoc :yaas/subscription subscription))
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

(defn check-authorized? [yaas service function ctx requested-scope]
  (when-not (authorized? yaas service function ctx requested-scope)
    (throw+ {:type ::not-authorized :hint (str service "/" function)}))
  true)

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
                                                                                                   (when (= (subscription ctx) subscription-id)
                                                                                                     {:yaas/service uri
                                                                                                      :yaas/subscription subscription-id}))}))))

(defn load-service-bundle [yaas id]
  (or (get-in @yaas [:yaas/service-bundles id])
      (throw+ {:type ::not-found :entity :yaas/service-bundle :hint id})))

(defn new-subscription [parent-subscription-id service-bundle-id]
  (cond-> {:subscription/id (-> (java.util.UUID/randomUUID) str keyword)
           :service-bundle/id service-bundle-id
           :subscription/state {}
           :subscription/created (System/currentTimeMillis)}
    parent-subscription-id (assoc :subscription/parent parent-subscription-id)))

(defn save-subscription [yaas ctx {:keys [:subscription/id] :as subscription}]
  (swap! yaas assoc-in [:yaas/subscriptions (tenant ctx) id] subscription))

(defn load-service [yaas id]
  (or (get-in @yaas [:yaas/services id])
      (throw+ {:type ::not-found :entity :yaas/service :hint id})))

(defn subscribe
  ([yaas ctx service-bundle-id]
   (subscribe  yaas ctx nil service-bundle-id))
  ([yaas ctx parent-subscription-id service-bundle-id]
   (check-authorized? yaas :hybris/platform :hybris/subscription-service-subscribe ctx #{:scope/subscription-manage})
   (let [{:keys [:subscription/id] :as subscription} (new-subscription parent-subscription-id service-bundle-id)
         {:keys [yaas/services] :as service-bundle} (load-service-bundle yaas service-bundle-id)]
     (doto yaas
       (save-subscription ctx subscription)
       (provision ctx service-bundle subscription)
       (announce ctx service-bundle id))
     (doall (for [[service] services]
              (doall (for [service-bunde-id (:yaas/depends-on (load-service yaas service))]
                       (subscribe yaas ctx id service-bunde-id)))))
     id)))

; top level applications

(defn product-service [yaas function ctx & args]
  (measure yaas ctx [:hybris/product-service-v1 function :api-call] 1)
  (condp = function
    :hybris/product-service-get (let [[id] args
                                      data-storage-service (lookup yaas ctx :hybris/data-storage-service-v1)
                                      ctx (claim-originator :hybris/product-service-v1)]
                                  (check-authorized? yaas :hybris/product-service-v1 function ctx #{:scope/product-read})
                                  (data-storage-service :hybris/data-storage-get ctx id))
    (throw+ {:type ::function-not-found :hint function})))

(defn data-storage-service [yaas function ctx & args]
  (measure yaas ctx [:hybris/data-storage-service-v1 function :api-call] 1)
  (condp = function
    :hybris/data-storage-get (let [[id] args
                                   data-storage-service (lookup yaas ctx :hybris/data-storage-service-v1)
                                   ctx (claim-originator :hybris/data-storage-service-v1)]
                               (check-authorized? yaas :hybris/data-storage-service-v1 function ctx #{:scope/data-storage-read})
                               [{:id id
                                 :name "banana"}])
    (throw+ {:type ::function-not-found :hint function})))

(defn category-service [yaas function ctx & args])

(defn subscription-ui-browser [yaas credentials service-bundles]
  (let [ctx (->> (authenticate yaas credentials)
                 (authorize yaas #{:role/administrator}))]
    (doall (for [service-bundle service-bundles]
             (subscribe yaas ctx service-bundle)))))

(defn saas-product-content-management-ui-browser [yaas credentials]
  (let [ctx (->> (authenticate yaas credentials)
                 (authorize yaas #{:role/product-manager})
                 (claim-saas))
        product-service (lookup yaas ctx :hybris/product-service-v1)]
    (product-service :hybris/product-service-get ctx :sku123)))
