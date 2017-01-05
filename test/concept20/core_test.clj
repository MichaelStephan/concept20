(ns concept20.core-test
  (:require [clojure.test :refer :all]
            [concept20.core :refer :all]))

(defn yaas []
  (atom {:yaas/service-bundles {::data-storage-v1 {:yaas/services [[:hybris/data-storage-service-v1 concept20.core/data-storage-service]]}
                                ::product-content-management-v1 {:yaas/services [[:hybris/product-service-v1 concept20.core/product-service]]}
                                ::product-content-management-v2 {:yaas/services [[:hybris/product-service-v1 concept20.core/product-service]
                                                                                 [:hybris/category-service-v2 concept20.core/category-service]]}}
         :yaas/services {:hybris/data-storage-service-v1 {:sec/scopes [:scope/data-storage-read :scope/data-storage-manage]
                                                          :yaas/depends-on []}
                         :hybris/product-service-v1 {:sec/scopes [:scope/product-read :scope/product-manage]
                                                     :yaas/depends-on [::data-storage-v1]}
                         :hybris/category-service-v2 {:sec/scopes [:scope/product-read :scope/product-manage]
                                                      :yaas/depends-on [::data-storage-v1]}}
         :yaas/subscriptions {}
         :yaas/measurements []
         :yaas/authentication-db {{:sec/username "MichaelStephan1982"
                                   :sec/purpose "e-commmerce backoffice"} {:sec/id "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
                                                                           :sec/password "123"
                                                                           :sec/tenant ::sap
                                                                           :sec/full-username "Michael Stephan"
                                                                           :sec/mail "michael.stephan@sap.com"}}
         :yaas/authorization-db {"6ba7b810-9dad-11d1-80b4-00c04fd430c8" {:sec/roles #{:role/product-manager :role/administrator}}}
         :yaas/config-db {::sap {:config/authorization {:role/product-manager #{:scope/product-read}
                                                        :role/administrator #{:scope/subscription-manage}}
                                 :config/discovery {:hybris/product-service-v1 []}}}}))

(deftest test-saas-ui-browser
  (testing "Calling service from saas ui from customer's browser"
    (let [yaas (yaas)
          credentials {:sec/username "MichaelStephan1982" :sec/password "123" :sec/purpose "e-commmerce backoffice"}
          subscriptions (subscription-ui-browser yaas credentials [::product-content-management-v1 ::product-content-management-v2])]
      (println subscriptions)
      (saas-product-content-management-ui-browser yaas (assoc credentials :yaas/subscription (second subscriptions))) ; does it make sense to propagate the subscription id along a call. TODO make sure that implict subscriptions are automatically taken into consideration !!! currently responsible for not not working
      (clojure.pprint/pprint @yaas))))
