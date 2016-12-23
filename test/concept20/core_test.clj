(ns concept20.core-test
  (:require [clojure.test :refer :all]
            [concept20.core :refer :all]))

(defn yaas []
  (atom
   {:yaas/service-bundles {::product-content-management-v1 {:yaas/services [[:hybris/product-service concept20.core/product-service]]}
                           ::product-content-management-v2 {:yaas/services [[:hybris/product-service concept20.core/product-service]
                                                                            [:hybris/category-service concept20.core/category-service]]}}
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
                            :config/discovery {:hybris/product-service []}}}}))

(deftest test-saas-ui-browser
  (testing "Calling service from saas ui from customer's browser"
    (let [yaas (yaas)
          credentials {:sec/username "MichaelStephan1982" :sec/password "123" :sec/purpose "e-commmerce backoffice"}]
      #_(subscription-ui-browser yaas credentials [::product-content-management-v1 ::product-content-management-v2]) ;; TODO can I be subscribed to two bundles containing the same service
      (saas-ui-browser yaas credentials)
      (clojure.pprint/pprint @yaas))))
