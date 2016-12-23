(ns concept20.core-test
  (:require [clojure.test :refer :all]
            [concept20.core :refer :all]))

(defn yaas []
  (atom
   {:yaas/measurements []
    :yaas/authentication-db {"michael.stephan@sap.com" {:sec/id "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
                                                        :sec/password "123"
                                                        :sec/tenant ::sap
                                                        :sec/roles #{:role/product-manager}}}
    :yaas/config-db {::sap {:config/authorization {:role/product-manager #{:scope/product-read}}
                            :config/mapping {:hybris/product-service [(fn [ctx]
                                                                        (when (saas? ctx)
                                                                          {:yaas/service concept20.core/product-service
                                                                           :yaas/subscription ::some-saas-subscription}))
                                                                      (fn [ctx]
                                                                        {:yaas/service product-service
                                                                         :yaas/subscription ::some-low-touch-subscription})]}}}}))

(deftest test-saas-ui-browser
  (testing "Calling service from saas ui for customers browser"
    (let [yaas (yaas)]
      (saas-ui-browser yaas {:sec/username "michael.stephan@sap.com" :sec/password "123"})
      (get @yaas :yaas/measurements))))
