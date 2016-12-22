(ns concept20.core-test
  (:require [clojure.test :refer :all]
            [concept20.core :refer :all]))

(deftest test-saas-ui-browser
  (testing "Calling service from saas ui for customers browser"
    (saas-ui-browser {:sec/username "michael.stephan@sap.com" :sec/password "123"})))
