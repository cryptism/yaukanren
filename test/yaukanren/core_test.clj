(ns yaukanren.core-test
  (:require [clojure.test :refer :all]
            [yaukanren.core :refer [assp
                                    lvar
                                    lvar=?
                                    walk
                                    lvar
                                    unify
                                    ===
                                    call-fresh]]))

(deftest yaukanren-tests
  (testing "assp"
    (are [u l, x] (= (assp (partial lvar=? u) l) x)
      (lvar 0) [[(lvar 0) "test"]], [(lvar 0) "test"]
      (lvar 1) [(lvar 0)], nil))

  (testing "walk"
    (are [u s, x] (= (walk u s) x)
      "test" [] "test"
      (lvar 0) [[(lvar 0) "test"]] "test"
      (lvar 1) [[(lvar 0) (lvar 1)] [(lvar 1) "test"]], "test"))

  (testing "unify"
    (are [u v, s] (= (unify u v []) s)
      0 0 []
      0 1 nil
      (lvar 0) "test" [[(lvar 0) "test"]]
      "test" (lvar 0) [[(lvar 0) "test"]]
      [(lvar 0) (lvar 1)] [(lvar 1) "test"] [[(lvar 0) (lvar 1)] [(lvar 1) "test"]]))

  (testing "call-fresh"
    (let [substitutions ((call-fresh #(=== % "test")) (list '() 0))
          state (first substitutions)
          ss (first state)]
      (is (= (second state) (count (first state))))
      (is (= "test" (second (assp #(lvar=? (lvar 0) %) ss)))))))
