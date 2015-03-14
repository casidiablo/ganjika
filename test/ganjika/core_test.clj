(ns ganjika.core-test
  (:require [clojure.test :refer :all]
            [ganjika.core :refer [def-java-fns]])
  (:import ganjika.Example))

(def-java-fns (new Example "Plain"))
(def-java-fns (new Example "Namespaced") :using-ns 'cool.ns)
(def-java-fns (new Example "No Coercion") :using-ns 'no.coercion.ns :disable-coercion)

(def example-instance (new Example "NoCurried"))
(def-java-fns Example :using-ns 'no.haskell :disable-currying)

(deftest functionality
  (testing "Functions were defined"
    ;; curried function
    (is (= "Hi, Plain!" (say-hello)))
    ;; curried namespaced function
    (is (= "Hi, Namespaced!" (cool.ns/say-hello)))
    ;; non-curried namespaced function
    (is (= "Hi, NoCurried!" (no.haskell/say-hello example-instance)))
    ;; currried function, second arity
    (is (= "Hi, Plain!!!!" (say-hello 4)))
    ;; methods with same arity but different signature
    (is (= "sum is 42" (coercive-sum "13" "29")))
    (is (= "sum is 42" (coercive-sum 13 29)))
    ;; static method
    (is (= 42 (add-two-numbers 13 29)))
    ;; type coercion
    (is (= "Bye Terence McKenna" (good-bye ["Terence" "McKenna"])))
    (is (= 25 (square 10/2)))
    ;; without type coercion
    (is (= 25 (no.coercion.ns/square (int 5))))
    (is (thrown? ClassCastException (no.coercion.ns/square 5))))

  (testing "Validation works"
    ;; can't accept nils
    (is (thrown? AssertionError (eval `(def-java-fns nil))))
    ;; if currying is disabled, argument must be a class
    (is (thrown? AssertionError (eval `(def-java-fns (new Object) :disable-currying))))))
