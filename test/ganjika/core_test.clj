(ns ganjika.core-test
  (:require [clojure.test :refer :all]
            [ganjika.core :refer [def-java-fns]])
  (:import ganjika.Example))

(def plain (new Example "Plain"))
(def-java-fns #'plain)

(def namespaced (new Example "Namespaced"))
(def-java-fns #'namespaced :using-ns 'cool.ns)

(def no-coercion (new Example "No Coercion"))
(def-java-fns #'no-coercion :using-ns 'no.coercion.ns :disable-coercion)

(def no-curried (new Example "NoCurried"))
(def-java-fns Example :using-ns 'no.haskell :disable-currying)

(deftest functionality
  (testing "Functions were defined"
    ;; curried function
    (is (= "Hi, Plain!" (say-hello)))
    ;; curried namespaced function
    (is (= "Hi, Namespaced!" (cool.ns/say-hello)))
    ;; non-curried namespaced function
    (is (= "Hi, NoCurried!" (no.haskell/say-hello no-curried)))
    ;; currried function, second arity
    (is (= "Hi, Plain!!!!" (say-hello 4)))
    ;; methods with same arity but different signature
    (is (= "sum is 42" (coercive-sum "13" "29")))
    (is (= "sum is 42" (coercive-sum 13 29)))
    ;; static method
    (is (= 42 (add-two-numbers 13 29))))

  (testing "Validation works"
    ;; can't accept nils
    (is (thrown? AssertionError (eval `(def-java-fns nil))))
    ;; if currying is disabled, argument must be a class
    (is (thrown? AssertionError (eval `(def-java-fns (new Object) :disable-currying)))))

  (testing "Coercions"
    ;; type coercion
    (is (= "Bye Terence McKenna" (good-bye ["Terence" "McKenna"])))
    (is (= 25 (square 10/2)))
    (is (= 42 (var-args-int [2 3 5 7 9 13 3])))
    (is (= 42 (var-args-int '(2 3 5 7 9 13 3))))
    (is (= 42 (var-args-long [2 3 5 7 9 13 3])))
    (is (= 42 (var-args-short [2 3 5 7 9 13 3])))
    (is (= "abc" (var-args-char [\a \b \c])))
    (is (= 42.0 (var-args-float [2.0 3.0 5 7.0 9 13.0 3])))
    (is (= 42.0 (var-args-double [2.0 3.0 5 7.0 9 13.0 3])))
    (is (= 42 (var-args-byte [2r100000 2r1000 2r10])))
    (is (var-args-boolean [false false false false true]))
    ;; without type coercion
    (is (= 25 (no.coercion.ns/square (int 5))))
    (is (thrown? ClassCastException (no.coercion.ns/square 5)))))
