# ganjika

Ganjika is a library that makes Java interop more clojurish. It provides
the `def-java-fns` macro which will define clojure-like functions out of
a Java class or object. This is what it does for you:

- Makes unnecessary to write/use the usual boilerplate associated with
  wrapping Java libraries
- Optional and customizable layer of type coercion
- Optional currying
- Optional namespacing
- Optional type-hinting

## Usage

Add to leiningen `[ganjika 0.3.1]`. Then:

```clojure
(use 'ganjika.core)

;; This defines clojure-like functions in the current namespace, which are
;; curried with the provided instance:
(def robot (new java.awt.Robot))
(def-java-fns #'robot)
;; This means now you can do: (mouse-move 200 50) instead of the usual
;; way (.mouseMove robot 200 50). Notice that the instance was curried.

;; You can specify a namespace to define the functions:
(def-java-fns #'robot :using-ns 'my.robot)
(my.robot/mouse-move 30 400)

;; Use refer (instead of require/use) to avoid the namespace prefix
(refer 'my.robot :only ['mouse-move])

;; In some scenarios currying is not desirable:
(def-java-fns java.awt.Robot :disable-currying)
(mouse-move robot 30 400)

;; By default the functions are defined using type hinting. This
;; behavior can be changed by doing:
(def-java-fns #'robot :disable-type-hinting)

;; By default functions parameters are coerced to the underlying method
;; types. If that's a problem it can be disabled:
(def-java-fns #'robot :disable-coercion)

;; It is possible to filter which functions will be defined by providing
;; a predicate that must return false or nil when the provided method
;; must be ignored:
(def-java-fns #'robot
  :method-predicate #(not= "someIgnoredMethod" (.getName %)))
```

## Coercion

Imagine a Java class defined like this:

```java
public void someMethod(String[] list) {
  // something
}
```

In order to call this from clojure you might have to do something like:

```clojure
(def obj (new SomeClass))
(def arr (make-array String 2))
(aset arr 0 "foo")
(aset arr 1 "bar")
(.someMethod obj arr)
```

ganjika will coerce the function parameters by default, which means you an just do:

```clojure
;; the vector will be coerced to an array of strings
(some-method ["foo" "bar"])
```

Coercion is driven by an internal map that looks like this:

```clojure
{src-type-1 {dest-type1 fn1
             dest-type2 fn2}}
```

Each top entry in the map represents a supported source type; the inner
map represents the supported destination types providing functions that
will coerce src-type to dest-type.

This map can be modified via :coercions-transformer

```clojure
(def-java-fns #'something
  :coercions-transformer #(assoc % additional-src-type {des-type fn}))
```

### TODO

- Take into account repeated arities for methods with the same name but
  different modifiers (static/non-static)

