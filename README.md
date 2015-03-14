# ganjika

Provides the `def-java-fns` macro which will define clojure-like
functions out of a Java class or object. It's meant to make it easier to
wrap Java libraries without the usual boilerplate. It also provides
coercion by from clojure to Java types and data structures.

## Usage

Add to leiningen `[ganjika 0.1.0-SNAPSHOT]`. Then:

```clojure
(use 'ganjika.core)

;; This defines clojure-like functions in the current namespace, which are
;; curried with the provided instance:

(def robot (new java.awt.Robot))
(def-java-fns robot)

;; This means now you can do: (mouse-move 200 50) instead of the usual
;; way (.mouseMove robot 200 50)

;; You can specify a namespace to define the functions:

(def-java-fns robot :using-ns 'my.robot)
(my.robot/mouse-move 30 400)

;; Use refer (instead of require/use) to avoid the namespace prefix

(refer 'my.robot :only ['mouse-move])

;; In some scenarios currying is not desirable:

(def-java-fns java.awt.Robot :currying false)
(mouse-move robot 30 400)

;; By default the functions are defined using type hinting. This
;; behavior can be changed by doing:

(def-java-fns java.awt.Robot :disable-type-hinting true)
```

### TODO

- Take into account repeated arities for methods with the same name but
  different modifiers (static/non-static)
- Make coercion optional
