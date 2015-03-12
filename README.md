# ganjika

Provides the `def-java-fns` macro which will define clojure-like
functions out of a Java class or object. It's meant to make it easier to
wrap Java libraries without the usual boilerplate.

## Usage

Add to leiningen `[ganjika 0.1.0-SNAPSHOT]`. Then:

```clojure
(use 'ganjika.core)

;; usage from class
(def-java-fns java.awt.Robot)

;; the above does two things:
;; - it creates an instance of the provided class (assumes a default ctor)
;; - defines clojure-like functions for that class in the current namespace,
;;   which are curried with the instance created; i.e. you can do:
(mouse-move 200 50) ;; instead of (.mouseMove robot 200 50)

;; you can specify an instance yourself, e.g.:
(def robot (new java.awt.Robot))
(def-java-fns robot)

;; you can specify a namespace to define the functions
(def-java-fns robot :using-ns 'my.robot)
(my.robot/mouse-move 30 400)
;; use refer instead of require/use to avoid the namespace prefix
(refer 'my.robot :only ['mouse-move])

;; in some scenarios currying is not desirable
(def-java-fns java.awt.Robot :currying false)
(mouse-move robot 30 400)

;; By default the functions are defined using type hinting.
;; This behavior can be changed by doing:
(def-java-fns java.awt.Robot :disable-type-hinting true)
```

### TODO

- Params coercion
- Constructor fns
- Testing
