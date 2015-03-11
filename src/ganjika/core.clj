(ns ganjika.core
  (:require [ganjika.util :refer [camel-to-kebab-case is-public
                                  map-values]]))

;; TODO better interop by coercing argument types

(defn- method-specs [^java.lang.Class c]
  (->> c
       .getDeclaredMethods
       (filter is-public)
       (map (fn [m]
              {:class-name (.getName c)
               :name (camel-to-kebab-case (.getName m))
               :raw-name (.getName m)
               :param-count (.getParameterCount m)
               :is-void (-> m
                            .getReturnType
                            .getName
                            (= "void"))
               :param-types (.getParameterTypes m)
               :is-static (java.lang.reflect.Modifier/isStatic (.getModifiers m))}))))

(defn hint-param [param type]
  (let [boxed-primitive {"int" java.lang.Integer "long" java.lang.Long
                         "short" java.lang.Short "float" java.lang.Float
                         "double" java.lang.Double "boolean" java.lang.Boolean
                         "byte" java.lang.Byte "char" java.lang.Character}]
    (vary-meta param assoc :tag (or (boxed-primitive (.getName type)) type))))

(defn build-params [use-type-hinting spec]
  (let [params (->> spec
                    :param-count
                    range
                    (map gensym)
                    vec)]
    (if (and use-type-hinting (not (:disable-hinting spec)))
      (map hint-param params (:param-types spec))
      params)))

(defn- build-arity
  "TODO"
  [instance no-currying use-type-hinting spec]
  (let [raw-method-symbol (symbol (str "." (:raw-name spec)))
        params (build-params use-type-hinting spec)
        is-void (:is-void spec)]
    (cond
     ;; define function for static method
     (:is-static spec)
     (let [static-method-symbol (symbol (str (:class-name spec) "/" (:raw-name spec)))]
       `([~@params] (~static-method-symbol ~@params)))
     ;; define non-curried function
     no-currying
     `([this# ~@params]
       (let [result# (~raw-method-symbol this# ~@params)]
         (if ~is-void
           this#
           result#)))
     ;; define curried function
     :else
     `([~@params]
       (let [result# (~raw-method-symbol @~instance ~@params)]
         (if ~is-void
           @~instance
           result#))))))

(defn build-function
  "TODO make private"
  [target-sym no-currying type-hinting [fn-name specs]]
  (let [fn-symbol (symbol fn-name)
        use-type-hinting type-hinting
        arity-fn (partial build-arity target-sym no-currying use-type-hinting)]
    `(defn ~fn-symbol ~@(map arity-fn specs))))

(defn- define-symbol
  "Creates and return a symbol that resolves to the provided object"
  [object]
  (let [class-name (.getSimpleName (class object))
        obj-symbol (gensym class-name)]
    (eval `(def ~obj-symbol ~object))))

(defn- remove-repeated-arities [specs]
  (map (fn [[_ [spec & has-specs-with-same-arity]]]
         (if has-specs-with-same-arity
           (assoc spec :disable-hinting true)
           spec))
       (group-by :param-count specs)))

(defn build-specs [clazz]
  (->> clazz
       method-specs
       (group-by :name)
       (map-values remove-repeated-arities)))

(defmacro def-java-methods
  "Maps the provided provided Java class or object methods to
  clojure-like functions (inside using-ns if any or in the current
  namespace). If target is a class an instance is created (assumes
  default constructor) unless currying is disabled (via :currying
  false)."
  [target & {:keys [using-ns currying disable-type-hinting]}]
  (let [no-currying (false? currying)
        type-hinting (not (true? disable-type-hinting))
        target (eval target)
        [clazz instance] (cond
                          (and (class? target) no-currying) [target nil]
                          no-currying [(class target) nil]
                          (class? target) [target (.newInstance target)]
                          :else [(class target) target])
        specs (build-specs clazz)
        target-sym (if no-currying clazz (define-symbol instance))
        current-ns (.getName *ns*)
        function-builder (partial build-function target-sym no-currying type-hinting)]
    (when-not no-currying (assert (instance? clazz instance)))
    `(do
       (when ~using-ns
         (in-ns ~using-ns))
       ~@(map function-builder specs)
       (in-ns (quote ~current-ns)))))

(comment
  (refer 'fus.noma.da :only ['mouse-move])
  (to-clj java.awt.Robot :using-ns 'fus.noma.da))
