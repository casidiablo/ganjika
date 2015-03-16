(ns ganjika.core
  (:require [ganjika.util :refer [camel-to-kebab-case is-public
                                  map-values]]
            [clojure.set]
            [ganjika.coercion :refer [primitives]]))

(defn- method-specs
  "Creates a seq of specs for all public methods of the provided class."
  [^java.lang.Class c]
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
               :param-types (vec (.getParameterTypes m))
               :is-static (java.lang.reflect.Modifier/isStatic (.getModifiers m))}))))

(defn- hint-symbol
  "Hints the provided symbol using the provided type.
  For primitive types uses the Java wrapper classes."
  [sym type]
  (vary-meta sym assoc :tag (or (primitives (.getName type)) type)))

(defn- build-params
  "Given a method spec returns a list of symbols to be used as function
  parameters (type-hinting them if necessary)"
  [spec opts-flags symbol-prefix]
  (let [params (->> spec
                    :param-count
                    range
                    (map #(gensym (str (name symbol-prefix) %)))
                    vec)]
    (if (and (not (:disable-type-hinting opts-flags)) (not (:disable-hinting spec)))
      (map hint-symbol params (:param-types spec))
      params)))

(defn- apply-coercing
  "Returns an unevaluated apply of method-symbol coercing the provided
  params if necessary."
  [coercions-transformer method-symbol instance params signatures]
  (let [instance (if (= instance :static) '() (list instance))
        coerced-params (map (fn [_] (gensym)) params)]
    `(apply (fn [~@coerced-params] (~method-symbol
                                   ~@instance
                                   ~@coerced-params))
            (if (not (empty? (list ~@params)))
              (ganjika.coercion/coerce-params
               (list ~@params)
               (list ~@signatures)
               ~coercions-transformer)
              '()))))

(defn- apply-without-coercing
  "Returns an unevaluated application of method-symbol"
  [method-symbol instance params signatures]
  (let [instance (if (= instance :static) '() (list instance))]
    `(~method-symbol ~@instance ~@params)))

(defn- build-arity
  "Giving a spec builds the arity part of a fn, i.e.: `([params] body)."
  [instance opts-flags coercions-xformer spec]
  (let [raw-method-symbol (symbol (str "." (:raw-name spec)))
        original-params (build-params spec opts-flags :original-param)
        signatures (:signatures spec)
        is-void (:is-void spec)
        application-strategy (if (:disable-coercion opts-flags)
                               apply-without-coercing
                               (partial apply-coercing coercions-xformer))
        apply-fn (fn [method target] (application-strategy method target original-params signatures))]
    (cond
     ;; arity for static method
     (:is-static spec)
     (let [static-method-symbol (symbol (str (:class-name spec) "/" (:raw-name spec)))]
       `([~@original-params] ~(apply-fn static-method-symbol :static)))
     ;; arity for non-curried function
     (:disable-currying opts-flags)
     (let [this-symbol (gensym "this")]
       `([~this-symbol ~@original-params]
         (let [result# ~(apply-fn raw-method-symbol this-symbol)]
           (if ~is-void
             ~this-symbol
             result#))))
     ;; arity for curried function
     :else
     `([~@original-params]
       (let [result# ~(apply-fn raw-method-symbol `@~instance)]
         (if ~is-void
           @~instance
           result#))))))

(defn- build-function
  "Builds a fn definition with n arities, where n is (count specs).
  The function will curry the target-sym unless no-currying is true.  If
  type-hinting is true, type-hinted all the function parameters (unless
  the spec forbids it with :disable-hinting)."
  [ns target-sym opts-flags coercions-xformer [fn-name specs]]
  (let [fn-symbol (symbol fn-name)
        arity-fn (partial build-arity target-sym opts-flags coercions-xformer)]
    `(intern (or ~ns *ns*) (quote ~fn-symbol) (fn ~@(map arity-fn specs)))))

(defn- define-symbol
  "Creates and return a symbol that resolves to the provided object"
  [object]
  (let [class-name (.getSimpleName (class object))
        obj-symbol (gensym class-name)]
    (eval `(def ~obj-symbol ~object))))

(defn- remove-repeated-arities
  "Given a seq of specs with the same :name, removes the ones that have
  repeated arities."
  [specs]
  (map (fn [[_ [spec & has-specs-with-same-arity]]]
         (assoc spec
           :disable-hinting (not (empty? has-specs-with-same-arity))
           :signatures (map :param-types (cons spec has-specs-with-same-arity))))
       (group-by :param-count specs)))

(defn- build-specs
  "Given a class builds map of method specs grouped by :name"
  [clazz]
  (->> clazz
       method-specs
       (group-by :name)
       (map-values remove-repeated-arities)))

(defmacro def-java-fns
  "Maps the methods of the provided object to clojure-like
  functions (inside :using-ns if any or in the current namespace). All
  fns are curried with the provided object unless :disable-currying flag
  is provided; when currying is disabled target *must* be a class.

  Function arguments are hinted by default unless :disable-type-hinting
  is provided.

  Function arguments are coerced if they don't type check against
  underlying method signature, unless :disable-coercion is
  provided. Coercion is driven by a map with this structure: {OriginType
  {DestType1 coercion-fn-1 DestType2 coercion-fn 2}}. The default map
  can be modified by providing a function that receives the default
  coercion map and returns a new one via :coercions-transformer"
  [target & opts]
  {:pre [(some? target),                    ;; target can't be null
         (if (:disable-currying (set opts)) ;; if no currying target must be a class
           (instance? java.lang.Class (eval target))
           :dont-care)]}
  (let [opts-flags #{:disable-currying :disable-type-hinting :disable-coercion}
        provided-flags (clojure.set/intersection opts-flags (set opts))
        {:keys [using-ns coercions-transformer]} (->> opts
                                                      (filter (complement opts-flags))
                                                      (apply hash-map))
        coercions-xformer (or coercions-transformer identity)
        no-currying (:disable-currying provided-flags)
        resolved-target (eval target)
        clazz (if no-currying resolved-target (class resolved-target))
        instance (if no-currying nil resolved-target)
        specs (build-specs clazz)
        target-sym (if no-currying clazz (define-symbol instance))
        function-builder (partial build-function using-ns target-sym provided-flags coercions-xformer)
        mappings (map-values #(:raw-name (first %)) specs)]
    (when-not no-currying (assert (instance? clazz instance)))
    `(do
       (require 'ganjika.coercion)
       (when ~using-ns
         (create-ns ~using-ns))
       ~@(map function-builder specs)
       ~mappings)))
