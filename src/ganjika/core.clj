(ns ganjika.core
  (:require [ganjika.util :refer [camel-to-kebab-case is-public
                                  map-values]]
            [clojure.set]
            [ganjika.coercion :refer [primitives]]))

(defn- method-specs
  "Creates a seq of specs for all public methods of the provided class."
  [opts-map ^java.lang.Class c]
  (->> c
       .getDeclaredMethods
       (filter is-public)
       (filter (if-let [pred (eval (:method-predicate opts-map))]
                 pred
                 identity))
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
  [spec opts-maps symbol-prefix]
  (let [params (->> spec
                    :param-count
                    range
                    (map #(gensym (str (name symbol-prefix) %)))
                    vec)]
    (if (or (:disable-type-hinting opts-maps) (:disable-hinting spec))
      (map hint-symbol params (:param-types spec))
      params)))

(defn- apply-coercing
  "Returns an unevaluated apply of method-symbol coercing the provided
  params if necessary."
  [coercions-transformer method-symbol target-var params signatures]
  (let [target-var (if (= target-var :static) '() (list target-var))
        coerced-params (map (fn [_] (gensym)) params)]
    `(apply (fn [~@coerced-params] (~method-symbol
                                   ~@target-var
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
  (let [target-var (if (= instance :static) '() (list instance))]
    `(~method-symbol ~@target-var ~@params)))

(defn- get-apply-strategy [opts-map]
  (if (:disable-coercion opts-map)
    apply-without-coercing
    (partial apply-coercing (or (:coercions-transformer opts-map)
                                identity))))

(defn- build-arity
  "Giving a spec builds the arity part of a fn, i.e.: `([params] body)."
  [target-var opts-map spec]
  (let [raw-method-symbol (symbol (str "." (:raw-name spec)))
        original-params (build-params spec opts-map :original-param)
        signatures (:signatures spec)
        is-void (:is-void spec)
        apply-strategy (get-apply-strategy opts-map)
        apply-fn (fn [method target] (apply-strategy method target original-params signatures))]
    (cond
     ;; arity for static method
     (:is-static spec)
     (let [static-method-symbol (symbol (str (:class-name spec) "/" (:raw-name spec)))]
       `([~@original-params] ~(apply-fn static-method-symbol :static)))
     ;; arity for non-curried function
     (:disable-currying opts-map)
     (let [this-symbol (gensym "this")]
       `([~this-symbol ~@original-params]
         (let [result# ~(apply-fn raw-method-symbol this-symbol)]
           (if ~is-void
             ~this-symbol
             result#))))
     ;; arity for curried function
     :else
     `([~@original-params]
       (let [result# ~(apply-fn raw-method-symbol `(deref ~target-var))]
         (if ~is-void
           @~target-var
           result#))))))

(defn- build-function
  "Builds a fn definition with n arities, where n is (count specs).
  The function will curry the target-sym unless no-currying is true.  If
  type-hinting is true, type-hinted all the function parameters (unless
  the spec forbids it with :disable-hinting)."
  [target-var opts-map [fn-name specs]]
  (let [fn-symbol (symbol fn-name)
        arity-fn (partial build-arity target-var opts-map)]
    `(intern (or ~(:using-ns opts-map) *ns*) (quote ~fn-symbol) (fn ~@(map arity-fn specs)))))

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
  [opts-map clazz]
  (->> clazz
       (method-specs opts-map)
       (group-by :name)
       (map-values remove-repeated-arities)))

(defn parse-opts
  "Takes a sequence of opts and returns an opts hashmap, e.g.:
  '(:using-ns 'foo.bar :disable-coercion) will yield {:using-ns
  'foo-bar :disable-coercion true}"
  [opts]
  (letfn [(complete-opts-pairds [acc opt]
            (let [opts-flags #{:disable-currying :disable-type-hinting :disable-coercion}
                  opt-pair (if (opts-flags opt)
                             (list opt true)
                             (list opt))]
              (concat acc opt-pair)))]
    (->> opts
         (reduce complete-opts-pairds '())
         (apply hash-map))))

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
  coercion map and returns a new one via :coercions-transformer

  You can control which functions are created via :method-predicate fn,
  where fn is a function that receives a java.lang.reflect.Method and
  returns false or nil if the method must be ignored"
  [target & opts]
  {:pre [(some? target),                    ;; target can't be null
         (if (:disable-currying (set opts)) ;; if no currying target must be a class
           (instance? java.lang.Class (eval target))
           :dont-care)]}
  (let [opts-map (parse-opts opts)
        no-currying (:disable-currying opts-map)
        evaled-target (eval target)
        clazz (if no-currying evaled-target (class @evaled-target))
        instance-var (if no-currying nil (eval `~target))
        specs (build-specs opts-map clazz)
        function-builder (partial build-function instance-var opts-map)
        mappings (map-values #(:raw-name (first %)) specs)]
    `(do
       (require 'ganjika.coercion)
       ~(when-let [ns (:using-ns opts-map)]
         `(create-ns ~ns))
       ~@(map function-builder specs)
       ~mappings)))
