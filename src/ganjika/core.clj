(ns ganjika.core)

(defn- camel-to-kebab-case [name]
  "Converts from camel to kebab case"
  (let [uppercase-to-hyphened-lowercase
        #(if (Character/isUpperCase %)
           (str "-" (Character/toLowerCase %)) %)]
    (reduce
     #(str %1 (uppercase-to-hyphened-lowercase %2))
     "" name)))

(defn- is-public [^java.lang.reflect.Method m]
  (java.lang.reflect.Modifier/isPublic (.getModifiers m)))

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

(defn hint-params [spec]
  (let [boxer {"int" "java.lang.Integer" "long" "java.lang.Long"
               "short" "java.lang.Short" "float" "java.lang.Float"
               "double" "java.lang.Double" "boolean" "java.lang.Boolean"
               "byte" "java.lang.Byte" "char" "java.lang.Character"}]
    (map #(let [name (.getName %)]
            (str "^" (or (boxer name) name)))
         (:param-types spec))))

(defn- build-arity
  "TODO"
  [instance no-currying use-type-hinting spec]
  (let [raw-method-name (symbol (str "." (:raw-name spec)))
        static-method-name (when (:is-static spec) (symbol (str (:class-name spec) "/" (:raw-name spec))))
        params (vec (map gensym (range (:param-count spec))))
        hinted-params (if use-type-hinting (interleave (hint-params spec) params) params)
        is-void (:is-void spec)]
    (cond
     ;; define function for static method
     (:is-static spec)
     `([~@params] (~static-method-name ~@params))
     ;; define non-curried function
     no-currying
     `([this# ~@params]
       (let [result# (~raw-method-name this# ~@params)]
         (if ~is-void
           this#
           result#)))
     ;; define curried function
     :else
     `([~@params]
       (let [result# (~raw-method-name @~instance ~@params)]
         (if ~is-void
           @~instance
           result#))))))

(defn build-function
   ;; TODO overloaded arities
  "TODO make private"
  [[fn-name specs] target-sym no-currying type-hinting]
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
        specs (method-specs clazz)
        specs-by-name (group-by :name specs)
        target-sym (if no-currying clazz (define-symbol instance))
        current-ns (.getName *ns*)]
    (when-not no-currying (assert (instance? clazz instance)))
    `(do
       (when ~using-ns
         (in-ns ~using-ns))
       ~@(map #(build-function % target-sym no-currying type-hinting) specs-by-name)
       (in-ns (quote ~current-ns)))))

(comment
  ;;- field/static methods with the same name??? renaming?
  ;;- better interop by coercing argument types
  (refer 'fus.noma.da :only ['mouse-move])
  (to-clj java.awt.Robot :using-ns 'fus.noma.da))
