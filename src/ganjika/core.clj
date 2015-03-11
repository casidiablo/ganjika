(ns ganjika.core)

(defn- camel-to-kebab-case [name]
  "Converts from camel to kebab case"
  (let [uppercase-to-hyphened-lowercase
        #(if (Character/isUpperCase %)
           (str "-" (Character/toLowerCase %)) %)]
    (reduce
     #(str %1 (uppercase-to-hyphened-lowercase %2))
     "" name)))

(defn- method-spec [m]
  {:name (camel-to-kebab-case (.getName m))
   :raw-name (.getName m)
   :args (.getParameterCount m)
   :is-void (-> m
                 .getReturnType
                 .getName
                 (= "void"))})

(defn- declare-fn
  "Given a method spec create an unevaluated function form, currying
  over the provided instance"
  [spec instance]
  (let [method-name (symbol (:name spec))
        raw-method-name (symbol (str "." (:raw-name spec)))
        params (vec (map gensym (range (:args spec))))
        is-void (:is-void spec)]
    `(defn ~method-name [~@params]
       (let [return# (~raw-method-name @~instance ~@params)]
         (if ~is-void
           @~instance
           return#)))))

(defn- define-symbol
  "Creates and return a symbol that resolves to the provided object"
  [object]
  (let [class-name (.getSimpleName (class object))
        obj-symbol (gensym class-name)]
    (eval `(def ~obj-symbol ~object))))

;; TODO allow to receive instance
(defmacro def-java-methods
  "TODO"
  [target & {:keys [using-ns]}]
  (let [target (eval target)
        [clazz instance] (if (class? target)
                           [target (.newInstance target)]
                           [(class target) target])
        specs (map method-spec (.getDeclaredMethods clazz))
        target-sym (define-symbol instance)
        current-ns (.getName *ns*)]
    (assert (instance? clazz instance))
    `(do
       (when ~using-ns
         (in-ns ~using-ns))
       ~@(map #(declare-fn % target-sym) specs)
       (in-ns (quote ~current-ns)))))

(comment
  ;; no currying
  ;; multiple arity
  (refer 'fus.noma.da :only ['mouse-move])
  (to-clj java.awt.Robot :using-ns 'fus.noma.da))
