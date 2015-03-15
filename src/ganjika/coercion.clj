(ns ganjika.coercion
  (:import clojure.lang.PersistentVector))

(def primitives
  "A map of primitive class names to boxed classes"
  {"int" Integer "long" Long
   "short" Short "float" Float
   "double" Double "boolean" Boolean
   "byte" Byte "char" Character})

(def ^:private basic-coercions
  "Map with coercions functions that can be used by most numeric types"
  {Integer int
   Long long
   String str
   Float float
   Double double
   Short short})

(defn- array-coercer [type coercer]
  (fn [coll]
    (let [size (count coll)
          arr (make-array type size)]
      (doall
       (for [i (range size)]
         (aset arr i (coercer (get coll i)))))
      arr)))

(def ^:private coercions
  "Dictionary of coercions. The key of this map is the \"origin type\"
  and the value are the \"supported destination types\", which is a map
  from the \"destination type\" to the function that can do the
  coercion. e.g.: it is possible to convert from Integer to Float,
  because this is not nil: (-> Integer coercions (get Float))"
  {Long basic-coercions
   Integer basic-coercions
   Float basic-coercions
   Double basic-coercions
   Short basic-coercions
   Character {String str
              Integer int
              Long long}
   Boolean {Integer #(if % 1 0)
            String str}
   PersistentVector {(Class/forName "[Ljava.lang.String;") (array-coercer String str)
                     (Class/forName "[Ljava.lang.Integer;") (array-coercer Integer int)
                     (Class/forName "[Ljava.lang.Long;") (array-coercer Long long)
                     (Class/forName "[Ljava.lang.Float;") (array-coercer Float float)
                     (Class/forName "[Ljava.lang.Double;") (array-coercer Double double)
                     (Class/forName "[Ljava.lang.Short;") (array-coercer Short short)
                     (Class/forName "[Ljava.lang.Byte;") (array-coercer Byte byte)
                     (Class/forName "[Ljava.lang.Character;") (array-coercer Character char)
                     (Class/forName "[Ljava.lang.Boolean;") (array-coercer Boolean boolean)
                     (Class/forName "[I") int-array
                     (Class/forName "[Z") boolean-array
                     (Class/forName "[B") byte-array
                     (Class/forName "[C") char-array
                     (Class/forName "[D") double-array
                     (Class/forName "[F") float-array
                     (Class/forName "[J") long-array
                     (Class/forName "[S") short-array}})

(defn- does-not-need-coercion
  "Returns true if all params match the signature types"
  [params signature]
  (every? (fn [[type param]] (instance? type param))
          (map vector signature params)))

(defn- can-coerce
  "Returns a function to coerce param to the provided type or nil if
  none is available"
  [param type]
  (if-let [coercion-fns (coercions (class param))]
    (let [target-type (or (primitives (.getName type)) type)]
      (coercion-fns target-type))))

(defn- get-coercion-fns
  "Get a list of fns that can be used to coerce each param to the
  corresponding signature type"
  [params signature]
  (take-while some? (map (fn [param signature]
                           (can-coerce param signature))
                         params signature)))

(defn- coerce
  "Coerce the list of params to the *first* possible signature in the
  signatures list. If no coercion is found, return params as-is"
  [params signatures]
  (if-let [coercion-fns (some #(if (not (empty? %)) % nil)
                              (map #(get-coercion-fns params %) signatures))]
    (map #(%1 %2) coercion-fns params)
    params))

(defn coerce-params
  "If coercion is needed, coerce the list of params to the *first*
  possible signature in the signatures list. Otherwise return params
  as-is. This assumes each signature has the same length that params."
  [params signatures]
  (let [sigs (filter #(= (count %) (count params)) signatures)]
    (if (some #(does-not-need-coercion params %) sigs)
      params ;; since it does not need coercion, return as-is
      (coerce params signatures))))
