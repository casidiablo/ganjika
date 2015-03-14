(ns ganjika.coercion
  (:import clojure.lang.PersistentVector))

(def primitives
  {"int" Integer "long" Long
   "short" Short "float" Float
   "double" Double "boolean" Boolean
   "byte" Byte "char" Character})

(def ^:private basic-coercions
  {Integer int
   String str
   Float float
   Double double
   Short short})

(def ^:private coercions
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
   PersistentVector {(Class/forName "[Ljava.lang.String;")
                     (fn [v] (let [size (count v)
                                  arr (make-array String size)]
                              (doall
                               (for [i (range size)]
                                 (aset arr i (get v i))))
                              arr))}})

(defn- does-not-need-coercion [params signature]
  (every? (fn [[type param]] (instance? type param))
          (map vector signature params)))

(defn- can-coerce [param type]
  (if-let [coercion-fns (coercions (class param))]
    (let [target-type (or (primitives (str type)) type)]
      (coercion-fns target-type))))

(defn- get-coercion-fns [params signature]
  (take-while some? (map (fn [param signature]
                           (can-coerce param signature))
                         params signature)))

(defn- coerce [params signatures]
  (if-let [coercion-fns (some #(if (not (empty? %)) % nil)
                              (map #(get-coercion-fns params %) signatures))]
    (map #(%1 %2) coercion-fns params)
    params))

(defn coerce-params [params signatures]
  (let [sigs (filter #(= (count %) (count params)) signatures)]
    (if (some #(does-not-need-coercion params %) sigs)
      params ;; since it does not need coercion, return as-is
      (coerce params signatures))))
