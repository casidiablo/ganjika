(ns ganjika.util)

(defn camel-to-kebab-case [name]
  "Converts from camel to kebab case"
  (let [uppercase-to-hyphened-lowercase
        #(if (Character/isUpperCase %)
           (str "-" (Character/toLowerCase %)) %)]
    (reduce
     #(str %1 (uppercase-to-hyphened-lowercase %2))
     "" name)))

(defn is-public [^java.lang.reflect.Method m]
  (java.lang.reflect.Modifier/isPublic (.getModifiers m)))

(defn map-values [f m]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))
