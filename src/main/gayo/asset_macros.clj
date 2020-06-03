(ns gayo.asset-macros
  "Macros for generating the js/require calls that expo needs."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint print-table]]
            [clojure.reflect :as r]))

(defmacro get-assets
  [path rel-path]
  `(do
     ~@(for [f (file-seq (io/file path))
             :when (.isFile f)]
         `(js/require ~(str rel-path f)))))

(defmacro assets-to-map
  [path rel-path]
  (into {} (for [f (file-seq (io/file path))
                 :when (and (.isFile f)
                            (not (str/ends-with? (str f) ".DS_Store")))]
             [(apply str (drop (inc (count path)) (str f))) `(js/require ~(str rel-path f))])))


(comment
  (clojure.pprint/pprint
   (macroexpand '(gayo.assets/assets-to-map "assets" "../../../")))
  )
