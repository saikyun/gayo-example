(ns gayo.path
  (:require #?@(:rn  [["react-native" :as rn]])))

#?(:rn (do
         (defn path
           "Converts a 'simple' resource path into a `file:///...`-path.
  E.g. from \"cat.png\" -> \"file:///var/mobile/Containers/Data/Application/47DD7C48-FE35-4139-A250-85C6C8020AEE/Library/Caches/ExponentExperienceData/%2540saikyun%252Fanimal-capital-demo/ExponentAsset-3adba97a466b5daf3f5bd49f5b17665e.png\"."
           [res]
           (if (.. rn/Image -resolveAssetSource)
             (.. rn/Image (resolveAssetSource res) -uri)
             res)))
   
   :browser (def ^{:doc "In browser mode, we can use the path as it is."}
              path identity))

