(ns gayo.log)

(defn log!
  ([s1]
   #?(:cljr (ac/log s1)
      :cljs (println s1)))
  ([s1 s2]
   #?(:cljr (ac/log s1 s2)
      :cljs (println s1 s2)))
  ([s1 s2 s3]
   #?(:cljr (ac/log s1 s2 s3)
      :cljs (println s1 s2 s3)))
  ([s1 s2 s3 & strs]
   #?(:cljr (apply ac/log s1 s2 s3 strs)
      :cljs (apply println s1 s2 s3 strs))))

#_(defn log!
    ([s1] nil)
    ([s1 s2] nil)
    ([s1 s2 s3] nil)
    ([s1 s2 s3 & strs] nil))