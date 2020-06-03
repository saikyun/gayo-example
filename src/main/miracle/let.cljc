(ns miracle.let
  (:refer-clojure :exclude [let])
  (:require [clojure.walk :refer [postwalk]]
            [clojure.test :as t :refer [is deftest]]))

(defn in-form
  [form pred]
  (clojure.core/let [vars (transient [])]
    (postwalk (fn [f]
                (when (pred f)
                  (conj! vars f))
                f)
              form)
    (persistent! vars)))

(defn used-returns
  [form]
  (in-form form #(and (symbol? %)
                      (= % '$return))))


(defn replace-in-form
  [form pred replace]
  (postwalk (fn [f]
              (if (pred f)
                (replace f)
                f))
            form))

(defn replace-return
  [form replace]
  (replace-in-form
   form
   (fn [f]
     (and (seq? f)
          (symbol? (first f))
          (= '$return (first f))))
   replace))

(defn has-return?
  [form]
  (seq (used-returns form)))

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

(defmacro if-cljs
  "Return then if we are generating cljs code and else for Clojure code.
   https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
  [then else]
  (if (cljs-env? &env) then else))

(def ops
  #{'!!
    '!nil
    '!not
    '!all
    '!err})

;; trying to make clj-kondo happy
(def $ ::implicit-error-variable)
(def !! ::nil-or-err)
(def !nil ::nil)
(def !not ::false)
(def !all ::all)
(def !err ::err)

(defn is-op?
  [v]
  (get ops v))

(declare handle-do-form)

(defn handle-nil
  [f err-f]
  `(clojure.core/let [res# ~f]
     (if-not (nil? res#)
       res#
       ~(if err-f
          `(clojure.core/let [~'$ nil ;; $ means "the thing that triggered `err-f`
                              new-res# ~err-f]
             (if-not (nil? new-res#)
               new-res#
               nil))
          `nil))))

(defn handle-false
  [f err-f]
  (let [res-sym (gensym "res")]
    `(clojure.core/let [~res-sym ~f]
       (if ~res-sym
         ~res-sym
         ~(if err-f
            `(clojure.core/let [~'$ ~res-sym ;; $ means "the thing that triggered `err-f`
                                new-res# ~err-f]
               new-res#)
            res-sym)))))

(defn handle-err
  [f err-f]
  `(try
     ~f
     (catch ~(if-cljs js/Error Exception) e#
       (clojure.core/let [~'$ e#] ;; $ means "the thing that triggered `err-f`
         ~err-f))))

(defn handle-op
  [f op err-f]
  (condp = op
    '!err
    (handle-err f err-f)
    
    '!nil
    (handle-nil f err-f)
    
    '!not
    (handle-false f err-f)
    
    '!!
    (handle-err (handle-nil f err-f)
                (handle-nil err-f nil) ;; if an error was threwn, check err-f for nil afterwards
                )
    
    '!all
    (handle-err (handle-false f err-f)
                (handle-false err-f nil) ;; if an error was threwn, check err-f for nil afterwards
                )
    
    ;; else
    (throw (#?(:cljs js/Error.
               :clj Exception.) (str "Operator " op " not handled.")))))

(defn return-wrap
  [form opts]
  (if (has-return? form)
    (clojure.core/let [opts (assoc opts ::return-v (::return-sym opts)) 
                       f2 ((::replace-return opts) form)]
      [f2 opts])
    [form opts]))

(defn return-if-return-set
  [opts form]
  (if-let [return-v (::return-v opts)]
    `(if (not= @~return-v ::not-set)
       @~return-v
       ~form)
    form))

(defn handle-do-form
  [[[k v] & forms] body opts]
  (clojure.core/let [[[next-k next-v]] forms]
    (return-if-return-set
     opts
     (cond
       (nil? k) (cond 
                  (not (seq? body)) body
                  (not (seq (drop 1 body))) (first body)
                  :else `(do ~@body))
       
       :else
       (if (is-op? next-k)
         (clojure.core/let [[form opts] (return-wrap
                                         (handle-op v
                                                    next-k ;; op
                                                    next-v ;; err-f
                                                    )
                                         opts)]
           `(clojure.core/let [~k ~form]
              ~(handle-do-form (rest forms) ;; we already took next-k and next-v
                               body
                               opts)))
         `(clojure.core/let [~k ~v]
            ~(handle-do-form forms body opts)))))))

(defmacro let
  "Acts like `clojure.core/let`, and supports `$return` various operators such as `!!`.
  
  `(let [a (/ 5 0) !! ($return :divided-by-zero)]
     a)` ;;=> :divided-by-zero
  
  `!err` catches errors, `$` is implicitly bound to the error
  `!nil` catches `nil`, `$` is bound to `nil`
  `!not` catches falsy values (`false` and `nil`), `$` is bound to the catched value
  `!!` like `!err` + `!nil`
  `!!all` like `!err` + `!not`

  See more examples in the tests.
  "
  [binds & body]
  (if (has-return? binds)
    (clojure.core/let [return-sym (gensym "ret-v")
                       rep-ret (fn [form] (replace-return form (fn [[_ value]]
                                                                 `(vreset! ~return-sym ~value))))]
      `(clojure.core/let [~return-sym (volatile! ::not-set)]
         ~(handle-do-form (partition 2 binds) body {::return-sym return-sym
                                                    ::replace-return rep-ret})))
    (handle-do-form (partition 2 binds) body {})))

(deftest no-crash
  (is (= 20 (let [a (get nil :a) !nil 20]
              a)))

  (is (= 1377 (let [a 10
                    b (= :cat :dog) !! ($return 20) ;; !! doesn't catch false
                    c 30]
                (+ a (or b 1337) c))))  
  
  (is (= 20 (let [a 10
                  b (= :cat :dog) !all ($return 20) ;; !all does though
                  c 30]
              (+ a (or b 1337) c))))
  
  (is (= "crazy cat")
      (let [_ (= :cat :hat) !not (println "that can't be right")
            _ (#{:a} :a) !not (println "that can be though")
            b (#{:b} :a) !not ($return "crazy cat")
            b 1337]
        (println "the end" b)))
  
  (is (= "the end1337")
      (let [_ (= :cat :hat) !not (println "that can't be right")
            _ (#{:a} :a) !not (println "that can be though")
            b 1337]
        (str "the end" b)))
  
  (is (= "the end1337")
      (let [_ (= :cat :hat) !all (println "that can't be right")
            _ (#{:a} :a) !all (println "that can be though")
            b 1337]
        (str "the end" b))) 
  
  (is (= 30 (let [a (/ 10 0) !! 20
                  b (/ 10 0) !! 30]
              b)))
  
  (is (= "wathej") (let [a (/ 5 0) !! "wat"]
                     (str a "hej")))
  
  
  (is (= "screw this")
      (let [a (* 5 5)
            !! (println "wat")
            a (get nil :kalle) !! 1337
            _ (println "first a" a)
            a (/ 5 0)
            !! (do (println "!! Error" (-> $ Throwable->map :via first :message))
                   ($return "screw this"))
            _ (println "second a" a)
            b 20]
        (* a b)))
  
  
  
  
  
  
  ;; verify that there is a `vreset!`-call is generated      
  ;; when there is no $return    
  (is (seq (in-form (macroexpand
                     '(let [a 10
                            b (/ a 0) !! ($return 20)
                            c 30]
                        (+ a b c)))
                    #(= % `vreset!))))  
  
  ;; and vice versa
  (is (empty? (in-form
               (macroexpand '(let [a 10
                                   b (/ a 0)
                                   c 30]
                               (+ a b c)))
               #(= % `vreset!))))  
  

  
  )

(t/run-tests)
