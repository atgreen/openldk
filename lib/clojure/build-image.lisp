;;; Build ./clojure, a Clojure image with clojure.core already loaded and
;;; its Java code compiled.  Run through `make clojure`, which downloads the
;;; Clojure jars first.  See docs/IMAGES.md.

(in-package :openldk)

;; Loading clojure.core is most of Clojure's startup, and it happens in the
;; runtime's static initializers, so keep the state the warmup builds
;; (:reset-classes nil); clojure.main then starts its REPL on the loaded
;; runtime.  The session works in a scratch namespace it removes at the end.
(defparameter *clojure-warmup-input* "
(ns warmup)
(+ 1 2)
(defn fact [n] (if (< n 2) 1 (*' n (fact (dec n)))))
(fact 30)
(defn make-adder [n] (fn [x] (+ x n)))
((make-adder 3) 4)
(map #(* % %) [1 2 3 4])
(reduce + (range 100))
(into {} (map (fn [k] [k (name k)]) [:a :b]))
(str \"hello\" \", \" 42)
(clojure.string/upper-case \"clojure\")
(let [a (atom 0)] (swap! a inc) @a)
(defrecord Point [x y])
(:x (->Point 3 4))
(try (throw (ex-info \"boom\" {:code 42})) (catch Exception e (ex-data e)))
(format \"%d-%s\" 7 \"x\")
(in-ns 'user)
(remove-ns 'warmup)
")

(dump-warm-image "clojure" "clojure.main"
                 :classpath "lib/clojure/clojure-1.12.4.jar:lib/clojure/spec.alpha-0.5.238.jar:lib/clojure/core.specs.alpha-0.4.74.jar"
                 :warmups (list (list :input *clojure-warmup-input*))
                 :timeout 3600
                 :reset-classes nil)
