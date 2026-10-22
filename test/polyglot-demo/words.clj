(ns words
  (:require [clojure.string :as str]))

(defn shout [s] (str (str/upper-case s) "!"))
