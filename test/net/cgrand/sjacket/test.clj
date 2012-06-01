(ns net.cgrand.sjacket.test
  (:use [clojure.test :only [deftest is are]])
  (:require [net.cgrand.sjacket :as sj]))

(def input1
"(z (a ;comment
    b)) (4/2
         d))")

;; fractional offsets allow to uniquely identify a character without adding a
;; bias argument

(deftest rename1
  (is (= (sj/transform-src input1 1.5 (constantly 'zoo))
"(zoo (a ;comment
      b)) (4/2
           d))")))

(deftest wrap1
  (is (= (sj/transform-src input1 1.5 list)
"((z) (a ;comment
      b)) (4/2
           d))")))

(deftest destructuring-proof
  (is (= (sj/transform-src input1 3.5 (fn [[a b]] (list 'fn [] a b)))
"(z (fn [] a ;comment
    b)) (4/2
         d))")))