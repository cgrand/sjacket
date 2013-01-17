(ns net.cgrand.sjacket.test
  (:use [clojure.test :only [deftest is are]])
  (:require [net.cgrand.sjacket :as sj]
            [net.cgrand.sjacket.parser :as p]))

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

(deftest strings
  (is (= (sj/transform-src "\"abc\"" 1 #(apply str (reverse %)))
         "\"cba\"")))

(def incomplete-string-input
"\"Hi,
")

(deftest incomplete-strings
  (is (= :net.cgrand.parsley/unfinished
         (:tag (p/parser incomplete-string-input))))
  (is (= incomplete-string-input
         (sj/str-pt (p/parser incomplete-string-input)))))

(defn parsed-tags [input]
  (let [parse-tree (p/parser input)]
    (map :tag (:content parse-tree))))

(deftest reader-literals
  (is (= [:reader-literal]
         (parsed-tags "#inst \"2012-09-13T01:00:36.439-00:00\"")))
  (is (= [:reader-literal]
         (parsed-tags "#
                      foo { 1 2, 3
                      4}"))))

(deftest symbols
  (is (= [:symbol] (parsed-tags "main")))
  (is (= [:symbol] (parsed-tags "foo/bar")))
  (is (= [:symbol] (parsed-tags "foo.core/is-bar-baz?")))
  (is (= [:symbol] (parsed-tags "-main"))))

(deftest numbers
  (is (= [:number] (parsed-tags "1")))
  (is (= [:number] (parsed-tags "12")))
  (is (= [:number] (parsed-tags "-1")))
  (is (= [:number] (parsed-tags "1e14")))
  (is (= [:number] (parsed-tags "15N")))
  (is (= [:number] (parsed-tags "1.5M")))
  (is (= [:number] (parsed-tags "1.03e14"))))

(deftest dispatch-macros
  (is (= [:meta] (parsed-tags "#^{:foo 1} hi"))) ; old style meta
  (is (= [:var] (parsed-tags "#'foo")))
  (is (= [:regex]
         (parsed-tags "#\"foo\\\"\\d+foo\""))) ; without escapes: #"foo\"\d+foo"
  (is (= [:fn] (parsed-tags "#(* % %)")))
  (is (= [:set] (parsed-tags "#{1 2 3}")))
  (is (= [:eval] (parsed-tags "#=(+ 1 2)")))
  (is (= [:comment]
         (parsed-tags "#!/usr/bin/env clojure")))
  (is (= [:unreadable :symbol :whitespace :symbol]
         (parsed-tags "#<Foo something>")))
  (is (= [:discard] (parsed-tags "#_foo"))))

