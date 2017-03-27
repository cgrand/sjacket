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

(defn has-no-unexpected-nodes? [input]
  (->> (tree-seq :tag :content input)
       (map :tag)
       (not-any? #(= :net.cgrand.parsley/unexpected %))))

(defn parses-without-unexpected-nodes? [input]
  (let [parsed-input (p/parser input)]
    (and parsed-input (has-no-unexpected-nodes? parsed-input))))

(deftest parse-characters
  (is (= [:char] (parsed-tags "\\newline")))
  (is (= [:char] (parsed-tags "\\space")))
  (is (= [:char] (parsed-tags "\\tab")))
  (is (= [:char] (parsed-tags "\\backspace")))
  (is (= [:char] (parsed-tags "\\formfeed")))
  (is (= [:char] (parsed-tags "\\return")))
  (is (= [:char] (parsed-tags "\\u0024")))
  (is (= [:char] (parsed-tags "\\o1")))
  (is (= [:char] (parsed-tags "\\o12")))
  (is (= [:char] (parsed-tags "\\o123")))
  (is (= [:char] (parsed-tags "\\t")))
  (is (= [:char] (parsed-tags "\\\"")))
  (is (= [:char] (parsed-tags "\\;")))
  (is (= [:char] (parsed-tags "\\@")))
  (is (= [:char] (parsed-tags "\\^")))
  (is (= [:char] (parsed-tags "\\ ")))
  (is (= [:char] (parsed-tags "\\\n")))
  (is (= [:char :whitespace] (parsed-tags "\\f ")))
  (is (= [:list] (parsed-tags "(comment \\a)")))
  (is (= [:vector] (parsed-tags "[\\a]")))
  (is (= [:char :comment] (parsed-tags "\\a; do something later"))))

(deftest parse-strings
  (is (= [:string] (parsed-tags "\" \"")))
  (is (= [:string] (parsed-tags "\"foo\"")))
  (is (= [:string] (parsed-tags "\"a word: \\\"foo\\\".\"")))
  (is (= [:string] (parsed-tags "\"foo\\tbar\"")))
  (is (= [:string] (parsed-tags "\"foo\\nbar\"")))
  (is (= [:string] (parsed-tags "\"foo\\r\\nbar\"")))
  (is (= [:list] (parsed-tags "(comment \"a\")")))
  (is (= [:vector] (parsed-tags "[\"a\"]")))
  (is (= [:string :comment] (parsed-tags "\"a\"; do something later"))))

(deftest parse-regexes
  (is (= [:regex] (parsed-tags "#\" \"")))
  (is (= [:regex] (parsed-tags "#\"foo\"")))
  (is (= [:regex] (parsed-tags "#\"a word: \\\"foo\\\".\"")))
  (is (= [:regex] (parsed-tags "#\"foo\\tbar\"")))
  (is (= [:regex] (parsed-tags "#\"foo\\nbar\"")))
  (is (= [:regex] (parsed-tags "#\"foo\\r\\nbar\"")))
  (is (= [:list] (parsed-tags "(comment #\"a\")")))
  (is (= [:vector] (parsed-tags "[#\"a\"]")))
  (is (= [:regex :comment] (parsed-tags "#\"a\"; do something later"))))

(deftest parse-long-strings
  (is (= [:string]
         (parsed-tags (apply str (concat [\"] (repeat 4000 \x) [\"]))))))

(deftest parse-long-regexes
  (is (= [:regex]
         (parsed-tags (apply str (concat [\# \"] (repeat 4000 \x) [\"]))))))

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
  (is (= [:symbol] (parsed-tags "clojure.core//")))
  (is (= [:symbol] (parsed-tags "-main"))))

(deftest numbers
  (is (= [:number] (parsed-tags "1")))
  (is (= [:number] (parsed-tags "12")))
  (is (= [:number] (parsed-tags "-1")))
  (is (= [:number] (parsed-tags "+1")))
  (is (= [:number] (parsed-tags "1e14")))
  (is (= [:number] (parsed-tags "15N")))
  (is (= [:number] (parsed-tags "1.5M")))
  (is (= [:number] (parsed-tags "1.03e14")))
  (is (= [:number] (parsed-tags "-1.03e14")))
  (is (= [:number] (parsed-tags "1.03e-14")))
  (is (= [:number] (parsed-tags "+1.03e+14")))
  (is (= [:number] (parsed-tags "4/3")))
  (is (= [:number] (parsed-tags "-4/3")))
  (is (= [:number] (parsed-tags "+4/3"))))

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

(defn- parsed-tag-and-content [input]
  (let [parse-tree (p/parser input)]
    (map (juxt :tag (comp first :content))
         (:content parse-tree))))

(deftest keywords
  (are [input] (= [[[:keyword ":"]] true]
                  [(parsed-tag-and-content input)
                   (parses-without-unexpected-nodes? input)])
       ":1"
       ":42"
       ":42/a"
       ":a/42"
       ":foo"
       ":clojure.core/map"
       ":core/map"
       ":li#last"
       ":xs:attribute"))

(deftest namespace-scoped-keywords
  (are [input] (and (= [[[:keyword "::"]] true]
                       [(parsed-tag-and-content input)
                        (parses-without-unexpected-nodes? input)]))
       "::100"
       "::foo"
       "::foo/bar"
       "::foo.bar/baz"))

(defn valid-file-parse? [classpath-to-file]
  (let [parsed-input (-> classpath-to-file
                         clojure.java.io/resource
                         slurp
                         p/parser)]
    (and parsed-input (has-no-unexpected-nodes? parsed-input))))

(deftest parse-own-source-code
  (is (valid-file-parse? "net/cgrand/sjacket/test.clj"))
  (is (valid-file-parse? "net/cgrand/sjacket.clj"))
  (is (valid-file-parse? "net/cgrand/sjacket/parser.clj")))

;; using clojure's source as an example (commenting some files to avoid tying
;;   to a particular clojure version above 1.3)
(deftest parse-clojure-source-code
  (is (valid-file-parse? "clojure/core.clj"))
  (is (valid-file-parse? "clojure/core_deftype.clj"))
  (is (valid-file-parse? "clojure/core_print.clj"))
  (is (valid-file-parse? "clojure/core_proxy.clj"))
  (is (valid-file-parse? "clojure/data.clj"))
  (is (valid-file-parse? "clojure/genclass.clj"))
  (is (valid-file-parse? "clojure/gvec.clj"))
  (is (valid-file-parse? "clojure/inspector.clj"))
  ;(is (valid-file-parse? "clojure/instant.clj"))
  (is (valid-file-parse? "clojure/main.clj"))
  (is (valid-file-parse? "clojure/parallel.clj"))
  (is (valid-file-parse? "clojure/pprint.clj"))
  (is (valid-file-parse? "clojure/reflect.clj"))
  (is (valid-file-parse? "clojure/repl.clj"))
  (is (valid-file-parse? "clojure/set.clj"))
  (is (valid-file-parse? "clojure/stacktrace.clj"))
  (is (valid-file-parse? "clojure/string.clj"))
  (is (valid-file-parse? "clojure/template.clj"))
  (is (valid-file-parse? "clojure/test.clj"))
  ;(is (valid-file-parse? "clojure/uuid.clj"))
  (is (valid-file-parse? "clojure/walk.clj"))
  (is (valid-file-parse? "clojure/xml.clj"))
  (is (valid-file-parse? "clojure/zip.clj"))
  (is (valid-file-parse? "clojure/java/browse.clj"))
  (is (valid-file-parse? "clojure/java/browse_ui.clj"))
  (is (valid-file-parse? "clojure/java/io.clj"))
  (is (valid-file-parse? "clojure/java/javadoc.clj"))
  (is (valid-file-parse? "clojure/java/shell.clj")))

