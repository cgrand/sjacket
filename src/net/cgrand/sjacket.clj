(ns net.cgrand.sjacket
  "Structural code transformations for the masses."
  (:require [net.cgrand.sjacket.parser :as p]
            [clojure.zip :as z]))

(defn- child [loc kw]
  (first (:content (some #(when (= kw (:tag %)) %) (z/children loc)))))

(defn- content-locs [loc]
  (take-while identity (iterate z/right (z/down loc))))

(defmulti ^:dynamic sexprs (comp :tag z/node) :default nil)

(defmethod sexprs nil [_] 
  nil)

(defmethod sexprs ::p/root [x] 
  (into [] (mapcat sexprs (content-locs x))))

(defmethod sexprs :list [x]
  [(into () (mapcat sexprs (reverse (content-locs x))))])

(defmethod sexprs :vector [x]
  [(into [] (mapcat sexprs (content-locs x)))])

(defmethod sexprs :set [x]
  [(into #{} (mapcat sexprs (content-locs x)))])

(defmethod sexprs :map [x]
  [(into {} (map vec (partition 2 (mapcat sexprs (content-locs x)))))])

(defmethod sexprs :fn [x] ;TODO
  [(list 'fn* [] (into () (mapcat sexprs (reverse (content-locs x)))))])

(defmethod sexprs :symbol [x]
  [(symbol (child x :ns) (child x :name))])

(defmethod sexprs :keyword [x]
  [(keyword (child x :ns) (child x :name))])

(defmethod sexprs :number [x]
  [(read-string (z/node (z/down x)))])

(defmethod sexprs :nil [_]
  [nil])

(defmethod sexprs :boolean [x]
  [({"true" true "false" false} (z/node (z/down x)))])

(defmethod sexprs :string [x]
  [(read-string (str \" (-> x z/down z/right z/node) \"))])

(defmethod sexprs :number [x]
  [(read-string (z/node (z/down x)))])

(defmethod sexprs :meta [x]
  (let [[m d] (mapcat sexprs (content-locs x))
        m (cond
            (keyword? m) {m true}
            (or (symbol? m) (string? m)) {:tag m}
            :else m)]
    [(vary-meta d merge m)]))

(defn to-sexprs [nodes]
  (let [log (atom [])
        sexprs-mm  sexprs] 
    [(binding [sexprs (fn [loc]
                       (let [r (sexprs-mm loc)]
                         (when r (swap! log conj [(first r) loc]))
                         r))]
      (sexprs (z/xml-zip {:tag ::p/root :content nodes})))
     @log]))

;; What must be remembered?
;; * composite nodes: original offset to correct indentation 

(defn- relocate
  "Takes a loc on a tree where edits where performed to current node or its 
   children or right siblings (or right siblings of ancestors) of old-loc and 
   returns a loc pointing to the edited version of the node originally pointed
   by old-loc."
  [loc old-loc]
  (reduce (fn [loc n]
            (nth (iterate z/right (z/down loc)) n)) (-> loc z/root z/xml-zip)
    (reverse (map (comp count z/lefts) (take-while z/up (iterate z/up old-loc))))))

(defn subedit
  "Delimits a loc transformation. f returns a loc."
  [loc f & args]
  (relocate (apply f loc args) loc))

(defmacro ^:private subedit-> [loc & edits]
  `(subedit ~loc
     (fn [loc#] (-> loc# ~@edits))))

(defn- deep-rightmost [loc]
  (if (when loc (z/branch? loc))
    (recur (z/rightmost (z/down loc)))
    loc))

(defn column 
  "Returns the number of characters to the left of the current loc."
  ([loc]
    (loop [column 0 loc loc]
      (let [loc (z/prev loc)
            node (and loc (z/node loc))
            node (when-not (= node "\n") node)]
        (if node
          (recur (if (string? node)
                   (+ column (count node))
                   column) loc)
          column))))
  ([loc inclusive]
    (if-let [loc (when inclusive (deep-rightmost loc))]
      (let [leaf (z/node loc)]
        (if (= "\n" leaf)
          0
          (+ (column loc) (count leaf))))
      (column loc))))


;; (z (a
;;     b)) (c
;;          d))
;; an append of two chars occur in z:
;; (zxx (a
;;       b)) (c
;;            d))
;; the change must be propagated
;; 

(defn- at-newline? [loc]
  (= :newline (:tag (z/node loc))))

(defn- spaces [n]
  (apply str (repeat n \space)))

(defn- adjust-whitespace 
  "loc is on a newline node, leading whitespace for the newt line is modified
   by delta spaces." 
  [loc delta]
  (if-let [rloc (z/right loc)]
    (cond
      (= :whitespace (:tag (z/node rloc)))
        (let [{[s] :content :as ws} (z/node rloc)
              n (min (- delta) (count s))]
          (cond
            (pos? delta)
              (z/replace rloc (update-in ws [:content 0] #(str (spaces delta) %)))
            (< n (count s))
              (z/replace rloc (update-in ws [:content 0] subs n))
            :else (z/remove rloc)))
        (pos? delta)
          (z/insert-right loc {:tag :whitespace :content [(spaces delta)]})
        :else loc)
    (if (pos? delta)
      (z/insert-right loc {:tag :whitespace :content [(spaces delta)]})
      loc)))

(defn- next-newline
  "Finds the next newline node, may return loc itself. Returns nil when no
   more newlines."
  [loc]
  (when-not (z/end? loc)
    (if (at-newline? loc)
      loc
      (recur (z/next loc)))))

(defn- right-or-up [loc]
  (when loc (or (z/right loc) (recur (z/up loc)))))

(defn- shift-node [node delta]
  (loop [loc (z/xml-zip node)]
    (if-let [loc (next-newline loc)]
      (recur (z/right (adjust-whitespace loc delta)))
      (z/root loc))))

(defn shift [loc delta]
  (if (or (zero? delta) (at-newline? loc))
    loc
    (let [loc (z/edit loc shift-node delta)]
      (if-let [loc (right-or-up loc)]
        (recur loc delta)
        loc))))

(defn shift-right [loc delta]
  (if-let [nloc (right-or-up loc)]
    (shift nloc delta)
    loc))

#_(
;; what I'd like to write:
(defn- unravel-first [expr]
  (if-let [[f x & xs] (and (seq? expr) (next expr) expr)] 
    (conj (unravel-first x) (if (or xs (seq? f)) (cons f xs) f))
    [expr]))

(defn thread-first [expr]
  (cons '-> (unravel-first expr)))
)

;; IDEA: convert from PT-zippers to sexpr-zippers so as to compute similarity

;;;;;;;
(defmulti append-pt-unknown (fn [loc expr ctx] (class expr)))

(defn- append [loc node]
  (if (and (z/branch? loc) (empty? (z/children loc)))
    (-> loc (z/insert-child node) z/down)
    (-> loc (z/insert-right node) z/right)))

(defn- original-loc [expr ctx]
  (or
    (some (fn [[e oloc]]
            (when (identical? e expr) oloc)) ctx)
    (some (fn [[e oloc]]
            ; TODO meta
            (when (= e expr) oloc)) ctx)))

(defn append-oloc [loc oloc]
  (let [delta (- (column loc true) (column oloc))]
    (append loc (z/node (subedit oloc shift delta)))))

;; TODO fix meta
(defn append-pt [loc expr ctx]
  (if-let [oloc (original-loc expr ctx)]
    (append-oloc loc oloc)
    (append-pt-unknown loc expr ctx)))

(declare append-pts)

(defn spliceable [x]
  (with-meta (sequence x) {::spliceable true}))

(defn ensure-spliceable [x]
  (if (::spliceable (meta x))
    x
    (with-meta (list x) {::spliceable true})))

(defn to-pt [expr ctx]
  (-> {:tag ::p/root :content []}
    z/xml-zip
    (append-pts (ensure-spliceable expr) ctx)
    z/root))

(defn- left-expr-loc [loc]
  (when-let [loc (z/left loc)]
    (if (p/space-nodes (:tag (z/node loc)))
      (recur loc)
      loc)))

(defn- spacer-locs 
  "If expr and nexpr where originally siblings (in this order) returns
   a sequence of locs on spaces between them (from left to roght).
   Returns nil if they were unrelated."
  [expr nexpr ctx]
  ;; TODO use u/when-let, avoid all the double computations
  (let [loc (original-loc expr ctx)
        nloc (original-loc nexpr ctx)]
    (when (and loc nloc (= loc (left-expr-loc nloc)))
      (take-while #(not= nloc %)
                  (iterate z/right (z/right loc))))))

(defn append-pts [loc exprs ctx]
  (if-let [[expr & exprs] (seq exprs)]
    (let [loc (append-pt loc expr ctx)]
      (if-let [[nexpr] exprs]
        (recur (if-let [slocs (spacer-locs expr nexpr ctx)]
                 (reduce 
                   (fn [loc sloc]
                     (append-oloc loc sloc)) 
                   loc slocs)
                 (append loc {:tag :whitespace :content [" "]}))
               exprs ctx)
        loc))
    loc))

(defmethod append-pt-unknown clojure.lang.Symbol [loc expr ctx]
  (append loc {:tag :symbol 
                     :content (if-let [ns (namespace expr)]
                                [{:tag :ns :content [ns]}
                                 "/"
                                 {:tag :name :content [(name expr)]}]
                                [{:tag :name :content [(name expr)]}])}))

(defmethod append-pt-unknown clojure.lang.Keyword [loc expr ctx]
  (append loc {:tag :keyword 
                     :content (if-let [ns (namespace expr)]
                                [":" {:tag :ns :content [ns]}
                                 "/"
                                 {:tag :name :content [(name expr)]}]
                                [":" {:tag :name :content [(name expr)]}])}))

(defmethod append-pt-unknown Number [loc expr ctx]
  (append loc {:tag :number 
                     :content [(pr-str expr)]}))

(defmethod append-pt-unknown String [loc expr ctx]
  (append loc {:tag :string 
                     :content ["\"" 
                               (let [s (pr-str expr)]
                                 (subs s 1 (dec (count s))))
                               "\""]}))

(defmethod append-pt-unknown Boolean [loc expr ctx]
  (append loc {:tag :boolean 
                     :content [(pr-str expr)]}))

(defmethod append-pt-unknown nil [loc expr ctx]
  (append loc {:tag :nil 
                     :content ["nil"]}))

(defmethod append-pt-unknown clojure.lang.ISeq [loc expr ctx]
  (-> loc
    (append {:tag :list :content ["(" ")"]})
    (subedit-> 
      z/down
      (append-pts expr ctx))))

(defmethod append-pt-unknown clojure.lang.IPersistentVector [loc expr ctx]
  (-> loc
    (append {:tag :list :content ["[" "]"]})
    (subedit-> 
      z/down
      (append-pts expr ctx))))

(defn str-pt [pt]
  (apply str (filter string? (tree-seq map? :content pt))))

#_(let [ptree (p/parser "(custom\n  indentation\n    ^:private in\n  action ;-)\n)")
      [[expr] ctx] (to-sexprs ptree)]
  (-> (list (list (list expr)))
    (to-pt ctx)
    str-pt
    println))

;; TODO: add bias
(defn loc-at [tree offset]
  (loop [loc (z/xml-zip tree) offset offset]
    (cond
      (z/end? loc) nil
      (z/branch? loc) (recur (z/next loc) offset)
      :else (let [n (count (z/node loc))
                  offset (- offset n)]
              (if (pos? offset)
                (recur (z/next loc) offset)
                [loc offset])))))

(defn expr-loc-at [tree offset]
  (when-let [[loc] (loc-at tree offset)]
    (let [loc (z/up loc)]
      (if (#{:name :namespace :whitespace} (:tag (z/node loc)))
        (z/up loc)
        loc))))

(defn- char-count [node]
  (if (string? node)
    (count node)
    (reduce + 0 (map char-count (:content node)))))

(defn offset-of
  ([loc]
    (offset-of loc false))
  ([loc inclusive]
    (loop [offset (if inclusive (char-count (z/node loc)) 0) loc loc] 
      (if-let [loc (z/left loc)]
        (recur (+ offset (char-count (z/node loc))) loc)
        (if-let [loc (z/up loc)]
          (recur offset loc)
          offset)))))

(defn transform-loc [loc f & args]
  (let [node (z/node loc)
        node (let [[[expr] ctx] (to-sexprs [node])
                   expr' (apply f expr args)]
               (to-pt expr' ctx))
        nloc (z/replace loc node)
        delta (- (column nloc true) (column loc true))]
    (subedit nloc shift-right delta)))

(defn transform [tree offset f & args]
  (z/root (apply transform-loc (expr-loc-at tree offset) f args)))

#_(let [ptree (p/parser "(custom\n  indentation\n    ^:private in\n  action ;-)\n)")
      ptree2 (transform ptree 0 list)]
  (println (str-pt ptree2)))

(defn transform-src  [src offset f & args]
  (let [ptree (p/parser src)
        ptree2 (apply transform ptree offset f args)]
    (str-pt ptree2)))



; bad tests
#_(=> (println (transform-src 
   "  (-> loc
   ;a
       (append {:tag :list :content [\"[\" \"]\"]})
       (subedit-> 
         z/down
         (append-pts expr ctx)))"
     3 (comp macroexpand-1 macroexpand-1)))
  (subedit-> (clojure.core/-> loc
;a
    (append {:tag :list :content ["[" "]"]})) z/down
      (append-pts expr ctx))
nil
)