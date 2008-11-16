
//======
//(in-ns (quote clojure.core))
//---
(function __user_fn_398(){
return (clojure.core.in_ns.apply(null,[clojure.core.symbol("clojure.core")]))}).apply(null,[]);

//======
//(def list (. clojure.lang.PersistentList creator))
//---
(function __clojure_core_fn_404(){
return (clojure.JS.def(clojure.core,"list",clojure.JS.getOrRun(clojure.lang.PersistentList,"creator")))}).apply(null,[]);
// Skipping: (def cons (fn* cons [x seq] (. clojure.lang.RT (cons x seq))))
// Skipping: (def let (fn* let [& decl] (cons (quote let*) decl)))
// Skipping: (def loop (fn* loop [& decl] (cons (quote loop*) decl)))
// Skipping: (def fn (fn* fn [& decl] (cons (quote fn*) decl)))
// Skipping: (def first (fn first [coll] (. clojure.lang.RT (first coll))))
// Skipping: (def rest (fn rest [x] (. clojure.lang.RT (rest x))))

//======
//(def conj (fn conj ([coll x] (. clojure.lang.RT (conj coll x))) ([coll x & xs] (if xs (recur (conj coll x) (first xs) (rest xs)) (conj coll x)))))
//---
(function __clojure_core_fn_437(){
return (clojure.JS.def(clojure.core,"conj",clojure.JS.variadic(2,(function __clojure_core_fn_437_conj_439(coll_1,x_2){switch(arguments.length){
case 2:var conj_0=arguments.callee;
return (clojure.lang.RT.conj(coll_1,x_2))}
var _cnt,_rtn,conj_0=arguments.callee,xs_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((xs_3)?((_cnt=1,_rtn=[conj_0.apply(null,[coll_1,x_2]),clojure.core.first.apply(null,[xs_3]),clojure.core.rest.apply(null,[xs_3])],coll_1=_rtn[0],x_2=_rtn[1],xs_3=_rtn[2])):(conj_0.apply(null,[coll_1,x_2])))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(def second (fn second [x] (first (rest x))))
//---
(function __clojure_core_fn_442(){
return (clojure.JS.def(clojure.core,"second",(function __clojure_core_fn_442_second_444(x_1){
var second_0=arguments.callee;
return (clojure.core.first.apply(null,[clojure.core.rest.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def ffirst (fn ffirst [x] (first (first x))))
//---
(function __clojure_core_fn_447(){
return (clojure.JS.def(clojure.core,"ffirst",(function __clojure_core_fn_447_ffirst_449(x_1){
var ffirst_0=arguments.callee;
return (clojure.core.first.apply(null,[clojure.core.first.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def rfirst (fn rfirst [x] (rest (first x))))
//---
(function __clojure_core_fn_452(){
return (clojure.JS.def(clojure.core,"rfirst",(function __clojure_core_fn_452_rfirst_454(x_1){
var rfirst_0=arguments.callee;
return (clojure.core.rest.apply(null,[clojure.core.first.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def frest (fn frest [x] (first (rest x))))
//---
(function __clojure_core_fn_457(){
return (clojure.JS.def(clojure.core,"frest",(function __clojure_core_fn_457_frest_459(x_1){
var frest_0=arguments.callee;
return (clojure.core.first.apply(null,[clojure.core.rest.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def rrest (fn rrest [x] (rest (rest x))))
//---
(function __clojure_core_fn_462(){
return (clojure.JS.def(clojure.core,"rrest",(function __clojure_core_fn_462_rrest_464(x_1){
var rrest_0=arguments.callee;
return (clojure.core.rest.apply(null,[clojure.core.rest.apply(null,[x_1])]))})))}).apply(null,[]);
// Skipping: (def seq (fn seq [coll] (. clojure.lang.RT (seq coll))))
// Skipping: (def instance? (fn instance? [c x] (. c (isInstance x))))

//======
//(def seq? (fn seq? [x] (instance? clojure.lang.ISeq x)))
//---
(function __clojure_core_fn_477(){
return (clojure.JS.def(clojure.core,"seq_QMARK_",(function __clojure_core_fn_477_seq_QMARK_479(x_1){
var seq_QMARK__0=arguments.callee;
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.ISeq,x_1]))})))}).apply(null,[]);
// Skipping: (def string? (fn string? [x] (instance? String x)))

//======
//(def map? (fn map? [x] (instance? clojure.lang.IPersistentMap x)))
//---
(function __clojure_core_fn_487(){
return (clojure.JS.def(clojure.core,"map_QMARK_",(function __clojure_core_fn_487_map_QMARK_489(x_1){
var map_QMARK__0=arguments.callee;
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.IPersistentMap,x_1]))})))}).apply(null,[]);

//======
//(def vector? (fn vector? [x] (instance? clojure.lang.IPersistentVector x)))
//---
(function __clojure_core_fn_492(){
return (clojure.JS.def(clojure.core,"vector_QMARK_",(function __clojure_core_fn_492_vector_QMARK_494(x_1){
var vector_QMARK__0=arguments.callee;
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.IPersistentVector,x_1]))})))}).apply(null,[]);

//======
//(def sigs (fn [fdecl] (if (seq? (first fdecl)) (loop [ret [] fdecl fdecl] (if fdecl (recur (conj ret (first (first fdecl))) (rest fdecl)) (seq ret))) (list (first fdecl)))))
//---
(function __clojure_core_fn_497(){
return (clojure.JS.def(clojure.core,"sigs",(function __clojure_core_fn_497_sigs_499(fdecl_1){
var fdecl_3,ret_2;
return (((clojure.core.seq_QMARK_.apply(null,[clojure.core.first.apply(null,[fdecl_1])]))?(((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentVector.EMPTY),
(fdecl_3=fdecl_1);do{_cnt=0;
_rtn=((fdecl_3)?((_cnt=1,_rtn=[clojure.core.conj.apply(null,[ret_2,clojure.core.first.apply(null,[clojure.core.first.apply(null,[fdecl_3])])]),clojure.core.rest.apply(null,[fdecl_3])],ret_2=_rtn[0],fdecl_3=_rtn[1])):(clojure.core.seq.apply(null,[ret_2])))}while(_cnt);return _rtn;})())):(clojure.core.list.apply(null,[clojure.core.first.apply(null,[fdecl_1])]))))})))}).apply(null,[]);
// Skipping: (def assoc (fn assoc ([map key val] (. clojure.lang.RT (assoc map key val))) ([map key val & kvs] (let [ret (assoc map key val)] (if kvs (recur ret (first kvs) (second kvs) (rrest kvs)) ret)))))

//======
//(def meta (fn meta [x] (if (instance? clojure.lang.IObj x) (. x (meta)))))
//---
(function __clojure_core_fn_508(){
return (clojure.JS.def(clojure.core,"meta",(function __clojure_core_fn_508_meta_510(x_1){
var meta_0=arguments.callee;
return (((clojure.core.instance_QMARK_.apply(null,[clojure.lang.IObj,x_1]))?((x_1).meta()):(null)))})))}).apply(null,[]);

//======
//(def with-meta (fn with-meta [x m] (. x (withMeta m))))
//---
(function __clojure_core_fn_513(){
return (clojure.JS.def(clojure.core,"with_meta",(function __clojure_core_fn_513_with_meta_515(x_1,m_2){
var with_meta_0=arguments.callee;
return ((x_1).withMeta(m_2))})))}).apply(null,[]);

//======
//(def last (fn last [s] (if (rest s) (recur (rest s)) (first s))))
//---
(function __clojure_core_fn_518(){
return (clojure.JS.def(clojure.core,"last",(function __clojure_core_fn_518_last_520(s_1){
var _cnt,_rtn,last_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.core.rest.apply(null,[s_1]))?((_cnt=1,_rtn=[clojure.core.rest.apply(null,[s_1])],s_1=_rtn[0])):(clojure.core.first.apply(null,[s_1])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def butlast (fn butlast [s] (loop [ret [] s s] (if (rest s) (recur (conj ret (first s)) (rest s)) (seq ret)))))
//---
(function __clojure_core_fn_523(){
return (clojure.JS.def(clojure.core,"butlast",(function __clojure_core_fn_523_butlast_525(s_1){
var ret_2,s_3,butlast_0=arguments.callee;
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentVector.EMPTY),
(s_3=s_1);do{_cnt=0;
_rtn=((clojure.core.rest.apply(null,[s_3]))?((_cnt=1,_rtn=[clojure.core.conj.apply(null,[ret_2,clojure.core.first.apply(null,[s_3])]),clojure.core.rest.apply(null,[s_3])],ret_2=_rtn[0],s_3=_rtn[1])):(clojure.core.seq.apply(null,[ret_2])))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);
// Skipping: (def defn (fn defn [name & fdecl] (let [m (if (string? (first fdecl)) {:doc (first fdecl)} {}) fdecl (if (string? (first fdecl)) (rest fdecl) fdecl) m (if (map? (first fdecl)) (conj m (first fdecl)) m) fdecl (if (map? (first fdecl)) (rest fdecl) fdecl) fdecl (if (vector? (first fdecl)) (list fdecl) fdecl) m (if (map? (last fdecl)) (conj m (last fdecl)) m) fdecl (if (map? (last fdecl)) (butlast fdecl) fdecl) m (conj {:arglists (list (quote quote) (sigs fdecl))} m)] (list (quote def) (with-meta name (conj (if (meta name) (meta name) {}) m)) (cons (quote clojure.core/fn) fdecl)))))
// Skipping: (. (var defn) (setMacro))

//======
//(defn cast "Throws a ClassCastException if x is not a c, else returns x." [c x] (. c (cast x)))
//---
(function __clojure_core_fn_536(){
return (clojure.JS.def(clojure.core,"cast",(function __clojure_core_fn_536_cast_538(c_1,x_2){
return ((c_1).cast(x_2))})))}).apply(null,[]);
// Skipping: (defn to-array "Returns an array of Objects containing the contents of coll, which\n  can be any Collection.  Maps to java.util.Collection.toArray()." [coll] (. clojure.lang.RT (toArray coll)))

//======
//(defn vector "Creates a new vector containing the args." ([] []) ([& args] (. clojure.lang.LazilyPersistentVector (create args))))
//---
(function __clojure_core_fn_548(){
return (clojure.JS.def(clojure.core,"vector",clojure.JS.variadic(0,(function __clojure_core_fn_548_vector_550(){switch(arguments.length){
case 0:return (clojure.lang.PersistentVector.EMPTY)}
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.LazilyPersistentVector.create(args_1))}))))}).apply(null,[]);

//======
//(defn vec "Creates a new vector containing the contents of coll." ([coll] (. clojure.lang.LazilyPersistentVector (createOwning (to-array coll)))))
//---
(function __clojure_core_fn_555(){
return (clojure.JS.def(clojure.core,"vec",(function __clojure_core_fn_555_vec_557(coll_1){
return (clojure.lang.LazilyPersistentVector.createOwning(clojure.core.to_array.apply(null,[coll_1])))})))}).apply(null,[]);
// Skipping: (defn hash-map "keyval => key val\n  Returns a new hash map with supplied mappings." ([] {}) ([& keyvals] (. clojure.lang.PersistentHashMap (create keyvals))))

//======
//(defn hash-set "Returns a new hash set with supplied keys." ([] #{}) ([& keys] (. clojure.lang.PersistentHashSet (create keys))))
//---
(function __clojure_core_fn_568(){
return (clojure.JS.def(clojure.core,"hash_set",clojure.JS.variadic(0,(function __clojure_core_fn_568_hash_set_570(){switch(arguments.length){
case 0:return (clojure.lang.PersistentHashSet.EMPTY)}
var keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentHashSet.create(keys_1))}))))}).apply(null,[]);

//======
//(defn sorted-map "keyval => key val\n  Returns a new sorted map with supplied mappings." ([& keyvals] (. clojure.lang.PersistentTreeMap (create keyvals))))
//---
(function __clojure_core_fn_575(){
return (clojure.JS.def(clojure.core,"sorted_map",clojure.JS.variadic(0,(function __clojure_core_fn_575_sorted_map_577(){
var keyvals_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentTreeMap.create(keyvals_1))}))))}).apply(null,[]);

//======
//(defn sorted-set "Returns a new sorted set with supplied keys." ([& keys] (. clojure.lang.PersistentTreeSet (create keys))))
//---
(function __clojure_core_fn_581(){
return (clojure.JS.def(clojure.core,"sorted_set",clojure.JS.variadic(0,(function __clojure_core_fn_581_sorted_set_583(){
var keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentTreeSet.create(keys_1))}))))}).apply(null,[]);

//======
//(defn sorted-map-by "keyval => key val\n  Returns a new sorted map with supplied mappings, using the supplied comparator." ([comparator & keyvals] (. clojure.lang.PersistentTreeMap (create comparator keyvals))))
//---
(function __clojure_core_fn_587(){
return (clojure.JS.def(clojure.core,"sorted_map_by",clojure.JS.variadic(1,(function __clojure_core_fn_587_sorted_map_by_589(comparator_1){
var keyvals_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentTreeMap.create(comparator_1,keyvals_2))}))))}).apply(null,[]);
// Skipping: (def defmacro (fn [name & args] (list (quote do) (cons (quote clojure.core/defn) (cons name args)) (list (quote .) (list (quote var) name) (quote (setMacro))))))
// Skipping: (. (var defmacro) (setMacro))
// Skipping: (defmacro when "Evaluates test. If logical true, evaluates body in an implicit do." [test & body] (list (quote if) test (cons (quote do) body)))
// Skipping: (defmacro when-not "Evaluates test. If logical false, evaluates body in an implicit do." [test & body] (list (quote if) test nil (cons (quote do) body)))

//======
//(defn nil? "Returns true if x is nil, false otherwise." {:tag Boolean} [x] (identical? x nil))
//---
(function __clojure_core_fn_620(){
return (clojure.JS.def(clojure.core,"nil_QMARK_",(function __clojure_core_fn_620_nil_QMARK_622(x_1){
return (clojure.core.identical_QMARK_.apply(null,[x_1,null]))})))}).apply(null,[]);

//======
//(defn false? "Returns true if x is the value false, false otherwise." {:tag Boolean} [x] (identical? x false))
//---
(function __clojure_core_fn_626(){
return (clojure.JS.def(clojure.core,"false_QMARK_",(function __clojure_core_fn_626_false_QMARK_628(x_1){
return (clojure.core.identical_QMARK_.apply(null,[x_1,false]))})))}).apply(null,[]);

//======
//(defn true? "Returns true if x is the value true, false otherwise." {:tag Boolean} [x] (identical? x true))
//---
(function __clojure_core_fn_632(){
return (clojure.JS.def(clojure.core,"true_QMARK_",(function __clojure_core_fn_632_true_QMARK_634(x_1){
return (clojure.core.identical_QMARK_.apply(null,[x_1,true]))})))}).apply(null,[]);

//======
//(defn not "Returns true if x is logical false, false otherwise." {:tag Boolean} [x] (if x false true))
//---
(function __clojure_core_fn_638(){
return (clojure.JS.def(clojure.core,"not",(function __clojure_core_fn_638_not_640(x_1){
return (((x_1)?(false):(true)))})))}).apply(null,[]);

//======
//(defn str "With no args, returns the empty string. With one arg x, returns\n  x.toString().  (str nil) returns the empty string. With more than\n  one arg, returns the concatenation of the str values of the args." {:tag String} ([] "") ([x] (if (nil? x) "" (. x (toString)))) ([x & ys] ((fn [sb more] (if more (recur (. sb (append (str (first more)))) (rest more)) (str sb))) (clojure.lang.RT/makeStringBuilder (str x)) ys)))
//---
(function __clojure_core_fn_644(){
return (clojure.JS.def(clojure.core,"str",clojure.JS.variadic(1,(function __clojure_core_fn_644_str_646(x_1){switch(arguments.length){
case 1:return (((clojure.core.nil_QMARK_.apply(null,[x_1]))?(""):((x_1).toString())))
case 0:return ("")}
var ys_2=clojure.JS.rest_args(this,arguments,1);
return ((function __clojure_core_fn_644_str_646_fn_650(sb_1,more_2){
var _cnt,_rtn;
do{_cnt=0;_rtn=((more_2)?((_cnt=1,_rtn=[(sb_1).append(clojure.core.str.apply(null,[clojure.core.first.apply(null,[more_2])])),clojure.core.rest.apply(null,[more_2])],sb_1=_rtn[0],more_2=_rtn[1])):(clojure.core.str.apply(null,[sb_1])))
}while(_cnt);return _rtn;}).apply(null,[clojure.lang.RT.makeStringBuilder(clojure.core.str.apply(null,[x_1])),ys_2]))}))))}).apply(null,[]);

//======
//(defn symbol? "Return true if x is a Symbol" [x] (instance? clojure.lang.Symbol x))
//---
(function __clojure_core_fn_655(){
return (clojure.JS.def(clojure.core,"symbol_QMARK_",(function __clojure_core_fn_655_symbol_QMARK_657(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Symbol,x_1]))})))}).apply(null,[]);

//======
//(defn keyword? "Return true if x is a Keyword" [x] (instance? clojure.lang.Keyword x))
//---
(function __clojure_core_fn_661(){
return (clojure.JS.def(clojure.core,"keyword_QMARK_",(function __clojure_core_fn_661_keyword_QMARK_663(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Keyword,x_1]))})))}).apply(null,[]);
// Skipping: (defn symbol "Returns a Symbol with the given namespace and name." ([name] (if (symbol? name) name (. clojure.lang.Symbol (intern name)))) ([ns name] (. clojure.lang.Symbol (intern ns name))))
// Skipping: (defn keyword "Returns a Keyword with the given namespace and name.  Do not use :\n  in the keyword strings, it will be added automatically." ([name] (if (keyword? name) name (. clojure.lang.Keyword (intern nil name)))) ([ns name] (. clojure.lang.Keyword (intern ns name))))

//======
//(defn gensym "Returns a new symbol with a unique name. If a prefix string is\n  supplied, the name is prefix# where # is some unique number. If\n  prefix is not supplied, the prefix is 'G'." ([] (gensym "G__")) ([prefix-string] (. clojure.lang.Symbol (intern (str prefix-string (str (. clojure.lang.RT (nextID))))))))
//---
(function __clojure_core_fn_681(){
return (clojure.JS.def(clojure.core,"gensym",(function __clojure_core_fn_681_gensym_683(prefix_string_1){switch(arguments.length){
case 0:return (clojure.core.gensym.apply(null,["G__"]))}
return (clojure.lang.Symbol.intern(clojure.core.str.apply(null,[prefix_string_1,clojure.core.str.apply(null,[clojure.lang.RT.nextID()])])))})))}).apply(null,[]);
// Skipping: (defmacro cond "Takes a set of test/expr pairs. It evaluates each test one at a\n  time.  If a test returns logical true, cond evaluates and returns\n  the value of the corresponding expr and doesn't evaluate any of the\n  other tests or exprs. (cond) returns nil." [& clauses] (when clauses (list (quote if) (first clauses) (second clauses) (cons (quote cond) (rest (rest clauses))))))

//======
//(defn spread {:private true} [arglist] (cond (nil? arglist) nil (nil? (rest arglist)) (seq (first arglist)) :else (cons (first arglist) (spread (rest arglist)))))
//---
(function __clojure_core_fn_697(){
return (clojure.JS.def(clojure.core,"spread",(function __clojure_core_fn_697_spread_699(arglist_1){
return (((clojure.core.nil_QMARK_.apply(null,[arglist_1]))?(null):(((clojure.core.nil_QMARK_.apply(null,[clojure.core.rest.apply(null,[arglist_1])]))?(clojure.core.seq.apply(null,[clojure.core.first.apply(null,[arglist_1])])):(((clojure.core.keyword("","else"))?(clojure.core.cons.apply(null,[clojure.core.first.apply(null,[arglist_1]),clojure.core.spread.apply(null,[clojure.core.rest.apply(null,[arglist_1])])])):(null)))))))})))}).apply(null,[]);
// Skipping: (defn apply "Applies fn f to the argument list formed by prepending args to argseq." {:arglists (quote ([f args* argseq]))} [f & args] (. f (applyTo (spread args))))

//======
//(defn list* "Creates a new list containing the item prepended to more." [item & more] (spread (cons item more)))
//---
(function __clojure_core_fn_709(){
return (clojure.JS.def(clojure.core,"list_STAR_",clojure.JS.variadic(1,(function __clojure_core_fn_709_list_STAR_711(item_1){
var more_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.core.spread.apply(null,[clojure.core.cons.apply(null,[item_1,more_2])]))}))))}).apply(null,[]);
// Skipping: (defmacro delay "Takes a body of expressions and yields a Delay object than will\n  invoke the body only the first time it is forced (with force), and\n  will cache the result and return it on all subsequent force calls" [& body] (list (quote new) (quote clojure.lang.Delay) (list* (quote clojure.core/fn) [] body)))

//======
//(defn delay? "returns true if x is a Delay created with delay" [x] (instance? clojure.lang.Delay x))
//---
(function __clojure_core_fn_724(){
return (clojure.JS.def(clojure.core,"delay_QMARK_",(function __clojure_core_fn_724_delay_QMARK_726(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Delay,x_1]))})))}).apply(null,[]);

//======
//(defn force "If x is a Delay, returns the (possibly cached) value of its expression, else returns x" [x] (. clojure.lang.Delay (force x)))
//---
(function __clojure_core_fn_730(){
return (clojure.JS.def(clojure.core,"force",(function __clojure_core_fn_730_force_732(x_1){
return (clojure.lang.Delay.force(x_1))})))}).apply(null,[]);

//======
//(defn fnseq "Returns a seq object whose first is first and whose rest is the\n  value produced by calling restfn with no arguments. restfn will be\n  called at most once per step in the sequence, e.g. calling rest\n  repeatedly on the head of the seq calls restfn once - the value it\n  yields is cached." [first restfn] (new clojure.lang.FnSeq first restfn))
//---
(function __clojure_core_fn_736(){
return (clojure.JS.def(clojure.core,"fnseq",(function __clojure_core_fn_736_fnseq_738(first_1,restfn_2){
return ((new clojure.lang.FnSeq(first_1,restfn_2)))})))}).apply(null,[]);
// Skipping: (defmacro lazy-cons "Expands to code which produces a seq object whose first is\n  first-expr and whose rest is rest-expr, neither of which is\n  evaluated until first/rest is called. Each expr will be evaluated at most\n  once per step in the sequence, e.g. calling first/rest repeatedly on the\n  same node of the seq evaluates first/rest-expr once - the values they yield are\n  cached." [first-expr & rest-expr] (list (quote new) (quote clojure.lang.LazyCons) (list (quote clojure.core/fn) (list [] first-expr) (list* [(gensym)] rest-expr))))

//======
//(defn cache-seq "Given a seq s, returns a lazy seq that will touch each element of s\n  at most once, caching the results." [s] (when s (clojure.lang.CachedSeq. s)))
//---
(function __clojure_core_fn_751(){
return (clojure.JS.def(clojure.core,"cache_seq",(function __clojure_core_fn_751_cache_seq_753(s_1){
return (((s_1)?((new clojure.lang.CachedSeq(s_1))):(null)))})))}).apply(null,[]);

//======
//(defn concat "Returns a lazy seq representing the concatenation of\tthe elements in the supplied colls." ([] nil) ([x] (seq x)) ([x y] (if (seq x) (lazy-cons (first x) (concat (rest x) y)) (seq y))) ([x y & zs] (let [cat (fn cat [xys zs] (if (seq xys) (lazy-cons (first xys) (cat (rest xys) zs)) (when zs (recur (first zs) (rest zs)))))] (cat (concat x y) zs))))
//---
(function __clojure_core_fn_757(){
return (clojure.JS.def(clojure.core,"concat",clojure.JS.variadic(2,(function __clojure_core_fn_757_concat_759(x_1,y_2){switch(arguments.length){
case 1:return (clojure.core.seq.apply(null,[x_1]))
case 2:return (((clojure.core.seq.apply(null,[x_1]))?((new clojure.lang.LazyCons((function __clojure_core_fn_757_concat_759_fn_764(G__763_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[x_1]))}
return (clojure.core.concat.apply(null,[clojure.core.rest.apply(null,[x_1]),y_2]))})))):(clojure.core.seq.apply(null,[y_2]))))
case 0:return (null)}
var cat_4,zs_3=clojure.JS.rest_args(this,arguments,2);
return (((cat_4=(function __clojure_core_fn_757_concat_759_cat_769(xys_1,zs_2){
var _cnt,_rtn,cat_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[xys_1]))?((new clojure.lang.LazyCons((function __clojure_core_fn_757_concat_759_cat_769_fn_771(G__770_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[xys_1]))}
return (cat_0.apply(null,[clojure.core.rest.apply(null,[xys_1]),zs_2]))})))):(((zs_2)?((_cnt=1,_rtn=[clojure.core.first.apply(null,[zs_2]),clojure.core.rest.apply(null,[zs_2])],xys_1=_rtn[0],zs_2=_rtn[1])):(null))))
}while(_cnt);return _rtn;})),
cat_4.apply(null,[clojure.core.concat.apply(null,[x_1,y_2]),zs_3])))}))))}).apply(null,[]);

//======
//(defn = "Equality. Returns true if x equals y, false if not. Same as\n  Java x.equals(y) except it also works for nil, and compares\n  numbers in a type-independent manner.  Clojure's immutable data\n  structures define equals() (and thus =) as a value, not an identity,\n  comparison." {:inline-arities #{2}, :tag Boolean, :inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Util)) (clojure.core/list (quote clojure.core/equal)) (clojure.core/list x) (clojure.core/list y)))} ([x] true) ([x y] (. clojure.lang.Util (equal x y))) ([x y & more] (if (= x y) (if (rest more) (recur y (first more) (rest more)) (= y (first more))) false)))
//---
(function __clojure_core_fn_778(){
return (clojure.JS.def(clojure.core,"_EQ_",clojure.JS.variadic(2,(function __clojure_core_fn_778_EQ_783(x_1,y_2){switch(arguments.length){
case 1:return (true)
case 2:return (clojure.lang.Util.equal(x_1,y_2))}
var _cnt,_rtn,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Util.equal(x_1,y_2))?(((clojure.core.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.core.first.apply(null,[more_3]),clojure.core.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Util.equal(y_2,clojure.core.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn not= "Same as (not (= obj1 obj2))" {:tag Boolean} ([x] false) ([x y] (not (= x y))) ([x y & more] (not (apply = x y more))))
//---
(function __clojure_core_fn_789(){
return (clojure.JS.def(clojure.core,"not_EQ_",clojure.JS.variadic(2,(function __clojure_core_fn_789_not_EQ_791(x_1,y_2){switch(arguments.length){
case 2:return (clojure.core.not.apply(null,[clojure.lang.Util.equal(x_1,y_2)]))
case 1:return (false)}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.not.apply(null,[clojure.core.apply.apply(null,[clojure.core._EQ_,x_1,y_2,more_3])]))}))))}).apply(null,[]);

//======
//(defn compare "Comparator. Returns 0 if x equals y, -1 if x is logically 'less\n  than' y, else 1. Same as Java x.compareTo(y) except it also works\n  for nil, and compares numbers in a type-independent manner. x must\n  implement Comparable" {:tag Integer, :inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Util)) (clojure.core/list (quote clojure.core/compare)) (clojure.core/list x) (clojure.core/list y)))} [x y] (. clojure.lang.Util (compare x y)))
//---
(function __clojure_core_fn_797(){
return (clojure.JS.def(clojure.core,"compare",(function __clojure_core_fn_797_compare_802(x_1,y_2){
return (clojure.lang.Util.compare(x_1,y_2))})))}).apply(null,[]);
// Skipping: (defmacro and "Evaluates exprs one at a time, from left to right. If a form\n  returns logical false (nil or false), and returns that value and\n  doesn't evaluate any of the other expressions, otherwise it returns\n  the value of the last expr. (and) returns true." ([] true) ([x] x) ([x & rest] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote and__806)) (clojure.core/list x)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (quote and__806)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/and)) rest)) (clojure.core/list (quote and__806)))))))
// Skipping: (defmacro or "Evaluates exprs one at a time, from left to right. If a form\n  returns a logical true value, or returns that value and doesn't\n  evaluate any of the other expressions, otherwise it returns the\n  value of the last expression. (or) returns nil." ([] nil) ([x] x) ([x & rest] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote or__820)) (clojure.core/list x)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (quote or__820)) (clojure.core/list (quote or__820)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/or)) rest)))))))

//======
//(defn reduce "f should be a function of 2 arguments. If val is not supplied,\n  returns the result of applying f to the first 2 items in coll, then\n  applying f to that result and the 3rd item, etc. If coll contains no\n  items, f must accept no arguments as well, and reduce returns the\n  result of calling f with no arguments.  If coll has only 1 item, it\n  is returned and f is not called.  If val is supplied, returns the\n  result of applying f to val and the first item in coll, then\n  applying f to that result and the 2nd item, etc. If coll contains no\n  items, returns val and f is not called." ([f coll] (let [s (seq coll)] (if s (if (instance? clojure.lang.IReduce s) (. s (reduce f)) (reduce f (first s) (rest s))) (f)))) ([f val coll] (let [s (seq coll)] (if (instance? clojure.lang.IReduce s) (. s (reduce f val)) ((fn [f val s] (if s (recur f (f val (first s)) (rest s)) val)) f val s)))))
//---
(function __clojure_core_fn_834(){
return (clojure.JS.def(clojure.core,"reduce",(function __clojure_core_fn_834_reduce_836(f_1,val_2,coll_3){switch(arguments.length){
case 2:var s_3,coll_2=arguments[1];
return (((s_3=clojure.core.seq.apply(null,[coll_2])),
((s_3)?(((clojure.core.instance_QMARK_.apply(null,[clojure.lang.IReduce,s_3]))?((s_3).reduce(f_1)):(clojure.core.reduce.apply(null,[f_1,clojure.core.first.apply(null,[s_3]),clojure.core.rest.apply(null,[s_3])])))):(f_1.apply(null,[])))))}
var s_4;
return (((s_4=clojure.core.seq.apply(null,[coll_3])),
((clojure.core.instance_QMARK_.apply(null,[clojure.lang.IReduce,s_4]))?((s_4).reduce(f_1,val_2)):((function __clojure_core_fn_834_reduce_836_fn_839(f_1,val_2,s_3){
var _cnt,_rtn;
do{_cnt=0;_rtn=((s_3)?((_cnt=1,_rtn=[f_1,f_1.apply(null,[val_2,clojure.core.first.apply(null,[s_3])]),clojure.core.rest.apply(null,[s_3])],f_1=_rtn[0],val_2=_rtn[1],s_3=_rtn[2])):(val_2))
}while(_cnt);return _rtn;}).apply(null,[f_1,val_2,s_4])))))})))}).apply(null,[]);

//======
//(defn reverse "Returns a seq of the items in coll in reverse order. Not lazy." [coll] (reduce conj nil coll))
//---
(function __clojure_core_fn_844(){
return (clojure.JS.def(clojure.core,"reverse",(function __clojure_core_fn_844_reverse_846(coll_1){
return (clojure.core.reduce.apply(null,[clojure.core.conj,null,coll_1]))})))}).apply(null,[]);

//======
//(defn + "Returns the sum of nums. (+) returns 0." {:inline-arities #{2}, :inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/add)) (clojure.core/list x) (clojure.core/list y)))))} ([] 0) ([x] (clojure.lang.RT/numberCast x)) ([x y] (. clojure.lang.Numbers (add x y))) ([x y & more] (reduce + (+ x y) more)))
//---
(function __clojure_core_fn_850(){
return (clojure.JS.def(clojure.core,"_PLUS_",clojure.JS.variadic(2,(function __clojure_core_fn_850_PLUS_855(x_1,y_2){switch(arguments.length){
case 1:return (clojure.lang.RT.numberCast(x_1))
case 0:return ((0))
case 2:return (clojure.lang.Numbers.add(x_1,y_2))}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.reduce.apply(null,[clojure.core._PLUS_,clojure.lang.Numbers.add(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn * "Returns the product of nums. (*) returns 1." {:inline-arities #{2}, :inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/multiply)) (clojure.core/list x) (clojure.core/list y)))))} ([] 1) ([x] (clojure.lang.RT/numberCast x)) ([x y] (. clojure.lang.Numbers (multiply x y))) ([x y & more] (reduce * (* x y) more)))
//---
(function __clojure_core_fn_862(){
return (clojure.JS.def(clojure.core,"_STAR_",clojure.JS.variadic(2,(function __clojure_core_fn_862_STAR_867(x_1,y_2){switch(arguments.length){
case 0:return ((1))
case 1:return (clojure.lang.RT.numberCast(x_1))
case 2:return (clojure.lang.Numbers.multiply(x_1,y_2))}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.reduce.apply(null,[clojure.core._STAR_,clojure.lang.Numbers.multiply(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn / "If no denominators are supplied, returns 1/numerator,\n  else returns numerator divided by all of the denominators." {:inline-arities #{2}, :inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/divide)) (clojure.core/list x) (clojure.core/list y)))))} ([x] (/ 1 x)) ([x y] (. clojure.lang.Numbers (divide x y))) ([x y & more] (reduce / (/ x y) more)))
//---
(function __clojure_core_fn_874(){
return (clojure.JS.def(clojure.core,"_SLASH_",clojure.JS.variadic(2,(function __clojure_core_fn_874_SLASH_879(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.divide(x_1,y_2))
case 1:return (clojure.lang.Numbers.divide((1),x_1))}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.reduce.apply(null,[clojure.core._SLASH_,clojure.lang.Numbers.divide(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn - "If no ys are supplied, returns the negation of x, else subtracts\n  the ys from x and returns the result." {:inline-arities #{1 2}, :inline (fn [& args] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/minus)) args))))} ([x] (. clojure.lang.Numbers (minus x))) ([x y] (. clojure.lang.Numbers (minus x y))) ([x y & more] (reduce - (- x y) more)))
//---
(function __clojure_core_fn_885(){
return (clojure.JS.def(clojure.core,"_",clojure.JS.variadic(2,(function __clojure_core_fn_885_890(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.minus(x_1,y_2))
case 1:return (clojure.lang.Numbers.minus(x_1))}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.reduce.apply(null,[clojure.core._,clojure.lang.Numbers.minus(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn < "Returns non-nil if nums are in monotonically increasing order,\n  otherwise false." {:inline-arities #{2}, :inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/lt)) (clojure.core/list x) (clojure.core/list y)))))} ([x] true) ([x y] (. clojure.lang.Numbers (lt x y))) ([x y & more] (if (< x y) (if (rest more) (recur y (first more) (rest more)) (< y (first more))) false)))
//---
(function __clojure_core_fn_896(){
return (clojure.JS.def(clojure.core,"_LT_",clojure.JS.variadic(2,(function __clojure_core_fn_896_LT_901(x_1,y_2){switch(arguments.length){
case 1:return (true)
case 2:return (clojure.lang.Numbers.lt(x_1,y_2))}
var _cnt,_rtn,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.lt(x_1,y_2))?(((clojure.core.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.core.first.apply(null,[more_3]),clojure.core.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.lt(y_2,clojure.core.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn <= "Returns non-nil if nums are in monotonically non-decreasing order,\n  otherwise false." {:inline-arities #{2}, :inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/lte)) (clojure.core/list x) (clojure.core/list y)))))} ([x] true) ([x y] (. clojure.lang.Numbers (lte x y))) ([x y & more] (if (<= x y) (if (rest more) (recur y (first more) (rest more)) (<= y (first more))) false)))
//---
(function __clojure_core_fn_907(){
return (clojure.JS.def(clojure.core,"_LT__EQ_",clojure.JS.variadic(2,(function __clojure_core_fn_907_LT_EQ_912(x_1,y_2){switch(arguments.length){
case 1:return (true)
case 2:return (clojure.lang.Numbers.lte(x_1,y_2))}
var _cnt,_rtn,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.lte(x_1,y_2))?(((clojure.core.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.core.first.apply(null,[more_3]),clojure.core.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.lte(y_2,clojure.core.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn > "Returns non-nil if nums are in monotonically decreasing order,\n  otherwise false." {:inline-arities #{2}, :inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/gt)) (clojure.core/list x) (clojure.core/list y)))))} ([x] true) ([x y] (. clojure.lang.Numbers (gt x y))) ([x y & more] (if (> x y) (if (rest more) (recur y (first more) (rest more)) (> y (first more))) false)))
//---
(function __clojure_core_fn_918(){
return (clojure.JS.def(clojure.core,"_GT_",clojure.JS.variadic(2,(function __clojure_core_fn_918_GT_923(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.gt(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.gt(x_1,y_2))?(((clojure.core.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.core.first.apply(null,[more_3]),clojure.core.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.gt(y_2,clojure.core.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn >= "Returns non-nil if nums are in monotonically non-increasing order,\n  otherwise false." {:inline-arities #{2}, :inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/gte)) (clojure.core/list x) (clojure.core/list y)))))} ([x] true) ([x y] (. clojure.lang.Numbers (gte x y))) ([x y & more] (if (>= x y) (if (rest more) (recur y (first more) (rest more)) (>= y (first more))) false)))
//---
(function __clojure_core_fn_929(){
return (clojure.JS.def(clojure.core,"_GT__EQ_",clojure.JS.variadic(2,(function __clojure_core_fn_929_GT_EQ_934(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.gte(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.gte(x_1,y_2))?(((clojure.core.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.core.first.apply(null,[more_3]),clojure.core.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.gte(y_2,clojure.core.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn == "Returns non-nil if nums all have the same value, otherwise false" {:inline-arities #{2}, :inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/equiv)) (clojure.core/list x) (clojure.core/list y)))))} ([x] true) ([x y] (. clojure.lang.Numbers (equiv x y))) ([x y & more] (if (== x y) (if (rest more) (recur y (first more) (rest more)) (== y (first more))) false)))
//---
(function __clojure_core_fn_940(){
return (clojure.JS.def(clojure.core,"_EQ__EQ_",clojure.JS.variadic(2,(function __clojure_core_fn_940_EQ_EQ_945(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.equiv(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.equiv(x_1,y_2))?(((clojure.core.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.core.first.apply(null,[more_3]),clojure.core.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.equiv(y_2,clojure.core.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn max "Returns the greatest of the nums." ([x] x) ([x y] (if (> x y) x y)) ([x y & more] (reduce max (max x y) more)))
//---
(function __clojure_core_fn_951(){
return (clojure.JS.def(clojure.core,"max",clojure.JS.variadic(2,(function __clojure_core_fn_951_max_953(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.lang.Numbers.gt(x_1,y_2))?(x_1):(y_2)))
case 1:return (x_1)}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.reduce.apply(null,[clojure.core.max,clojure.core.max.apply(null,[x_1,y_2]),more_3]))}))))}).apply(null,[]);

//======
//(defn min "Returns the least of the nums." ([x] x) ([x y] (if (< x y) x y)) ([x y & more] (reduce min (min x y) more)))
//---
(function __clojure_core_fn_959(){
return (clojure.JS.def(clojure.core,"min",clojure.JS.variadic(2,(function __clojure_core_fn_959_min_961(x_1,y_2){switch(arguments.length){
case 1:return (x_1)
case 2:return (((clojure.lang.Numbers.lt(x_1,y_2))?(x_1):(y_2)))}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.reduce.apply(null,[clojure.core.min,clojure.core.min.apply(null,[x_1,y_2]),more_3]))}))))}).apply(null,[]);

//======
//(defn inc "Returns a number one greater than num." {:inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/inc)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (inc x)))
//---
(function __clojure_core_fn_967(){
return (clojure.JS.def(clojure.core,"inc",(function __clojure_core_fn_967_inc_972(x_1){
return (clojure.lang.Numbers.inc(x_1))})))}).apply(null,[]);

//======
//(defn dec "Returns a number one less than num." {:inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/dec)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (dec x)))
//---
(function __clojure_core_fn_976(){
return (clojure.JS.def(clojure.core,"dec",(function __clojure_core_fn_976_dec_981(x_1){
return (clojure.lang.Numbers.dec(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-inc "Returns a number one greater than x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_inc)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (unchecked_inc x)))
//---
(function __clojure_core_fn_985(){
return (clojure.JS.def(clojure.core,"unchecked_inc",(function __clojure_core_fn_985_unchecked_inc_990(x_1){
return (clojure.lang.Numbers.unchecked_inc(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-dec "Returns a number one less than x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_dec)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (unchecked_dec x)))
//---
(function __clojure_core_fn_994(){
return (clojure.JS.def(clojure.core,"unchecked_dec",(function __clojure_core_fn_994_unchecked_dec_999(x_1){
return (clojure.lang.Numbers.unchecked_dec(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-negate "Returns the negation of x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_negate)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (unchecked_negate x)))
//---
(function __clojure_core_fn_1003(){
return (clojure.JS.def(clojure.core,"unchecked_negate",(function __clojure_core_fn_1003_unchecked_negate_1008(x_1){
return (clojure.lang.Numbers.unchecked_negate(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-add "Returns the sum of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_add)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_add x y)))
//---
(function __clojure_core_fn_1012(){
return (clojure.JS.def(clojure.core,"unchecked_add",(function __clojure_core_fn_1012_unchecked_add_1017(x_1,y_2){
return (clojure.lang.Numbers.unchecked_add(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-subtract "Returns the difference of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_subtract)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_subtract x y)))
//---
(function __clojure_core_fn_1021(){
return (clojure.JS.def(clojure.core,"unchecked_subtract",(function __clojure_core_fn_1021_unchecked_subtract_1026(x_1,y_2){
return (clojure.lang.Numbers.unchecked_subtract(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-multiply "Returns the product of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_multiply)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_multiply x y)))
//---
(function __clojure_core_fn_1030(){
return (clojure.JS.def(clojure.core,"unchecked_multiply",(function __clojure_core_fn_1030_unchecked_multiply_1035(x_1,y_2){
return (clojure.lang.Numbers.unchecked_multiply(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-divide "Returns the division of x by y, both int or long. \n  Note - uses a primitive operator subject to truncation." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_divide)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_divide x y)))
//---
(function __clojure_core_fn_1039(){
return (clojure.JS.def(clojure.core,"unchecked_divide",(function __clojure_core_fn_1039_unchecked_divide_1044(x_1,y_2){
return (clojure.lang.Numbers.unchecked_divide(x_1,y_2))})))}).apply(null,[]);

//======
//(defn pos? "Returns true if num is greater than zero, else false" {:tag Boolean, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/isPos)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (isPos x)))
//---
(function __clojure_core_fn_1048(){
return (clojure.JS.def(clojure.core,"pos_QMARK_",(function __clojure_core_fn_1048_pos_QMARK_1053(x_1){
return (clojure.lang.Numbers.isPos(x_1))})))}).apply(null,[]);

//======
//(defn neg? "Returns true if num is less than zero, else false" {:tag Boolean, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/isNeg)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (isNeg x)))
//---
(function __clojure_core_fn_1057(){
return (clojure.JS.def(clojure.core,"neg_QMARK_",(function __clojure_core_fn_1057_neg_QMARK_1062(x_1){
return (clojure.lang.Numbers.isNeg(x_1))})))}).apply(null,[]);

//======
//(defn zero? "Returns true if num is zero, else false" {:tag Boolean, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/isZero)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (isZero x)))
//---
(function __clojure_core_fn_1066(){
return (clojure.JS.def(clojure.core,"zero_QMARK_",(function __clojure_core_fn_1066_zero_QMARK_1071(x_1){
return (clojure.lang.Numbers.isZero(x_1))})))}).apply(null,[]);

//======
//(defn quot "quot[ient] of dividing numerator by denominator." [num div] (. clojure.lang.Numbers (quotient num div)))
//---
(function __clojure_core_fn_1075(){
return (clojure.JS.def(clojure.core,"quot",(function __clojure_core_fn_1075_quot_1077(num_1,div_2){
return (clojure.lang.Numbers.quotient(num_1,div_2))})))}).apply(null,[]);

//======
//(defn rem "remainder of dividing numerator by denominator." [num div] (. clojure.lang.Numbers (remainder num div)))
//---
(function __clojure_core_fn_1081(){
return (clojure.JS.def(clojure.core,"rem",(function __clojure_core_fn_1081_rem_1083(num_1,div_2){
return (clojure.lang.Numbers.remainder(num_1,div_2))})))}).apply(null,[]);

//======
//(defn rationalize "returns the rational value of num" [num] (. clojure.lang.Numbers (rationalize num)))
//---
(function __clojure_core_fn_1087(){
return (clojure.JS.def(clojure.core,"rationalize",(function __clojure_core_fn_1087_rationalize_1089(num_1){
return (clojure.lang.Numbers.rationalize(num_1))})))}).apply(null,[]);

//======
//(defn bit-not "Bitwise complement" {:inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/not)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers not x))
//---
(function __clojure_core_fn_1093(){
return (clojure.JS.def(clojure.core,"bit_not",(function __clojure_core_fn_1093_bit_not_1098(x_1){
return (clojure.lang.Numbers.not(x_1))})))}).apply(null,[]);

//======
//(defn bit-and "Bitwise and" {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/and)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers and x y))
//---
(function __clojure_core_fn_1102(){
return (clojure.JS.def(clojure.core,"bit_and",(function __clojure_core_fn_1102_bit_and_1107(x_1,y_2){
return (clojure.lang.Numbers.and(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-or "Bitwise or" {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/or)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers or x y))
//---
(function __clojure_core_fn_1111(){
return (clojure.JS.def(clojure.core,"bit_or",(function __clojure_core_fn_1111_bit_or_1116(x_1,y_2){
return (clojure.lang.Numbers.or(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-xor "Bitwise exclusive or" {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/xor)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers xor x y))
//---
(function __clojure_core_fn_1120(){
return (clojure.JS.def(clojure.core,"bit_xor",(function __clojure_core_fn_1120_bit_xor_1125(x_1,y_2){
return (clojure.lang.Numbers.xor(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-and-not "Bitwise and with complement" [x y] (. clojure.lang.Numbers andNot x y))
//---
(function __clojure_core_fn_1129(){
return (clojure.JS.def(clojure.core,"bit_and_not",(function __clojure_core_fn_1129_bit_and_not_1131(x_1,y_2){
return (clojure.lang.Numbers.andNot(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-clear "Clear bit at index n" [x n] (. clojure.lang.Numbers clearBit x n))
//---
(function __clojure_core_fn_1135(){
return (clojure.JS.def(clojure.core,"bit_clear",(function __clojure_core_fn_1135_bit_clear_1137(x_1,n_2){
return (clojure.lang.Numbers.clearBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-set "Set bit at index n" [x n] (. clojure.lang.Numbers setBit x n))
//---
(function __clojure_core_fn_1141(){
return (clojure.JS.def(clojure.core,"bit_set",(function __clojure_core_fn_1141_bit_set_1143(x_1,n_2){
return (clojure.lang.Numbers.setBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-flip "Flip bit at index n" [x n] (. clojure.lang.Numbers flipBit x n))
//---
(function __clojure_core_fn_1147(){
return (clojure.JS.def(clojure.core,"bit_flip",(function __clojure_core_fn_1147_bit_flip_1149(x_1,n_2){
return (clojure.lang.Numbers.flipBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-test "Test bit at index n" [x n] (. clojure.lang.Numbers testBit x n))
//---
(function __clojure_core_fn_1153(){
return (clojure.JS.def(clojure.core,"bit_test",(function __clojure_core_fn_1153_bit_test_1155(x_1,n_2){
return (clojure.lang.Numbers.testBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-shift-left "Bitwise shift left" [x n] (. clojure.lang.Numbers shiftLeft x n))
//---
(function __clojure_core_fn_1159(){
return (clojure.JS.def(clojure.core,"bit_shift_left",(function __clojure_core_fn_1159_bit_shift_left_1161(x_1,n_2){
return (clojure.lang.Numbers.shiftLeft(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-shift-right "Bitwise shift right" [x n] (. clojure.lang.Numbers shiftRight x n))
//---
(function __clojure_core_fn_1165(){
return (clojure.JS.def(clojure.core,"bit_shift_right",(function __clojure_core_fn_1165_bit_shift_right_1167(x_1,n_2){
return (clojure.lang.Numbers.shiftRight(x_1,n_2))})))}).apply(null,[]);

//======
//(defn even? "Returns true if n is even, throws an exception if n is not an integer" [n] (zero? (bit-and n 1)))
//---
(function __clojure_core_fn_1171(){
return (clojure.JS.def(clojure.core,"even_QMARK_",(function __clojure_core_fn_1171_even_QMARK_1173(n_1){
return (clojure.lang.Numbers.isZero(clojure.lang.Numbers.and(n_1,(1))))})))}).apply(null,[]);

//======
//(defn odd? "Returns true if n is odd, throws an exception if n is not an integer" [n] (not (even? n)))
//---
(function __clojure_core_fn_1177(){
return (clojure.JS.def(clojure.core,"odd_QMARK_",(function __clojure_core_fn_1177_odd_QMARK_1179(n_1){
return (clojure.core.not.apply(null,[clojure.core.even_QMARK_.apply(null,[n_1])]))})))}).apply(null,[]);

//======
//(defn complement "Takes a fn f and returns a fn that takes the same arguments as f,\n  has the same effects, if any, and returns the opposite truth value." [f] (fn [& args] (not (apply f args))))
//---
(function __clojure_core_fn_1183(){
return (clojure.JS.def(clojure.core,"complement",(function __clojure_core_fn_1183_complement_1185(f_1){
return (clojure.JS.variadic(0,(function __clojure_core_fn_1183_complement_1185_fn_1187(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.not.apply(null,[clojure.core.apply.apply(null,[f_1,args_1])]))})))})))}).apply(null,[]);

//======
//(defn constantly "Returns a function that takes any number of arguments and returns x." [x] (fn [& args] x))
//---
(function __clojure_core_fn_1192(){
return (clojure.JS.def(clojure.core,"constantly",(function __clojure_core_fn_1192_constantly_1194(x_1){
return (clojure.JS.variadic(0,(function __clojure_core_fn_1192_constantly_1194_fn_1196(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (x_1)})))})))}).apply(null,[]);

//======
//(defn identity "Returns its argument." [x] x)
//---
(function __clojure_core_fn_1201(){
return (clojure.JS.def(clojure.core,"identity",(function __clojure_core_fn_1201_identity_1203(x_1){
return (x_1)})))}).apply(null,[]);
// Skipping: (defn count "Returns the number of items in the collection. (count nil) returns\n  0.  Also works on strings, arrays, and Java Collections and Maps" [coll] (. clojure.lang.RT (count coll)))

//======
//(defn peek "For a list or queue, same as first, for a vector, same as, but much\n  more efficient than, last. If the collection is empty, returns nil." [coll] (. clojure.lang.RT (peek coll)))
//---
(function __clojure_core_fn_1213(){
return (clojure.JS.def(clojure.core,"peek",(function __clojure_core_fn_1213_peek_1215(coll_1){
return (clojure.lang.RT.peek(coll_1))})))}).apply(null,[]);

//======
//(defn pop "For a list or queue, returns a new list/queue without the first\n  item, for a vector, returns a new vector without the last item. If\n  the collection is empty, throws an exception.  Note - not the same\n  as rest/butlast." [coll] (. clojure.lang.RT (pop coll)))
//---
(function __clojure_core_fn_1219(){
return (clojure.JS.def(clojure.core,"pop",(function __clojure_core_fn_1219_pop_1221(coll_1){
return (clojure.lang.RT.pop(coll_1))})))}).apply(null,[]);
// Skipping: (defn nth "Returns the value at the index. get returns nil if index out of\n  bounds, nth throws an exception unless not-found is supplied.  nth\n  also works for strings, Java arrays, regex Matchers and Lists, and,\n  in O(n) time, for sequences." ([coll index] (. clojure.lang.RT (nth coll index))) ([coll index not-found] (. clojure.lang.RT (nth coll index not-found))))
// Skipping: (defn contains? "Returns true if key is present, else false." [map key] (. clojure.lang.RT (contains map key)))
// Skipping: (defn get "Returns the value mapped to key, not-found or nil if key not present." ([map key] (. clojure.lang.RT (get map key))) ([map key not-found] (. clojure.lang.RT (get map key not-found))))

//======
//(defn dissoc "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,\n  that does not contain a mapping for key(s)." ([map] map) ([map key] (. clojure.lang.RT (dissoc map key))) ([map key & ks] (let [ret (dissoc map key)] (if ks (recur ret (first ks) (rest ks)) ret))))
//---
(function __clojure_core_fn_1245(){
return (clojure.JS.def(clojure.core,"dissoc",clojure.JS.variadic(2,(function __clojure_core_fn_1245_dissoc_1247(map_1,key_2){switch(arguments.length){
case 2:return (clojure.lang.RT.dissoc(map_1,key_2))
case 1:return (map_1)}
var _cnt,_rtn,ret_4,ks_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((ret_4=clojure.core.dissoc.apply(null,[map_1,key_2])),
((ks_3)?((_cnt=1,_rtn=[ret_4,clojure.core.first.apply(null,[ks_3]),clojure.core.rest.apply(null,[ks_3])],map_1=_rtn[0],key_2=_rtn[1],ks_3=_rtn[2])):(ret_4)))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn disj "disj[oin]. Returns a new set of the same (hashed/sorted) type, that\n  does not contain key(s)." ([set] set) ([set key] (. set (disjoin key))) ([set key & ks] (let [ret (disj set key)] (if ks (recur ret (first ks) (rest ks)) ret))))
//---
(function __clojure_core_fn_1253(){
return (clojure.JS.def(clojure.core,"disj",clojure.JS.variadic(2,(function __clojure_core_fn_1253_disj_1255(set_1,key_2){switch(arguments.length){
case 2:return ((set_1).disjoin(key_2))
case 1:return (set_1)}
var _cnt,_rtn,ret_4,ks_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((ret_4=clojure.core.disj.apply(null,[set_1,key_2])),
((ks_3)?((_cnt=1,_rtn=[ret_4,clojure.core.first.apply(null,[ks_3]),clojure.core.rest.apply(null,[ks_3])],set_1=_rtn[0],key_2=_rtn[1],ks_3=_rtn[2])):(ret_4)))
}while(_cnt);return _rtn;}))))}).apply(null,[]);
// Skipping: (defn find "Returns the map entry for key, or nil if key not present." [map key] (. clojure.lang.RT (find map key)))

//======
//(defn select-keys "Returns a map containing only those entries in map whose key is in keys" [map keyseq] (loop [ret {} keys (seq keyseq)] (if keys (let [entry (. clojure.lang.RT (find map (first keys)))] (recur (if entry (conj ret entry) ret) (rest keys))) ret)))
//---
(function __clojure_core_fn_1267(){
return (clojure.JS.def(clojure.core,"select_keys",(function __clojure_core_fn_1267_select_keys_1269(map_1,keyseq_2){
var entry_5,keys_4,ret_3;
return (((function __loop(){var _rtn,_cnt;(ret_3=clojure.lang.PersistentHashMap.EMPTY),
(keys_4=clojure.core.seq.apply(null,[keyseq_2]));do{_cnt=0;
_rtn=((keys_4)?(((entry_5=clojure.lang.RT.find(map_1,clojure.core.first.apply(null,[keys_4]))),
(_cnt=1,_rtn=[((entry_5)?(clojure.core.conj.apply(null,[ret_3,entry_5])):(ret_3)),clojure.core.rest.apply(null,[keys_4])],ret_3=_rtn[0],keys_4=_rtn[1]))):(ret_3))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);
// Skipping: (defn keys "Returns a sequence of the map's keys." [map] (. clojure.lang.RT (keys map)))
// Skipping: (defn vals "Returns a sequence of the map's values." [map] (. clojure.lang.RT (vals map)))

//======
//(defn key "Returns the key of the map entry." [e] (. e (getKey)))
//---
(function __clojure_core_fn_1285(){
return (clojure.JS.def(clojure.core,"key",(function __clojure_core_fn_1285_key_1287(e_1){
return ((e_1).getKey())})))}).apply(null,[]);

//======
//(defn val "Returns the value in the map entry." [e] (. e (getValue)))
//---
(function __clojure_core_fn_1291(){
return (clojure.JS.def(clojure.core,"val",(function __clojure_core_fn_1291_val_1293(e_1){
return ((e_1).getValue())})))}).apply(null,[]);

//======
//(defn rseq "Returns, in constant time, a sequence of the items in rev (which\n  can be a vector or sorted-map), in reverse order." [rev] (. rev (rseq)))
//---
(function __clojure_core_fn_1297(){
return (clojure.JS.def(clojure.core,"rseq",(function __clojure_core_fn_1297_rseq_1299(rev_1){
return ((rev_1).rseq())})))}).apply(null,[]);

//======
//(defn name "Returns the name String of a symbol or keyword." [x] (. x (getName)))
//---
(function __clojure_core_fn_1303(){
return (clojure.JS.def(clojure.core,"name",(function __clojure_core_fn_1303_name_1305(x_1){
return ((x_1).getName())})))}).apply(null,[]);

//======
//(defn namespace "Returns the namespace String of a symbol or keyword, or nil if not present." [x] (. x (getNamespace)))
//---
(function __clojure_core_fn_1309(){
return (clojure.JS.def(clojure.core,"namespace",(function __clojure_core_fn_1309_namespace_1311(x_1){
return ((x_1).getNamespace())})))}).apply(null,[]);
// Skipping: (defmacro locking "Executes exprs in an implicit do, while holding the monitor of x.\n  Will release the monitor of x in all circumstances." [x & body] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote lockee__1315)) (clojure.core/list x)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote try)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote monitor-enter)) (clojure.core/list (quote lockee__1315)))) body (clojure.core/list (clojure.core/concat (clojure.core/list (quote finally)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote monitor-exit)) (clojure.core/list (quote lockee__1315))))))))))
// Skipping: (defmacro .. "form => fieldName-symbol or (instanceMethodName-symbol args*)\n\n  Expands into a member access (.) of the first member on the first\n  argument, followed by the next member on the result, etc. For\n  instance:\n\n  (.. System (getProperties) (get \"os.name\"))\n\n  expands to:\n\n  (. (. System (getProperties)) (get \"os.name\"))\n\n  but is easier to write, read, and understand." ([x form] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list x) (clojure.core/list form))) ([x form & more] (clojure.core/concat (clojure.core/list (quote clojure.core/..)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list x) (clojure.core/list form))) more)))
// Skipping: (defmacro -> "Threads the expr through the forms. Inserts x as the\n  second item in the first form, making a list of it if it is not a\n  list already. If there are more forms, inserts the first form as the\n  second item in second form, etc." ([x form] (if (seq? form) (clojure.core/concat (clojure.core/list (first form)) (clojure.core/list x) (rest form)) (list form x))) ([x form & more] (clojure.core/concat (clojure.core/list (quote clojure.core/->)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/->)) (clojure.core/list x) (clojure.core/list form))) more)))
// Skipping: (defmacro defmulti "Creates a new multimethod with the associated dispatch function. If\n  default-dispatch-val is supplied it becomes the default dispatch\n  value of the multimethod, otherwise the default dispatch value\n  is :default." ([name dispatch-fn] (clojure.core/concat (clojure.core/list (quote clojure.core/defmulti)) (clojure.core/list name) (clojure.core/list dispatch-fn) (clojure.core/list :default))) ([name dispatch-fn default-val] (clojure.core/concat (clojure.core/list (quote def)) (clojure.core/list (with-meta name (assoc (clojure.core/meta name) :tag (quote clojure.lang.MultiFn)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote new)) (clojure.core/list (quote clojure.lang.MultiFn)) (clojure.core/list dispatch-fn) (clojure.core/list default-val))))))
// Skipping: (defmacro defmethod "Creates and installs a new method of multimethod associated with dispatch-value. " [multifn dispatch-val & fn-tail] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list multifn) (clojure.core/list (quote clojure.core/addMethod)) (clojure.core/list dispatch-val) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/fn)) fn-tail))))
// Skipping: (defmacro remove-method "Removes the method of multimethod associated\twith dispatch-value." [multifn dispatch-val] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list multifn) (clojure.core/list (quote clojure.core/removeMethod)) (clojure.core/list dispatch-val)))
// Skipping: (defmacro prefer-method "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y when there is a conflict" [multifn dispatch-val-x dispatch-val-y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list multifn) (clojure.core/list (quote clojure.core/preferMethod)) (clojure.core/list dispatch-val-x) (clojure.core/list dispatch-val-y)))
// Skipping: (defmacro binding "binding => var-symbol init-expr \n\n  Creates new bindings for the (already-existing) vars, with the\n  supplied initial values, executes the exprs in an implicit do, then\n  re-establishes the bindings that existed before." [bindings & body] (let [var-ize (fn [var-vals] (loop [ret [] vvs (seq var-vals)] (if vvs (recur (conj (conj ret (clojure.core/concat (clojure.core/list (quote var)) (clojure.core/list (first vvs)))) (second vvs)) (rest (rest vvs))) (seq ret))))] (clojure.core/concat (clojure.core/list (quote do)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Var)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/pushThreadBindings)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/hash-map)) (var-ize bindings))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote try)) body (clojure.core/list (clojure.core/concat (clojure.core/list (quote finally)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Var)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/popThreadBindings)))))))))))))

//======
//(defn find-var "Returns the global var named by the namespace-qualified symbol, or\n  nil if no var with that name." [sym] (. clojure.lang.Var (find sym)))
//---
(function __clojure_core_fn_1400(){
return (clojure.JS.def(clojure.core,"find_var",(function __clojure_core_fn_1400_find_var_1402(sym_1){
return (clojure.lang.Var.find(sym_1))})))}).apply(null,[]);

//======
//(defn agent "Creates and returns an agent with an initial value of state and an\n  optional validate fn. validate-fn must be nil or a side-effect-free fn of\n  one argument, which will be passed the intended new state on any state\n  change. If the new state is unacceptable, the validate-fn should\n  throw an exception." ([state] (new clojure.lang.Agent state)) ([state validate-fn] (new clojure.lang.Agent state validate-fn)))
//---
(function __clojure_core_fn_1406(){
return (clojure.JS.def(clojure.core,"agent",(function __clojure_core_fn_1406_agent_1408(state_1,validate_fn_2){switch(arguments.length){
case 1:return ((new clojure.lang.Agent(state_1)))}
return ((new clojure.lang.Agent(state_1,validate_fn_2)))})))}).apply(null,[]);

//======
//(defn ! [& args] (throw (clojure.lang.RT/makeException "! is now send. See also send-off")))
//---
(function __clojure_core_fn_1413(){
return (clojure.JS.def(clojure.core,"_BANG_",clojure.JS.variadic(0,(function __clojure_core_fn_1413_BANG_1415(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw clojure.lang.RT.makeException("! is now send. See also send-off")})())}))))}).apply(null,[]);

//======
//(defn send "Dispatch an action to an agent. Returns the agent immediately.\n  Subsequently, in a thread from a thread pool, the state of the agent\n  will be set to the value of:\n\n  (apply action-fn state-of-agent args)" [a f & args] (. a (dispatch f args false)))
//---
(function __clojure_core_fn_1419(){
return (clojure.JS.def(clojure.core,"send",clojure.JS.variadic(2,(function __clojure_core_fn_1419_send_1421(a_1,f_2){
var args_3=clojure.JS.rest_args(this,arguments,2);
return ((a_1).dispatch(f_2,args_3,false))}))))}).apply(null,[]);

//======
//(defn send-off "Dispatch a potentially blocking action to an agent. Returns the\n  agent immediately. Subsequently, in a separate thread, the state of\n  the agent will be set to the value of:\n\n  (apply action-fn state-of-agent args)" [a f & args] (. a (dispatch f args true)))
//---
(function __clojure_core_fn_1425(){
return (clojure.JS.def(clojure.core,"send_off",clojure.JS.variadic(2,(function __clojure_core_fn_1425_send_off_1427(a_1,f_2){
var args_3=clojure.JS.rest_args(this,arguments,2);
return ((a_1).dispatch(f_2,args_3,true))}))))}).apply(null,[]);

//======
//(defn add-watch "Experimental.\n  Adds a watcher to an agent. Whenever the agent runs an action, any\n  registered watchers will have their callback function called.  The\n  callback fn will be passed 3 args, the watcher, the agent and a boolean\n  which will be true if the agent's state was (potentially) changed by\n  the action. The callback fn is run synchronously with the action,\n  and thus derefs of the agent in the callback will see the value set\n  during that action. Because it is run on the action thread, the\n  callback should not block, but can send messages." [a watcher callback] (.addWatch a watcher callback))
//---
(function __clojure_core_fn_1431(){
return (clojure.JS.def(clojure.core,"add_watch",(function __clojure_core_fn_1431_add_watch_1433(a_1,watcher_2,callback_3){
return ((a_1).addWatch(watcher_2,callback_3))})))}).apply(null,[]);

//======
//(defn remove-watch "Experimental.\n  Removes a watcher (set by add-watch) from an agent" [a watcher] (.removeWatch a watcher))
//---
(function __clojure_core_fn_1437(){
return (clojure.JS.def(clojure.core,"remove_watch",(function __clojure_core_fn_1437_remove_watch_1439(a_1,watcher_2){
return ((a_1).removeWatch(watcher_2))})))}).apply(null,[]);

//======
//(defn agent-errors "Returns a sequence of the exceptions thrown during asynchronous\n  actions of the agent." [a] (. a (getErrors)))
//---
(function __clojure_core_fn_1443(){
return (clojure.JS.def(clojure.core,"agent_errors",(function __clojure_core_fn_1443_agent_errors_1445(a_1){
return ((a_1).getErrors())})))}).apply(null,[]);

//======
//(defn clear-agent-errors "Clears any exceptions thrown during asynchronous actions of the\n  agent, allowing subsequent actions to occur." [a] (. a (clearErrors)))
//---
(function __clojure_core_fn_1449(){
return (clojure.JS.def(clojure.core,"clear_agent_errors",(function __clojure_core_fn_1449_clear_agent_errors_1451(a_1){
return ((a_1).clearErrors())})))}).apply(null,[]);

//======
//(defn shutdown-agents "Initiates a shutdown of the thread pools that back the agent\n  system. Running actions will complete, but no new actions will be\n  accepted" [] (. clojure.lang.Agent shutdown))
//---
(function __clojure_core_fn_1455(){
return (clojure.JS.def(clojure.core,"shutdown_agents",(function __clojure_core_fn_1455_shutdown_agents_1457(){
return (clojure.lang.Agent.shutdown())})))}).apply(null,[]);

//======
//(defn ref "Creates and returns a Ref with an initial value of x and an optional validate fn.\n  validate-fn must be nil or a side-effect-free fn of one argument, which will\n  be passed the intended new state on any state change. If the new\n  state is unacceptable, the validate-fn should throw an\n  exception. validate-fn will be called on transaction commit, when\n  all refs have their final values." ([x] (new clojure.lang.Ref x)) ([x validate-fn] (new clojure.lang.Ref x validate-fn)))
//---
(function __clojure_core_fn_1461(){
return (clojure.JS.def(clojure.core,"ref",(function __clojure_core_fn_1461_ref_1463(x_1,validate_fn_2){switch(arguments.length){
case 1:return ((new clojure.lang.Ref(x_1)))}
return ((new clojure.lang.Ref(x_1,validate_fn_2)))})))}).apply(null,[]);

//======
//(defn deref "Also reader macro: @ref/@agent Within a transaction, returns the\n  in-transaction-value of ref, else returns the\n  most-recently-committed value of ref. When applied to an agent,\n  returns its current state." [ref] (. ref (get)))
//---
(function __clojure_core_fn_1468(){
return (clojure.JS.def(clojure.core,"deref",(function __clojure_core_fn_1468_deref_1470(ref_1){
return ((ref_1).get())})))}).apply(null,[]);

//======
//(defn set-validator "Sets the validator-fn for a var/ref/agent. validator-fn must be nil or a\n  side-effect-free fn of one argument, which will be passed the intended\n  new state on any state change. If the new state is unacceptable, the\n  validator-fn should throw an exception. If the current state (root\n  value if var) is not acceptable to the new validator, an exception\n  will be thrown and the validator will not be changed." [iref validator-fn] (. iref (setValidator validator-fn)))
//---
(function __clojure_core_fn_1474(){
return (clojure.JS.def(clojure.core,"set_validator",(function __clojure_core_fn_1474_set_validator_1476(iref_1,validator_fn_2){
return ((iref_1).setValidator(validator_fn_2))})))}).apply(null,[]);

//======
//(defn get-validator "Gets the validator-fn for a var/ref/agent." [iref] (. iref (getValidator)))
//---
(function __clojure_core_fn_1480(){
return (clojure.JS.def(clojure.core,"get_validator",(function __clojure_core_fn_1480_get_validator_1482(iref_1){
return ((iref_1).getValidator())})))}).apply(null,[]);

//======
//(defn commute "Must be called in a transaction. Sets the in-transaction-value of\n  ref to:\n\n  (apply fun in-transaction-value-of-ref args)\n\n  and returns the in-transaction-value of ref.\n\n  At the commit point of the transaction, sets the value of ref to be:\n\n  (apply fun most-recently-committed-value-of-ref args)\n\n  Thus fun should be commutative, or, failing that, you must accept\n  last-one-in-wins behavior.  commute allows for more concurrency than\n  ref-set." [ref fun & args] (. ref (commute fun args)))
//---
(function __clojure_core_fn_1486(){
return (clojure.JS.def(clojure.core,"commute",clojure.JS.variadic(2,(function __clojure_core_fn_1486_commute_1488(ref_1,fun_2){
var args_3=clojure.JS.rest_args(this,arguments,2);
return ((ref_1).commute(fun_2,args_3))}))))}).apply(null,[]);

//======
//(defn alter "Must be called in a transaction. Sets the in-transaction-value of\n  ref to:\n\n  (apply fun in-transaction-value-of-ref args)\n\n  and returns the in-transaction-value of ref." [ref fun & args] (. ref (alter fun args)))
//---
(function __clojure_core_fn_1492(){
return (clojure.JS.def(clojure.core,"alter",clojure.JS.variadic(2,(function __clojure_core_fn_1492_alter_1494(ref_1,fun_2){
var args_3=clojure.JS.rest_args(this,arguments,2);
return ((ref_1).alter(fun_2,args_3))}))))}).apply(null,[]);

//======
//(defn ref-set "Must be called in a transaction. Sets the value of ref.\n  Returns val." [ref val] (. ref (set val)))
//---
(function __clojure_core_fn_1498(){
return (clojure.JS.def(clojure.core,"ref_set",(function __clojure_core_fn_1498_ref_set_1500(ref_1,val_2){
return ((ref_1).set(val_2))})))}).apply(null,[]);

//======
//(defn ensure "Must be called in a transaction. Protects the ref from modification\n  by other transactions.  Returns the in-transaction-value of\n  ref. Allows for more concurrency than (ref-set ref @ref)" [ref] (. ref (touch)) (. ref (get)))
//---
(function __clojure_core_fn_1504(){
return (clojure.JS.def(clojure.core,"ensure",(function __clojure_core_fn_1504_ensure_1506(ref_1){
return ((ref_1).touch(),
(ref_1).get())})))}).apply(null,[]);
// Skipping: (defmacro sync "transaction-flags => TBD, pass nil for now\n\n  Runs the exprs (in an implicit do) in a transaction that encompasses\n  exprs and any nested calls.  Starts a transaction if none is already\n  running on this thread. Any uncaught exception will abort the\n  transaction and flow out of sync. The exprs may be run more than\n  once, but any effects on Refs will be atomic." [flags-ignored-for-now & body] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.LockingTransaction)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/runInTransaction)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/fn)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat))) body))))))

//======
//(defn comp "Takes a set of functions and returns a fn that is the composition\n  of those fns.  The returned fn takes a variable number of args,\n  applies the rightmost of fns to the args, the next\n  fn (right-to-left) to the result, etc." [& fs] (let [fs (reverse fs)] (fn [& args] (loop [ret (apply (first fs) args) fs (rest fs)] (if fs (recur ((first fs) ret) (rest fs)) ret)))))
//---
(function __clojure_core_fn_1519(){
return (clojure.JS.def(clojure.core,"comp",clojure.JS.variadic(0,(function __clojure_core_fn_1519_comp_1521(){
var fs_2,fs_1=clojure.JS.rest_args(this,arguments,0);
return (((fs_2=clojure.core.reverse.apply(null,[fs_1])),
clojure.JS.variadic(0,(function __clojure_core_fn_1519_comp_1521_fn_1523(){
var fs_3,ret_2,args_1=clojure.JS.rest_args(this,arguments,0);
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.core.apply.apply(null,[clojure.core.first.apply(null,[fs_2]),args_1])),
(fs_3=clojure.core.rest.apply(null,[fs_2]));do{_cnt=0;
_rtn=((fs_3)?((_cnt=1,_rtn=[clojure.core.first.apply(null,[fs_3]).apply(null,[ret_2]),clojure.core.rest.apply(null,[fs_3])],ret_2=_rtn[0],fs_3=_rtn[1])):(ret_2))}while(_cnt);return _rtn;})()))}))))}))))}).apply(null,[]);

//======
//(defn partial "Takes a function f and fewer than the normal arguments to f, and\n  returns a fn that takes a variable number of additional args. When\n  called, the returned function calls f with args + additional args." ([f arg1] (fn [& args] (apply f arg1 args))) ([f arg1 arg2] (fn [& args] (apply f arg1 arg2 args))) ([f arg1 arg2 arg3] (fn [& args] (apply f arg1 arg2 arg3 args))) ([f arg1 arg2 arg3 & more] (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))
//---
(function __clojure_core_fn_1528(){
return (clojure.JS.def(clojure.core,"partial",clojure.JS.variadic(4,(function __clojure_core_fn_1528_partial_1530(f_1,arg1_2,arg2_3,arg3_4){switch(arguments.length){
case 3:return (clojure.JS.variadic(0,(function __clojure_core_fn_1528_partial_1530_fn_1536(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[f_1,arg1_2,arg2_3,args_1]))})))
case 2:return (clojure.JS.variadic(0,(function __clojure_core_fn_1528_partial_1530_fn_1532(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[f_1,arg1_2,args_1]))})))
case 4:return (clojure.JS.variadic(0,(function __clojure_core_fn_1528_partial_1530_fn_1540(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[f_1,arg1_2,arg2_3,arg3_4,args_1]))})))}
var more_5=clojure.JS.rest_args(this,arguments,4);
return (clojure.JS.variadic(0,(function __clojure_core_fn_1528_partial_1530_fn_1544(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[f_1,arg1_2,arg2_3,arg3_4,clojure.core.concat.apply(null,[more_5,args_1])]))})))}))))}).apply(null,[]);

//======
//(defn every? "Returns true if (pred x) is logical true for every x in coll, else\n  false." {:tag Boolean} [pred coll] (if (seq coll) (and (pred (first coll)) (recur pred (rest coll))) true))
//---
(function __clojure_core_fn_1549(){
return (clojure.JS.def(clojure.core,"every_QMARK_",(function __clojure_core_fn_1549_every_QMARK_1551(pred_1,coll_2){
var _cnt,_rtn,and__806_3;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[coll_2]))?(((and__806_3=pred_1.apply(null,[clojure.core.first.apply(null,[coll_2])])),
((and__806_3)?((_cnt=1,_rtn=[pred_1,clojure.core.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])):(and__806_3)))):(true))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def not-every? (comp not every?))
//---
(function __clojure_core_fn_1555(){
return (clojure.JS.def(clojure.core,"not_every_QMARK_",clojure.core.comp.apply(null,[clojure.core.not,clojure.core.every_QMARK_])))}).apply(null,[]);

//======
//(defn some "Returns the first logical true value of (pred x) for any x in coll,\n  else nil." [pred coll] (when (seq coll) (or (pred (first coll)) (recur pred (rest coll)))))
//---
(function __clojure_core_fn_1558(){
return (clojure.JS.def(clojure.core,"some",(function __clojure_core_fn_1558_some_1560(pred_1,coll_2){
var _cnt,_rtn,or__820_3;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[coll_2]))?(((or__820_3=pred_1.apply(null,[clojure.core.first.apply(null,[coll_2])])),
((or__820_3)?(or__820_3):((_cnt=1,_rtn=[pred_1,clojure.core.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1]))))):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def not-any? (comp not some))
//---
(function __clojure_core_fn_1564(){
return (clojure.JS.def(clojure.core,"not_any_QMARK_",clojure.core.comp.apply(null,[clojure.core.not,clojure.core.some])))}).apply(null,[]);

//======
//(defn map "Returns a lazy seq consisting of the result of applying f to the\n  set of first items of each coll, followed by applying f to the set\n  of second items in each coll, until any one of the colls is\n  exhausted.  Any remaining items in other colls are ignored. Function\n  f should accept number-of-colls arguments." ([f coll] (when (seq coll) (lazy-cons (f (first coll)) (map f (rest coll))))) ([f c1 c2] (when (and (seq c1) (seq c2)) (lazy-cons (f (first c1) (first c2)) (map f (rest c1) (rest c2))))) ([f c1 c2 c3] (when (and (seq c1) (seq c2) (seq c3)) (lazy-cons (f (first c1) (first c2) (first c3)) (map f (rest c1) (rest c2) (rest c3))))) ([f c1 c2 c3 & colls] (let [step (fn step [cs] (when (every? seq cs) (lazy-cons (map first cs) (step (map rest cs)))))] (map (fn* [p1__1567] (apply f p1__1567)) (step (conj colls c3 c2 c1))))))
//---
(function __clojure_core_fn_1568(){
return (clojure.JS.def(clojure.core,"map",clojure.JS.variadic(4,(function __clojure_core_fn_1568_map_1570(f_1,c1_2,c2_3,c3_4){switch(arguments.length){
case 3:var and__806_4;
return (((((and__806_4=clojure.core.seq.apply(null,[c1_2])),
((and__806_4)?(clojure.core.seq.apply(null,[c2_3])):(and__806_4))))?((new clojure.lang.LazyCons((function __clojure_core_fn_1568_map_1570_fn_1579(G__1578_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.core.first.apply(null,[c1_2]),clojure.core.first.apply(null,[c2_3])]))}
return (clojure.core.map.apply(null,[f_1,clojure.core.rest.apply(null,[c1_2]),clojure.core.rest.apply(null,[c2_3])]))})))):(null)))
case 4:var and__806_6,and__806_5;
return (((((and__806_5=clojure.core.seq.apply(null,[c1_2])),
((and__806_5)?(((and__806_6=clojure.core.seq.apply(null,[c2_3])),
((and__806_6)?(clojure.core.seq.apply(null,[c3_4])):(and__806_6)))):(and__806_5))))?((new clojure.lang.LazyCons((function __clojure_core_fn_1568_map_1570_fn_1585(G__1584_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.core.first.apply(null,[c1_2]),clojure.core.first.apply(null,[c2_3]),clojure.core.first.apply(null,[c3_4])]))}
return (clojure.core.map.apply(null,[f_1,clojure.core.rest.apply(null,[c1_2]),clojure.core.rest.apply(null,[c2_3]),clojure.core.rest.apply(null,[c3_4])]))})))):(null)))
case 2:var coll_2=arguments[1];
return (((clojure.core.seq.apply(null,[coll_2]))?((new clojure.lang.LazyCons((function __clojure_core_fn_1568_map_1570_fn_1573(G__1572_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.core.first.apply(null,[coll_2])]))}
return (clojure.core.map.apply(null,[f_1,clojure.core.rest.apply(null,[coll_2])]))})))):(null)))}
var step_6,colls_5=clojure.JS.rest_args(this,arguments,4);
return (((step_6=(function __clojure_core_fn_1568_map_1570_step_1590(cs_1){
var step_0=arguments.callee;
return (((clojure.core.every_QMARK_.apply(null,[clojure.core.seq,cs_1]))?((new clojure.lang.LazyCons((function __clojure_core_fn_1568_map_1570_step_1590_fn_1592(G__1591_1){switch(arguments.length){
case 0:return (clojure.core.map.apply(null,[clojure.core.first,cs_1]))}
return (step_0.apply(null,[clojure.core.map.apply(null,[clojure.core.rest,cs_1])]))})))):(null)))})),
clojure.core.map.apply(null,[(function __clojure_core_fn_1568_map_1570_fn_1597(p1__1567_1){
return (clojure.core.apply.apply(null,[f_1,p1__1567_1]))}),step_6.apply(null,[clojure.core.conj.apply(null,[colls_5,c3_4,c2_3,c1_2])])])))}))))}).apply(null,[]);

//======
//(defn mapcat "Returns the result of applying concat to the result of applying map\n  to f and colls.  Thus function f should return a collection." [f & colls] (apply concat (apply map f colls)))
//---
(function __clojure_core_fn_1602(){
return (clojure.JS.def(clojure.core,"mapcat",clojure.JS.variadic(1,(function __clojure_core_fn_1602_mapcat_1604(f_1){
var colls_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.core.apply.apply(null,[clojure.core.concat,clojure.core.apply.apply(null,[clojure.core.map,f_1,colls_2])]))}))))}).apply(null,[]);

//======
//(defn filter "Returns a lazy seq of the items in coll for which\n  (pred item) returns true. pred must be free of side-effects." [pred coll] (when (seq coll) (if (pred (first coll)) (lazy-cons (first coll) (filter pred (rest coll))) (recur pred (rest coll)))))
//---
(function __clojure_core_fn_1608(){
return (clojure.JS.def(clojure.core,"filter",(function __clojure_core_fn_1608_filter_1610(pred_1,coll_2){
var _cnt,_rtn;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[coll_2]))?(((pred_1.apply(null,[clojure.core.first.apply(null,[coll_2])]))?((new clojure.lang.LazyCons((function __clojure_core_fn_1608_filter_1610_fn_1613(G__1612_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll_2]))}
return (clojure.core.filter.apply(null,[pred_1,clojure.core.rest.apply(null,[coll_2])]))})))):((_cnt=1,_rtn=[pred_1,clojure.core.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])))):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn remove "Returns a lazy seq of the items in coll for which\n  (pred item) returns false. pred must be free of side-effects." [pred coll] (when (seq coll) (if (pred (first coll)) (recur pred (rest coll)) (lazy-cons (first coll) (remove pred (rest coll))))))
//---
(function __clojure_core_fn_1619(){
return (clojure.JS.def(clojure.core,"remove",(function __clojure_core_fn_1619_remove_1621(pred_1,coll_2){
var _cnt,_rtn;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[coll_2]))?(((pred_1.apply(null,[clojure.core.first.apply(null,[coll_2])]))?((_cnt=1,_rtn=[pred_1,clojure.core.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])):((new clojure.lang.LazyCons((function __clojure_core_fn_1619_remove_1621_fn_1624(G__1623_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll_2]))}
return (clojure.core.remove.apply(null,[pred_1,clojure.core.rest.apply(null,[coll_2])]))})))))):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn take "Returns a lazy seq of the first n items in coll, or all items if\n  there are fewer than n." [n coll] (when (and (pos? n) (seq coll)) (lazy-cons (first coll) (when (> n 1) (take (dec n) (rest coll))))))
//---
(function __clojure_core_fn_1630(){
return (clojure.JS.def(clojure.core,"take",(function __clojure_core_fn_1630_take_1632(n_1,coll_2){
var and__806_3;
return (((((and__806_3=clojure.lang.Numbers.isPos(n_1)),
((and__806_3)?(clojure.core.seq.apply(null,[coll_2])):(and__806_3))))?((new clojure.lang.LazyCons((function __clojure_core_fn_1630_take_1632_fn_1635(G__1634_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll_2]))}
return (((clojure.lang.Numbers.gt(n_1,(1)))?(clojure.core.take.apply(null,[clojure.lang.Numbers.dec(n_1),clojure.core.rest.apply(null,[coll_2])])):(null)))})))):(null)))})))}).apply(null,[]);

//======
//(defn take-while "Returns a lazy seq of successive items from coll while\n  (pred item) returns true. pred must be free of side-effects." [pred coll] (when (and (seq coll) (pred (first coll))) (lazy-cons (first coll) (take-while pred (rest coll)))))
//---
(function __clojure_core_fn_1641(){
return (clojure.JS.def(clojure.core,"take_while",(function __clojure_core_fn_1641_take_while_1643(pred_1,coll_2){
var and__806_3;
return (((((and__806_3=clojure.core.seq.apply(null,[coll_2])),
((and__806_3)?(pred_1.apply(null,[clojure.core.first.apply(null,[coll_2])])):(and__806_3))))?((new clojure.lang.LazyCons((function __clojure_core_fn_1641_take_while_1643_fn_1646(G__1645_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll_2]))}
return (clojure.core.take_while.apply(null,[pred_1,clojure.core.rest.apply(null,[coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn drop "Returns a lazy seq of all but the first n items in coll." [n coll] (if (and (pos? n) (seq coll)) (recur (dec n) (rest coll)) (seq coll)))
//---
(function __clojure_core_fn_1652(){
return (clojure.JS.def(clojure.core,"drop",(function __clojure_core_fn_1652_drop_1654(n_1,coll_2){
var _cnt,_rtn,and__806_3;
do{_cnt=0;_rtn=((((and__806_3=clojure.lang.Numbers.isPos(n_1)),
((and__806_3)?(clojure.core.seq.apply(null,[coll_2])):(and__806_3))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_1),clojure.core.rest.apply(null,[coll_2])],n_1=_rtn[0],coll_2=_rtn[1])):(clojure.core.seq.apply(null,[coll_2])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn drop-last "Return a lazy seq of all but the last n (default 1) items in coll" ([s] (drop-last 1 s)) ([n s] (map (fn [x _] x) (seq s) (drop n s))))
//---
(function __clojure_core_fn_1658(){
return (clojure.JS.def(clojure.core,"drop_last",(function __clojure_core_fn_1658_drop_last_1660(n_1,s_2){switch(arguments.length){
case 1:var s_1=arguments[0];
return (clojure.core.drop_last.apply(null,[(1),s_1]))}
return (clojure.core.map.apply(null,[(function __clojure_core_fn_1658_drop_last_1660_fn_1663(x_1,__2){
return (x_1)}),clojure.core.seq.apply(null,[s_2]),clojure.core.drop.apply(null,[n_1,s_2])]))})))}).apply(null,[]);

//======
//(defn drop-while "Returns a lazy seq of the items in coll starting from the first\n  item for which (pred item) returns nil." [pred coll] (if (and (seq coll) (pred (first coll))) (recur pred (rest coll)) (seq coll)))
//---
(function __clojure_core_fn_1668(){
return (clojure.JS.def(clojure.core,"drop_while",(function __clojure_core_fn_1668_drop_while_1670(pred_1,coll_2){
var _cnt,_rtn,and__806_3;
do{_cnt=0;_rtn=((((and__806_3=clojure.core.seq.apply(null,[coll_2])),
((and__806_3)?(pred_1.apply(null,[clojure.core.first.apply(null,[coll_2])])):(and__806_3))))?((_cnt=1,_rtn=[pred_1,clojure.core.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])):(clojure.core.seq.apply(null,[coll_2])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn cycle "Returns a lazy (infinite!) seq of repetitions of the items in\n  coll." [coll] (when (seq coll) (let [rep (fn thisfn [xs] (if xs (lazy-cons (first xs) (thisfn (rest xs))) (recur (seq coll))))] (rep (seq coll)))))
//---
(function __clojure_core_fn_1674(){
return (clojure.JS.def(clojure.core,"cycle",(function __clojure_core_fn_1674_cycle_1676(coll_1){
var rep_2;
return (((clojure.core.seq.apply(null,[coll_1]))?(((rep_2=(function __clojure_core_fn_1674_cycle_1676_thisfn_1678(xs_1){
var _cnt,_rtn,thisfn_0=arguments.callee;
do{_cnt=0;_rtn=((xs_1)?((new clojure.lang.LazyCons((function __clojure_core_fn_1674_cycle_1676_thisfn_1678_fn_1680(G__1679_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[xs_1]))}
return (thisfn_0.apply(null,[clojure.core.rest.apply(null,[xs_1])]))})))):((_cnt=1,_rtn=[clojure.core.seq.apply(null,[coll_1])],xs_1=_rtn[0])))
}while(_cnt);return _rtn;})),
rep_2.apply(null,[clojure.core.seq.apply(null,[coll_1])]))):(null)))})))}).apply(null,[]);

//======
//(defn split-at "Returns a vector of [(take n coll) (drop n coll)]" [n coll] [(take n coll) (drop n coll)])
//---
(function __clojure_core_fn_1687(){
return (clojure.JS.def(clojure.core,"split_at",(function __clojure_core_fn_1687_split_at_1689(n_1,coll_2){
return (clojure.JS.lit_vector([clojure.core.take.apply(null,[n_1,coll_2]),clojure.core.drop.apply(null,[n_1,coll_2])]))})))}).apply(null,[]);

//======
//(defn split-with "Returns a vector of [(take-while pred coll) (drop-while pred coll)]" [pred coll] [(take-while pred coll) (drop-while pred coll)])
//---
(function __clojure_core_fn_1693(){
return (clojure.JS.def(clojure.core,"split_with",(function __clojure_core_fn_1693_split_with_1695(pred_1,coll_2){
return (clojure.JS.lit_vector([clojure.core.take_while.apply(null,[pred_1,coll_2]),clojure.core.drop_while.apply(null,[pred_1,coll_2])]))})))}).apply(null,[]);

//======
//(defn repeat "Returns a lazy (infinite!) seq of xs." [x] (lazy-cons x (repeat x)))
//---
(function __clojure_core_fn_1699(){
return (clojure.JS.def(clojure.core,"repeat",(function __clojure_core_fn_1699_repeat_1701(x_1){
return ((new clojure.lang.LazyCons((function __clojure_core_fn_1699_repeat_1701_fn_1704(G__1703_1){switch(arguments.length){
case 0:return (x_1)}
return (clojure.core.repeat.apply(null,[x_1]))}))))})))}).apply(null,[]);

//======
//(defn replicate "Returns a lazy seq of n xs." [n x] (take n (repeat x)))
//---
(function __clojure_core_fn_1710(){
return (clojure.JS.def(clojure.core,"replicate",(function __clojure_core_fn_1710_replicate_1712(n_1,x_2){
return (clojure.core.take.apply(null,[n_1,clojure.core.repeat.apply(null,[x_2])]))})))}).apply(null,[]);

//======
//(defn iterate "Returns a lazy seq of x, (f x), (f (f x)) etc. f must be free of side-effects" [f x] (lazy-cons x (iterate f (f x))))
//---
(function __clojure_core_fn_1716(){
return (clojure.JS.def(clojure.core,"iterate",(function __clojure_core_fn_1716_iterate_1718(f_1,x_2){
return ((new clojure.lang.LazyCons((function __clojure_core_fn_1716_iterate_1718_fn_1721(G__1720_1){switch(arguments.length){
case 0:return (x_2)}
return (clojure.core.iterate.apply(null,[f_1,f_1.apply(null,[x_2])]))}))))})))}).apply(null,[]);

//======
//(defn range "Returns a lazy seq of nums from start (inclusive) to end\n  (exclusive), by step, where start defaults to 0 and step to 1." ([end] (if (and (> end 0) (< end clojure.lang.RT/IntegerMaxValue)) (new clojure.lang.Range 0 end) (take end (iterate inc 0)))) ([start end] (if (and (< start end) (< end clojure.lang.RT/IntegerMaxValue)) (new clojure.lang.Range start end) (take (- end start) (iterate inc start)))) ([start end step] (take-while (partial (if (pos? step) > <) end) (iterate (partial + step) start))))
//---
(function __clojure_core_fn_1727(){
return (clojure.JS.def(clojure.core,"range",(function __clojure_core_fn_1727_range_1729(start_1,end_2,step_3){switch(arguments.length){
case 1:var and__806_2,end_1=arguments[0];
return (((((and__806_2=clojure.lang.Numbers.gt(end_1,(0))),
((and__806_2)?(clojure.lang.Numbers.lt(end_1,clojure.JS.getOrRun(clojure.lang.RT,"IntegerMaxValue"))):(and__806_2))))?((new clojure.lang.Range((0),end_1))):(clojure.core.take.apply(null,[end_1,clojure.core.iterate.apply(null,[clojure.core.inc,(0)])]))))
case 2:var and__806_3;
return (((((and__806_3=clojure.lang.Numbers.lt(start_1,end_2)),
((and__806_3)?(clojure.lang.Numbers.lt(end_2,clojure.JS.getOrRun(clojure.lang.RT,"IntegerMaxValue"))):(and__806_3))))?((new clojure.lang.Range(start_1,end_2))):(clojure.core.take.apply(null,[clojure.lang.Numbers.minus(end_2,start_1),clojure.core.iterate.apply(null,[clojure.core.inc,start_1])]))))}
return (clojure.core.take_while.apply(null,[clojure.core.partial.apply(null,[((clojure.lang.Numbers.isPos(step_3))?(clojure.core._GT_):(clojure.core._LT_)),end_2]),clojure.core.iterate.apply(null,[clojure.core.partial.apply(null,[clojure.core._PLUS_,step_3]),start_1])]))})))}).apply(null,[]);

//======
//(defn merge "Returns a map that consists of the rest of the maps conj-ed onto\n  the first.  If a key occurs in more than one map, the mapping from\n  the latter (left-to-right) will be the mapping in the result." [& maps] (when (some identity maps) (reduce (fn* [p1__1735 p2__1736] (conj (or p1__1735 {}) p2__1736)) maps)))
//---
(function __clojure_core_fn_1737(){
return (clojure.JS.def(clojure.core,"merge",clojure.JS.variadic(0,(function __clojure_core_fn_1737_merge_1739(){
var maps_1=clojure.JS.rest_args(this,arguments,0);
return (((clojure.core.some.apply(null,[clojure.core.identity,maps_1]))?(clojure.core.reduce.apply(null,[(function __clojure_core_fn_1737_merge_1739_fn_1741(p1__1735_1,p2__1736_2){
var or__820_3;
return (clojure.core.conj.apply(null,[((or__820_3=p1__1735_1),
((or__820_3)?(or__820_3):(clojure.lang.PersistentHashMap.EMPTY))),p2__1736_2]))}),maps_1])):(null)))}))))}).apply(null,[]);

//======
//(defn merge-with "Returns a map that consists of the rest of the maps conj-ed onto\n  the first.  If a key occurs in more than one map, the mapping(s)\n  from the latter (left-to-right) will be combined with the mapping in\n  the result by calling (f val-in-result val-in-latter)." [f & maps] (when (some identity maps) (let [merge-entry (fn [m e] (let [k (key e) v (val e)] (if (contains? m k) (assoc m k (f (m k) v)) (assoc m k v)))) merge2 (fn [m1 m2] (reduce merge-entry (or m1 {}) (seq m2)))] (reduce merge2 maps))))
//---
(function __clojure_core_fn_1746(){
return (clojure.JS.def(clojure.core,"merge_with",clojure.JS.variadic(1,(function __clojure_core_fn_1746_merge_with_1748(f_1){
var merge2_4,merge_entry_3,maps_2=clojure.JS.rest_args(this,arguments,1);
return (((clojure.core.some.apply(null,[clojure.core.identity,maps_2]))?(((merge_entry_3=(function __clojure_core_fn_1746_merge_with_1748_merge_entry_1750(m_1,e_2){
var v_4,k_3;
return (((k_3=clojure.core.key.apply(null,[e_2])),
(v_4=clojure.core.val.apply(null,[e_2])),
((clojure.core.contains_QMARK_.apply(null,[m_1,k_3]))?(clojure.core.assoc.apply(null,[m_1,k_3,f_1.apply(null,[m_1.apply(null,[k_3]),v_4])])):(clojure.core.assoc.apply(null,[m_1,k_3,v_4])))))})),
(merge2_4=(function __clojure_core_fn_1746_merge_with_1748_merge2_1753(m1_1,m2_2){
var or__820_3;
return (clojure.core.reduce.apply(null,[merge_entry_3,((or__820_3=m1_1),
((or__820_3)?(or__820_3):(clojure.lang.PersistentHashMap.EMPTY))),clojure.core.seq.apply(null,[m2_2])]))})),
clojure.core.reduce.apply(null,[merge2_4,maps_2]))):(null)))}))))}).apply(null,[]);

//======
//(defn zipmap "Returns a map with the keys mapped to the corresponding vals." [keys vals] (loop [map {} ks (seq keys) vs (seq vals)] (if (and ks vs) (recur (assoc map (first ks) (first vs)) (rest ks) (rest vs)) map)))
//---
(function __clojure_core_fn_1758(){
return (clojure.JS.def(clojure.core,"zipmap",(function __clojure_core_fn_1758_zipmap_1760(keys_1,vals_2){
var and__806_6,ks_4,vs_5,map_3;
return (((function __loop(){var _rtn,_cnt;(map_3=clojure.lang.PersistentHashMap.EMPTY),
(ks_4=clojure.core.seq.apply(null,[keys_1])),
(vs_5=clojure.core.seq.apply(null,[vals_2]));do{_cnt=0;
_rtn=((((and__806_6=ks_4),
((and__806_6)?(vs_5):(and__806_6))))?((_cnt=1,_rtn=[clojure.core.assoc.apply(null,[map_3,clojure.core.first.apply(null,[ks_4]),clojure.core.first.apply(null,[vs_5])]),clojure.core.rest.apply(null,[ks_4]),clojure.core.rest.apply(null,[vs_5])],map_3=_rtn[0],ks_4=_rtn[1],vs_5=_rtn[2])):(map_3))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn line-seq "Returns the lines of text from rdr as a lazy sequence of strings.\n  rdr must implement java.io.BufferedReader." [rdr] (let [line (. rdr (readLine))] (when line (lazy-cons line (line-seq rdr)))))
//---
(function __clojure_core_fn_1764(){
return (clojure.JS.def(clojure.core,"line_seq",(function __clojure_core_fn_1764_line_seq_1766(rdr_1){
var line_2;
return (((line_2=(rdr_1).readLine()),
((line_2)?((new clojure.lang.LazyCons((function __clojure_core_fn_1764_line_seq_1766_fn_1769(G__1768_1){switch(arguments.length){
case 0:return (line_2)}
return (clojure.core.line_seq.apply(null,[rdr_1]))})))):(null))))})))}).apply(null,[]);

//======
//(defn comparator "Returns an implementation of java.util.Comparator based upon pred." [pred] (fn [x y] (cond (pred x y) -1 (pred y x) 1 :else 0)))
//---
(function __clojure_core_fn_1775(){
return (clojure.JS.def(clojure.core,"comparator",(function __clojure_core_fn_1775_comparator_1777(pred_1){
return ((function __clojure_core_fn_1775_comparator_1777_fn_1779(x_1,y_2){
return (((pred_1.apply(null,[x_1,y_2]))?((-1)):(((pred_1.apply(null,[y_2,x_1]))?((1)):(((clojure.core.keyword("","else"))?((0)):(null)))))))}))})))}).apply(null,[]);

//======
//(defn sort "Returns a sorted sequence of the items in coll. If no comparator is\n  supplied, uses compare. comparator must\n  implement java.util.Comparator." ([coll] (sort compare coll)) ([comp coll] (when (and coll (not (zero? (count coll)))) (let [a (to-array coll)] (clojure.lang.RT/sortArray a comp) (seq a)))))
//---
(function __clojure_core_fn_1784(){
return (clojure.JS.def(clojure.core,"sort",(function __clojure_core_fn_1784_sort_1786(comp_1,coll_2){switch(arguments.length){
case 1:var coll_1=arguments[0];
return (clojure.core.sort.apply(null,[clojure.core.compare,coll_1]))}
var and__806_3,a_3;
return (((((and__806_3=coll_2),
((and__806_3)?(clojure.core.not.apply(null,[clojure.lang.Numbers.isZero(clojure.core.count.apply(null,[coll_2]))])):(and__806_3))))?(((a_3=clojure.core.to_array.apply(null,[coll_2])),
clojure.lang.RT.sortArray(a_3,comp_1),
clojure.core.seq.apply(null,[a_3]))):(null)))})))}).apply(null,[]);

//======
//(defn sort-by "Returns a sorted sequence of the items in coll, where the sort\n  order is determined by comparing (keyfn item).  If no comparator is\n  supplied, uses compare. comparator must\n  implement java.util.Comparator." ([keyfn coll] (sort-by keyfn compare coll)) ([keyfn comp coll] (sort (fn [x y] (. comp (compare (keyfn x) (keyfn y)))) coll)))
//---
(function __clojure_core_fn_1791(){
return (clojure.JS.def(clojure.core,"sort_by",(function __clojure_core_fn_1791_sort_by_1793(keyfn_1,comp_2,coll_3){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (clojure.core.sort_by.apply(null,[keyfn_1,clojure.core.compare,coll_2]))}
return (clojure.core.sort.apply(null,[(function __clojure_core_fn_1791_sort_by_1793_fn_1796(x_1,y_2){
return ((comp_2).compare(keyfn_1.apply(null,[x_1]),keyfn_1.apply(null,[y_2])))}),coll_3]))})))}).apply(null,[]);

//======
//(defn partition "Returns a lazy sequence of lists of n items each, at offsets step\n  apart. If step is not supplied, defaults to n, i.e. the partitions\n  do not overlap." ([n coll] (partition n n coll)) ([n step coll] (when (seq coll) (let [p (take n coll)] (when (= n (count p)) (lazy-cons p (partition n step (drop step coll))))))))
//---
(function __clojure_core_fn_1801(){
return (clojure.JS.def(clojure.core,"partition",(function __clojure_core_fn_1801_partition_1803(n_1,step_2,coll_3){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (clojure.core.partition.apply(null,[n_1,n_1,coll_2]))}
var p_4;
return (((clojure.core.seq.apply(null,[coll_3]))?(((p_4=clojure.core.take.apply(null,[n_1,coll_3])),
((clojure.lang.Util.equal(n_1,clojure.core.count.apply(null,[p_4])))?((new clojure.lang.LazyCons((function __clojure_core_fn_1801_partition_1803_fn_1807(G__1806_1){switch(arguments.length){
case 0:return (p_4)}
return (clojure.core.partition.apply(null,[n_1,step_2,clojure.core.drop.apply(null,[step_2,coll_3])]))})))):(null)))):(null)))})))}).apply(null,[]);
// Skipping: (defn eval "Evaluates the form data structure (not text!) and returns the result." [form] (. clojure.lang.Compiler (eval form)))
// Skipping: (defmacro doseq "Repeatedly executes body (presumably for side-effects) with\n  bindings and filtering as provided by \"for\".  Does not retain\n  the head of the sequence. Returns nil." [seq-exprs & body] (let [binds (reduce (fn [binds p] (if (instance? clojure.lang.Keyword (first p)) (conj (pop binds) (apply assoc (peek binds) p)) (conj binds {:name (first p), :init (second p)}))) [] (partition 2 seq-exprs)) emit (fn emit [bind & binds] (clojure.core/concat (clojure.core/list (quote clojure.core/loop)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote sq__1819)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/seq)) (clojure.core/list (:init bind))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (quote sq__1819)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (:name bind)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/first)) (clojure.core/list (quote sq__1819))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (or (:while bind) true)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (or (:when bind) true)) (clojure.core/list (if binds (apply emit binds) (clojure.core/concat (clojure.core/list (quote do)) body))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote recur)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/rest)) (clojure.core/list (quote sq__1819))))))))))))))] (apply emit binds)))

//======
//(defn scan [& args] (throw (clojure.lang.RT/makeException "scan is now called dorun")))
//---
(function __clojure_core_fn_1839(){
return (clojure.JS.def(clojure.core,"scan",clojure.JS.variadic(0,(function __clojure_core_fn_1839_scan_1841(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw clojure.lang.RT.makeException("scan is now called dorun")})())}))))}).apply(null,[]);

//======
//(defn touch [& args] (throw (clojure.lang.RT/makeException "touch is now called doall")))
//---
(function __clojure_core_fn_1845(){
return (clojure.JS.def(clojure.core,"touch",clojure.JS.variadic(0,(function __clojure_core_fn_1845_touch_1847(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw clojure.lang.RT.makeException("touch is now called doall")})())}))))}).apply(null,[]);

//======
//(defn dorun "When lazy sequences are produced via functions that have side\n  effects, any effects other than those needed to produce the first\n  element in the seq do not occur until the seq is consumed. dorun can\n  be used to force any effects. Walks through the successive rests of\n  the seq, does not retain the head and returns nil." ([coll] (when (and (seq coll) (or (first coll) true)) (recur (rest coll)))) ([n coll] (when (and (seq coll) (pos? n) (or (first coll) true)) (recur (dec n) (rest coll)))))
//---
(function __clojure_core_fn_1851(){
return (clojure.JS.def(clojure.core,"dorun",(function __clojure_core_fn_1851_dorun_1853(n_1,coll_2){switch(arguments.length){
case 1:var _cnt,_rtn,and__806_2,or__820_3,coll_1=arguments[0];
do{_cnt=0;_rtn=((((and__806_2=clojure.core.seq.apply(null,[coll_1])),
((and__806_2)?(((or__820_3=clojure.core.first.apply(null,[coll_1])),
((or__820_3)?(or__820_3):(true)))):(and__806_2))))?((_cnt=1,_rtn=[clojure.core.rest.apply(null,[coll_1])],coll_1=_rtn[0])):(null))
}while(_cnt);return _rtn;}
var _cnt,_rtn,or__820_5,and__806_4,and__806_3;
do{_cnt=0;_rtn=((((and__806_3=clojure.core.seq.apply(null,[coll_2])),
((and__806_3)?(((and__806_4=clojure.lang.Numbers.isPos(n_1)),
((and__806_4)?(((or__820_5=clojure.core.first.apply(null,[coll_2])),
((or__820_5)?(or__820_5):(true)))):(and__806_4)))):(and__806_3))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_1),clojure.core.rest.apply(null,[coll_2])],n_1=_rtn[0],coll_2=_rtn[1])):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn doall "When lazy sequences are produced via functions that have side\n  effects, any effects other than those needed to produce the first\n  element in the seq do not occur until the seq is consumed. doall can\n  be used to force any effects. Walks through the successive rests of\n  the seq, retains the head and returns it, thus causing the entire\n  seq to reside in memory at one time." ([coll] (dorun coll) coll) ([n coll] (dorun n coll) coll))
//---
(function __clojure_core_fn_1858(){
return (clojure.JS.def(clojure.core,"doall",(function __clojure_core_fn_1858_doall_1860(n_1,coll_2){switch(arguments.length){
case 1:var coll_1=arguments[0];
return (clojure.core.dorun.apply(null,[coll_1]),
coll_1)}
return (clojure.core.dorun.apply(null,[n_1,coll_2]),
coll_2)})))}).apply(null,[]);
// Skipping: (defn await "Blocks the current thread (indefinitely!) until all actions\n  dispatched thus far, from this thread or agent, to the agent(s) have\n  occurred." [& agents] (when *agent* (throw (clojure.lang.RT/makeException "Can't await in agent action"))) (let [latch (new java.util.concurrent.CountDownLatch (count agents)) count-down (fn [agent] (. latch (countDown)) agent)] (doseq [agent agents] (send agent count-down)) (. latch (await))))

//======
//(defn await1 [a] (when (pos? (.getQueueCount a)) (await a)) a)
//---
(function __clojure_core_fn_1874(){
return (clojure.JS.def(clojure.core,"await1",(function __clojure_core_fn_1874_await1_1876(a_1){
return (((clojure.lang.Numbers.isPos((a_1).getQueueCount()))?(clojure.core.await.apply(null,[a_1])):(null)),
a_1)})))}).apply(null,[]);
// Skipping: (defn await-for "Blocks the current thread until all actions dispatched thus\n  far (from this thread or agent) to the agents have occurred, or the\n  timeout (in milliseconds) has elapsed. Returns nil if returning due\n  to timeout, non-nil otherwise." [timeout-ms & agents] (when *agent* (throw (clojure.lang.RT/makeException "Can't await in agent action"))) (let [latch (new java.util.concurrent.CountDownLatch (count agents)) count-down (fn [agent] (. latch (countDown)) agent)] (doseq [agent agents] (send agent count-down)) (. latch (await timeout-ms (. java.util.concurrent.TimeUnit MILLISECONDS)))))
// Skipping: (defmacro dotimes "bindings => name n\n\n  Repeatedly executes body (presumably for side-effects) with name\n  bound to integers from 0 through n-1." [bindings & body] (if (vector? bindings) (let [i (first bindings) n (second bindings)] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote n__1889)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/int)) (clojure.core/list n)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/loop)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list i) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/int)) (clojure.core/list 0)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/<)) (clojure.core/list i) (clojure.core/list (quote n__1889)))) body (clojure.core/list (clojure.core/concat (clojure.core/list (quote recur)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked-inc)) (clojure.core/list i))))))))))) (throw (IllegalArgumentException. "dotimes now requires a vector for its binding"))))
// Skipping: (defn import "import-list => (package-symbol class-name-symbols*)\n\n  For each name in class-name-symbols, adds a mapping from name to the\n  class named by package.name to the current namespace. Use :import in the ns \n  macro in preference to calling this directly." [& import-lists] (when import-lists (let [ns *ns* pkg (ffirst import-lists) classes (rfirst import-lists)] (doseq [c classes] (. ns (importClass c (. Class (forName (str pkg "." c))))))) (apply import (rest import-lists))))

//======
//(defn into-array "Returns an array with components set to the values in aseq. The array's\n  component type is type if provided, or the type of the first value in\n  aseq if present, or Object. All values in aseq must be compatible with\n  the component type. Class objects for the primitive types can be obtained\n  using, e.g., Integer/TYPE." ([aseq] (clojure.lang.RT/seqToTypedArray (seq aseq))) ([type aseq] (clojure.lang.RT/seqToTypedArray type (seq aseq))))
//---
(function __clojure_core_fn_1905(){
return (clojure.JS.def(clojure.core,"into_array",(function __clojure_core_fn_1905_into_array_1907(type_1,aseq_2){switch(arguments.length){
case 1:var aseq_1=arguments[0];
return (clojure.lang.RT.seqToTypedArray(clojure.core.seq.apply(null,[aseq_1])))}
return (clojure.lang.RT.seqToTypedArray(type_1,clojure.core.seq.apply(null,[aseq_2])))})))}).apply(null,[]);

//======
//(defn into "Returns a new coll consisting of to-coll with all of the items of\n  from-coll conjoined." [to from] (let [ret to items (seq from)] (if items (recur (conj ret (first items)) (rest items)) ret)))
//---
(function __clojure_core_fn_1912(){
return (clojure.JS.def(clojure.core,"into",(function __clojure_core_fn_1912_into_1914(to_1,from_2){
var _cnt,_rtn,ret_3,items_4;
do{_cnt=0;_rtn=((ret_3=to_1),
(items_4=clojure.core.seq.apply(null,[from_2])),
((items_4)?((_cnt=1,_rtn=[clojure.core.conj.apply(null,[ret_3,clojure.core.first.apply(null,[items_4])]),clojure.core.rest.apply(null,[items_4])],to_1=_rtn[0],from_2=_rtn[1])):(ret_3)))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn array [& items] (into-array items))
//---
(function __clojure_core_fn_1918(){
return (clojure.JS.def(clojure.core,"array",clojure.JS.variadic(0,(function __clojure_core_fn_1918_array_1920(){
var items_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.into_array.apply(null,[items_1]))}))))}).apply(null,[]);
// Skipping: (defn class "Returns the Class of x" [x] (if (nil? x) x (. x (getClass))))

//======
//(defn num "Coerce to Number" {:tag Number, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/num)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (num x)))
//---
(function __clojure_core_fn_1930(){
return (clojure.JS.def(clojure.core,"num",(function __clojure_core_fn_1930_num_1935(x_1){
return (clojure.lang.Numbers.num(x_1))})))}).apply(null,[]);

//======
//(defn int "Coerce to int" {:tag Integer, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/intCast)) (clojure.core/list x)))))} [x] (. clojure.lang.RT (intCast x)))
//---
(function __clojure_core_fn_1939(){
return (clojure.JS.def(clojure.core,"int",(function __clojure_core_fn_1939_int_1944(x_1){
return (clojure.lang.RT.intCast(x_1))})))}).apply(null,[]);

//======
//(defn long "Coerce to long" {:tag Long, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/longCast)) (clojure.core/list x)))))} [x] (. x (longValue)))
//---
(function __clojure_core_fn_1948(){
return (clojure.JS.def(clojure.core,"long",(function __clojure_core_fn_1948_long_1953(x_1){
return ((x_1).longValue())})))}).apply(null,[]);

//======
//(defn float "Coerce to float" {:tag Float, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/floatCast)) (clojure.core/list x)))))} [x] (. x (floatValue)))
//---
(function __clojure_core_fn_1957(){
return (clojure.JS.def(clojure.core,"float",(function __clojure_core_fn_1957_float_1962(x_1){
return ((x_1).floatValue())})))}).apply(null,[]);

//======
//(defn double "Coerce to double" {:tag Double, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/doubleCast)) (clojure.core/list x)))))} [x] (. x (doubleValue)))
//---
(function __clojure_core_fn_1966(){
return (clojure.JS.def(clojure.core,"double",(function __clojure_core_fn_1966_double_1971(x_1){
return ((x_1).doubleValue())})))}).apply(null,[]);

//======
//(defn short "Coerce to short" {:tag Short} [x] (. x (shortValue)))
//---
(function __clojure_core_fn_1975(){
return (clojure.JS.def(clojure.core,"short_",(function __clojure_core_fn_1975_short_1977(x_1){
return ((x_1).shortValue())})))}).apply(null,[]);

//======
//(defn byte "Coerce to byte" {:tag Byte} [x] (. x (byteValue)))
//---
(function __clojure_core_fn_1981(){
return (clojure.JS.def(clojure.core,"byte_",(function __clojure_core_fn_1981_byte_1983(x_1){
return ((x_1).byteValue())})))}).apply(null,[]);

//======
//(defn char "Coerce to char" {:tag Character} [x] (. clojure.lang.RT (charCast x)))
//---
(function __clojure_core_fn_1987(){
return (clojure.JS.def(clojure.core,"char_",(function __clojure_core_fn_1987_char_1989(x_1){
return (clojure.lang.RT.charCast(x_1))})))}).apply(null,[]);

//======
//(defn boolean "Coerce to boolean" {:tag Boolean} [x] (if x true false))
//---
(function __clojure_core_fn_1993(){
return (clojure.JS.def(clojure.core,"boolean_",(function __clojure_core_fn_1993_boolean_1995(x_1){
return (((x_1)?(true):(false)))})))}).apply(null,[]);
// Skipping: (defn bigint "Coerce to BigInteger" {:tag BigInteger} [x] (. BigInteger valueOf x))
// Skipping: (defn bigdec "Coerce to BigDecimal" {:tag BigDecimal} [x] (. BigDecimal valueOf x))

//======
//(def print-initialized false)
//---
(function __clojure_core_fn_2011(){
return (clojure.JS.def(clojure.core,"print_initialized",false))}).apply(null,[]);
// Skipping: (defmulti print-method (fn [x writer] (class x)))

//======
//(defmulti print-dup (fn [x writer] (class x)))
//---
(function __clojure_core_fn_2020(){
return (clojure.JS.def(clojure.core,"print_dup",(new clojure.lang.MultiFn((function __clojure_core_fn_2020_fn_2022(x_1,writer_2){
return (clojure.core.class_.apply(null,[x_1]))}),clojure.core.keyword("","default")))))}).apply(null,[]);

//======
//(defn pr-on {:private true} [x w] (if *print-dup* (print-dup x w) (print-method x w)) nil)
//---
(function __clojure_core_fn_2026(){
return (clojure.JS.def(clojure.core,"pr_on",(function __clojure_core_fn_2026_pr_on_2028(x_1,w_2){
return (((clojure.core._STAR_print_dup_STAR_)?(clojure.core.print_dup.apply(null,[x_1,w_2])):(clojure.core.print_method.apply(null,[x_1,w_2]))),
null)})))}).apply(null,[]);

//======
//(defn pr "Prints the object(s) to the output stream that is the current value\n  of *out*.  Prints the object(s), separated by spaces if there is\n  more than one.  By default, pr and prn print in a way that objects\n  can be read by the reader" ([] nil) ([x] (pr-on x *out*)) ([x & more] (pr x) (. *out* (append \space)) (apply pr more)))
//---
(function __clojure_core_fn_2032(){
return (clojure.JS.def(clojure.core,"pr",clojure.JS.variadic(1,(function __clojure_core_fn_2032_pr_2034(x_1){switch(arguments.length){
case 1:return (clojure.core.pr_on.apply(null,[x_1,clojure.core._STAR_out_STAR_]))
case 0:return (null)}
var more_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.core.pr.apply(null,[x_1]),
(clojure.core._STAR_out_STAR_).append(" "),
clojure.core.apply.apply(null,[clojure.core.pr,more_2]))}))))}).apply(null,[]);

//======
//(defn newline "Writes a newline to the output stream that is the current value of\n  *out*" [] (. *out* (append \newline)) nil)
//---
(function __clojure_core_fn_2040(){
return (clojure.JS.def(clojure.core,"newline",(function __clojure_core_fn_2040_newline_2042(){
return ((clojure.core._STAR_out_STAR_).append("\n"),
null)})))}).apply(null,[]);

//======
//(defn flush "Flushes the output stream that is the current value of\n  *out*" [] (. *out* (flush)) nil)
//---
(function __clojure_core_fn_2046(){
return (clojure.JS.def(clojure.core,"flush",(function __clojure_core_fn_2046_flush_2048(){
return ((clojure.core._STAR_out_STAR_).flush(),
null)})))}).apply(null,[]);

//======
//(defn prn "Same as pr followed by (newline). Observes *flush-on-newline*" [& more] (apply pr more) (newline) (when *flush-on-newline* (flush)))
//---
(function __clojure_core_fn_2052(){
return (clojure.JS.def(clojure.core,"prn",clojure.JS.variadic(0,(function __clojure_core_fn_2052_prn_2054(){
var more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[clojure.core.pr,more_1]),
clojure.core.newline.apply(null,[]),
((clojure.core._STAR_flush_on_newline_STAR_)?(clojure.core.flush.apply(null,[])):(null)))}))))}).apply(null,[]);

//======
//(defn print "Prints the object(s) to the output stream that is the current value\n  of *out*.  print and println produce output for human consumption." [& more] (binding [*print-readably* nil] (apply pr more)))
//---
(function __clojure_core_fn_2058(){
return (clojure.JS.def(clojure.core,"print",clojure.JS.variadic(0,(function __clojure_core_fn_2058_print_2060(){
var more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_print_readably_STAR_,null])),
(function __try(){try{var _rtn=(clojure.core.apply.apply(null,[clojure.core.pr,more_1]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())}))))}).apply(null,[]);

//======
//(defn println "Same as print followed by (newline)" [& more] (binding [*print-readably* nil] (apply prn more)))
//---
(function __clojure_core_fn_2064(){
return (clojure.JS.def(clojure.core,"println",clojure.JS.variadic(0,(function __clojure_core_fn_2064_println_2066(){
var more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_print_readably_STAR_,null])),
(function __try(){try{var _rtn=(clojure.core.apply.apply(null,[clojure.core.prn,more_1]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())}))))}).apply(null,[]);

//======
//(defn read "Reads the next object from stream, which must be an instance of\n  java.io.PushbackReader or some derivee.  stream defaults to the\n  current value of *in* ." ([] (read *in*)) ([stream] (read stream true nil)) ([stream eof-error? eof-value] (read stream eof-error? eof-value false)) ([stream eof-error? eof-value recursive?] (. clojure.lang.LispReader (read stream (boolean eof-error?) eof-value recursive?))))
//---
(function __clojure_core_fn_2070(){
return (clojure.JS.def(clojure.core,"read",(function __clojure_core_fn_2070_read_2072(stream_1,eof_error_QMARK__2,eof_value_3,recursive_QMARK__4){switch(arguments.length){
case 1:return (clojure.core.read.apply(null,[stream_1,true,null]))
case 3:return (clojure.core.read.apply(null,[stream_1,eof_error_QMARK__2,eof_value_3,false]))
case 0:return (clojure.core.read.apply(null,[clojure.core._STAR_in_STAR_]))}
return (clojure.lang.LispReader.read(stream_1,clojure.core.boolean_.apply(null,[eof_error_QMARK__2]),eof_value_3,recursive_QMARK__4))})))}).apply(null,[]);

//======
//(defn read-line "Reads the next line from stream that is the current value of *in* ." [] (. *in* (readLine)))
//---
(function __clojure_core_fn_2079(){
return (clojure.JS.def(clojure.core,"read_line",(function __clojure_core_fn_2079_read_line_2081(){
return ((clojure.core._STAR_in_STAR_).readLine())})))}).apply(null,[]);

//======
//(defn read-string "Reads one object from the string s" [s] (clojure.lang.RT/readString s))
//---
(function __clojure_core_fn_2085(){
return (clojure.JS.def(clojure.core,"read_string",(function __clojure_core_fn_2085_read_string_2087(s_1){
return (clojure.lang.RT.readString(s_1))})))}).apply(null,[]);
// Skipping: (defmacro with-open "bindings => name init\n\n  Evaluates body in a try expression with name bound to the value of\n  init, and a finally clause that calls (.close name)." [bindings & body] (if (vector? bindings) (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list bindings) (clojure.core/list (clojure.core/concat (clojure.core/list (quote try)) body (clojure.core/list (clojure.core/concat (clojure.core/list (quote finally)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/.close)) (clojure.core/list (first bindings))))))))) (throw (IllegalArgumentException. "with-open now requires a vector for its binding"))))
// Skipping: (defmacro doto "Evaluates x then calls all of the methods with the supplied\n  arguments in succession on the resulting object, returning it.\n\n  (doto (new java.util.HashMap) (put \"a\" 1) (put \"b\" 2))" [x & members] (let [gx (gensym)] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list gx) (clojure.core/list x)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote do)) (map (fn [m] (list (quote .) gx m)) members))) (clojure.core/list gx))))
// Skipping: (defmacro memfn "Expands into code that creates a fn that expects to be passed an\n  object and any args and calls the named instance method on the\n  object passing the args. Use when you want to treat a Java method as\n  a first-class fn." [name & args] (clojure.core/concat (clojure.core/list (quote clojure.core/fn)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote target__2115)) args))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote target__2115)) (clojure.core/list (clojure.core/concat (clojure.core/list name) args))))))
// Skipping: (defmacro time "Evaluates expr and prints the time it took.  Returns the value of\n expr." [expr] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote start__2125)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote java.lang.System)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/nanoTime)))))) (clojure.core/list (quote ret__2126)) (clojure.core/list expr)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/prn)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/str)) (clojure.core/list "Elapsed time: ") (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core//)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/double)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/-)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote java.lang.System)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/nanoTime)))))) (clojure.core/list (quote start__2125)))))) (clojure.core/list 1000000.0))) (clojure.core/list " msecs"))))) (clojure.core/list (quote ret__2126))))

//======
//(import (quote (java.lang.reflect Array)))
//---
(function __clojure_core_fn_2136(){
return (clojure.core.import_.apply(null,[clojure.JS.lit_list([clojure.core.symbol("java.lang.reflect"),clojure.core.symbol("Array")])]))}).apply(null,[]);

//======
//(import (quote (clojure.lang RT)))
//---
(function __clojure_core_fn_2139(){
return (clojure.core.import_.apply(null,[clojure.JS.lit_list([clojure.core.symbol("clojure.lang"),clojure.core.symbol("RT")])]))}).apply(null,[]);

//======
//(defn alength "Returns the length of the Java array. Works on arrays of all\n  types." {:inline (fn [a] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/alength)) (clojure.core/list a)))))} [array] (. clojure.lang.RT (alength array)))
//---
(function __clojure_core_fn_2142(){
return (clojure.JS.def(clojure.core,"alength",(function __clojure_core_fn_2142_alength_2147(array_1){
return (clojure.lang.RT.alength(array_1))})))}).apply(null,[]);

//======
//(defn aclone "Returns a clone of the Java array. Works on arrays of known\n  types." {:inline (fn [a] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/aclone)) (clojure.core/list a)))))} [array] (. clojure.lang.RT (aclone array)))
//---
(function __clojure_core_fn_2151(){
return (clojure.JS.def(clojure.core,"aclone",(function __clojure_core_fn_2151_aclone_2156(array_1){
return (clojure.lang.RT.aclone(array_1))})))}).apply(null,[]);

//======
//(defn aget "Returns the value at the index/indices. Works on Java arrays of all\n  types." {:inline-arities #{2}, :inline (fn [a i] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/aget)) (clojure.core/list a) (clojure.core/list i)))))} ([array idx] (RT/aget array idx)) ([array idx & idxs] (apply aget (aget array idx) idxs)))
//---
(function __clojure_core_fn_2160(){
return (clojure.JS.def(clojure.core,"aget",clojure.JS.variadic(2,(function __clojure_core_fn_2160_aget_2165(array_1,idx_2){switch(arguments.length){
case 2:return (clojure.lang.RT.aget(array_1,idx_2))}
var idxs_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.apply.apply(null,[clojure.core.aget,clojure.lang.RT.aget(array_1,idx_2),idxs_3]))}))))}).apply(null,[]);

//======
//(defn aset "Sets the value at the index/indices. Works on Java arrays of\n  reference types. Returns val." {:inline-arities #{3}, :inline (fn [a i v] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/aset)) (clojure.core/list a) (clojure.core/list i) (clojure.core/list v)))))} ([array idx val] (RT/aset array idx val) val) ([array idx idx2 & idxv] (apply aset (aget array idx) idx2 idxv)))
//---
(function __clojure_core_fn_2170(){
return (clojure.JS.def(clojure.core,"aset",clojure.JS.variadic(3,(function __clojure_core_fn_2170_aset_2175(array_1,idx_2,idx2_3){switch(arguments.length){
case 3:var val_3=arguments[2];
return (clojure.lang.RT.aset(array_1,idx_2,val_3),
val_3)}
var idxv_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.core.apply.apply(null,[clojure.core.aset,clojure.lang.RT.aget(array_1,idx_2),idx2_3,idxv_4]))}))))}).apply(null,[]);
// Skipping: (defmacro def-aset [name method coerce] (clojure.core/concat (clojure.core/list (quote clojure.core/defn)) (clojure.core/list name) (clojure.core/list (clojure.core/apply clojure.core/hash-map (clojure.core/concat (clojure.core/list :arglists) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list (clojure.core/concat (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote array)) (clojure.core/list (quote idx)) (clojure.core/list (quote val))))) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote array)) (clojure.core/list (quote idx)) (clojure.core/list (quote idx2)) (clojure.core/list (quote &)) (clojure.core/list (quote idxv)))))))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote array__2180)) (clojure.core/list (quote idx__2181)) (clojure.core/list (quote val__2182))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote java.lang.reflect.Array)) (clojure.core/list (clojure.core/concat (clojure.core/list method) (clojure.core/list (quote array__2180)) (clojure.core/list (quote idx__2181)) (clojure.core/list (clojure.core/concat (clojure.core/list coerce) (clojure.core/list (quote val__2182)))))))) (clojure.core/list (quote val__2182)))) (clojure.core/list (clojure.core/concat (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote array__2180)) (clojure.core/list (quote idx__2181)) (clojure.core/list (quote idx2__2183)) (clojure.core/list (quote &)) (clojure.core/list (quote idxv__2184))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/apply)) (clojure.core/list name) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/aget)) (clojure.core/list (quote array__2180)) (clojure.core/list (quote idx__2181)))) (clojure.core/list (quote idx2__2183)) (clojure.core/list (quote idxv__2184))))))))
// Skipping: (def-aset aset-int setInt int)
// Skipping: (def-aset aset-long setLong long)
// Skipping: (def-aset aset-boolean setBoolean boolean)
// Skipping: (def-aset aset-float setFloat float)
// Skipping: (def-aset aset-double setDouble double)
// Skipping: (def-aset aset-short setShort short)
// Skipping: (def-aset aset-byte setByte byte)
// Skipping: (def-aset aset-char setChar char)
// Skipping: (defn make-array "Creates and returns an array of instances of the specified class of\n  the specified dimension(s).  Note that a class object is required.\n  Class objects can be obtained by using their imported or\n  fully-qualified name.  Class objects for the primitive types can be\n  obtained using, e.g., Integer/TYPE." ([type len] (. Array (newInstance type (int len)))) ([type dim & more-dims] (let [dims (cons dim more-dims) dimarray (make-array (. Integer TYPE) (count dims))] (dotimes [i (alength dimarray)] (aset-int dimarray i (nth dims i))) (. Array (newInstance type dimarray)))))
// Skipping: (defn to-array-2d "Returns a (potentially-ragged) 2-dimensional array of Objects\n  containing the contents of coll, which can be any Collection of any\n  Collection." [coll] (let [ret (make-array (. Class (forName "[Ljava.lang.Object;")) (. coll (size)))] (loop [i 0 xs (seq coll)] (when xs (aset ret i (to-array (first xs))) (recur (inc i) (rest xs)))) ret))
// Skipping: (defn macroexpand-1 "If form represents a macro form, returns its expansion,\n  else returns form." [form] (. clojure.lang.Compiler (macroexpand1 form)))
// Skipping: (defn macroexpand "Repeatedly calls macroexpand-1 on form until it no longer\n  represents a macro form, then returns it.  Note neither\n  macroexpand-1 nor macroexpand expand macros in subforms." [form] (let [ex (macroexpand-1 form)] (if (identical? ex form) form (macroexpand ex))))

//======
//(defn create-struct "Returns a structure basis object." [& keys] (. clojure.lang.PersistentStructMap (createSlotMap keys)))
//---
(function __clojure_core_fn_2275(){
return (clojure.JS.def(clojure.core,"create_struct",clojure.JS.variadic(0,(function __clojure_core_fn_2275_create_struct_2277(){
var keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentStructMap.createSlotMap(keys_1))}))))}).apply(null,[]);
// Skipping: (defmacro defstruct "Same as (def name (create-struct keys...))" [name & keys] (clojure.core/concat (clojure.core/list (quote def)) (clojure.core/list name) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/create-struct)) keys))))

//======
//(defn struct-map "Returns a new structmap instance with the keys of the\n  structure-basis. keyvals may contain all, some or none of the basis\n  keys - where values are not supplied they will default to nil.\n  keyvals can also contain keys not in the basis." [s & inits] (. clojure.lang.PersistentStructMap (create s inits)))
//---
(function __clojure_core_fn_2290(){
return (clojure.JS.def(clojure.core,"struct_map",clojure.JS.variadic(1,(function __clojure_core_fn_2290_struct_map_2292(s_1){
var inits_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentStructMap.create(s_1,inits_2))}))))}).apply(null,[]);

//======
//(defn struct "Returns a new structmap instance with the keys of the\n  structure-basis. vals must be supplied for basis keys in order -\n  where values are not supplied they will default to nil." [s & vals] (. clojure.lang.PersistentStructMap (construct s vals)))
//---
(function __clojure_core_fn_2296(){
return (clojure.JS.def(clojure.core,"struct",clojure.JS.variadic(1,(function __clojure_core_fn_2296_struct_2298(s_1){
var vals_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentStructMap.construct(s_1,vals_2))}))))}).apply(null,[]);

//======
//(defn accessor "Returns a fn that, given an instance of a structmap with the basis,\n  returns the value at the key.  The key must be in the basis. The\n  returned function should be (slightly) more efficient than using\n  get, but such use of accessors should be limited to known\n  performance-critical areas." [s key] (. clojure.lang.PersistentStructMap (getAccessor s key)))
//---
(function __clojure_core_fn_2302(){
return (clojure.JS.def(clojure.core,"accessor",(function __clojure_core_fn_2302_accessor_2304(s_1,key_2){
return (clojure.lang.PersistentStructMap.getAccessor(s_1,key_2))})))}).apply(null,[]);

//======
//(defn subvec "Returns a persistent vector of the items in vector from\n  start (inclusive) to end (exclusive).  If end is not supplied,\n  defaults to (count vector). This operation is O(1) and very fast, as\n  the resulting vector shares structure with the original and no\n  trimming is done." ([v start] (subvec v start (count v))) ([v start end] (. clojure.lang.RT (subvec v start end))))
//---
(function __clojure_core_fn_2308(){
return (clojure.JS.def(clojure.core,"subvec",(function __clojure_core_fn_2308_subvec_2310(v_1,start_2,end_3){switch(arguments.length){
case 2:return (clojure.core.subvec.apply(null,[v_1,start_2,clojure.core.count.apply(null,[v_1])]))}
return (clojure.lang.RT.subvec(v_1,start_2,end_3))})))}).apply(null,[]);
// Skipping: (defn load-reader "Sequentially read and evaluate the set of forms contained in the\n  stream/file" [rdr] (. clojure.lang.Compiler (load rdr)))
// Skipping: (defn load-string "Sequentially read and evaluate the set of forms contained in the\n  string" [s] (let [rdr (-> (java.io.StringReader. s) (clojure.lang.LineNumberingPushbackReader.))] (load-reader rdr)))

//======
//(defn resultset-seq "Creates and returns a lazy sequence of structmaps corresponding to\n  the rows in the java.sql.ResultSet rs" [rs] (let [rsmeta (. rs (getMetaData)) idxs (range 1 (inc (. rsmeta (getColumnCount)))) keys (map (comp keyword (memfn toLowerCase)) (map (fn [i] (. rsmeta (getColumnName i))) idxs)) row-struct (apply create-struct keys) row-values (fn [] (map (fn [i] (. rs (getObject i))) idxs)) rows (fn thisfn [] (when (. rs (next)) (lazy-cons (apply struct row-struct (row-values)) (thisfn))))] (rows)))
//---
(function __clojure_core_fn_2327(){
return (clojure.JS.def(clojure.core,"resultset_seq",(function __clojure_core_fn_2327_resultset_seq_2329(rs_1){
var row_values_6,rsmeta_2,keys_4,rows_7,idxs_3,row_struct_5;
return (((rsmeta_2=(rs_1).getMetaData()),
(idxs_3=clojure.core.range.apply(null,[(1),clojure.lang.Numbers.inc((rsmeta_2).getColumnCount())])),
(keys_4=clojure.core.map.apply(null,[clojure.core.comp.apply(null,[clojure.core.keyword,(function __clojure_core_fn_2327_resultset_seq_2329_fn_2331(target__2115_1){
return ((target__2115_1).toLowerCase())})]),clojure.core.map.apply(null,[(function __clojure_core_fn_2327_resultset_seq_2329_fn_2334(i_1){
return ((rsmeta_2).getColumnName(i_1))}),idxs_3])])),
(row_struct_5=clojure.core.apply.apply(null,[clojure.core.create_struct,keys_4])),
(row_values_6=(function __clojure_core_fn_2327_resultset_seq_2329_row_values_2337(){
return (clojure.core.map.apply(null,[(function __clojure_core_fn_2327_resultset_seq_2329_row_values_2337_fn_2339(i_1){
return ((rs_1).getObject(i_1))}),idxs_3]))})),
(rows_7=(function __clojure_core_fn_2327_resultset_seq_2329_thisfn_2343(){
var thisfn_0=arguments.callee;
return ((((rs_1).next())?((new clojure.lang.LazyCons((function __clojure_core_fn_2327_resultset_seq_2329_thisfn_2343_fn_2345(G__2344_1){switch(arguments.length){
case 0:return (clojure.core.apply.apply(null,[clojure.core.struct,row_struct_5,row_values_6.apply(null,[])]))}
return (thisfn_0.apply(null,[]))})))):(null)))})),
rows_7.apply(null,[])))})))}).apply(null,[]);

//======
//(defn set "Returns a set of the distinct elements of coll." [coll] (apply hash-set coll))
//---
(function __clojure_core_fn_2352(){
return (clojure.JS.def(clojure.core,"set",(function __clojure_core_fn_2352_set_2354(coll_1){
return (clojure.core.apply.apply(null,[clojure.core.hash_set,coll_1]))})))}).apply(null,[]);
// Skipping: (defn class? "Returns true if x is an instance of Class" [x] (instance? Class x))

//======
//(defn filter-key [keyfn pred amap] (loop [ret {} es (seq amap)] (if es (if (pred (keyfn (first es))) (recur (assoc ret (key (first es)) (val (first es))) (rest es)) (recur ret (rest es))) ret)))
//---
(function __clojure_core_fn_2364(){
return (clojure.JS.def(clojure.core,"filter_key",(function __clojure_core_fn_2364_filter_key_2366(keyfn_1,pred_2,amap_3){
var es_5,ret_4;
return (((function __loop(){var _rtn,_cnt;(ret_4=clojure.lang.PersistentHashMap.EMPTY),
(es_5=clojure.core.seq.apply(null,[amap_3]));do{_cnt=0;
_rtn=((es_5)?(((pred_2.apply(null,[keyfn_1.apply(null,[clojure.core.first.apply(null,[es_5])])]))?((_cnt=1,_rtn=[clojure.core.assoc.apply(null,[ret_4,clojure.core.key.apply(null,[clojure.core.first.apply(null,[es_5])]),clojure.core.val.apply(null,[clojure.core.first.apply(null,[es_5])])]),clojure.core.rest.apply(null,[es_5])],ret_4=_rtn[0],es_5=_rtn[1])):((_cnt=1,_rtn=[ret_4,clojure.core.rest.apply(null,[es_5])],ret_4=_rtn[0],es_5=_rtn[1])))):(ret_4))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn find-ns "Returns the namespace named by the symbol or nil if it doesn't exist." [sym] (clojure.lang.Namespace/find sym))
//---
(function __clojure_core_fn_2370(){
return (clojure.JS.def(clojure.core,"find_ns",(function __clojure_core_fn_2370_find_ns_2372(sym_1){
return (clojure.lang.Namespace.find(sym_1))})))}).apply(null,[]);

//======
//(defn create-ns "Create a new namespace named by the symbol if one doesn't already\n  exist, returns it or the already-existing namespace of the same\n  name." [sym] (clojure.lang.Namespace/findOrCreate sym))
//---
(function __clojure_core_fn_2376(){
return (clojure.JS.def(clojure.core,"create_ns",(function __clojure_core_fn_2376_create_ns_2378(sym_1){
return (clojure.lang.Namespace.findOrCreate(sym_1))})))}).apply(null,[]);

//======
//(defn remove-ns "Removes the namespace named by the symbol. Use with caution.\n  Cannot be used to remove the clojure namespace." [sym] (clojure.lang.Namespace/remove sym))
//---
(function __clojure_core_fn_2382(){
return (clojure.JS.def(clojure.core,"remove_ns",(function __clojure_core_fn_2382_remove_ns_2384(sym_1){
return (clojure.lang.Namespace.remove(sym_1))})))}).apply(null,[]);

//======
//(defn all-ns "Returns a sequence of all namespaces." [] (clojure.lang.Namespace/all))
//---
(function __clojure_core_fn_2388(){
return (clojure.JS.def(clojure.core,"all_ns",(function __clojure_core_fn_2388_all_ns_2390(){
return (clojure.lang.Namespace.all())})))}).apply(null,[]);

//======
//(defn the-ns [x] (if (instance? clojure.lang.Namespace x) x (or (find-ns x) (throw (RT/makeException (str "No namespace: " x " found"))))))
//---
(function __clojure_core_fn_2394(){
return (clojure.JS.def(clojure.core,"the_ns",(function __clojure_core_fn_2394_the_ns_2396(x_1){
var or__820_2;
return (((clojure.core.instance_QMARK_.apply(null,[clojure.lang.Namespace,x_1]))?(x_1):(((or__820_2=clojure.core.find_ns.apply(null,[x_1])),
((or__820_2)?(or__820_2):((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["No namespace: ",x_1," found"]))})()))))))})))}).apply(null,[]);

//======
//(defn ns-name "Returns the name of the namespace, a symbol." [ns] (.getName (the-ns ns)))
//---
(function __clojure_core_fn_2400(){
return (clojure.JS.def(clojure.core,"ns_name",(function __clojure_core_fn_2400_ns_name_2402(ns_1){
return ((clojure.core.the_ns.apply(null,[ns_1])).getName())})))}).apply(null,[]);

//======
//(defn ns-map "Returns a map of all the mappings for the namespace." [ns] (.getMappings (the-ns ns)))
//---
(function __clojure_core_fn_2406(){
return (clojure.JS.def(clojure.core,"ns_map",(function __clojure_core_fn_2406_ns_map_2408(ns_1){
return ((clojure.core.the_ns.apply(null,[ns_1])).getMappings())})))}).apply(null,[]);

//======
//(defn ns-unmap "Removes the mappings for the symbol from the namespace." [ns sym] (.unmap (the-ns ns) sym))
//---
(function __clojure_core_fn_2412(){
return (clojure.JS.def(clojure.core,"ns_unmap",(function __clojure_core_fn_2412_ns_unmap_2414(ns_1,sym_2){
return ((clojure.core.the_ns.apply(null,[ns_1])).unmap(sym_2))})))}).apply(null,[]);

//======
//(defn ns-publics "Returns a map of the public intern mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (= ns (.ns v)) (.isPublic v))) (ns-map ns))))
//---
(function __clojure_core_fn_2418(){
return (clojure.JS.def(clojure.core,"ns_publics",(function __clojure_core_fn_2418_ns_publics_2420(ns_1){
var ns_2;
return (((ns_2=clojure.core.the_ns.apply(null,[ns_1])),
clojure.core.filter_key.apply(null,[clojure.core.val,(function __clojure_core_fn_2418_ns_publics_2420_fn_2422(v_1){
var and__806_3,and__806_2;
return (((and__806_2=clojure.core.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__806_2)?(((and__806_3=clojure.lang.Util.equal(ns_2,clojure.JS.getOrRun(v_1,"ns"))),
((and__806_3)?((v_1).isPublic()):(and__806_3)))):(and__806_2))))}),clojure.core.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn ns-imports "Returns a map of the import mappings for the namespace." [ns] (filter-key val class? (ns-map ns)))
//---
(function __clojure_core_fn_2427(){
return (clojure.JS.def(clojure.core,"ns_imports",(function __clojure_core_fn_2427_ns_imports_2429(ns_1){
return (clojure.core.filter_key.apply(null,[clojure.core.val,clojure.core.class_QMARK_,clojure.core.ns_map.apply(null,[ns_1])]))})))}).apply(null,[]);
// Skipping: (defn refer "refers to all public vars of ns, subject to filters.\n  filters can include at most one each of:\n\n  :exclude list-of-symbols\n  :only list-of-symbols\n  :rename map-of-fromsymbol-tosymbol\n\n  For each public interned var in the namespace named by the symbol,\n  adds a mapping from the name of the var to the var to the current\n  namespace.  Throws an exception if name is already mapped to\n  something else in the current namespace. Filters can be used to\n  select a subset, via inclusion or exclusion, or to provide a mapping\n  to a symbol different from the var's name, in order to prevent\n  clashes. Use :use in the ns macro in preference to calling this directly." [ns-sym & filters] (let [ns (or (find-ns ns-sym) (throw (RT/makeException (str "No namespace: " ns-sym)))) fs (apply hash-map filters) nspublics (ns-publics ns) rename (or (:rename fs) {}) exclude (set (:exclude fs)) to-do (or (:only fs) (keys nspublics))] (doseq [sym to-do] (when-not (exclude sym) (let [v (nspublics sym)] (when-not v (throw (new java.lang.IllegalAccessError (str sym " is not public")))) (. *ns* (refer (or (rename sym) sym) v)))))))

//======
//(defn ns-refers "Returns a map of the refer mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (not= ns (.ns v)))) (ns-map ns))))
//---
(function __clojure_core_fn_2439(){
return (clojure.JS.def(clojure.core,"ns_refers",(function __clojure_core_fn_2439_ns_refers_2441(ns_1){
var ns_2;
return (((ns_2=clojure.core.the_ns.apply(null,[ns_1])),
clojure.core.filter_key.apply(null,[clojure.core.val,(function __clojure_core_fn_2439_ns_refers_2441_fn_2443(v_1){
var and__806_2;
return (((and__806_2=clojure.core.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__806_2)?(clojure.core.not_EQ_.apply(null,[ns_2,clojure.JS.getOrRun(v_1,"ns")])):(and__806_2))))}),clojure.core.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn ns-interns "Returns a map of the intern mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (= ns (.ns v)))) (ns-map ns))))
//---
(function __clojure_core_fn_2448(){
return (clojure.JS.def(clojure.core,"ns_interns",(function __clojure_core_fn_2448_ns_interns_2450(ns_1){
var ns_2;
return (((ns_2=clojure.core.the_ns.apply(null,[ns_1])),
clojure.core.filter_key.apply(null,[clojure.core.val,(function __clojure_core_fn_2448_ns_interns_2450_fn_2452(v_1){
var and__806_2;
return (((and__806_2=clojure.core.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__806_2)?(clojure.lang.Util.equal(ns_2,clojure.JS.getOrRun(v_1,"ns"))):(and__806_2))))}),clojure.core.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn alias "Add an alias in the current namespace to another\n  namespace. Arguments are two symbols: the alias to be used, and\n  the symbolic name of the target namespace. Use :as in the ns macro in preference \n  to calling this directly." [alias namespace-sym] (.addAlias *ns* alias (find-ns namespace-sym)))
//---
(function __clojure_core_fn_2457(){
return (clojure.JS.def(clojure.core,"alias",(function __clojure_core_fn_2457_alias_2459(alias_1,namespace_sym_2){
return ((clojure.core._STAR_ns_STAR_).addAlias(alias_1,clojure.core.find_ns.apply(null,[namespace_sym_2])))})))}).apply(null,[]);

//======
//(defn ns-aliases "Returns a map of the aliases for the namespace." [ns] (.getAliases (the-ns ns)))
//---
(function __clojure_core_fn_2463(){
return (clojure.JS.def(clojure.core,"ns_aliases",(function __clojure_core_fn_2463_ns_aliases_2465(ns_1){
return ((clojure.core.the_ns.apply(null,[ns_1])).getAliases())})))}).apply(null,[]);

//======
//(defn ns-unalias "Removes the alias for the symbol from the namespace." [ns sym] (.removeAlias (the-ns ns) sym))
//---
(function __clojure_core_fn_2469(){
return (clojure.JS.def(clojure.core,"ns_unalias",(function __clojure_core_fn_2469_ns_unalias_2471(ns_1,sym_2){
return ((clojure.core.the_ns.apply(null,[ns_1])).removeAlias(sym_2))})))}).apply(null,[]);

//======
//(defn take-nth "Returns a lazy seq of every nth item in coll." [n coll] (when (seq coll) (lazy-cons (first coll) (take-nth n (drop n coll)))))
//---
(function __clojure_core_fn_2475(){
return (clojure.JS.def(clojure.core,"take_nth",(function __clojure_core_fn_2475_take_nth_2477(n_1,coll_2){
return (((clojure.core.seq.apply(null,[coll_2]))?((new clojure.lang.LazyCons((function __clojure_core_fn_2475_take_nth_2477_fn_2480(G__2479_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll_2]))}
return (clojure.core.take_nth.apply(null,[n_1,clojure.core.drop.apply(null,[n_1,coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn interleave "Returns a lazy seq of the first item in each coll, then the second\n  etc." [& colls] (apply concat (apply map list colls)))
//---
(function __clojure_core_fn_2486(){
return (clojure.JS.def(clojure.core,"interleave",clojure.JS.variadic(0,(function __clojure_core_fn_2486_interleave_2488(){
var colls_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[clojure.core.concat,clojure.core.apply.apply(null,[clojure.core.map,clojure.core.list,colls_1])]))}))))}).apply(null,[]);

//======
//(defn var-get "Gets the value in the var object" [x] (. x (get)))
//---
(function __clojure_core_fn_2492(){
return (clojure.JS.def(clojure.core,"var_get",(function __clojure_core_fn_2492_var_get_2494(x_1){
return ((x_1).get())})))}).apply(null,[]);

//======
//(defn var-set "Sets the value in the var object to val. The var must be\n thread-locally bound." [x val] (. x (set val)))
//---
(function __clojure_core_fn_2498(){
return (clojure.JS.def(clojure.core,"var_set",(function __clojure_core_fn_2498_var_set_2500(x_1,val_2){
return ((x_1).set(val_2))})))}).apply(null,[]);
// Skipping: (defmacro with-local-vars "varbinding=> symbol init-expr\n\n  Executes the exprs in a context in which the symbols are bound to\n  vars with per-thread bindings to the init-exprs.  The symbols refer\n  to the var objects themselves, and must be accessed with var-get and\n  var-set" [name-vals-vec & body] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (interleave (take-nth 2 name-vals-vec) (repeat (quote (. clojure.lang.Var (create)))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Var)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/pushThreadBindings)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/hash-map)) name-vals-vec)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote try)) body (clojure.core/list (clojure.core/concat (clojure.core/list (quote finally)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Var)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/popThreadBindings))))))))))))
// Skipping: (defn ns-resolve "Returns the var or Class to which a symbol will be resolved in the\n  namespace, else nil.  Note that if the symbol is fully qualified,\n  the var/Class to which it resolves need not be present in the\n  namespace." [ns sym] (clojure.lang.Compiler/maybeResolveIn (the-ns ns) sym))
// Skipping: (defn resolve "same as (ns-resolve *ns* symbol)" [sym] (ns-resolve *ns* sym))

//======
//(defn array-map "Constructs an array-map." ([] (. clojure.lang.PersistentArrayMap EMPTY)) ([& keyvals] (new clojure.lang.PersistentArrayMap (to-array keyvals))))
//---
(function __clojure_core_fn_2525(){
return (clojure.JS.def(clojure.core,"array_map",clojure.JS.variadic(0,(function __clojure_core_fn_2525_array_map_2527(){switch(arguments.length){
case 0:return (clojure.JS.getOrRun(clojure.lang.PersistentArrayMap,"EMPTY"))}
var keyvals_1=clojure.JS.rest_args(this,arguments,0);
return ((new clojure.lang.PersistentArrayMap(clojure.core.to_array.apply(null,[keyvals_1]))))}))))}).apply(null,[]);

//======
//(defn nthrest "Returns the nth rest of coll, (seq coll) when n is 0." [coll n] (loop [n n xs (seq coll)] (if (and xs (pos? n)) (recur (dec n) (rest xs)) xs)))
//---
(function __clojure_core_fn_2532(){
return (clojure.JS.def(clojure.core,"nthrest",(function __clojure_core_fn_2532_nthrest_2534(coll_1,n_2){
var n_3,xs_4,and__806_5;
return (((function __loop(){var _rtn,_cnt;(n_3=n_2),
(xs_4=clojure.core.seq.apply(null,[coll_1]));do{_cnt=0;
_rtn=((((and__806_5=xs_4),
((and__806_5)?(clojure.lang.Numbers.isPos(n_3)):(and__806_5))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_3),clojure.core.rest.apply(null,[xs_4])],n_3=_rtn[0],xs_4=_rtn[1])):(xs_4))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn destructure [bindings] (let [bmap (apply array-map bindings) pb (fn pb [bvec b v] (let [pvec (fn [bvec b val] (let [gvec (gensym "vec__")] (loop [ret (-> bvec (conj gvec) (conj val)) n 0 bs b seen-rest? false] (if (seq bs) (let [firstb (first bs)] (cond (= firstb (quote &)) (recur (pb ret (second bs) (list (quote clojure.core/nthrest) gvec n)) n (rrest bs) true) (= firstb :as) (pb ret (second bs) gvec) :else (if seen-rest? (throw (RT/makeException "Unsupported binding form, only :as can follow & parameter")) (recur (pb ret firstb (list (quote clojure.core/nth) gvec n nil)) (inc n) (rest bs) seen-rest?)))) ret)))) pmap (fn [bvec b v] (let [gmap (or (:as b) (gensym "map__")) defaults (:or b)] (loop [ret (-> bvec (conj gmap) (conj v)) bes (reduce (fn [bes entry] (reduce (fn* [p1__2538 p2__2539] (assoc p1__2538 p2__2539 ((val entry) p2__2539))) (dissoc bes (key entry)) ((key entry) bes))) (dissoc b :as :or) {:keys (fn* [p1__2540] (keyword (str p1__2540))), :syms (fn* [p1__2541] (list (quote quote) p1__2541)), :strs str})] (if (seq bes) (let [bb (key (first bes)) bk (val (first bes)) has-default (contains? defaults bb)] (recur (pb ret bb (if has-default (list (quote clojure.core/get) gmap bk (defaults bb)) (list (quote clojure.core/get) gmap bk))) (rest bes))) ret))))] (cond (symbol? b) (-> bvec (conj b) (conj v)) (vector? b) (pvec bvec b v) (map? b) (pmap bvec b v) :else (throw (RT/makeException (str "Unsupported binding form: " b)))))) process-entry (fn [bvec b] (pb bvec (key b) (val b)))] (if (every? symbol? (keys bmap)) bindings (reduce process-entry [] bmap))))
//---
(function __clojure_core_fn_2542(){
return (clojure.JS.def(clojure.core,"destructure",(function __clojure_core_fn_2542_destructure_2544(bindings_1){
var process_entry_4,bmap_2,pb_3;
return (((bmap_2=clojure.core.apply.apply(null,[clojure.core.array_map,bindings_1])),
(pb_3=(function __clojure_core_fn_2542_destructure_2544_pb_2546(bvec_1,b_2,v_3){
var pvec_4,pmap_5,pb_0=arguments.callee;
return (((pvec_4=(function __clojure_core_fn_2542_destructure_2544_pb_2546_pvec_2547(bvec_1,b_2,val_3){
var ret_5,seen_rest_QMARK__8,bs_7,gvec_4,firstb_9,n_6;
return (((gvec_4=clojure.core.gensym.apply(null,["vec__"])),
((function __loop(){var _rtn,_cnt;(ret_5=clojure.core.conj.apply(null,[clojure.core.conj.apply(null,[bvec_1,gvec_4]),val_3])),
(n_6=(0)),
(bs_7=b_2),
(seen_rest_QMARK__8=false);do{_cnt=0;
_rtn=((clojure.core.seq.apply(null,[bs_7]))?(((firstb_9=clojure.core.first.apply(null,[bs_7])),
((clojure.lang.Util.equal(firstb_9,clojure.core.symbol("&")))?((_cnt=1,_rtn=[pb_0.apply(null,[ret_5,clojure.core.second.apply(null,[bs_7]),clojure.core.list.apply(null,[clojure.core.symbol("clojure.core/nthrest"),gvec_4,n_6])]),n_6,clojure.core.rrest.apply(null,[bs_7]),true],ret_5=_rtn[0],n_6=_rtn[1],bs_7=_rtn[2],seen_rest_QMARK__8=_rtn[3])):(((clojure.lang.Util.equal(firstb_9,clojure.core.keyword("","as")))?(pb_0.apply(null,[ret_5,clojure.core.second.apply(null,[bs_7]),gvec_4])):(((clojure.core.keyword("","else"))?(((seen_rest_QMARK__8)?((function __throw(){throw clojure.lang.RT.makeException("Unsupported binding form, only :as can follow & parameter")})()):((_cnt=1,_rtn=[pb_0.apply(null,[ret_5,firstb_9,clojure.core.list.apply(null,[clojure.core.symbol("clojure.core/nth"),gvec_4,n_6,null])]),clojure.lang.Numbers.inc(n_6),clojure.core.rest.apply(null,[bs_7]),seen_rest_QMARK__8],ret_5=_rtn[0],n_6=_rtn[1],bs_7=_rtn[2],seen_rest_QMARK__8=_rtn[3])))):(null)))))))):(ret_5))}while(_cnt);return _rtn;})())))})),
(pmap_5=(function __clojure_core_fn_2542_destructure_2544_pb_2546_pmap_2550(bvec_1,b_2,v_3){
var or__820_4,gmap_4,ret_6,has_default_10,defaults_5,bb_8,bk_9,bes_7;
return (((gmap_4=((or__820_4=clojure.core.keyword("","as").apply(null,[b_2])),
((or__820_4)?(or__820_4):(clojure.core.gensym.apply(null,["map__"]))))),
(defaults_5=clojure.core.keyword("","or").apply(null,[b_2])),
((function __loop(){var _rtn,_cnt;(ret_6=clojure.core.conj.apply(null,[clojure.core.conj.apply(null,[bvec_1,gmap_4]),v_3])),
(bes_7=clojure.core.reduce.apply(null,[(function __clojure_core_fn_2542_destructure_2544_pb_2546_pmap_2550_fn_2552(bes_1,entry_2){
return (clojure.core.reduce.apply(null,[(function __clojure_core_fn_2542_destructure_2544_pb_2546_pmap_2550_fn_2552_fn_2554(p1__2538_1,p2__2539_2){
return (clojure.core.assoc.apply(null,[p1__2538_1,p2__2539_2,clojure.core.val.apply(null,[entry_2]).apply(null,[p2__2539_2])]))}),clojure.core.dissoc.apply(null,[bes_1,clojure.core.key.apply(null,[entry_2])]),clojure.core.key.apply(null,[entry_2]).apply(null,[bes_1])]))}),clojure.core.dissoc.apply(null,[b_2,clojure.core.keyword("","as"),clojure.core.keyword("","or")]),clojure.core.hash_map(clojure.core.keyword("","keys"),(function __clojure_core_fn_2542_destructure_2544_pb_2546_pmap_2550_fn_2558(p1__2540_1){
return (clojure.core.keyword.apply(null,[clojure.core.str.apply(null,[p1__2540_1])]))}),clojure.core.keyword("","syms"),(function __clojure_core_fn_2542_destructure_2544_pb_2546_pmap_2550_fn_2561(p1__2541_1){
return (clojure.core.list.apply(null,[clojure.core.symbol("quote"),p1__2541_1]))}),clojure.core.keyword("","strs"),clojure.core.str)]));do{_cnt=0;
_rtn=((clojure.core.seq.apply(null,[bes_7]))?(((bb_8=clojure.core.key.apply(null,[clojure.core.first.apply(null,[bes_7])])),
(bk_9=clojure.core.val.apply(null,[clojure.core.first.apply(null,[bes_7])])),
(has_default_10=clojure.core.contains_QMARK_.apply(null,[defaults_5,bb_8])),
(_cnt=1,_rtn=[pb_0.apply(null,[ret_6,bb_8,((has_default_10)?(clojure.core.list.apply(null,[clojure.core.symbol("clojure.core/get"),gmap_4,bk_9,defaults_5.apply(null,[bb_8])])):(clojure.core.list.apply(null,[clojure.core.symbol("clojure.core/get"),gmap_4,bk_9])))]),clojure.core.rest.apply(null,[bes_7])],ret_6=_rtn[0],bes_7=_rtn[1]))):(ret_6))}while(_cnt);return _rtn;})())))})),
((clojure.core.symbol_QMARK_.apply(null,[b_2]))?(clojure.core.conj.apply(null,[clojure.core.conj.apply(null,[bvec_1,b_2]),v_3])):(((clojure.core.vector_QMARK_.apply(null,[b_2]))?(pvec_4.apply(null,[bvec_1,b_2,v_3])):(((clojure.core.map_QMARK_.apply(null,[b_2]))?(pmap_5.apply(null,[bvec_1,b_2,v_3])):(((clojure.core.keyword("","else"))?((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["Unsupported binding form: ",b_2]))})()):(null))))))))))})),
(process_entry_4=(function __clojure_core_fn_2542_destructure_2544_process_entry_2566(bvec_1,b_2){
return (pb_3.apply(null,[bvec_1,clojure.core.key.apply(null,[b_2]),clojure.core.val.apply(null,[b_2])]))})),
((clojure.core.every_QMARK_.apply(null,[clojure.core.symbol_QMARK_,clojure.core.keys.apply(null,[bmap_2])]))?(bindings_1):(clojure.core.reduce.apply(null,[process_entry_4,clojure.lang.PersistentVector.EMPTY,bmap_2])))))})))}).apply(null,[]);
// Skipping: (defmacro let "Evaluates the exprs in a lexical context in which the symbols in\n  the binding-forms are bound to their respective init-exprs or parts\n  therein." [bindings & body] (when (odd? (count bindings)) (throw (Exception. "Odd number of elements in let bindings"))) (clojure.core/concat (clojure.core/list (quote let*)) (clojure.core/list (destructure bindings)) body))
// Skipping: (defmacro fn "(fn name? [params* ] exprs*)\n  (fn name? ([params* ] exprs*)+)\n\n  params => positional-params* , or positional-params* & rest-param\n  positional-param => binding-form\n  rest-param => binding-form\n  name => symbol\n\n  Defines a function" [& sigs] (let [name (if (symbol? (first sigs)) (first sigs) nil) sigs (if name (rest sigs) sigs) sigs (if (vector? (first sigs)) (list sigs) sigs) psig (fn [sig] (let [[params & body] sig] (if (every? symbol? params) sig (loop [params params new-params [] lets []] (if params (if (symbol? (first params)) (recur (rest params) (conj new-params (first params)) lets) (let [gparam (gensym "p__")] (recur (rest params) (conj new-params gparam) (-> lets (conj (first params)) (conj gparam))))) (clojure.core/concat (clojure.core/list new-params) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list lets) body)))))))) new-sigs (map psig sigs)] (with-meta (if name (list* (quote fn*) name new-sigs) (cons (quote fn*) new-sigs)) *macro-meta*)))
// Skipping: (defmacro loop "Evaluates the exprs in a lexical context in which the symbols in\n  the binding-forms are bound to their respective init-exprs or parts\n  therein. Acts as a recur target." [bindings & body] (let [db (destructure bindings)] (if (= db bindings) (clojure.core/concat (clojure.core/list (quote loop*)) (clojure.core/list bindings) body) (let [vs (take-nth 2 (drop 1 bindings)) bs (take-nth 2 bindings) gs (map (fn [b] (if (symbol? b) b (gensym))) bs) bfs (reduce (fn [ret [b v g]] (if (symbol? b) (conj ret g v) (conj ret g v b g))) [] (map vector bs vs gs))] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list bfs) (clojure.core/list (clojure.core/concat (clojure.core/list (quote loop*)) (clojure.core/list (vec (interleave gs gs))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (vec (interleave bs gs))) body)))))))))
// Skipping: (defmacro when-first "bindings => x xs\n\n  Same as (when (seq xs) (let [x (first xs)] body))" [bindings & body] (if (vector? bindings) (let [[x xs] bindings] (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/seq)) (clojure.core/list xs))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list x) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/first)) (clojure.core/list xs)))))) body)))) (throw (IllegalArgumentException. "when-first now requires a vector for its binding"))))
// Skipping: (defmacro lazy-cat "Expands to code which yields a lazy sequence of the concatenation\n  of the supplied colls.  Each coll expr is not evaluated until it is\n  needed." ([coll] (clojure.core/concat (clojure.core/list (quote clojure.core/seq)) (clojure.core/list coll))) ([coll & colls] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote iter__2633)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/fn)) (clojure.core/list (quote iter__2633)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote coll__2634))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/seq)) (clojure.core/list (quote coll__2634)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/lazy-cons)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/first)) (clojure.core/list (quote coll__2634)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote iter__2633)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/rest)) (clojure.core/list (quote coll__2634)))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/lazy-cat)) colls))))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote iter__2633)) (clojure.core/list coll))))))
// Skipping: (defmacro for "List comprehension. Takes a vector of one or more\n binding-form/collection-expr pairs, each followed by an optional filtering\n :when/:while expression (:when test or :while test), and yields a\n lazy sequence of evaluations of expr. Collections are iterated in a\n nested fashion, rightmost fastest, and nested coll-exprs can refer to\n bindings created in prior binding-forms.\n\n (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)]  [x y]))" ([seq-exprs expr] (let [pargs (fn [xs] (loop [ret [] [b e & [w f & wr :as r] :as xs] (seq xs)] (if xs (cond (= w :when) (recur (conj ret {:w :when, :f f, :b b, :e e}) wr) (= w :while) (recur (conj ret {:w :while, :f f, :b b, :e e}) wr) :else (recur (conj ret {:w :while, :f true, :b b, :e e}) r)) (seq ret)))) emit (fn emit [[{w :w, b :b, f :f} & [{ys :e} :as rses]]] (let [giter (gensym "iter__") gxs (gensym "s__")] (clojure.core/concat (clojure.core/list (quote clojure.core/fn)) (clojure.core/list giter) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list gxs)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when-first)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list b) (clojure.core/list gxs)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list f) (clojure.core/list (if rses (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote iterys__2646)) (clojure.core/list (emit rses)) (clojure.core/list (quote fs__2647)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote iterys__2646)) (clojure.core/list ys)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (quote fs__2647)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/lazy-cat)) (clojure.core/list (quote fs__2647)) (clojure.core/list (clojure.core/concat (clojure.core/list giter) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/rest)) (clojure.core/list gxs))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote recur)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/rest)) (clojure.core/list gxs)))))))) (clojure.core/concat (clojure.core/list (quote clojure.core/lazy-cons)) (clojure.core/list expr) (clojure.core/list (clojure.core/concat (clojure.core/list giter) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/rest)) (clojure.core/list gxs)))))))) (clojure.core/list (if (= w :when) (clojure.core/concat (clojure.core/list (quote recur)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/rest)) (clojure.core/list gxs)))) nil)))))))))] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote iter__2648)) (clojure.core/list (emit (pargs seq-exprs)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote iter__2648)) (clojure.core/list (second seq-exprs))))))))
// Skipping: (defmacro comment "Ignores body, yields nil" [& body])
// Skipping: (defmacro with-out-str "Evaluates exprs in a context in which *out* is bound to a fresh\n  StringWriter.  Returns the string created by any nested printing\n  calls." [& body] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote s__2701)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.lang.RT/makeStringWriter))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/binding)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote clojure.core/*out*)) (clojure.core/list (quote s__2701))))) body (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/str)) (clojure.core/list (quote s__2701))))))))
// Skipping: (defmacro with-in-str "Evaluates body in a context in which *in* is bound to a fresh\n  StringReader initialized with the string s." [s & body] (clojure.core/concat (clojure.core/list (quote clojure.core/with-open)) (clojure.core/list (quote s__2711)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/->)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote java.io.StringReader.)) (clojure.core/list s))) (clojure.core/list (quote clojure.lang.LineNumberingPushbackReader.)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/binding)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote clojure.core/*in*)) (clojure.core/list (quote s__2711))))) body))))

//======
//(defn pr-str "pr to a string, returning it" {:tag String} [& xs] (with-out-str (apply pr xs)))
//---
(function __clojure_core_fn_2721(){
return (clojure.JS.def(clojure.core,"pr_str",clojure.JS.variadic(0,(function __clojure_core_fn_2721_pr_str_2723(){
var s__2701_2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__2701_2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_out_STAR_,s__2701_2])),
(function __try(){try{var _rtn=(clojure.core.apply.apply(null,[clojure.core.pr,xs_1]),
clojure.core.str.apply(null,[s__2701_2]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})()))}))))}).apply(null,[]);

//======
//(defn prn-str "prn to a string, returning it" {:tag String} [& xs] (with-out-str (apply prn xs)))
//---
(function __clojure_core_fn_2727(){
return (clojure.JS.def(clojure.core,"prn_str",clojure.JS.variadic(0,(function __clojure_core_fn_2727_prn_str_2729(){
var s__2701_2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__2701_2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_out_STAR_,s__2701_2])),
(function __try(){try{var _rtn=(clojure.core.apply.apply(null,[clojure.core.prn,xs_1]),
clojure.core.str.apply(null,[s__2701_2]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})()))}))))}).apply(null,[]);

//======
//(defn print-str "print to a string, returning it" {:tag String} [& xs] (with-out-str (apply print xs)))
//---
(function __clojure_core_fn_2733(){
return (clojure.JS.def(clojure.core,"print_str",clojure.JS.variadic(0,(function __clojure_core_fn_2733_print_str_2735(){
var s__2701_2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__2701_2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_out_STAR_,s__2701_2])),
(function __try(){try{var _rtn=(clojure.core.apply.apply(null,[clojure.core.print,xs_1]),
clojure.core.str.apply(null,[s__2701_2]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})()))}))))}).apply(null,[]);

//======
//(defn println-str "println to a string, returning it" {:tag String} [& xs] (with-out-str (apply println xs)))
//---
(function __clojure_core_fn_2739(){
return (clojure.JS.def(clojure.core,"println_str",clojure.JS.variadic(0,(function __clojure_core_fn_2739_println_str_2741(){
var s__2701_2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__2701_2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_out_STAR_,s__2701_2])),
(function __try(){try{var _rtn=(clojure.core.apply.apply(null,[clojure.core.println,xs_1]),
clojure.core.str.apply(null,[s__2701_2]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})()))}))))}).apply(null,[]);
// Skipping: (defmacro assert "Evaluates expr and throws an exception if it does not evaluate to\n logical true." [x] (clojure.core/concat (clojure.core/list (quote clojure.core/when-not)) (clojure.core/list x) (clojure.core/list (clojure.core/concat (clojure.core/list (quote throw)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.lang.RT/makeException)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/str)) (clojure.core/list "Assert failed: ") (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/pr-str)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list x)))))))))))))

//======
//(defn test "test [v] finds fn at key :test in var metadata and calls it,\n  presuming failure will throw exception" [v] (let [f (:test (clojure.core/meta v))] (if f (do (f) :ok) :no-test)))
//---
(function __clojure_core_fn_2754(){
return (clojure.JS.def(clojure.core,"test",(function __clojure_core_fn_2754_test_2756(v_1){
var f_2;
return (((f_2=clojure.core.keyword("","test").apply(null,[clojure.core.meta.apply(null,[v_1])])),
((f_2)?(f_2.apply(null,[]),
clojure.core.keyword("","ok")):(clojure.core.keyword("","no-test")))))})))}).apply(null,[]);
// Skipping: (defn re-pattern "Returns an instance of java.util.regex.Pattern, for use, e.g. in\n  re-matcher." {:tag java.util.regex.Pattern} [s] (. java.util.regex.Pattern (compile s)))
// Skipping: (defn re-matcher "Returns an instance of java.util.regex.Matcher, for use, e.g. in\n  re-find." {:tag java.util.regex.Matcher} [re s] (. re (matcher s)))
// Skipping: (defn re-groups "Returns the groups from the most recent match/find. If there are no\n  nested groups, returns a string of the entire match. If there are\n  nested groups, returns a vector of the groups, the first element\n  being the entire match." [m] (let [gc (. m (groupCount))] (if (zero? gc) (. m (group)) (loop [ret [] c 0] (if (<= c gc) (recur (conj ret (. m (group c))) (inc c)) ret)))))
// Skipping: (defn re-seq "Returns a lazy sequence of successive matches of pattern in string,\n  using java.util.regex.Matcher.find(), each such match processed with\n  re-groups." [re s] (let [m (re-matcher re s)] ((fn step [] (when (. m (find)) (lazy-cons (re-groups m) (step)))))))
// Skipping: (defn re-matches "Returns the match, if any, of string to pattern, using\n  java.util.regex.Matcher.matches().  Uses re-groups to return the\n  groups." [re s] (let [m (re-matcher re s)] (when (. m (matches)) (re-groups m))))
// Skipping: (defn re-find "Returns the next regex match, if any, of string to pattern, using\n  java.util.regex.Matcher.find().  Uses re-groups to return the\n  groups." ([m] (when (. m (find)) (re-groups m))) ([re s] (let [m (re-matcher re s)] (re-find m))))

//======
//(defn rand "Returns a random floating point number between 0 (inclusive) and\n  1 (exclusive)." ([] (RT/random)) ([n] (* n (rand))))
//---
(function __clojure_core_fn_2804(){
return (clojure.JS.def(clojure.core,"rand",(function __clojure_core_fn_2804_rand_2806(n_1){switch(arguments.length){
case 0:return (clojure.lang.RT.random())}
return (clojure.lang.Numbers.multiply(n_1,clojure.core.rand.apply(null,[])))})))}).apply(null,[]);

//======
//(defn rand-int "Returns a random integer between 0 (inclusive) and n (exclusive)." [n] (int (rand n)))
//---
(function __clojure_core_fn_2811(){
return (clojure.JS.def(clojure.core,"rand_int",(function __clojure_core_fn_2811_rand_int_2813(n_1){
return (clojure.lang.RT.intCast(clojure.core.rand.apply(null,[n_1])))})))}).apply(null,[]);
// Skipping: (defmacro defn- "same as defn, yielding non-public def" [name & decls] (list* (quote clojure.core/defn) (with-meta name (assoc (meta name) :private true)) decls))

//======
//(defn print-doc [v] (println "-------------------------") (println (str (ns-name (:ns (clojure.core/meta v))) "/" (:name (clojure.core/meta v)))) (prn (:arglists (clojure.core/meta v))) (when (:macro (clojure.core/meta v)) (println "Macro")) (println " " (:doc (clojure.core/meta v))))
//---
(function __clojure_core_fn_2826(){
return (clojure.JS.def(clojure.core,"print_doc",(function __clojure_core_fn_2826_print_doc_2828(v_1){
return (clojure.core.println.apply(null,["-------------------------"]),
clojure.core.println.apply(null,[clojure.core.str.apply(null,[clojure.core.ns_name.apply(null,[clojure.core.keyword("","ns").apply(null,[clojure.core.meta.apply(null,[v_1])])]),"/",clojure.core.keyword("","name").apply(null,[clojure.core.meta.apply(null,[v_1])])])]),
clojure.core.prn.apply(null,[clojure.core.keyword("","arglists").apply(null,[clojure.core.meta.apply(null,[v_1])])]),
((clojure.core.keyword("","macro").apply(null,[clojure.core.meta.apply(null,[v_1])]))?(clojure.core.println.apply(null,["Macro"])):(null)),
clojure.core.println.apply(null,[" ",clojure.core.keyword("","doc").apply(null,[clojure.core.meta.apply(null,[v_1])])]))})))}).apply(null,[]);

//======
//(defn find-doc "Prints documentation for any var whose documentation or name\n contains a match for re-string" [re-string] (let [re (re-pattern re-string)] (dorun (for [ns (all-ns) v (sort-by (comp :name meta) (vals (ns-interns ns))) :when (and (:doc (clojure.core/meta v)) (or (re-find (re-matcher re (:doc (clojure.core/meta v)))) (re-find (re-matcher re (str (:name (clojure.core/meta v)))))))] (print-doc v)))))
//---
(function __clojure_core_fn_2832(){
return (clojure.JS.def(clojure.core,"find_doc",(function __clojure_core_fn_2832_find_doc_2834(re_string_1){
var re_2,iter__2648_3;
return (((re_2=clojure.core.re_pattern.apply(null,[re_string_1])),
clojure.core.dorun.apply(null,[((iter__2648_3=(function __clojure_core_fn_2832_find_doc_2834_iter_2836_2840(s__2837_1){
var _cnt,_rtn,iter__2633_5,iterys__2646_3,ns_2,fs__2647_4,iter__2836_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[s__2837_1]))?(((ns_2=clojure.core.first.apply(null,[s__2837_1])),
((true)?(((iterys__2646_3=(function __clojure_core_fn_2832_find_doc_2834_iter_2836_2840_iter_2838_2841(s__2839_1){
var _cnt,_rtn,and__806_3,or__820_4,v_2,iter__2838_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[s__2839_1]))?(((v_2=clojure.core.first.apply(null,[s__2839_1])),
((((and__806_3=clojure.core.keyword("","doc").apply(null,[clojure.core.meta.apply(null,[v_2])])),
((and__806_3)?(((or__820_4=clojure.core.re_find.apply(null,[clojure.core.re_matcher.apply(null,[re_2,clojure.core.keyword("","doc").apply(null,[clojure.core.meta.apply(null,[v_2])])])])),
((or__820_4)?(or__820_4):(clojure.core.re_find.apply(null,[clojure.core.re_matcher.apply(null,[re_2,clojure.core.str.apply(null,[clojure.core.keyword("","name").apply(null,[clojure.core.meta.apply(null,[v_2])])])])]))))):(and__806_3))))?((new clojure.lang.LazyCons((function __clojure_core_fn_2832_find_doc_2834_iter_2836_2840_iter_2838_2841_fn_2843(G__2842_1){switch(arguments.length){
case 0:return (clojure.core.print_doc.apply(null,[v_2]))}
return (iter__2838_0.apply(null,[clojure.core.rest.apply(null,[s__2839_1])]))})))):((_cnt=1,_rtn=[clojure.core.rest.apply(null,[s__2839_1])],s__2839_1=_rtn[0]))))):(null))
}while(_cnt);return _rtn;})),
(fs__2647_4=iterys__2646_3.apply(null,[clojure.core.sort_by.apply(null,[clojure.core.comp.apply(null,[clojure.core.keyword("","name"),clojure.core.meta]),clojure.core.vals.apply(null,[clojure.core.ns_interns.apply(null,[ns_2])])])])),
((fs__2647_4)?(((iter__2633_5=(function __clojure_core_fn_2832_find_doc_2834_iter_2836_2840_iter_2633_2848(coll__2634_1){
var iter__2633_0=arguments.callee;
return (((clojure.core.seq.apply(null,[coll__2634_1]))?((new clojure.lang.LazyCons((function __clojure_core_fn_2832_find_doc_2834_iter_2836_2840_iter_2633_2848_fn_2850(G__2849_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll__2634_1]))}
return (iter__2633_0.apply(null,[clojure.core.rest.apply(null,[coll__2634_1])]))})))):(clojure.core.seq.apply(null,[iter__2836_0.apply(null,[clojure.core.rest.apply(null,[s__2837_1])])]))))})),
iter__2633_5.apply(null,[fs__2647_4]))):((_cnt=1,_rtn=[clojure.core.rest.apply(null,[s__2837_1])],s__2837_1=_rtn[0]))))):(null)))):(null))
}while(_cnt);return _rtn;})),
iter__2648_3.apply(null,[clojure.core.all_ns.apply(null,[])]))])))})))}).apply(null,[]);

//======
//(defn special-form-anchor "Returns the anchor tag on http://clojure.org/special_forms for the\n  special form x, or nil" [x] (#{(quote recur) (quote .) (quote var) (quote let) (quote quote) (quote set!) (quote monitor-enter) (quote loop) (quote new) (quote fn) (quote if) (quote try) (quote def) (quote monitor-exit) (quote throw) (quote do)} x))
//---
(function __clojure_core_fn_2858(){
return (clojure.JS.def(clojure.core,"special_form_anchor",(function __clojure_core_fn_2858_special_form_anchor_2860(x_1){
return (clojure.core.hash_set(clojure.core.symbol("recur"),clojure.core.symbol("."),clojure.core.symbol("var"),clojure.core.symbol("let"),clojure.core.symbol("quote"),clojure.core.symbol("set!"),clojure.core.symbol("monitor-enter"),clojure.core.symbol("loop"),clojure.core.symbol("new"),clojure.core.symbol("fn"),clojure.core.symbol("if"),clojure.core.symbol("try"),clojure.core.symbol("def"),clojure.core.symbol("monitor-exit"),clojure.core.symbol("throw"),clojure.core.symbol("do")).apply(null,[x_1]))})))}).apply(null,[]);

//======
//(defn syntax-symbol-anchor "Returns the anchor tag on http://clojure.org/special_forms for the\n  special form that uses syntax symbol x, or nil" [x] ({(quote &) (quote fn), (quote catch) (quote try), (quote finally) (quote try)} x))
//---
(function __clojure_core_fn_2864(){
return (clojure.JS.def(clojure.core,"syntax_symbol_anchor",(function __clojure_core_fn_2864_syntax_symbol_anchor_2866(x_1){
return (clojure.core.hash_map(clojure.core.symbol("&"),clojure.core.symbol("fn"),clojure.core.symbol("catch"),clojure.core.symbol("try"),clojure.core.symbol("finally"),clojure.core.symbol("try")).apply(null,[x_1]))})))}).apply(null,[]);

//======
//(defn print-special-doc [name type anchor] (println "-------------------------") (println name) (println type) (println (str "  Please see http://clojure.org/special_forms#" anchor)))
//---
(function __clojure_core_fn_2870(){
return (clojure.JS.def(clojure.core,"print_special_doc",(function __clojure_core_fn_2870_print_special_doc_2872(name_1,type_2,anchor_3){
return (clojure.core.println.apply(null,["-------------------------"]),
clojure.core.println.apply(null,[name_1]),
clojure.core.println.apply(null,[type_2]),
clojure.core.println.apply(null,[clojure.core.str.apply(null,["  Please see http://clojure.org/special_forms#",anchor_3])]))})))}).apply(null,[]);
// Skipping: (defmacro doc "Prints documentation for a var or special form given its name" [name] (cond (special-form-anchor name) (clojure.core/concat (clojure.core/list (quote clojure.core/print-special-doc)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list name))) (clojure.core/list "Special Form") (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/special-form-anchor)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list name)))))) (syntax-symbol-anchor name) (clojure.core/concat (clojure.core/list (quote clojure.core/print-special-doc)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list name))) (clojure.core/list "Syntax Symbol") (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/syntax-symbol-anchor)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list name)))))) :else (clojure.core/concat (clojure.core/list (quote clojure.core/print-doc)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote var)) (clojure.core/list name))))))

//======
//(defn tree-seq "returns a lazy sequence of the nodes in a tree, via a depth-first walk.\n  branch? must be a fn of one arg that returns true if passed a node\n  that can have children (but may not).  children must be a fn of one\n  arg that returns a sequence of the children. Will only be called on\n  nodes for which branch? returns true. Root is the root node of the\n  tree, must be a branch." [branch? children root] (let [walk (fn walk [nodes] (when-first [node nodes] (lazy-cons node (if (branch? node) (lazy-cat (walk (children node)) (walk (rest nodes))) (walk (rest nodes))))))] (lazy-cons root (walk (children root)))))
//---
(function __clojure_core_fn_2885(){
return (clojure.JS.def(clojure.core,"tree_seq",(function __clojure_core_fn_2885_tree_seq_2887(branch_QMARK__1,children_2,root_3){
var walk_4;
return (((walk_4=(function __clojure_core_fn_2885_tree_seq_2887_walk_2889(nodes_1){
var node_2,walk_0=arguments.callee;
return (((clojure.core.seq.apply(null,[nodes_1]))?(((node_2=clojure.core.first.apply(null,[nodes_1])),
(new clojure.lang.LazyCons((function __clojure_core_fn_2885_tree_seq_2887_walk_2889_fn_2891(G__2890_1){switch(arguments.length){
case 0:return (node_2)}
var iter__2633_2;
return (((branch_QMARK__1.apply(null,[node_2]))?(((iter__2633_2=(function __clojure_core_fn_2885_tree_seq_2887_walk_2889_fn_2891_iter_2633_2894(coll__2634_1){
var iter__2633_0=arguments.callee;
return (((clojure.core.seq.apply(null,[coll__2634_1]))?((new clojure.lang.LazyCons((function __clojure_core_fn_2885_tree_seq_2887_walk_2889_fn_2891_iter_2633_2894_fn_2896(G__2895_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll__2634_1]))}
return (iter__2633_0.apply(null,[clojure.core.rest.apply(null,[coll__2634_1])]))})))):(clojure.core.seq.apply(null,[walk_0.apply(null,[clojure.core.rest.apply(null,[nodes_1])])]))))})),
iter__2633_2.apply(null,[walk_0.apply(null,[children_2.apply(null,[node_2])])]))):(walk_0.apply(null,[clojure.core.rest.apply(null,[nodes_1])]))))}))))):(null)))})),
(new clojure.lang.LazyCons((function __clojure_core_fn_2885_tree_seq_2887_fn_2904(G__2903_1){switch(arguments.length){
case 0:return (root_3)}
return (walk_4.apply(null,[children_2.apply(null,[root_3])]))})))))})))}).apply(null,[]);

//======
//(defn file-seq "A tree seq on java.io.Files" [dir] (tree-seq (fn [f] (. f (isDirectory))) (fn [d] (seq (. d (listFiles)))) dir))
//---
(function __clojure_core_fn_2910(){
return (clojure.JS.def(clojure.core,"file_seq",(function __clojure_core_fn_2910_file_seq_2912(dir_1){
return (clojure.core.tree_seq.apply(null,[(function __clojure_core_fn_2910_file_seq_2912_fn_2914(f_1){
return ((f_1).isDirectory())}),(function __clojure_core_fn_2910_file_seq_2912_fn_2917(d_1){
return (clojure.core.seq.apply(null,[(d_1).listFiles()]))}),dir_1]))})))}).apply(null,[]);

//======
//(defn xml-seq "A tree seq on the xml elements as per xml/parse" [root] (tree-seq (complement string?) (comp seq :content) root))
//---
(function __clojure_core_fn_2922(){
return (clojure.JS.def(clojure.core,"xml_seq",(function __clojure_core_fn_2922_xml_seq_2924(root_1){
return (clojure.core.tree_seq.apply(null,[clojure.core.complement.apply(null,[clojure.core.string_QMARK_]),clojure.core.comp.apply(null,[clojure.core.seq,clojure.core.keyword("","content")]),root_1]))})))}).apply(null,[]);
// Skipping: (defn special-symbol? "Returns true if s names a special form" [s] (contains? (. clojure.lang.Compiler specials) s))

//======
//(defn var? "Returns true if v is of type clojure.lang.Var" [v] (instance? clojure.lang.Var v))
//---
(function __clojure_core_fn_2934(){
return (clojure.JS.def(clojure.core,"var_QMARK_",(function __clojure_core_fn_2934_var_QMARK_2936(v_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Var,v_1]))})))}).apply(null,[]);
// Skipping: (defn slurp "Reads the file named by f into a string and returns it." [f] (with-open [r (new java.io.BufferedReader (new java.io.FileReader f))] (let [sb (RT/makeStringBuilder)] (loop [c (. r (read))] (if (neg? c) (str sb) (do (. sb (append (char c))) (recur (. r (read)))))))))

//======
//(defn subs "Returns the substring of s beginning at start inclusive, and ending\n  at end (defaults to length of string), exclusive." ([s start] (. s (substring start))) ([s start end] (. s (substring start end))))
//---
(function __clojure_core_fn_2946(){
return (clojure.JS.def(clojure.core,"subs",(function __clojure_core_fn_2946_subs_2948(s_1,start_2,end_3){switch(arguments.length){
case 2:return ((s_1).substring(start_2))}
return ((s_1).substring(start_2,end_3))})))}).apply(null,[]);

//======
//(defn max-key "Returns the x for which (k x), a number, is greatest." ([k x] x) ([k x y] (if (> (k x) (k y)) x y)) ([k x y & more] (reduce (fn* [p1__2953 p2__2954] (max-key k p1__2953 p2__2954)) (max-key k x y) more)))
//---
(function __clojure_core_fn_2955(){
return (clojure.JS.def(clojure.core,"max_key",clojure.JS.variadic(3,(function __clojure_core_fn_2955_max_key_2957(k_1,x_2,y_3){switch(arguments.length){
case 3:return (((clojure.lang.Numbers.gt(k_1.apply(null,[x_2]),k_1.apply(null,[y_3])))?(x_2):(y_3)))
case 2:return (x_2)}
var more_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.core.reduce.apply(null,[(function __clojure_core_fn_2955_max_key_2957_fn_2961(p1__2953_1,p2__2954_2){
return (clojure.core.max_key.apply(null,[k_1,p1__2953_1,p2__2954_2]))}),clojure.core.max_key.apply(null,[k_1,x_2,y_3]),more_4]))}))))}).apply(null,[]);

//======
//(defn min-key "Returns the x for which (k x), a number, is least." ([k x] x) ([k x y] (if (< (k x) (k y)) x y)) ([k x y & more] (reduce (fn* [p1__2966 p2__2967] (min-key k p1__2966 p2__2967)) (min-key k x y) more)))
//---
(function __clojure_core_fn_2968(){
return (clojure.JS.def(clojure.core,"min_key",clojure.JS.variadic(3,(function __clojure_core_fn_2968_min_key_2970(k_1,x_2,y_3){switch(arguments.length){
case 2:return (x_2)
case 3:return (((clojure.lang.Numbers.lt(k_1.apply(null,[x_2]),k_1.apply(null,[y_3])))?(x_2):(y_3)))}
var more_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.core.reduce.apply(null,[(function __clojure_core_fn_2968_min_key_2970_fn_2974(p1__2966_1,p2__2967_2){
return (clojure.core.min_key.apply(null,[k_1,p1__2966_1,p2__2967_2]))}),clojure.core.min_key.apply(null,[k_1,x_2,y_3]),more_4]))}))))}).apply(null,[]);

//======
//(defn distinct "Returns a lazy sequence of the elements of coll with duplicates removed" [coll] (let [step (fn step [[f & r :as xs] seen] (when xs (if (seen f) (recur r seen) (lazy-cons f (step r (conj seen f))))))] (step (seq coll) #{})))
//---
(function __clojure_core_fn_2979(){
return (clojure.JS.def(clojure.core,"distinct",(function __clojure_core_fn_2979_distinct_2981(coll_1){
var step_2;
return (((step_2=(function __clojure_core_fn_2979_distinct_2981_step_2984(p__2983_1,seen_2){
var _cnt,_rtn,xs_6,vec__2985_3,f_4,r_5,step_0=arguments.callee;
do{_cnt=0;_rtn=((vec__2985_3=p__2983_1),
(f_4=clojure.core.nth.apply(null,[vec__2985_3,(0),null])),
(r_5=clojure.core.nthrest.apply(null,[vec__2985_3,(1)])),
(xs_6=vec__2985_3),
((xs_6)?(((seen_2.apply(null,[f_4]))?((_cnt=1,_rtn=[r_5,seen_2],p__2983_1=_rtn[0],seen_2=_rtn[1])):((new clojure.lang.LazyCons((function __clojure_core_fn_2979_distinct_2981_step_2984_fn_2987(G__2986_1){switch(arguments.length){
case 0:return (f_4)}
return (step_0.apply(null,[r_5,clojure.core.conj.apply(null,[seen_2,f_4])]))})))))):(null)))
}while(_cnt);return _rtn;})),
step_2.apply(null,[clojure.core.seq.apply(null,[coll_1]),clojure.lang.PersistentHashSet.EMPTY])))})))}).apply(null,[]);
// Skipping: (defmacro if-let "bindings => binding-form test\n\n  If test is true, evaluates then with binding-form bound to the value of test, if not, yields else" ([bindings then] (clojure.core/concat (clojure.core/list (quote clojure.core/if-let)) (clojure.core/list bindings) (clojure.core/list then) (clojure.core/list (quote nil)))) ([bindings then else & oldform] (if (and (vector? bindings) (nil? oldform)) (let [[form tst] bindings] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote temp__2994)) (clojure.core/list tst)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (quote temp__2994)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list form) (clojure.core/list (quote temp__2994))))) (clojure.core/list then))) (clojure.core/list else))))) (throw (IllegalArgumentException. "if-let now requires a vector for its binding")))))
// Skipping: (defmacro when-let "bindings => binding-form test\n\n  When test is true, evaluates body with binding-form bound to the value of test" [bindings & body] (if (vector? bindings) (let [[form tst] bindings] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote temp__3008)) (clojure.core/list tst)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (quote temp__3008)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list form) (clojure.core/list (quote temp__3008))))) body)))))) (throw (IllegalArgumentException. "when-let now requires a vector for its binding"))))

//======
//(defn replace "Given a map of replacement pairs and a vector/collection, returns a\n  vector/seq with any elements = a key in smap replaced with the\n  corresponding val in smap" [smap coll] (if (vector? coll) (reduce (fn [v i] (if-let [e (find smap (nth v i))] (assoc v i (val e)) v)) coll (range (count coll))) (map (fn* [p1__3020] (if-let [e (find smap p1__3020)] (val e) p1__3020)) coll)))
//---
(function __clojure_core_fn_3021(){
return (clojure.JS.def(clojure.core,"replace",(function __clojure_core_fn_3021_replace_3023(smap_1,coll_2){
return (((clojure.core.vector_QMARK_.apply(null,[coll_2]))?(clojure.core.reduce.apply(null,[(function __clojure_core_fn_3021_replace_3023_fn_3025(v_1,i_2){
var temp__2994_3,e_4;
return (((temp__2994_3=clojure.core.find.apply(null,[smap_1,clojure.core.nth.apply(null,[v_1,i_2])])),
((temp__2994_3)?(((e_4=temp__2994_3),
clojure.core.assoc.apply(null,[v_1,i_2,clojure.core.val.apply(null,[e_4])]))):(v_1))))}),coll_2,clojure.core.range.apply(null,[clojure.core.count.apply(null,[coll_2])])])):(clojure.core.map.apply(null,[(function __clojure_core_fn_3021_replace_3023_fn_3028(p1__3020_1){
var e_3,temp__2994_2;
return (((temp__2994_2=clojure.core.find.apply(null,[smap_1,p1__3020_1])),
((temp__2994_2)?(((e_3=temp__2994_2),
clojure.core.val.apply(null,[e_3]))):(p1__3020_1))))}),coll_2]))))})))}).apply(null,[]);
// Skipping: (defmacro dosync "Runs the exprs (in an implicit do) in a transaction that encompasses\n  exprs and any nested calls.  Starts a transaction if none is already\n  running on this thread. Any uncaught exception will abort the\n  transaction and flow out of dosync. The exprs may be run more than\n  once, but any effects on Refs will be atomic." [& exprs] (clojure.core/concat (clojure.core/list (quote clojure.core/sync)) (clojure.core/list (quote nil)) exprs))
// Skipping: (defmacro with-precision "Sets the precision and rounding mode to be used for BigDecimal operations.\n\n  Usage: (with-precision 10 (/ 1M 3))\n  or:    (with-precision 10 :rounding HALF_DOWN (/ 1M 3))\n  \n  The rounding mode is one of CEILING, FLOOR, HALF_UP, HALF_DOWN,\n  HALF_EVEN, UP, DOWN and UNNECESSARY; it defaults to HALF_UP." [precision & exprs] (let [[body rm] (if (= (first exprs) :rounding) [(rest (rest exprs)) (clojure.core/concat (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote java.math.RoundingMode)) (clojure.core/list (second exprs)))))] [exprs nil])] (clojure.core/concat (clojure.core/list (quote clojure.core/binding)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote clojure.core/*math-context*)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote java.math.MathContext.)) (clojure.core/list precision) rm))))) body)))

//======
//(defn bound-fn {:private true} [sc test key] (fn [e] (test (.. sc comparator (compare (. sc entryKey e) key)) 0)))
//---
(function __clojure_core_fn_3053(){
return (clojure.JS.def(clojure.core,"bound_fn",(function __clojure_core_fn_3053_bound_fn_3055(sc_1,test_2,key_3){
return ((function __clojure_core_fn_3053_bound_fn_3055_fn_3057(e_1){
return (test_2.apply(null,[((sc_1).comparator()).compare((sc_1).entryKey(e_1),key_3),(0)]))}))})))}).apply(null,[]);

//======
//(defn subseq "sc must be a sorted collection, test(s) one of <, <=, > or\n  >=. Returns a seq of those entries with keys ek for\n  which (test (.. sc comparator (compare ek key)) 0) is true" ([sc test key] (let [include (bound-fn sc test key)] (if (#{> >=} test) (when-let [[e :as s] (. sc seqFrom key true)] (if (include e) s (rest s))) (take-while include (. sc seq true))))) ([sc start-test start-key end-test end-key] (when-let [[e :as s] (. sc seqFrom start-key true)] (take-while (bound-fn sc end-test end-key) (if ((bound-fn sc start-test start-key) e) s (rest s))))))
//---
(function __clojure_core_fn_3062(){
return (clojure.JS.def(clojure.core,"subseq",(function __clojure_core_fn_3062_subseq_3064(sc_1,start_test_2,start_key_3,end_test_4,end_key_5){switch(arguments.length){
case 3:var s_8,e_7,temp__3008_5,vec__3066_6,include_4,test_2=arguments[1],key_3=arguments[2];
return (((include_4=clojure.core.bound_fn.apply(null,[sc_1,test_2,key_3])),
((clojure.core.hash_set(clojure.core._GT_,clojure.core._GT__EQ_).apply(null,[test_2]))?(((temp__3008_5=(sc_1).seqFrom(key_3,true)),
((temp__3008_5)?(((vec__3066_6=temp__3008_5),
(e_7=clojure.core.nth.apply(null,[vec__3066_6,(0),null])),
(s_8=vec__3066_6),
((include_4.apply(null,[e_7]))?(s_8):(clojure.core.rest.apply(null,[s_8]))))):(null)))):(clojure.core.take_while.apply(null,[include_4,(sc_1).seq(true)])))))}
var vec__3068_7,temp__3008_6,s_9,e_8;
return (((temp__3008_6=(sc_1).seqFrom(start_key_3,true)),
((temp__3008_6)?(((vec__3068_7=temp__3008_6),
(e_8=clojure.core.nth.apply(null,[vec__3068_7,(0),null])),
(s_9=vec__3068_7),
clojure.core.take_while.apply(null,[clojure.core.bound_fn.apply(null,[sc_1,end_test_4,end_key_5]),((clojure.core.bound_fn.apply(null,[sc_1,start_test_2,start_key_3]).apply(null,[e_8]))?(s_9):(clojure.core.rest.apply(null,[s_9])))]))):(null))))})))}).apply(null,[]);

//======
//(defn rsubseq "sc must be a sorted collection, test(s) one of <, <=, > or\n  >=. Returns a reverse seq of those entries with keys ek for\n  which (test (.. sc comparator (compare ek key)) 0) is true" ([sc test key] (let [include (bound-fn sc test key)] (if (#{< <=} test) (when-let [[e :as s] (. sc seqFrom key false)] (if (include e) s (rest s))) (take-while include (. sc seq false))))) ([sc start-test start-key end-test end-key] (when-let [[e :as s] (. sc seqFrom end-key false)] (take-while (bound-fn sc start-test start-key) (if ((bound-fn sc end-test end-key) e) s (rest s))))))
//---
(function __clojure_core_fn_3071(){
return (clojure.JS.def(clojure.core,"rsubseq",(function __clojure_core_fn_3071_rsubseq_3073(sc_1,start_test_2,start_key_3,end_test_4,end_key_5){switch(arguments.length){
case 3:var include_4,temp__3008_5,s_8,vec__3075_6,e_7,test_2=arguments[1],key_3=arguments[2];
return (((include_4=clojure.core.bound_fn.apply(null,[sc_1,test_2,key_3])),
((clojure.core.hash_set(clojure.core._LT_,clojure.core._LT__EQ_).apply(null,[test_2]))?(((temp__3008_5=(sc_1).seqFrom(key_3,false)),
((temp__3008_5)?(((vec__3075_6=temp__3008_5),
(e_7=clojure.core.nth.apply(null,[vec__3075_6,(0),null])),
(s_8=vec__3075_6),
((include_4.apply(null,[e_7]))?(s_8):(clojure.core.rest.apply(null,[s_8]))))):(null)))):(clojure.core.take_while.apply(null,[include_4,(sc_1).seq(false)])))))}
var temp__3008_6,vec__3077_7,s_9,e_8;
return (((temp__3008_6=(sc_1).seqFrom(end_key_5,false)),
((temp__3008_6)?(((vec__3077_7=temp__3008_6),
(e_8=clojure.core.nth.apply(null,[vec__3077_7,(0),null])),
(s_9=vec__3077_7),
clojure.core.take_while.apply(null,[clojure.core.bound_fn.apply(null,[sc_1,start_test_2,start_key_3]),((clojure.core.bound_fn.apply(null,[sc_1,end_test_4,end_key_5]).apply(null,[e_8]))?(s_9):(clojure.core.rest.apply(null,[s_9])))]))):(null))))})))}).apply(null,[]);

//======
//(defn repeatedly "Takes a function of no args, presumably with side effects, and returns an infinite\n  lazy sequence of calls to it" [f] (lazy-cons (f) (repeatedly f)))
//---
(function __clojure_core_fn_3080(){
return (clojure.JS.def(clojure.core,"repeatedly",(function __clojure_core_fn_3080_repeatedly_3082(f_1){
return ((new clojure.lang.LazyCons((function __clojure_core_fn_3080_repeatedly_3082_fn_3085(G__3084_1){switch(arguments.length){
case 0:return (f_1.apply(null,[]))}
return (clojure.core.repeatedly.apply(null,[f_1]))}))))})))}).apply(null,[]);

//======
//(defn add-classpath "Adds the url (String or URL object) to the classpath per URLClassLoader.addURL" [url] (. clojure.lang.RT addURL url))
//---
(function __clojure_core_fn_3091(){
return (clojure.JS.def(clojure.core,"add_classpath",(function __clojure_core_fn_3091_add_classpath_3093(url_1){
return (clojure.lang.RT.addURL(url_1))})))}).apply(null,[]);

//======
//(defn hash "Returns the hash code of its argument" [x] (. clojure.lang.Util (hash x)))
//---
(function __clojure_core_fn_3097(){
return (clojure.JS.def(clojure.core,"hash",(function __clojure_core_fn_3097_hash_3099(x_1){
return (clojure.lang.Util.hash(x_1))})))}).apply(null,[]);

//======
//(defn interpose "Returns a lazy seq of the elements of coll separated by sep" [sep coll] (drop 1 (interleave (repeat sep) coll)))
//---
(function __clojure_core_fn_3103(){
return (clojure.JS.def(clojure.core,"interpose",(function __clojure_core_fn_3103_interpose_3105(sep_1,coll_2){
return (clojure.core.drop.apply(null,[(1),clojure.core.interleave.apply(null,[clojure.core.repeat.apply(null,[sep_1]),coll_2])]))})))}).apply(null,[]);
// Skipping: (defmacro definline "Experimental - like defmacro, except defines a named function whose\n  body is the expansion, calls to which may be expanded inline as if\n  it were a macro. Cannot be used with variadic (&) args." [name & decl] (let [[args expr] (drop-while (comp not vector?) decl) inline (eval (list (quote fn) args expr))] (clojure.core/concat (clojure.core/list (quote do)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/defn)) (clojure.core/list name) (clojure.core/list args) (clojure.core/list (apply inline args)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote v__3109)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote var)) (clojure.core/list name)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/.setMeta)) (clojure.core/list (quote v__3109)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/assoc)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/meta)) (clojure.core/list (quote v__3109)))) (clojure.core/list :inline) (clojure.core/list inline))))))))))

//======
//(defn empty "Returns an empty collection of the same category as coll, or nil" [coll] (.empty coll))
//---
(function __clojure_core_fn_3121(){
return (clojure.JS.def(clojure.core,"empty",(function __clojure_core_fn_3121_empty_3123(coll_1){
return ((coll_1).empty())})))}).apply(null,[]);
// Skipping: (defmacro amap "Maps an expression across an array a, using an index named idx, and\n  return value named ret, initialized to a clone of a, then setting each element of\n  ret to the evaluation of expr, returning the new array ret." [a idx ret expr] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote a__3127)) (clojure.core/list a) (clojure.core/list ret) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/aclone)) (clojure.core/list (quote a__3127))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/loop)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list idx) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/int)) (clojure.core/list 0)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/<)) (clojure.core/list idx) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/alength)) (clojure.core/list (quote a__3127)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote do)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/aset)) (clojure.core/list ret) (clojure.core/list idx) (clojure.core/list expr))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote recur)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked-inc)) (clojure.core/list idx))))))) (clojure.core/list ret)))))))
// Skipping: (defmacro areduce "Reduces an expression across an array a, using an index named idx,\n  and return value named ret, initialized to init, setting ret to the evaluation of expr at\n  each step, returning ret." [a idx ret init expr] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote a__3137)) (clojure.core/list a)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/loop)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list idx) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/int)) (clojure.core/list 0))) (clojure.core/list ret) (clojure.core/list init)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/<)) (clojure.core/list idx) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/alength)) (clojure.core/list (quote a__3137)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote recur)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked-inc)) (clojure.core/list idx))) (clojure.core/list expr))) (clojure.core/list ret)))))))
// Skipping: (defn float-array "Creates an array of floats" {:inline-arities #{1 2}, :inline (fn [& args] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/float_array)) args))} ([size-or-seq] (. clojure.lang.Numbers float_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers float_array size init-val-or-seq)))
// Skipping: (defn double-array "Creates an array of doubles" {:inline-arities #{1 2}, :inline (fn [& args] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/double_array)) args))} ([size-or-seq] (. clojure.lang.Numbers double_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers double_array size init-val-or-seq)))
// Skipping: (defn int-array "Creates an array of ints" {:inline-arities #{1 2}, :inline (fn [& args] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/int_array)) args))} ([size-or-seq] (. clojure.lang.Numbers int_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers int_array size init-val-or-seq)))
// Skipping: (defn long-array "Creates an array of ints" {:inline-arities #{1 2}, :inline (fn [& args] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/long_array)) args))} ([size-or-seq] (. clojure.lang.Numbers long_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers long_array size init-val-or-seq)))
// Skipping: (definline floats "Casts to float[]" [xs] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/floats)) (clojure.core/list xs)))
// Skipping: (definline ints "Casts to int[]" [xs] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/ints)) (clojure.core/list xs)))
// Skipping: (definline doubles "Casts to double[]" [xs] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/doubles)) (clojure.core/list xs)))
// Skipping: (definline longs "Casts to long[]" [xs] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/longs)) (clojure.core/list xs)))

//======
//(import (quote (java.util.concurrent BlockingQueue LinkedBlockingQueue)))
//---
(function __clojure_core_fn_3187(){
return (clojure.core.import_.apply(null,[clojure.JS.lit_list([clojure.core.symbol("java.util.concurrent"),clojure.core.symbol("BlockingQueue"),clojure.core.symbol("LinkedBlockingQueue")])]))}).apply(null,[]);
// Skipping: (defn seque "Creates a queued seq on another (presumably lazy) seq s. The queued\n  seq will produce a concrete seq in the background, and can get up to\n  n items ahead of the consumer. n-or-q can be an integer n buffer\n  size, or an instance of java.util.concurrent BlockingQueue. Note\n  that reading from a seque can block if the reader gets ahead of the\n  producer." ([s] (seque 100 s)) ([n-or-q s] (let [q (if (instance? BlockingQueue n-or-q) n-or-q (LinkedBlockingQueue. (int n-or-q))) NIL (Object.) agt (agent (seq s)) fill (fn [s] (try (loop [[x & xs :as s] s] (if s (if (.offer q (if (nil? x) NIL x)) (recur xs) s) (.put q q))) (catch Exception e (.put q q) (throw e)))) drain (fn drain [] (let [x (.take q)] (if (identical? x q) (clojure.core/deref agt) (do (send-off agt fill) (lazy-cons (if (identical? x NIL) nil x) (drain))))))] (send-off agt fill) (drain))))

//======
//(defn alter-var-root "Atomically alters the root binding of var v by applying f to its\n  current value plus any args" [v f & args] (.alterRoot v f args))
//---
(function __clojure_core_fn_3211(){
return (clojure.JS.def(clojure.core,"alter_var_root",clojure.JS.variadic(2,(function __clojure_core_fn_3211_alter_var_root_3213(v_1,f_2){
var args_3=clojure.JS.rest_args(this,arguments,2);
return ((v_1).alterRoot(f_2,args_3))}))))}).apply(null,[]);

//======
//(defn make-hierarchy "Creates a hierarchy object for use with derive, isa? etc." [] {:ancestors {}, :descendants {}, :parents {}})
//---
(function __clojure_core_fn_3217(){
return (clojure.JS.def(clojure.core,"make_hierarchy",(function __clojure_core_fn_3217_make_hierarchy_3219(){
return (clojure.core.hash_map(clojure.core.keyword("","ancestors"),clojure.lang.PersistentHashMap.EMPTY,clojure.core.keyword("","descendants"),clojure.lang.PersistentHashMap.EMPTY,clojure.core.keyword("","parents"),clojure.lang.PersistentHashMap.EMPTY))})))}).apply(null,[]);

//======
//(def global-hierarchy (make-hierarchy))
//---
(function __clojure_core_fn_3223(){
return (clojure.JS.def(clojure.core,"global_hierarchy",clojure.core.make_hierarchy.apply(null,[])))}).apply(null,[]);

//======
//(defn not-empty "If coll is empty, returns nil, else coll" [coll] (when (seq coll) coll))
//---
(function __clojure_core_fn_3226(){
return (clojure.JS.def(clojure.core,"not_empty",(function __clojure_core_fn_3226_not_empty_3228(coll_1){
return (((clojure.core.seq.apply(null,[coll_1]))?(coll_1):(null)))})))}).apply(null,[]);

//======
//(defn bases "Returns the immediate superclass and direct interfaces of c, if any" [c] (let [i (.getInterfaces c) s (.getSuperclass c)] (not-empty (if s (cons s i) i))))
//---
(function __clojure_core_fn_3232(){
return (clojure.JS.def(clojure.core,"bases",(function __clojure_core_fn_3232_bases_3234(c_1){
var s_3,i_2;
return (((i_2=(c_1).getInterfaces()),
(s_3=(c_1).getSuperclass()),
clojure.core.not_empty.apply(null,[((s_3)?(clojure.core.cons.apply(null,[s_3,i_2])):(i_2))])))})))}).apply(null,[]);

//======
//(defn supers "Returns the immediate and indirect superclasses and interfaces of c, if any" [class] (loop [ret (set (bases class)) cs ret] (if (seq cs) (let [c (first cs) bs (bases c)] (recur (into ret bs) (into (disj cs c) bs))) (not-empty ret))))
//---
(function __clojure_core_fn_3238(){
return (clojure.JS.def(clojure.core,"supers",(function __clojure_core_fn_3238_supers_3240(class_1){
var cs_3,bs_5,c_4,ret_2;
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.core.set.apply(null,[clojure.core.bases.apply(null,[class_1])])),
(cs_3=ret_2);do{_cnt=0;
_rtn=((clojure.core.seq.apply(null,[cs_3]))?(((c_4=clojure.core.first.apply(null,[cs_3])),
(bs_5=clojure.core.bases.apply(null,[c_4])),
(_cnt=1,_rtn=[clojure.core.into.apply(null,[ret_2,bs_5]),clojure.core.into.apply(null,[clojure.core.disj.apply(null,[cs_3,c_4]),bs_5])],ret_2=_rtn[0],cs_3=_rtn[1]))):(clojure.core.not_empty.apply(null,[ret_2])))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn isa? "Returns true if (= child parent), or child is directly or indirectly derived from\n  parent, either via a Java type inheritance relationship or a\n  relationship established via derive. h must be a hierarchy obtained\n  from make-hierarchy, if not supplied defaults to the global\n  hierarchy" ([child parent] (isa? global-hierarchy child parent)) ([h child parent] (or (= child parent) (and (class? parent) (class? child) (. parent isAssignableFrom child)) (contains? ((:ancestors h) child) parent) (and (class? child) (some (fn* [p1__3244] (contains? ((:ancestors h) p1__3244) parent)) (supers child))) (and (vector? parent) (vector? child) (= (count parent) (count child)) (loop [ret true i 0] (if (or (not ret) (= i (count parent))) ret (recur (isa? h (child i) (parent i)) (inc i))))))))
//---
(function __clojure_core_fn_3245(){
return (clojure.JS.def(clojure.core,"isa_QMARK_",(function __clojure_core_fn_3245_isa_QMARK_3247(h_1,child_2,parent_3){switch(arguments.length){
case 2:var child_1=arguments[0],parent_2=arguments[1];
return (clojure.core.isa_QMARK_.apply(null,[clojure.core.global_hierarchy,child_1,parent_2]))}
var and__806_8,and__806_6,or__820_6,or__820_13,or__820_7,or__820_4,i_12,or__820_5,and__806_5,ret_11,and__806_9,and__806_10,and__806_7;
return (((or__820_4=clojure.lang.Util.equal(child_2,parent_3)),
((or__820_4)?(or__820_4):(((or__820_5=((and__806_5=clojure.core.class_QMARK_.apply(null,[parent_3])),
((and__806_5)?(((and__806_6=clojure.core.class_QMARK_.apply(null,[child_2])),
((and__806_6)?((parent_3).isAssignableFrom(child_2)):(and__806_6)))):(and__806_5)))),
((or__820_5)?(or__820_5):(((or__820_6=clojure.core.contains_QMARK_.apply(null,[clojure.core.keyword("","ancestors").apply(null,[h_1]).apply(null,[child_2]),parent_3])),
((or__820_6)?(or__820_6):(((or__820_7=((and__806_7=clojure.core.class_QMARK_.apply(null,[child_2])),
((and__806_7)?(clojure.core.some.apply(null,[(function __clojure_core_fn_3245_isa_QMARK_3247_fn_3250(p1__3244_1){
return (clojure.core.contains_QMARK_.apply(null,[clojure.core.keyword("","ancestors").apply(null,[h_1]).apply(null,[p1__3244_1]),parent_3]))}),clojure.core.supers.apply(null,[child_2])])):(and__806_7)))),
((or__820_7)?(or__820_7):(((and__806_8=clojure.core.vector_QMARK_.apply(null,[parent_3])),
((and__806_8)?(((and__806_9=clojure.core.vector_QMARK_.apply(null,[child_2])),
((and__806_9)?(((and__806_10=clojure.lang.Util.equal(clojure.core.count.apply(null,[parent_3]),clojure.core.count.apply(null,[child_2]))),
((and__806_10)?(((function __loop(){var _rtn,_cnt;(ret_11=true),
(i_12=(0));do{_cnt=0;
_rtn=((((or__820_13=clojure.core.not.apply(null,[ret_11])),
((or__820_13)?(or__820_13):(clojure.lang.Util.equal(i_12,clojure.core.count.apply(null,[parent_3]))))))?(ret_11):((_cnt=1,_rtn=[clojure.core.isa_QMARK_.apply(null,[h_1,child_2.apply(null,[i_12]),parent_3.apply(null,[i_12])]),clojure.lang.Numbers.inc(i_12)],ret_11=_rtn[0],i_12=_rtn[1])))}while(_cnt);return _rtn;})())):(and__806_10)))):(and__806_9)))):(and__806_8))))))))))))))))})))}).apply(null,[]);

//======
//(defn parents "Returns the immediate parents of tag, either via a Java type\n  inheritance relationship or a relationship established via derive. h\n  must be a hierarchy obtained from make-hierarchy, if not supplied\n  defaults to the global hierarchy" ([tag] (parents global-hierarchy tag)) ([h tag] (not-empty (let [tp (get (:parents h) tag)] (if (class? tag) (into (set (bases tag)) tp) tp)))))
//---
(function __clojure_core_fn_3255(){
return (clojure.JS.def(clojure.core,"parents",(function __clojure_core_fn_3255_parents_3257(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.core.parents.apply(null,[clojure.core.global_hierarchy,tag_1]))}
var tp_3;
return (clojure.core.not_empty.apply(null,[((tp_3=clojure.core.get.apply(null,[clojure.core.keyword("","parents").apply(null,[h_1]),tag_2])),
((clojure.core.class_QMARK_.apply(null,[tag_2]))?(clojure.core.into.apply(null,[clojure.core.set.apply(null,[clojure.core.bases.apply(null,[tag_2])]),tp_3])):(tp_3)))]))})))}).apply(null,[]);

//======
//(defn ancestors "Returns the immediate and indirect parents of tag, either via a Java type\n  inheritance relationship or a relationship established via derive. h\n  must be a hierarchy obtained from make-hierarchy, if not supplied\n  defaults to the global hierarchy" ([tag] (ancestors global-hierarchy tag)) ([h tag] (not-empty (let [ta (get (:ancestors h) tag)] (if (class? tag) (into (set (supers tag)) ta) ta)))))
//---
(function __clojure_core_fn_3262(){
return (clojure.JS.def(clojure.core,"ancestors",(function __clojure_core_fn_3262_ancestors_3264(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.core.ancestors.apply(null,[clojure.core.global_hierarchy,tag_1]))}
var ta_3;
return (clojure.core.not_empty.apply(null,[((ta_3=clojure.core.get.apply(null,[clojure.core.keyword("","ancestors").apply(null,[h_1]),tag_2])),
((clojure.core.class_QMARK_.apply(null,[tag_2]))?(clojure.core.into.apply(null,[clojure.core.set.apply(null,[clojure.core.supers.apply(null,[tag_2])]),ta_3])):(ta_3)))]))})))}).apply(null,[]);

//======
//(defn descendants "Returns the immediate and indirect children of tag, through a\n  relationship established via derive. h must be a hierarchy obtained\n  from make-hierarchy, if not supplied defaults to the global\n  hierarchy. Note: does not work on Java type inheritance\n  relationships." ([tag] (descendants global-hierarchy tag)) ([h tag] (if (class? tag) (throw (RT/makeUnsupportedException "Can't get descendants of classes")) (not-empty (get (:descendants h) tag)))))
//---
(function __clojure_core_fn_3269(){
return (clojure.JS.def(clojure.core,"descendants",(function __clojure_core_fn_3269_descendants_3271(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.core.descendants.apply(null,[clojure.core.global_hierarchy,tag_1]))}
return (((clojure.core.class_QMARK_.apply(null,[tag_2]))?((function __throw(){throw clojure.lang.RT.makeUnsupportedException("Can't get descendants of classes")})()):(clojure.core.not_empty.apply(null,[clojure.core.get.apply(null,[clojure.core.keyword("","descendants").apply(null,[h_1]),tag_2])]))))})))}).apply(null,[]);

//======
//(defn derive "Establishes a parent/child relationship between parent and\n  tag. Parent must be a namespace-qualified symbol or keyword and\n  child can be either a namespace-qualified symbol or keyword or a\n  class. h must be a hierarchy obtained from make-hierarchy, if not\n  supplied defaults to, and modifies, the global hierarchy." ([tag parent] (alter-var-root (var global-hierarchy) derive tag parent) nil) ([h tag parent] (assert (not= tag parent)) (assert (or (class? tag) (and (instance? clojure.lang.Named tag) (namespace tag)))) (assert (instance? clojure.lang.Named parent)) (assert (namespace parent)) (let [tp (:parents h) td (:descendants h) ta (:ancestors h) tf (fn [m source sources target targets] (reduce (fn [ret k] (assoc ret k (reduce conj (get targets k #{}) (cons target (targets target))))) m (cons source (sources source))))] (or (when-not (contains? (tp tag) parent) (when (contains? (ta tag) parent) (throw (RT/makeException (print-str tag "already has" parent "as ancestor")))) (when (contains? (ta parent) tag) (throw (RT/makeException (print-str "Cyclic derivation:" parent "has" tag "as ancestor")))) {:ancestors (tf (:ancestors h) tag td parent ta), :descendants (tf (:descendants h) parent ta tag td), :parents (assoc (:parents h) tag (conj (get tp tag #{}) parent))}) h))))
//---
(function __clojure_core_fn_3276(){
return (clojure.JS.def(clojure.core,"derive",(function __clojure_core_fn_3276_derive_3278(h_1,tag_2,parent_3){switch(arguments.length){
case 2:var tag_1=arguments[0],parent_2=arguments[1];
return (clojure.core.alter_var_root.apply(null,[clojure.core._var_global_hierarchy,clojure.core.derive,tag_1,parent_2]),
null)}
var and__806_5,td_5,or__820_8,tp_4,tf_7,ta_6,or__820_4;
return (((clojure.core.not_EQ_.apply(null,[tag_2,parent_3]))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["Assert failed: ",clojure.core.pr_str.apply(null,[clojure.JS.lit_list([clojure.core.symbol("not="),clojure.core.symbol("tag"),clojure.core.symbol("parent")])])]))})())),
((((or__820_4=clojure.core.class_QMARK_.apply(null,[tag_2])),
((or__820_4)?(or__820_4):(((and__806_5=clojure.core.instance_QMARK_.apply(null,[clojure.lang.Named,tag_2])),
((and__806_5)?(clojure.core.namespace.apply(null,[tag_2])):(and__806_5)))))))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["Assert failed: ",clojure.core.pr_str.apply(null,[clojure.JS.lit_list([clojure.core.symbol("or"),clojure.JS.lit_list([clojure.core.symbol("class?"),clojure.core.symbol("tag")]),clojure.JS.lit_list([clojure.core.symbol("and"),clojure.JS.lit_list([clojure.core.symbol("instance?"),clojure.core.symbol("clojure.lang.Named"),clojure.core.symbol("tag")]),clojure.JS.lit_list([clojure.core.symbol("namespace"),clojure.core.symbol("tag")])])])])]))})())),
((clojure.core.instance_QMARK_.apply(null,[clojure.lang.Named,parent_3]))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["Assert failed: ",clojure.core.pr_str.apply(null,[clojure.JS.lit_list([clojure.core.symbol("instance?"),clojure.core.symbol("clojure.lang.Named"),clojure.core.symbol("parent")])])]))})())),
((clojure.core.namespace.apply(null,[parent_3]))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["Assert failed: ",clojure.core.pr_str.apply(null,[clojure.JS.lit_list([clojure.core.symbol("namespace"),clojure.core.symbol("parent")])])]))})())),
((tp_4=clojure.core.keyword("","parents").apply(null,[h_1])),
(td_5=clojure.core.keyword("","descendants").apply(null,[h_1])),
(ta_6=clojure.core.keyword("","ancestors").apply(null,[h_1])),
(tf_7=(function __clojure_core_fn_3276_derive_3278_tf_3281(m_1,source_2,sources_3,target_4,targets_5){
return (clojure.core.reduce.apply(null,[(function __clojure_core_fn_3276_derive_3278_tf_3281_fn_3283(ret_1,k_2){
return (clojure.core.assoc.apply(null,[ret_1,k_2,clojure.core.reduce.apply(null,[clojure.core.conj,clojure.core.get.apply(null,[targets_5,k_2,clojure.lang.PersistentHashSet.EMPTY]),clojure.core.cons.apply(null,[target_4,targets_5.apply(null,[target_4])])])]))}),m_1,clojure.core.cons.apply(null,[source_2,sources_3.apply(null,[source_2])])]))})),
((or__820_8=((clojure.core.contains_QMARK_.apply(null,[tp_4.apply(null,[tag_2]),parent_3]))?(null):(((clojure.core.contains_QMARK_.apply(null,[ta_6.apply(null,[tag_2]),parent_3]))?((function __throw(){throw clojure.lang.RT.makeException(clojure.core.print_str.apply(null,[tag_2,"already has",parent_3,"as ancestor"]))})()):(null)),
((clojure.core.contains_QMARK_.apply(null,[ta_6.apply(null,[parent_3]),tag_2]))?((function __throw(){throw clojure.lang.RT.makeException(clojure.core.print_str.apply(null,["Cyclic derivation:",parent_3,"has",tag_2,"as ancestor"]))})()):(null)),
clojure.core.hash_map(clojure.core.keyword("","ancestors"),tf_7.apply(null,[clojure.core.keyword("","ancestors").apply(null,[h_1]),tag_2,td_5,parent_3,ta_6]),clojure.core.keyword("","descendants"),tf_7.apply(null,[clojure.core.keyword("","descendants").apply(null,[h_1]),parent_3,ta_6,tag_2,td_5]),clojure.core.keyword("","parents"),clojure.core.assoc.apply(null,[clojure.core.keyword("","parents").apply(null,[h_1]),tag_2,clojure.core.conj.apply(null,[clojure.core.get.apply(null,[tp_4,tag_2,clojure.lang.PersistentHashSet.EMPTY]),parent_3])]))))),
((or__820_8)?(or__820_8):(h_1)))))})))}).apply(null,[]);

//======
//(defn underive "Removes a parent/child relationship between parent and\n  tag. h must be a hierarchy obtained from make-hierarchy, if not\n  supplied defaults to, and modifies, the global hierarchy." ([tag parent] (alter-var-root (var global-hierarchy) underive tag parent) nil) ([h tag parent] (let [tp (:parents h) td (:descendants h) ta (:ancestors h) tf (fn [m source sources target targets] (reduce (fn [ret k] (assoc ret k (reduce disj (get targets k) (cons target (targets target))))) m (cons source (sources source))))] (if (contains? (tp tag) parent) {:parent (assoc (:parents h) tag (disj (get tp tag) parent)), :ancestors (tf (:ancestors h) tag td parent ta), :descendants (tf (:descendants h) parent ta tag td)} h))))
//---
(function __clojure_core_fn_3289(){
return (clojure.JS.def(clojure.core,"underive",(function __clojure_core_fn_3289_underive_3291(h_1,tag_2,parent_3){switch(arguments.length){
case 2:var tag_1=arguments[0],parent_2=arguments[1];
return (clojure.core.alter_var_root.apply(null,[clojure.core._var_global_hierarchy,clojure.core.underive,tag_1,parent_2]),
null)}
var td_5,tp_4,tf_7,ta_6;
return (((tp_4=clojure.core.keyword("","parents").apply(null,[h_1])),
(td_5=clojure.core.keyword("","descendants").apply(null,[h_1])),
(ta_6=clojure.core.keyword("","ancestors").apply(null,[h_1])),
(tf_7=(function __clojure_core_fn_3289_underive_3291_tf_3294(m_1,source_2,sources_3,target_4,targets_5){
return (clojure.core.reduce.apply(null,[(function __clojure_core_fn_3289_underive_3291_tf_3294_fn_3296(ret_1,k_2){
return (clojure.core.assoc.apply(null,[ret_1,k_2,clojure.core.reduce.apply(null,[clojure.core.disj,clojure.core.get.apply(null,[targets_5,k_2]),clojure.core.cons.apply(null,[target_4,targets_5.apply(null,[target_4])])])]))}),m_1,clojure.core.cons.apply(null,[source_2,sources_3.apply(null,[source_2])])]))})),
((clojure.core.contains_QMARK_.apply(null,[tp_4.apply(null,[tag_2]),parent_3]))?(clojure.core.hash_map(clojure.core.keyword("","parent"),clojure.core.assoc.apply(null,[clojure.core.keyword("","parents").apply(null,[h_1]),tag_2,clojure.core.disj.apply(null,[clojure.core.get.apply(null,[tp_4,tag_2]),parent_3])]),clojure.core.keyword("","ancestors"),tf_7.apply(null,[clojure.core.keyword("","ancestors").apply(null,[h_1]),tag_2,td_5,parent_3,ta_6]),clojure.core.keyword("","descendants"),tf_7.apply(null,[clojure.core.keyword("","descendants").apply(null,[h_1]),parent_3,ta_6,tag_2,td_5]))):(h_1))))})))}).apply(null,[]);

//======
//(defn distinct? "Returns true if no two of the arguments are equal" {:tag Boolean} ([x] true) ([x y] (not (= x y))) ([x y & more] (if (not= x y) (loop [s #{y x} [x & etc :as xs] more] (if xs (if (contains? s x) false (recur (conj s x) etc)) true)) false)))
//---
(function __clojure_core_fn_3302(){
return (clojure.JS.def(clojure.core,"distinct_QMARK_",clojure.JS.variadic(2,(function __clojure_core_fn_3302_distinct_QMARK_3304(x_1,y_2){switch(arguments.length){
case 1:return (true)
case 2:return (clojure.core.not.apply(null,[clojure.lang.Util.equal(x_1,y_2)]))}
var s_4,etc_8,s_12,etc_15,x_7,G__3309_11,s_10,xs_9,x_14,G__3309_5,vec__3310_6,xs_16,vec__3311_13,more_3=clojure.JS.rest_args(this,arguments,2);
return (((clojure.core.not_EQ_.apply(null,[x_1,y_2]))?(((s_4=clojure.core.hash_set(y_2,x_1)),
(G__3309_5=more_3),
(vec__3310_6=G__3309_5),
(x_7=clojure.core.nth.apply(null,[vec__3310_6,(0),null])),
(etc_8=clojure.core.nthrest.apply(null,[vec__3310_6,(1)])),
(xs_9=vec__3310_6),
((function __loop(){var _rtn,_cnt;(s_10=s_4),
(G__3309_11=G__3309_5);do{_cnt=0;
_rtn=((s_12=s_10),
(vec__3311_13=G__3309_11),
(x_14=clojure.core.nth.apply(null,[vec__3311_13,(0),null])),
(etc_15=clojure.core.nthrest.apply(null,[vec__3311_13,(1)])),
(xs_16=vec__3311_13),
((xs_16)?(((clojure.core.contains_QMARK_.apply(null,[s_12,x_14]))?(false):((_cnt=1,_rtn=[clojure.core.conj.apply(null,[s_12,x_14]),etc_15],s_10=_rtn[0],G__3309_11=_rtn[1])))):(true)))}while(_cnt);return _rtn;})()))):(false)))}))))}).apply(null,[]);

//======
//(defn iterator-seq "Returns a seq on a java.util.Iterator. Note that most collections\n  providing iterators implement Iterable and thus support seq directly." [iter] (clojure.lang.IteratorSeq/create iter))
//---
(function __clojure_core_fn_3314(){
return (clojure.JS.def(clojure.core,"iterator_seq",(function __clojure_core_fn_3314_iterator_seq_3316(iter_1){
return (clojure.lang.IteratorSeq.create(iter_1))})))}).apply(null,[]);

//======
//(defn enumeration-seq "Returns a seq on a java.lang.Enumeration" [e] (clojure.lang.EnumerationSeq/create e))
//---
(function __clojure_core_fn_3320(){
return (clojure.JS.def(clojure.core,"enumeration_seq",(function __clojure_core_fn_3320_enumeration_seq_3322(e_1){
return (clojure.lang.EnumerationSeq.create(e_1))})))}).apply(null,[]);
// Skipping: (defn format "Formats a string using java.lang.String.format, see java.util.Formatter for format\n  string syntax" [fmt & args] (String/format fmt (to-array args)))

//======
//(defn printf "Prints formatted output, as per format" [fmt & args] (print (apply format fmt args)))
//---
(function __clojure_core_fn_3332(){
return (clojure.JS.def(clojure.core,"printf",clojure.JS.variadic(1,(function __clojure_core_fn_3332_printf_3334(fmt_1){
var args_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.core.print.apply(null,[clojure.core.apply.apply(null,[clojure.core.format,fmt_1,args_2])]))}))))}).apply(null,[]);
// Skipping: (defmacro ns "Sets *ns* to the namespace named by name (unevaluated), creating it\n  if needed.  references can be zero or more of: (:refer-clojure ...)\n  (:require ...) (:use ...) (:import ...) (:load ...) with the syntax\n  of refer-clojure/require/use/import/load respectively, except the\n  arguments are unevaluated and need not be quoted.  If :refer-clojure\n  is not used, a default (refer 'clojure) is used. Use of ns is preferred\n  to individual calls to in-ns/require/use/import:\n\n  (ns foo\n    (:refer-clojure :exclude [ancestors printf])\n    (:require (clojure.contrib sql sql.tests))\n    (:use (my.lib this that))\n    (:import (java.util Date Timer Random)\n              (java.sql Connection Statement))\n    (:load \"/mystuff/foo.clj\"))" [name & references] (let [process-reference (fn [[kname & args]] (clojure.core/concat (clojure.core/list (symbol "clojure.core" (clojure.core/name kname))) (map (fn* [p1__3338] (list (quote quote) p1__3338)) args)))] (clojure.core/concat (clojure.core/list (quote do)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/in-ns)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list name))))) (when (not-any? (fn* [p1__3339] (= :refer-clojure (first p1__3339))) references) (clojure.core/concat (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/refer)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list (quote clojure.core)))))))) (map process-reference references))))
// Skipping: (defmacro refer-clojure "Same as (refer 'clojure <filters>)" [& filters] (clojure.core/concat (clojure.core/list (quote clojure.core/refer)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list (quote clojure.core)))) filters))
// Skipping: (defmacro defonce "defs name to have the root value of the expr iff the named var has no root value, \n  else expr is unevaluated" [name expr] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote v__3380)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote def)) (clojure.core/list name)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when-not)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/.hasRoot)) (clojure.core/list (quote v__3380)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote def)) (clojure.core/list name) (clojure.core/list expr)))))))

//======
//(defonce *loaded-libs* (ref (sorted-set)))
//---
(function __clojure_core_fn_3390(){
var v__3380_1;
return (((v__3380_1=clojure.JS.def(clojure.core,"_STAR_loaded_libs_STAR_",null)),
(((v__3380_1).hasRoot())?(null):(clojure.JS.def(clojure.core,"_STAR_loaded_libs_STAR_",clojure.core.ref.apply(null,[clojure.core.sorted_set.apply(null,[])]))))))}).apply(null,[]);

//======
//(defonce *pending-paths* #{})
//---
(function __clojure_core_fn_3393(){
var v__3380_1;
return (((v__3380_1=clojure.JS.def(clojure.core,"_STAR_pending_paths_STAR_",null)),
(((v__3380_1).hasRoot())?(null):(clojure.JS.def(clojure.core,"_STAR_pending_paths_STAR_",clojure.lang.PersistentHashSet.EMPTY)))))}).apply(null,[]);

//======
//(defonce *loading-verbosely* false)
//---
(function __clojure_core_fn_3396(){
var v__3380_1;
return (((v__3380_1=clojure.JS.def(clojure.core,"_STAR_loading_verbosely_STAR_",null)),
(((v__3380_1).hasRoot())?(null):(clojure.JS.def(clojure.core,"_STAR_loading_verbosely_STAR_",false)))))}).apply(null,[]);

//======
//(defn- throw-if "Throws an exception with a message if pred is true" [pred fmt & args] (when pred (let [message (apply format fmt args) exception (RT/makeException message) raw-trace (.getStackTrace exception) boring? (fn* [p1__3399] (not= (.getMethodName p1__3399) "doInvoke")) trace (into-array (drop 2 (drop-while boring? raw-trace)))] (.setStackTrace exception trace) (throw exception))))
//---
(function __clojure_core_fn_3400(){
return (clojure.JS.def(clojure.core,"throw_if",clojure.JS.variadic(2,(function __clojure_core_fn_3400_throw_if_3402(pred_1,fmt_2){
var exception_5,trace_8,message_4,raw_trace_6,boring_QMARK__7,args_3=clojure.JS.rest_args(this,arguments,2);
return (((pred_1)?(((message_4=clojure.core.apply.apply(null,[clojure.core.format,fmt_2,args_3])),
(exception_5=clojure.lang.RT.makeException(message_4)),
(raw_trace_6=(exception_5).getStackTrace()),
(boring_QMARK__7=(function __clojure_core_fn_3400_throw_if_3402_boring_QMARK_3404(p1__3399_1){
return (clojure.core.not_EQ_.apply(null,[clojure.JS.getOrRun(p1__3399_1,"getMethodName"),"doInvoke"]))})),
(trace_8=clojure.core.into_array.apply(null,[clojure.core.drop.apply(null,[(2),clojure.core.drop_while.apply(null,[boring_QMARK__7,raw_trace_6])])])),
(exception_5).setStackTrace(trace_8),
(function __throw(){throw exception_5})())):(null)))}))))}).apply(null,[]);

//======
//(defn- libspec? "Returns true if x is a libspec" [x] (or (symbol? x) (and (vector? x) (or (nil? (second x)) (keyword? (second x))))))
//---
(function __clojure_core_fn_3409(){
return (clojure.JS.def(clojure.core,"libspec_QMARK_",(function __clojure_core_fn_3409_libspec_QMARK_3411(x_1){
var or__820_2,or__820_4,and__806_3;
return (((or__820_2=clojure.core.symbol_QMARK_.apply(null,[x_1])),
((or__820_2)?(or__820_2):(((and__806_3=clojure.core.vector_QMARK_.apply(null,[x_1])),
((and__806_3)?(((or__820_4=clojure.core.nil_QMARK_.apply(null,[clojure.core.second.apply(null,[x_1])])),
((or__820_4)?(or__820_4):(clojure.core.keyword_QMARK_.apply(null,[clojure.core.second.apply(null,[x_1])]))))):(and__806_3)))))))})))}).apply(null,[]);

//======
//(defn- prependss "Prepends a symbol or a seq to coll" [x coll] (if (symbol? x) (cons x coll) (concat x coll)))
//---
(function __clojure_core_fn_3415(){
return (clojure.JS.def(clojure.core,"prependss",(function __clojure_core_fn_3415_prependss_3417(x_1,coll_2){
return (((clojure.core.symbol_QMARK_.apply(null,[x_1]))?(clojure.core.cons.apply(null,[x_1,coll_2])):(clojure.core.concat.apply(null,[x_1,coll_2]))))})))}).apply(null,[]);

//======
//(defn- root-resource "Returns the root directory path for a lib" [lib] (str \/ (.. (name lib) (replace \- \_) (replace \. \/))))
//---
(function __clojure_core_fn_3421(){
return (clojure.JS.def(clojure.core,"root_resource",(function __clojure_core_fn_3421_root_resource_3423(lib_1){
return (clojure.core.str.apply(null,["/",((clojure.core.name.apply(null,[lib_1])).replace("-","_")).replace(".","/")]))})))}).apply(null,[]);

//======
//(defn- root-directory "Returns the root resource path for a lib" [lib] (let [d (root-resource lib)] (subs d 0 (.lastIndexOf d "/"))))
//---
(function __clojure_core_fn_3427(){
return (clojure.JS.def(clojure.core,"root_directory",(function __clojure_core_fn_3427_root_directory_3429(lib_1){
var d_2;
return (((d_2=clojure.core.root_resource.apply(null,[lib_1])),
clojure.core.subs.apply(null,[d_2,(0),(d_2).lastIndexOf("/")])))})))}).apply(null,[]);
// Skipping: (def load)

//======
//(defn- load-one "Loads a lib given its name. If need-ns, ensures that the associated\n  namespace exists after loading. If require, records the load so any\n  duplicate loads can be skipped." [lib need-ns require] (load (root-resource lib)) (throw-if (and need-ns (not (find-ns lib))) "namespace '%s' not found after loading '%s'" lib (root-resource lib)) (when require (dosync (commute *loaded-libs* conj lib))))
//---
(function __clojure_core_fn_3436(){
return (clojure.JS.def(clojure.core,"load_one",(function __clojure_core_fn_3436_load_one_3438(lib_1,need_ns_2,require_3){
var and__806_4;
return (clojure.core.load.apply(null,[clojure.core.root_resource.apply(null,[lib_1])]),
clojure.core.throw_if.apply(null,[((and__806_4=need_ns_2),
((and__806_4)?(clojure.core.not.apply(null,[clojure.core.find_ns.apply(null,[lib_1])])):(and__806_4))),"namespace '%s' not found after loading '%s'",lib_1,clojure.core.root_resource.apply(null,[lib_1])]),
((require_3)?(clojure.lang.LockingTransaction.runInTransaction((function __clojure_core_fn_3436_load_one_3438_fn_3440(){
return (clojure.core.commute.apply(null,[clojure.core._STAR_loaded_libs_STAR_,clojure.core.conj,lib_1]))}))):(null)))})))}).apply(null,[]);

//======
//(defn- load-all "Loads a lib given its name and forces a load of any libs it directly or\n  indirectly loads. If need-ns, ensures that the associated namespace\n  exists after loading. If require, records the load so any duplicate loads\n  can be skipped." [lib need-ns require] (dosync (commute *loaded-libs* (fn* [p1__3445 p2__3446] (reduce conj p1__3445 p2__3446)) (binding [*loaded-libs* (ref (sorted-set))] (load-one lib need-ns require) (clojure.core/deref *loaded-libs*)))))
//---
(function __clojure_core_fn_3447(){
return (clojure.JS.def(clojure.core,"load_all",(function __clojure_core_fn_3447_load_all_3449(lib_1,need_ns_2,require_3){
return (clojure.lang.LockingTransaction.runInTransaction((function __clojure_core_fn_3447_load_all_3449_fn_3451(){
return (clojure.core.commute.apply(null,[clojure.core._STAR_loaded_libs_STAR_,(function __clojure_core_fn_3447_load_all_3449_fn_3451_fn_3453(p1__3445_1,p2__3446_2){
return (clojure.core.reduce.apply(null,[clojure.core.conj,p1__3445_1,p2__3446_2]))}),clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_loaded_libs_STAR_,clojure.core.ref.apply(null,[clojure.core.sorted_set.apply(null,[])])])),
(function __clojure_core_fn_3447_load_all_3449_fn_3451_fn_3456(){
return ((function __try(){try{var _rtn=(clojure.core.load_one.apply(null,[lib_1,need_ns_2,require_3]),
clojure.core.deref.apply(null,[clojure.core._STAR_loaded_libs_STAR_]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())}).apply(null,[])]))})))})))}).apply(null,[]);

//======
//(defn- load-lib "Loads a lib with options" [prefix lib & options] (throw-if (and prefix (pos? (.indexOf (name lib) (int \.)))) "lib names inside prefix lists must not contain periods") (let [lib (if prefix (symbol (str prefix \. lib)) lib) opts (apply hash-map options) {:keys [as reload reload-all require use verbose]} opts loaded (contains? (clojure.core/deref *loaded-libs*) lib) load (cond reload-all load-all (or reload (not require) (not loaded)) load-one) need-ns (or as use) filter-opts (select-keys opts (quote (:exclude :only :rename)))] (binding [*loading-verbosely* (or *loading-verbosely* verbose)] (if load (load lib need-ns require) (throw-if (and need-ns (not (find-ns lib))) "namespace '%s' not found" lib)) (when (and need-ns *loading-verbosely*) (printf "(clojure.core/in-ns '%s)\n" (ns-name *ns*))) (when as (when *loading-verbosely* (printf "(clojure.core/alias '%s '%s)\n" as lib)) (alias as lib)) (when use (when *loading-verbosely* (printf "(clojure.core/refer '%s" lib) (doseq [opt filter-opts] (printf " %s '%s" (key opt) (print-str (val opt)))) (printf ")\n")) (apply refer lib (mapcat seq filter-opts))))))
//---
(function __clojure_core_fn_3462(){
return (clojure.JS.def(clojure.core,"load_lib",clojure.JS.variadic(2,(function __clojure_core_fn_3462_load_lib_3464(prefix_1,lib_2){
var or__820_17,opts_5,load_14,and__806_19,or__820_14,and__806_19,or__820_15,opt_20,as_8,need_ns_15,filter_opts_16,reload_all_10,and__806_4,use_12,reload_7,loaded_13,lib_4,require_9,map__3466_6,sq__1819_19,verbose_11,or__820_15,options_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.throw_if.apply(null,[((and__806_4=prefix_1),
((and__806_4)?(clojure.lang.Numbers.isPos((clojure.core.name.apply(null,[lib_2])).indexOf(clojure.lang.RT.intCast(".")))):(and__806_4))),"lib names inside prefix lists must not contain periods"]),
((lib_4=((prefix_1)?(clojure.core.symbol.apply(null,[clojure.core.str.apply(null,[prefix_1,".",lib_2])])):(lib_2))),
(opts_5=clojure.core.apply.apply(null,[clojure.core.hash_map,options_3])),
(map__3466_6=opts_5),
(reload_7=clojure.core.get.apply(null,[map__3466_6,clojure.core.keyword("","reload")])),
(as_8=clojure.core.get.apply(null,[map__3466_6,clojure.core.keyword("","as")])),
(require_9=clojure.core.get.apply(null,[map__3466_6,clojure.core.keyword("","require")])),
(reload_all_10=clojure.core.get.apply(null,[map__3466_6,clojure.core.keyword("","reload-all")])),
(verbose_11=clojure.core.get.apply(null,[map__3466_6,clojure.core.keyword("","verbose")])),
(use_12=clojure.core.get.apply(null,[map__3466_6,clojure.core.keyword("","use")])),
(loaded_13=clojure.core.contains_QMARK_.apply(null,[clojure.core.deref.apply(null,[clojure.core._STAR_loaded_libs_STAR_]),lib_4])),
(load_14=((reload_all_10)?(clojure.core.load_all):(((((or__820_14=reload_7),
((or__820_14)?(or__820_14):(((or__820_15=clojure.core.not.apply(null,[require_9])),
((or__820_15)?(or__820_15):(clojure.core.not.apply(null,[loaded_13]))))))))?(clojure.core.load_one):(null))))),
(need_ns_15=((or__820_15=as_8),
((or__820_15)?(or__820_15):(use_12)))),
(filter_opts_16=clojure.core.select_keys.apply(null,[opts_5,clojure.JS.lit_list([clojure.core.keyword("","exclude"),clojure.core.keyword("","only"),clojure.core.keyword("","rename")])])),
clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_loading_verbosely_STAR_,((or__820_17=clojure.core._STAR_loading_verbosely_STAR_),
((or__820_17)?(or__820_17):(verbose_11)))])),
(function __try(){try{var _rtn=(((load_14)?(load_14.apply(null,[lib_4,need_ns_15,require_9])):(clojure.core.throw_if.apply(null,[((and__806_19=need_ns_15),
((and__806_19)?(clojure.core.not.apply(null,[clojure.core.find_ns.apply(null,[lib_4])])):(and__806_19))),"namespace '%s' not found",lib_4]))),
((((and__806_19=need_ns_15),
((and__806_19)?(clojure.core._STAR_loading_verbosely_STAR_):(and__806_19))))?(clojure.core.printf.apply(null,["(clojure.core/in-ns '%s)\n",clojure.core.ns_name.apply(null,[clojure.core._STAR_ns_STAR_])])):(null)),
((as_8)?(((clojure.core._STAR_loading_verbosely_STAR_)?(clojure.core.printf.apply(null,["(clojure.core/alias '%s '%s)\n",as_8,lib_4])):(null)),
clojure.core.alias.apply(null,[as_8,lib_4])):(null)),
((use_12)?(((clojure.core._STAR_loading_verbosely_STAR_)?(clojure.core.printf.apply(null,["(clojure.core/refer '%s",lib_4]),
((function __loop(){var _rtn,_cnt;(sq__1819_19=clojure.core.seq.apply(null,[filter_opts_16]));do{_cnt=0;
_rtn=((sq__1819_19)?(((opt_20=clojure.core.first.apply(null,[sq__1819_19])),
((true)?(((true)?(clojure.core.printf.apply(null,[" %s '%s",clojure.core.key.apply(null,[opt_20]),clojure.core.print_str.apply(null,[clojure.core.val.apply(null,[opt_20])])])):(null)),
(_cnt=1,_rtn=[clojure.core.rest.apply(null,[sq__1819_19])],sq__1819_19=_rtn[0])):(null)))):(null))}while(_cnt);return _rtn;})()),
clojure.core.printf.apply(null,[")\n"])):(null)),
clojure.core.apply.apply(null,[clojure.core.refer,lib_4,clojure.core.mapcat.apply(null,[clojure.core.seq,filter_opts_16])])):(null)))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})()))}))))}).apply(null,[]);

//======
//(defn- load-libs "Loads libs, interpreting libspecs, prefix lists, and flags for\n  forwarding to load-lib" [& args] (let [flags (filter keyword? args) opts (interleave flags (repeat true)) args (filter (complement keyword?) args)] (doseq [arg args] (if (libspec? arg) (apply load-lib nil (prependss arg opts)) (let [[prefix & args] arg] (throw-if (nil? prefix) "prefix cannot be nil") (doseq [arg args] (apply load-lib prefix (prependss arg opts))))))))
//---
(function __clojure_core_fn_3469(){
return (clojure.JS.def(clojure.core,"load_libs",clojure.JS.variadic(0,(function __clojure_core_fn_3469_load_libs_3471(){
var prefix_8,args_9,arg_6,sq__1819_5,sq__1819_10,vec__3473_7,arg_11,args_4,opts_3,flags_2,args_1=clojure.JS.rest_args(this,arguments,0);
return (((flags_2=clojure.core.filter.apply(null,[clojure.core.keyword_QMARK_,args_1])),
(opts_3=clojure.core.interleave.apply(null,[flags_2,clojure.core.repeat.apply(null,[true])])),
(args_4=clojure.core.filter.apply(null,[clojure.core.complement.apply(null,[clojure.core.keyword_QMARK_]),args_1])),
((function __loop(){var _rtn,_cnt;(sq__1819_5=clojure.core.seq.apply(null,[args_4]));do{_cnt=0;
_rtn=((sq__1819_5)?(((arg_6=clojure.core.first.apply(null,[sq__1819_5])),
((true)?(((true)?(((clojure.core.libspec_QMARK_.apply(null,[arg_6]))?(clojure.core.apply.apply(null,[clojure.core.load_lib,null,clojure.core.prependss.apply(null,[arg_6,opts_3])])):(((vec__3473_7=arg_6),
(prefix_8=clojure.core.nth.apply(null,[vec__3473_7,(0),null])),
(args_9=clojure.core.nthrest.apply(null,[vec__3473_7,(1)])),
clojure.core.throw_if.apply(null,[clojure.core.nil_QMARK_.apply(null,[prefix_8]),"prefix cannot be nil"]),
((function __loop(){var _rtn,_cnt;(sq__1819_10=clojure.core.seq.apply(null,[args_9]));do{_cnt=0;
_rtn=((sq__1819_10)?(((arg_11=clojure.core.first.apply(null,[sq__1819_10])),
((true)?(((true)?(clojure.core.apply.apply(null,[clojure.core.load_lib,prefix_8,clojure.core.prependss.apply(null,[arg_11,opts_3])])):(null)),
(_cnt=1,_rtn=[clojure.core.rest.apply(null,[sq__1819_10])],sq__1819_10=_rtn[0])):(null)))):(null))}while(_cnt);return _rtn;})()))))):(null)),
(_cnt=1,_rtn=[clojure.core.rest.apply(null,[sq__1819_5])],sq__1819_5=_rtn[0])):(null)))):(null))}while(_cnt);return _rtn;})())))}))))}).apply(null,[]);

//======
//(defn require "Loads libs, skipping any that are already loaded. Each argument is\n  either a libspec that identifies a lib, a prefix list that identifies\n  multiple libs whose names share a common prefix, or a flag that modifies\n  how all the identified libs are loaded. Use :require in the ns macro \n  in preference to calling this directly.\n\n  Libs\n\n  A 'lib' is a named set of resources in classpath whose contents define a\n  library of Clojure code. Lib names are symbols and each lib is associated\n  with a Clojure namespace and a Java package that share its name. A lib's\n  name also locates its root directory within classpath using Java's\n  package name to classpath-relative path mapping. All resources in a lib\n  should be contained in the directory structure under its root directory.\n  All definitions a lib makes should be in its associated namespace.\n\n  'require loads a lib by loading its root resource. The root resource path\n  is derived from the root directory path by repeating its last component\n  and appending '.clj'. For example, the lib 'x.y.z has root directory\n  <classpath>/x/y/z; root resource <classpath>/x/y/z/z.clj. The root\n  resource should contain code to create the lib's namespace and load any\n  additional lib resources.\n\n  Libspecs\n\n  A libspec is a lib name or a vector containing a lib name followed by\n  options expressed as sequential keywords and arguments.\n\n  Recognized options: :as\n  :as takes a symbol as its argument and makes that symbol an alias to the\n    lib's namespace in the current namespace.\n\n  Prefix Lists\n\n  It's common for Clojure code to depend on several libs whose names have\n  the same prefix. When specifying libs, prefix lists can be used to reduce\n  repetition. A prefix list contains the shared prefix followed by libspecs\n  with the shared prefix removed from the lib names. After removing the\n  prefix, the names that remain must not contain any periods.\n\n  Flags\n\n  A flag is a keyword.\n  Recognized flags: :reload, :reload-all, :verbose\n  :reload forces loading of all the identified libs even if they are\n    already loaded\n  :reload-all implies :reload and also forces loading of all libs that the\n    identified libs directly or indirectly load via require or use\n  :verbose triggers printing information about each load, alias, and refer" [& args] (apply load-libs :require args))
//---
(function __clojure_core_fn_3476(){
return (clojure.JS.def(clojure.core,"require",clojure.JS.variadic(0,(function __clojure_core_fn_3476_require_3478(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[clojure.core.load_libs,clojure.core.keyword("","require"),args_1]))}))))}).apply(null,[]);

//======
//(defn use "Like 'require, but also refers to each lib's namespace using\n  clojure.core/refer. Use :use in the ns macro in preference to calling\n  this directly.\n\n  'use accepts additional options in libspecs: :exclude, :only, :rename.\n  The arguments and semantics for :exclude, :only, and :rename are the same\n  as those documented for clojure.core/refer." [& args] (apply load-libs :require :use args))
//---
(function __clojure_core_fn_3482(){
return (clojure.JS.def(clojure.core,"use",clojure.JS.variadic(0,(function __clojure_core_fn_3482_use_3484(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[clojure.core.load_libs,clojure.core.keyword("","require"),clojure.core.keyword("","use"),args_1]))}))))}).apply(null,[]);

//======
//(defn loaded-libs "Returns a sorted set of symbols naming the currently loaded libs" [] (clojure.core/deref *loaded-libs*))
//---
(function __clojure_core_fn_3488(){
return (clojure.JS.def(clojure.core,"loaded_libs",(function __clojure_core_fn_3488_loaded_libs_3490(){
return (clojure.core.deref.apply(null,[clojure.core._STAR_loaded_libs_STAR_]))})))}).apply(null,[]);
// Skipping: (defn load "Loads Clojure code from resources in classpath. A path is interpreted as\n  classpath-relative if it begins with a slash or relative to the root\n  directory for the current namespace otherwise." [& paths] (doseq [path paths] (let [path (if (.startsWith path "/") path (str (root-directory (ns-name *ns*)) \/ path))] (when *loading-verbosely* (printf "(clojure.core/load \"%s\")\n" path) (flush)) (throw-if (*pending-paths* path) "cannot load '%s' again while it is loading" path) (binding [*pending-paths* (conj *pending-paths* path)] (clojure.lang.RT/load (.substring path 1))))))

//======
//(defn compile [lib] (binding [*compile-files* true] (load-one lib true true)))
//---
(function __clojure_core_fn_3503(){
return (clojure.JS.def(clojure.core,"compile",(function __clojure_core_fn_3503_compile_3505(lib_1){
return (clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_compile_files_STAR_,true])),
(function __try(){try{var _rtn=(clojure.core.load_one.apply(null,[lib_1,true,true]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())})))}).apply(null,[]);

//======
//(defn get-in "returns the value in a nested associative structure, where ks is a sequence of keys" [m ks] (reduce get m ks))
//---
(function __clojure_core_fn_3509(){
return (clojure.JS.def(clojure.core,"get_in",(function __clojure_core_fn_3509_get_in_3511(m_1,ks_2){
return (clojure.core.reduce.apply(null,[clojure.core.get,m_1,ks_2]))})))}).apply(null,[]);

//======
//(defn assoc-in "Associates a value in a nested associative structure, where ks is a\n  sequence of keys and v is the new value and returns a new nested structure.  \n  If any levels do not exist, hash-maps will be created." [m [k & ks] v] (if ks (assoc m k (assoc-in (get m k) ks v)) (assoc m k v)))
//---
(function __clojure_core_fn_3515(){
return (clojure.JS.def(clojure.core,"assoc_in",(function __clojure_core_fn_3515_assoc_in_3518(m_1,p__3517_2,v_3){
var k_5,ks_6,vec__3520_4;
return (((vec__3520_4=p__3517_2),
(k_5=clojure.core.nth.apply(null,[vec__3520_4,(0),null])),
(ks_6=clojure.core.nthrest.apply(null,[vec__3520_4,(1)])),
((ks_6)?(clojure.core.assoc.apply(null,[m_1,k_5,clojure.core.assoc_in.apply(null,[clojure.core.get.apply(null,[m_1,k_5]),ks_6,v_3])])):(clojure.core.assoc.apply(null,[m_1,k_5,v_3])))))})))}).apply(null,[]);

//======
//(defn update-in "'Updates' a value in a nested associative structure, where ks is a\n  sequence of keys and f is a function that will take the old value\n  and any supplied args and return the new value, and returns a new\n  nested structure.  If any levels do not exist, hash-maps will be\n  created." ([m [k & ks] f & args] (if ks (assoc m k (apply update-in (get m k) ks f args)) (assoc m k (apply f (get m k) args)))))
//---
(function __clojure_core_fn_3523(){
return (clojure.JS.def(clojure.core,"update_in",clojure.JS.variadic(3,(function __clojure_core_fn_3523_update_in_3526(m_1,p__3525_2,f_3){
var k_6,ks_7,vec__3528_5,args_4=clojure.JS.rest_args(this,arguments,3);
return (((vec__3528_5=p__3525_2),
(k_6=clojure.core.nth.apply(null,[vec__3528_5,(0),null])),
(ks_7=clojure.core.nthrest.apply(null,[vec__3528_5,(1)])),
((ks_7)?(clojure.core.assoc.apply(null,[m_1,k_6,clojure.core.apply.apply(null,[clojure.core.update_in,clojure.core.get.apply(null,[m_1,k_6]),ks_7,f_3,args_4])])):(clojure.core.assoc.apply(null,[m_1,k_6,clojure.core.apply.apply(null,[f_3,clojure.core.get.apply(null,[m_1,k_6]),args_4])])))))}))))}).apply(null,[]);

//======
//(defn empty? "Returns true if coll has no items - same as (not (seq coll)). \n  Please use the idiom (seq x) rather than (not (empty? x))" [coll] (not (seq coll)))
//---
(function __clojure_core_fn_3531(){
return (clojure.JS.def(clojure.core,"empty_QMARK_",(function __clojure_core_fn_3531_empty_QMARK_3533(coll_1){
return (clojure.core.not.apply(null,[clojure.core.seq.apply(null,[coll_1])]))})))}).apply(null,[]);

//======
//(defn coll? "Returns true if x implements IPersistentCollection" [x] (instance? clojure.lang.IPersistentCollection x))
//---
(function __clojure_core_fn_3537(){
return (clojure.JS.def(clojure.core,"coll_QMARK_",(function __clojure_core_fn_3537_coll_QMARK_3539(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.IPersistentCollection,x_1]))})))}).apply(null,[]);

//======
//(defn list? "Returns true if x implements IPersistentList" [x] (instance? clojure.lang.IPersistentList x))
//---
(function __clojure_core_fn_3543(){
return (clojure.JS.def(clojure.core,"list_QMARK_",(function __clojure_core_fn_3543_list_QMARK_3545(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.IPersistentList,x_1]))})))}).apply(null,[]);

//======
//(defn set? "Returns true if x implements IPersistentSet" [x] (instance? clojure.lang.IPersistentSet x))
//---
(function __clojure_core_fn_3549(){
return (clojure.JS.def(clojure.core,"set_QMARK_",(function __clojure_core_fn_3549_set_QMARK_3551(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.IPersistentSet,x_1]))})))}).apply(null,[]);
// Skipping: (defn number? "Returns true if x is a Number" [x] (instance? Number x))

//======
//(defn fn? "Returns true if x implements IFn. Note that many data structures \n  (e.g. sets and maps) implement IFn" [x] (instance? clojure.lang.IFn x))
//---
(function __clojure_core_fn_3561(){
return (clojure.JS.def(clojure.core,"fn_QMARK_",(function __clojure_core_fn_3561_fn_QMARK_3563(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.IFn,x_1]))})))}).apply(null,[]);
// Skipping: (defn integer? "Returns true if n is an integer" [n] (or (instance? Integer n) (instance? Long n) (instance? BigInteger n) (instance? Short n) (instance? Byte n)))

//======
//(defn ratio? "Returns true if n is a Ratio" [n] (instance? clojure.lang.Ratio n))
//---
(function __clojure_core_fn_3573(){
return (clojure.JS.def(clojure.core,"ratio_QMARK_",(function __clojure_core_fn_3573_ratio_QMARK_3575(n_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Ratio,n_1]))})))}).apply(null,[]);
// Skipping: (defn decimal? "Returns true if n is a BigDecimal" [n] (instance? BigDecimal n))
// Skipping: (defn float? "Returns true if n is a floating point number" [n] (or (instance? Double n) (instance? Float n)))

//======
//(defn rational? [n] "Returns true if n is a rational number" (or (integer? n) (ratio? n) (decimal? n)))
//---
(function __clojure_core_fn_3591(){
return (clojure.JS.def(clojure.core,"rational_QMARK_",(function __clojure_core_fn_3591_rational_QMARK_3593(n_1){
var or__820_2,or__820_3;
return ("Returns true if n is a rational number",
((or__820_2=clojure.core.integer_QMARK_.apply(null,[n_1])),
((or__820_2)?(or__820_2):(((or__820_3=clojure.core.ratio_QMARK_.apply(null,[n_1])),
((or__820_3)?(or__820_3):(clojure.core.decimal_QMARK_.apply(null,[n_1]))))))))})))}).apply(null,[]);

//======
//(defn associative? "Returns true if coll implements Associative" [coll] (instance? clojure.lang.Associative coll))
//---
(function __clojure_core_fn_3597(){
return (clojure.JS.def(clojure.core,"associative_QMARK_",(function __clojure_core_fn_3597_associative_QMARK_3599(coll_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Associative,coll_1]))})))}).apply(null,[]);

//======
//(defn sequential? "Returns true if coll implements Sequential" [coll] (instance? clojure.lang.Sequential coll))
//---
(function __clojure_core_fn_3603(){
return (clojure.JS.def(clojure.core,"sequential_QMARK_",(function __clojure_core_fn_3603_sequential_QMARK_3605(coll_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Sequential,coll_1]))})))}).apply(null,[]);

//======
//(defn sorted? "Returns true if coll implements Sorted" [coll] (instance? clojure.lang.Sorted coll))
//---
(function __clojure_core_fn_3609(){
return (clojure.JS.def(clojure.core,"sorted_QMARK_",(function __clojure_core_fn_3609_sorted_QMARK_3611(coll_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Sorted,coll_1]))})))}).apply(null,[]);

//======
//(defn reversible? "Returns true if coll implements Reversible" [coll] (instance? clojure.lang.Reversible coll))
//---
(function __clojure_core_fn_3615(){
return (clojure.JS.def(clojure.core,"reversible_QMARK_",(function __clojure_core_fn_3615_reversible_QMARK_3617(coll_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Reversible,coll_1]))})))}).apply(null,[]);
// Skipping: (defn pmap "Like map, except f is applied in parallel. Semi-lazy in that the\n  parallel computation stays ahead of the consumption, but doesn't\n  realize the entire result unless required. Only useful for\n  computationally intensive functions where the time of f dominates\n  the coordination overhead." ([f coll] (let [n (inc (.. Runtime getRuntime availableProcessors)) agents (doall (map (fn* [p1__3621] (agent (f p1__3621))) (take n coll))) wget (fn [a] (await1 a) (clojure.core/deref a)) step (fn step [[x & xs :as s] [a & as :as acycle]] (if s (let [v (wget a)] (send a (fn [_] (f x))) (lazy-cons v (step xs as))) (map wget (take (count agents) acycle))))] (step (drop n coll) (cycle agents)))) ([f coll & colls] (let [step (fn step [cs] (when (every? seq cs) (lazy-cons (map first cs) (step (map rest cs)))))] (pmap (fn* [p1__3622] (apply f p1__3622)) (step (cons coll colls))))))

//======
//(def *1)
//---
(function __clojure_core_fn_3660(){
return (clojure.JS.def(clojure.core,"_STAR_1",null))}).apply(null,[]);

//======
//(def *2)
//---
(function __clojure_core_fn_3663(){
return (clojure.JS.def(clojure.core,"_STAR_2",null))}).apply(null,[]);

//======
//(def *3)
//---
(function __clojure_core_fn_3666(){
return (clojure.JS.def(clojure.core,"_STAR_3",null))}).apply(null,[]);

//======
//(def *e)
//---
(function __clojure_core_fn_3669(){
return (clojure.JS.def(clojure.core,"_STAR_e",null))}).apply(null,[]);
// Skipping: (defmacro declare "defs the supplied var names with no bindings, useful for making forward declarations." [& names] (clojure.core/concat (clojure.core/list (quote do)) (map (fn* [p1__3672] (list (quote def) p1__3672)) names)))

//======
//(load "core-proxy")
//---
(function __clojure_core_fn_3688(){
return (clojure.core.load.apply(null,["core-proxy"]))}).apply(null,[]);

//======
//(load "core-print")
//---
(function __clojure_core_fn_3691(){
return (clojure.core.load.apply(null,["core-print"]))}).apply(null,[]);

//======
//(load "genclass")
//---
(function __clojure_core_fn_3694(){
return (clojure.core.load.apply(null,["genclass"]))}).apply(null,[]);

//======
//(in-ns (quote clojure.core))
//---
(function __user_fn_3697(){
return (clojure.core.in_ns.apply(null,[clojure.core.symbol("clojure.core")]))}).apply(null,[]);

//======
//(import (quote (java.io Writer)))
//---
(function __clojure_core_fn_3703(){
return (clojure.core.import_.apply(null,[clojure.JS.lit_list([clojure.core.symbol("java.io"),clojure.core.symbol("Writer")])]))}).apply(null,[]);

//======
//(def *print-length* nil)
//---
(function __clojure_core_fn_3706(){
return (clojure.JS.def(clojure.core,"_STAR_print_length_STAR_",null))}).apply(null,[]);

//======
//(def *print-level* nil)
//---
(function __clojure_core_fn_3709(){
return (clojure.JS.def(clojure.core,"_STAR_print_level_STAR_",null))}).apply(null,[]);

//======
//(defn- print-sequential [begin print-one sep end sequence w] (binding [*print-level* (and (not *print-dup*) *print-level* (dec *print-level*))] (if (and *print-level* (neg? *print-level*)) (.write w "#") (do (.write w begin) (when-let [xs (seq sequence)] (if (and (not *print-dup*) *print-length*) (loop [[x & xs] xs print-length *print-length*] (if (zero? print-length) (.write w "...") (do (print-one x w) (when xs (.write w sep) (recur xs (dec print-length)))))) (loop [[x & xs] xs] (print-one x w) (when xs (.write w sep) (recur xs))))) (.write w end)))))
//---
(function __clojure_core_fn_3712(){
return (clojure.JS.def(clojure.core,"print_sequential",(function __clojure_core_fn_3712_print_sequential_3714(begin_1,print_one_2,sep_3,end_4,sequence_5,w_6){
var G__3721_15,vec__3722_12,x_13,and__806_8,G__3721_11,x_19,xs_14,vec__3719_18,xs_14,and__806_7,temp__3008_9,print_length_17,vec__3723_16,vec__3718_12,xs_10,and__806_9,G__3717_11,and__806_11,xs_18,G__3717_16,x_13,xs_20,x_17,print_length_21,print_length_15;
return (clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_print_level_STAR_,((and__806_7=clojure.core.not.apply(null,[clojure.core._STAR_print_dup_STAR_])),
((and__806_7)?(((and__806_8=clojure.core._STAR_print_level_STAR_),
((and__806_8)?(clojure.lang.Numbers.dec(clojure.core._STAR_print_level_STAR_)):(and__806_8)))):(and__806_7)))])),
(function __try(){try{var _rtn=(((((and__806_9=clojure.core._STAR_print_level_STAR_),
((and__806_9)?(clojure.lang.Numbers.isNeg(clojure.core._STAR_print_level_STAR_)):(and__806_9))))?((w_6).write("#")):((w_6).write(begin_1),
((temp__3008_9=clojure.core.seq.apply(null,[sequence_5])),
((temp__3008_9)?(((xs_10=temp__3008_9),
((((and__806_11=clojure.core.not.apply(null,[clojure.core._STAR_print_dup_STAR_])),
((and__806_11)?(clojure.core._STAR_print_length_STAR_):(and__806_11))))?(((G__3717_11=xs_10),
(vec__3718_12=G__3717_11),
(x_13=clojure.core.nth.apply(null,[vec__3718_12,(0),null])),
(xs_14=clojure.core.nthrest.apply(null,[vec__3718_12,(1)])),
(print_length_15=clojure.core._STAR_print_length_STAR_),
((function __loop(){var _rtn,_cnt;(G__3717_16=G__3717_11),
(print_length_17=print_length_15);do{_cnt=0;
_rtn=((vec__3719_18=G__3717_16),
(x_19=clojure.core.nth.apply(null,[vec__3719_18,(0),null])),
(xs_20=clojure.core.nthrest.apply(null,[vec__3719_18,(1)])),
(print_length_21=print_length_17),
((clojure.lang.Numbers.isZero(print_length_21))?((w_6).write("...")):(print_one_2.apply(null,[x_19,w_6]),
((xs_20)?((w_6).write(sep_3),
(_cnt=1,_rtn=[xs_20,clojure.lang.Numbers.dec(print_length_21)],G__3717_16=_rtn[0],print_length_17=_rtn[1])):(null)))))}while(_cnt);return _rtn;})()))):(((G__3721_11=xs_10),
(vec__3722_12=G__3721_11),
(x_13=clojure.core.nth.apply(null,[vec__3722_12,(0),null])),
(xs_14=clojure.core.nthrest.apply(null,[vec__3722_12,(1)])),
((function __loop(){var _rtn,_cnt;(G__3721_15=G__3721_11);do{_cnt=0;
_rtn=((vec__3723_16=G__3721_15),
(x_17=clojure.core.nth.apply(null,[vec__3723_16,(0),null])),
(xs_18=clojure.core.nthrest.apply(null,[vec__3723_16,(1)])),
print_one_2.apply(null,[x_17,w_6]),
((xs_18)?((w_6).write(sep_3),
(_cnt=1,_rtn=[xs_18],G__3721_15=_rtn[0])):(null)))}while(_cnt);return _rtn;})())))))):(null))),
(w_6).write(end_4))))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())})))}).apply(null,[]);

//======
//(defn- print-meta [o w] (when-let [m (meta o)] (when (and (pos? (count m)) (or *print-dup* (and *print-meta* *print-readably*))) (.write w "#^") (if (and (= (count m) 1) (:tag m)) (pr-on (:tag m) w) (pr-on m w)) (.write w " "))))
//---
(function __clojure_core_fn_3726(){
return (clojure.JS.def(clojure.core,"print_meta",(function __clojure_core_fn_3726_print_meta_3728(o_1,w_2){
var and__806_7,and__806_5,m_4,and__806_5,temp__3008_3,or__820_6;
return (((temp__3008_3=clojure.core.meta.apply(null,[o_1])),
((temp__3008_3)?(((m_4=temp__3008_3),
((((and__806_5=clojure.lang.Numbers.isPos(clojure.core.count.apply(null,[m_4]))),
((and__806_5)?(((or__820_6=clojure.core._STAR_print_dup_STAR_),
((or__820_6)?(or__820_6):(((and__806_7=clojure.core._STAR_print_meta_STAR_),
((and__806_7)?(clojure.core._STAR_print_readably_STAR_):(and__806_7))))))):(and__806_5))))?((w_2).write("#^"),
((((and__806_5=clojure.lang.Util.equal(clojure.core.count.apply(null,[m_4]),(1))),
((and__806_5)?(clojure.core.keyword("","tag").apply(null,[m_4])):(and__806_5))))?(clojure.core.pr_on.apply(null,[clojure.core.keyword("","tag").apply(null,[m_4]),w_2])):(clojure.core.pr_on.apply(null,[m_4,w_2]))),
(w_2).write(" ")):(null)))):(null))))})))}).apply(null,[]);

//======
//(defmethod print-method nil [o w] (.write w "nil"))
//---
(function __clojure_core_fn_3732(){
return ((clojure.core.print_method).addMethod(null,(function __clojure_core_fn_3732_fn_3734(o_1,w_2){
return ((w_2).write("nil"))})))}).apply(null,[]);

//======
//(defmethod print-dup nil [o w] (print-method o w))
//---
(function __clojure_core_fn_3738(){
return ((clojure.core.print_dup).addMethod(null,(function __clojure_core_fn_3738_fn_3740(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defn print-ctor [o print-args w] (.write w "#=(") (.write w (RT/className (class o))) (.write w ". ") (print-args o w) (.write w ")"))
//---
(function __clojure_core_fn_3744(){
return (clojure.JS.def(clojure.core,"print_ctor",(function __clojure_core_fn_3744_print_ctor_3746(o_1,print_args_2,w_3){
return ((w_3).write("#=("),
(w_3).write(clojure.lang.RT.className(clojure.core.class_.apply(null,[o_1]))),
(w_3).write(". "),
print_args_2.apply(null,[o_1,w_3]),
(w_3).write(")"))})))}).apply(null,[]);

//======
//(defmethod print-method :default [o w] (.write w "#<") (.write w (RT/simpleClassName (class o))) (.write w " ") (.write w (str o)) (.write w ">"))
//---
(function __clojure_core_fn_3750(){
return ((clojure.core.print_method).addMethod(clojure.core.keyword("","default"),(function __clojure_core_fn_3750_fn_3752(o_1,w_2){
return ((w_2).write("#<"),
(w_2).write(clojure.lang.RT.simpleClassName(clojure.core.class_.apply(null,[o_1]))),
(w_2).write(" "),
(w_2).write(clojure.core.str.apply(null,[o_1])),
(w_2).write(">"))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.Keyword [o w] (.write w (str o)))
//---
(function __clojure_core_fn_3756(){
return ((clojure.core.print_method).addMethod(clojure.lang.Keyword,(function __clojure_core_fn_3756_fn_3758(o_1,w_2){
return ((w_2).write(clojure.core.str.apply(null,[o_1])))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.Keyword [o w] (print-method o w))
//---
(function __clojure_core_fn_3762(){
return ((clojure.core.print_dup).addMethod(clojure.lang.Keyword,(function __clojure_core_fn_3762_fn_3764(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method Number [o w] (.write w (str o)))
//---
(function __clojure_core_fn_3768(){
return ((clojure.core.print_method).addMethod(java.lang.Number,(function __clojure_core_fn_3768_fn_3770(o_1,w_2){
return ((w_2).write(clojure.core.str.apply(null,[o_1])))})))}).apply(null,[]);

//======
//(defmethod print-dup Number [o w] (print-ctor o (fn [o w] (print-dup (str o) w)) w))
//---
(function __clojure_core_fn_3774(){
return ((clojure.core.print_dup).addMethod(java.lang.Number,(function __clojure_core_fn_3774_fn_3776(o_1,w_2){
return (clojure.core.print_ctor.apply(null,[o_1,(function __clojure_core_fn_3774_fn_3776_fn_3778(o_1,w_2){
return (clojure.core.print_dup.apply(null,[clojure.core.str.apply(null,[o_1]),w_2]))}),w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.AFn [o w] (print-ctor o (fn [o w]) w))
//---
(function __clojure_core_fn_3783(){
return ((clojure.core.print_dup).addMethod(clojure.lang.AFn,(function __clojure_core_fn_3783_fn_3785(o_1,w_2){
return (clojure.core.print_ctor.apply(null,[o_1,(function __clojure_core_fn_3783_fn_3785_fn_3787(o_1,w_2){
return (null)}),w_2]))})))}).apply(null,[]);

//======
//(prefer-method print-dup clojure.lang.IPersistentCollection clojure.lang.AFn)
//---
(function __clojure_core_fn_3792(){
return ((clojure.core.print_dup).preferMethod(clojure.lang.IPersistentCollection,clojure.lang.AFn))}).apply(null,[]);

//======
//(prefer-method print-dup java.util.Map clojure.lang.AFn)
//---
(function __clojure_core_fn_3795(){
return ((clojure.core.print_dup).preferMethod(java.util.Map,clojure.lang.AFn))}).apply(null,[]);

//======
//(prefer-method print-dup java.util.Collection clojure.lang.AFn)
//---
(function __clojure_core_fn_3798(){
return ((clojure.core.print_dup).preferMethod(java.util.Collection,clojure.lang.AFn))}).apply(null,[]);

//======
//(defmethod print-method Boolean [o w] (.write w (str o)))
//---
(function __clojure_core_fn_3801(){
return ((clojure.core.print_method).addMethod(java.lang.Boolean,(function __clojure_core_fn_3801_fn_3803(o_1,w_2){
return ((w_2).write(clojure.core.str.apply(null,[o_1])))})))}).apply(null,[]);

//======
//(defmethod print-dup Boolean [o w] (print-method o w))
//---
(function __clojure_core_fn_3807(){
return ((clojure.core.print_dup).addMethod(java.lang.Boolean,(function __clojure_core_fn_3807_fn_3809(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defn print-simple [o w] (print-meta o w) (.write w (str o)))
//---
(function __clojure_core_fn_3813(){
return (clojure.JS.def(clojure.core,"print_simple",(function __clojure_core_fn_3813_print_simple_3815(o_1,w_2){
return (clojure.core.print_meta.apply(null,[o_1,w_2]),
(w_2).write(clojure.core.str.apply(null,[o_1])))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.Symbol [o w] (print-simple o w))
//---
(function __clojure_core_fn_3819(){
return ((clojure.core.print_method).addMethod(clojure.lang.Symbol,(function __clojure_core_fn_3819_fn_3821(o_1,w_2){
return (clojure.core.print_simple.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.Symbol [o w] (print-method o w))
//---
(function __clojure_core_fn_3825(){
return ((clojure.core.print_dup).addMethod(clojure.lang.Symbol,(function __clojure_core_fn_3825_fn_3827(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.Var [o w] (print-simple o w))
//---
(function __clojure_core_fn_3831(){
return ((clojure.core.print_method).addMethod(clojure.lang.Var,(function __clojure_core_fn_3831_fn_3833(o_1,w_2){
return (clojure.core.print_simple.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.Var [o w] (.write w (str "#=(var " (.name (.ns o)) "/" (.sym o) ")")))
//---
(function __clojure_core_fn_3837(){
return ((clojure.core.print_dup).addMethod(clojure.lang.Var,(function __clojure_core_fn_3837_fn_3839(o_1,w_2){
return ((w_2).write(clojure.core.str.apply(null,["#=(var ",clojure.JS.getOrRun(clojure.JS.getOrRun(o_1,"ns"),"name"),"/",clojure.JS.getOrRun(o_1,"sym"),")"])))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.ISeq [o w] (print-meta o w) (print-sequential "(" pr-on " " ")" o w))
//---
(function __clojure_core_fn_3843(){
return ((clojure.core.print_method).addMethod(clojure.lang.ISeq,(function __clojure_core_fn_3843_fn_3845(o_1,w_2){
return (clojure.core.print_meta.apply(null,[o_1,w_2]),
clojure.core.print_sequential.apply(null,["(",clojure.core.pr_on," ",")",o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.ISeq [o w] (print-method o w))
//---
(function __clojure_core_fn_3849(){
return ((clojure.core.print_dup).addMethod(clojure.lang.ISeq,(function __clojure_core_fn_3849_fn_3851(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.IPersistentList [o w] (print-method o w))
//---
(function __clojure_core_fn_3855(){
return ((clojure.core.print_dup).addMethod(clojure.lang.IPersistentList,(function __clojure_core_fn_3855_fn_3857(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(prefer-method print-method clojure.lang.IPersistentList clojure.lang.ISeq)
//---
(function __clojure_core_fn_3861(){
return ((clojure.core.print_method).preferMethod(clojure.lang.IPersistentList,clojure.lang.ISeq))}).apply(null,[]);

//======
//(prefer-method print-dup clojure.lang.IPersistentList clojure.lang.ISeq)
//---
(function __clojure_core_fn_3864(){
return ((clojure.core.print_dup).preferMethod(clojure.lang.IPersistentList,clojure.lang.ISeq))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentList [o w] (print-meta o w) (print-sequential "(" print-method " " ")" o w))
//---
(function __clojure_core_fn_3867(){
return ((clojure.core.print_method).addMethod(clojure.lang.IPersistentList,(function __clojure_core_fn_3867_fn_3869(o_1,w_2){
return (clojure.core.print_meta.apply(null,[o_1,w_2]),
clojure.core.print_sequential.apply(null,["(",clojure.core.print_method," ",")",o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method java.util.Collection [o w] (print-ctor o (fn* [p1__3873 p2__3874] (print-sequential "[" print-method " " "]" p1__3873 p2__3874)) w))
//---
(function __clojure_core_fn_3875(){
return ((clojure.core.print_method).addMethod(java.util.Collection,(function __clojure_core_fn_3875_fn_3877(o_1,w_2){
return (clojure.core.print_ctor.apply(null,[o_1,(function __clojure_core_fn_3875_fn_3877_fn_3879(p1__3873_1,p2__3874_2){
return (clojure.core.print_sequential.apply(null,["[",clojure.core.print_method," ","]",p1__3873_1,p2__3874_2]))}),w_2]))})))}).apply(null,[]);

//======
//(prefer-method print-method clojure.lang.IPersistentCollection java.util.Collection)
//---
(function __clojure_core_fn_3884(){
return ((clojure.core.print_method).preferMethod(clojure.lang.IPersistentCollection,java.util.Collection))}).apply(null,[]);

//======
//(defmethod print-dup java.util.Collection [o w] (print-ctor o (fn* [p1__3887 p2__3888] (print-sequential "[" print-dup " " "]" p1__3887 p2__3888)) w))
//---
(function __clojure_core_fn_3889(){
return ((clojure.core.print_dup).addMethod(java.util.Collection,(function __clojure_core_fn_3889_fn_3891(o_1,w_2){
return (clojure.core.print_ctor.apply(null,[o_1,(function __clojure_core_fn_3889_fn_3891_fn_3893(p1__3887_1,p2__3888_2){
return (clojure.core.print_sequential.apply(null,["[",clojure.core.print_dup," ","]",p1__3887_1,p2__3888_2]))}),w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.IPersistentCollection [o w] (print-meta o w) (.write w "#=(") (.write w (RT/className (class o))) (.write w "/create ") (print-sequential "[" print-dup " " "]" o w) (.write w ")"))
//---
(function __clojure_core_fn_3898(){
return ((clojure.core.print_dup).addMethod(clojure.lang.IPersistentCollection,(function __clojure_core_fn_3898_fn_3900(o_1,w_2){
return (clojure.core.print_meta.apply(null,[o_1,w_2]),
(w_2).write("#=("),
(w_2).write(clojure.lang.RT.className(clojure.core.class_.apply(null,[o_1]))),
(w_2).write("/create "),
clojure.core.print_sequential.apply(null,["[",clojure.core.print_dup," ","]",o_1,w_2]),
(w_2).write(")"))})))}).apply(null,[]);

//======
//(prefer-method print-dup clojure.lang.IPersistentCollection java.util.Collection)
//---
(function __clojure_core_fn_3904(){
return ((clojure.core.print_dup).preferMethod(clojure.lang.IPersistentCollection,java.util.Collection))}).apply(null,[]);

//======
//(def char-escape-string {\" "\\\"", \backspace "\\b", \tab "\\t", \newline "\\n", \formfeed "\\f", \return "\\r", \\ "\\\\"})
//---
(function __clojure_core_fn_3907(){
return (clojure.JS.def(clojure.core,"char_escape_string",clojure.core.hash_map("\"","\\\"","\b","\\b","\t","\\t","\n","\\n","\f","\\f","\r","\\r","\\","\\\\")))}).apply(null,[]);

//======
//(defmethod print-method String [s w] (if (or *print-dup* *print-readably*) (do (.append w \") (dotimes [n (count s)] (let [c (.charAt s n) e (char-escape-string c)] (if e (.write w e) (.append w c)))) (.append w \")) (.write w s)) nil)
//---
(function __clojure_core_fn_3910(){
return ((clojure.core.print_method).addMethod(java.lang.String,(function __clojure_core_fn_3910_fn_3912(s_1,w_2){
var n_4,or__820_3,c_5,n__1889_3,e_6;
return (((((or__820_3=clojure.core._STAR_print_dup_STAR_),
((or__820_3)?(or__820_3):(clojure.core._STAR_print_readably_STAR_))))?((w_2).append("\""),
((n__1889_3=clojure.lang.RT.intCast(clojure.core.count.apply(null,[s_1]))),
((function __loop(){var _rtn,_cnt;(n_4=clojure.lang.RT.intCast((0)));do{_cnt=0;
_rtn=((clojure.lang.Numbers.lt(n_4,n__1889_3))?(((c_5=(s_1).charAt(n_4)),
(e_6=clojure.core.char_escape_string.apply(null,[c_5])),
((e_6)?((w_2).write(e_6)):((w_2).append(c_5)))),
(_cnt=1,_rtn=[clojure.lang.Numbers.unchecked_inc(n_4)],n_4=_rtn[0])):(null))}while(_cnt);return _rtn;})())),
(w_2).append("\"")):((w_2).write(s_1))),
null)})))}).apply(null,[]);

//======
//(defmethod print-dup String [s w] (print-method s w))
//---
(function __clojure_core_fn_3916(){
return ((clojure.core.print_dup).addMethod(java.lang.String,(function __clojure_core_fn_3916_fn_3918(s_1,w_2){
return (clojure.core.print_method.apply(null,[s_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentVector [v w] (print-meta v w) (print-sequential "[" pr-on " " "]" v w))
//---
(function __clojure_core_fn_3922(){
return ((clojure.core.print_method).addMethod(clojure.lang.IPersistentVector,(function __clojure_core_fn_3922_fn_3924(v_1,w_2){
return (clojure.core.print_meta.apply(null,[v_1,w_2]),
clojure.core.print_sequential.apply(null,["[",clojure.core.pr_on," ","]",v_1,w_2]))})))}).apply(null,[]);

//======
//(defn- print-map [m print-one w] (print-sequential "{" (fn [e w] (do (print-one (key e) w) (.append w \space) (print-one (val e) w))) ", " "}" (seq m) w))
//---
(function __clojure_core_fn_3928(){
return (clojure.JS.def(clojure.core,"print_map",(function __clojure_core_fn_3928_print_map_3930(m_1,print_one_2,w_3){
return (clojure.core.print_sequential.apply(null,["{",(function __clojure_core_fn_3928_print_map_3930_fn_3932(e_1,w_2){
return (print_one_2.apply(null,[clojure.core.key.apply(null,[e_1]),w_2]),
(w_2).append(" "),
print_one_2.apply(null,[clojure.core.val.apply(null,[e_1]),w_2]))}),", ","}",clojure.core.seq.apply(null,[m_1]),w_3]))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentMap [m w] (print-meta m w) (print-map m pr-on w))
//---
(function __clojure_core_fn_3937(){
return ((clojure.core.print_method).addMethod(clojure.lang.IPersistentMap,(function __clojure_core_fn_3937_fn_3939(m_1,w_2){
return (clojure.core.print_meta.apply(null,[m_1,w_2]),
clojure.core.print_map.apply(null,[m_1,clojure.core.pr_on,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method java.util.Map [m w] (print-ctor m (fn* [p1__3943 p2__3944] (print-map (seq p1__3943) print-method p2__3944)) w))
//---
(function __clojure_core_fn_3945(){
return ((clojure.core.print_method).addMethod(java.util.Map,(function __clojure_core_fn_3945_fn_3947(m_1,w_2){
return (clojure.core.print_ctor.apply(null,[m_1,(function __clojure_core_fn_3945_fn_3947_fn_3949(p1__3943_1,p2__3944_2){
return (clojure.core.print_map.apply(null,[clojure.core.seq.apply(null,[p1__3943_1]),clojure.core.print_method,p2__3944_2]))}),w_2]))})))}).apply(null,[]);

//======
//(prefer-method print-method clojure.lang.IPersistentMap java.util.Map)
//---
(function __clojure_core_fn_3954(){
return ((clojure.core.print_method).preferMethod(clojure.lang.IPersistentMap,java.util.Map))}).apply(null,[]);

//======
//(defmethod print-dup java.util.Map [m w] (print-ctor m (fn* [p1__3957 p2__3958] (print-map (seq p1__3957) print-dup p2__3958)) w))
//---
(function __clojure_core_fn_3959(){
return ((clojure.core.print_dup).addMethod(java.util.Map,(function __clojure_core_fn_3959_fn_3961(m_1,w_2){
return (clojure.core.print_ctor.apply(null,[m_1,(function __clojure_core_fn_3959_fn_3961_fn_3963(p1__3957_1,p2__3958_2){
return (clojure.core.print_map.apply(null,[clojure.core.seq.apply(null,[p1__3957_1]),clojure.core.print_dup,p2__3958_2]))}),w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.IPersistentMap [m w] (print-meta m w) (.write w "#=(") (.write w (RT/className (class m))) (.write w "/create ") (print-map m print-dup w) (.write w ")"))
//---
(function __clojure_core_fn_3968(){
return ((clojure.core.print_dup).addMethod(clojure.lang.IPersistentMap,(function __clojure_core_fn_3968_fn_3970(m_1,w_2){
return (clojure.core.print_meta.apply(null,[m_1,w_2]),
(w_2).write("#=("),
(w_2).write(clojure.lang.RT.className(clojure.core.class_.apply(null,[m_1]))),
(w_2).write("/create "),
clojure.core.print_map.apply(null,[m_1,clojure.core.print_dup,w_2]),
(w_2).write(")"))})))}).apply(null,[]);

//======
//(prefer-method print-dup clojure.lang.IPersistentCollection java.util.Map)
//---
(function __clojure_core_fn_3974(){
return ((clojure.core.print_dup).preferMethod(clojure.lang.IPersistentCollection,java.util.Map))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentSet [s w] (print-meta s w) (print-sequential "#{" pr-on " " "}" (seq s) w))
//---
(function __clojure_core_fn_3977(){
return ((clojure.core.print_method).addMethod(clojure.lang.IPersistentSet,(function __clojure_core_fn_3977_fn_3979(s_1,w_2){
return (clojure.core.print_meta.apply(null,[s_1,w_2]),
clojure.core.print_sequential.apply(null,["#{",clojure.core.pr_on," ","}",clojure.core.seq.apply(null,[s_1]),w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method java.util.Set [s w] (print-ctor s (fn* [p1__3983 p2__3984] (print-sequential "#{" print-method " " "}" (seq p1__3983) p2__3984)) w))
//---
(function __clojure_core_fn_3985(){
return ((clojure.core.print_method).addMethod(java.util.Set,(function __clojure_core_fn_3985_fn_3987(s_1,w_2){
return (clojure.core.print_ctor.apply(null,[s_1,(function __clojure_core_fn_3985_fn_3987_fn_3989(p1__3983_1,p2__3984_2){
return (clojure.core.print_sequential.apply(null,["#{",clojure.core.print_method," ","}",clojure.core.seq.apply(null,[p1__3983_1]),p2__3984_2]))}),w_2]))})))}).apply(null,[]);

//======
//(def char-name-string {\space "space", \backspace "backspace", \tab "tab", \newline "newline", \formfeed "formfeed", \return "return"})
//---
(function __clojure_core_fn_3994(){
return (clojure.JS.def(clojure.core,"char_name_string",clojure.core.hash_map(" ","space","\b","backspace","\t","tab","\n","newline","\f","formfeed","\r","return")))}).apply(null,[]);

//======
//(defmethod print-method java.lang.Character [c w] (if (or *print-dup* *print-readably*) (do (.append w \\) (let [n (char-name-string c)] (if n (.write w n) (.append w c)))) (.append w c)) nil)
//---
(function __clojure_core_fn_3997(){
return ((clojure.core.print_method).addMethod(java.lang.Character,(function __clojure_core_fn_3997_fn_3999(c_1,w_2){
var n_3,or__820_3;
return (((((or__820_3=clojure.core._STAR_print_dup_STAR_),
((or__820_3)?(or__820_3):(clojure.core._STAR_print_readably_STAR_))))?((w_2).append("\\"),
((n_3=clojure.core.char_name_string.apply(null,[c_1])),
((n_3)?((w_2).write(n_3)):((w_2).append(c_1))))):((w_2).append(c_1))),
null)})))}).apply(null,[]);

//======
//(defmethod print-dup java.lang.Character [c w] (print-method c w))
//---
(function __clojure_core_fn_4003(){
return ((clojure.core.print_dup).addMethod(java.lang.Character,(function __clojure_core_fn_4003_fn_4005(c_1,w_2){
return (clojure.core.print_method.apply(null,[c_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup java.lang.Integer [o w] (print-method o w))
//---
(function __clojure_core_fn_4009(){
return ((clojure.core.print_dup).addMethod(java.lang.Integer,(function __clojure_core_fn_4009_fn_4011(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup java.lang.Double [o w] (print-method o w))
//---
(function __clojure_core_fn_4015(){
return ((clojure.core.print_dup).addMethod(java.lang.Double,(function __clojure_core_fn_4015_fn_4017(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.Ratio [o w] (print-method o w))
//---
(function __clojure_core_fn_4021(){
return ((clojure.core.print_dup).addMethod(clojure.lang.Ratio,(function __clojure_core_fn_4021_fn_4023(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup java.math.BigDecimal [o w] (print-method o w))
//---
(function __clojure_core_fn_4027(){
return ((clojure.core.print_dup).addMethod(java.math.BigDecimal,(function __clojure_core_fn_4027_fn_4029(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.PersistentHashMap [o w] (print-method o w))
//---
(function __clojure_core_fn_4033(){
return ((clojure.core.print_dup).addMethod(clojure.lang.PersistentHashMap,(function __clojure_core_fn_4033_fn_4035(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.PersistentHashSet [o w] (print-method o w))
//---
(function __clojure_core_fn_4039(){
return ((clojure.core.print_dup).addMethod(clojure.lang.PersistentHashSet,(function __clojure_core_fn_4039_fn_4041(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.PersistentVector [o w] (print-method o w))
//---
(function __clojure_core_fn_4045(){
return ((clojure.core.print_dup).addMethod(clojure.lang.PersistentVector,(function __clojure_core_fn_4045_fn_4047(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);
// Skipping: (def primitives-classnames {Double/TYPE "Double/TYPE", Character/TYPE "Character/TYPE", Byte/TYPE "Byte/TYPE", Boolean/TYPE "Boolean/TYPE", Short/TYPE "Short/TYPE", Float/TYPE "Float/TYPE", Long/TYPE "Long/TYPE", Integer/TYPE "Integer/TYPE"})
// Skipping: (defmethod print-method Class [c w] (.write w (RT/className c)))
// Skipping: (defmethod print-dup Class [c w] (cond (.isPrimitive c) (do (.write w "#=(identity ") (.write w (primitives-classnames c)) (.write w ")")) (.isArray c) (do (.write w "#=(java.lang.Class/forName \"") (.write w (RT/className c)) (.write w "\")")) :else (do (.write w "#=") (.write w (RT/className c)))))

//======
//(defmethod print-method java.math.BigDecimal [b w] (.write w (str b)) (.write w "M"))
//---
(function __clojure_core_fn_4066(){
return ((clojure.core.print_method).addMethod(java.math.BigDecimal,(function __clojure_core_fn_4066_fn_4068(b_1,w_2){
return ((w_2).write(clojure.core.str.apply(null,[b_1])),
(w_2).write("M"))})))}).apply(null,[]);

//======
//(defmethod print-method java.util.regex.Pattern [p w] (.write w "#\"") (loop [[c & r :as s] (seq (.pattern p)) qmode false] (when s (cond (= c \\) (let [[c2 & r2] r] (.append w \\) (.append w c2) (if qmode (recur r2 (not= c2 \E)) (recur r2 (= c2 \Q)))) (= c \") (do (if qmode (.write w "\\E\\\"\\Q") (.write w "\\\"")) (recur r qmode)) :else (do (.append w c) (recur r qmode))))) (.append w \"))
//---
(function __clojure_core_fn_4072(){
return ((clojure.core.print_method).addMethod(java.util.regex.Pattern,(function __clojure_core_fn_4072_fn_4074(p_1,w_2){
var r_6,qmode_10,qmode_8,r_13,c_12,G__4077_3,c_5,r2_18,vec__4080_16,vec__4078_4,s_7,c2_17,qmode_15,s_14,G__4077_9,vec__4079_11;
return ((w_2).write("#\""),
((G__4077_3=clojure.core.seq.apply(null,[(p_1).pattern()])),
(vec__4078_4=G__4077_3),
(c_5=clojure.core.nth.apply(null,[vec__4078_4,(0),null])),
(r_6=clojure.core.nthrest.apply(null,[vec__4078_4,(1)])),
(s_7=vec__4078_4),
(qmode_8=false),
((function __loop(){var _rtn,_cnt;(G__4077_9=G__4077_3),
(qmode_10=qmode_8);do{_cnt=0;
_rtn=((vec__4079_11=G__4077_9),
(c_12=clojure.core.nth.apply(null,[vec__4079_11,(0),null])),
(r_13=clojure.core.nthrest.apply(null,[vec__4079_11,(1)])),
(s_14=vec__4079_11),
(qmode_15=qmode_10),
((s_14)?(((clojure.lang.Util.equal(c_12,"\\"))?(((vec__4080_16=r_13),
(c2_17=clojure.core.nth.apply(null,[vec__4080_16,(0),null])),
(r2_18=clojure.core.nthrest.apply(null,[vec__4080_16,(1)])),
(w_2).append("\\"),
(w_2).append(c2_17),
((qmode_15)?((_cnt=1,_rtn=[r2_18,clojure.core.not_EQ_.apply(null,[c2_17,"E"])],G__4077_9=_rtn[0],qmode_10=_rtn[1])):((_cnt=1,_rtn=[r2_18,clojure.lang.Util.equal(c2_17,"Q")],G__4077_9=_rtn[0],qmode_10=_rtn[1]))))):(((clojure.lang.Util.equal(c_12,"\""))?(((qmode_15)?((w_2).write("\\E\\\"\\Q")):((w_2).write("\\\""))),
(_cnt=1,_rtn=[r_13,qmode_15],G__4077_9=_rtn[0],qmode_10=_rtn[1])):(((clojure.core.keyword("","else"))?((w_2).append(c_12),
(_cnt=1,_rtn=[r_13,qmode_15],G__4077_9=_rtn[0],qmode_10=_rtn[1])):(null))))))):(null)))}while(_cnt);return _rtn;})())),
(w_2).append("\""))})))}).apply(null,[]);

//======
//(defmethod print-dup java.util.regex.Pattern [p w] (print-method p w))
//---
(function __clojure_core_fn_4083(){
return ((clojure.core.print_dup).addMethod(java.util.regex.Pattern,(function __clojure_core_fn_4083_fn_4085(p_1,w_2){
return (clojure.core.print_method.apply(null,[p_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.Namespace [n w] (.write w "#=(find-ns ") (print-dup (.name n) w) (.write w ")"))
//---
(function __clojure_core_fn_4089(){
return ((clojure.core.print_dup).addMethod(clojure.lang.Namespace,(function __clojure_core_fn_4089_fn_4091(n_1,w_2){
return ((w_2).write("#=(find-ns "),
clojure.core.print_dup.apply(null,[clojure.JS.getOrRun(n_1,"name"),w_2]),
(w_2).write(")"))})))}).apply(null,[]);

//======
//(def print-initialized true)
//---
(function __clojure_core_fn_4095(){
return (clojure.JS.def(clojure.core,"print_initialized",true))}).apply(null,[]);
