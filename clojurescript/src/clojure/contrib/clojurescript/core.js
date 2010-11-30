
//======
//(ns clojure.core)
//---
(function __user_fn_520(){
return (clojure.core.in_ns.apply(null,[clojure.core.symbol("clojure.core")]))}).apply(null,[]);

//======
//(def unquote)
//---
(function __clojure_core_fn_526(){
return (clojure.JS.def(clojure.core,"unquote",null))}).apply(null,[]);

//======
//(def list (. clojure.lang.PersistentList creator))
//---
(function __clojure_core_fn_529(){
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
(function __clojure_core_fn_562(){
return (clojure.JS.def(clojure.core,"conj",clojure.JS.variadic(2,(function __clojure_core_fn_562_conj_564(coll_1,x_2){switch(arguments.length){
case 2:var conj_0=arguments.callee;
return (clojure.lang.RT.conj(coll_1,x_2))}
var _cnt,_rtn,conj_0=arguments.callee,xs_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((xs_3)?((_cnt=1,_rtn=[conj_0.apply(null,[coll_1,x_2]),clojure.core.first.apply(null,[xs_3]),clojure.core.rest.apply(null,[xs_3])],coll_1=_rtn[0],x_2=_rtn[1],xs_3=_rtn[2])):(conj_0.apply(null,[coll_1,x_2])))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(def second (fn second [x] (first (rest x))))
//---
(function __clojure_core_fn_567(){
return (clojure.JS.def(clojure.core,"second",(function __clojure_core_fn_567_second_569(x_1){
var second_0=arguments.callee;
return (clojure.core.first.apply(null,[clojure.core.rest.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def ffirst (fn ffirst [x] (first (first x))))
//---
(function __clojure_core_fn_572(){
return (clojure.JS.def(clojure.core,"ffirst",(function __clojure_core_fn_572_ffirst_574(x_1){
var ffirst_0=arguments.callee;
return (clojure.core.first.apply(null,[clojure.core.first.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def rfirst (fn rfirst [x] (rest (first x))))
//---
(function __clojure_core_fn_577(){
return (clojure.JS.def(clojure.core,"rfirst",(function __clojure_core_fn_577_rfirst_579(x_1){
var rfirst_0=arguments.callee;
return (clojure.core.rest.apply(null,[clojure.core.first.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def frest (fn frest [x] (first (rest x))))
//---
(function __clojure_core_fn_582(){
return (clojure.JS.def(clojure.core,"frest",(function __clojure_core_fn_582_frest_584(x_1){
var frest_0=arguments.callee;
return (clojure.core.first.apply(null,[clojure.core.rest.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def rrest (fn rrest [x] (rest (rest x))))
//---
(function __clojure_core_fn_587(){
return (clojure.JS.def(clojure.core,"rrest",(function __clojure_core_fn_587_rrest_589(x_1){
var rrest_0=arguments.callee;
return (clojure.core.rest.apply(null,[clojure.core.rest.apply(null,[x_1])]))})))}).apply(null,[]);
// Skipping: (def seq (fn seq [coll] (. clojure.lang.RT (seq coll))))
// Skipping: (def instance? (fn instance? [c x] (. c (isInstance x))))

//======
//(def seq? (fn seq? [x] (instance? clojure.lang.ISeq x)))
//---
(function __clojure_core_fn_602(){
return (clojure.JS.def(clojure.core,"seq_QMARK_",(function __clojure_core_fn_602_seq_QMARK_604(x_1){
var seq_QMARK__0=arguments.callee;
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.ISeq,x_1]))})))}).apply(null,[]);
// Skipping: (def string? (fn string? [x] (instance? String x)))

//======
//(def map? (fn map? [x] (instance? clojure.lang.IPersistentMap x)))
//---
(function __clojure_core_fn_612(){
return (clojure.JS.def(clojure.core,"map_QMARK_",(function __clojure_core_fn_612_map_QMARK_614(x_1){
var map_QMARK__0=arguments.callee;
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.IPersistentMap,x_1]))})))}).apply(null,[]);

//======
//(def vector? (fn vector? [x] (instance? clojure.lang.IPersistentVector x)))
//---
(function __clojure_core_fn_617(){
return (clojure.JS.def(clojure.core,"vector_QMARK_",(function __clojure_core_fn_617_vector_QMARK_619(x_1){
var vector_QMARK__0=arguments.callee;
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.IPersistentVector,x_1]))})))}).apply(null,[]);

//======
//(def sigs (fn [fdecl] (if (seq? (first fdecl)) (loop [ret [] fdecl fdecl] (if fdecl (recur (conj ret (first (first fdecl))) (rest fdecl)) (seq ret))) (list (first fdecl)))))
//---
(function __clojure_core_fn_622(){
return (clojure.JS.def(clojure.core,"sigs",(function __clojure_core_fn_622_sigs_624(fdecl_1){
var ret_2,fdecl_3;
return (((clojure.core.seq_QMARK_.apply(null,[clojure.core.first.apply(null,[fdecl_1])]))?(((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentVector.EMPTY),
(fdecl_3=fdecl_1);do{_cnt=0;
_rtn=((fdecl_3)?((_cnt=1,_rtn=[clojure.core.conj.apply(null,[ret_2,clojure.core.first.apply(null,[clojure.core.first.apply(null,[fdecl_3])])]),clojure.core.rest.apply(null,[fdecl_3])],ret_2=_rtn[0],fdecl_3=_rtn[1])):(clojure.core.seq.apply(null,[ret_2])))}while(_cnt);return _rtn;})())):(clojure.core.list.apply(null,[clojure.core.first.apply(null,[fdecl_1])]))))})))}).apply(null,[]);
// Skipping: (def assoc (fn assoc ([map key val] (. clojure.lang.RT (assoc map key val))) ([map key val & kvs] (let [ret (assoc map key val)] (if kvs (recur ret (first kvs) (second kvs) (rrest kvs)) ret)))))

//======
//(def meta (fn meta [x] (if (instance? clojure.lang.IMeta x) (. x (meta)))))
//---
(function __clojure_core_fn_633(){
return (clojure.JS.def(clojure.core,"meta",(function __clojure_core_fn_633_meta_635(x_1){
var meta_0=arguments.callee;
return (((clojure.core.instance_QMARK_.apply(null,[clojure.lang.IMeta,x_1]))?((x_1).meta()):(null)))})))}).apply(null,[]);

//======
//(def with-meta (fn with-meta [x m] (. x (withMeta m))))
//---
(function __clojure_core_fn_638(){
return (clojure.JS.def(clojure.core,"with_meta",(function __clojure_core_fn_638_with_meta_640(x_1,m_2){
var with_meta_0=arguments.callee;
return ((x_1).withMeta(m_2))})))}).apply(null,[]);

//======
//(def last (fn last [s] (if (rest s) (recur (rest s)) (first s))))
//---
(function __clojure_core_fn_643(){
return (clojure.JS.def(clojure.core,"last",(function __clojure_core_fn_643_last_645(s_1){
var _cnt,_rtn,last_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.core.rest.apply(null,[s_1]))?((_cnt=1,_rtn=[clojure.core.rest.apply(null,[s_1])],s_1=_rtn[0])):(clojure.core.first.apply(null,[s_1])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def butlast (fn butlast [s] (loop [ret [] s s] (if (rest s) (recur (conj ret (first s)) (rest s)) (seq ret)))))
//---
(function __clojure_core_fn_648(){
return (clojure.JS.def(clojure.core,"butlast",(function __clojure_core_fn_648_butlast_650(s_1){
var ret_2,s_3,butlast_0=arguments.callee;
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentVector.EMPTY),
(s_3=s_1);do{_cnt=0;
_rtn=((clojure.core.rest.apply(null,[s_3]))?((_cnt=1,_rtn=[clojure.core.conj.apply(null,[ret_2,clojure.core.first.apply(null,[s_3])]),clojure.core.rest.apply(null,[s_3])],ret_2=_rtn[0],s_3=_rtn[1])):(clojure.core.seq.apply(null,[ret_2])))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);
// Skipping: (def defn (fn defn [name & fdecl] (let [m (if (string? (first fdecl)) {:doc (first fdecl)} {}) fdecl (if (string? (first fdecl)) (rest fdecl) fdecl) m (if (map? (first fdecl)) (conj m (first fdecl)) m) fdecl (if (map? (first fdecl)) (rest fdecl) fdecl) fdecl (if (vector? (first fdecl)) (list fdecl) fdecl) m (if (map? (last fdecl)) (conj m (last fdecl)) m) fdecl (if (map? (last fdecl)) (butlast fdecl) fdecl) m (conj {:arglists (list (quote quote) (sigs fdecl))} m)] (list (quote def) (with-meta name (conj (if (meta name) (meta name) {}) m)) (cons (quote clojure.core/fn) fdecl)))))
// Skipping: (. (var defn) (setMacro))

//======
//(defn cast "Throws a ClassCastException if x is not a c, else returns x." [c x] (. c (cast x)))
//---
(function __clojure_core_fn_661(){
return (clojure.JS.def(clojure.core,"cast",(function __clojure_core_fn_661_cast_663(c_1,x_2){
return ((c_1).cast(x_2))})))}).apply(null,[]);
// Skipping: (defn to-array "Returns an array of Objects containing the contents of coll, which\n  can be any Collection.  Maps to java.util.Collection.toArray()." [coll] (. clojure.lang.RT (toArray coll)))

//======
//(defn vector "Creates a new vector containing the args." ([] []) ([& args] (. clojure.lang.LazilyPersistentVector (create args))))
//---
(function __clojure_core_fn_673(){
return (clojure.JS.def(clojure.core,"vector",clojure.JS.variadic(0,(function __clojure_core_fn_673_vector_675(){switch(arguments.length){
case 0:return (clojure.lang.PersistentVector.EMPTY)}
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.LazilyPersistentVector.create(args_1))}))))}).apply(null,[]);

//======
//(defn vec "Creates a new vector containing the contents of coll." ([coll] (. clojure.lang.LazilyPersistentVector (createOwning (to-array coll)))))
//---
(function __clojure_core_fn_680(){
return (clojure.JS.def(clojure.core,"vec",(function __clojure_core_fn_680_vec_682(coll_1){
return (clojure.lang.LazilyPersistentVector.createOwning(clojure.core.to_array.apply(null,[coll_1])))})))}).apply(null,[]);
// Skipping: (defn hash-map "keyval => key val\n  Returns a new hash map with supplied mappings." ([] {}) ([& keyvals] (. clojure.lang.PersistentHashMap (create keyvals))))

//======
//(defn hash-set "Returns a new hash set with supplied keys." ([] #{}) ([& keys] (. clojure.lang.PersistentHashSet (create keys))))
//---
(function __clojure_core_fn_693(){
return (clojure.JS.def(clojure.core,"hash_set",clojure.JS.variadic(0,(function __clojure_core_fn_693_hash_set_695(){switch(arguments.length){
case 0:return (clojure.lang.PersistentHashSet.EMPTY)}
var keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentHashSet.create(keys_1))}))))}).apply(null,[]);

//======
//(defn sorted-map "keyval => key val\n  Returns a new sorted map with supplied mappings." ([& keyvals] (. clojure.lang.PersistentTreeMap (create keyvals))))
//---
(function __clojure_core_fn_700(){
return (clojure.JS.def(clojure.core,"sorted_map",clojure.JS.variadic(0,(function __clojure_core_fn_700_sorted_map_702(){
var keyvals_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentTreeMap.create(keyvals_1))}))))}).apply(null,[]);

//======
//(defn sorted-set "Returns a new sorted set with supplied keys." ([& keys] (. clojure.lang.PersistentTreeSet (create keys))))
//---
(function __clojure_core_fn_706(){
return (clojure.JS.def(clojure.core,"sorted_set",clojure.JS.variadic(0,(function __clojure_core_fn_706_sorted_set_708(){
var keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentTreeSet.create(keys_1))}))))}).apply(null,[]);

//======
//(defn sorted-map-by "keyval => key val\n  Returns a new sorted map with supplied mappings, using the supplied comparator." ([comparator & keyvals] (. clojure.lang.PersistentTreeMap (create comparator keyvals))))
//---
(function __clojure_core_fn_712(){
return (clojure.JS.def(clojure.core,"sorted_map_by",clojure.JS.variadic(1,(function __clojure_core_fn_712_sorted_map_by_714(comparator_1){
var keyvals_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentTreeMap.create(comparator_1,keyvals_2))}))))}).apply(null,[]);
// Skipping: (def defmacro (fn [name & args] (list (quote do) (cons (quote clojure.core/defn) (cons name args)) (list (quote .) (list (quote var) name) (quote (setMacro))))))
// Skipping: (. (var defmacro) (setMacro))
// Skipping: (defmacro when "Evaluates test. If logical true, evaluates body in an implicit do." [test & body] (list (quote if) test (cons (quote do) body)))
// Skipping: (defmacro when-not "Evaluates test. If logical false, evaluates body in an implicit do." [test & body] (list (quote if) test nil (cons (quote do) body)))

//======
//(defn nil? "Returns true if x is nil, false otherwise." {:tag Boolean} [x] (identical? x nil))
//---
(function __clojure_core_fn_745(){
return (clojure.JS.def(clojure.core,"nil_QMARK_",(function __clojure_core_fn_745_nil_QMARK_747(x_1){
return (clojure.core.identical_QMARK_.apply(null,[x_1,null]))})))}).apply(null,[]);

//======
//(defn false? "Returns true if x is the value false, false otherwise." {:tag Boolean} [x] (identical? x false))
//---
(function __clojure_core_fn_751(){
return (clojure.JS.def(clojure.core,"false_QMARK_",(function __clojure_core_fn_751_false_QMARK_753(x_1){
return (clojure.core.identical_QMARK_.apply(null,[x_1,false]))})))}).apply(null,[]);

//======
//(defn true? "Returns true if x is the value true, false otherwise." {:tag Boolean} [x] (identical? x true))
//---
(function __clojure_core_fn_757(){
return (clojure.JS.def(clojure.core,"true_QMARK_",(function __clojure_core_fn_757_true_QMARK_759(x_1){
return (clojure.core.identical_QMARK_.apply(null,[x_1,true]))})))}).apply(null,[]);

//======
//(defn not "Returns true if x is logical false, false otherwise." {:tag Boolean} [x] (if x false true))
//---
(function __clojure_core_fn_763(){
return (clojure.JS.def(clojure.core,"not",(function __clojure_core_fn_763_not_765(x_1){
return (((x_1)?(false):(true)))})))}).apply(null,[]);

//======
//(defn str "With no args, returns the empty string. With one arg x, returns\n  x.toString().  (str nil) returns the empty string. With more than\n  one arg, returns the concatenation of the str values of the args." {:tag String} ([] "") ([x] (if (nil? x) "" (. x (toString)))) ([x & ys] ((fn [sb more] (if more (recur (. sb (append (str (first more)))) (rest more)) (str sb))) (clojure.lang.RT/makeStringBuilder (str x)) ys)))
//---
(function __clojure_core_fn_769(){
return (clojure.JS.def(clojure.core,"str",clojure.JS.variadic(1,(function __clojure_core_fn_769_str_771(x_1){switch(arguments.length){
case 0:return ("")
case 1:return (((clojure.core.nil_QMARK_.apply(null,[x_1]))?(""):((x_1).toString())))}
var ys_2=clojure.JS.rest_args(this,arguments,1);
return ((function __clojure_core_fn_769_str_771_fn_775(sb_1,more_2){
var _cnt,_rtn;
do{_cnt=0;_rtn=((more_2)?((_cnt=1,_rtn=[(sb_1).append(clojure.core.str.apply(null,[clojure.core.first.apply(null,[more_2])])),clojure.core.rest.apply(null,[more_2])],sb_1=_rtn[0],more_2=_rtn[1])):(clojure.core.str.apply(null,[sb_1])))
}while(_cnt);return _rtn;}).apply(null,[clojure.lang.RT.makeStringBuilder(clojure.core.str.apply(null,[x_1])),ys_2]))}))))}).apply(null,[]);

//======
//(defn symbol? "Return true if x is a Symbol" [x] (instance? clojure.lang.Symbol x))
//---
(function __clojure_core_fn_780(){
return (clojure.JS.def(clojure.core,"symbol_QMARK_",(function __clojure_core_fn_780_symbol_QMARK_782(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Symbol,x_1]))})))}).apply(null,[]);

//======
//(defn keyword? "Return true if x is a Keyword" [x] (instance? clojure.lang.Keyword x))
//---
(function __clojure_core_fn_786(){
return (clojure.JS.def(clojure.core,"keyword_QMARK_",(function __clojure_core_fn_786_keyword_QMARK_788(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Keyword,x_1]))})))}).apply(null,[]);
// Skipping: (defn symbol "Returns a Symbol with the given namespace and name." ([name] (if (symbol? name) name (. clojure.lang.Symbol (intern name)))) ([ns name] (. clojure.lang.Symbol (intern ns name))))
// Skipping: (defn keyword "Returns a Keyword with the given namespace and name.  Do not use :\n  in the keyword strings, it will be added automatically." ([name] (if (keyword? name) name (. clojure.lang.Keyword (intern nil name)))) ([ns name] (. clojure.lang.Keyword (intern ns name))))

//======
//(defn gensym "Returns a new symbol with a unique name. If a prefix string is\n  supplied, the name is prefix# where # is some unique number. If\n  prefix is not supplied, the prefix is 'G__'." ([] (gensym "G__")) ([prefix-string] (. clojure.lang.Symbol (intern (str prefix-string (str (. clojure.lang.RT (nextID))))))))
//---
(function __clojure_core_fn_806(){
return (clojure.JS.def(clojure.core,"gensym",(function __clojure_core_fn_806_gensym_808(prefix_string_1){switch(arguments.length){
case 0:return (clojure.core.gensym.apply(null,["G__"]))}
return (clojure.lang.Symbol.intern(clojure.core.str.apply(null,[prefix_string_1,clojure.core.str.apply(null,[clojure.lang.RT.nextID()])])))})))}).apply(null,[]);
// Skipping: (defmacro cond "Takes a set of test/expr pairs. It evaluates each test one at a\n  time.  If a test returns logical true, cond evaluates and returns\n  the value of the corresponding expr and doesn't evaluate any of the\n  other tests or exprs. (cond) returns nil." [& clauses] (when clauses (list (quote if) (first clauses) (if (rest clauses) (second clauses) (throw (IllegalArgumentException. "cond requires an even number of forms"))) (cons (quote clojure.core/cond) (rest (rest clauses))))))

//======
//(defn spread {:private true} [arglist] (cond (nil? arglist) nil (nil? (rest arglist)) (seq (first arglist)) :else (cons (first arglist) (spread (rest arglist)))))
//---
(function __clojure_core_fn_822(){
return (clojure.JS.def(clojure.core,"spread",(function __clojure_core_fn_822_spread_824(arglist_1){
return (((clojure.core.nil_QMARK_.apply(null,[arglist_1]))?(null):(((clojure.core.nil_QMARK_.apply(null,[clojure.core.rest.apply(null,[arglist_1])]))?(clojure.core.seq.apply(null,[clojure.core.first.apply(null,[arglist_1])])):(((clojure.core.keyword("","else"))?(clojure.core.cons.apply(null,[clojure.core.first.apply(null,[arglist_1]),clojure.core.spread.apply(null,[clojure.core.rest.apply(null,[arglist_1])])])):(null)))))))})))}).apply(null,[]);
// Skipping: (defn apply "Applies fn f to the argument list formed by prepending args to argseq." {:arglists (quote ([f args* argseq]))} [f & args] (. f (applyTo (spread args))))

//======
//(defn vary-meta "Returns an object of the same type and value as obj, with\n  (apply f (meta obj) args) as its metadata." [obj f & args] (with-meta obj (apply f (meta obj) args)))
//---
(function __clojure_core_fn_834(){
return (clojure.JS.def(clojure.core,"vary_meta",clojure.JS.variadic(2,(function __clojure_core_fn_834_vary_meta_836(obj_1,f_2){
var args_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.with_meta.apply(null,[obj_1,clojure.core.apply.apply(null,[f_2,clojure.core.meta.apply(null,[obj_1]),args_3])]))}))))}).apply(null,[]);

//======
//(defn list* "Creates a new list containing the item prepended to more." [item & more] (spread (cons item more)))
//---
(function __clojure_core_fn_840(){
return (clojure.JS.def(clojure.core,"list_STAR_",clojure.JS.variadic(1,(function __clojure_core_fn_840_list_STAR_842(item_1){
var more_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.core.spread.apply(null,[clojure.core.cons.apply(null,[item_1,more_2])]))}))))}).apply(null,[]);
// Skipping: (defmacro delay "Takes a body of expressions and yields a Delay object that will\n  invoke the body only the first time it is forced (with force), and\n  will cache the result and return it on all subsequent force calls" [& body] (list (quote new) (quote clojure.lang.Delay) (list* (quote clojure.core/fn) [] body)))

//======
//(defn delay? "returns true if x is a Delay created with delay" [x] (instance? clojure.lang.Delay x))
//---
(function __clojure_core_fn_855(){
return (clojure.JS.def(clojure.core,"delay_QMARK_",(function __clojure_core_fn_855_delay_QMARK_857(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Delay,x_1]))})))}).apply(null,[]);

//======
//(defn force "If x is a Delay, returns the (possibly cached) value of its expression, else returns x" [x] (. clojure.lang.Delay (force x)))
//---
(function __clojure_core_fn_861(){
return (clojure.JS.def(clojure.core,"force",(function __clojure_core_fn_861_force_863(x_1){
return (clojure.lang.Delay.force(x_1))})))}).apply(null,[]);

//======
//(defn fnseq "Returns a seq object whose first is first and whose rest is the\n  value produced by calling restfn with no arguments. restfn will be\n  called at most once per step in the sequence, e.g. calling rest\n  repeatedly on the head of the seq calls restfn once - the value it\n  yields is cached." [first restfn] (new clojure.lang.FnSeq first restfn))
//---
(function __clojure_core_fn_867(){
return (clojure.JS.def(clojure.core,"fnseq",(function __clojure_core_fn_867_fnseq_869(first_1,restfn_2){
return ((new clojure.lang.FnSeq(first_1,restfn_2)))})))}).apply(null,[]);
// Skipping: (defmacro lazy-cons "Expands to code which produces a seq object whose first is\n  first-expr and whose rest is rest-expr, neither of which is\n  evaluated until first/rest is called. Each expr will be evaluated at most\n  once per step in the sequence, e.g. calling first/rest repeatedly on the\n  same node of the seq evaluates first/rest-expr once - the values they yield are\n  cached." [first-expr & rest-expr] (list (quote new) (quote clojure.lang.LazyCons) (list (quote clojure.core/fn) (list [] first-expr) (list* [(gensym)] rest-expr))))

//======
//(defn cache-seq "Given a seq s, returns a lazy seq that will touch each element of s\n  at most once, caching the results." [s] (when s (clojure.lang.CachedSeq. s)))
//---
(function __clojure_core_fn_882(){
return (clojure.JS.def(clojure.core,"cache_seq",(function __clojure_core_fn_882_cache_seq_884(s_1){
return (((s_1)?((new clojure.lang.CachedSeq(s_1))):(null)))})))}).apply(null,[]);

//======
//(defn concat "Returns a lazy seq representing the concatenation of\tthe elements in the supplied colls." ([] nil) ([x] (seq x)) ([x y] (if (seq x) (lazy-cons (first x) (concat (rest x) y)) (seq y))) ([x y & zs] (let [cat (fn cat [xys zs] (if (seq xys) (lazy-cons (first xys) (cat (rest xys) zs)) (when zs (recur (first zs) (rest zs)))))] (cat (concat x y) zs))))
//---
(function __clojure_core_fn_888(){
return (clojure.JS.def(clojure.core,"concat",clojure.JS.variadic(2,(function __clojure_core_fn_888_concat_890(x_1,y_2){switch(arguments.length){
case 0:return (null)
case 1:return (clojure.core.seq.apply(null,[x_1]))
case 2:return (((clojure.core.seq.apply(null,[x_1]))?((new clojure.lang.LazyCons((function __clojure_core_fn_888_concat_890_fn_895(G__894_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[x_1]))}
return (clojure.core.concat.apply(null,[clojure.core.rest.apply(null,[x_1]),y_2]))})))):(clojure.core.seq.apply(null,[y_2]))))}
var cat_4,zs_3=clojure.JS.rest_args(this,arguments,2);
return (((cat_4=(function __clojure_core_fn_888_concat_890_cat_900(xys_1,zs_2){
var _cnt,_rtn,cat_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[xys_1]))?((new clojure.lang.LazyCons((function __clojure_core_fn_888_concat_890_cat_900_fn_902(G__901_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[xys_1]))}
return (cat_0.apply(null,[clojure.core.rest.apply(null,[xys_1]),zs_2]))})))):(((zs_2)?((_cnt=1,_rtn=[clojure.core.first.apply(null,[zs_2]),clojure.core.rest.apply(null,[zs_2])],xys_1=_rtn[0],zs_2=_rtn[1])):(null))))
}while(_cnt);return _rtn;})),
cat_4.apply(null,[clojure.core.concat.apply(null,[x_1,y_2]),zs_3])))}))))}).apply(null,[]);
// Skipping: (defmacro if-not "Evaluates test. If logical false, evaluates and returns then expr, otherwise else expr, if supplied, else nil." ([test then] (clojure.core/concat (clojure.core/list (quote clojure.core/if-not)) (clojure.core/list test) (clojure.core/list then) (clojure.core/list (quote nil)))) ([test then else] (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/not)) (clojure.core/list test))) (clojure.core/list then) (clojure.core/list else))))

//======
//(defn = "Equality. Returns true if x equals y, false if not. Same as\n  Java x.equals(y) except it also works for nil, and compares\n  numbers and collections in a type-independent manner.  Clojure's immutable data\n  structures define equals() (and thus =) as a value, not an identity,\n  comparison." {:tag Boolean, :inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Util)) (clojure.core/list (quote clojure.core/equiv)) (clojure.core/list x) (clojure.core/list y))), :inline-arities #{2}} ([x] true) ([x y] (clojure.lang.Util/equiv x y)) ([x y & more] (if (= x y) (if (rest more) (recur y (first more) (rest more)) (= y (first more))) false)))
//---
(function __clojure_core_fn_920(){
return (clojure.JS.def(clojure.core,"_EQ_",clojure.JS.variadic(2,(function __clojure_core_fn_920_EQ_925(x_1,y_2){switch(arguments.length){
case 1:return (true)
case 2:return (clojure.lang.Util.equiv(x_1,y_2))}
var _cnt,_rtn,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Util.equiv(x_1,y_2))?(((clojure.core.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.core.first.apply(null,[more_3]),clojure.core.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Util.equiv(y_2,clojure.core.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn not= "Same as (not (= obj1 obj2))" {:tag Boolean} ([x] false) ([x y] (not (= x y))) ([x y & more] (not (apply = x y more))))
//---
(function __clojure_core_fn_931(){
return (clojure.JS.def(clojure.core,"not_EQ_",clojure.JS.variadic(2,(function __clojure_core_fn_931_not_EQ_933(x_1,y_2){switch(arguments.length){
case 1:return (false)
case 2:return (clojure.core.not.apply(null,[clojure.lang.Util.equiv(x_1,y_2)]))}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.not.apply(null,[clojure.core.apply.apply(null,[clojure.core._EQ_,x_1,y_2,more_3])]))}))))}).apply(null,[]);

//======
//(defn compare "Comparator. Returns 0 if x equals y, -1 if x is logically 'less\n  than' y, else 1. Same as Java x.compareTo(y) except it also works\n  for nil, and compares numbers and collections in a type-independent\n  manner. x must implement Comparable" {:tag Integer, :inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Util)) (clojure.core/list (quote clojure.core/compare)) (clojure.core/list x) (clojure.core/list y)))} [x y] (. clojure.lang.Util (compare x y)))
//---
(function __clojure_core_fn_939(){
return (clojure.JS.def(clojure.core,"compare",(function __clojure_core_fn_939_compare_944(x_1,y_2){
return (clojure.lang.Util.compare(x_1,y_2))})))}).apply(null,[]);
// Skipping: (defmacro and "Evaluates exprs one at a time, from left to right. If a form\n  returns logical false (nil or false), and returns that value and\n  doesn't evaluate any of the other expressions, otherwise it returns\n  the value of the last expr. (and) returns true." ([] true) ([x] x) ([x & rest] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote and__948__auto__)) (clojure.core/list x)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (quote and__948__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/and)) rest)) (clojure.core/list (quote and__948__auto__)))))))
// Skipping: (defmacro or "Evaluates exprs one at a time, from left to right. If a form\n  returns a logical true value, or returns that value and doesn't\n  evaluate any of the other expressions, otherwise it returns the\n  value of the last expression. (or) returns nil." ([] nil) ([x] x) ([x & rest] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote or__962__auto__)) (clojure.core/list x)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (quote or__962__auto__)) (clojure.core/list (quote or__962__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/or)) rest)))))))

//======
//(defn reduce "f should be a function of 2 arguments. If val is not supplied,\n  returns the result of applying f to the first 2 items in coll, then\n  applying f to that result and the 3rd item, etc. If coll contains no\n  items, f must accept no arguments as well, and reduce returns the\n  result of calling f with no arguments.  If coll has only 1 item, it\n  is returned and f is not called.  If val is supplied, returns the\n  result of applying f to val and the first item in coll, then\n  applying f to that result and the 2nd item, etc. If coll contains no\n  items, returns val and f is not called." ([f coll] (let [s (seq coll)] (if s (if (instance? clojure.lang.IReduce s) (. s (reduce f)) (reduce f (first s) (rest s))) (f)))) ([f val coll] (let [s (seq coll)] (if (instance? clojure.lang.IReduce s) (. s (reduce f val)) ((fn [f val s] (if s (recur f (f val (first s)) (rest s)) val)) f val s)))))
//---
(function __clojure_core_fn_976(){
return (clojure.JS.def(clojure.core,"reduce",(function __clojure_core_fn_976_reduce_978(f_1,val_2,coll_3){switch(arguments.length){
case 2:var s_3,coll_2=arguments[1];
return (((s_3=clojure.core.seq.apply(null,[coll_2])),
((s_3)?(((clojure.core.instance_QMARK_.apply(null,[clojure.lang.IReduce,s_3]))?((s_3).reduce(f_1)):(clojure.core.reduce.apply(null,[f_1,clojure.core.first.apply(null,[s_3]),clojure.core.rest.apply(null,[s_3])])))):(f_1.apply(null,[])))))}
var s_4;
return (((s_4=clojure.core.seq.apply(null,[coll_3])),
((clojure.core.instance_QMARK_.apply(null,[clojure.lang.IReduce,s_4]))?((s_4).reduce(f_1,val_2)):((function __clojure_core_fn_976_reduce_978_fn_981(f_1,val_2,s_3){
var _cnt,_rtn;
do{_cnt=0;_rtn=((s_3)?((_cnt=1,_rtn=[f_1,f_1.apply(null,[val_2,clojure.core.first.apply(null,[s_3])]),clojure.core.rest.apply(null,[s_3])],f_1=_rtn[0],val_2=_rtn[1],s_3=_rtn[2])):(val_2))
}while(_cnt);return _rtn;}).apply(null,[f_1,val_2,s_4])))))})))}).apply(null,[]);

//======
//(defn reverse "Returns a seq of the items in coll in reverse order. Not lazy." [coll] (reduce conj nil coll))
//---
(function __clojure_core_fn_986(){
return (clojure.JS.def(clojure.core,"reverse",(function __clojure_core_fn_986_reverse_988(coll_1){
return (clojure.core.reduce.apply(null,[clojure.core.conj,null,coll_1]))})))}).apply(null,[]);

//======
//(defn + "Returns the sum of nums. (+) returns 0." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/add)) (clojure.core/list x) (clojure.core/list y))))), :inline-arities #{2}} ([] 0) ([x] (clojure.lang.RT/numberCast x)) ([x y] (. clojure.lang.Numbers (add x y))) ([x y & more] (reduce + (+ x y) more)))
//---
(function __clojure_core_fn_992(){
return (clojure.JS.def(clojure.core,"_PLUS_",clojure.JS.variadic(2,(function __clojure_core_fn_992_PLUS_997(x_1,y_2){switch(arguments.length){
case 0:return ((0))
case 1:return (clojure.lang.RT.numberCast(x_1))
case 2:return (clojure.lang.Numbers.add(x_1,y_2))}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.reduce.apply(null,[clojure.core._PLUS_,clojure.lang.Numbers.add(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn * "Returns the product of nums. (*) returns 1." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/multiply)) (clojure.core/list x) (clojure.core/list y))))), :inline-arities #{2}} ([] 1) ([x] (clojure.lang.RT/numberCast x)) ([x y] (. clojure.lang.Numbers (multiply x y))) ([x y & more] (reduce * (* x y) more)))
//---
(function __clojure_core_fn_1004(){
return (clojure.JS.def(clojure.core,"_STAR_",clojure.JS.variadic(2,(function __clojure_core_fn_1004_STAR_1009(x_1,y_2){switch(arguments.length){
case 0:return ((1))
case 1:return (clojure.lang.RT.numberCast(x_1))
case 2:return (clojure.lang.Numbers.multiply(x_1,y_2))}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.reduce.apply(null,[clojure.core._STAR_,clojure.lang.Numbers.multiply(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn / "If no denominators are supplied, returns 1/numerator,\n  else returns numerator divided by all of the denominators." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/divide)) (clojure.core/list x) (clojure.core/list y))))), :inline-arities #{2}} ([x] (/ 1 x)) ([x y] (. clojure.lang.Numbers (divide x y))) ([x y & more] (reduce / (/ x y) more)))
//---
(function __clojure_core_fn_1016(){
return (clojure.JS.def(clojure.core,"_SLASH_",clojure.JS.variadic(2,(function __clojure_core_fn_1016_SLASH_1021(x_1,y_2){switch(arguments.length){
case 1:return (clojure.lang.Numbers.divide((1),x_1))
case 2:return (clojure.lang.Numbers.divide(x_1,y_2))}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.reduce.apply(null,[clojure.core._SLASH_,clojure.lang.Numbers.divide(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn - "If no ys are supplied, returns the negation of x, else subtracts\n  the ys from x and returns the result." {:inline (fn [& args] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/minus)) args)))), :inline-arities #{1 2}} ([x] (. clojure.lang.Numbers (minus x))) ([x y] (. clojure.lang.Numbers (minus x y))) ([x y & more] (reduce - (- x y) more)))
//---
(function __clojure_core_fn_1027(){
return (clojure.JS.def(clojure.core,"_",clojure.JS.variadic(2,(function __clojure_core_fn_1027_1032(x_1,y_2){switch(arguments.length){
case 1:return (clojure.lang.Numbers.minus(x_1))
case 2:return (clojure.lang.Numbers.minus(x_1,y_2))}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.reduce.apply(null,[clojure.core._,clojure.lang.Numbers.minus(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn < "Returns non-nil if nums are in monotonically increasing order,\n  otherwise false." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/lt)) (clojure.core/list x) (clojure.core/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (lt x y))) ([x y & more] (if (< x y) (if (rest more) (recur y (first more) (rest more)) (< y (first more))) false)))
//---
(function __clojure_core_fn_1038(){
return (clojure.JS.def(clojure.core,"_LT_",clojure.JS.variadic(2,(function __clojure_core_fn_1038_LT_1043(x_1,y_2){switch(arguments.length){
case 1:return (true)
case 2:return (clojure.lang.Numbers.lt(x_1,y_2))}
var _cnt,_rtn,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.lt(x_1,y_2))?(((clojure.core.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.core.first.apply(null,[more_3]),clojure.core.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.lt(y_2,clojure.core.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn <= "Returns non-nil if nums are in monotonically non-decreasing order,\n  otherwise false." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/lte)) (clojure.core/list x) (clojure.core/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (lte x y))) ([x y & more] (if (<= x y) (if (rest more) (recur y (first more) (rest more)) (<= y (first more))) false)))
//---
(function __clojure_core_fn_1049(){
return (clojure.JS.def(clojure.core,"_LT__EQ_",clojure.JS.variadic(2,(function __clojure_core_fn_1049_LT_EQ_1054(x_1,y_2){switch(arguments.length){
case 1:return (true)
case 2:return (clojure.lang.Numbers.lte(x_1,y_2))}
var _cnt,_rtn,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.lte(x_1,y_2))?(((clojure.core.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.core.first.apply(null,[more_3]),clojure.core.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.lte(y_2,clojure.core.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn > "Returns non-nil if nums are in monotonically decreasing order,\n  otherwise false." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/gt)) (clojure.core/list x) (clojure.core/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (gt x y))) ([x y & more] (if (> x y) (if (rest more) (recur y (first more) (rest more)) (> y (first more))) false)))
//---
(function __clojure_core_fn_1060(){
return (clojure.JS.def(clojure.core,"_GT_",clojure.JS.variadic(2,(function __clojure_core_fn_1060_GT_1065(x_1,y_2){switch(arguments.length){
case 1:return (true)
case 2:return (clojure.lang.Numbers.gt(x_1,y_2))}
var _cnt,_rtn,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.gt(x_1,y_2))?(((clojure.core.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.core.first.apply(null,[more_3]),clojure.core.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.gt(y_2,clojure.core.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn >= "Returns non-nil if nums are in monotonically non-increasing order,\n  otherwise false." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/gte)) (clojure.core/list x) (clojure.core/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (gte x y))) ([x y & more] (if (>= x y) (if (rest more) (recur y (first more) (rest more)) (>= y (first more))) false)))
//---
(function __clojure_core_fn_1071(){
return (clojure.JS.def(clojure.core,"_GT__EQ_",clojure.JS.variadic(2,(function __clojure_core_fn_1071_GT_EQ_1076(x_1,y_2){switch(arguments.length){
case 1:return (true)
case 2:return (clojure.lang.Numbers.gte(x_1,y_2))}
var _cnt,_rtn,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.gte(x_1,y_2))?(((clojure.core.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.core.first.apply(null,[more_3]),clojure.core.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.gte(y_2,clojure.core.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn == "Returns non-nil if nums all have the same value, otherwise false" {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/equiv)) (clojure.core/list x) (clojure.core/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (equiv x y))) ([x y & more] (if (== x y) (if (rest more) (recur y (first more) (rest more)) (== y (first more))) false)))
//---
(function __clojure_core_fn_1082(){
return (clojure.JS.def(clojure.core,"_EQ__EQ_",clojure.JS.variadic(2,(function __clojure_core_fn_1082_EQ_EQ_1087(x_1,y_2){switch(arguments.length){
case 1:return (true)
case 2:return (clojure.lang.Numbers.equiv(x_1,y_2))}
var _cnt,_rtn,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.equiv(x_1,y_2))?(((clojure.core.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.core.first.apply(null,[more_3]),clojure.core.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.equiv(y_2,clojure.core.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn max "Returns the greatest of the nums." ([x] x) ([x y] (if (> x y) x y)) ([x y & more] (reduce max (max x y) more)))
//---
(function __clojure_core_fn_1093(){
return (clojure.JS.def(clojure.core,"max",clojure.JS.variadic(2,(function __clojure_core_fn_1093_max_1095(x_1,y_2){switch(arguments.length){
case 1:return (x_1)
case 2:return (((clojure.lang.Numbers.gt(x_1,y_2))?(x_1):(y_2)))}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.reduce.apply(null,[clojure.core.max,clojure.core.max.apply(null,[x_1,y_2]),more_3]))}))))}).apply(null,[]);

//======
//(defn min "Returns the least of the nums." ([x] x) ([x y] (if (< x y) x y)) ([x y & more] (reduce min (min x y) more)))
//---
(function __clojure_core_fn_1101(){
return (clojure.JS.def(clojure.core,"min",clojure.JS.variadic(2,(function __clojure_core_fn_1101_min_1103(x_1,y_2){switch(arguments.length){
case 1:return (x_1)
case 2:return (((clojure.lang.Numbers.lt(x_1,y_2))?(x_1):(y_2)))}
var more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.reduce.apply(null,[clojure.core.min,clojure.core.min.apply(null,[x_1,y_2]),more_3]))}))))}).apply(null,[]);

//======
//(defn inc "Returns a number one greater than num." {:inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/inc)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (inc x)))
//---
(function __clojure_core_fn_1109(){
return (clojure.JS.def(clojure.core,"inc",(function __clojure_core_fn_1109_inc_1114(x_1){
return (clojure.lang.Numbers.inc(x_1))})))}).apply(null,[]);

//======
//(defn dec "Returns a number one less than num." {:inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/dec)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (dec x)))
//---
(function __clojure_core_fn_1118(){
return (clojure.JS.def(clojure.core,"dec",(function __clojure_core_fn_1118_dec_1123(x_1){
return (clojure.lang.Numbers.dec(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-inc "Returns a number one greater than x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_inc)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (unchecked_inc x)))
//---
(function __clojure_core_fn_1127(){
return (clojure.JS.def(clojure.core,"unchecked_inc",(function __clojure_core_fn_1127_unchecked_inc_1132(x_1){
return (clojure.lang.Numbers.unchecked_inc(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-dec "Returns a number one less than x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_dec)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (unchecked_dec x)))
//---
(function __clojure_core_fn_1136(){
return (clojure.JS.def(clojure.core,"unchecked_dec",(function __clojure_core_fn_1136_unchecked_dec_1141(x_1){
return (clojure.lang.Numbers.unchecked_dec(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-negate "Returns the negation of x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_negate)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (unchecked_negate x)))
//---
(function __clojure_core_fn_1145(){
return (clojure.JS.def(clojure.core,"unchecked_negate",(function __clojure_core_fn_1145_unchecked_negate_1150(x_1){
return (clojure.lang.Numbers.unchecked_negate(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-add "Returns the sum of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_add)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_add x y)))
//---
(function __clojure_core_fn_1154(){
return (clojure.JS.def(clojure.core,"unchecked_add",(function __clojure_core_fn_1154_unchecked_add_1159(x_1,y_2){
return (clojure.lang.Numbers.unchecked_add(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-subtract "Returns the difference of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_subtract)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_subtract x y)))
//---
(function __clojure_core_fn_1163(){
return (clojure.JS.def(clojure.core,"unchecked_subtract",(function __clojure_core_fn_1163_unchecked_subtract_1168(x_1,y_2){
return (clojure.lang.Numbers.unchecked_subtract(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-multiply "Returns the product of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_multiply)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_multiply x y)))
//---
(function __clojure_core_fn_1172(){
return (clojure.JS.def(clojure.core,"unchecked_multiply",(function __clojure_core_fn_1172_unchecked_multiply_1177(x_1,y_2){
return (clojure.lang.Numbers.unchecked_multiply(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-divide "Returns the division of x by y, both int or long. \n  Note - uses a primitive operator subject to truncation." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_divide)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_divide x y)))
//---
(function __clojure_core_fn_1181(){
return (clojure.JS.def(clojure.core,"unchecked_divide",(function __clojure_core_fn_1181_unchecked_divide_1186(x_1,y_2){
return (clojure.lang.Numbers.unchecked_divide(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-remainder "Returns the remainder of division of x by y, both int or long. \n  Note - uses a primitive operator subject to truncation." {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked_remainder)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_remainder x y)))
//---
(function __clojure_core_fn_1190(){
return (clojure.JS.def(clojure.core,"unchecked_remainder",(function __clojure_core_fn_1190_unchecked_remainder_1195(x_1,y_2){
return (clojure.lang.Numbers.unchecked_remainder(x_1,y_2))})))}).apply(null,[]);

//======
//(defn pos? "Returns true if num is greater than zero, else false" {:tag Boolean, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/isPos)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (isPos x)))
//---
(function __clojure_core_fn_1199(){
return (clojure.JS.def(clojure.core,"pos_QMARK_",(function __clojure_core_fn_1199_pos_QMARK_1204(x_1){
return (clojure.lang.Numbers.isPos(x_1))})))}).apply(null,[]);

//======
//(defn neg? "Returns true if num is less than zero, else false" {:tag Boolean, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/isNeg)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (isNeg x)))
//---
(function __clojure_core_fn_1208(){
return (clojure.JS.def(clojure.core,"neg_QMARK_",(function __clojure_core_fn_1208_neg_QMARK_1213(x_1){
return (clojure.lang.Numbers.isNeg(x_1))})))}).apply(null,[]);

//======
//(defn zero? "Returns true if num is zero, else false" {:tag Boolean, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/isZero)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (isZero x)))
//---
(function __clojure_core_fn_1217(){
return (clojure.JS.def(clojure.core,"zero_QMARK_",(function __clojure_core_fn_1217_zero_QMARK_1222(x_1){
return (clojure.lang.Numbers.isZero(x_1))})))}).apply(null,[]);

//======
//(defn quot "quot[ient] of dividing numerator by denominator." [num div] (. clojure.lang.Numbers (quotient num div)))
//---
(function __clojure_core_fn_1226(){
return (clojure.JS.def(clojure.core,"quot",(function __clojure_core_fn_1226_quot_1228(num_1,div_2){
return (clojure.lang.Numbers.quotient(num_1,div_2))})))}).apply(null,[]);

//======
//(defn rem "remainder of dividing numerator by denominator." [num div] (. clojure.lang.Numbers (remainder num div)))
//---
(function __clojure_core_fn_1232(){
return (clojure.JS.def(clojure.core,"rem",(function __clojure_core_fn_1232_rem_1234(num_1,div_2){
return (clojure.lang.Numbers.remainder(num_1,div_2))})))}).apply(null,[]);

//======
//(defn rationalize "returns the rational value of num" [num] (. clojure.lang.Numbers (rationalize num)))
//---
(function __clojure_core_fn_1238(){
return (clojure.JS.def(clojure.core,"rationalize",(function __clojure_core_fn_1238_rationalize_1240(num_1){
return (clojure.lang.Numbers.rationalize(num_1))})))}).apply(null,[]);

//======
//(defn bit-not "Bitwise complement" {:inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/not)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers not x))
//---
(function __clojure_core_fn_1244(){
return (clojure.JS.def(clojure.core,"bit_not",(function __clojure_core_fn_1244_bit_not_1249(x_1){
return (clojure.lang.Numbers.not(x_1))})))}).apply(null,[]);

//======
//(defn bit-and "Bitwise and" {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/and)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers and x y))
//---
(function __clojure_core_fn_1253(){
return (clojure.JS.def(clojure.core,"bit_and",(function __clojure_core_fn_1253_bit_and_1258(x_1,y_2){
return (clojure.lang.Numbers.and(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-or "Bitwise or" {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/or)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers or x y))
//---
(function __clojure_core_fn_1262(){
return (clojure.JS.def(clojure.core,"bit_or",(function __clojure_core_fn_1262_bit_or_1267(x_1,y_2){
return (clojure.lang.Numbers.or(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-xor "Bitwise exclusive or" {:inline (fn [x y] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/xor)) (clojure.core/list x) (clojure.core/list y)))))} [x y] (. clojure.lang.Numbers xor x y))
//---
(function __clojure_core_fn_1271(){
return (clojure.JS.def(clojure.core,"bit_xor",(function __clojure_core_fn_1271_bit_xor_1276(x_1,y_2){
return (clojure.lang.Numbers.xor(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-and-not "Bitwise and with complement" [x y] (. clojure.lang.Numbers andNot x y))
//---
(function __clojure_core_fn_1280(){
return (clojure.JS.def(clojure.core,"bit_and_not",(function __clojure_core_fn_1280_bit_and_not_1282(x_1,y_2){
return (clojure.lang.Numbers.andNot(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-clear "Clear bit at index n" [x n] (. clojure.lang.Numbers clearBit x n))
//---
(function __clojure_core_fn_1286(){
return (clojure.JS.def(clojure.core,"bit_clear",(function __clojure_core_fn_1286_bit_clear_1288(x_1,n_2){
return (clojure.lang.Numbers.clearBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-set "Set bit at index n" [x n] (. clojure.lang.Numbers setBit x n))
//---
(function __clojure_core_fn_1292(){
return (clojure.JS.def(clojure.core,"bit_set",(function __clojure_core_fn_1292_bit_set_1294(x_1,n_2){
return (clojure.lang.Numbers.setBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-flip "Flip bit at index n" [x n] (. clojure.lang.Numbers flipBit x n))
//---
(function __clojure_core_fn_1298(){
return (clojure.JS.def(clojure.core,"bit_flip",(function __clojure_core_fn_1298_bit_flip_1300(x_1,n_2){
return (clojure.lang.Numbers.flipBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-test "Test bit at index n" [x n] (. clojure.lang.Numbers testBit x n))
//---
(function __clojure_core_fn_1304(){
return (clojure.JS.def(clojure.core,"bit_test",(function __clojure_core_fn_1304_bit_test_1306(x_1,n_2){
return (clojure.lang.Numbers.testBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-shift-left "Bitwise shift left" [x n] (. clojure.lang.Numbers shiftLeft x n))
//---
(function __clojure_core_fn_1310(){
return (clojure.JS.def(clojure.core,"bit_shift_left",(function __clojure_core_fn_1310_bit_shift_left_1312(x_1,n_2){
return (clojure.lang.Numbers.shiftLeft(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-shift-right "Bitwise shift right" [x n] (. clojure.lang.Numbers shiftRight x n))
//---
(function __clojure_core_fn_1316(){
return (clojure.JS.def(clojure.core,"bit_shift_right",(function __clojure_core_fn_1316_bit_shift_right_1318(x_1,n_2){
return (clojure.lang.Numbers.shiftRight(x_1,n_2))})))}).apply(null,[]);

//======
//(defn even? "Returns true if n is even, throws an exception if n is not an integer" [n] (zero? (bit-and n 1)))
//---
(function __clojure_core_fn_1322(){
return (clojure.JS.def(clojure.core,"even_QMARK_",(function __clojure_core_fn_1322_even_QMARK_1324(n_1){
return (clojure.lang.Numbers.isZero(clojure.lang.Numbers.and(n_1,(1))))})))}).apply(null,[]);

//======
//(defn odd? "Returns true if n is odd, throws an exception if n is not an integer" [n] (not (even? n)))
//---
(function __clojure_core_fn_1328(){
return (clojure.JS.def(clojure.core,"odd_QMARK_",(function __clojure_core_fn_1328_odd_QMARK_1330(n_1){
return (clojure.core.not.apply(null,[clojure.core.even_QMARK_.apply(null,[n_1])]))})))}).apply(null,[]);

//======
//(defn complement "Takes a fn f and returns a fn that takes the same arguments as f,\n  has the same effects, if any, and returns the opposite truth value." [f] (fn [& args] (not (apply f args))))
//---
(function __clojure_core_fn_1334(){
return (clojure.JS.def(clojure.core,"complement",(function __clojure_core_fn_1334_complement_1336(f_1){
return (clojure.JS.variadic(0,(function __clojure_core_fn_1334_complement_1336_fn_1338(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.not.apply(null,[clojure.core.apply.apply(null,[f_1,args_1])]))})))})))}).apply(null,[]);

//======
//(defn constantly "Returns a function that takes any number of arguments and returns x." [x] (fn [& args] x))
//---
(function __clojure_core_fn_1343(){
return (clojure.JS.def(clojure.core,"constantly",(function __clojure_core_fn_1343_constantly_1345(x_1){
return (clojure.JS.variadic(0,(function __clojure_core_fn_1343_constantly_1345_fn_1347(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (x_1)})))})))}).apply(null,[]);

//======
//(defn identity "Returns its argument." [x] x)
//---
(function __clojure_core_fn_1352(){
return (clojure.JS.def(clojure.core,"identity",(function __clojure_core_fn_1352_identity_1354(x_1){
return (x_1)})))}).apply(null,[]);
// Skipping: (defn count "Returns the number of items in the collection. (count nil) returns\n  0.  Also works on strings, arrays, and Java Collections and Maps" [coll] (. clojure.lang.RT (count coll)))

//======
//(defn peek "For a list or queue, same as first, for a vector, same as, but much\n  more efficient than, last. If the collection is empty, returns nil." [coll] (. clojure.lang.RT (peek coll)))
//---
(function __clojure_core_fn_1364(){
return (clojure.JS.def(clojure.core,"peek",(function __clojure_core_fn_1364_peek_1366(coll_1){
return (clojure.lang.RT.peek(coll_1))})))}).apply(null,[]);

//======
//(defn pop "For a list or queue, returns a new list/queue without the first\n  item, for a vector, returns a new vector without the last item. If\n  the collection is empty, throws an exception.  Note - not the same\n  as rest/butlast." [coll] (. clojure.lang.RT (pop coll)))
//---
(function __clojure_core_fn_1370(){
return (clojure.JS.def(clojure.core,"pop",(function __clojure_core_fn_1370_pop_1372(coll_1){
return (clojure.lang.RT.pop(coll_1))})))}).apply(null,[]);
// Skipping: (defn nth "Returns the value at the index. get returns nil if index out of\n  bounds, nth throws an exception unless not-found is supplied.  nth\n  also works for strings, Java arrays, regex Matchers and Lists, and,\n  in O(n) time, for sequences." ([coll index] (. clojure.lang.RT (nth coll index))) ([coll index not-found] (. clojure.lang.RT (nth coll index not-found))))
// Skipping: (defn contains? "Returns true if key is present in the given collection, otherwise\n  returns false.  Note that for numerically indexed collections like\n  vectors and Java arrays, this tests if the numeric key is within the\n  range of indexes. 'contains?' operates constant or logarithmic time;\n  it will not perform a linear search for a value.  See also 'some'." [coll key] (. clojure.lang.RT (contains coll key)))
// Skipping: (defn get "Returns the value mapped to key, not-found or nil if key not present." ([map key] (. clojure.lang.RT (get map key))) ([map key not-found] (. clojure.lang.RT (get map key not-found))))

//======
//(defn dissoc "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,\n  that does not contain a mapping for key(s)." ([map] map) ([map key] (. clojure.lang.RT (dissoc map key))) ([map key & ks] (let [ret (dissoc map key)] (if ks (recur ret (first ks) (rest ks)) ret))))
//---
(function __clojure_core_fn_1396(){
return (clojure.JS.def(clojure.core,"dissoc",clojure.JS.variadic(2,(function __clojure_core_fn_1396_dissoc_1398(map_1,key_2){switch(arguments.length){
case 1:return (map_1)
case 2:return (clojure.lang.RT.dissoc(map_1,key_2))}
var _cnt,_rtn,ret_4,ks_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((ret_4=clojure.core.dissoc.apply(null,[map_1,key_2])),
((ks_3)?((_cnt=1,_rtn=[ret_4,clojure.core.first.apply(null,[ks_3]),clojure.core.rest.apply(null,[ks_3])],map_1=_rtn[0],key_2=_rtn[1],ks_3=_rtn[2])):(ret_4)))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn disj "disj[oin]. Returns a new set of the same (hashed/sorted) type, that\n  does not contain key(s)." ([set] set) ([set key] (. set (disjoin key))) ([set key & ks] (let [ret (disj set key)] (if ks (recur ret (first ks) (rest ks)) ret))))
//---
(function __clojure_core_fn_1404(){
return (clojure.JS.def(clojure.core,"disj",clojure.JS.variadic(2,(function __clojure_core_fn_1404_disj_1406(set_1,key_2){switch(arguments.length){
case 1:return (set_1)
case 2:return ((set_1).disjoin(key_2))}
var _cnt,_rtn,ret_4,ks_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((ret_4=clojure.core.disj.apply(null,[set_1,key_2])),
((ks_3)?((_cnt=1,_rtn=[ret_4,clojure.core.first.apply(null,[ks_3]),clojure.core.rest.apply(null,[ks_3])],set_1=_rtn[0],key_2=_rtn[1],ks_3=_rtn[2])):(ret_4)))
}while(_cnt);return _rtn;}))))}).apply(null,[]);
// Skipping: (defn find "Returns the map entry for key, or nil if key not present." [map key] (. clojure.lang.RT (find map key)))

//======
//(defn select-keys "Returns a map containing only those entries in map whose key is in keys" [map keyseq] (loop [ret {} keys (seq keyseq)] (if keys (let [entry (. clojure.lang.RT (find map (first keys)))] (recur (if entry (conj ret entry) ret) (rest keys))) ret)))
//---
(function __clojure_core_fn_1418(){
return (clojure.JS.def(clojure.core,"select_keys",(function __clojure_core_fn_1418_select_keys_1420(map_1,keyseq_2){
var ret_3,keys_4,entry_5;
return (((function __loop(){var _rtn,_cnt;(ret_3=clojure.lang.PersistentArrayMap.EMPTY),
(keys_4=clojure.core.seq.apply(null,[keyseq_2]));do{_cnt=0;
_rtn=((keys_4)?(((entry_5=clojure.lang.RT.find(map_1,clojure.core.first.apply(null,[keys_4]))),
(_cnt=1,_rtn=[((entry_5)?(clojure.core.conj.apply(null,[ret_3,entry_5])):(ret_3)),clojure.core.rest.apply(null,[keys_4])],ret_3=_rtn[0],keys_4=_rtn[1]))):(ret_3))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);
// Skipping: (defn keys "Returns a sequence of the map's keys." [map] (. clojure.lang.RT (keys map)))
// Skipping: (defn vals "Returns a sequence of the map's values." [map] (. clojure.lang.RT (vals map)))

//======
//(defn key "Returns the key of the map entry." [e] (. e (getKey)))
//---
(function __clojure_core_fn_1436(){
return (clojure.JS.def(clojure.core,"key",(function __clojure_core_fn_1436_key_1438(e_1){
return ((e_1).getKey())})))}).apply(null,[]);

//======
//(defn val "Returns the value in the map entry." [e] (. e (getValue)))
//---
(function __clojure_core_fn_1442(){
return (clojure.JS.def(clojure.core,"val",(function __clojure_core_fn_1442_val_1444(e_1){
return ((e_1).getValue())})))}).apply(null,[]);

//======
//(defn rseq "Returns, in constant time, a sequence of the items in rev (which\n  can be a vector or sorted-map), in reverse order." [rev] (. rev (rseq)))
//---
(function __clojure_core_fn_1448(){
return (clojure.JS.def(clojure.core,"rseq",(function __clojure_core_fn_1448_rseq_1450(rev_1){
return ((rev_1).rseq())})))}).apply(null,[]);

//======
//(defn name "Returns the name String of a symbol or keyword." [x] (. x (getName)))
//---
(function __clojure_core_fn_1454(){
return (clojure.JS.def(clojure.core,"name",(function __clojure_core_fn_1454_name_1456(x_1){
return ((x_1).getName())})))}).apply(null,[]);

//======
//(defn namespace "Returns the namespace String of a symbol or keyword, or nil if not present." [x] (. x (getNamespace)))
//---
(function __clojure_core_fn_1460(){
return (clojure.JS.def(clojure.core,"namespace",(function __clojure_core_fn_1460_namespace_1462(x_1){
return ((x_1).getNamespace())})))}).apply(null,[]);
// Skipping: (defmacro locking "Executes exprs in an implicit do, while holding the monitor of x.\n  Will release the monitor of x in all circumstances." [x & body] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote lockee__1466__auto__)) (clojure.core/list x)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote try)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote monitor-enter)) (clojure.core/list (quote lockee__1466__auto__)))) body (clojure.core/list (clojure.core/concat (clojure.core/list (quote finally)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote monitor-exit)) (clojure.core/list (quote lockee__1466__auto__))))))))))
// Skipping: (defmacro .. "form => fieldName-symbol or (instanceMethodName-symbol args*)\n\n  Expands into a member access (.) of the first member on the first\n  argument, followed by the next member on the result, etc. For\n  instance:\n\n  (.. System (getProperties) (get \"os.name\"))\n\n  expands to:\n\n  (. (. System (getProperties)) (get \"os.name\"))\n\n  but is easier to write, read, and understand." ([x form] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list x) (clojure.core/list form))) ([x form & more] (clojure.core/concat (clojure.core/list (quote ..)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list x) (clojure.core/list form))) more)))
// Skipping: (defmacro -> "Threads the expr through the forms. Inserts x as the\n  second item in the first form, making a list of it if it is not a\n  list already. If there are more forms, inserts the first form as the\n  second item in second form, etc." ([x form] (if (seq? form) (clojure.core/concat (clojure.core/list (first form)) (clojure.core/list x) (rest form)) (list form x))) ([x form & more] (clojure.core/concat (clojure.core/list (quote clojure.core/->)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/->)) (clojure.core/list x) (clojure.core/list form))) more)))
// Skipping: (defmacro defmulti "Creates a new multimethod with the associated dispatch function. If\n  default-dispatch-val is supplied it becomes the default dispatch\n  value of the multimethod, otherwise the default dispatch value\n  is :default." ([name dispatch-fn] (clojure.core/concat (clojure.core/list (quote clojure.core/defmulti)) (clojure.core/list name) (clojure.core/list dispatch-fn) (clojure.core/list :default))) ([name dispatch-fn default-val] (clojure.core/concat (clojure.core/list (quote def)) (clojure.core/list (with-meta name (assoc (clojure.core/meta name) :tag (quote clojure.lang.MultiFn)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote new)) (clojure.core/list (quote clojure.lang.MultiFn)) (clojure.core/list dispatch-fn) (clojure.core/list default-val))))))
// Skipping: (defmacro defmethod "Creates and installs a new method of multimethod associated with dispatch-value. " [multifn dispatch-val & fn-tail] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list multifn) (clojure.core/list (quote clojure.core/addMethod)) (clojure.core/list dispatch-val) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/fn)) fn-tail))))

//======
//(defn remove-method "Removes the method of multimethod associated\twith dispatch-value." [multifn dispatch-val] (. multifn removeMethod dispatch-val))
//---
(function __clojure_core_fn_1518(){
return (clojure.JS.def(clojure.core,"remove_method",(function __clojure_core_fn_1518_remove_method_1520(multifn_1,dispatch_val_2){
return ((multifn_1).removeMethod(dispatch_val_2))})))}).apply(null,[]);

//======
//(defn prefer-method "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y when there is a conflict" [multifn dispatch-val-x dispatch-val-y] (. multifn preferMethod dispatch-val-x dispatch-val-y))
//---
(function __clojure_core_fn_1524(){
return (clojure.JS.def(clojure.core,"prefer_method",(function __clojure_core_fn_1524_prefer_method_1526(multifn_1,dispatch_val_x_2,dispatch_val_y_3){
return ((multifn_1).preferMethod(dispatch_val_x_2,dispatch_val_y_3))})))}).apply(null,[]);

//======
//(defn methods "Given a multimethod, returns a map of dispatch values -> dispatch fns" [multifn] (.getMethodTable multifn))
//---
(function __clojure_core_fn_1530(){
return (clojure.JS.def(clojure.core,"methods",(function __clojure_core_fn_1530_methods_1532(multifn_1){
return (clojure.JS.getOrRun(multifn_1,"getMethodTable"))})))}).apply(null,[]);

//======
//(defn prefers "Given a multimethod, returns a map of preferred value -> set of other values" [multifn] (.getPreferTable multifn))
//---
(function __clojure_core_fn_1536(){
return (clojure.JS.def(clojure.core,"prefers",(function __clojure_core_fn_1536_prefers_1538(multifn_1){
return (clojure.JS.getOrRun(multifn_1,"getPreferTable"))})))}).apply(null,[]);
// Skipping: (defmacro assert-args [fnname & pairs] (clojure.core/concat (clojure.core/list (quote do)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when-not)) (clojure.core/list (first pairs)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote throw)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote java.lang.IllegalArgumentException.)) (clojure.core/list (str fnname " requires " (second pairs))))))))) (clojure.core/list (let [more (rrest pairs)] (when more (list* (quote clojure.core/assert-args) fnname more))))))
// Skipping: (defmacro binding "binding => var-symbol init-expr \n\n  Creates new bindings for the (already-existing) vars, with the\n  supplied initial values, executes the exprs in an implicit do, then\n  re-establishes the bindings that existed before." [bindings & body] (assert-args binding (vector? bindings) "a vector for its binding" (even? (count bindings)) "an even number of forms in binding vector") (let [var-ize (fn [var-vals] (loop [ret [] vvs (seq var-vals)] (if vvs (recur (conj (conj ret (clojure.core/concat (clojure.core/list (quote var)) (clojure.core/list (first vvs)))) (second vvs)) (rest (rest vvs))) (seq ret))))] (clojure.core/concat (clojure.core/list (quote do)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Var)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/pushThreadBindings)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/hash-map)) (var-ize bindings))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote try)) body (clojure.core/list (clojure.core/concat (clojure.core/list (quote finally)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Var)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/popThreadBindings)))))))))))))

//======
//(defn find-var "Returns the global var named by the namespace-qualified symbol, or\n  nil if no var with that name." [sym] (. clojure.lang.Var (find sym)))
//---
(function __clojure_core_fn_1566(){
return (clojure.JS.def(clojure.core,"find_var",(function __clojure_core_fn_1566_find_var_1568(sym_1){
return (clojure.lang.Var.find(sym_1))})))}).apply(null,[]);

//======
//(defn setup-reference [r options] (let [opts (apply hash-map options)] (when (:meta opts) (.resetMeta r (:meta opts))) (when (:validator opts) (.setValidator r (:validator opts))) r))
//---
(function __clojure_core_fn_1572(){
return (clojure.JS.def(clojure.core,"setup_reference",(function __clojure_core_fn_1572_setup_reference_1574(r_1,options_2){
var opts_3;
return (((opts_3=clojure.core.apply.apply(null,[clojure.core.hash_map,options_2])),
((clojure.core.keyword("","meta").apply(null,[opts_3]))?((r_1).resetMeta(clojure.core.keyword("","meta").apply(null,[opts_3]))):(null)),
((clojure.core.keyword("","validator").apply(null,[opts_3]))?((r_1).setValidator(clojure.core.keyword("","validator").apply(null,[opts_3]))):(null)),
r_1))})))}).apply(null,[]);

//======
//(defn agent "Creates and returns an agent with an initial value of state and\n  zero or more options (in any order):\n  \n  :meta metadata-map\n  \n  :validator validate-fn\n\n  If metadata-map is supplied, it will be come the metadata on the\n  agent. validate-fn must be nil or a side-effect-free fn of one\n  argument, which will be passed the intended new state on any state\n  change. If the new state is unacceptable, the validate-fn should\n  return false or throw an exception." ([state] (new clojure.lang.Agent state)) ([state & options] (setup-reference (agent state) options)))
//---
(function __clojure_core_fn_1578(){
return (clojure.JS.def(clojure.core,"agent",clojure.JS.variadic(1,(function __clojure_core_fn_1578_agent_1580(state_1){switch(arguments.length){
case 1:return ((new clojure.lang.Agent(state_1)))}
var options_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.core.setup_reference.apply(null,[clojure.core.agent.apply(null,[state_1]),options_2]))}))))}).apply(null,[]);

//======
//(defn send "Dispatch an action to an agent. Returns the agent immediately.\n  Subsequently, in a thread from a thread pool, the state of the agent\n  will be set to the value of:\n\n  (apply action-fn state-of-agent args)" [a f & args] (. a (dispatch f args false)))
//---
(function __clojure_core_fn_1585(){
return (clojure.JS.def(clojure.core,"send",clojure.JS.variadic(2,(function __clojure_core_fn_1585_send_1587(a_1,f_2){
var args_3=clojure.JS.rest_args(this,arguments,2);
return ((a_1).dispatch(f_2,args_3,false))}))))}).apply(null,[]);

//======
//(defn send-off "Dispatch a potentially blocking action to an agent. Returns the\n  agent immediately. Subsequently, in a separate thread, the state of\n  the agent will be set to the value of:\n\n  (apply action-fn state-of-agent args)" [a f & args] (. a (dispatch f args true)))
//---
(function __clojure_core_fn_1591(){
return (clojure.JS.def(clojure.core,"send_off",clojure.JS.variadic(2,(function __clojure_core_fn_1591_send_off_1593(a_1,f_2){
var args_3=clojure.JS.rest_args(this,arguments,2);
return ((a_1).dispatch(f_2,args_3,true))}))))}).apply(null,[]);

//======
//(defn release-pending-sends "Normally, actions sent directly or indirectly during another action\n  are held until the action completes (changes the agent's\n  state). This function can be used to dispatch any pending sent\n  actions immediately. This has no impact on actions sent during a\n  transaction, which are still held until commit. If no action is\n  occurring, does nothing. Returns the number of actions dispatched." [] (clojure.lang.Agent/releasePendingSends))
//---
(function __clojure_core_fn_1597(){
return (clojure.JS.def(clojure.core,"release_pending_sends",(function __clojure_core_fn_1597_release_pending_sends_1599(){
return (clojure.lang.Agent.releasePendingSends())})))}).apply(null,[]);

//======
//(defn add-watcher "Experimental.\n  Adds a watcher to an agent/atom/var/ref reference. The watcher must\n  be an Agent, and the action a function of the agent's state and one\n  additional arg, the reference. Whenever the reference's state\n  changes, any registered watchers will have their actions\n  sent. send-type must be one of :send or :send-off. The actions will\n  be sent after the reference's state is changed. Var watchers are\n  triggered only by root binding changes, not thread-local set!s" [reference send-type watcher-agent action-fn] (.addWatch reference watcher-agent action-fn (= send-type :send-off)))
//---
(function __clojure_core_fn_1603(){
return (clojure.JS.def(clojure.core,"add_watcher",(function __clojure_core_fn_1603_add_watcher_1605(reference_1,send_type_2,watcher_agent_3,action_fn_4){
return ((reference_1).addWatch(watcher_agent_3,action_fn_4,clojure.lang.Util.equiv(send_type_2,clojure.core.keyword("","send-off"))))})))}).apply(null,[]);

//======
//(defn remove-watcher "Experimental.\n  Removes a watcher (set by add-watcher) from a reference" [reference watcher-agent] (.removeWatch reference watcher-agent))
//---
(function __clojure_core_fn_1609(){
return (clojure.JS.def(clojure.core,"remove_watcher",(function __clojure_core_fn_1609_remove_watcher_1611(reference_1,watcher_agent_2){
return ((reference_1).removeWatch(watcher_agent_2))})))}).apply(null,[]);

//======
//(defn agent-errors "Returns a sequence of the exceptions thrown during asynchronous\n  actions of the agent." [a] (. a (getErrors)))
//---
(function __clojure_core_fn_1615(){
return (clojure.JS.def(clojure.core,"agent_errors",(function __clojure_core_fn_1615_agent_errors_1617(a_1){
return ((a_1).getErrors())})))}).apply(null,[]);

//======
//(defn clear-agent-errors "Clears any exceptions thrown during asynchronous actions of the\n  agent, allowing subsequent actions to occur." [a] (. a (clearErrors)))
//---
(function __clojure_core_fn_1621(){
return (clojure.JS.def(clojure.core,"clear_agent_errors",(function __clojure_core_fn_1621_clear_agent_errors_1623(a_1){
return ((a_1).clearErrors())})))}).apply(null,[]);

//======
//(defn shutdown-agents "Initiates a shutdown of the thread pools that back the agent\n  system. Running actions will complete, but no new actions will be\n  accepted" [] (. clojure.lang.Agent shutdown))
//---
(function __clojure_core_fn_1627(){
return (clojure.JS.def(clojure.core,"shutdown_agents",(function __clojure_core_fn_1627_shutdown_agents_1629(){
return (clojure.lang.Agent.shutdown())})))}).apply(null,[]);

//======
//(defn ref "Creates and returns a Ref with an initial value of x and zero or\n  more options (in any order):\n  \n  :meta metadata-map\n  \n  :validator validate-fn\n\n  If metadata-map is supplied, it will be come the metadata on the\n  ref. validate-fn must be nil or a side-effect-free fn of one\n  argument, which will be passed the intended new state on any state\n  change. If the new state is unacceptable, the validate-fn should\n  return false or throw an exception. validate-fn will be called on\n  transaction commit, when all refs have their final values." ([x] (new clojure.lang.Ref x)) ([x & options] (setup-reference (ref x) options)))
//---
(function __clojure_core_fn_1633(){
return (clojure.JS.def(clojure.core,"ref",clojure.JS.variadic(1,(function __clojure_core_fn_1633_ref_1635(x_1){switch(arguments.length){
case 1:return ((new clojure.lang.Ref(x_1)))}
var options_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.core.setup_reference.apply(null,[clojure.core.ref.apply(null,[x_1]),options_2]))}))))}).apply(null,[]);

//======
//(defn deref "Also reader macro: @ref/@agent/@var/@atom Within a transaction,\n  returns the in-transaction-value of ref, else returns the\n  most-recently-committed value of ref. When applied to a var, agent\n  or atom, returns its current state." [ref] (. ref (get)))
//---
(function __clojure_core_fn_1640(){
return (clojure.JS.def(clojure.core,"deref",(function __clojure_core_fn_1640_deref_1642(ref_1){
return ((ref_1).get())})))}).apply(null,[]);

//======
//(defn atom "Creates and returns an Atom with an initial value of x and zero or\n  more options (in any order):\n  \n  :meta metadata-map\n  \n  :validator validate-fn\n\n  If metadata-map is supplied, it will be come the metadata on the\n  atom. validate-fn must be nil or a side-effect-free fn of one\n  argument, which will be passed the intended new state on any state\n  change. If the new state is unacceptable, the validate-fn should\n  return false or throw an exception." ([x] (new clojure.lang.Atom x)) ([x & options] (setup-reference (atom x) options)))
//---
(function __clojure_core_fn_1646(){
return (clojure.JS.def(clojure.core,"atom",clojure.JS.variadic(1,(function __clojure_core_fn_1646_atom_1648(x_1){switch(arguments.length){
case 1:return ((new clojure.lang.Atom(x_1)))}
var options_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.core.setup_reference.apply(null,[clojure.core.atom.apply(null,[x_1]),options_2]))}))))}).apply(null,[]);

//======
//(defn swap! "Atomically swaps the value of atom to be:\n  (apply f current-value-of-atom args). Note that f may be called\n  multiple times, and thus should be free of side effects.  Returns\n  the value that was swapped in." ([atom f] (.swap atom f)) ([atom f x] (.swap atom f x)) ([atom f x y] (.swap atom f x y)) ([atom f x y & args] (.swap atom f x y args)))
//---
(function __clojure_core_fn_1653(){
return (clojure.JS.def(clojure.core,"swap_BANG_",clojure.JS.variadic(4,(function __clojure_core_fn_1653_swap_BANG_1655(atom_1,f_2,x_3,y_4){switch(arguments.length){
case 2:return ((atom_1).swap(f_2))
case 3:return ((atom_1).swap(f_2,x_3))
case 4:return ((atom_1).swap(f_2,x_3,y_4))}
var args_5=clojure.JS.rest_args(this,arguments,4);
return ((atom_1).swap(f_2,x_3,y_4,args_5))}))))}).apply(null,[]);

//======
//(defn compare-and-set! "Atomically sets the value of atom to newval if and only if the\n  current value of the atom is identical to oldval. Returns true if\n  set happened, else false" [atom oldval newval] (.compareAndSet atom oldval newval))
//---
(function __clojure_core_fn_1662(){
return (clojure.JS.def(clojure.core,"compare_and_set_BANG_",(function __clojure_core_fn_1662_compare_and_set_BANG_1664(atom_1,oldval_2,newval_3){
return ((atom_1).compareAndSet(oldval_2,newval_3))})))}).apply(null,[]);

//======
//(defn reset! "Sets the value of atom to newval without regard for the\n  current value. Returns newval." [atom newval] (.reset atom newval))
//---
(function __clojure_core_fn_1668(){
return (clojure.JS.def(clojure.core,"reset_BANG_",(function __clojure_core_fn_1668_reset_BANG_1670(atom_1,newval_2){
return ((atom_1).reset(newval_2))})))}).apply(null,[]);

//======
//(defn set-validator! "Sets the validator-fn for a var/ref/agent/atom. validator-fn must be nil or a\n  side-effect-free fn of one argument, which will be passed the intended\n  new state on any state change. If the new state is unacceptable, the\n  validator-fn should return false or throw an exception. If the current state (root\n  value if var) is not acceptable to the new validator, an exception\n  will be thrown and the validator will not be changed." [iref validator-fn] (. iref (setValidator validator-fn)))
//---
(function __clojure_core_fn_1674(){
return (clojure.JS.def(clojure.core,"set_validator_BANG_",(function __clojure_core_fn_1674_set_validator_BANG_1676(iref_1,validator_fn_2){
return ((iref_1).setValidator(validator_fn_2))})))}).apply(null,[]);

//======
//(defn get-validator "Gets the validator-fn for a var/ref/agent/atom." [iref] (. iref (getValidator)))
//---
(function __clojure_core_fn_1680(){
return (clojure.JS.def(clojure.core,"get_validator",(function __clojure_core_fn_1680_get_validator_1682(iref_1){
return ((iref_1).getValidator())})))}).apply(null,[]);

//======
//(defn alter-meta! "Atomically sets the metadata for a namespace/var/ref/agent/atom to be: \n  \n  (apply f its-current-meta args) \n  \n  f must be free of side-effects" [iref f & args] (.alterMeta iref f args))
//---
(function __clojure_core_fn_1686(){
return (clojure.JS.def(clojure.core,"alter_meta_BANG_",clojure.JS.variadic(2,(function __clojure_core_fn_1686_alter_meta_BANG_1688(iref_1,f_2){
var args_3=clojure.JS.rest_args(this,arguments,2);
return ((iref_1).alterMeta(f_2,args_3))}))))}).apply(null,[]);

//======
//(defn reset-meta! "Atomically resets the metadata for a namespace/var/ref/agent/atom" [iref metadata-map] (.resetMeta iref metadata-map))
//---
(function __clojure_core_fn_1692(){
return (clojure.JS.def(clojure.core,"reset_meta_BANG_",(function __clojure_core_fn_1692_reset_meta_BANG_1694(iref_1,metadata_map_2){
return ((iref_1).resetMeta(metadata_map_2))})))}).apply(null,[]);

//======
//(defn commute "Must be called in a transaction. Sets the in-transaction-value of\n  ref to:\n\n  (apply fun in-transaction-value-of-ref args)\n\n  and returns the in-transaction-value of ref.\n\n  At the commit point of the transaction, sets the value of ref to be:\n\n  (apply fun most-recently-committed-value-of-ref args)\n\n  Thus fun should be commutative, or, failing that, you must accept\n  last-one-in-wins behavior.  commute allows for more concurrency than\n  ref-set." [ref fun & args] (. ref (commute fun args)))
//---
(function __clojure_core_fn_1698(){
return (clojure.JS.def(clojure.core,"commute",clojure.JS.variadic(2,(function __clojure_core_fn_1698_commute_1700(ref_1,fun_2){
var args_3=clojure.JS.rest_args(this,arguments,2);
return ((ref_1).commute(fun_2,args_3))}))))}).apply(null,[]);

//======
//(defn alter "Must be called in a transaction. Sets the in-transaction-value of\n  ref to:\n\n  (apply fun in-transaction-value-of-ref args)\n\n  and returns the in-transaction-value of ref." [ref fun & args] (. ref (alter fun args)))
//---
(function __clojure_core_fn_1704(){
return (clojure.JS.def(clojure.core,"alter",clojure.JS.variadic(2,(function __clojure_core_fn_1704_alter_1706(ref_1,fun_2){
var args_3=clojure.JS.rest_args(this,arguments,2);
return ((ref_1).alter(fun_2,args_3))}))))}).apply(null,[]);

//======
//(defn ref-set "Must be called in a transaction. Sets the value of ref.\n  Returns val." [ref val] (. ref (set val)))
//---
(function __clojure_core_fn_1710(){
return (clojure.JS.def(clojure.core,"ref_set",(function __clojure_core_fn_1710_ref_set_1712(ref_1,val_2){
return ((ref_1).set(val_2))})))}).apply(null,[]);

//======
//(defn ensure "Must be called in a transaction. Protects the ref from modification\n  by other transactions.  Returns the in-transaction-value of\n  ref. Allows for more concurrency than (ref-set ref @ref)" [ref] (. ref (touch)) (. ref (get)))
//---
(function __clojure_core_fn_1716(){
return (clojure.JS.def(clojure.core,"ensure",(function __clojure_core_fn_1716_ensure_1718(ref_1){
return ((ref_1).touch(),
(ref_1).get())})))}).apply(null,[]);
// Skipping: (defmacro sync "transaction-flags => TBD, pass nil for now\n\n  Runs the exprs (in an implicit do) in a transaction that encompasses\n  exprs and any nested calls.  Starts a transaction if none is already\n  running on this thread. Any uncaught exception will abort the\n  transaction and flow out of sync. The exprs may be run more than\n  once, but any effects on Refs will be atomic." [flags-ignored-for-now & body] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.LockingTransaction)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/runInTransaction)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/fn)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat))) body))))))
// Skipping: (defmacro io! "If an io! block occurs in a transaction, throws an\n  IllegalStateException, else runs body in an implicit do. If the\n  first expression in body is a literal string, will use that as the\n  exception message." [& body] (let [message (when (string? (first body)) (first body)) body (if message (rest body) body)] (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.lang.LockingTransaction/isRunning)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote throw)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote new)) (clojure.core/list (quote java.lang.IllegalStateException)) (clojure.core/list (or message "I/O in transaction")))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote do)) body)))))

//======
//(defn comp "Takes a set of functions and returns a fn that is the composition\n  of those fns.  The returned fn takes a variable number of args,\n  applies the rightmost of fns to the args, the next\n  fn (right-to-left) to the result, etc." [& fs] (let [fs (reverse fs)] (fn [& args] (loop [ret (apply (first fs) args) fs (rest fs)] (if fs (recur ((first fs) ret) (rest fs)) ret)))))
//---
(function __clojure_core_fn_1741(){
return (clojure.JS.def(clojure.core,"comp",clojure.JS.variadic(0,(function __clojure_core_fn_1741_comp_1743(){
var fs_2,fs_1=clojure.JS.rest_args(this,arguments,0);
return (((fs_2=clojure.core.reverse.apply(null,[fs_1])),
clojure.JS.variadic(0,(function __clojure_core_fn_1741_comp_1743_fn_1745(){
var ret_2,fs_3,args_1=clojure.JS.rest_args(this,arguments,0);
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.core.apply.apply(null,[clojure.core.first.apply(null,[fs_2]),args_1])),
(fs_3=clojure.core.rest.apply(null,[fs_2]));do{_cnt=0;
_rtn=((fs_3)?((_cnt=1,_rtn=[clojure.core.first.apply(null,[fs_3]).apply(null,[ret_2]),clojure.core.rest.apply(null,[fs_3])],ret_2=_rtn[0],fs_3=_rtn[1])):(ret_2))}while(_cnt);return _rtn;})()))}))))}))))}).apply(null,[]);

//======
//(defn partial "Takes a function f and fewer than the normal arguments to f, and\n  returns a fn that takes a variable number of additional args. When\n  called, the returned function calls f with args + additional args." ([f arg1] (fn [& args] (apply f arg1 args))) ([f arg1 arg2] (fn [& args] (apply f arg1 arg2 args))) ([f arg1 arg2 arg3] (fn [& args] (apply f arg1 arg2 arg3 args))) ([f arg1 arg2 arg3 & more] (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))
//---
(function __clojure_core_fn_1750(){
return (clojure.JS.def(clojure.core,"partial",clojure.JS.variadic(4,(function __clojure_core_fn_1750_partial_1752(f_1,arg1_2,arg2_3,arg3_4){switch(arguments.length){
case 2:return (clojure.JS.variadic(0,(function __clojure_core_fn_1750_partial_1752_fn_1754(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[f_1,arg1_2,args_1]))})))
case 3:return (clojure.JS.variadic(0,(function __clojure_core_fn_1750_partial_1752_fn_1758(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[f_1,arg1_2,arg2_3,args_1]))})))
case 4:return (clojure.JS.variadic(0,(function __clojure_core_fn_1750_partial_1752_fn_1762(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[f_1,arg1_2,arg2_3,arg3_4,args_1]))})))}
var more_5=clojure.JS.rest_args(this,arguments,4);
return (clojure.JS.variadic(0,(function __clojure_core_fn_1750_partial_1752_fn_1766(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[f_1,arg1_2,arg2_3,arg3_4,clojure.core.concat.apply(null,[more_5,args_1])]))})))}))))}).apply(null,[]);

//======
//(defn every? "Returns true if (pred x) is logical true for every x in coll, else\n  false." {:tag Boolean} [pred coll] (if (seq coll) (and (pred (first coll)) (recur pred (rest coll))) true))
//---
(function __clojure_core_fn_1771(){
return (clojure.JS.def(clojure.core,"every_QMARK_",(function __clojure_core_fn_1771_every_QMARK_1773(pred_1,coll_2){
var _cnt,_rtn,and__948__auto___3;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[coll_2]))?(((and__948__auto___3=pred_1.apply(null,[clojure.core.first.apply(null,[coll_2])])),
((and__948__auto___3)?((_cnt=1,_rtn=[pred_1,clojure.core.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])):(and__948__auto___3)))):(true))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def not-every? (comp not every?))
//---
(function __clojure_core_fn_1777(){
return (clojure.JS.def(clojure.core,"not_every_QMARK_",clojure.core.comp.apply(null,[clojure.core.not,clojure.core.every_QMARK_])))}).apply(null,[]);

//======
//(defn some "Returns the first logical true value of (pred x) for any x in coll,\n  else nil.  One common idiom is to use a set as pred, for example\n  this will return true if :fred is in the sequence, otherwise nil:\n  (some #{:fred} coll)" [pred coll] (when (seq coll) (or (pred (first coll)) (recur pred (rest coll)))))
//---
(function __clojure_core_fn_1780(){
return (clojure.JS.def(clojure.core,"some",(function __clojure_core_fn_1780_some_1782(pred_1,coll_2){
var _cnt,_rtn,or__962__auto___3;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[coll_2]))?(((or__962__auto___3=pred_1.apply(null,[clojure.core.first.apply(null,[coll_2])])),
((or__962__auto___3)?(or__962__auto___3):((_cnt=1,_rtn=[pred_1,clojure.core.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1]))))):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def not-any? (comp not some))
//---
(function __clojure_core_fn_1786(){
return (clojure.JS.def(clojure.core,"not_any_QMARK_",clojure.core.comp.apply(null,[clojure.core.not,clojure.core.some])))}).apply(null,[]);

//======
//(defn map "Returns a lazy seq consisting of the result of applying f to the\n  set of first items of each coll, followed by applying f to the set\n  of second items in each coll, until any one of the colls is\n  exhausted.  Any remaining items in other colls are ignored. Function\n  f should accept number-of-colls arguments." ([f coll] (when (seq coll) (lazy-cons (f (first coll)) (map f (rest coll))))) ([f c1 c2] (when (and (seq c1) (seq c2)) (lazy-cons (f (first c1) (first c2)) (map f (rest c1) (rest c2))))) ([f c1 c2 c3] (when (and (seq c1) (seq c2) (seq c3)) (lazy-cons (f (first c1) (first c2) (first c3)) (map f (rest c1) (rest c2) (rest c3))))) ([f c1 c2 c3 & colls] (let [step (fn step [cs] (when (every? seq cs) (lazy-cons (map first cs) (step (map rest cs)))))] (map (fn* [p1__1789] (apply f p1__1789)) (step (conj colls c3 c2 c1))))))
//---
(function __clojure_core_fn_1790(){
return (clojure.JS.def(clojure.core,"map",clojure.JS.variadic(4,(function __clojure_core_fn_1790_map_1792(f_1,c1_2,c2_3,c3_4){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (((clojure.core.seq.apply(null,[coll_2]))?((new clojure.lang.LazyCons((function __clojure_core_fn_1790_map_1792_fn_1795(G__1794_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.core.first.apply(null,[coll_2])]))}
return (clojure.core.map.apply(null,[f_1,clojure.core.rest.apply(null,[coll_2])]))})))):(null)))
case 3:var and__948__auto___4;
return (((((and__948__auto___4=clojure.core.seq.apply(null,[c1_2])),
((and__948__auto___4)?(clojure.core.seq.apply(null,[c2_3])):(and__948__auto___4))))?((new clojure.lang.LazyCons((function __clojure_core_fn_1790_map_1792_fn_1801(G__1800_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.core.first.apply(null,[c1_2]),clojure.core.first.apply(null,[c2_3])]))}
return (clojure.core.map.apply(null,[f_1,clojure.core.rest.apply(null,[c1_2]),clojure.core.rest.apply(null,[c2_3])]))})))):(null)))
case 4:var and__948__auto___5,and__948__auto___6;
return (((((and__948__auto___5=clojure.core.seq.apply(null,[c1_2])),
((and__948__auto___5)?(((and__948__auto___6=clojure.core.seq.apply(null,[c2_3])),
((and__948__auto___6)?(clojure.core.seq.apply(null,[c3_4])):(and__948__auto___6)))):(and__948__auto___5))))?((new clojure.lang.LazyCons((function __clojure_core_fn_1790_map_1792_fn_1807(G__1806_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.core.first.apply(null,[c1_2]),clojure.core.first.apply(null,[c2_3]),clojure.core.first.apply(null,[c3_4])]))}
return (clojure.core.map.apply(null,[f_1,clojure.core.rest.apply(null,[c1_2]),clojure.core.rest.apply(null,[c2_3]),clojure.core.rest.apply(null,[c3_4])]))})))):(null)))}
var step_6,colls_5=clojure.JS.rest_args(this,arguments,4);
return (((step_6=(function __clojure_core_fn_1790_map_1792_step_1812(cs_1){
var step_0=arguments.callee;
return (((clojure.core.every_QMARK_.apply(null,[clojure.core.seq,cs_1]))?((new clojure.lang.LazyCons((function __clojure_core_fn_1790_map_1792_step_1812_fn_1814(G__1813_1){switch(arguments.length){
case 0:return (clojure.core.map.apply(null,[clojure.core.first,cs_1]))}
return (step_0.apply(null,[clojure.core.map.apply(null,[clojure.core.rest,cs_1])]))})))):(null)))})),
clojure.core.map.apply(null,[(function __clojure_core_fn_1790_map_1792_fn_1819(p1__1789_1){
return (clojure.core.apply.apply(null,[f_1,p1__1789_1]))}),step_6.apply(null,[clojure.core.conj.apply(null,[colls_5,c3_4,c2_3,c1_2])])])))}))))}).apply(null,[]);

//======
//(defn mapcat "Returns the result of applying concat to the result of applying map\n  to f and colls.  Thus function f should return a collection." [f & colls] (apply concat (apply map f colls)))
//---
(function __clojure_core_fn_1824(){
return (clojure.JS.def(clojure.core,"mapcat",clojure.JS.variadic(1,(function __clojure_core_fn_1824_mapcat_1826(f_1){
var colls_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.core.apply.apply(null,[clojure.core.concat,clojure.core.apply.apply(null,[clojure.core.map,f_1,colls_2])]))}))))}).apply(null,[]);

//======
//(defn filter "Returns a lazy seq of the items in coll for which\n  (pred item) returns true. pred must be free of side-effects." [pred coll] (when (seq coll) (if (pred (first coll)) (lazy-cons (first coll) (filter pred (rest coll))) (recur pred (rest coll)))))
//---
(function __clojure_core_fn_1830(){
return (clojure.JS.def(clojure.core,"filter",(function __clojure_core_fn_1830_filter_1832(pred_1,coll_2){
var _cnt,_rtn;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[coll_2]))?(((pred_1.apply(null,[clojure.core.first.apply(null,[coll_2])]))?((new clojure.lang.LazyCons((function __clojure_core_fn_1830_filter_1832_fn_1835(G__1834_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll_2]))}
return (clojure.core.filter.apply(null,[pred_1,clojure.core.rest.apply(null,[coll_2])]))})))):((_cnt=1,_rtn=[pred_1,clojure.core.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])))):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn remove "Returns a lazy seq of the items in coll for which\n  (pred item) returns false. pred must be free of side-effects." [pred coll] (when (seq coll) (if (pred (first coll)) (recur pred (rest coll)) (lazy-cons (first coll) (remove pred (rest coll))))))
//---
(function __clojure_core_fn_1841(){
return (clojure.JS.def(clojure.core,"remove",(function __clojure_core_fn_1841_remove_1843(pred_1,coll_2){
var _cnt,_rtn;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[coll_2]))?(((pred_1.apply(null,[clojure.core.first.apply(null,[coll_2])]))?((_cnt=1,_rtn=[pred_1,clojure.core.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])):((new clojure.lang.LazyCons((function __clojure_core_fn_1841_remove_1843_fn_1846(G__1845_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll_2]))}
return (clojure.core.remove.apply(null,[pred_1,clojure.core.rest.apply(null,[coll_2])]))})))))):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn take "Returns a lazy seq of the first n items in coll, or all items if\n  there are fewer than n." [n coll] (when (and (pos? n) (seq coll)) (lazy-cons (first coll) (when (> n 1) (take (dec n) (rest coll))))))
//---
(function __clojure_core_fn_1852(){
return (clojure.JS.def(clojure.core,"take",(function __clojure_core_fn_1852_take_1854(n_1,coll_2){
var and__948__auto___3;
return (((((and__948__auto___3=clojure.lang.Numbers.isPos(n_1)),
((and__948__auto___3)?(clojure.core.seq.apply(null,[coll_2])):(and__948__auto___3))))?((new clojure.lang.LazyCons((function __clojure_core_fn_1852_take_1854_fn_1857(G__1856_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll_2]))}
return (((clojure.lang.Numbers.gt(n_1,(1)))?(clojure.core.take.apply(null,[clojure.lang.Numbers.dec(n_1),clojure.core.rest.apply(null,[coll_2])])):(null)))})))):(null)))})))}).apply(null,[]);

//======
//(defn take-while "Returns a lazy seq of successive items from coll while\n  (pred item) returns true. pred must be free of side-effects." [pred coll] (when (and (seq coll) (pred (first coll))) (lazy-cons (first coll) (take-while pred (rest coll)))))
//---
(function __clojure_core_fn_1863(){
return (clojure.JS.def(clojure.core,"take_while",(function __clojure_core_fn_1863_take_while_1865(pred_1,coll_2){
var and__948__auto___3;
return (((((and__948__auto___3=clojure.core.seq.apply(null,[coll_2])),
((and__948__auto___3)?(pred_1.apply(null,[clojure.core.first.apply(null,[coll_2])])):(and__948__auto___3))))?((new clojure.lang.LazyCons((function __clojure_core_fn_1863_take_while_1865_fn_1868(G__1867_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll_2]))}
return (clojure.core.take_while.apply(null,[pred_1,clojure.core.rest.apply(null,[coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn drop "Returns a lazy seq of all but the first n items in coll." [n coll] (if (and (pos? n) (seq coll)) (recur (dec n) (rest coll)) (seq coll)))
//---
(function __clojure_core_fn_1874(){
return (clojure.JS.def(clojure.core,"drop",(function __clojure_core_fn_1874_drop_1876(n_1,coll_2){
var _cnt,_rtn,and__948__auto___3;
do{_cnt=0;_rtn=((((and__948__auto___3=clojure.lang.Numbers.isPos(n_1)),
((and__948__auto___3)?(clojure.core.seq.apply(null,[coll_2])):(and__948__auto___3))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_1),clojure.core.rest.apply(null,[coll_2])],n_1=_rtn[0],coll_2=_rtn[1])):(clojure.core.seq.apply(null,[coll_2])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn drop-last "Return a lazy seq of all but the last n (default 1) items in coll" ([s] (drop-last 1 s)) ([n s] (map (fn [x _] x) (seq s) (drop n s))))
//---
(function __clojure_core_fn_1880(){
return (clojure.JS.def(clojure.core,"drop_last",(function __clojure_core_fn_1880_drop_last_1882(n_1,s_2){switch(arguments.length){
case 1:var s_1=arguments[0];
return (clojure.core.drop_last.apply(null,[(1),s_1]))}
return (clojure.core.map.apply(null,[(function __clojure_core_fn_1880_drop_last_1882_fn_1885(x_1,__2){
return (x_1)}),clojure.core.seq.apply(null,[s_2]),clojure.core.drop.apply(null,[n_1,s_2])]))})))}).apply(null,[]);

//======
//(defn drop-while "Returns a lazy seq of the items in coll starting from the first\n  item for which (pred item) returns nil." [pred coll] (if (and (seq coll) (pred (first coll))) (recur pred (rest coll)) (seq coll)))
//---
(function __clojure_core_fn_1890(){
return (clojure.JS.def(clojure.core,"drop_while",(function __clojure_core_fn_1890_drop_while_1892(pred_1,coll_2){
var _cnt,_rtn,and__948__auto___3;
do{_cnt=0;_rtn=((((and__948__auto___3=clojure.core.seq.apply(null,[coll_2])),
((and__948__auto___3)?(pred_1.apply(null,[clojure.core.first.apply(null,[coll_2])])):(and__948__auto___3))))?((_cnt=1,_rtn=[pred_1,clojure.core.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])):(clojure.core.seq.apply(null,[coll_2])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn cycle "Returns a lazy (infinite!) seq of repetitions of the items in\n  coll." [coll] (when (seq coll) (let [rep (fn thisfn [xs] (if xs (lazy-cons (first xs) (thisfn (rest xs))) (recur (seq coll))))] (rep (seq coll)))))
//---
(function __clojure_core_fn_1896(){
return (clojure.JS.def(clojure.core,"cycle",(function __clojure_core_fn_1896_cycle_1898(coll_1){
var rep_2;
return (((clojure.core.seq.apply(null,[coll_1]))?(((rep_2=(function __clojure_core_fn_1896_cycle_1898_thisfn_1900(xs_1){
var _cnt,_rtn,thisfn_0=arguments.callee;
do{_cnt=0;_rtn=((xs_1)?((new clojure.lang.LazyCons((function __clojure_core_fn_1896_cycle_1898_thisfn_1900_fn_1902(G__1901_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[xs_1]))}
return (thisfn_0.apply(null,[clojure.core.rest.apply(null,[xs_1])]))})))):((_cnt=1,_rtn=[clojure.core.seq.apply(null,[coll_1])],xs_1=_rtn[0])))
}while(_cnt);return _rtn;})),
rep_2.apply(null,[clojure.core.seq.apply(null,[coll_1])]))):(null)))})))}).apply(null,[]);

//======
//(defn split-at "Returns a vector of [(take n coll) (drop n coll)]" [n coll] [(take n coll) (drop n coll)])
//---
(function __clojure_core_fn_1909(){
return (clojure.JS.def(clojure.core,"split_at",(function __clojure_core_fn_1909_split_at_1911(n_1,coll_2){
return (clojure.JS.lit_vector([clojure.core.take.apply(null,[n_1,coll_2]),clojure.core.drop.apply(null,[n_1,coll_2])]))})))}).apply(null,[]);

//======
//(defn split-with "Returns a vector of [(take-while pred coll) (drop-while pred coll)]" [pred coll] [(take-while pred coll) (drop-while pred coll)])
//---
(function __clojure_core_fn_1915(){
return (clojure.JS.def(clojure.core,"split_with",(function __clojure_core_fn_1915_split_with_1917(pred_1,coll_2){
return (clojure.JS.lit_vector([clojure.core.take_while.apply(null,[pred_1,coll_2]),clojure.core.drop_while.apply(null,[pred_1,coll_2])]))})))}).apply(null,[]);

//======
//(defn repeat "Returns a lazy (infinite!) seq of xs." [x] (lazy-cons x (repeat x)))
//---
(function __clojure_core_fn_1921(){
return (clojure.JS.def(clojure.core,"repeat",(function __clojure_core_fn_1921_repeat_1923(x_1){
return ((new clojure.lang.LazyCons((function __clojure_core_fn_1921_repeat_1923_fn_1926(G__1925_1){switch(arguments.length){
case 0:return (x_1)}
return (clojure.core.repeat.apply(null,[x_1]))}))))})))}).apply(null,[]);

//======
//(defn replicate "Returns a lazy seq of n xs." [n x] (take n (repeat x)))
//---
(function __clojure_core_fn_1932(){
return (clojure.JS.def(clojure.core,"replicate",(function __clojure_core_fn_1932_replicate_1934(n_1,x_2){
return (clojure.core.take.apply(null,[n_1,clojure.core.repeat.apply(null,[x_2])]))})))}).apply(null,[]);

//======
//(defn iterate "Returns a lazy seq of x, (f x), (f (f x)) etc. f must be free of side-effects" [f x] (lazy-cons x (iterate f (f x))))
//---
(function __clojure_core_fn_1938(){
return (clojure.JS.def(clojure.core,"iterate",(function __clojure_core_fn_1938_iterate_1940(f_1,x_2){
return ((new clojure.lang.LazyCons((function __clojure_core_fn_1938_iterate_1940_fn_1943(G__1942_1){switch(arguments.length){
case 0:return (x_2)}
return (clojure.core.iterate.apply(null,[f_1,f_1.apply(null,[x_2])]))}))))})))}).apply(null,[]);

//======
//(defn range "Returns a lazy seq of nums from start (inclusive) to end\n  (exclusive), by step, where start defaults to 0 and step to 1." ([end] (if (and (> end 0) (<= end clojure.lang.RT/IntegerMaxValue)) (new clojure.lang.Range 0 end) (take end (iterate inc 0)))) ([start end] (if (and (< start end) (>= start clojure.lang.RT/IntegerMinValue) (<= end clojure.lang.RT/IntegerMaxValue)) (new clojure.lang.Range start end) (take (- end start) (iterate inc start)))) ([start end step] (take-while (partial (if (pos? step) > <) end) (iterate (partial + step) start))))
//---
(function __clojure_core_fn_1949(){
return (clojure.JS.def(clojure.core,"range",(function __clojure_core_fn_1949_range_1951(start_1,end_2,step_3){switch(arguments.length){
case 1:var and__948__auto___2,end_1=arguments[0];
return (((((and__948__auto___2=clojure.lang.Numbers.gt(end_1,(0))),
((and__948__auto___2)?(clojure.lang.Numbers.lte(end_1,clojure.JS.getOrRun(clojure.lang.RT,"IntegerMaxValue"))):(and__948__auto___2))))?((new clojure.lang.Range((0),end_1))):(clojure.core.take.apply(null,[end_1,clojure.core.iterate.apply(null,[clojure.core.inc,(0)])]))))
case 2:var and__948__auto___3,and__948__auto___4;
return (((((and__948__auto___3=clojure.lang.Numbers.lt(start_1,end_2)),
((and__948__auto___3)?(((and__948__auto___4=clojure.lang.Numbers.gte(start_1,clojure.JS.getOrRun(clojure.lang.RT,"IntegerMinValue"))),
((and__948__auto___4)?(clojure.lang.Numbers.lte(end_2,clojure.JS.getOrRun(clojure.lang.RT,"IntegerMaxValue"))):(and__948__auto___4)))):(and__948__auto___3))))?((new clojure.lang.Range(start_1,end_2))):(clojure.core.take.apply(null,[clojure.lang.Numbers.minus(end_2,start_1),clojure.core.iterate.apply(null,[clojure.core.inc,start_1])]))))}
return (clojure.core.take_while.apply(null,[clojure.core.partial.apply(null,[((clojure.lang.Numbers.isPos(step_3))?(clojure.core._GT_):(clojure.core._LT_)),end_2]),clojure.core.iterate.apply(null,[clojure.core.partial.apply(null,[clojure.core._PLUS_,step_3]),start_1])]))})))}).apply(null,[]);

//======
//(defn merge "Returns a map that consists of the rest of the maps conj-ed onto\n  the first.  If a key occurs in more than one map, the mapping from\n  the latter (left-to-right) will be the mapping in the result." [& maps] (when (some identity maps) (reduce (fn* [p1__1957 p2__1958] (conj (or p1__1957 {}) p2__1958)) maps)))
//---
(function __clojure_core_fn_1959(){
return (clojure.JS.def(clojure.core,"merge",clojure.JS.variadic(0,(function __clojure_core_fn_1959_merge_1961(){
var maps_1=clojure.JS.rest_args(this,arguments,0);
return (((clojure.core.some.apply(null,[clojure.core.identity,maps_1]))?(clojure.core.reduce.apply(null,[(function __clojure_core_fn_1959_merge_1961_fn_1963(p1__1957_1,p2__1958_2){
var or__962__auto___3;
return (clojure.core.conj.apply(null,[((or__962__auto___3=p1__1957_1),
((or__962__auto___3)?(or__962__auto___3):(clojure.lang.PersistentArrayMap.EMPTY))),p2__1958_2]))}),maps_1])):(null)))}))))}).apply(null,[]);

//======
//(defn merge-with "Returns a map that consists of the rest of the maps conj-ed onto\n  the first.  If a key occurs in more than one map, the mapping(s)\n  from the latter (left-to-right) will be combined with the mapping in\n  the result by calling (f val-in-result val-in-latter)." [f & maps] (when (some identity maps) (let [merge-entry (fn [m e] (let [k (key e) v (val e)] (if (contains? m k) (assoc m k (f (m k) v)) (assoc m k v)))) merge2 (fn [m1 m2] (reduce merge-entry (or m1 {}) (seq m2)))] (reduce merge2 maps))))
//---
(function __clojure_core_fn_1968(){
return (clojure.JS.def(clojure.core,"merge_with",clojure.JS.variadic(1,(function __clojure_core_fn_1968_merge_with_1970(f_1){
var merge_entry_3,merge2_4,maps_2=clojure.JS.rest_args(this,arguments,1);
return (((clojure.core.some.apply(null,[clojure.core.identity,maps_2]))?(((merge_entry_3=(function __clojure_core_fn_1968_merge_with_1970_merge_entry_1972(m_1,e_2){
var k_3,v_4;
return (((k_3=clojure.core.key.apply(null,[e_2])),
(v_4=clojure.core.val.apply(null,[e_2])),
((clojure.core.contains_QMARK_.apply(null,[m_1,k_3]))?(clojure.core.assoc.apply(null,[m_1,k_3,f_1.apply(null,[m_1.apply(null,[k_3]),v_4])])):(clojure.core.assoc.apply(null,[m_1,k_3,v_4])))))})),
(merge2_4=(function __clojure_core_fn_1968_merge_with_1970_merge2_1975(m1_1,m2_2){
var or__962__auto___3;
return (clojure.core.reduce.apply(null,[merge_entry_3,((or__962__auto___3=m1_1),
((or__962__auto___3)?(or__962__auto___3):(clojure.lang.PersistentArrayMap.EMPTY))),clojure.core.seq.apply(null,[m2_2])]))})),
clojure.core.reduce.apply(null,[merge2_4,maps_2]))):(null)))}))))}).apply(null,[]);

//======
//(defn zipmap "Returns a map with the keys mapped to the corresponding vals." [keys vals] (loop [map {} ks (seq keys) vs (seq vals)] (if (and ks vs) (recur (assoc map (first ks) (first vs)) (rest ks) (rest vs)) map)))
//---
(function __clojure_core_fn_1980(){
return (clojure.JS.def(clojure.core,"zipmap",(function __clojure_core_fn_1980_zipmap_1982(keys_1,vals_2){
var map_3,ks_4,vs_5,and__948__auto___6;
return (((function __loop(){var _rtn,_cnt;(map_3=clojure.lang.PersistentArrayMap.EMPTY),
(ks_4=clojure.core.seq.apply(null,[keys_1])),
(vs_5=clojure.core.seq.apply(null,[vals_2]));do{_cnt=0;
_rtn=((((and__948__auto___6=ks_4),
((and__948__auto___6)?(vs_5):(and__948__auto___6))))?((_cnt=1,_rtn=[clojure.core.assoc.apply(null,[map_3,clojure.core.first.apply(null,[ks_4]),clojure.core.first.apply(null,[vs_5])]),clojure.core.rest.apply(null,[ks_4]),clojure.core.rest.apply(null,[vs_5])],map_3=_rtn[0],ks_4=_rtn[1],vs_5=_rtn[2])):(map_3))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn line-seq "Returns the lines of text from rdr as a lazy sequence of strings.\n  rdr must implement java.io.BufferedReader." [rdr] (let [line (. rdr (readLine))] (when line (lazy-cons line (line-seq rdr)))))
//---
(function __clojure_core_fn_1986(){
return (clojure.JS.def(clojure.core,"line_seq",(function __clojure_core_fn_1986_line_seq_1988(rdr_1){
var line_2;
return (((line_2=(rdr_1).readLine()),
((line_2)?((new clojure.lang.LazyCons((function __clojure_core_fn_1986_line_seq_1988_fn_1991(G__1990_1){switch(arguments.length){
case 0:return (line_2)}
return (clojure.core.line_seq.apply(null,[rdr_1]))})))):(null))))})))}).apply(null,[]);

//======
//(defn comparator "Returns an implementation of java.util.Comparator based upon pred." [pred] (fn [x y] (cond (pred x y) -1 (pred y x) 1 :else 0)))
//---
(function __clojure_core_fn_1997(){
return (clojure.JS.def(clojure.core,"comparator",(function __clojure_core_fn_1997_comparator_1999(pred_1){
return ((function __clojure_core_fn_1997_comparator_1999_fn_2001(x_1,y_2){
return (((pred_1.apply(null,[x_1,y_2]))?((-1)):(((pred_1.apply(null,[y_2,x_1]))?((1)):(((clojure.core.keyword("","else"))?((0)):(null)))))))}))})))}).apply(null,[]);

//======
//(defn sort "Returns a sorted sequence of the items in coll. If no comparator is\n  supplied, uses compare. comparator must\n  implement java.util.Comparator." ([coll] (sort compare coll)) ([comp coll] (when (and coll (not (zero? (count coll)))) (let [a (to-array coll)] (clojure.lang.RT/sortArray a comp) (seq a)))))
//---
(function __clojure_core_fn_2006(){
return (clojure.JS.def(clojure.core,"sort",(function __clojure_core_fn_2006_sort_2008(comp_1,coll_2){switch(arguments.length){
case 1:var coll_1=arguments[0];
return (clojure.core.sort.apply(null,[clojure.core.compare,coll_1]))}
var and__948__auto___3,a_3;
return (((((and__948__auto___3=coll_2),
((and__948__auto___3)?(clojure.core.not.apply(null,[clojure.lang.Numbers.isZero(clojure.core.count.apply(null,[coll_2]))])):(and__948__auto___3))))?(((a_3=clojure.core.to_array.apply(null,[coll_2])),
clojure.lang.RT.sortArray(a_3,comp_1),
clojure.core.seq.apply(null,[a_3]))):(null)))})))}).apply(null,[]);

//======
//(defn sort-by "Returns a sorted sequence of the items in coll, where the sort\n  order is determined by comparing (keyfn item).  If no comparator is\n  supplied, uses compare. comparator must\n  implement java.util.Comparator." ([keyfn coll] (sort-by keyfn compare coll)) ([keyfn comp coll] (sort (fn [x y] (. comp (compare (keyfn x) (keyfn y)))) coll)))
//---
(function __clojure_core_fn_2013(){
return (clojure.JS.def(clojure.core,"sort_by",(function __clojure_core_fn_2013_sort_by_2015(keyfn_1,comp_2,coll_3){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (clojure.core.sort_by.apply(null,[keyfn_1,clojure.core.compare,coll_2]))}
return (clojure.core.sort.apply(null,[(function __clojure_core_fn_2013_sort_by_2015_fn_2018(x_1,y_2){
return ((comp_2).compare(keyfn_1.apply(null,[x_1]),keyfn_1.apply(null,[y_2])))}),coll_3]))})))}).apply(null,[]);

//======
//(defn partition "Returns a lazy sequence of lists of n items each, at offsets step\n  apart. If step is not supplied, defaults to n, i.e. the partitions\n  do not overlap." ([n coll] (partition n n coll)) ([n step coll] (when (seq coll) (let [p (take n coll)] (when (= n (count p)) (lazy-cons p (partition n step (drop step coll))))))))
//---
(function __clojure_core_fn_2023(){
return (clojure.JS.def(clojure.core,"partition",(function __clojure_core_fn_2023_partition_2025(n_1,step_2,coll_3){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (clojure.core.partition.apply(null,[n_1,n_1,coll_2]))}
var p_4;
return (((clojure.core.seq.apply(null,[coll_3]))?(((p_4=clojure.core.take.apply(null,[n_1,coll_3])),
((clojure.lang.Util.equiv(n_1,clojure.core.count.apply(null,[p_4])))?((new clojure.lang.LazyCons((function __clojure_core_fn_2023_partition_2025_fn_2029(G__2028_1){switch(arguments.length){
case 0:return (p_4)}
return (clojure.core.partition.apply(null,[n_1,step_2,clojure.core.drop.apply(null,[step_2,coll_3])]))})))):(null)))):(null)))})))}).apply(null,[]);
// Skipping: (defn eval "Evaluates the form data structure (not text!) and returns the result." [form] (. clojure.lang.Compiler (eval form)))
// Skipping: (defmacro doseq "Repeatedly executes body (presumably for side-effects) with\n  bindings and filtering as provided by \"for\".  Does not retain\n  the head of the sequence. Returns nil." [seq-exprs & body] (assert-args doseq (vector? seq-exprs) "a vector for its binding" (even? (count seq-exprs)) "an even number of forms in binding vector") (let [groups (reduce (fn [groups p] (if (keyword? (first p)) (conj (pop groups) (apply assoc (peek groups) p)) (conj groups {:bind (first p), :seq (second p)}))) [] (partition 2 seq-exprs)) emit (fn emit [group & more-groups] (clojure.core/concat (clojure.core/list (quote clojure.core/loop)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote sq__2041__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/seq)) (clojure.core/list (:seq group))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (quote sq__2041__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (:bind group)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/first)) (clojure.core/list (quote sq__2041__auto__))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (or (:while group) true)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (or (:when group) true)) (clojure.core/list (if more-groups (apply emit more-groups) (clojure.core/concat (clojure.core/list (quote do)) body))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote recur)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/rest)) (clojure.core/list (quote sq__2041__auto__))))))))))))))] (apply emit groups)))

//======
//(defn dorun "When lazy sequences are produced via functions that have side\n  effects, any effects other than those needed to produce the first\n  element in the seq do not occur until the seq is consumed. dorun can\n  be used to force any effects. Walks through the successive rests of\n  the seq, does not retain the head and returns nil." ([coll] (when (and (seq coll) (or (first coll) true)) (recur (rest coll)))) ([n coll] (when (and (seq coll) (pos? n) (or (first coll) true)) (recur (dec n) (rest coll)))))
//---
(function __clojure_core_fn_2063(){
return (clojure.JS.def(clojure.core,"dorun",(function __clojure_core_fn_2063_dorun_2065(n_1,coll_2){switch(arguments.length){
case 1:var _cnt,_rtn,and__948__auto___2,or__962__auto___3,coll_1=arguments[0];
do{_cnt=0;_rtn=((((and__948__auto___2=clojure.core.seq.apply(null,[coll_1])),
((and__948__auto___2)?(((or__962__auto___3=clojure.core.first.apply(null,[coll_1])),
((or__962__auto___3)?(or__962__auto___3):(true)))):(and__948__auto___2))))?((_cnt=1,_rtn=[clojure.core.rest.apply(null,[coll_1])],coll_1=_rtn[0])):(null))
}while(_cnt);return _rtn;}
var _cnt,_rtn,and__948__auto___3,and__948__auto___4,or__962__auto___5;
do{_cnt=0;_rtn=((((and__948__auto___3=clojure.core.seq.apply(null,[coll_2])),
((and__948__auto___3)?(((and__948__auto___4=clojure.lang.Numbers.isPos(n_1)),
((and__948__auto___4)?(((or__962__auto___5=clojure.core.first.apply(null,[coll_2])),
((or__962__auto___5)?(or__962__auto___5):(true)))):(and__948__auto___4)))):(and__948__auto___3))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_1),clojure.core.rest.apply(null,[coll_2])],n_1=_rtn[0],coll_2=_rtn[1])):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn doall "When lazy sequences are produced via functions that have side\n  effects, any effects other than those needed to produce the first\n  element in the seq do not occur until the seq is consumed. doall can\n  be used to force any effects. Walks through the successive rests of\n  the seq, retains the head and returns it, thus causing the entire\n  seq to reside in memory at one time." ([coll] (dorun coll) coll) ([n coll] (dorun n coll) coll))
//---
(function __clojure_core_fn_2070(){
return (clojure.JS.def(clojure.core,"doall",(function __clojure_core_fn_2070_doall_2072(n_1,coll_2){switch(arguments.length){
case 1:var coll_1=arguments[0];
return (clojure.core.dorun.apply(null,[coll_1]),
coll_1)}
return (clojure.core.dorun.apply(null,[n_1,coll_2]),
coll_2)})))}).apply(null,[]);
// Skipping: (defn await "Blocks the current thread (indefinitely!) until all actions\n  dispatched thus far, from this thread or agent, to the agent(s) have\n  occurred." [& agents] (io! "await in transaction" (when *agent* (throw (clojure.lang.RT/makeException "Can't await in agent action"))) (let [latch (new java.util.concurrent.CountDownLatch (count agents)) count-down (fn [agent] (. latch (countDown)) agent)] (doseq [agent agents] (send agent count-down)) (. latch (await)))))

//======
//(defn await1 [a] (when (pos? (.getQueueCount a)) (await a)) a)
//---
(function __clojure_core_fn_2086(){
return (clojure.JS.def(clojure.core,"await1",(function __clojure_core_fn_2086_await1_2088(a_1){
return (((clojure.lang.Numbers.isPos((a_1).getQueueCount()))?(clojure.core.await.apply(null,[a_1])):(null)),
a_1)})))}).apply(null,[]);
// Skipping: (defn await-for "Blocks the current thread until all actions dispatched thus\n  far (from this thread or agent) to the agents have occurred, or the\n  timeout (in milliseconds) has elapsed. Returns nil if returning due\n  to timeout, non-nil otherwise." [timeout-ms & agents] (io! "await-for in transaction" (when *agent* (throw (clojure.lang.RT/makeException "Can't await in agent action"))) (let [latch (new java.util.concurrent.CountDownLatch (count agents)) count-down (fn [agent] (. latch (countDown)) agent)] (doseq [agent agents] (send agent count-down)) (. latch (await timeout-ms (. java.util.concurrent.TimeUnit MILLISECONDS))))))
// Skipping: (defmacro dotimes "bindings => name n\n\n  Repeatedly executes body (presumably for side-effects) with name\n  bound to integers from 0 through n-1." [bindings & body] (assert-args dotimes (vector? bindings) "a vector for its binding" (= 2 (count bindings)) "exactly 2 forms in binding vector") (let [i (first bindings) n (second bindings)] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote n__2101__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/int)) (clojure.core/list n)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/loop)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list i) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/int)) (clojure.core/list 0)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/<)) (clojure.core/list i) (clojure.core/list (quote n__2101__auto__)))) body (clojure.core/list (clojure.core/concat (clojure.core/list (quote recur)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked-inc)) (clojure.core/list i))))))))))))
// Skipping: (defn import "import-list => (package-symbol class-name-symbols*)\n\n  For each name in class-name-symbols, adds a mapping from name to the\n  class named by package.name to the current namespace. Use :import in the ns \n  macro in preference to calling this directly." [& import-symbols-or-lists] (let [ns *ns*] (doseq [spec import-symbols-or-lists] (if (symbol? spec) (let [n (name spec) dot (.lastIndexOf n (. clojure.lang.RT (intCast \.))) c (symbol (.substring n (inc dot)))] (. ns (importClass c (. clojure.lang.RT (classForName (name spec)))))) (let [pkg (first spec) classes (rest spec)] (doseq [c classes] (. ns (importClass c (. clojure.lang.RT (classForName (str pkg "." c)))))))))))

//======
//(defn into-array "Returns an array with components set to the values in aseq. The array's\n  component type is type if provided, or the type of the first value in\n  aseq if present, or Object. All values in aseq must be compatible with\n  the component type. Class objects for the primitive types can be obtained\n  using, e.g., Integer/TYPE." ([aseq] (clojure.lang.RT/seqToTypedArray (seq aseq))) ([type aseq] (clojure.lang.RT/seqToTypedArray type (seq aseq))))
//---
(function __clojure_core_fn_2117(){
return (clojure.JS.def(clojure.core,"into_array",(function __clojure_core_fn_2117_into_array_2119(type_1,aseq_2){switch(arguments.length){
case 1:var aseq_1=arguments[0];
return (clojure.lang.RT.seqToTypedArray(clojure.core.seq.apply(null,[aseq_1])))}
return (clojure.lang.RT.seqToTypedArray(type_1,clojure.core.seq.apply(null,[aseq_2])))})))}).apply(null,[]);

//======
//(defn into "Returns a new coll consisting of to-coll with all of the items of\n  from-coll conjoined." [to from] (let [ret to items (seq from)] (if items (recur (conj ret (first items)) (rest items)) ret)))
//---
(function __clojure_core_fn_2124(){
return (clojure.JS.def(clojure.core,"into",(function __clojure_core_fn_2124_into_2126(to_1,from_2){
var _cnt,_rtn,ret_3,items_4;
do{_cnt=0;_rtn=((ret_3=to_1),
(items_4=clojure.core.seq.apply(null,[from_2])),
((items_4)?((_cnt=1,_rtn=[clojure.core.conj.apply(null,[ret_3,clojure.core.first.apply(null,[items_4])]),clojure.core.rest.apply(null,[items_4])],to_1=_rtn[0],from_2=_rtn[1])):(ret_3)))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn array [& items] (into-array items))
//---
(function __clojure_core_fn_2130(){
return (clojure.JS.def(clojure.core,"array",clojure.JS.variadic(0,(function __clojure_core_fn_2130_array_2132(){
var items_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.into_array.apply(null,[items_1]))}))))}).apply(null,[]);
// Skipping: (defn class "Returns the Class of x" [x] (if (nil? x) x (. x (getClass))))

//======
//(defn num "Coerce to Number" {:tag Number, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/num)) (clojure.core/list x)))))} [x] (. clojure.lang.Numbers (num x)))
//---
(function __clojure_core_fn_2142(){
return (clojure.JS.def(clojure.core,"num",(function __clojure_core_fn_2142_num_2147(x_1){
return (clojure.lang.Numbers.num(x_1))})))}).apply(null,[]);

//======
//(defn int "Coerce to int" {:tag Integer, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/intCast)) (clojure.core/list x)))))} [x] (. clojure.lang.RT (intCast x)))
//---
(function __clojure_core_fn_2151(){
return (clojure.JS.def(clojure.core,"int",(function __clojure_core_fn_2151_int_2156(x_1){
return (clojure.lang.RT.intCast(x_1))})))}).apply(null,[]);

//======
//(defn long "Coerce to long" {:tag Long, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/longCast)) (clojure.core/list x)))))} [x] (. x (longValue)))
//---
(function __clojure_core_fn_2160(){
return (clojure.JS.def(clojure.core,"long",(function __clojure_core_fn_2160_long_2165(x_1){
return ((x_1).longValue())})))}).apply(null,[]);

//======
//(defn float "Coerce to float" {:tag Float, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/floatCast)) (clojure.core/list x)))))} [x] (. x (floatValue)))
//---
(function __clojure_core_fn_2169(){
return (clojure.JS.def(clojure.core,"float",(function __clojure_core_fn_2169_float_2174(x_1){
return ((x_1).floatValue())})))}).apply(null,[]);

//======
//(defn double "Coerce to double" {:tag Double, :inline (fn [x] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/doubleCast)) (clojure.core/list x)))))} [x] (. x (doubleValue)))
//---
(function __clojure_core_fn_2178(){
return (clojure.JS.def(clojure.core,"double",(function __clojure_core_fn_2178_double_2183(x_1){
return ((x_1).doubleValue())})))}).apply(null,[]);

//======
//(defn short "Coerce to short" {:tag Short} [x] (. x (shortValue)))
//---
(function __clojure_core_fn_2187(){
return (clojure.JS.def(clojure.core,"short_",(function __clojure_core_fn_2187_short_2189(x_1){
return ((x_1).shortValue())})))}).apply(null,[]);

//======
//(defn byte "Coerce to byte" {:tag Byte} [x] (. x (byteValue)))
//---
(function __clojure_core_fn_2193(){
return (clojure.JS.def(clojure.core,"byte_",(function __clojure_core_fn_2193_byte_2195(x_1){
return ((x_1).byteValue())})))}).apply(null,[]);

//======
//(defn char "Coerce to char" {:tag Character} [x] (. clojure.lang.RT (charCast x)))
//---
(function __clojure_core_fn_2199(){
return (clojure.JS.def(clojure.core,"char_",(function __clojure_core_fn_2199_char_2201(x_1){
return (clojure.lang.RT.charCast(x_1))})))}).apply(null,[]);

//======
//(defn boolean "Coerce to boolean" {:tag Boolean} [x] (if x true false))
//---
(function __clojure_core_fn_2205(){
return (clojure.JS.def(clojure.core,"boolean_",(function __clojure_core_fn_2205_boolean_2207(x_1){
return (((x_1)?(true):(false)))})))}).apply(null,[]);
// Skipping: (defn number? "Returns true if x is a Number" [x] (instance? Number x))
// Skipping: (defn integer? "Returns true if n is an integer" [n] (or (instance? Integer n) (instance? Long n) (instance? BigInteger n) (instance? Short n) (instance? Byte n)))

//======
//(defn ratio? "Returns true if n is a Ratio" [n] (instance? clojure.lang.Ratio n))
//---
(function __clojure_core_fn_2223(){
return (clojure.JS.def(clojure.core,"ratio_QMARK_",(function __clojure_core_fn_2223_ratio_QMARK_2225(n_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Ratio,n_1]))})))}).apply(null,[]);
// Skipping: (defn decimal? "Returns true if n is a BigDecimal" [n] (instance? BigDecimal n))
// Skipping: (defn float? "Returns true if n is a floating point number" [n] (or (instance? Double n) (instance? Float n)))

//======
//(defn rational? [n] "Returns true if n is a rational number" (or (integer? n) (ratio? n) (decimal? n)))
//---
(function __clojure_core_fn_2241(){
return (clojure.JS.def(clojure.core,"rational_QMARK_",(function __clojure_core_fn_2241_rational_QMARK_2243(n_1){
var or__962__auto___2,or__962__auto___3;
return ("Returns true if n is a rational number",
((or__962__auto___2=clojure.core.integer_QMARK_.apply(null,[n_1])),
((or__962__auto___2)?(or__962__auto___2):(((or__962__auto___3=clojure.core.ratio_QMARK_.apply(null,[n_1])),
((or__962__auto___3)?(or__962__auto___3):(clojure.core.decimal_QMARK_.apply(null,[n_1]))))))))})))}).apply(null,[]);
// Skipping: (defn bigint "Coerce to BigInteger" {:tag BigInteger} [x] (cond (instance? BigInteger x) x (decimal? x) (.toBigInteger x) (number? x) (BigInteger/valueOf (long x)) :else (BigInteger. x)))
// Skipping: (defn bigdec "Coerce to BigDecimal" {:tag BigDecimal} [x] (cond (decimal? x) x (float? x) (. BigDecimal valueOf (double x)) (instance? BigInteger x) (BigDecimal. x) (number? x) (BigDecimal/valueOf (long x)) :else (BigDecimal. x)))

//======
//(def print-initialized false)
//---
(function __clojure_core_fn_2259(){
return (clojure.JS.def(clojure.core,"print_initialized",false))}).apply(null,[]);
// Skipping: (defmulti print-method (fn [x writer] (class x)))

//======
//(defmulti print-dup (fn [x writer] (class x)))
//---
(function __clojure_core_fn_2268(){
return (clojure.JS.def(clojure.core,"print_dup",(new clojure.lang.MultiFn((function __clojure_core_fn_2268_fn_2270(x_1,writer_2){
return (clojure.core.class_.apply(null,[x_1]))}),clojure.core.keyword("","default")))))}).apply(null,[]);

//======
//(defn pr-on {:private true} [x w] (if *print-dup* (print-dup x w) (print-method x w)) nil)
//---
(function __clojure_core_fn_2274(){
return (clojure.JS.def(clojure.core,"pr_on",(function __clojure_core_fn_2274_pr_on_2276(x_1,w_2){
return (((clojure.core._STAR_print_dup_STAR_)?(clojure.core.print_dup.apply(null,[x_1,w_2])):(clojure.core.print_method.apply(null,[x_1,w_2]))),
null)})))}).apply(null,[]);

//======
//(defn pr "Prints the object(s) to the output stream that is the current value\n  of *out*.  Prints the object(s), separated by spaces if there is\n  more than one.  By default, pr and prn print in a way that objects\n  can be read by the reader" ([] nil) ([x] (pr-on x *out*)) ([x & more] (pr x) (. *out* (append \space)) (apply pr more)))
//---
(function __clojure_core_fn_2280(){
return (clojure.JS.def(clojure.core,"pr",clojure.JS.variadic(1,(function __clojure_core_fn_2280_pr_2282(x_1){switch(arguments.length){
case 0:return (null)
case 1:return (clojure.core.pr_on.apply(null,[x_1,clojure.core._STAR_out_STAR_]))}
var more_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.core.pr.apply(null,[x_1]),
(clojure.core._STAR_out_STAR_).append(" "),
clojure.core.apply.apply(null,[clojure.core.pr,more_2]))}))))}).apply(null,[]);

//======
//(defn newline "Writes a newline to the output stream that is the current value of\n  *out*" [] (. *out* (append \newline)) nil)
//---
(function __clojure_core_fn_2288(){
return (clojure.JS.def(clojure.core,"newline",(function __clojure_core_fn_2288_newline_2290(){
return ((clojure.core._STAR_out_STAR_).append("\n"),
null)})))}).apply(null,[]);

//======
//(defn flush "Flushes the output stream that is the current value of\n  *out*" [] (. *out* (flush)) nil)
//---
(function __clojure_core_fn_2294(){
return (clojure.JS.def(clojure.core,"flush",(function __clojure_core_fn_2294_flush_2296(){
return ((clojure.core._STAR_out_STAR_).flush(),
null)})))}).apply(null,[]);

//======
//(defn prn "Same as pr followed by (newline). Observes *flush-on-newline*" [& more] (apply pr more) (newline) (when *flush-on-newline* (flush)))
//---
(function __clojure_core_fn_2300(){
return (clojure.JS.def(clojure.core,"prn",clojure.JS.variadic(0,(function __clojure_core_fn_2300_prn_2302(){
var more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[clojure.core.pr,more_1]),
clojure.core.newline.apply(null,[]),
((clojure.core._STAR_flush_on_newline_STAR_)?(clojure.core.flush.apply(null,[])):(null)))}))))}).apply(null,[]);

//======
//(defn print "Prints the object(s) to the output stream that is the current value\n  of *out*.  print and println produce output for human consumption." [& more] (binding [*print-readably* nil] (apply pr more)))
//---
(function __clojure_core_fn_2306(){
return (clojure.JS.def(clojure.core,"print",clojure.JS.variadic(0,(function __clojure_core_fn_2306_print_2308(){
var more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_print_readably_STAR_,null])),
(function __try(){try{var _rtn=(clojure.core.apply.apply(null,[clojure.core.pr,more_1]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())}))))}).apply(null,[]);

//======
//(defn println "Same as print followed by (newline)" [& more] (binding [*print-readably* nil] (apply prn more)))
//---
(function __clojure_core_fn_2312(){
return (clojure.JS.def(clojure.core,"println",clojure.JS.variadic(0,(function __clojure_core_fn_2312_println_2314(){
var more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_print_readably_STAR_,null])),
(function __try(){try{var _rtn=(clojure.core.apply.apply(null,[clojure.core.prn,more_1]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())}))))}).apply(null,[]);

//======
//(defn read "Reads the next object from stream, which must be an instance of\n  java.io.PushbackReader or some derivee.  stream defaults to the\n  current value of *in* ." ([] (read *in*)) ([stream] (read stream true nil)) ([stream eof-error? eof-value] (read stream eof-error? eof-value false)) ([stream eof-error? eof-value recursive?] (. clojure.lang.LispReader (read stream (boolean eof-error?) eof-value recursive?))))
//---
(function __clojure_core_fn_2318(){
return (clojure.JS.def(clojure.core,"read",(function __clojure_core_fn_2318_read_2320(stream_1,eof_error_QMARK__2,eof_value_3,recursive_QMARK__4){switch(arguments.length){
case 0:return (clojure.core.read.apply(null,[clojure.core._STAR_in_STAR_]))
case 1:return (clojure.core.read.apply(null,[stream_1,true,null]))
case 3:return (clojure.core.read.apply(null,[stream_1,eof_error_QMARK__2,eof_value_3,false]))}
return (clojure.lang.LispReader.read(stream_1,clojure.core.boolean_.apply(null,[eof_error_QMARK__2]),eof_value_3,recursive_QMARK__4))})))}).apply(null,[]);

//======
//(defn read-line "Reads the next line from stream that is the current value of *in* ." [] (. *in* (readLine)))
//---
(function __clojure_core_fn_2327(){
return (clojure.JS.def(clojure.core,"read_line",(function __clojure_core_fn_2327_read_line_2329(){
return ((clojure.core._STAR_in_STAR_).readLine())})))}).apply(null,[]);

//======
//(defn read-string "Reads one object from the string s" [s] (clojure.lang.RT/readString s))
//---
(function __clojure_core_fn_2333(){
return (clojure.JS.def(clojure.core,"read_string",(function __clojure_core_fn_2333_read_string_2335(s_1){
return (clojure.lang.RT.readString(s_1))})))}).apply(null,[]);

//======
//(defn subvec "Returns a persistent vector of the items in vector from\n  start (inclusive) to end (exclusive).  If end is not supplied,\n  defaults to (count vector). This operation is O(1) and very fast, as\n  the resulting vector shares structure with the original and no\n  trimming is done." ([v start] (subvec v start (count v))) ([v start end] (. clojure.lang.RT (subvec v start end))))
//---
(function __clojure_core_fn_2339(){
return (clojure.JS.def(clojure.core,"subvec",(function __clojure_core_fn_2339_subvec_2341(v_1,start_2,end_3){switch(arguments.length){
case 2:return (clojure.core.subvec.apply(null,[v_1,start_2,clojure.core.count.apply(null,[v_1])]))}
return (clojure.lang.RT.subvec(v_1,start_2,end_3))})))}).apply(null,[]);
// Skipping: (defmacro with-open "bindings => [name init ...]\n\n  Evaluates body in a try expression with names bound to the values\n  of the inits, and a finally clause that calls (.close name) on each\n  name in reverse order." [bindings & body] (assert-args with-open (vector? bindings) "a vector for its binding" (even? (count bindings)) "an even number of forms in binding vector") (cond (= (count bindings) 0) (clojure.core/concat (clojure.core/list (quote do)) body) (symbol? (bindings 0)) (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (subvec bindings 0 2)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote try)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/with-open)) (clojure.core/list (subvec bindings 2)) body)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote finally)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (bindings 0)) (clojure.core/list (quote clojure.core/close))))))))) :else (throw (IllegalArgumentException. "with-open only allows Symbols in bindings"))))
// Skipping: (defmacro doto "Evaluates x then calls all of the methods and functions with the \n  value of x supplied at the from of the given arguments.  The forms\n  are evaluated in order.  Returns x.\n\n  (doto (new java.util.HashMap) (.put \"a\" 1) (.put \"b\" 2))" [x & forms] (let [gx (gensym)] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list gx) (clojure.core/list x)))) (map (fn [f] (if (seq? f) (clojure.core/concat (clojure.core/list (first f)) (clojure.core/list gx) (rest f)) (clojure.core/concat (clojure.core/list f) (clojure.core/list gx)))) forms) (clojure.core/list gx))))
// Skipping: (defmacro memfn "Expands into code that creates a fn that expects to be passed an\n  object and any args and calls the named instance method on the\n  object passing the args. Use when you want to treat a Java method as\n  a first-class fn." [name & args] (clojure.core/concat (clojure.core/list (quote clojure.core/fn)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote target__2370__auto__)) args))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote target__2370__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list name) args))))))
// Skipping: (defmacro time "Evaluates expr and prints the time it took.  Returns the value of\n expr." [expr] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote start__2380__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote java.lang.System)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/nanoTime)))))) (clojure.core/list (quote ret__2381__auto__)) (clojure.core/list expr)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/prn)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/str)) (clojure.core/list "Elapsed time: ") (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core//)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/double)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/-)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote java.lang.System)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/nanoTime)))))) (clojure.core/list (quote start__2380__auto__)))))) (clojure.core/list 1000000.0))) (clojure.core/list " msecs"))))) (clojure.core/list (quote ret__2381__auto__))))

//======
//(import (quote (java.lang.reflect Array)))
//---
(function __clojure_core_fn_2391(){
return (clojure.core.import_.apply(null,[clojure.JS.lit_list([clojure.core.symbol("java.lang.reflect"),clojure.core.symbol("Array")])]))}).apply(null,[]);

//======
//(import (quote (clojure.lang RT)))
//---
(function __clojure_core_fn_2394(){
return (clojure.core.import_.apply(null,[clojure.JS.lit_list([clojure.core.symbol("clojure.lang"),clojure.core.symbol("RT")])]))}).apply(null,[]);

//======
//(defn alength "Returns the length of the Java array. Works on arrays of all\n  types." {:inline (fn [a] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/alength)) (clojure.core/list a)))))} [array] (. clojure.lang.RT (alength array)))
//---
(function __clojure_core_fn_2397(){
return (clojure.JS.def(clojure.core,"alength",(function __clojure_core_fn_2397_alength_2402(array_1){
return (clojure.lang.RT.alength(array_1))})))}).apply(null,[]);

//======
//(defn aclone "Returns a clone of the Java array. Works on arrays of known\n  types." {:inline (fn [a] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/aclone)) (clojure.core/list a)))))} [array] (. clojure.lang.RT (aclone array)))
//---
(function __clojure_core_fn_2406(){
return (clojure.JS.def(clojure.core,"aclone",(function __clojure_core_fn_2406_aclone_2411(array_1){
return (clojure.lang.RT.aclone(array_1))})))}).apply(null,[]);

//======
//(defn aget "Returns the value at the index/indices. Works on Java arrays of all\n  types." {:inline (fn [a i] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/aget)) (clojure.core/list a) (clojure.core/list i))))), :inline-arities #{2}} ([array idx] (clojure.lang.Reflector/prepRet (RT/aget array idx))) ([array idx & idxs] (apply aget (aget array idx) idxs)))
//---
(function __clojure_core_fn_2415(){
return (clojure.JS.def(clojure.core,"aget",clojure.JS.variadic(2,(function __clojure_core_fn_2415_aget_2420(array_1,idx_2){switch(arguments.length){
case 2:return (clojure.lang.Reflector.prepRet(clojure.lang.RT.aget(array_1,idx_2)))}
var idxs_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.apply.apply(null,[clojure.core.aget,clojure.lang.RT.aget(array_1,idx_2),idxs_3]))}))))}).apply(null,[]);

//======
//(defn aset "Sets the value at the index/indices. Works on Java arrays of\n  reference types. Returns val." {:inline (fn [a i v] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.RT)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/aset)) (clojure.core/list a) (clojure.core/list i) (clojure.core/list v))))), :inline-arities #{3}} ([array idx val] (RT/aset array idx val) val) ([array idx idx2 & idxv] (apply aset (aget array idx) idx2 idxv)))
//---
(function __clojure_core_fn_2425(){
return (clojure.JS.def(clojure.core,"aset",clojure.JS.variadic(3,(function __clojure_core_fn_2425_aset_2430(array_1,idx_2,idx2_3){switch(arguments.length){
case 3:var val_3=arguments[2];
return (clojure.lang.RT.aset(array_1,idx_2,val_3),
val_3)}
var idxv_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.core.apply.apply(null,[clojure.core.aset,clojure.lang.RT.aget(array_1,idx_2),idx2_3,idxv_4]))}))))}).apply(null,[]);
// Skipping: (defmacro def-aset [name method coerce] (clojure.core/concat (clojure.core/list (quote clojure.core/defn)) (clojure.core/list name) (clojure.core/list (clojure.core/apply clojure.core/hash-map (clojure.core/concat (clojure.core/list :arglists) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list (clojure.core/concat (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote array)) (clojure.core/list (quote idx)) (clojure.core/list (quote val))))) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote array)) (clojure.core/list (quote idx)) (clojure.core/list (quote idx2)) (clojure.core/list (quote &)) (clojure.core/list (quote idxv)))))))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote array__2435__auto__)) (clojure.core/list (quote idx__2436__auto__)) (clojure.core/list (quote val__2437__auto__))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote java.lang.reflect.Array)) (clojure.core/list (clojure.core/concat (clojure.core/list method) (clojure.core/list (quote array__2435__auto__)) (clojure.core/list (quote idx__2436__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list coerce) (clojure.core/list (quote val__2437__auto__)))))))) (clojure.core/list (quote val__2437__auto__)))) (clojure.core/list (clojure.core/concat (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote array__2435__auto__)) (clojure.core/list (quote idx__2436__auto__)) (clojure.core/list (quote idx2__2438__auto__)) (clojure.core/list (quote &)) (clojure.core/list (quote idxv__2439__auto__))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/apply)) (clojure.core/list name) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/aget)) (clojure.core/list (quote array__2435__auto__)) (clojure.core/list (quote idx__2436__auto__)))) (clojure.core/list (quote idx2__2438__auto__)) (clojure.core/list (quote idxv__2439__auto__))))))))
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
(function __clojure_core_fn_2530(){
return (clojure.JS.def(clojure.core,"create_struct",clojure.JS.variadic(0,(function __clojure_core_fn_2530_create_struct_2532(){
var keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentStructMap.createSlotMap(keys_1))}))))}).apply(null,[]);
// Skipping: (defmacro defstruct "Same as (def name (create-struct keys...))" [name & keys] (clojure.core/concat (clojure.core/list (quote def)) (clojure.core/list name) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/create-struct)) keys))))

//======
//(defn struct-map "Returns a new structmap instance with the keys of the\n  structure-basis. keyvals may contain all, some or none of the basis\n  keys - where values are not supplied they will default to nil.\n  keyvals can also contain keys not in the basis." [s & inits] (. clojure.lang.PersistentStructMap (create s inits)))
//---
(function __clojure_core_fn_2545(){
return (clojure.JS.def(clojure.core,"struct_map",clojure.JS.variadic(1,(function __clojure_core_fn_2545_struct_map_2547(s_1){
var inits_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentStructMap.create(s_1,inits_2))}))))}).apply(null,[]);

//======
//(defn struct "Returns a new structmap instance with the keys of the\n  structure-basis. vals must be supplied for basis keys in order -\n  where values are not supplied they will default to nil." [s & vals] (. clojure.lang.PersistentStructMap (construct s vals)))
//---
(function __clojure_core_fn_2551(){
return (clojure.JS.def(clojure.core,"struct",clojure.JS.variadic(1,(function __clojure_core_fn_2551_struct_2553(s_1){
var vals_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentStructMap.construct(s_1,vals_2))}))))}).apply(null,[]);

//======
//(defn accessor "Returns a fn that, given an instance of a structmap with the basis,\n  returns the value at the key.  The key must be in the basis. The\n  returned function should be (slightly) more efficient than using\n  get, but such use of accessors should be limited to known\n  performance-critical areas." [s key] (. clojure.lang.PersistentStructMap (getAccessor s key)))
//---
(function __clojure_core_fn_2557(){
return (clojure.JS.def(clojure.core,"accessor",(function __clojure_core_fn_2557_accessor_2559(s_1,key_2){
return (clojure.lang.PersistentStructMap.getAccessor(s_1,key_2))})))}).apply(null,[]);
// Skipping: (defn load-reader "Sequentially read and evaluate the set of forms contained in the\n  stream/file" [rdr] (. clojure.lang.Compiler (load rdr)))
// Skipping: (defn load-string "Sequentially read and evaluate the set of forms contained in the\n  string" [s] (let [rdr (-> (java.io.StringReader. s) (clojure.lang.LineNumberingPushbackReader.))] (load-reader rdr)))

//======
//(defn resultset-seq "Creates and returns a lazy sequence of structmaps corresponding to\n  the rows in the java.sql.ResultSet rs" [rs] (let [rsmeta (. rs (getMetaData)) idxs (range 1 (inc (. rsmeta (getColumnCount)))) keys (map (comp keyword (memfn toLowerCase)) (map (fn [i] (. rsmeta (getColumnName i))) idxs)) row-struct (apply create-struct keys) row-values (fn [] (map (fn [i] (. rs (getObject i))) idxs)) rows (fn thisfn [] (when (. rs (next)) (lazy-cons (apply struct row-struct (row-values)) (thisfn))))] (rows)))
//---
(function __clojure_core_fn_2575(){
return (clojure.JS.def(clojure.core,"resultset_seq",(function __clojure_core_fn_2575_resultset_seq_2577(rs_1){
var rsmeta_2,idxs_3,keys_4,row_struct_5,row_values_6,rows_7;
return (((rsmeta_2=(rs_1).getMetaData()),
(idxs_3=clojure.core.range.apply(null,[(1),clojure.lang.Numbers.inc((rsmeta_2).getColumnCount())])),
(keys_4=clojure.core.map.apply(null,[clojure.core.comp.apply(null,[clojure.core.keyword,(function __clojure_core_fn_2575_resultset_seq_2577_fn_2579(target__2370__auto___1){
return ((target__2370__auto___1).toLowerCase())})]),clojure.core.map.apply(null,[(function __clojure_core_fn_2575_resultset_seq_2577_fn_2582(i_1){
return ((rsmeta_2).getColumnName(i_1))}),idxs_3])])),
(row_struct_5=clojure.core.apply.apply(null,[clojure.core.create_struct,keys_4])),
(row_values_6=(function __clojure_core_fn_2575_resultset_seq_2577_row_values_2585(){
return (clojure.core.map.apply(null,[(function __clojure_core_fn_2575_resultset_seq_2577_row_values_2585_fn_2587(i_1){
return ((rs_1).getObject(i_1))}),idxs_3]))})),
(rows_7=(function __clojure_core_fn_2575_resultset_seq_2577_thisfn_2591(){
var thisfn_0=arguments.callee;
return ((((rs_1).next())?((new clojure.lang.LazyCons((function __clojure_core_fn_2575_resultset_seq_2577_thisfn_2591_fn_2593(G__2592_1){switch(arguments.length){
case 0:return (clojure.core.apply.apply(null,[clojure.core.struct,row_struct_5,row_values_6.apply(null,[])]))}
return (thisfn_0.apply(null,[]))})))):(null)))})),
rows_7.apply(null,[])))})))}).apply(null,[]);

//======
//(defn set "Returns a set of the distinct elements of coll." [coll] (apply hash-set coll))
//---
(function __clojure_core_fn_2600(){
return (clojure.JS.def(clojure.core,"set",(function __clojure_core_fn_2600_set_2602(coll_1){
return (clojure.core.apply.apply(null,[clojure.core.hash_set,coll_1]))})))}).apply(null,[]);
// Skipping: (defn class? "Returns true if x is an instance of Class" [x] (instance? Class x))

//======
//(defn filter-key [keyfn pred amap] (loop [ret {} es (seq amap)] (if es (if (pred (keyfn (first es))) (recur (assoc ret (key (first es)) (val (first es))) (rest es)) (recur ret (rest es))) ret)))
//---
(function __clojure_core_fn_2612(){
return (clojure.JS.def(clojure.core,"filter_key",(function __clojure_core_fn_2612_filter_key_2614(keyfn_1,pred_2,amap_3){
var ret_4,es_5;
return (((function __loop(){var _rtn,_cnt;(ret_4=clojure.lang.PersistentArrayMap.EMPTY),
(es_5=clojure.core.seq.apply(null,[amap_3]));do{_cnt=0;
_rtn=((es_5)?(((pred_2.apply(null,[keyfn_1.apply(null,[clojure.core.first.apply(null,[es_5])])]))?((_cnt=1,_rtn=[clojure.core.assoc.apply(null,[ret_4,clojure.core.key.apply(null,[clojure.core.first.apply(null,[es_5])]),clojure.core.val.apply(null,[clojure.core.first.apply(null,[es_5])])]),clojure.core.rest.apply(null,[es_5])],ret_4=_rtn[0],es_5=_rtn[1])):((_cnt=1,_rtn=[ret_4,clojure.core.rest.apply(null,[es_5])],ret_4=_rtn[0],es_5=_rtn[1])))):(ret_4))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn find-ns "Returns the namespace named by the symbol or nil if it doesn't exist." [sym] (clojure.lang.Namespace/find sym))
//---
(function __clojure_core_fn_2618(){
return (clojure.JS.def(clojure.core,"find_ns",(function __clojure_core_fn_2618_find_ns_2620(sym_1){
return (clojure.lang.Namespace.find(sym_1))})))}).apply(null,[]);

//======
//(defn create-ns "Create a new namespace named by the symbol if one doesn't already\n  exist, returns it or the already-existing namespace of the same\n  name." [sym] (clojure.lang.Namespace/findOrCreate sym))
//---
(function __clojure_core_fn_2624(){
return (clojure.JS.def(clojure.core,"create_ns",(function __clojure_core_fn_2624_create_ns_2626(sym_1){
return (clojure.lang.Namespace.findOrCreate(sym_1))})))}).apply(null,[]);

//======
//(defn remove-ns "Removes the namespace named by the symbol. Use with caution.\n  Cannot be used to remove the clojure namespace." [sym] (clojure.lang.Namespace/remove sym))
//---
(function __clojure_core_fn_2630(){
return (clojure.JS.def(clojure.core,"remove_ns",(function __clojure_core_fn_2630_remove_ns_2632(sym_1){
return (clojure.lang.Namespace.remove(sym_1))})))}).apply(null,[]);

//======
//(defn all-ns "Returns a sequence of all namespaces." [] (clojure.lang.Namespace/all))
//---
(function __clojure_core_fn_2636(){
return (clojure.JS.def(clojure.core,"all_ns",(function __clojure_core_fn_2636_all_ns_2638(){
return (clojure.lang.Namespace.all())})))}).apply(null,[]);

//======
//(defn the-ns "If passed a namespace, returns it. Else, when passed a symbol,\n  returns the namespace named by it, throwing an exception if not\n  found." [x] (if (instance? clojure.lang.Namespace x) x (or (find-ns x) (throw (RT/makeException (str "No namespace: " x " found"))))))
//---
(function __clojure_core_fn_2642(){
return (clojure.JS.def(clojure.core,"the_ns",(function __clojure_core_fn_2642_the_ns_2644(x_1){
var or__962__auto___2;
return (((clojure.core.instance_QMARK_.apply(null,[clojure.lang.Namespace,x_1]))?(x_1):(((or__962__auto___2=clojure.core.find_ns.apply(null,[x_1])),
((or__962__auto___2)?(or__962__auto___2):((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["No namespace: ",x_1," found"]))})()))))))})))}).apply(null,[]);

//======
//(defn ns-name "Returns the name of the namespace, a symbol." [ns] (.getName (the-ns ns)))
//---
(function __clojure_core_fn_2648(){
return (clojure.JS.def(clojure.core,"ns_name",(function __clojure_core_fn_2648_ns_name_2650(ns_1){
return ((clojure.core.the_ns.apply(null,[ns_1])).getName())})))}).apply(null,[]);

//======
//(defn ns-map "Returns a map of all the mappings for the namespace." [ns] (.getMappings (the-ns ns)))
//---
(function __clojure_core_fn_2654(){
return (clojure.JS.def(clojure.core,"ns_map",(function __clojure_core_fn_2654_ns_map_2656(ns_1){
return ((clojure.core.the_ns.apply(null,[ns_1])).getMappings())})))}).apply(null,[]);

//======
//(defn ns-unmap "Removes the mappings for the symbol from the namespace." [ns sym] (.unmap (the-ns ns) sym))
//---
(function __clojure_core_fn_2660(){
return (clojure.JS.def(clojure.core,"ns_unmap",(function __clojure_core_fn_2660_ns_unmap_2662(ns_1,sym_2){
return ((clojure.core.the_ns.apply(null,[ns_1])).unmap(sym_2))})))}).apply(null,[]);

//======
//(defn ns-publics "Returns a map of the public intern mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (= ns (.ns v)) (.isPublic v))) (ns-map ns))))
//---
(function __clojure_core_fn_2666(){
return (clojure.JS.def(clojure.core,"ns_publics",(function __clojure_core_fn_2666_ns_publics_2668(ns_1){
var ns_2;
return (((ns_2=clojure.core.the_ns.apply(null,[ns_1])),
clojure.core.filter_key.apply(null,[clojure.core.val,(function __clojure_core_fn_2666_ns_publics_2668_fn_2670(v_1){
var and__948__auto___2,and__948__auto___3;
return (((and__948__auto___2=clojure.core.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__948__auto___2)?(((and__948__auto___3=clojure.lang.Util.equiv(ns_2,clojure.JS.getOrRun(v_1,"ns"))),
((and__948__auto___3)?((v_1).isPublic()):(and__948__auto___3)))):(and__948__auto___2))))}),clojure.core.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn ns-imports "Returns a map of the import mappings for the namespace." [ns] (filter-key val class? (ns-map ns)))
//---
(function __clojure_core_fn_2675(){
return (clojure.JS.def(clojure.core,"ns_imports",(function __clojure_core_fn_2675_ns_imports_2677(ns_1){
return (clojure.core.filter_key.apply(null,[clojure.core.val,clojure.core.class_QMARK_,clojure.core.ns_map.apply(null,[ns_1])]))})))}).apply(null,[]);
// Skipping: (defn refer "refers to all public vars of ns, subject to filters.\n  filters can include at most one each of:\n\n  :exclude list-of-symbols\n  :only list-of-symbols\n  :rename map-of-fromsymbol-tosymbol\n\n  For each public interned var in the namespace named by the symbol,\n  adds a mapping from the name of the var to the var to the current\n  namespace.  Throws an exception if name is already mapped to\n  something else in the current namespace. Filters can be used to\n  select a subset, via inclusion or exclusion, or to provide a mapping\n  to a symbol different from the var's name, in order to prevent\n  clashes. Use :use in the ns macro in preference to calling this directly." [ns-sym & filters] (let [ns (or (find-ns ns-sym) (throw (RT/makeException (str "No namespace: " ns-sym)))) fs (apply hash-map filters) nspublics (ns-publics ns) rename (or (:rename fs) {}) exclude (set (:exclude fs)) to-do (or (:only fs) (keys nspublics))] (doseq [sym to-do] (when-not (exclude sym) (let [v (nspublics sym)] (when-not v (throw (new java.lang.IllegalAccessError (str sym " is not public")))) (. *ns* (refer (or (rename sym) sym) v)))))))

//======
//(defn ns-refers "Returns a map of the refer mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (not= ns (.ns v)))) (ns-map ns))))
//---
(function __clojure_core_fn_2687(){
return (clojure.JS.def(clojure.core,"ns_refers",(function __clojure_core_fn_2687_ns_refers_2689(ns_1){
var ns_2;
return (((ns_2=clojure.core.the_ns.apply(null,[ns_1])),
clojure.core.filter_key.apply(null,[clojure.core.val,(function __clojure_core_fn_2687_ns_refers_2689_fn_2691(v_1){
var and__948__auto___2;
return (((and__948__auto___2=clojure.core.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__948__auto___2)?(clojure.core.not_EQ_.apply(null,[ns_2,clojure.JS.getOrRun(v_1,"ns")])):(and__948__auto___2))))}),clojure.core.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn ns-interns "Returns a map of the intern mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (= ns (.ns v)))) (ns-map ns))))
//---
(function __clojure_core_fn_2696(){
return (clojure.JS.def(clojure.core,"ns_interns",(function __clojure_core_fn_2696_ns_interns_2698(ns_1){
var ns_2;
return (((ns_2=clojure.core.the_ns.apply(null,[ns_1])),
clojure.core.filter_key.apply(null,[clojure.core.val,(function __clojure_core_fn_2696_ns_interns_2698_fn_2700(v_1){
var and__948__auto___2;
return (((and__948__auto___2=clojure.core.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__948__auto___2)?(clojure.lang.Util.equiv(ns_2,clojure.JS.getOrRun(v_1,"ns"))):(and__948__auto___2))))}),clojure.core.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn alias "Add an alias in the current namespace to another\n  namespace. Arguments are two symbols: the alias to be used, and\n  the symbolic name of the target namespace. Use :as in the ns macro in preference \n  to calling this directly." [alias namespace-sym] (.addAlias *ns* alias (find-ns namespace-sym)))
//---
(function __clojure_core_fn_2705(){
return (clojure.JS.def(clojure.core,"alias",(function __clojure_core_fn_2705_alias_2707(alias_1,namespace_sym_2){
return ((clojure.core._STAR_ns_STAR_).addAlias(alias_1,clojure.core.find_ns.apply(null,[namespace_sym_2])))})))}).apply(null,[]);

//======
//(defn ns-aliases "Returns a map of the aliases for the namespace." [ns] (.getAliases (the-ns ns)))
//---
(function __clojure_core_fn_2711(){
return (clojure.JS.def(clojure.core,"ns_aliases",(function __clojure_core_fn_2711_ns_aliases_2713(ns_1){
return ((clojure.core.the_ns.apply(null,[ns_1])).getAliases())})))}).apply(null,[]);

//======
//(defn ns-unalias "Removes the alias for the symbol from the namespace." [ns sym] (.removeAlias (the-ns ns) sym))
//---
(function __clojure_core_fn_2717(){
return (clojure.JS.def(clojure.core,"ns_unalias",(function __clojure_core_fn_2717_ns_unalias_2719(ns_1,sym_2){
return ((clojure.core.the_ns.apply(null,[ns_1])).removeAlias(sym_2))})))}).apply(null,[]);

//======
//(defn take-nth "Returns a lazy seq of every nth item in coll." [n coll] (when (seq coll) (lazy-cons (first coll) (take-nth n (drop n coll)))))
//---
(function __clojure_core_fn_2723(){
return (clojure.JS.def(clojure.core,"take_nth",(function __clojure_core_fn_2723_take_nth_2725(n_1,coll_2){
return (((clojure.core.seq.apply(null,[coll_2]))?((new clojure.lang.LazyCons((function __clojure_core_fn_2723_take_nth_2725_fn_2728(G__2727_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll_2]))}
return (clojure.core.take_nth.apply(null,[n_1,clojure.core.drop.apply(null,[n_1,coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn interleave "Returns a lazy seq of the first item in each coll, then the second\n  etc." [& colls] (apply concat (apply map list colls)))
//---
(function __clojure_core_fn_2734(){
return (clojure.JS.def(clojure.core,"interleave",clojure.JS.variadic(0,(function __clojure_core_fn_2734_interleave_2736(){
var colls_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[clojure.core.concat,clojure.core.apply.apply(null,[clojure.core.map,clojure.core.list,colls_1])]))}))))}).apply(null,[]);

//======
//(defn var-get "Gets the value in the var object" [x] (. x (get)))
//---
(function __clojure_core_fn_2740(){
return (clojure.JS.def(clojure.core,"var_get",(function __clojure_core_fn_2740_var_get_2742(x_1){
return ((x_1).get())})))}).apply(null,[]);

//======
//(defn var-set "Sets the value in the var object to val. The var must be\n thread-locally bound." [x val] (. x (set val)))
//---
(function __clojure_core_fn_2746(){
return (clojure.JS.def(clojure.core,"var_set",(function __clojure_core_fn_2746_var_set_2748(x_1,val_2){
return ((x_1).set(val_2))})))}).apply(null,[]);
// Skipping: (defmacro with-local-vars "varbinding=> symbol init-expr\n\n  Executes the exprs in a context in which the symbols are bound to\n  vars with per-thread bindings to the init-exprs.  The symbols refer\n  to the var objects themselves, and must be accessed with var-get and\n  var-set" [name-vals-vec & body] (assert-args with-local-vars (vector? name-vals-vec) "a vector for its binding" (even? (count name-vals-vec)) "an even number of forms in binding vector") (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (interleave (take-nth 2 name-vals-vec) (repeat (quote (. clojure.lang.Var (create)))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Var)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/pushThreadBindings)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/hash-map)) name-vals-vec)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote try)) body (clojure.core/list (clojure.core/concat (clojure.core/list (quote finally)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Var)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/popThreadBindings))))))))))))
// Skipping: (defn ns-resolve "Returns the var or Class to which a symbol will be resolved in the\n  namespace, else nil.  Note that if the symbol is fully qualified,\n  the var/Class to which it resolves need not be present in the\n  namespace." [ns sym] (clojure.lang.Compiler/maybeResolveIn (the-ns ns) sym))
// Skipping: (defn resolve "same as (ns-resolve *ns* symbol)" [sym] (ns-resolve *ns* sym))

//======
//(defn array-map "Constructs an array-map." ([] (. clojure.lang.PersistentArrayMap EMPTY)) ([& keyvals] (new clojure.lang.PersistentArrayMap (to-array keyvals))))
//---
(function __clojure_core_fn_2773(){
return (clojure.JS.def(clojure.core,"array_map",clojure.JS.variadic(0,(function __clojure_core_fn_2773_array_map_2775(){switch(arguments.length){
case 0:return (clojure.JS.getOrRun(clojure.lang.PersistentArrayMap,"EMPTY"))}
var keyvals_1=clojure.JS.rest_args(this,arguments,0);
return ((new clojure.lang.PersistentArrayMap(clojure.core.to_array.apply(null,[keyvals_1]))))}))))}).apply(null,[]);

//======
//(defn nthrest "Returns the nth rest of coll, (seq coll) when n is 0." [coll n] (loop [n n xs (seq coll)] (if (and xs (pos? n)) (recur (dec n) (rest xs)) xs)))
//---
(function __clojure_core_fn_2780(){
return (clojure.JS.def(clojure.core,"nthrest",(function __clojure_core_fn_2780_nthrest_2782(coll_1,n_2){
var n_3,xs_4,and__948__auto___5;
return (((function __loop(){var _rtn,_cnt;(n_3=n_2),
(xs_4=clojure.core.seq.apply(null,[coll_1]));do{_cnt=0;
_rtn=((((and__948__auto___5=xs_4),
((and__948__auto___5)?(clojure.lang.Numbers.isPos(n_3)):(and__948__auto___5))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_3),clojure.core.rest.apply(null,[xs_4])],n_3=_rtn[0],xs_4=_rtn[1])):(xs_4))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn destructure [bindings] (let [bmap (apply array-map bindings) pb (fn pb [bvec b v] (let [pvec (fn [bvec b val] (let [gvec (gensym "vec__")] (loop [ret (-> bvec (conj gvec) (conj val)) n 0 bs b seen-rest? false] (if (seq bs) (let [firstb (first bs)] (cond (= firstb (quote &)) (recur (pb ret (second bs) (list (quote clojure.core/nthrest) gvec n)) n (rrest bs) true) (= firstb :as) (pb ret (second bs) gvec) :else (if seen-rest? (throw (RT/makeException "Unsupported binding form, only :as can follow & parameter")) (recur (pb ret firstb (list (quote clojure.core/nth) gvec n nil)) (inc n) (rest bs) seen-rest?)))) ret)))) pmap (fn [bvec b v] (let [gmap (or (:as b) (gensym "map__")) defaults (:or b)] (loop [ret (-> bvec (conj gmap) (conj v)) bes (reduce (fn [bes entry] (reduce (fn* [p1__2786 p2__2787] (assoc p1__2786 p2__2787 ((val entry) p2__2787))) (dissoc bes (key entry)) ((key entry) bes))) (dissoc b :as :or) {:keys (fn* [p1__2788] (keyword (str p1__2788))), :strs str, :syms (fn* [p1__2789] (list (quote quote) p1__2789))})] (if (seq bes) (let [bb (key (first bes)) bk (val (first bes)) has-default (contains? defaults bb)] (recur (pb ret bb (if has-default (list (quote clojure.core/get) gmap bk (defaults bb)) (list (quote clojure.core/get) gmap bk))) (rest bes))) ret))))] (cond (symbol? b) (-> bvec (conj b) (conj v)) (vector? b) (pvec bvec b v) (map? b) (pmap bvec b v) :else (throw (RT/makeException (str "Unsupported binding form: " b)))))) process-entry (fn [bvec b] (pb bvec (key b) (val b)))] (if (every? symbol? (keys bmap)) bindings (reduce process-entry [] bmap))))
//---
(function __clojure_core_fn_2790(){
return (clojure.JS.def(clojure.core,"destructure",(function __clojure_core_fn_2790_destructure_2792(bindings_1){
var bmap_2,pb_3,process_entry_4;
return (((bmap_2=clojure.core.apply.apply(null,[clojure.core.array_map,bindings_1])),
(pb_3=(function __clojure_core_fn_2790_destructure_2792_pb_2794(bvec_1,b_2,v_3){
var pvec_4,pmap_5,pb_0=arguments.callee;
return (((pvec_4=(function __clojure_core_fn_2790_destructure_2792_pb_2794_pvec_2795(bvec_1,b_2,val_3){
var seen_rest_QMARK__8,firstb_9,gvec_4,n_6,bs_7,ret_5;
return (((gvec_4=clojure.core.gensym.apply(null,["vec__"])),
((function __loop(){var _rtn,_cnt;(ret_5=clojure.core.conj.apply(null,[clojure.core.conj.apply(null,[bvec_1,gvec_4]),val_3])),
(n_6=(0)),
(bs_7=b_2),
(seen_rest_QMARK__8=false);do{_cnt=0;
_rtn=((clojure.core.seq.apply(null,[bs_7]))?(((firstb_9=clojure.core.first.apply(null,[bs_7])),
((clojure.lang.Util.equiv(firstb_9,clojure.core.symbol("&")))?((_cnt=1,_rtn=[pb_0.apply(null,[ret_5,clojure.core.second.apply(null,[bs_7]),clojure.core.list.apply(null,[clojure.core.symbol("clojure.core/nthrest"),gvec_4,n_6])]),n_6,clojure.core.rrest.apply(null,[bs_7]),true],ret_5=_rtn[0],n_6=_rtn[1],bs_7=_rtn[2],seen_rest_QMARK__8=_rtn[3])):(((clojure.lang.Util.equiv(firstb_9,clojure.core.keyword("","as")))?(pb_0.apply(null,[ret_5,clojure.core.second.apply(null,[bs_7]),gvec_4])):(((clojure.core.keyword("","else"))?(((seen_rest_QMARK__8)?((function __throw(){throw clojure.lang.RT.makeException("Unsupported binding form, only :as can follow & parameter")})()):((_cnt=1,_rtn=[pb_0.apply(null,[ret_5,firstb_9,clojure.core.list.apply(null,[clojure.core.symbol("clojure.core/nth"),gvec_4,n_6,null])]),clojure.lang.Numbers.inc(n_6),clojure.core.rest.apply(null,[bs_7]),seen_rest_QMARK__8],ret_5=_rtn[0],n_6=_rtn[1],bs_7=_rtn[2],seen_rest_QMARK__8=_rtn[3])))):(null)))))))):(ret_5))}while(_cnt);return _rtn;})())))})),
(pmap_5=(function __clojure_core_fn_2790_destructure_2792_pb_2794_pmap_2798(bvec_1,b_2,v_3){
var has_default_10,bes_7,or__962__auto___4,gmap_4,defaults_5,ret_6,bk_9,bb_8;
return (((gmap_4=((or__962__auto___4=clojure.core.keyword("","as").apply(null,[b_2])),
((or__962__auto___4)?(or__962__auto___4):(clojure.core.gensym.apply(null,["map__"]))))),
(defaults_5=clojure.core.keyword("","or").apply(null,[b_2])),
((function __loop(){var _rtn,_cnt;(ret_6=clojure.core.conj.apply(null,[clojure.core.conj.apply(null,[bvec_1,gmap_4]),v_3])),
(bes_7=clojure.core.reduce.apply(null,[(function __clojure_core_fn_2790_destructure_2792_pb_2794_pmap_2798_fn_2800(bes_1,entry_2){
return (clojure.core.reduce.apply(null,[(function __clojure_core_fn_2790_destructure_2792_pb_2794_pmap_2798_fn_2800_fn_2802(p1__2786_1,p2__2787_2){
return (clojure.core.assoc.apply(null,[p1__2786_1,p2__2787_2,clojure.core.val.apply(null,[entry_2]).apply(null,[p2__2787_2])]))}),clojure.core.dissoc.apply(null,[bes_1,clojure.core.key.apply(null,[entry_2])]),clojure.core.key.apply(null,[entry_2]).apply(null,[bes_1])]))}),clojure.core.dissoc.apply(null,[b_2,clojure.core.keyword("","as"),clojure.core.keyword("","or")]),clojure.core.hash_map(clojure.core.keyword("","keys"),(function __clojure_core_fn_2790_destructure_2792_pb_2794_pmap_2798_fn_2806(p1__2788_1){
return (clojure.core.keyword.apply(null,[clojure.core.str.apply(null,[p1__2788_1])]))}),clojure.core.keyword("","strs"),clojure.core.str,clojure.core.keyword("","syms"),(function __clojure_core_fn_2790_destructure_2792_pb_2794_pmap_2798_fn_2809(p1__2789_1){
return (clojure.core.list.apply(null,[clojure.core.symbol("quote"),p1__2789_1]))}))]));do{_cnt=0;
_rtn=((clojure.core.seq.apply(null,[bes_7]))?(((bb_8=clojure.core.key.apply(null,[clojure.core.first.apply(null,[bes_7])])),
(bk_9=clojure.core.val.apply(null,[clojure.core.first.apply(null,[bes_7])])),
(has_default_10=clojure.core.contains_QMARK_.apply(null,[defaults_5,bb_8])),
(_cnt=1,_rtn=[pb_0.apply(null,[ret_6,bb_8,((has_default_10)?(clojure.core.list.apply(null,[clojure.core.symbol("clojure.core/get"),gmap_4,bk_9,defaults_5.apply(null,[bb_8])])):(clojure.core.list.apply(null,[clojure.core.symbol("clojure.core/get"),gmap_4,bk_9])))]),clojure.core.rest.apply(null,[bes_7])],ret_6=_rtn[0],bes_7=_rtn[1]))):(ret_6))}while(_cnt);return _rtn;})())))})),
((clojure.core.symbol_QMARK_.apply(null,[b_2]))?(clojure.core.conj.apply(null,[clojure.core.conj.apply(null,[bvec_1,b_2]),v_3])):(((clojure.core.vector_QMARK_.apply(null,[b_2]))?(pvec_4.apply(null,[bvec_1,b_2,v_3])):(((clojure.core.map_QMARK_.apply(null,[b_2]))?(pmap_5.apply(null,[bvec_1,b_2,v_3])):(((clojure.core.keyword("","else"))?((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["Unsupported binding form: ",b_2]))})()):(null))))))))))})),
(process_entry_4=(function __clojure_core_fn_2790_destructure_2792_process_entry_2814(bvec_1,b_2){
return (pb_3.apply(null,[bvec_1,clojure.core.key.apply(null,[b_2]),clojure.core.val.apply(null,[b_2])]))})),
((clojure.core.every_QMARK_.apply(null,[clojure.core.symbol_QMARK_,clojure.core.keys.apply(null,[bmap_2])]))?(bindings_1):(clojure.core.reduce.apply(null,[process_entry_4,clojure.lang.PersistentVector.EMPTY,bmap_2])))))})))}).apply(null,[]);
// Skipping: (defmacro let "Evaluates the exprs in a lexical context in which the symbols in\n  the binding-forms are bound to their respective init-exprs or parts\n  therein." [bindings & body] (assert-args let (vector? bindings) "a vector for its binding" (even? (count bindings)) "an even number of forms in binding vector") (clojure.core/concat (clojure.core/list (quote let*)) (clojure.core/list (destructure bindings)) body))
// Skipping: (defmacro fn "(fn name? [params* ] exprs*)\n  (fn name? ([params* ] exprs*)+)\n\n  params => positional-params* , or positional-params* & rest-param\n  positional-param => binding-form\n  rest-param => binding-form\n  name => symbol\n\n  Defines a function" [& sigs] (let [name (if (symbol? (first sigs)) (first sigs) nil) sigs (if name (rest sigs) sigs) sigs (if (vector? (first sigs)) (list sigs) sigs) psig (fn [sig] (let [[params & body] sig] (if (every? symbol? params) sig (loop [params params new-params [] lets []] (if params (if (symbol? (first params)) (recur (rest params) (conj new-params (first params)) lets) (let [gparam (gensym "p__")] (recur (rest params) (conj new-params gparam) (-> lets (conj (first params)) (conj gparam))))) (clojure.core/concat (clojure.core/list new-params) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list lets) body)))))))) new-sigs (map psig sigs)] (with-meta (if name (list* (quote fn*) name new-sigs) (cons (quote fn*) new-sigs)) *macro-meta*)))
// Skipping: (defmacro loop "Evaluates the exprs in a lexical context in which the symbols in\n  the binding-forms are bound to their respective init-exprs or parts\n  therein. Acts as a recur target." [bindings & body] (assert-args loop (vector? bindings) "a vector for its binding" (even? (count bindings)) "an even number of forms in binding vector") (let [db (destructure bindings)] (if (= db bindings) (clojure.core/concat (clojure.core/list (quote loop*)) (clojure.core/list bindings) body) (let [vs (take-nth 2 (drop 1 bindings)) bs (take-nth 2 bindings) gs (map (fn [b] (if (symbol? b) b (gensym))) bs) bfs (reduce (fn [ret [b v g]] (if (symbol? b) (conj ret g v) (conj ret g v b g))) [] (map vector bs vs gs))] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list bfs) (clojure.core/list (clojure.core/concat (clojure.core/list (quote loop*)) (clojure.core/list (vec (interleave gs gs))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (vec (interleave bs gs))) body)))))))))
// Skipping: (defmacro when-first "bindings => x xs\n\n  Same as (when (seq xs) (let [x (first xs)] body))" [bindings & body] (assert-args when-first (vector? bindings) "a vector for its binding" (= 2 (count bindings)) "exactly 2 forms in binding vector") (let [[x xs] bindings] (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/seq)) (clojure.core/list xs))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list x) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/first)) (clojure.core/list xs)))))) body)))))
// Skipping: (defmacro lazy-cat "Expands to code which yields a lazy sequence of the concatenation\n  of the supplied colls.  Each coll expr is not evaluated until it is\n  needed." ([coll] (clojure.core/concat (clojure.core/list (quote clojure.core/seq)) (clojure.core/list coll))) ([coll & colls] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote iter__2881__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/fn)) (clojure.core/list (quote iter__2881__auto__)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote coll__2882__auto__))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/seq)) (clojure.core/list (quote coll__2882__auto__)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/lazy-cons)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/first)) (clojure.core/list (quote coll__2882__auto__)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote iter__2881__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/rest)) (clojure.core/list (quote coll__2882__auto__)))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/lazy-cat)) colls))))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote iter__2881__auto__)) (clojure.core/list coll))))))
// Skipping: (defmacro for "List comprehension. Takes a vector of one or more\n binding-form/collection-expr pairs, each followed by an optional filtering\n :when/:while expression (:when test or :while test), and yields a\n lazy sequence of evaluations of expr. Collections are iterated in a\n nested fashion, rightmost fastest, and nested coll-exprs can refer to\n bindings created in prior binding-forms.\n\n (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)]  [x y]))" ([seq-exprs expr] (assert-args for (vector? seq-exprs) "a vector for its binding" (even? (count seq-exprs)) "an even number of forms in binding vector") (let [to-groups (fn [seq-exprs] (reduce (fn [groups [k v]] (if (keyword? k) (conj (pop groups) (assoc (peek groups) k v)) (conj groups {:bind k, :seq v}))) [] (partition 2 seq-exprs))) emit (fn emit [[group & [{next-seq :seq} :as more-groups]]] (let [giter (gensym "iter__") gxs (gensym "s__")] (clojure.core/concat (clojure.core/list (quote clojure.core/fn)) (clojure.core/list giter) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list gxs)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when-first)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (:bind group)) (clojure.core/list gxs)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (or (:while group) true)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (or (:when group) true)) (clojure.core/list (if more-groups (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote iterys__2894__auto__)) (clojure.core/list (emit more-groups)) (clojure.core/list (quote fs__2895__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote iterys__2894__auto__)) (clojure.core/list next-seq)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (quote fs__2895__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/lazy-cat)) (clojure.core/list (quote fs__2895__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list giter) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/rest)) (clojure.core/list gxs))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote recur)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/rest)) (clojure.core/list gxs)))))))) (clojure.core/concat (clojure.core/list (quote clojure.core/lazy-cons)) (clojure.core/list expr) (clojure.core/list (clojure.core/concat (clojure.core/list giter) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/rest)) (clojure.core/list gxs)))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote recur)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/rest)) (clojure.core/list gxs))))))))))))))] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote iter__2896__auto__)) (clojure.core/list (emit (to-groups seq-exprs)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote iter__2896__auto__)) (clojure.core/list (second seq-exprs))))))))
// Skipping: (defmacro comment "Ignores body, yields nil" [& body])
// Skipping: (defmacro with-out-str "Evaluates exprs in a context in which *out* is bound to a fresh\n  StringWriter.  Returns the string created by any nested printing\n  calls." [& body] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote s__2945__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.lang.RT/makeStringWriter))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/binding)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote clojure.core/*out*)) (clojure.core/list (quote s__2945__auto__))))) body (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/str)) (clojure.core/list (quote s__2945__auto__))))))))
// Skipping: (defmacro with-in-str "Evaluates body in a context in which *in* is bound to a fresh\n  StringReader initialized with the string s." [s & body] (clojure.core/concat (clojure.core/list (quote clojure.core/with-open)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote s__2955__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/->)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote java.io.StringReader.)) (clojure.core/list s))) (clojure.core/list (quote clojure.lang.LineNumberingPushbackReader.))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/binding)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote clojure.core/*in*)) (clojure.core/list (quote s__2955__auto__))))) body))))

//======
//(defn pr-str "pr to a string, returning it" {:tag String} [& xs] (with-out-str (apply pr xs)))
//---
(function __clojure_core_fn_2965(){
return (clojure.JS.def(clojure.core,"pr_str",clojure.JS.variadic(0,(function __clojure_core_fn_2965_pr_str_2967(){
var s__2945__auto___2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__2945__auto___2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_out_STAR_,s__2945__auto___2])),
(function __try(){try{var _rtn=(clojure.core.apply.apply(null,[clojure.core.pr,xs_1]),
clojure.core.str.apply(null,[s__2945__auto___2]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})()))}))))}).apply(null,[]);

//======
//(defn prn-str "prn to a string, returning it" {:tag String} [& xs] (with-out-str (apply prn xs)))
//---
(function __clojure_core_fn_2971(){
return (clojure.JS.def(clojure.core,"prn_str",clojure.JS.variadic(0,(function __clojure_core_fn_2971_prn_str_2973(){
var s__2945__auto___2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__2945__auto___2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_out_STAR_,s__2945__auto___2])),
(function __try(){try{var _rtn=(clojure.core.apply.apply(null,[clojure.core.prn,xs_1]),
clojure.core.str.apply(null,[s__2945__auto___2]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})()))}))))}).apply(null,[]);

//======
//(defn print-str "print to a string, returning it" {:tag String} [& xs] (with-out-str (apply print xs)))
//---
(function __clojure_core_fn_2977(){
return (clojure.JS.def(clojure.core,"print_str",clojure.JS.variadic(0,(function __clojure_core_fn_2977_print_str_2979(){
var s__2945__auto___2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__2945__auto___2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_out_STAR_,s__2945__auto___2])),
(function __try(){try{var _rtn=(clojure.core.apply.apply(null,[clojure.core.print,xs_1]),
clojure.core.str.apply(null,[s__2945__auto___2]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})()))}))))}).apply(null,[]);

//======
//(defn println-str "println to a string, returning it" {:tag String} [& xs] (with-out-str (apply println xs)))
//---
(function __clojure_core_fn_2983(){
return (clojure.JS.def(clojure.core,"println_str",clojure.JS.variadic(0,(function __clojure_core_fn_2983_println_str_2985(){
var s__2945__auto___2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__2945__auto___2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_out_STAR_,s__2945__auto___2])),
(function __try(){try{var _rtn=(clojure.core.apply.apply(null,[clojure.core.println,xs_1]),
clojure.core.str.apply(null,[s__2945__auto___2]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})()))}))))}).apply(null,[]);
// Skipping: (defmacro assert "Evaluates expr and throws an exception if it does not evaluate to\n logical true." [x] (clojure.core/concat (clojure.core/list (quote clojure.core/when-not)) (clojure.core/list x) (clojure.core/list (clojure.core/concat (clojure.core/list (quote throw)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.lang.RT/makeException)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/str)) (clojure.core/list "Assert failed: ") (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/pr-str)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list x)))))))))))))

//======
//(defn test "test [v] finds fn at key :test in var metadata and calls it,\n  presuming failure will throw exception" [v] (let [f (:test (clojure.core/meta v))] (if f (do (f) :ok) :no-test)))
//---
(function __clojure_core_fn_2998(){
return (clojure.JS.def(clojure.core,"test",(function __clojure_core_fn_2998_test_3000(v_1){
var f_2;
return (((f_2=clojure.core.keyword("","test").apply(null,[clojure.core.meta.apply(null,[v_1])])),
((f_2)?(f_2.apply(null,[]),
clojure.core.keyword("","ok")):(clojure.core.keyword("","no-test")))))})))}).apply(null,[]);
// Skipping: (defn re-pattern "Returns an instance of java.util.regex.Pattern, for use, e.g. in\n  re-matcher." {:tag java.util.regex.Pattern} [s] (if (instance? java.util.regex.Pattern s) s (. java.util.regex.Pattern (compile s))))
// Skipping: (defn re-matcher "Returns an instance of java.util.regex.Matcher, for use, e.g. in\n  re-find." {:tag java.util.regex.Matcher} [re s] (. re (matcher s)))
// Skipping: (defn re-groups "Returns the groups from the most recent match/find. If there are no\n  nested groups, returns a string of the entire match. If there are\n  nested groups, returns a vector of the groups, the first element\n  being the entire match." [m] (let [gc (. m (groupCount))] (if (zero? gc) (. m (group)) (loop [ret [] c 0] (if (<= c gc) (recur (conj ret (. m (group c))) (inc c)) ret)))))
// Skipping: (defn re-seq "Returns a lazy sequence of successive matches of pattern in string,\n  using java.util.regex.Matcher.find(), each such match processed with\n  re-groups." [re s] (let [m (re-matcher re s)] ((fn step [] (when (. m (find)) (lazy-cons (re-groups m) (step)))))))
// Skipping: (defn re-matches "Returns the match, if any, of string to pattern, using\n  java.util.regex.Matcher.matches().  Uses re-groups to return the\n  groups." [re s] (let [m (re-matcher re s)] (when (. m (matches)) (re-groups m))))
// Skipping: (defn re-find "Returns the next regex match, if any, of string to pattern, using\n  java.util.regex.Matcher.find().  Uses re-groups to return the\n  groups." ([m] (when (. m (find)) (re-groups m))) ([re s] (let [m (re-matcher re s)] (re-find m))))

//======
//(defn rand "Returns a random floating point number between 0 (inclusive) and\n  1 (exclusive)." ([] (RT/random)) ([n] (* n (rand))))
//---
(function __clojure_core_fn_3048(){
return (clojure.JS.def(clojure.core,"rand",(function __clojure_core_fn_3048_rand_3050(n_1){switch(arguments.length){
case 0:return (clojure.lang.RT.random())}
return (clojure.lang.Numbers.multiply(n_1,clojure.core.rand.apply(null,[])))})))}).apply(null,[]);

//======
//(defn rand-int "Returns a random integer between 0 (inclusive) and n (exclusive)." [n] (int (rand n)))
//---
(function __clojure_core_fn_3055(){
return (clojure.JS.def(clojure.core,"rand_int",(function __clojure_core_fn_3055_rand_int_3057(n_1){
return (clojure.lang.RT.intCast(clojure.core.rand.apply(null,[n_1])))})))}).apply(null,[]);
// Skipping: (defmacro defn- "same as defn, yielding non-public def" [name & decls] (list* (quote clojure.core/defn) (with-meta name (assoc (meta name) :private true)) decls))

//======
//(defn print-doc [v] (println "-------------------------") (println (str (ns-name (:ns (clojure.core/meta v))) "/" (:name (clojure.core/meta v)))) (prn (:arglists (clojure.core/meta v))) (when (:macro (clojure.core/meta v)) (println "Macro")) (println " " (:doc (clojure.core/meta v))))
//---
(function __clojure_core_fn_3070(){
return (clojure.JS.def(clojure.core,"print_doc",(function __clojure_core_fn_3070_print_doc_3072(v_1){
return (clojure.core.println.apply(null,["-------------------------"]),
clojure.core.println.apply(null,[clojure.core.str.apply(null,[clojure.core.ns_name.apply(null,[clojure.core.keyword("","ns").apply(null,[clojure.core.meta.apply(null,[v_1])])]),"/",clojure.core.keyword("","name").apply(null,[clojure.core.meta.apply(null,[v_1])])])]),
clojure.core.prn.apply(null,[clojure.core.keyword("","arglists").apply(null,[clojure.core.meta.apply(null,[v_1])])]),
((clojure.core.keyword("","macro").apply(null,[clojure.core.meta.apply(null,[v_1])]))?(clojure.core.println.apply(null,["Macro"])):(null)),
clojure.core.println.apply(null,[" ",clojure.core.keyword("","doc").apply(null,[clojure.core.meta.apply(null,[v_1])])]))})))}).apply(null,[]);

//======
//(defn find-doc "Prints documentation for any var whose documentation or name\n contains a match for re-string-or-pattern" [re-string-or-pattern] (let [re (re-pattern re-string-or-pattern)] (dorun (for [ns (all-ns) v (sort-by (comp :name meta) (vals (ns-interns ns))) :when (and (:doc (clojure.core/meta v)) (or (re-find (re-matcher re (:doc (clojure.core/meta v)))) (re-find (re-matcher re (str (:name (clojure.core/meta v)))))))] (print-doc v)))))
//---
(function __clojure_core_fn_3076(){
return (clojure.JS.def(clojure.core,"find_doc",(function __clojure_core_fn_3076_find_doc_3078(re_string_or_pattern_1){
var re_2,iter__2896__auto___3;
return (((re_2=clojure.core.re_pattern.apply(null,[re_string_or_pattern_1])),
clojure.core.dorun.apply(null,[((iter__2896__auto___3=(function __clojure_core_fn_3076_find_doc_3078_iter_3080_3084(s__3081_1){
var _cnt,_rtn,ns_2,iterys__2894__auto___3,fs__2895__auto___4,iter__2881__auto___5,iter__3080_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[s__3081_1]))?(((ns_2=clojure.core.first.apply(null,[s__3081_1])),
((true)?(((true)?(((iterys__2894__auto___3=(function __clojure_core_fn_3076_find_doc_3078_iter_3080_3084_iter_3082_3085(s__3083_1){
var _cnt,_rtn,v_2,and__948__auto___3,or__962__auto___4,iter__3082_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.core.seq.apply(null,[s__3083_1]))?(((v_2=clojure.core.first.apply(null,[s__3083_1])),
((true)?(((((and__948__auto___3=clojure.core.keyword("","doc").apply(null,[clojure.core.meta.apply(null,[v_2])])),
((and__948__auto___3)?(((or__962__auto___4=clojure.core.re_find.apply(null,[clojure.core.re_matcher.apply(null,[re_2,clojure.core.keyword("","doc").apply(null,[clojure.core.meta.apply(null,[v_2])])])])),
((or__962__auto___4)?(or__962__auto___4):(clojure.core.re_find.apply(null,[clojure.core.re_matcher.apply(null,[re_2,clojure.core.str.apply(null,[clojure.core.keyword("","name").apply(null,[clojure.core.meta.apply(null,[v_2])])])])]))))):(and__948__auto___3))))?((new clojure.lang.LazyCons((function __clojure_core_fn_3076_find_doc_3078_iter_3080_3084_iter_3082_3085_fn_3087(G__3086_1){switch(arguments.length){
case 0:return (clojure.core.print_doc.apply(null,[v_2]))}
return (iter__3082_0.apply(null,[clojure.core.rest.apply(null,[s__3083_1])]))})))):((_cnt=1,_rtn=[clojure.core.rest.apply(null,[s__3083_1])],s__3083_1=_rtn[0])))):(null)))):(null))
}while(_cnt);return _rtn;})),
(fs__2895__auto___4=iterys__2894__auto___3.apply(null,[clojure.core.sort_by.apply(null,[clojure.core.comp.apply(null,[clojure.core.keyword("","name"),clojure.core.meta]),clojure.core.vals.apply(null,[clojure.core.ns_interns.apply(null,[ns_2])])])])),
((fs__2895__auto___4)?(((iter__2881__auto___5=(function __clojure_core_fn_3076_find_doc_3078_iter_3080_3084_iter_2881_auto_3092(coll__2882__auto___1){
var iter__2881__auto___0=arguments.callee;
return (((clojure.core.seq.apply(null,[coll__2882__auto___1]))?((new clojure.lang.LazyCons((function __clojure_core_fn_3076_find_doc_3078_iter_3080_3084_iter_2881_auto_3092_fn_3094(G__3093_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll__2882__auto___1]))}
return (iter__2881__auto___0.apply(null,[clojure.core.rest.apply(null,[coll__2882__auto___1])]))})))):(clojure.core.seq.apply(null,[iter__3080_0.apply(null,[clojure.core.rest.apply(null,[s__3081_1])])]))))})),
iter__2881__auto___5.apply(null,[fs__2895__auto___4]))):((_cnt=1,_rtn=[clojure.core.rest.apply(null,[s__3081_1])],s__3081_1=_rtn[0]))))):((_cnt=1,_rtn=[clojure.core.rest.apply(null,[s__3081_1])],s__3081_1=_rtn[0])))):(null)))):(null))
}while(_cnt);return _rtn;})),
iter__2896__auto___3.apply(null,[clojure.core.all_ns.apply(null,[])]))])))})))}).apply(null,[]);

//======
//(defn special-form-anchor "Returns the anchor tag on http://clojure.org/special_forms for the\n  special form x, or nil" [x] (#{(quote fn) (quote quote) (quote let) (quote var) (quote loop) (quote set!) (quote monitor-enter) (quote recur) (quote .) (quote do) (quote throw) (quote monitor-exit) (quote try) (quote if) (quote def) (quote new)} x))
//---
(function __clojure_core_fn_3102(){
return (clojure.JS.def(clojure.core,"special_form_anchor",(function __clojure_core_fn_3102_special_form_anchor_3104(x_1){
return (clojure.core.hash_set(clojure.core.symbol("fn"),clojure.core.symbol("quote"),clojure.core.symbol("let"),clojure.core.symbol("var"),clojure.core.symbol("loop"),clojure.core.symbol("set!"),clojure.core.symbol("monitor-enter"),clojure.core.symbol("recur"),clojure.core.symbol("."),clojure.core.symbol("do"),clojure.core.symbol("throw"),clojure.core.symbol("monitor-exit"),clojure.core.symbol("try"),clojure.core.symbol("if"),clojure.core.symbol("def"),clojure.core.symbol("new")).apply(null,[x_1]))})))}).apply(null,[]);

//======
//(defn syntax-symbol-anchor "Returns the anchor tag on http://clojure.org/special_forms for the\n  special form that uses syntax symbol x, or nil" [x] ({(quote &) (quote fn), (quote catch) (quote try), (quote finally) (quote try)} x))
//---
(function __clojure_core_fn_3108(){
return (clojure.JS.def(clojure.core,"syntax_symbol_anchor",(function __clojure_core_fn_3108_syntax_symbol_anchor_3110(x_1){
return (clojure.core.hash_map(clojure.core.symbol("&"),clojure.core.symbol("fn"),clojure.core.symbol("catch"),clojure.core.symbol("try"),clojure.core.symbol("finally"),clojure.core.symbol("try")).apply(null,[x_1]))})))}).apply(null,[]);

//======
//(defn print-special-doc [name type anchor] (println "-------------------------") (println name) (println type) (println (str "  Please see http://clojure.org/special_forms#" anchor)))
//---
(function __clojure_core_fn_3114(){
return (clojure.JS.def(clojure.core,"print_special_doc",(function __clojure_core_fn_3114_print_special_doc_3116(name_1,type_2,anchor_3){
return (clojure.core.println.apply(null,["-------------------------"]),
clojure.core.println.apply(null,[name_1]),
clojure.core.println.apply(null,[type_2]),
clojure.core.println.apply(null,[clojure.core.str.apply(null,["  Please see http://clojure.org/special_forms#",anchor_3])]))})))}).apply(null,[]);
// Skipping: (defmacro doc "Prints documentation for a var or special form given its name" [name] (cond (special-form-anchor name) (clojure.core/concat (clojure.core/list (quote clojure.core/print-special-doc)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list name))) (clojure.core/list "Special Form") (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/special-form-anchor)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list name)))))) (syntax-symbol-anchor name) (clojure.core/concat (clojure.core/list (quote clojure.core/print-special-doc)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list name))) (clojure.core/list "Syntax Symbol") (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/syntax-symbol-anchor)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list name)))))) :else (clojure.core/concat (clojure.core/list (quote clojure.core/print-doc)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote var)) (clojure.core/list name))))))

//======
//(defn tree-seq "returns a lazy sequence of the nodes in a tree, via a depth-first walk.\n  branch? must be a fn of one arg that returns true if passed a node\n  that can have children (but may not).  children must be a fn of one\n  arg that returns a sequence of the children. Will only be called on\n  nodes for which branch? returns true. Root is the root node of the\n  tree, must be a branch." [branch? children root] (let [walk (fn walk [nodes] (when-first [node nodes] (lazy-cons node (if (branch? node) (lazy-cat (walk (children node)) (walk (rest nodes))) (walk (rest nodes))))))] (lazy-cons root (walk (children root)))))
//---
(function __clojure_core_fn_3129(){
return (clojure.JS.def(clojure.core,"tree_seq",(function __clojure_core_fn_3129_tree_seq_3131(branch_QMARK__1,children_2,root_3){
var walk_4;
return (((walk_4=(function __clojure_core_fn_3129_tree_seq_3131_walk_3133(nodes_1){
var node_2,walk_0=arguments.callee;
return (((clojure.core.seq.apply(null,[nodes_1]))?(((node_2=clojure.core.first.apply(null,[nodes_1])),
(new clojure.lang.LazyCons((function __clojure_core_fn_3129_tree_seq_3131_walk_3133_fn_3135(G__3134_1){switch(arguments.length){
case 0:return (node_2)}
var iter__2881__auto___2;
return (((branch_QMARK__1.apply(null,[node_2]))?(((iter__2881__auto___2=(function __clojure_core_fn_3129_tree_seq_3131_walk_3133_fn_3135_iter_2881_auto_3138(coll__2882__auto___1){
var iter__2881__auto___0=arguments.callee;
return (((clojure.core.seq.apply(null,[coll__2882__auto___1]))?((new clojure.lang.LazyCons((function __clojure_core_fn_3129_tree_seq_3131_walk_3133_fn_3135_iter_2881_auto_3138_fn_3140(G__3139_1){switch(arguments.length){
case 0:return (clojure.core.first.apply(null,[coll__2882__auto___1]))}
return (iter__2881__auto___0.apply(null,[clojure.core.rest.apply(null,[coll__2882__auto___1])]))})))):(clojure.core.seq.apply(null,[walk_0.apply(null,[clojure.core.rest.apply(null,[nodes_1])])]))))})),
iter__2881__auto___2.apply(null,[walk_0.apply(null,[children_2.apply(null,[node_2])])]))):(walk_0.apply(null,[clojure.core.rest.apply(null,[nodes_1])]))))}))))):(null)))})),
(new clojure.lang.LazyCons((function __clojure_core_fn_3129_tree_seq_3131_fn_3148(G__3147_1){switch(arguments.length){
case 0:return (root_3)}
return (walk_4.apply(null,[children_2.apply(null,[root_3])]))})))))})))}).apply(null,[]);

//======
//(defn file-seq "A tree seq on java.io.Files" [dir] (tree-seq (fn [f] (. f (isDirectory))) (fn [d] (seq (. d (listFiles)))) dir))
//---
(function __clojure_core_fn_3154(){
return (clojure.JS.def(clojure.core,"file_seq",(function __clojure_core_fn_3154_file_seq_3156(dir_1){
return (clojure.core.tree_seq.apply(null,[(function __clojure_core_fn_3154_file_seq_3156_fn_3158(f_1){
return ((f_1).isDirectory())}),(function __clojure_core_fn_3154_file_seq_3156_fn_3161(d_1){
return (clojure.core.seq.apply(null,[(d_1).listFiles()]))}),dir_1]))})))}).apply(null,[]);

//======
//(defn xml-seq "A tree seq on the xml elements as per xml/parse" [root] (tree-seq (complement string?) (comp seq :content) root))
//---
(function __clojure_core_fn_3166(){
return (clojure.JS.def(clojure.core,"xml_seq",(function __clojure_core_fn_3166_xml_seq_3168(root_1){
return (clojure.core.tree_seq.apply(null,[clojure.core.complement.apply(null,[clojure.core.string_QMARK_]),clojure.core.comp.apply(null,[clojure.core.seq,clojure.core.keyword("","content")]),root_1]))})))}).apply(null,[]);
// Skipping: (defn special-symbol? "Returns true if s names a special form" [s] (contains? (. clojure.lang.Compiler specials) s))

//======
//(defn var? "Returns true if v is of type clojure.lang.Var" [v] (instance? clojure.lang.Var v))
//---
(function __clojure_core_fn_3178(){
return (clojure.JS.def(clojure.core,"var_QMARK_",(function __clojure_core_fn_3178_var_QMARK_3180(v_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Var,v_1]))})))}).apply(null,[]);
// Skipping: (defn slurp "Reads the file named by f into a string and returns it." [f] (with-open [r (new java.io.BufferedReader (new java.io.FileReader f))] (let [sb (RT/makeStringBuilder)] (loop [c (. r (read))] (if (neg? c) (str sb) (do (. sb (append (char c))) (recur (. r (read)))))))))

//======
//(defn subs "Returns the substring of s beginning at start inclusive, and ending\n  at end (defaults to length of string), exclusive." ([s start] (. s (substring start))) ([s start end] (. s (substring start end))))
//---
(function __clojure_core_fn_3190(){
return (clojure.JS.def(clojure.core,"subs",(function __clojure_core_fn_3190_subs_3192(s_1,start_2,end_3){switch(arguments.length){
case 2:return ((s_1).substring(start_2))}
return ((s_1).substring(start_2,end_3))})))}).apply(null,[]);

//======
//(defn max-key "Returns the x for which (k x), a number, is greatest." ([k x] x) ([k x y] (if (> (k x) (k y)) x y)) ([k x y & more] (reduce (fn* [p1__3197 p2__3198] (max-key k p1__3197 p2__3198)) (max-key k x y) more)))
//---
(function __clojure_core_fn_3199(){
return (clojure.JS.def(clojure.core,"max_key",clojure.JS.variadic(3,(function __clojure_core_fn_3199_max_key_3201(k_1,x_2,y_3){switch(arguments.length){
case 2:return (x_2)
case 3:return (((clojure.lang.Numbers.gt(k_1.apply(null,[x_2]),k_1.apply(null,[y_3])))?(x_2):(y_3)))}
var more_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.core.reduce.apply(null,[(function __clojure_core_fn_3199_max_key_3201_fn_3205(p1__3197_1,p2__3198_2){
return (clojure.core.max_key.apply(null,[k_1,p1__3197_1,p2__3198_2]))}),clojure.core.max_key.apply(null,[k_1,x_2,y_3]),more_4]))}))))}).apply(null,[]);

//======
//(defn min-key "Returns the x for which (k x), a number, is least." ([k x] x) ([k x y] (if (< (k x) (k y)) x y)) ([k x y & more] (reduce (fn* [p1__3210 p2__3211] (min-key k p1__3210 p2__3211)) (min-key k x y) more)))
//---
(function __clojure_core_fn_3212(){
return (clojure.JS.def(clojure.core,"min_key",clojure.JS.variadic(3,(function __clojure_core_fn_3212_min_key_3214(k_1,x_2,y_3){switch(arguments.length){
case 2:return (x_2)
case 3:return (((clojure.lang.Numbers.lt(k_1.apply(null,[x_2]),k_1.apply(null,[y_3])))?(x_2):(y_3)))}
var more_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.core.reduce.apply(null,[(function __clojure_core_fn_3212_min_key_3214_fn_3218(p1__3210_1,p2__3211_2){
return (clojure.core.min_key.apply(null,[k_1,p1__3210_1,p2__3211_2]))}),clojure.core.min_key.apply(null,[k_1,x_2,y_3]),more_4]))}))))}).apply(null,[]);

//======
//(defn distinct "Returns a lazy sequence of the elements of coll with duplicates removed" [coll] (let [step (fn step [[f & r :as xs] seen] (when xs (if (seen f) (recur r seen) (lazy-cons f (step r (conj seen f))))))] (step (seq coll) #{})))
//---
(function __clojure_core_fn_3223(){
return (clojure.JS.def(clojure.core,"distinct",(function __clojure_core_fn_3223_distinct_3225(coll_1){
var step_2;
return (((step_2=(function __clojure_core_fn_3223_distinct_3225_step_3228(p__3227_1,seen_2){
var _cnt,_rtn,vec__3229_3,f_4,r_5,xs_6,step_0=arguments.callee;
do{_cnt=0;_rtn=((vec__3229_3=p__3227_1),
(f_4=clojure.core.nth.apply(null,[vec__3229_3,(0),null])),
(r_5=clojure.core.nthrest.apply(null,[vec__3229_3,(1)])),
(xs_6=vec__3229_3),
((xs_6)?(((seen_2.apply(null,[f_4]))?((_cnt=1,_rtn=[r_5,seen_2],p__3227_1=_rtn[0],seen_2=_rtn[1])):((new clojure.lang.LazyCons((function __clojure_core_fn_3223_distinct_3225_step_3228_fn_3231(G__3230_1){switch(arguments.length){
case 0:return (f_4)}
return (step_0.apply(null,[r_5,clojure.core.conj.apply(null,[seen_2,f_4])]))})))))):(null)))
}while(_cnt);return _rtn;})),
step_2.apply(null,[clojure.core.seq.apply(null,[coll_1]),clojure.lang.PersistentHashSet.EMPTY])))})))}).apply(null,[]);
// Skipping: (defmacro if-let "bindings => binding-form test\n\n  If test is true, evaluates then with binding-form bound to the value of test, if not, yields else" ([bindings then] (clojure.core/concat (clojure.core/list (quote clojure.core/if-let)) (clojure.core/list bindings) (clojure.core/list then) (clojure.core/list (quote nil)))) ([bindings then else & oldform] (assert-args if-let (and (vector? bindings) (nil? oldform)) "a vector for its binding" (= 2 (count bindings)) "exactly 2 forms in binding vector") (let [[form tst] bindings] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote temp__3238__auto__)) (clojure.core/list tst)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (quote temp__3238__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list form) (clojure.core/list (quote temp__3238__auto__))))) (clojure.core/list then))) (clojure.core/list else)))))))
// Skipping: (defmacro when-let "bindings => binding-form test\n\n  When test is true, evaluates body with binding-form bound to the value of test" [bindings & body] (assert-args when-let (vector? bindings) "a vector for its binding" (= 2 (count bindings)) "exactly 2 forms in binding vector") (let [[form tst] bindings] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote temp__3253__auto__)) (clojure.core/list tst)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list (quote temp__3253__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list form) (clojure.core/list (quote temp__3253__auto__))))) body)))))))

//======
//(defn replace "Given a map of replacement pairs and a vector/collection, returns a\n  vector/seq with any elements = a key in smap replaced with the\n  corresponding val in smap" [smap coll] (if (vector? coll) (reduce (fn [v i] (if-let [e (find smap (nth v i))] (assoc v i (val e)) v)) coll (range (count coll))) (map (fn* [p1__3265] (if-let [e (find smap p1__3265)] (val e) p1__3265)) coll)))
//---
(function __clojure_core_fn_3266(){
return (clojure.JS.def(clojure.core,"replace",(function __clojure_core_fn_3266_replace_3268(smap_1,coll_2){
return (((clojure.core.vector_QMARK_.apply(null,[coll_2]))?(clojure.core.reduce.apply(null,[(function __clojure_core_fn_3266_replace_3268_fn_3270(v_1,i_2){
var temp__3238__auto___3,e_4;
return (((temp__3238__auto___3=clojure.core.find.apply(null,[smap_1,clojure.core.nth.apply(null,[v_1,i_2])])),
((temp__3238__auto___3)?(((e_4=temp__3238__auto___3),
clojure.core.assoc.apply(null,[v_1,i_2,clojure.core.val.apply(null,[e_4])]))):(v_1))))}),coll_2,clojure.core.range.apply(null,[clojure.core.count.apply(null,[coll_2])])])):(clojure.core.map.apply(null,[(function __clojure_core_fn_3266_replace_3268_fn_3273(p1__3265_1){
var temp__3238__auto___2,e_3;
return (((temp__3238__auto___2=clojure.core.find.apply(null,[smap_1,p1__3265_1])),
((temp__3238__auto___2)?(((e_3=temp__3238__auto___2),
clojure.core.val.apply(null,[e_3]))):(p1__3265_1))))}),coll_2]))))})))}).apply(null,[]);
// Skipping: (defmacro dosync "Runs the exprs (in an implicit do) in a transaction that encompasses\n  exprs and any nested calls.  Starts a transaction if none is already\n  running on this thread. Any uncaught exception will abort the\n  transaction and flow out of dosync. The exprs may be run more than\n  once, but any effects on Refs will be atomic." [& exprs] (clojure.core/concat (clojure.core/list (quote clojure.core/sync)) (clojure.core/list (quote nil)) exprs))
// Skipping: (defmacro with-precision "Sets the precision and rounding mode to be used for BigDecimal operations.\n\n  Usage: (with-precision 10 (/ 1M 3))\n  or:    (with-precision 10 :rounding HALF_DOWN (/ 1M 3))\n  \n  The rounding mode is one of CEILING, FLOOR, HALF_UP, HALF_DOWN,\n  HALF_EVEN, UP, DOWN and UNNECESSARY; it defaults to HALF_UP." [precision & exprs] (let [[body rm] (if (= (first exprs) :rounding) [(rest (rest exprs)) (clojure.core/concat (clojure.core/list (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote java.math.RoundingMode)) (clojure.core/list (second exprs)))))] [exprs nil])] (clojure.core/concat (clojure.core/list (quote clojure.core/binding)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote clojure.core/*math-context*)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote java.math.MathContext.)) (clojure.core/list precision) rm))))) body)))

//======
//(defn bound-fn {:private true} [sc test key] (fn [e] (test (.. sc comparator (compare (. sc entryKey e) key)) 0)))
//---
(function __clojure_core_fn_3298(){
return (clojure.JS.def(clojure.core,"bound_fn",(function __clojure_core_fn_3298_bound_fn_3300(sc_1,test_2,key_3){
return ((function __clojure_core_fn_3298_bound_fn_3300_fn_3302(e_1){
return (test_2.apply(null,[((sc_1).comparator()).compare((sc_1).entryKey(e_1),key_3),(0)]))}))})))}).apply(null,[]);

//======
//(defn subseq "sc must be a sorted collection, test(s) one of <, <=, > or\n  >=. Returns a seq of those entries with keys ek for\n  which (test (.. sc comparator (compare ek key)) 0) is true" ([sc test key] (let [include (bound-fn sc test key)] (if (#{> >=} test) (when-let [[e :as s] (. sc seqFrom key true)] (if (include e) s (rest s))) (take-while include (. sc seq true))))) ([sc start-test start-key end-test end-key] (when-let [[e :as s] (. sc seqFrom start-key true)] (take-while (bound-fn sc end-test end-key) (if ((bound-fn sc start-test start-key) e) s (rest s))))))
//---
(function __clojure_core_fn_3307(){
return (clojure.JS.def(clojure.core,"subseq",(function __clojure_core_fn_3307_subseq_3309(sc_1,start_test_2,start_key_3,end_test_4,end_key_5){switch(arguments.length){
case 3:var include_4,temp__3253__auto___5,vec__3311_6,e_7,s_8,test_2=arguments[1],key_3=arguments[2];
return (((include_4=clojure.core.bound_fn.apply(null,[sc_1,test_2,key_3])),
((clojure.core.hash_set(clojure.core._GT_,clojure.core._GT__EQ_).apply(null,[test_2]))?(((temp__3253__auto___5=(sc_1).seqFrom(key_3,true)),
((temp__3253__auto___5)?(((vec__3311_6=temp__3253__auto___5),
(e_7=clojure.core.nth.apply(null,[vec__3311_6,(0),null])),
(s_8=vec__3311_6),
((include_4.apply(null,[e_7]))?(s_8):(clojure.core.rest.apply(null,[s_8]))))):(null)))):(clojure.core.take_while.apply(null,[include_4,(sc_1).seq(true)])))))}
var e_8,s_9,temp__3253__auto___6,vec__3313_7;
return (((temp__3253__auto___6=(sc_1).seqFrom(start_key_3,true)),
((temp__3253__auto___6)?(((vec__3313_7=temp__3253__auto___6),
(e_8=clojure.core.nth.apply(null,[vec__3313_7,(0),null])),
(s_9=vec__3313_7),
clojure.core.take_while.apply(null,[clojure.core.bound_fn.apply(null,[sc_1,end_test_4,end_key_5]),((clojure.core.bound_fn.apply(null,[sc_1,start_test_2,start_key_3]).apply(null,[e_8]))?(s_9):(clojure.core.rest.apply(null,[s_9])))]))):(null))))})))}).apply(null,[]);

//======
//(defn rsubseq "sc must be a sorted collection, test(s) one of <, <=, > or\n  >=. Returns a reverse seq of those entries with keys ek for\n  which (test (.. sc comparator (compare ek key)) 0) is true" ([sc test key] (let [include (bound-fn sc test key)] (if (#{< <=} test) (when-let [[e :as s] (. sc seqFrom key false)] (if (include e) s (rest s))) (take-while include (. sc seq false))))) ([sc start-test start-key end-test end-key] (when-let [[e :as s] (. sc seqFrom end-key false)] (take-while (bound-fn sc start-test start-key) (if ((bound-fn sc end-test end-key) e) s (rest s))))))
//---
(function __clojure_core_fn_3316(){
return (clojure.JS.def(clojure.core,"rsubseq",(function __clojure_core_fn_3316_rsubseq_3318(sc_1,start_test_2,start_key_3,end_test_4,end_key_5){switch(arguments.length){
case 3:var include_4,temp__3253__auto___5,vec__3320_6,e_7,s_8,test_2=arguments[1],key_3=arguments[2];
return (((include_4=clojure.core.bound_fn.apply(null,[sc_1,test_2,key_3])),
((clojure.core.hash_set(clojure.core._LT_,clojure.core._LT__EQ_).apply(null,[test_2]))?(((temp__3253__auto___5=(sc_1).seqFrom(key_3,false)),
((temp__3253__auto___5)?(((vec__3320_6=temp__3253__auto___5),
(e_7=clojure.core.nth.apply(null,[vec__3320_6,(0),null])),
(s_8=vec__3320_6),
((include_4.apply(null,[e_7]))?(s_8):(clojure.core.rest.apply(null,[s_8]))))):(null)))):(clojure.core.take_while.apply(null,[include_4,(sc_1).seq(false)])))))}
var vec__3322_7,temp__3253__auto___6,e_8,s_9;
return (((temp__3253__auto___6=(sc_1).seqFrom(end_key_5,false)),
((temp__3253__auto___6)?(((vec__3322_7=temp__3253__auto___6),
(e_8=clojure.core.nth.apply(null,[vec__3322_7,(0),null])),
(s_9=vec__3322_7),
clojure.core.take_while.apply(null,[clojure.core.bound_fn.apply(null,[sc_1,start_test_2,start_key_3]),((clojure.core.bound_fn.apply(null,[sc_1,end_test_4,end_key_5]).apply(null,[e_8]))?(s_9):(clojure.core.rest.apply(null,[s_9])))]))):(null))))})))}).apply(null,[]);

//======
//(defn repeatedly "Takes a function of no args, presumably with side effects, and returns an infinite\n  lazy sequence of calls to it" [f] (lazy-cons (f) (repeatedly f)))
//---
(function __clojure_core_fn_3325(){
return (clojure.JS.def(clojure.core,"repeatedly",(function __clojure_core_fn_3325_repeatedly_3327(f_1){
return ((new clojure.lang.LazyCons((function __clojure_core_fn_3325_repeatedly_3327_fn_3330(G__3329_1){switch(arguments.length){
case 0:return (f_1.apply(null,[]))}
return (clojure.core.repeatedly.apply(null,[f_1]))}))))})))}).apply(null,[]);

//======
//(defn add-classpath "Adds the url (String or URL object) to the classpath per URLClassLoader.addURL" [url] (. clojure.lang.RT addURL url))
//---
(function __clojure_core_fn_3336(){
return (clojure.JS.def(clojure.core,"add_classpath",(function __clojure_core_fn_3336_add_classpath_3338(url_1){
return (clojure.lang.RT.addURL(url_1))})))}).apply(null,[]);

//======
//(defn hash "Returns the hash code of its argument" [x] (. clojure.lang.Util (hash x)))
//---
(function __clojure_core_fn_3342(){
return (clojure.JS.def(clojure.core,"hash",(function __clojure_core_fn_3342_hash_3344(x_1){
return (clojure.lang.Util.hash(x_1))})))}).apply(null,[]);

//======
//(defn interpose "Returns a lazy seq of the elements of coll separated by sep" [sep coll] (drop 1 (interleave (repeat sep) coll)))
//---
(function __clojure_core_fn_3348(){
return (clojure.JS.def(clojure.core,"interpose",(function __clojure_core_fn_3348_interpose_3350(sep_1,coll_2){
return (clojure.core.drop.apply(null,[(1),clojure.core.interleave.apply(null,[clojure.core.repeat.apply(null,[sep_1]),coll_2])]))})))}).apply(null,[]);
// Skipping: (defmacro definline "Experimental - like defmacro, except defines a named function whose\n  body is the expansion, calls to which may be expanded inline as if\n  it were a macro. Cannot be used with variadic (&) args." [name & decl] (let [[args expr] (drop-while (comp not vector?) decl) inline (eval (list (quote clojure.core/fn) args expr))] (clojure.core/concat (clojure.core/list (quote do)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/defn)) (clojure.core/list name) (clojure.core/list args) (clojure.core/list (apply inline args)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote v__3354__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote var)) (clojure.core/list name)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .setMeta)) (clojure.core/list (quote v__3354__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/assoc)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/meta)) (clojure.core/list (quote v__3354__auto__)))) (clojure.core/list :inline) (clojure.core/list inline))))))))))

//======
//(defn empty "Returns an empty collection of the same category as coll, or nil" [coll] (.empty coll))
//---
(function __clojure_core_fn_3366(){
return (clojure.JS.def(clojure.core,"empty",(function __clojure_core_fn_3366_empty_3368(coll_1){
return ((coll_1).empty())})))}).apply(null,[]);
// Skipping: (defmacro amap "Maps an expression across an array a, using an index named idx, and\n  return value named ret, initialized to a clone of a, then setting each element of\n  ret to the evaluation of expr, returning the new array ret." [a idx ret expr] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote a__3372__auto__)) (clojure.core/list a) (clojure.core/list ret) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/aclone)) (clojure.core/list (quote a__3372__auto__))))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/loop)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list idx) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/int)) (clojure.core/list 0)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/<)) (clojure.core/list idx) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/alength)) (clojure.core/list (quote a__3372__auto__)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote do)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/aset)) (clojure.core/list ret) (clojure.core/list idx) (clojure.core/list expr))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote recur)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked-inc)) (clojure.core/list idx))))))) (clojure.core/list ret)))))))
// Skipping: (defmacro areduce "Reduces an expression across an array a, using an index named idx,\n  and return value named ret, initialized to init, setting ret to the evaluation of expr at\n  each step, returning ret." [a idx ret init expr] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote a__3382__auto__)) (clojure.core/list a)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/loop)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list idx) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/int)) (clojure.core/list 0))) (clojure.core/list ret) (clojure.core/list init)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/<)) (clojure.core/list idx) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/alength)) (clojure.core/list (quote a__3382__auto__)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote recur)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/unchecked-inc)) (clojure.core/list idx))) (clojure.core/list expr))) (clojure.core/list ret)))))))
// Skipping: (defn float-array "Creates an array of floats" {:inline (fn [& args] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/float_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers float_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers float_array size init-val-or-seq)))
// Skipping: (defn double-array "Creates an array of doubles" {:inline (fn [& args] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/double_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers double_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers double_array size init-val-or-seq)))
// Skipping: (defn int-array "Creates an array of ints" {:inline (fn [& args] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/int_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers int_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers int_array size init-val-or-seq)))
// Skipping: (defn long-array "Creates an array of ints" {:inline (fn [& args] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/long_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers long_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers long_array size init-val-or-seq)))
// Skipping: (definline floats "Casts to float[]" [xs] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/floats)) (clojure.core/list xs)))
// Skipping: (definline ints "Casts to int[]" [xs] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/ints)) (clojure.core/list xs)))
// Skipping: (definline doubles "Casts to double[]" [xs] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/doubles)) (clojure.core/list xs)))
// Skipping: (definline longs "Casts to long[]" [xs] (clojure.core/concat (clojure.core/list (quote .)) (clojure.core/list (quote clojure.lang.Numbers)) (clojure.core/list (quote clojure.core/longs)) (clojure.core/list xs)))

//======
//(import (quote (java.util.concurrent BlockingQueue LinkedBlockingQueue)))
//---
(function __clojure_core_fn_3432(){
return (clojure.core.import_.apply(null,[clojure.JS.lit_list([clojure.core.symbol("java.util.concurrent"),clojure.core.symbol("BlockingQueue"),clojure.core.symbol("LinkedBlockingQueue")])]))}).apply(null,[]);
// Skipping: (defn seque "Creates a queued seq on another (presumably lazy) seq s. The queued\n  seq will produce a concrete seq in the background, and can get up to\n  n items ahead of the consumer. n-or-q can be an integer n buffer\n  size, or an instance of java.util.concurrent BlockingQueue. Note\n  that reading from a seque can block if the reader gets ahead of the\n  producer." ([s] (seque 100 s)) ([n-or-q s] (let [q (if (instance? BlockingQueue n-or-q) n-or-q (LinkedBlockingQueue. (int n-or-q))) NIL (Object.) agt (agent (seq s)) fill (fn [s] (try (loop [[x & xs :as s] s] (if s (if (.offer q (if (nil? x) NIL x)) (recur xs) s) (.put q q))) (catch Exception e (.put q q) (throw e)))) drain (fn drain [] (let [x (.take q)] (if (identical? x q) (clojure.core/deref agt) (do (send-off agt fill) (lazy-cons (if (identical? x NIL) nil x) (drain))))))] (send-off agt fill) (drain))))

//======
//(defn alter-var-root "Atomically alters the root binding of var v by applying f to its\n  current value plus any args" [v f & args] (.alterRoot v f args))
//---
(function __clojure_core_fn_3456(){
return (clojure.JS.def(clojure.core,"alter_var_root",clojure.JS.variadic(2,(function __clojure_core_fn_3456_alter_var_root_3458(v_1,f_2){
var args_3=clojure.JS.rest_args(this,arguments,2);
return ((v_1).alterRoot(f_2,args_3))}))))}).apply(null,[]);

//======
//(defn make-hierarchy "Creates a hierarchy object for use with derive, isa? etc." [] {:parents {}, :descendants {}, :ancestors {}})
//---
(function __clojure_core_fn_3462(){
return (clojure.JS.def(clojure.core,"make_hierarchy",(function __clojure_core_fn_3462_make_hierarchy_3464(){
return (clojure.core.hash_map(clojure.core.keyword("","parents"),clojure.lang.PersistentArrayMap.EMPTY,clojure.core.keyword("","descendants"),clojure.lang.PersistentArrayMap.EMPTY,clojure.core.keyword("","ancestors"),clojure.lang.PersistentArrayMap.EMPTY))})))}).apply(null,[]);

//======
//(def global-hierarchy (make-hierarchy))
//---
(function __clojure_core_fn_3468(){
return (clojure.JS.def(clojure.core,"global_hierarchy",clojure.core.make_hierarchy.apply(null,[])))}).apply(null,[]);

//======
//(defn not-empty "If coll is empty, returns nil, else coll" [coll] (when (seq coll) coll))
//---
(function __clojure_core_fn_3471(){
return (clojure.JS.def(clojure.core,"not_empty",(function __clojure_core_fn_3471_not_empty_3473(coll_1){
return (((clojure.core.seq.apply(null,[coll_1]))?(coll_1):(null)))})))}).apply(null,[]);

//======
//(defn bases "Returns the immediate superclass and direct interfaces of c, if any" [c] (let [i (.getInterfaces c) s (.getSuperclass c)] (not-empty (if s (cons s i) i))))
//---
(function __clojure_core_fn_3477(){
return (clojure.JS.def(clojure.core,"bases",(function __clojure_core_fn_3477_bases_3479(c_1){
var i_2,s_3;
return (((i_2=(c_1).getInterfaces()),
(s_3=(c_1).getSuperclass()),
clojure.core.not_empty.apply(null,[((s_3)?(clojure.core.cons.apply(null,[s_3,i_2])):(i_2))])))})))}).apply(null,[]);

//======
//(defn supers "Returns the immediate and indirect superclasses and interfaces of c, if any" [class] (loop [ret (set (bases class)) cs ret] (if (seq cs) (let [c (first cs) bs (bases c)] (recur (into ret bs) (into (disj cs c) bs))) (not-empty ret))))
//---
(function __clojure_core_fn_3483(){
return (clojure.JS.def(clojure.core,"supers",(function __clojure_core_fn_3483_supers_3485(class_1){
var ret_2,cs_3,c_4,bs_5;
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.core.set.apply(null,[clojure.core.bases.apply(null,[class_1])])),
(cs_3=ret_2);do{_cnt=0;
_rtn=((clojure.core.seq.apply(null,[cs_3]))?(((c_4=clojure.core.first.apply(null,[cs_3])),
(bs_5=clojure.core.bases.apply(null,[c_4])),
(_cnt=1,_rtn=[clojure.core.into.apply(null,[ret_2,bs_5]),clojure.core.into.apply(null,[clojure.core.disj.apply(null,[cs_3,c_4]),bs_5])],ret_2=_rtn[0],cs_3=_rtn[1]))):(clojure.core.not_empty.apply(null,[ret_2])))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn isa? "Returns true if (= child parent), or child is directly or indirectly derived from\n  parent, either via a Java type inheritance relationship or a\n  relationship established via derive. h must be a hierarchy obtained\n  from make-hierarchy, if not supplied defaults to the global\n  hierarchy" ([child parent] (isa? global-hierarchy child parent)) ([h child parent] (or (= child parent) (and (class? parent) (class? child) (. parent isAssignableFrom child)) (contains? ((:ancestors h) child) parent) (and (class? child) (some (fn* [p1__3489] (contains? ((:ancestors h) p1__3489) parent)) (supers child))) (and (vector? parent) (vector? child) (= (count parent) (count child)) (loop [ret true i 0] (if (or (not ret) (= i (count parent))) ret (recur (isa? h (child i) (parent i)) (inc i))))))))
//---
(function __clojure_core_fn_3490(){
return (clojure.JS.def(clojure.core,"isa_QMARK_",(function __clojure_core_fn_3490_isa_QMARK_3492(h_1,child_2,parent_3){switch(arguments.length){
case 2:var child_1=arguments[0],parent_2=arguments[1];
return (clojure.core.isa_QMARK_.apply(null,[clojure.core.global_hierarchy,child_1,parent_2]))}
var or__962__auto___6,i_12,and__948__auto___7,and__948__auto___5,and__948__auto___10,or__962__auto___13,or__962__auto___7,and__948__auto___9,and__948__auto___6,and__948__auto___8,or__962__auto___5,ret_11,or__962__auto___4;
return (((or__962__auto___4=clojure.lang.Util.equiv(child_2,parent_3)),
((or__962__auto___4)?(or__962__auto___4):(((or__962__auto___5=((and__948__auto___5=clojure.core.class_QMARK_.apply(null,[parent_3])),
((and__948__auto___5)?(((and__948__auto___6=clojure.core.class_QMARK_.apply(null,[child_2])),
((and__948__auto___6)?((parent_3).isAssignableFrom(child_2)):(and__948__auto___6)))):(and__948__auto___5)))),
((or__962__auto___5)?(or__962__auto___5):(((or__962__auto___6=clojure.core.contains_QMARK_.apply(null,[clojure.core.keyword("","ancestors").apply(null,[h_1]).apply(null,[child_2]),parent_3])),
((or__962__auto___6)?(or__962__auto___6):(((or__962__auto___7=((and__948__auto___7=clojure.core.class_QMARK_.apply(null,[child_2])),
((and__948__auto___7)?(clojure.core.some.apply(null,[(function __clojure_core_fn_3490_isa_QMARK_3492_fn_3495(p1__3489_1){
return (clojure.core.contains_QMARK_.apply(null,[clojure.core.keyword("","ancestors").apply(null,[h_1]).apply(null,[p1__3489_1]),parent_3]))}),clojure.core.supers.apply(null,[child_2])])):(and__948__auto___7)))),
((or__962__auto___7)?(or__962__auto___7):(((and__948__auto___8=clojure.core.vector_QMARK_.apply(null,[parent_3])),
((and__948__auto___8)?(((and__948__auto___9=clojure.core.vector_QMARK_.apply(null,[child_2])),
((and__948__auto___9)?(((and__948__auto___10=clojure.lang.Util.equiv(clojure.core.count.apply(null,[parent_3]),clojure.core.count.apply(null,[child_2]))),
((and__948__auto___10)?(((function __loop(){var _rtn,_cnt;(ret_11=true),
(i_12=(0));do{_cnt=0;
_rtn=((((or__962__auto___13=clojure.core.not.apply(null,[ret_11])),
((or__962__auto___13)?(or__962__auto___13):(clojure.lang.Util.equiv(i_12,clojure.core.count.apply(null,[parent_3]))))))?(ret_11):((_cnt=1,_rtn=[clojure.core.isa_QMARK_.apply(null,[h_1,child_2.apply(null,[i_12]),parent_3.apply(null,[i_12])]),clojure.lang.Numbers.inc(i_12)],ret_11=_rtn[0],i_12=_rtn[1])))}while(_cnt);return _rtn;})())):(and__948__auto___10)))):(and__948__auto___9)))):(and__948__auto___8))))))))))))))))})))}).apply(null,[]);

//======
//(defn parents "Returns the immediate parents of tag, either via a Java type\n  inheritance relationship or a relationship established via derive. h\n  must be a hierarchy obtained from make-hierarchy, if not supplied\n  defaults to the global hierarchy" ([tag] (parents global-hierarchy tag)) ([h tag] (not-empty (let [tp (get (:parents h) tag)] (if (class? tag) (into (set (bases tag)) tp) tp)))))
//---
(function __clojure_core_fn_3500(){
return (clojure.JS.def(clojure.core,"parents",(function __clojure_core_fn_3500_parents_3502(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.core.parents.apply(null,[clojure.core.global_hierarchy,tag_1]))}
var tp_3;
return (clojure.core.not_empty.apply(null,[((tp_3=clojure.core.get.apply(null,[clojure.core.keyword("","parents").apply(null,[h_1]),tag_2])),
((clojure.core.class_QMARK_.apply(null,[tag_2]))?(clojure.core.into.apply(null,[clojure.core.set.apply(null,[clojure.core.bases.apply(null,[tag_2])]),tp_3])):(tp_3)))]))})))}).apply(null,[]);

//======
//(defn ancestors "Returns the immediate and indirect parents of tag, either via a Java type\n  inheritance relationship or a relationship established via derive. h\n  must be a hierarchy obtained from make-hierarchy, if not supplied\n  defaults to the global hierarchy" ([tag] (ancestors global-hierarchy tag)) ([h tag] (not-empty (let [ta (get (:ancestors h) tag)] (if (class? tag) (into (set (supers tag)) ta) ta)))))
//---
(function __clojure_core_fn_3507(){
return (clojure.JS.def(clojure.core,"ancestors",(function __clojure_core_fn_3507_ancestors_3509(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.core.ancestors.apply(null,[clojure.core.global_hierarchy,tag_1]))}
var ta_3;
return (clojure.core.not_empty.apply(null,[((ta_3=clojure.core.get.apply(null,[clojure.core.keyword("","ancestors").apply(null,[h_1]),tag_2])),
((clojure.core.class_QMARK_.apply(null,[tag_2]))?(clojure.core.into.apply(null,[clojure.core.set.apply(null,[clojure.core.supers.apply(null,[tag_2])]),ta_3])):(ta_3)))]))})))}).apply(null,[]);

//======
//(defn descendants "Returns the immediate and indirect children of tag, through a\n  relationship established via derive. h must be a hierarchy obtained\n  from make-hierarchy, if not supplied defaults to the global\n  hierarchy. Note: does not work on Java type inheritance\n  relationships." ([tag] (descendants global-hierarchy tag)) ([h tag] (if (class? tag) (throw (RT/makeUnsupportedException "Can't get descendants of classes")) (not-empty (get (:descendants h) tag)))))
//---
(function __clojure_core_fn_3514(){
return (clojure.JS.def(clojure.core,"descendants",(function __clojure_core_fn_3514_descendants_3516(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.core.descendants.apply(null,[clojure.core.global_hierarchy,tag_1]))}
return (((clojure.core.class_QMARK_.apply(null,[tag_2]))?((function __throw(){throw clojure.lang.RT.makeUnsupportedException("Can't get descendants of classes")})()):(clojure.core.not_empty.apply(null,[clojure.core.get.apply(null,[clojure.core.keyword("","descendants").apply(null,[h_1]),tag_2])]))))})))}).apply(null,[]);

//======
//(defn derive "Establishes a parent/child relationship between parent and\n  tag. Parent must be a namespace-qualified symbol or keyword and\n  child can be either a namespace-qualified symbol or keyword or a\n  class. h must be a hierarchy obtained from make-hierarchy, if not\n  supplied defaults to, and modifies, the global hierarchy." ([tag parent] (assert (namespace parent)) (assert (or (class? tag) (and (instance? clojure.lang.Named tag) (namespace tag)))) (alter-var-root (var global-hierarchy) derive tag parent) nil) ([h tag parent] (assert (not= tag parent)) (assert (or (class? tag) (instance? clojure.lang.Named tag))) (assert (instance? clojure.lang.Named parent)) (let [tp (:parents h) td (:descendants h) ta (:ancestors h) tf (fn [m source sources target targets] (reduce (fn [ret k] (assoc ret k (reduce conj (get targets k #{}) (cons target (targets target))))) m (cons source (sources source))))] (or (when-not (contains? (tp tag) parent) (when (contains? (ta tag) parent) (throw (RT/makeException (print-str tag "already has" parent "as ancestor")))) (when (contains? (ta parent) tag) (throw (RT/makeException (print-str "Cyclic derivation:" parent "has" tag "as ancestor")))) {:parents (assoc (:parents h) tag (conj (get tp tag #{}) parent)), :ancestors (tf (:ancestors h) tag td parent ta), :descendants (tf (:descendants h) parent ta tag td)}) h))))
//---
(function __clojure_core_fn_3521(){
return (clojure.JS.def(clojure.core,"derive",(function __clojure_core_fn_3521_derive_3523(h_1,tag_2,parent_3){switch(arguments.length){
case 2:var or__962__auto___3,and__948__auto___4,tag_1=arguments[0],parent_2=arguments[1];
return (((clojure.core.namespace.apply(null,[parent_2]))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["Assert failed: ",clojure.core.pr_str.apply(null,[clojure.JS.lit_list([clojure.core.symbol("namespace"),clojure.core.symbol("parent")])])]))})())),
((((or__962__auto___3=clojure.core.class_QMARK_.apply(null,[tag_1])),
((or__962__auto___3)?(or__962__auto___3):(((and__948__auto___4=clojure.core.instance_QMARK_.apply(null,[clojure.lang.Named,tag_1])),
((and__948__auto___4)?(clojure.core.namespace.apply(null,[tag_1])):(and__948__auto___4)))))))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["Assert failed: ",clojure.core.pr_str.apply(null,[clojure.JS.lit_list([clojure.core.symbol("or"),clojure.JS.lit_list([clojure.core.symbol("class?"),clojure.core.symbol("tag")]),clojure.JS.lit_list([clojure.core.symbol("and"),clojure.JS.lit_list([clojure.core.symbol("instance?"),clojure.core.symbol("clojure.lang.Named"),clojure.core.symbol("tag")]),clojure.JS.lit_list([clojure.core.symbol("namespace"),clojure.core.symbol("tag")])])])])]))})())),
clojure.core.alter_var_root.apply(null,[clojure.core._var_global_hierarchy,clojure.core.derive,tag_1,parent_2]),
null)}
var tf_7,ta_6,tp_4,or__962__auto___4,td_5,or__962__auto___8;
return (((clojure.core.not_EQ_.apply(null,[tag_2,parent_3]))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["Assert failed: ",clojure.core.pr_str.apply(null,[clojure.JS.lit_list([clojure.core.symbol("not="),clojure.core.symbol("tag"),clojure.core.symbol("parent")])])]))})())),
((((or__962__auto___4=clojure.core.class_QMARK_.apply(null,[tag_2])),
((or__962__auto___4)?(or__962__auto___4):(clojure.core.instance_QMARK_.apply(null,[clojure.lang.Named,tag_2])))))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["Assert failed: ",clojure.core.pr_str.apply(null,[clojure.JS.lit_list([clojure.core.symbol("or"),clojure.JS.lit_list([clojure.core.symbol("class?"),clojure.core.symbol("tag")]),clojure.JS.lit_list([clojure.core.symbol("instance?"),clojure.core.symbol("clojure.lang.Named"),clojure.core.symbol("tag")])])])]))})())),
((clojure.core.instance_QMARK_.apply(null,[clojure.lang.Named,parent_3]))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.core.str.apply(null,["Assert failed: ",clojure.core.pr_str.apply(null,[clojure.JS.lit_list([clojure.core.symbol("instance?"),clojure.core.symbol("clojure.lang.Named"),clojure.core.symbol("parent")])])]))})())),
((tp_4=clojure.core.keyword("","parents").apply(null,[h_1])),
(td_5=clojure.core.keyword("","descendants").apply(null,[h_1])),
(ta_6=clojure.core.keyword("","ancestors").apply(null,[h_1])),
(tf_7=(function __clojure_core_fn_3521_derive_3523_tf_3526(m_1,source_2,sources_3,target_4,targets_5){
return (clojure.core.reduce.apply(null,[(function __clojure_core_fn_3521_derive_3523_tf_3526_fn_3528(ret_1,k_2){
return (clojure.core.assoc.apply(null,[ret_1,k_2,clojure.core.reduce.apply(null,[clojure.core.conj,clojure.core.get.apply(null,[targets_5,k_2,clojure.lang.PersistentHashSet.EMPTY]),clojure.core.cons.apply(null,[target_4,targets_5.apply(null,[target_4])])])]))}),m_1,clojure.core.cons.apply(null,[source_2,sources_3.apply(null,[source_2])])]))})),
((or__962__auto___8=((clojure.core.contains_QMARK_.apply(null,[tp_4.apply(null,[tag_2]),parent_3]))?(null):(((clojure.core.contains_QMARK_.apply(null,[ta_6.apply(null,[tag_2]),parent_3]))?((function __throw(){throw clojure.lang.RT.makeException(clojure.core.print_str.apply(null,[tag_2,"already has",parent_3,"as ancestor"]))})()):(null)),
((clojure.core.contains_QMARK_.apply(null,[ta_6.apply(null,[parent_3]),tag_2]))?((function __throw(){throw clojure.lang.RT.makeException(clojure.core.print_str.apply(null,["Cyclic derivation:",parent_3,"has",tag_2,"as ancestor"]))})()):(null)),
clojure.core.hash_map(clojure.core.keyword("","parents"),clojure.core.assoc.apply(null,[clojure.core.keyword("","parents").apply(null,[h_1]),tag_2,clojure.core.conj.apply(null,[clojure.core.get.apply(null,[tp_4,tag_2,clojure.lang.PersistentHashSet.EMPTY]),parent_3])]),clojure.core.keyword("","ancestors"),tf_7.apply(null,[clojure.core.keyword("","ancestors").apply(null,[h_1]),tag_2,td_5,parent_3,ta_6]),clojure.core.keyword("","descendants"),tf_7.apply(null,[clojure.core.keyword("","descendants").apply(null,[h_1]),parent_3,ta_6,tag_2,td_5]))))),
((or__962__auto___8)?(or__962__auto___8):(h_1)))))})))}).apply(null,[]);

//======
//(defn underive "Removes a parent/child relationship between parent and\n  tag. h must be a hierarchy obtained from make-hierarchy, if not\n  supplied defaults to, and modifies, the global hierarchy." ([tag parent] (alter-var-root (var global-hierarchy) underive tag parent) nil) ([h tag parent] (let [tp (:parents h) td (:descendants h) ta (:ancestors h) tf (fn [m source sources target targets] (reduce (fn [ret k] (assoc ret k (reduce disj (get targets k) (cons target (targets target))))) m (cons source (sources source))))] (if (contains? (tp tag) parent) {:parent (assoc (:parents h) tag (disj (get tp tag) parent)), :ancestors (tf (:ancestors h) tag td parent ta), :descendants (tf (:descendants h) parent ta tag td)} h))))
//---
(function __clojure_core_fn_3534(){
return (clojure.JS.def(clojure.core,"underive",(function __clojure_core_fn_3534_underive_3536(h_1,tag_2,parent_3){switch(arguments.length){
case 2:var tag_1=arguments[0],parent_2=arguments[1];
return (clojure.core.alter_var_root.apply(null,[clojure.core._var_global_hierarchy,clojure.core.underive,tag_1,parent_2]),
null)}
var tp_4,td_5,ta_6,tf_7;
return (((tp_4=clojure.core.keyword("","parents").apply(null,[h_1])),
(td_5=clojure.core.keyword("","descendants").apply(null,[h_1])),
(ta_6=clojure.core.keyword("","ancestors").apply(null,[h_1])),
(tf_7=(function __clojure_core_fn_3534_underive_3536_tf_3539(m_1,source_2,sources_3,target_4,targets_5){
return (clojure.core.reduce.apply(null,[(function __clojure_core_fn_3534_underive_3536_tf_3539_fn_3541(ret_1,k_2){
return (clojure.core.assoc.apply(null,[ret_1,k_2,clojure.core.reduce.apply(null,[clojure.core.disj,clojure.core.get.apply(null,[targets_5,k_2]),clojure.core.cons.apply(null,[target_4,targets_5.apply(null,[target_4])])])]))}),m_1,clojure.core.cons.apply(null,[source_2,sources_3.apply(null,[source_2])])]))})),
((clojure.core.contains_QMARK_.apply(null,[tp_4.apply(null,[tag_2]),parent_3]))?(clojure.core.hash_map(clojure.core.keyword("","parent"),clojure.core.assoc.apply(null,[clojure.core.keyword("","parents").apply(null,[h_1]),tag_2,clojure.core.disj.apply(null,[clojure.core.get.apply(null,[tp_4,tag_2]),parent_3])]),clojure.core.keyword("","ancestors"),tf_7.apply(null,[clojure.core.keyword("","ancestors").apply(null,[h_1]),tag_2,td_5,parent_3,ta_6]),clojure.core.keyword("","descendants"),tf_7.apply(null,[clojure.core.keyword("","descendants").apply(null,[h_1]),parent_3,ta_6,tag_2,td_5]))):(h_1))))})))}).apply(null,[]);

//======
//(defn distinct? "Returns true if no two of the arguments are =" {:tag Boolean} ([x] true) ([x y] (not (= x y))) ([x y & more] (if (not= x y) (loop [s #{y x} [x & etc :as xs] more] (if xs (if (contains? s x) false (recur (conj s x) etc)) true)) false)))
//---
(function __clojure_core_fn_3547(){
return (clojure.JS.def(clojure.core,"distinct_QMARK_",clojure.JS.variadic(2,(function __clojure_core_fn_3547_distinct_QMARK_3549(x_1,y_2){switch(arguments.length){
case 1:return (true)
case 2:return (clojure.core.not.apply(null,[clojure.lang.Util.equiv(x_1,y_2)]))}
var vec__3556_13,xs_9,G__3554_5,etc_8,s_4,s_10,G__3554_11,etc_15,xs_16,x_14,s_12,x_7,vec__3555_6,more_3=clojure.JS.rest_args(this,arguments,2);
return (((clojure.core.not_EQ_.apply(null,[x_1,y_2]))?(((s_4=clojure.core.hash_set(y_2,x_1)),
(G__3554_5=more_3),
(vec__3555_6=G__3554_5),
(x_7=clojure.core.nth.apply(null,[vec__3555_6,(0),null])),
(etc_8=clojure.core.nthrest.apply(null,[vec__3555_6,(1)])),
(xs_9=vec__3555_6),
((function __loop(){var _rtn,_cnt;(s_10=s_4),
(G__3554_11=G__3554_5);do{_cnt=0;
_rtn=((s_12=s_10),
(vec__3556_13=G__3554_11),
(x_14=clojure.core.nth.apply(null,[vec__3556_13,(0),null])),
(etc_15=clojure.core.nthrest.apply(null,[vec__3556_13,(1)])),
(xs_16=vec__3556_13),
((xs_16)?(((clojure.core.contains_QMARK_.apply(null,[s_12,x_14]))?(false):((_cnt=1,_rtn=[clojure.core.conj.apply(null,[s_12,x_14]),etc_15],s_10=_rtn[0],G__3554_11=_rtn[1])))):(true)))}while(_cnt);return _rtn;})()))):(false)))}))))}).apply(null,[]);

//======
//(defn iterator-seq "Returns a seq on a java.util.Iterator. Note that most collections\n  providing iterators implement Iterable and thus support seq directly." [iter] (clojure.lang.IteratorSeq/create iter))
//---
(function __clojure_core_fn_3559(){
return (clojure.JS.def(clojure.core,"iterator_seq",(function __clojure_core_fn_3559_iterator_seq_3561(iter_1){
return (clojure.lang.IteratorSeq.create(iter_1))})))}).apply(null,[]);

//======
//(defn enumeration-seq "Returns a seq on a java.lang.Enumeration" [e] (clojure.lang.EnumerationSeq/create e))
//---
(function __clojure_core_fn_3565(){
return (clojure.JS.def(clojure.core,"enumeration_seq",(function __clojure_core_fn_3565_enumeration_seq_3567(e_1){
return (clojure.lang.EnumerationSeq.create(e_1))})))}).apply(null,[]);
// Skipping: (defn format "Formats a string using java.lang.String.format, see java.util.Formatter for format\n  string syntax" [fmt & args] (String/format fmt (to-array args)))

//======
//(defn printf "Prints formatted output, as per format" [fmt & args] (print (apply format fmt args)))
//---
(function __clojure_core_fn_3577(){
return (clojure.JS.def(clojure.core,"printf",clojure.JS.variadic(1,(function __clojure_core_fn_3577_printf_3579(fmt_1){
var args_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.core.print.apply(null,[clojure.core.apply.apply(null,[clojure.core.format,fmt_1,args_2])]))}))))}).apply(null,[]);

//======
//(def gen-class)
//---
(function __clojure_core_fn_3583(){
return (clojure.JS.def(clojure.core,"gen_class",null))}).apply(null,[]);
// Skipping: (defmacro ns "Sets *ns* to the namespace named by name (unevaluated), creating it\n  if needed.  references can be zero or more of: (:refer-clojure ...)\n  (:require ...) (:use ...) (:import ...) (:load ...) (:gen-class)\n  with the syntax of refer-clojure/require/use/import/load/gen-class\n  respectively, except the arguments are unevaluated and need not be\n  quoted. (:gen-class ...), when supplied, defaults to :name\n  corresponding to the ns name, :main true, :impl-ns same as ns, and\n  :init-impl-ns true. All options of gen-class are\n  supported. The :gen-class directive is ignored when not\n  compiling. If :gen-class is not supplied, when compiled only an\n  nsname__init.class will be generated. If :refer-clojure is not used, a\n  default (refer 'clojure) is used.  Use of ns is preferred to\n  individual calls to in-ns/require/use/import:\n\n  (ns foo.bar\n    (:refer-clojure :exclude [ancestors printf])\n    (:require (clojure.contrib sql sql.tests))\n    (:use (my.lib this that))\n    (:import (java.util Date Timer Random)\n              (java.sql Connection Statement)))" [name & references] (let [process-reference (fn [[kname & args]] (clojure.core/concat (clojure.core/list (symbol "clojure.core" (clojure.core/name kname))) (map (fn* [p1__3586] (list (quote quote) p1__3586)) args))) gen-class-clause (first (filter (fn* [p1__3587] (= :gen-class (first p1__3587))) references)) gen-class-call (when gen-class-clause (list* (quote clojure.core/gen-class) :name (.replace (str name) \- \_) :impl-ns name :main true (rest gen-class-clause))) references (remove (fn* [p1__3588] (= :gen-class (first p1__3588))) references)] (clojure.core/concat (clojure.core/list (quote do)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/in-ns)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list name))))) (when gen-class-call (list gen-class-call)) (when (and (not= name (quote clojure.core)) (not-any? (fn* [p1__3589] (= :refer-clojure (first p1__3589))) references)) (clojure.core/concat (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/refer)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list (quote clojure.core)))))))) (map process-reference references))))
// Skipping: (defmacro refer-clojure "Same as (refer 'clojure <filters>)" [& filters] (clojure.core/concat (clojure.core/list (quote clojure.core/refer)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote quote)) (clojure.core/list (quote clojure.core)))) filters))
// Skipping: (defmacro defonce "defs name to have the root value of the expr iff the named var has no root value, \n  else expr is unevaluated" [name expr] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote v__3643__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote def)) (clojure.core/list name)))))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when-not)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote .hasRoot)) (clojure.core/list (quote v__3643__auto__)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote def)) (clojure.core/list name) (clojure.core/list expr)))))))

//======
//(defonce *loaded-libs* (ref (sorted-set)))
//---
(function __clojure_core_fn_3653(){
var v__3643__auto___1;
return (((v__3643__auto___1=clojure.JS.def(clojure.core,"_STAR_loaded_libs_STAR_",null)),
(((v__3643__auto___1).hasRoot())?(null):(clojure.JS.def(clojure.core,"_STAR_loaded_libs_STAR_",clojure.core.ref.apply(null,[clojure.core.sorted_set.apply(null,[])]))))))}).apply(null,[]);

//======
//(defonce *pending-paths* #{})
//---
(function __clojure_core_fn_3656(){
var v__3643__auto___1;
return (((v__3643__auto___1=clojure.JS.def(clojure.core,"_STAR_pending_paths_STAR_",null)),
(((v__3643__auto___1).hasRoot())?(null):(clojure.JS.def(clojure.core,"_STAR_pending_paths_STAR_",clojure.lang.PersistentHashSet.EMPTY)))))}).apply(null,[]);

//======
//(defonce *loading-verbosely* false)
//---
(function __clojure_core_fn_3659(){
var v__3643__auto___1;
return (((v__3643__auto___1=clojure.JS.def(clojure.core,"_STAR_loading_verbosely_STAR_",null)),
(((v__3643__auto___1).hasRoot())?(null):(clojure.JS.def(clojure.core,"_STAR_loading_verbosely_STAR_",false)))))}).apply(null,[]);

//======
//(defn- throw-if "Throws an exception with a message if pred is true" [pred fmt & args] (when pred (let [message (apply format fmt args) exception (RT/makeException message) raw-trace (.getStackTrace exception) boring? (fn* [p1__3662] (not= (.getMethodName p1__3662) "doInvoke")) trace (into-array (drop 2 (drop-while boring? raw-trace)))] (.setStackTrace exception trace) (throw exception))))
//---
(function __clojure_core_fn_3663(){
return (clojure.JS.def(clojure.core,"throw_if",clojure.JS.variadic(2,(function __clojure_core_fn_3663_throw_if_3665(pred_1,fmt_2){
var message_4,exception_5,raw_trace_6,boring_QMARK__7,trace_8,args_3=clojure.JS.rest_args(this,arguments,2);
return (((pred_1)?(((message_4=clojure.core.apply.apply(null,[clojure.core.format,fmt_2,args_3])),
(exception_5=clojure.lang.RT.makeException(message_4)),
(raw_trace_6=(exception_5).getStackTrace()),
(boring_QMARK__7=(function __clojure_core_fn_3663_throw_if_3665_boring_QMARK_3667(p1__3662_1){
return (clojure.core.not_EQ_.apply(null,[clojure.JS.getOrRun(p1__3662_1,"getMethodName"),"doInvoke"]))})),
(trace_8=clojure.core.into_array.apply(null,[clojure.core.drop.apply(null,[(2),clojure.core.drop_while.apply(null,[boring_QMARK__7,raw_trace_6])])])),
(exception_5).setStackTrace(trace_8),
(function __throw(){throw exception_5})())):(null)))}))))}).apply(null,[]);

//======
//(defn- libspec? "Returns true if x is a libspec" [x] (or (symbol? x) (and (vector? x) (or (nil? (second x)) (keyword? (second x))))))
//---
(function __clojure_core_fn_3672(){
return (clojure.JS.def(clojure.core,"libspec_QMARK_",(function __clojure_core_fn_3672_libspec_QMARK_3674(x_1){
var or__962__auto___2,and__948__auto___3,or__962__auto___4;
return (((or__962__auto___2=clojure.core.symbol_QMARK_.apply(null,[x_1])),
((or__962__auto___2)?(or__962__auto___2):(((and__948__auto___3=clojure.core.vector_QMARK_.apply(null,[x_1])),
((and__948__auto___3)?(((or__962__auto___4=clojure.core.nil_QMARK_.apply(null,[clojure.core.second.apply(null,[x_1])])),
((or__962__auto___4)?(or__962__auto___4):(clojure.core.keyword_QMARK_.apply(null,[clojure.core.second.apply(null,[x_1])]))))):(and__948__auto___3)))))))})))}).apply(null,[]);

//======
//(defn- prependss "Prepends a symbol or a seq to coll" [x coll] (if (symbol? x) (cons x coll) (concat x coll)))
//---
(function __clojure_core_fn_3678(){
return (clojure.JS.def(clojure.core,"prependss",(function __clojure_core_fn_3678_prependss_3680(x_1,coll_2){
return (((clojure.core.symbol_QMARK_.apply(null,[x_1]))?(clojure.core.cons.apply(null,[x_1,coll_2])):(clojure.core.concat.apply(null,[x_1,coll_2]))))})))}).apply(null,[]);

//======
//(defn- root-resource "Returns the root directory path for a lib" [lib] (str \/ (.. (name lib) (replace \- \_) (replace \. \/))))
//---
(function __clojure_core_fn_3684(){
return (clojure.JS.def(clojure.core,"root_resource",(function __clojure_core_fn_3684_root_resource_3686(lib_1){
return (clojure.core.str.apply(null,["/",((clojure.core.name.apply(null,[lib_1])).replace("-","_")).replace(".","/")]))})))}).apply(null,[]);

//======
//(defn- root-directory "Returns the root resource path for a lib" [lib] (let [d (root-resource lib)] (subs d 0 (.lastIndexOf d "/"))))
//---
(function __clojure_core_fn_3690(){
return (clojure.JS.def(clojure.core,"root_directory",(function __clojure_core_fn_3690_root_directory_3692(lib_1){
var d_2;
return (((d_2=clojure.core.root_resource.apply(null,[lib_1])),
clojure.core.subs.apply(null,[d_2,(0),(d_2).lastIndexOf("/")])))})))}).apply(null,[]);
// Skipping: (def load)

//======
//(defn- load-one "Loads a lib given its name. If need-ns, ensures that the associated\n  namespace exists after loading. If require, records the load so any\n  duplicate loads can be skipped." [lib need-ns require] (load (root-resource lib)) (throw-if (and need-ns (not (find-ns lib))) "namespace '%s' not found after loading '%s'" lib (root-resource lib)) (when require (dosync (commute *loaded-libs* conj lib))))
//---
(function __clojure_core_fn_3699(){
return (clojure.JS.def(clojure.core,"load_one",(function __clojure_core_fn_3699_load_one_3701(lib_1,need_ns_2,require_3){
var and__948__auto___4;
return (clojure.core.load.apply(null,[clojure.core.root_resource.apply(null,[lib_1])]),
clojure.core.throw_if.apply(null,[((and__948__auto___4=need_ns_2),
((and__948__auto___4)?(clojure.core.not.apply(null,[clojure.core.find_ns.apply(null,[lib_1])])):(and__948__auto___4))),"namespace '%s' not found after loading '%s'",lib_1,clojure.core.root_resource.apply(null,[lib_1])]),
((require_3)?(clojure.lang.LockingTransaction.runInTransaction((function __clojure_core_fn_3699_load_one_3701_fn_3703(){
return (clojure.core.commute.apply(null,[clojure.core._STAR_loaded_libs_STAR_,clojure.core.conj,lib_1]))}))):(null)))})))}).apply(null,[]);

//======
//(defn- load-all "Loads a lib given its name and forces a load of any libs it directly or\n  indirectly loads. If need-ns, ensures that the associated namespace\n  exists after loading. If require, records the load so any duplicate loads\n  can be skipped." [lib need-ns require] (dosync (commute *loaded-libs* (fn* [p1__3708 p2__3709] (reduce conj p1__3708 p2__3709)) (binding [*loaded-libs* (ref (sorted-set))] (load-one lib need-ns require) (clojure.core/deref *loaded-libs*)))))
//---
(function __clojure_core_fn_3710(){
return (clojure.JS.def(clojure.core,"load_all",(function __clojure_core_fn_3710_load_all_3712(lib_1,need_ns_2,require_3){
return (clojure.lang.LockingTransaction.runInTransaction((function __clojure_core_fn_3710_load_all_3712_fn_3714(){
return (clojure.core.commute.apply(null,[clojure.core._STAR_loaded_libs_STAR_,(function __clojure_core_fn_3710_load_all_3712_fn_3714_fn_3716(p1__3708_1,p2__3709_2){
return (clojure.core.reduce.apply(null,[clojure.core.conj,p1__3708_1,p2__3709_2]))}),clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_loaded_libs_STAR_,clojure.core.ref.apply(null,[clojure.core.sorted_set.apply(null,[])])])),
(function __clojure_core_fn_3710_load_all_3712_fn_3714_fn_3719(){
return ((function __try(){try{var _rtn=(clojure.core.load_one.apply(null,[lib_1,need_ns_2,require_3]),
clojure.core.deref.apply(null,[clojure.core._STAR_loaded_libs_STAR_]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())}).apply(null,[])]))})))})))}).apply(null,[]);

//======
//(defn- load-lib "Loads a lib with options" [prefix lib & options] (throw-if (and prefix (pos? (.indexOf (name lib) (int \.)))) "lib names inside prefix lists must not contain periods") (let [lib (if prefix (symbol (str prefix \. lib)) lib) opts (apply hash-map options) {:keys [as reload reload-all require use verbose]} opts loaded (contains? (clojure.core/deref *loaded-libs*) lib) load (cond reload-all load-all (or reload (not require) (not loaded)) load-one) need-ns (or as use) filter-opts (select-keys opts (quote (:exclude :only :rename)))] (binding [*loading-verbosely* (or *loading-verbosely* verbose)] (if load (load lib need-ns require) (throw-if (and need-ns (not (find-ns lib))) "namespace '%s' not found" lib)) (when (and need-ns *loading-verbosely*) (printf "(clojure.core/in-ns '%s)\n" (ns-name *ns*))) (when as (when *loading-verbosely* (printf "(clojure.core/alias '%s '%s)\n" as lib)) (alias as lib)) (when use (when *loading-verbosely* (printf "(clojure.core/refer '%s" lib) (doseq [opt filter-opts] (printf " %s '%s" (key opt) (print-str (val opt)))) (printf ")\n")) (apply refer lib (mapcat seq filter-opts))))))
//---
(function __clojure_core_fn_3725(){
return (clojure.JS.def(clojure.core,"load_lib",clojure.JS.variadic(2,(function __clojure_core_fn_3725_load_lib_3727(prefix_1,lib_2){
var opts_5,and__948__auto___19,or__962__auto___15,and__948__auto___4,filter_opts_16,opt_20,loaded_13,reload_all_10,as_12,load_14,need_ns_15,lib_4,and__948__auto___19,or__962__auto___15,reload_11,use_8,verbose_7,or__962__auto___14,require_9,map__3729_6,sq__2041__auto___19,or__962__auto___17,options_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.core.throw_if.apply(null,[((and__948__auto___4=prefix_1),
((and__948__auto___4)?(clojure.lang.Numbers.isPos((clojure.core.name.apply(null,[lib_2])).indexOf(clojure.lang.RT.intCast(".")))):(and__948__auto___4))),"lib names inside prefix lists must not contain periods"]),
((lib_4=((prefix_1)?(clojure.core.symbol.apply(null,[clojure.core.str.apply(null,[prefix_1,".",lib_2])])):(lib_2))),
(opts_5=clojure.core.apply.apply(null,[clojure.core.hash_map,options_3])),
(map__3729_6=opts_5),
(verbose_7=clojure.core.get.apply(null,[map__3729_6,clojure.core.keyword("","verbose")])),
(use_8=clojure.core.get.apply(null,[map__3729_6,clojure.core.keyword("","use")])),
(require_9=clojure.core.get.apply(null,[map__3729_6,clojure.core.keyword("","require")])),
(reload_all_10=clojure.core.get.apply(null,[map__3729_6,clojure.core.keyword("","reload-all")])),
(reload_11=clojure.core.get.apply(null,[map__3729_6,clojure.core.keyword("","reload")])),
(as_12=clojure.core.get.apply(null,[map__3729_6,clojure.core.keyword("","as")])),
(loaded_13=clojure.core.contains_QMARK_.apply(null,[clojure.core.deref.apply(null,[clojure.core._STAR_loaded_libs_STAR_]),lib_4])),
(load_14=((reload_all_10)?(clojure.core.load_all):(((((or__962__auto___14=reload_11),
((or__962__auto___14)?(or__962__auto___14):(((or__962__auto___15=clojure.core.not.apply(null,[require_9])),
((or__962__auto___15)?(or__962__auto___15):(clojure.core.not.apply(null,[loaded_13]))))))))?(clojure.core.load_one):(null))))),
(need_ns_15=((or__962__auto___15=as_12),
((or__962__auto___15)?(or__962__auto___15):(use_8)))),
(filter_opts_16=clojure.core.select_keys.apply(null,[opts_5,clojure.JS.lit_list([clojure.core.keyword("","exclude"),clojure.core.keyword("","only"),clojure.core.keyword("","rename")])])),
clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_loading_verbosely_STAR_,((or__962__auto___17=clojure.core._STAR_loading_verbosely_STAR_),
((or__962__auto___17)?(or__962__auto___17):(verbose_7)))])),
(function __try(){try{var _rtn=(((load_14)?(load_14.apply(null,[lib_4,need_ns_15,require_9])):(clojure.core.throw_if.apply(null,[((and__948__auto___19=need_ns_15),
((and__948__auto___19)?(clojure.core.not.apply(null,[clojure.core.find_ns.apply(null,[lib_4])])):(and__948__auto___19))),"namespace '%s' not found",lib_4]))),
((((and__948__auto___19=need_ns_15),
((and__948__auto___19)?(clojure.core._STAR_loading_verbosely_STAR_):(and__948__auto___19))))?(clojure.core.printf.apply(null,["(clojure.core/in-ns '%s)\n",clojure.core.ns_name.apply(null,[clojure.core._STAR_ns_STAR_])])):(null)),
((as_12)?(((clojure.core._STAR_loading_verbosely_STAR_)?(clojure.core.printf.apply(null,["(clojure.core/alias '%s '%s)\n",as_12,lib_4])):(null)),
clojure.core.alias.apply(null,[as_12,lib_4])):(null)),
((use_8)?(((clojure.core._STAR_loading_verbosely_STAR_)?(clojure.core.printf.apply(null,["(clojure.core/refer '%s",lib_4]),
((function __loop(){var _rtn,_cnt;(sq__2041__auto___19=clojure.core.seq.apply(null,[filter_opts_16]));do{_cnt=0;
_rtn=((sq__2041__auto___19)?(((opt_20=clojure.core.first.apply(null,[sq__2041__auto___19])),
((true)?(((true)?(clojure.core.printf.apply(null,[" %s '%s",clojure.core.key.apply(null,[opt_20]),clojure.core.print_str.apply(null,[clojure.core.val.apply(null,[opt_20])])])):(null)),
(_cnt=1,_rtn=[clojure.core.rest.apply(null,[sq__2041__auto___19])],sq__2041__auto___19=_rtn[0])):(null)))):(null))}while(_cnt);return _rtn;})()),
clojure.core.printf.apply(null,[")\n"])):(null)),
clojure.core.apply.apply(null,[clojure.core.refer,lib_4,clojure.core.mapcat.apply(null,[clojure.core.seq,filter_opts_16])])):(null)))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})()))}))))}).apply(null,[]);

//======
//(defn- load-libs "Loads libs, interpreting libspecs, prefix lists, and flags for\n  forwarding to load-lib" [& args] (let [flags (filter keyword? args) opts (interleave flags (repeat true)) args (filter (complement keyword?) args)] (doseq [arg args] (if (libspec? arg) (apply load-lib nil (prependss arg opts)) (let [[prefix & args] arg] (throw-if (nil? prefix) "prefix cannot be nil") (doseq [arg args] (apply load-lib prefix (prependss arg opts))))))))
//---
(function __clojure_core_fn_3732(){
return (clojure.JS.def(clojure.core,"load_libs",clojure.JS.variadic(0,(function __clojure_core_fn_3732_load_libs_3734(){
var sq__2041__auto___10,opts_3,vec__3736_7,arg_6,args_4,flags_2,sq__2041__auto___5,arg_11,prefix_8,args_9,args_1=clojure.JS.rest_args(this,arguments,0);
return (((flags_2=clojure.core.filter.apply(null,[clojure.core.keyword_QMARK_,args_1])),
(opts_3=clojure.core.interleave.apply(null,[flags_2,clojure.core.repeat.apply(null,[true])])),
(args_4=clojure.core.filter.apply(null,[clojure.core.complement.apply(null,[clojure.core.keyword_QMARK_]),args_1])),
((function __loop(){var _rtn,_cnt;(sq__2041__auto___5=clojure.core.seq.apply(null,[args_4]));do{_cnt=0;
_rtn=((sq__2041__auto___5)?(((arg_6=clojure.core.first.apply(null,[sq__2041__auto___5])),
((true)?(((true)?(((clojure.core.libspec_QMARK_.apply(null,[arg_6]))?(clojure.core.apply.apply(null,[clojure.core.load_lib,null,clojure.core.prependss.apply(null,[arg_6,opts_3])])):(((vec__3736_7=arg_6),
(prefix_8=clojure.core.nth.apply(null,[vec__3736_7,(0),null])),
(args_9=clojure.core.nthrest.apply(null,[vec__3736_7,(1)])),
clojure.core.throw_if.apply(null,[clojure.core.nil_QMARK_.apply(null,[prefix_8]),"prefix cannot be nil"]),
((function __loop(){var _rtn,_cnt;(sq__2041__auto___10=clojure.core.seq.apply(null,[args_9]));do{_cnt=0;
_rtn=((sq__2041__auto___10)?(((arg_11=clojure.core.first.apply(null,[sq__2041__auto___10])),
((true)?(((true)?(clojure.core.apply.apply(null,[clojure.core.load_lib,prefix_8,clojure.core.prependss.apply(null,[arg_11,opts_3])])):(null)),
(_cnt=1,_rtn=[clojure.core.rest.apply(null,[sq__2041__auto___10])],sq__2041__auto___10=_rtn[0])):(null)))):(null))}while(_cnt);return _rtn;})()))))):(null)),
(_cnt=1,_rtn=[clojure.core.rest.apply(null,[sq__2041__auto___5])],sq__2041__auto___5=_rtn[0])):(null)))):(null))}while(_cnt);return _rtn;})())))}))))}).apply(null,[]);

//======
//(defn require "Loads libs, skipping any that are already loaded. Each argument is\n  either a libspec that identifies a lib, a prefix list that identifies\n  multiple libs whose names share a common prefix, or a flag that modifies\n  how all the identified libs are loaded. Use :require in the ns macro \n  in preference to calling this directly.\n\n  Libs\n\n  A 'lib' is a named set of resources in classpath whose contents define a\n  library of Clojure code. Lib names are symbols and each lib is associated\n  with a Clojure namespace and a Java package that share its name. A lib's\n  name also locates its root directory within classpath using Java's\n  package name to classpath-relative path mapping. All resources in a lib\n  should be contained in the directory structure under its root directory.\n  All definitions a lib makes should be in its associated namespace.\n\n  'require loads a lib by loading its root resource. The root resource path\n  is derived from the root directory path by repeating its last component\n  and appending '.clj'. For example, the lib 'x.y.z has root directory\n  <classpath>/x/y/z; root resource <classpath>/x/y/z/z.clj. The root\n  resource should contain code to create the lib's namespace and load any\n  additional lib resources.\n\n  Libspecs\n\n  A libspec is a lib name or a vector containing a lib name followed by\n  options expressed as sequential keywords and arguments.\n\n  Recognized options: :as\n  :as takes a symbol as its argument and makes that symbol an alias to the\n    lib's namespace in the current namespace.\n\n  Prefix Lists\n\n  It's common for Clojure code to depend on several libs whose names have\n  the same prefix. When specifying libs, prefix lists can be used to reduce\n  repetition. A prefix list contains the shared prefix followed by libspecs\n  with the shared prefix removed from the lib names. After removing the\n  prefix, the names that remain must not contain any periods.\n\n  Flags\n\n  A flag is a keyword.\n  Recognized flags: :reload, :reload-all, :verbose\n  :reload forces loading of all the identified libs even if they are\n    already loaded\n  :reload-all implies :reload and also forces loading of all libs that the\n    identified libs directly or indirectly load via require or use\n  :verbose triggers printing information about each load, alias, and refer" [& args] (apply load-libs :require args))
//---
(function __clojure_core_fn_3739(){
return (clojure.JS.def(clojure.core,"require",clojure.JS.variadic(0,(function __clojure_core_fn_3739_require_3741(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[clojure.core.load_libs,clojure.core.keyword("","require"),args_1]))}))))}).apply(null,[]);

//======
//(defn use "Like 'require, but also refers to each lib's namespace using\n  clojure.core/refer. Use :use in the ns macro in preference to calling\n  this directly.\n\n  'use accepts additional options in libspecs: :exclude, :only, :rename.\n  The arguments and semantics for :exclude, :only, and :rename are the same\n  as those documented for clojure.core/refer." [& args] (apply load-libs :require :use args))
//---
(function __clojure_core_fn_3745(){
return (clojure.JS.def(clojure.core,"use",clojure.JS.variadic(0,(function __clojure_core_fn_3745_use_3747(){
var args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.core.apply.apply(null,[clojure.core.load_libs,clojure.core.keyword("","require"),clojure.core.keyword("","use"),args_1]))}))))}).apply(null,[]);

//======
//(defn loaded-libs "Returns a sorted set of symbols naming the currently loaded libs" [] (clojure.core/deref *loaded-libs*))
//---
(function __clojure_core_fn_3751(){
return (clojure.JS.def(clojure.core,"loaded_libs",(function __clojure_core_fn_3751_loaded_libs_3753(){
return (clojure.core.deref.apply(null,[clojure.core._STAR_loaded_libs_STAR_]))})))}).apply(null,[]);
// Skipping: (defn load "Loads Clojure code from resources in classpath. A path is interpreted as\n  classpath-relative if it begins with a slash or relative to the root\n  directory for the current namespace otherwise." [& paths] (doseq [path paths] (let [path (if (.startsWith path "/") path (str (root-directory (ns-name *ns*)) \/ path))] (when *loading-verbosely* (printf "(clojure.core/load \"%s\")\n" path) (flush)) (when-not (*pending-paths* path) (binding [*pending-paths* (conj *pending-paths* path)] (clojure.lang.RT/load (.substring path 1)))))))

//======
//(defn compile "Compiles the namespace named by the symbol lib into a set of\n  classfiles. The source for the lib must be in a proper\n  classpath-relative directory. The output files will go into the\n  directory specified by *compile-path*, and that directory too must\n  be in the classpath." [lib] (binding [*compile-files* true] (load-one lib true true)) lib)
//---
(function __clojure_core_fn_3766(){
return (clojure.JS.def(clojure.core,"compile",(function __clojure_core_fn_3766_compile_3768(lib_1){
return (clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_compile_files_STAR_,true])),
(function __clojure_core_fn_3766_compile_3768_fn_3770(){
return ((function __try(){try{var _rtn=(clojure.core.load_one.apply(null,[lib_1,true,true]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())}).apply(null,[]),
lib_1)})))}).apply(null,[]);

//======
//(defn get-in "returns the value in a nested associative structure, where ks is a sequence of keys" [m ks] (reduce get m ks))
//---
(function __clojure_core_fn_3775(){
return (clojure.JS.def(clojure.core,"get_in",(function __clojure_core_fn_3775_get_in_3777(m_1,ks_2){
return (clojure.core.reduce.apply(null,[clojure.core.get,m_1,ks_2]))})))}).apply(null,[]);

//======
//(defn assoc-in "Associates a value in a nested associative structure, where ks is a\n  sequence of keys and v is the new value and returns a new nested structure.  \n  If any levels do not exist, hash-maps will be created." [m [k & ks] v] (if ks (assoc m k (assoc-in (get m k) ks v)) (assoc m k v)))
//---
(function __clojure_core_fn_3781(){
return (clojure.JS.def(clojure.core,"assoc_in",(function __clojure_core_fn_3781_assoc_in_3784(m_1,p__3783_2,v_3){
var vec__3786_4,k_5,ks_6;
return (((vec__3786_4=p__3783_2),
(k_5=clojure.core.nth.apply(null,[vec__3786_4,(0),null])),
(ks_6=clojure.core.nthrest.apply(null,[vec__3786_4,(1)])),
((ks_6)?(clojure.core.assoc.apply(null,[m_1,k_5,clojure.core.assoc_in.apply(null,[clojure.core.get.apply(null,[m_1,k_5]),ks_6,v_3])])):(clojure.core.assoc.apply(null,[m_1,k_5,v_3])))))})))}).apply(null,[]);

//======
//(defn update-in "'Updates' a value in a nested associative structure, where ks is a\n  sequence of keys and f is a function that will take the old value\n  and any supplied args and return the new value, and returns a new\n  nested structure.  If any levels do not exist, hash-maps will be\n  created." ([m [k & ks] f & args] (if ks (assoc m k (apply update-in (get m k) ks f args)) (assoc m k (apply f (get m k) args)))))
//---
(function __clojure_core_fn_3789(){
return (clojure.JS.def(clojure.core,"update_in",clojure.JS.variadic(3,(function __clojure_core_fn_3789_update_in_3792(m_1,p__3791_2,f_3){
var vec__3794_5,k_6,ks_7,args_4=clojure.JS.rest_args(this,arguments,3);
return (((vec__3794_5=p__3791_2),
(k_6=clojure.core.nth.apply(null,[vec__3794_5,(0),null])),
(ks_7=clojure.core.nthrest.apply(null,[vec__3794_5,(1)])),
((ks_7)?(clojure.core.assoc.apply(null,[m_1,k_6,clojure.core.apply.apply(null,[clojure.core.update_in,clojure.core.get.apply(null,[m_1,k_6]),ks_7,f_3,args_4])])):(clojure.core.assoc.apply(null,[m_1,k_6,clojure.core.apply.apply(null,[f_3,clojure.core.get.apply(null,[m_1,k_6]),args_4])])))))}))))}).apply(null,[]);

//======
//(defn empty? "Returns true if coll has no items - same as (not (seq coll)). \n  Please use the idiom (seq x) rather than (not (empty? x))" [coll] (not (seq coll)))
//---
(function __clojure_core_fn_3797(){
return (clojure.JS.def(clojure.core,"empty_QMARK_",(function __clojure_core_fn_3797_empty_QMARK_3799(coll_1){
return (clojure.core.not.apply(null,[clojure.core.seq.apply(null,[coll_1])]))})))}).apply(null,[]);

//======
//(defn coll? "Returns true if x implements IPersistentCollection" [x] (instance? clojure.lang.IPersistentCollection x))
//---
(function __clojure_core_fn_3803(){
return (clojure.JS.def(clojure.core,"coll_QMARK_",(function __clojure_core_fn_3803_coll_QMARK_3805(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.IPersistentCollection,x_1]))})))}).apply(null,[]);

//======
//(defn list? "Returns true if x implements IPersistentList" [x] (instance? clojure.lang.IPersistentList x))
//---
(function __clojure_core_fn_3809(){
return (clojure.JS.def(clojure.core,"list_QMARK_",(function __clojure_core_fn_3809_list_QMARK_3811(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.IPersistentList,x_1]))})))}).apply(null,[]);

//======
//(defn set? "Returns true if x implements IPersistentSet" [x] (instance? clojure.lang.IPersistentSet x))
//---
(function __clojure_core_fn_3815(){
return (clojure.JS.def(clojure.core,"set_QMARK_",(function __clojure_core_fn_3815_set_QMARK_3817(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.IPersistentSet,x_1]))})))}).apply(null,[]);

//======
//(defn ifn? "Returns true if x implements IFn. Note that many data structures \n  (e.g. sets and maps) implement IFn" [x] (instance? clojure.lang.IFn x))
//---
(function __clojure_core_fn_3821(){
return (clojure.JS.def(clojure.core,"ifn_QMARK_",(function __clojure_core_fn_3821_ifn_QMARK_3823(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.IFn,x_1]))})))}).apply(null,[]);

//======
//(defn fn? "Returns true if x implements Fn, i.e. is an object created via fn." [x] (instance? clojure.lang.Fn x))
//---
(function __clojure_core_fn_3827(){
return (clojure.JS.def(clojure.core,"fn_QMARK_",(function __clojure_core_fn_3827_fn_QMARK_3829(x_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Fn,x_1]))})))}).apply(null,[]);

//======
//(defn associative? "Returns true if coll implements Associative" [coll] (instance? clojure.lang.Associative coll))
//---
(function __clojure_core_fn_3833(){
return (clojure.JS.def(clojure.core,"associative_QMARK_",(function __clojure_core_fn_3833_associative_QMARK_3835(coll_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Associative,coll_1]))})))}).apply(null,[]);

//======
//(defn sequential? "Returns true if coll implements Sequential" [coll] (instance? clojure.lang.Sequential coll))
//---
(function __clojure_core_fn_3839(){
return (clojure.JS.def(clojure.core,"sequential_QMARK_",(function __clojure_core_fn_3839_sequential_QMARK_3841(coll_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Sequential,coll_1]))})))}).apply(null,[]);

//======
//(defn sorted? "Returns true if coll implements Sorted" [coll] (instance? clojure.lang.Sorted coll))
//---
(function __clojure_core_fn_3845(){
return (clojure.JS.def(clojure.core,"sorted_QMARK_",(function __clojure_core_fn_3845_sorted_QMARK_3847(coll_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Sorted,coll_1]))})))}).apply(null,[]);

//======
//(defn reversible? "Returns true if coll implements Reversible" [coll] (instance? clojure.lang.Reversible coll))
//---
(function __clojure_core_fn_3851(){
return (clojure.JS.def(clojure.core,"reversible_QMARK_",(function __clojure_core_fn_3851_reversible_QMARK_3853(coll_1){
return (clojure.core.instance_QMARK_.apply(null,[clojure.lang.Reversible,coll_1]))})))}).apply(null,[]);
// Skipping: (defn pmap "Like map, except f is applied in parallel. Semi-lazy in that the\n  parallel computation stays ahead of the consumption, but doesn't\n  realize the entire result unless required. Only useful for\n  computationally intensive functions where the time of f dominates\n  the coordination overhead." ([f coll] (let [n (inc (.. Runtime getRuntime availableProcessors)) agents (doall (map (fn* [p1__3857] (agent (f p1__3857))) (take n coll))) wget (fn [a] (await1 a) (clojure.core/deref a)) step (fn step [[x & xs :as s] [a & as :as acycle]] (if s (let [v (wget a)] (send a (fn [_] (f x))) (lazy-cons v (step xs as))) (map wget (take (count agents) acycle))))] (step (drop n coll) (cycle agents)))) ([f coll & colls] (let [step (fn step [cs] (when (every? seq cs) (lazy-cons (map first cs) (step (map rest cs)))))] (pmap (fn* [p1__3858] (apply f p1__3858)) (step (cons coll colls))))))

//======
//(def *1)
//---
(function __clojure_core_fn_3896(){
return (clojure.JS.def(clojure.core,"_STAR_1",null))}).apply(null,[]);

//======
//(def *2)
//---
(function __clojure_core_fn_3899(){
return (clojure.JS.def(clojure.core,"_STAR_2",null))}).apply(null,[]);

//======
//(def *3)
//---
(function __clojure_core_fn_3902(){
return (clojure.JS.def(clojure.core,"_STAR_3",null))}).apply(null,[]);

//======
//(def *e)
//---
(function __clojure_core_fn_3905(){
return (clojure.JS.def(clojure.core,"_STAR_e",null))}).apply(null,[]);
// Skipping: (defmacro declare "defs the supplied var names with no bindings, useful for making forward declarations." [& names] (clojure.core/concat (clojure.core/list (quote do)) (map (fn* [p1__3908] (list (quote def) p1__3908)) names)))

//======
//(defn trampoline "trampoline can be used to convert algorithms requiring mutual\n  recursion without stack consumption. Calls f with supplied args, if\n  any. If f returns a fn, calls that fn with no arguments, and\n  continues to repeat, until the return value is not a fn, then\n  returns that non-fn value. Note that if you want to return a fn as a\n  final value, you must wrap it in some data structure and unpack it\n  after trampoline returns." ([f] (let [ret (f)] (if (fn? ret) (recur ret) ret))) ([f & args] (trampoline (fn* [] (apply f args)))))
//---
(function __clojure_core_fn_3924(){
return (clojure.JS.def(clojure.core,"trampoline",clojure.JS.variadic(1,(function __clojure_core_fn_3924_trampoline_3926(f_1){switch(arguments.length){
case 1:var _cnt,_rtn,ret_2;
do{_cnt=0;_rtn=((ret_2=f_1.apply(null,[])),
((clojure.core.fn_QMARK_.apply(null,[ret_2]))?((_cnt=1,_rtn=[ret_2],f_1=_rtn[0])):(ret_2)))
}while(_cnt);return _rtn;}
var args_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.core.trampoline.apply(null,[(function __clojure_core_fn_3924_trampoline_3926_fn_3929(){
return (clojure.core.apply.apply(null,[f_1,args_2]))})]))}))))}).apply(null,[]);

//======
//(defn intern "Finds or creates a var named by the symbol name in the namespace\n  ns (which can be a symbol or a namespace), setting its root binding\n  to val if supplied. The namespace must exist. The var will adopt any\n  metadata from the name symbol.  Returns the var." ([ns name] (let [v (clojure.lang.Var/intern (the-ns ns) name)] (when (clojure.core/meta name) (.setMeta v (clojure.core/meta name))) v)) ([ns name val] (let [v (clojure.lang.Var/intern (the-ns ns) name val)] (when (clojure.core/meta name) (.setMeta v (clojure.core/meta name))) v)))
//---
(function __clojure_core_fn_3934(){
return (clojure.JS.def(clojure.core,"intern",(function __clojure_core_fn_3934_intern_3936(ns_1,name_2,val_3){switch(arguments.length){
case 2:var v_3;
return (((v_3=clojure.lang.Var.intern(clojure.core.the_ns.apply(null,[ns_1]),name_2)),
((clojure.core.meta.apply(null,[name_2]))?((v_3).setMeta(clojure.core.meta.apply(null,[name_2]))):(null)),
v_3))}
var v_4;
return (((v_4=clojure.lang.Var.intern(clojure.core.the_ns.apply(null,[ns_1]),name_2,val_3)),
((clojure.core.meta.apply(null,[name_2]))?((v_4).setMeta(clojure.core.meta.apply(null,[name_2]))):(null)),
v_4))})))}).apply(null,[]);
// Skipping: (defmacro while "Repeatedly executes body while test expression is true. Presumes\n  some side-effect will cause test to become false/nil. Returns nil" [test & body] (clojure.core/concat (clojure.core/list (quote clojure.core/loop)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/when)) (clojure.core/list test) body (clojure.core/list (clojure.core/concat (clojure.core/list (quote recur))))))))

//======
//(defn memoize "Returns a memoized version of a referentially transparent function. The\n  memoized version of the function keeps a cache of the mapping from arguments\n  to results and, when calls with the same arguments are repeated often, has\n  higher performance at the expense of higher memory use." [f] (let [mem (atom {})] (fn [& args] (if-let [e (find (clojure.core/deref mem) args)] (val e) (let [ret (apply f args)] (swap! mem assoc args ret) ret)))))
//---
(function __clojure_core_fn_3950(){
return (clojure.JS.def(clojure.core,"memoize",(function __clojure_core_fn_3950_memoize_3952(f_1){
var mem_2;
return (((mem_2=clojure.core.atom.apply(null,[clojure.lang.PersistentArrayMap.EMPTY])),
clojure.JS.variadic(0,(function __clojure_core_fn_3950_memoize_3952_fn_3954(){
var temp__3238__auto___2,e_3,ret_3,args_1=clojure.JS.rest_args(this,arguments,0);
return (((temp__3238__auto___2=clojure.core.find.apply(null,[clojure.core.deref.apply(null,[mem_2]),args_1])),
((temp__3238__auto___2)?(((e_3=temp__3238__auto___2),
clojure.core.val.apply(null,[e_3]))):(((ret_3=clojure.core.apply.apply(null,[f_1,args_1])),
clojure.core.swap_BANG_.apply(null,[mem_2,clojure.core.assoc,args_1,ret_3]),
ret_3)))))}))))})))}).apply(null,[]);
// Skipping: (defmacro condp "Takes a binary predicate, an expression, and a set of clauses.\n  Each clause can take the form of either:\n  \n  test-expr result-expr\n\n  test-expr :>> result-fn\n\n  Note :>> is an ordinary keyword.\n\n  For each clause, (pred test-expr expr) is evaluated. If it returns\n  logical true, the clause is a match. If a binary clause matches, the\n  result-expr is returned, if a ternary clause matches, its result-fn,\n  which must be a unary function, is called with the result of the\n  predicate as its argument, the result of that call being the return\n  value of condp. A single default expression can follow the clauses,\n  and its value will be returned if no clause matches. If no default\n  expression is provided and no clause matches, an\n  IllegalArgumentException is thrown." [pred expr & clauses] (let [gpred (gensym "pred__") gexpr (gensym "expr__") emit (fn emit [pred expr args] (let [[[a b c :as clause] more] (split-at (if (= :>> (second args)) 3 2) args) n (count clause)] (cond (= 0 n) (clojure.core/concat (clojure.core/list (quote throw)) (clojure.core/list (clojure.core/concat (clojure.core/list (quote java.lang.IllegalArgumentException.)) (clojure.core/list "No matching clause")))) (= 1 n) a (= 2 n) (clojure.core/concat (clojure.core/list (quote if)) (clojure.core/list (clojure.core/concat (clojure.core/list pred) (clojure.core/list a) (clojure.core/list expr))) (clojure.core/list b) (clojure.core/list (emit pred expr more))) :else (clojure.core/concat (clojure.core/list (quote clojure.core/if-let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote p__3959__auto__)) (clojure.core/list (clojure.core/concat (clojure.core/list pred) (clojure.core/list a) (clojure.core/list expr)))))) (clojure.core/list (clojure.core/concat (clojure.core/list c) (clojure.core/list (quote p__3959__auto__)))) (clojure.core/list (emit pred expr more)))))) gres (gensym "res__")] (clojure.core/concat (clojure.core/list (quote clojure.core/let)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list gpred) (clojure.core/list pred) (clojure.core/list gexpr) (clojure.core/list expr)))) (clojure.core/list (emit gpred gexpr clauses)))))

//======
//(load "core_proxy")
//---
(function __clojure_core_fn_3977(){
return (clojure.core.load.apply(null,["core_proxy"]))}).apply(null,[]);

//======
//(load "core_print")
//---
(function __clojure_core_fn_3980(){
return (clojure.core.load.apply(null,["core_print"]))}).apply(null,[]);

//======
//(load "genclass")
//---
(function __clojure_core_fn_3983(){
return (clojure.core.load.apply(null,["genclass"]))}).apply(null,[]);

//======
//(in-ns (quote clojure.core))
//---
(function __user_fn_3986(){
return (clojure.core.in_ns.apply(null,[clojure.core.symbol("clojure.core")]))}).apply(null,[]);

//======
//(import (quote (java.io Writer)))
//---
(function __clojure_core_fn_3992(){
return (clojure.core.import_.apply(null,[clojure.JS.lit_list([clojure.core.symbol("java.io"),clojure.core.symbol("Writer")])]))}).apply(null,[]);

//======
//(def *print-length* nil)
//---
(function __clojure_core_fn_3995(){
return (clojure.JS.def(clojure.core,"_STAR_print_length_STAR_",null))}).apply(null,[]);

//======
//(def *print-level* nil)
//---
(function __clojure_core_fn_3998(){
return (clojure.JS.def(clojure.core,"_STAR_print_level_STAR_",null))}).apply(null,[]);

//======
//(defn- print-sequential [begin print-one sep end sequence w] (binding [*print-level* (and (not *print-dup*) *print-level* (dec *print-level*))] (if (and *print-level* (neg? *print-level*)) (.write w "#") (do (.write w begin) (when-let [xs (seq sequence)] (if (and (not *print-dup*) *print-length*) (loop [[x & xs] xs print-length *print-length*] (if (zero? print-length) (.write w "...") (do (print-one x w) (when xs (.write w sep) (recur xs (dec print-length)))))) (loop [[x & xs] xs] (print-one x w) (when xs (.write w sep) (recur xs))))) (.write w end)))))
//---
(function __clojure_core_fn_4001(){
return (clojure.JS.def(clojure.core,"print_sequential",(function __clojure_core_fn_4001_print_sequential_4003(begin_1,print_one_2,sep_3,end_4,sequence_5,w_6){
var vec__4007_12,print_length_21,vec__4012_16,xs_18,x_17,xs_10,G__4010_11,and__948__auto___11,temp__3253__auto___9,and__948__auto___7,x_19,G__4006_11,print_length_15,xs_14,print_length_17,x_13,G__4006_16,xs_20,x_13,and__948__auto___8,xs_14,vec__4011_12,vec__4008_18,G__4010_15,and__948__auto___9;
return (clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[clojure.core._var__STAR_print_level_STAR_,((and__948__auto___7=clojure.core.not.apply(null,[clojure.core._STAR_print_dup_STAR_])),
((and__948__auto___7)?(((and__948__auto___8=clojure.core._STAR_print_level_STAR_),
((and__948__auto___8)?(clojure.lang.Numbers.dec(clojure.core._STAR_print_level_STAR_)):(and__948__auto___8)))):(and__948__auto___7)))])),
(function __try(){try{var _rtn=(((((and__948__auto___9=clojure.core._STAR_print_level_STAR_),
((and__948__auto___9)?(clojure.lang.Numbers.isNeg(clojure.core._STAR_print_level_STAR_)):(and__948__auto___9))))?((w_6).write("#")):((w_6).write(begin_1),
((temp__3253__auto___9=clojure.core.seq.apply(null,[sequence_5])),
((temp__3253__auto___9)?(((xs_10=temp__3253__auto___9),
((((and__948__auto___11=clojure.core.not.apply(null,[clojure.core._STAR_print_dup_STAR_])),
((and__948__auto___11)?(clojure.core._STAR_print_length_STAR_):(and__948__auto___11))))?(((G__4006_11=xs_10),
(vec__4007_12=G__4006_11),
(x_13=clojure.core.nth.apply(null,[vec__4007_12,(0),null])),
(xs_14=clojure.core.nthrest.apply(null,[vec__4007_12,(1)])),
(print_length_15=clojure.core._STAR_print_length_STAR_),
((function __loop(){var _rtn,_cnt;(G__4006_16=G__4006_11),
(print_length_17=print_length_15);do{_cnt=0;
_rtn=((vec__4008_18=G__4006_16),
(x_19=clojure.core.nth.apply(null,[vec__4008_18,(0),null])),
(xs_20=clojure.core.nthrest.apply(null,[vec__4008_18,(1)])),
(print_length_21=print_length_17),
((clojure.lang.Numbers.isZero(print_length_21))?((w_6).write("...")):(print_one_2.apply(null,[x_19,w_6]),
((xs_20)?((w_6).write(sep_3),
(_cnt=1,_rtn=[xs_20,clojure.lang.Numbers.dec(print_length_21)],G__4006_16=_rtn[0],print_length_17=_rtn[1])):(null)))))}while(_cnt);return _rtn;})()))):(((G__4010_11=xs_10),
(vec__4011_12=G__4010_11),
(x_13=clojure.core.nth.apply(null,[vec__4011_12,(0),null])),
(xs_14=clojure.core.nthrest.apply(null,[vec__4011_12,(1)])),
((function __loop(){var _rtn,_cnt;(G__4010_15=G__4010_11);do{_cnt=0;
_rtn=((vec__4012_16=G__4010_15),
(x_17=clojure.core.nth.apply(null,[vec__4012_16,(0),null])),
(xs_18=clojure.core.nthrest.apply(null,[vec__4012_16,(1)])),
print_one_2.apply(null,[x_17,w_6]),
((xs_18)?((w_6).write(sep_3),
(_cnt=1,_rtn=[xs_18],G__4010_15=_rtn[0])):(null)))}while(_cnt);return _rtn;})())))))):(null))),
(w_6).write(end_4))))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())})))}).apply(null,[]);

//======
//(defn- print-meta [o w] (when-let [m (meta o)] (when (and (pos? (count m)) (or *print-dup* (and *print-meta* *print-readably*))) (.write w "#^") (if (and (= (count m) 1) (:tag m)) (pr-on (:tag m) w) (pr-on m w)) (.write w " "))))
//---
(function __clojure_core_fn_4015(){
return (clojure.JS.def(clojure.core,"print_meta",(function __clojure_core_fn_4015_print_meta_4017(o_1,w_2){
var temp__3253__auto___3,m_4,and__948__auto___5,or__962__auto___6,and__948__auto___7,and__948__auto___5;
return (((temp__3253__auto___3=clojure.core.meta.apply(null,[o_1])),
((temp__3253__auto___3)?(((m_4=temp__3253__auto___3),
((((and__948__auto___5=clojure.lang.Numbers.isPos(clojure.core.count.apply(null,[m_4]))),
((and__948__auto___5)?(((or__962__auto___6=clojure.core._STAR_print_dup_STAR_),
((or__962__auto___6)?(or__962__auto___6):(((and__948__auto___7=clojure.core._STAR_print_meta_STAR_),
((and__948__auto___7)?(clojure.core._STAR_print_readably_STAR_):(and__948__auto___7))))))):(and__948__auto___5))))?((w_2).write("#^"),
((((and__948__auto___5=clojure.lang.Util.equiv(clojure.core.count.apply(null,[m_4]),(1))),
((and__948__auto___5)?(clojure.core.keyword("","tag").apply(null,[m_4])):(and__948__auto___5))))?(clojure.core.pr_on.apply(null,[clojure.core.keyword("","tag").apply(null,[m_4]),w_2])):(clojure.core.pr_on.apply(null,[m_4,w_2]))),
(w_2).write(" ")):(null)))):(null))))})))}).apply(null,[]);

//======
//(defmethod print-method nil [o w] (.write w "nil"))
//---
(function __clojure_core_fn_4021(){
return ((clojure.core.print_method).addMethod(null,(function __clojure_core_fn_4021_fn_4023(o_1,w_2){
return ((w_2).write("nil"))})))}).apply(null,[]);

//======
//(defmethod print-dup nil [o w] (print-method o w))
//---
(function __clojure_core_fn_4027(){
return ((clojure.core.print_dup).addMethod(null,(function __clojure_core_fn_4027_fn_4029(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defn print-ctor [o print-args w] (.write w "#=(") (.write w (RT/className (class o))) (.write w ". ") (print-args o w) (.write w ")"))
//---
(function __clojure_core_fn_4033(){
return (clojure.JS.def(clojure.core,"print_ctor",(function __clojure_core_fn_4033_print_ctor_4035(o_1,print_args_2,w_3){
return ((w_3).write("#=("),
(w_3).write(clojure.lang.RT.className(clojure.core.class_.apply(null,[o_1]))),
(w_3).write(". "),
print_args_2.apply(null,[o_1,w_3]),
(w_3).write(")"))})))}).apply(null,[]);

//======
//(defmethod print-method :default [o w] (.write w "#<") (.write w (RT/simpleClassName (class o))) (.write w " ") (.write w (str o)) (.write w ">"))
//---
(function __clojure_core_fn_4039(){
return ((clojure.core.print_method).addMethod(clojure.core.keyword("","default"),(function __clojure_core_fn_4039_fn_4041(o_1,w_2){
return ((w_2).write("#<"),
(w_2).write(clojure.lang.RT.simpleClassName(clojure.core.class_.apply(null,[o_1]))),
(w_2).write(" "),
(w_2).write(clojure.core.str.apply(null,[o_1])),
(w_2).write(">"))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.Keyword [o w] (.write w (str o)))
//---
(function __clojure_core_fn_4045(){
return ((clojure.core.print_method).addMethod(clojure.lang.Keyword,(function __clojure_core_fn_4045_fn_4047(o_1,w_2){
return ((w_2).write(clojure.core.str.apply(null,[o_1])))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.Keyword [o w] (print-method o w))
//---
(function __clojure_core_fn_4051(){
return ((clojure.core.print_dup).addMethod(clojure.lang.Keyword,(function __clojure_core_fn_4051_fn_4053(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method Number [o w] (.write w (str o)))
//---
(function __clojure_core_fn_4057(){
return ((clojure.core.print_method).addMethod(java.lang.Number,(function __clojure_core_fn_4057_fn_4059(o_1,w_2){
return ((w_2).write(clojure.core.str.apply(null,[o_1])))})))}).apply(null,[]);

//======
//(defmethod print-dup Number [o w] (print-ctor o (fn [o w] (print-dup (str o) w)) w))
//---
(function __clojure_core_fn_4063(){
return ((clojure.core.print_dup).addMethod(java.lang.Number,(function __clojure_core_fn_4063_fn_4065(o_1,w_2){
return (clojure.core.print_ctor.apply(null,[o_1,(function __clojure_core_fn_4063_fn_4065_fn_4067(o_1,w_2){
return (clojure.core.print_dup.apply(null,[clojure.core.str.apply(null,[o_1]),w_2]))}),w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.AFn [o w] (print-ctor o (fn [o w]) w))
//---
(function __clojure_core_fn_4072(){
return ((clojure.core.print_dup).addMethod(clojure.lang.AFn,(function __clojure_core_fn_4072_fn_4074(o_1,w_2){
return (clojure.core.print_ctor.apply(null,[o_1,(function __clojure_core_fn_4072_fn_4074_fn_4076(o_1,w_2){
return (null)}),w_2]))})))}).apply(null,[]);

//======
//(prefer-method print-dup clojure.lang.IPersistentCollection clojure.lang.AFn)
//---
(function __clojure_core_fn_4081(){
return (clojure.core.prefer_method.apply(null,[clojure.core.print_dup,clojure.lang.IPersistentCollection,clojure.lang.AFn]))}).apply(null,[]);

//======
//(prefer-method print-dup java.util.Map clojure.lang.AFn)
//---
(function __clojure_core_fn_4084(){
return (clojure.core.prefer_method.apply(null,[clojure.core.print_dup,java.util.Map,clojure.lang.AFn]))}).apply(null,[]);

//======
//(prefer-method print-dup java.util.Collection clojure.lang.AFn)
//---
(function __clojure_core_fn_4087(){
return (clojure.core.prefer_method.apply(null,[clojure.core.print_dup,java.util.Collection,clojure.lang.AFn]))}).apply(null,[]);

//======
//(defmethod print-method Boolean [o w] (.write w (str o)))
//---
(function __clojure_core_fn_4090(){
return ((clojure.core.print_method).addMethod(java.lang.Boolean,(function __clojure_core_fn_4090_fn_4092(o_1,w_2){
return ((w_2).write(clojure.core.str.apply(null,[o_1])))})))}).apply(null,[]);

//======
//(defmethod print-dup Boolean [o w] (print-method o w))
//---
(function __clojure_core_fn_4096(){
return ((clojure.core.print_dup).addMethod(java.lang.Boolean,(function __clojure_core_fn_4096_fn_4098(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defn print-simple [o w] (print-meta o w) (.write w (str o)))
//---
(function __clojure_core_fn_4102(){
return (clojure.JS.def(clojure.core,"print_simple",(function __clojure_core_fn_4102_print_simple_4104(o_1,w_2){
return (clojure.core.print_meta.apply(null,[o_1,w_2]),
(w_2).write(clojure.core.str.apply(null,[o_1])))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.Symbol [o w] (print-simple o w))
//---
(function __clojure_core_fn_4108(){
return ((clojure.core.print_method).addMethod(clojure.lang.Symbol,(function __clojure_core_fn_4108_fn_4110(o_1,w_2){
return (clojure.core.print_simple.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.Symbol [o w] (print-method o w))
//---
(function __clojure_core_fn_4114(){
return ((clojure.core.print_dup).addMethod(clojure.lang.Symbol,(function __clojure_core_fn_4114_fn_4116(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.Var [o w] (print-simple o w))
//---
(function __clojure_core_fn_4120(){
return ((clojure.core.print_method).addMethod(clojure.lang.Var,(function __clojure_core_fn_4120_fn_4122(o_1,w_2){
return (clojure.core.print_simple.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.Var [o w] (.write w (str "#=(var " (.name (.ns o)) "/" (.sym o) ")")))
//---
(function __clojure_core_fn_4126(){
return ((clojure.core.print_dup).addMethod(clojure.lang.Var,(function __clojure_core_fn_4126_fn_4128(o_1,w_2){
return ((w_2).write(clojure.core.str.apply(null,["#=(var ",clojure.JS.getOrRun(clojure.JS.getOrRun(o_1,"ns"),"name"),"/",clojure.JS.getOrRun(o_1,"sym"),")"])))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.ISeq [o w] (print-meta o w) (print-sequential "(" pr-on " " ")" o w))
//---
(function __clojure_core_fn_4132(){
return ((clojure.core.print_method).addMethod(clojure.lang.ISeq,(function __clojure_core_fn_4132_fn_4134(o_1,w_2){
return (clojure.core.print_meta.apply(null,[o_1,w_2]),
clojure.core.print_sequential.apply(null,["(",clojure.core.pr_on," ",")",o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.ISeq [o w] (print-method o w))
//---
(function __clojure_core_fn_4138(){
return ((clojure.core.print_dup).addMethod(clojure.lang.ISeq,(function __clojure_core_fn_4138_fn_4140(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.IPersistentList [o w] (print-method o w))
//---
(function __clojure_core_fn_4144(){
return ((clojure.core.print_dup).addMethod(clojure.lang.IPersistentList,(function __clojure_core_fn_4144_fn_4146(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(prefer-method print-method clojure.lang.IPersistentList clojure.lang.ISeq)
//---
(function __clojure_core_fn_4150(){
return (clojure.core.prefer_method.apply(null,[clojure.core.print_method,clojure.lang.IPersistentList,clojure.lang.ISeq]))}).apply(null,[]);

//======
//(prefer-method print-dup clojure.lang.IPersistentList clojure.lang.ISeq)
//---
(function __clojure_core_fn_4153(){
return (clojure.core.prefer_method.apply(null,[clojure.core.print_dup,clojure.lang.IPersistentList,clojure.lang.ISeq]))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentList [o w] (print-meta o w) (print-sequential "(" print-method " " ")" o w))
//---
(function __clojure_core_fn_4156(){
return ((clojure.core.print_method).addMethod(clojure.lang.IPersistentList,(function __clojure_core_fn_4156_fn_4158(o_1,w_2){
return (clojure.core.print_meta.apply(null,[o_1,w_2]),
clojure.core.print_sequential.apply(null,["(",clojure.core.print_method," ",")",o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup java.util.Collection [o w] (print-ctor o (fn* [p1__4162 p2__4163] (print-sequential "[" print-dup " " "]" p1__4162 p2__4163)) w))
//---
(function __clojure_core_fn_4164(){
return ((clojure.core.print_dup).addMethod(java.util.Collection,(function __clojure_core_fn_4164_fn_4166(o_1,w_2){
return (clojure.core.print_ctor.apply(null,[o_1,(function __clojure_core_fn_4164_fn_4166_fn_4168(p1__4162_1,p2__4163_2){
return (clojure.core.print_sequential.apply(null,["[",clojure.core.print_dup," ","]",p1__4162_1,p2__4163_2]))}),w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.IPersistentCollection [o w] (print-meta o w) (.write w "#=(") (.write w (RT/className (class o))) (.write w "/create ") (print-sequential "[" print-dup " " "]" o w) (.write w ")"))
//---
(function __clojure_core_fn_4173(){
return ((clojure.core.print_dup).addMethod(clojure.lang.IPersistentCollection,(function __clojure_core_fn_4173_fn_4175(o_1,w_2){
return (clojure.core.print_meta.apply(null,[o_1,w_2]),
(w_2).write("#=("),
(w_2).write(clojure.lang.RT.className(clojure.core.class_.apply(null,[o_1]))),
(w_2).write("/create "),
clojure.core.print_sequential.apply(null,["[",clojure.core.print_dup," ","]",o_1,w_2]),
(w_2).write(")"))})))}).apply(null,[]);

//======
//(prefer-method print-dup clojure.lang.IPersistentCollection java.util.Collection)
//---
(function __clojure_core_fn_4179(){
return (clojure.core.prefer_method.apply(null,[clojure.core.print_dup,clojure.lang.IPersistentCollection,java.util.Collection]))}).apply(null,[]);

//======
//(def char-escape-string {\newline "\\n", \tab "\\t", \return "\\r", \" "\\\"", \\ "\\\\", \formfeed "\\f", \backspace "\\b"})
//---
(function __clojure_core_fn_4182(){
return (clojure.JS.def(clojure.core,"char_escape_string",clojure.core.hash_map("\n","\\n","\t","\\t","\r","\\r","\"","\\\"","\\","\\\\","\f","\\f","\b","\\b")))}).apply(null,[]);

//======
//(defmethod print-method String [s w] (if (or *print-dup* *print-readably*) (do (.append w \") (dotimes [n (count s)] (let [c (.charAt s n) e (char-escape-string c)] (if e (.write w e) (.append w c)))) (.append w \")) (.write w s)) nil)
//---
(function __clojure_core_fn_4185(){
return ((clojure.core.print_method).addMethod(java.lang.String,(function __clojure_core_fn_4185_fn_4187(s_1,w_2){
var or__962__auto___3,n__2101__auto___3,n_4,c_5,e_6;
return (((((or__962__auto___3=clojure.core._STAR_print_dup_STAR_),
((or__962__auto___3)?(or__962__auto___3):(clojure.core._STAR_print_readably_STAR_))))?((w_2).append("\""),
((n__2101__auto___3=clojure.lang.RT.intCast(clojure.core.count.apply(null,[s_1]))),
((function __loop(){var _rtn,_cnt;(n_4=clojure.lang.RT.intCast((0)));do{_cnt=0;
_rtn=((clojure.lang.Numbers.lt(n_4,n__2101__auto___3))?(((c_5=(s_1).charAt(n_4)),
(e_6=clojure.core.char_escape_string.apply(null,[c_5])),
((e_6)?((w_2).write(e_6)):((w_2).append(c_5)))),
(_cnt=1,_rtn=[clojure.lang.Numbers.unchecked_inc(n_4)],n_4=_rtn[0])):(null))}while(_cnt);return _rtn;})())),
(w_2).append("\"")):((w_2).write(s_1))),
null)})))}).apply(null,[]);

//======
//(defmethod print-dup String [s w] (print-method s w))
//---
(function __clojure_core_fn_4191(){
return ((clojure.core.print_dup).addMethod(java.lang.String,(function __clojure_core_fn_4191_fn_4193(s_1,w_2){
return (clojure.core.print_method.apply(null,[s_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentVector [v w] (print-meta v w) (print-sequential "[" pr-on " " "]" v w))
//---
(function __clojure_core_fn_4197(){
return ((clojure.core.print_method).addMethod(clojure.lang.IPersistentVector,(function __clojure_core_fn_4197_fn_4199(v_1,w_2){
return (clojure.core.print_meta.apply(null,[v_1,w_2]),
clojure.core.print_sequential.apply(null,["[",clojure.core.pr_on," ","]",v_1,w_2]))})))}).apply(null,[]);

//======
//(defn- print-map [m print-one w] (print-sequential "{" (fn [e w] (do (print-one (key e) w) (.append w \space) (print-one (val e) w))) ", " "}" (seq m) w))
//---
(function __clojure_core_fn_4203(){
return (clojure.JS.def(clojure.core,"print_map",(function __clojure_core_fn_4203_print_map_4205(m_1,print_one_2,w_3){
return (clojure.core.print_sequential.apply(null,["{",(function __clojure_core_fn_4203_print_map_4205_fn_4207(e_1,w_2){
return (print_one_2.apply(null,[clojure.core.key.apply(null,[e_1]),w_2]),
(w_2).append(" "),
print_one_2.apply(null,[clojure.core.val.apply(null,[e_1]),w_2]))}),", ","}",clojure.core.seq.apply(null,[m_1]),w_3]))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentMap [m w] (print-meta m w) (print-map m pr-on w))
//---
(function __clojure_core_fn_4212(){
return ((clojure.core.print_method).addMethod(clojure.lang.IPersistentMap,(function __clojure_core_fn_4212_fn_4214(m_1,w_2){
return (clojure.core.print_meta.apply(null,[m_1,w_2]),
clojure.core.print_map.apply(null,[m_1,clojure.core.pr_on,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup java.util.Map [m w] (print-ctor m (fn* [p1__4218 p2__4219] (print-map (seq p1__4218) print-dup p2__4219)) w))
//---
(function __clojure_core_fn_4220(){
return ((clojure.core.print_dup).addMethod(java.util.Map,(function __clojure_core_fn_4220_fn_4222(m_1,w_2){
return (clojure.core.print_ctor.apply(null,[m_1,(function __clojure_core_fn_4220_fn_4222_fn_4224(p1__4218_1,p2__4219_2){
return (clojure.core.print_map.apply(null,[clojure.core.seq.apply(null,[p1__4218_1]),clojure.core.print_dup,p2__4219_2]))}),w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.IPersistentMap [m w] (print-meta m w) (.write w "#=(") (.write w (RT/className (class m))) (.write w "/create ") (print-map m print-dup w) (.write w ")"))
//---
(function __clojure_core_fn_4229(){
return ((clojure.core.print_dup).addMethod(clojure.lang.IPersistentMap,(function __clojure_core_fn_4229_fn_4231(m_1,w_2){
return (clojure.core.print_meta.apply(null,[m_1,w_2]),
(w_2).write("#=("),
(w_2).write(clojure.lang.RT.className(clojure.core.class_.apply(null,[m_1]))),
(w_2).write("/create "),
clojure.core.print_map.apply(null,[m_1,clojure.core.print_dup,w_2]),
(w_2).write(")"))})))}).apply(null,[]);

//======
//(prefer-method print-dup clojure.lang.IPersistentCollection java.util.Map)
//---
(function __clojure_core_fn_4235(){
return (clojure.core.prefer_method.apply(null,[clojure.core.print_dup,clojure.lang.IPersistentCollection,java.util.Map]))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentSet [s w] (print-meta s w) (print-sequential "#{" pr-on " " "}" (seq s) w))
//---
(function __clojure_core_fn_4238(){
return ((clojure.core.print_method).addMethod(clojure.lang.IPersistentSet,(function __clojure_core_fn_4238_fn_4240(s_1,w_2){
return (clojure.core.print_meta.apply(null,[s_1,w_2]),
clojure.core.print_sequential.apply(null,["#{",clojure.core.pr_on," ","}",clojure.core.seq.apply(null,[s_1]),w_2]))})))}).apply(null,[]);

//======
//(def char-name-string {\newline "newline", \tab "tab", \space "space", \backspace "backspace", \formfeed "formfeed", \return "return"})
//---
(function __clojure_core_fn_4244(){
return (clojure.JS.def(clojure.core,"char_name_string",clojure.core.hash_map("\n","newline","\t","tab"," ","space","\b","backspace","\f","formfeed","\r","return")))}).apply(null,[]);

//======
//(defmethod print-method java.lang.Character [c w] (if (or *print-dup* *print-readably*) (do (.append w \\) (let [n (char-name-string c)] (if n (.write w n) (.append w c)))) (.append w c)) nil)
//---
(function __clojure_core_fn_4247(){
return ((clojure.core.print_method).addMethod(java.lang.Character,(function __clojure_core_fn_4247_fn_4249(c_1,w_2){
var or__962__auto___3,n_3;
return (((((or__962__auto___3=clojure.core._STAR_print_dup_STAR_),
((or__962__auto___3)?(or__962__auto___3):(clojure.core._STAR_print_readably_STAR_))))?((w_2).append("\\"),
((n_3=clojure.core.char_name_string.apply(null,[c_1])),
((n_3)?((w_2).write(n_3)):((w_2).append(c_1))))):((w_2).append(c_1))),
null)})))}).apply(null,[]);

//======
//(defmethod print-dup java.lang.Character [c w] (print-method c w))
//---
(function __clojure_core_fn_4253(){
return ((clojure.core.print_dup).addMethod(java.lang.Character,(function __clojure_core_fn_4253_fn_4255(c_1,w_2){
return (clojure.core.print_method.apply(null,[c_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup java.lang.Integer [o w] (print-method o w))
//---
(function __clojure_core_fn_4259(){
return ((clojure.core.print_dup).addMethod(java.lang.Integer,(function __clojure_core_fn_4259_fn_4261(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup java.lang.Double [o w] (print-method o w))
//---
(function __clojure_core_fn_4265(){
return ((clojure.core.print_dup).addMethod(java.lang.Double,(function __clojure_core_fn_4265_fn_4267(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.Ratio [o w] (print-method o w))
//---
(function __clojure_core_fn_4271(){
return ((clojure.core.print_dup).addMethod(clojure.lang.Ratio,(function __clojure_core_fn_4271_fn_4273(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup java.math.BigDecimal [o w] (print-method o w))
//---
(function __clojure_core_fn_4277(){
return ((clojure.core.print_dup).addMethod(java.math.BigDecimal,(function __clojure_core_fn_4277_fn_4279(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.PersistentHashMap [o w] (print-method o w))
//---
(function __clojure_core_fn_4283(){
return ((clojure.core.print_dup).addMethod(clojure.lang.PersistentHashMap,(function __clojure_core_fn_4283_fn_4285(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.PersistentHashSet [o w] (print-method o w))
//---
(function __clojure_core_fn_4289(){
return ((clojure.core.print_dup).addMethod(clojure.lang.PersistentHashSet,(function __clojure_core_fn_4289_fn_4291(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.PersistentVector [o w] (print-method o w))
//---
(function __clojure_core_fn_4295(){
return ((clojure.core.print_dup).addMethod(clojure.lang.PersistentVector,(function __clojure_core_fn_4295_fn_4297(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.LazilyPersistentVector [o w] (print-method o w))
//---
(function __clojure_core_fn_4301(){
return ((clojure.core.print_dup).addMethod(clojure.lang.LazilyPersistentVector,(function __clojure_core_fn_4301_fn_4303(o_1,w_2){
return (clojure.core.print_method.apply(null,[o_1,w_2]))})))}).apply(null,[]);
// Skipping: (def primitives-classnames {Float/TYPE "Float/TYPE", Integer/TYPE "Integer/TYPE", Long/TYPE "Long/TYPE", Boolean/TYPE "Boolean/TYPE", Character/TYPE "Character/TYPE", Double/TYPE "Double/TYPE", Byte/TYPE "Byte/TYPE", Short/TYPE "Short/TYPE"})
// Skipping: (defmethod print-method Class [c w] (.write w (RT/className c)))
// Skipping: (defmethod print-dup Class [c w] (cond (.isPrimitive c) (do (.write w "#=(identity ") (.write w (primitives-classnames c)) (.write w ")")) (.isArray c) (do (.write w "#=(java.lang.Class/forName \"") (.write w (RT/className c)) (.write w "\")")) :else (do (.write w "#=") (.write w (RT/className c)))))

//======
//(defmethod print-method java.math.BigDecimal [b w] (.write w (str b)) (.write w "M"))
//---
(function __clojure_core_fn_4322(){
return ((clojure.core.print_method).addMethod(java.math.BigDecimal,(function __clojure_core_fn_4322_fn_4324(b_1,w_2){
return ((w_2).write(clojure.core.str.apply(null,[b_1])),
(w_2).write("M"))})))}).apply(null,[]);

//======
//(defmethod print-method java.util.regex.Pattern [p w] (.write w "#\"") (loop [[c & r :as s] (seq (.pattern p)) qmode false] (when s (cond (= c \\) (let [[c2 & r2] r] (.append w \\) (.append w c2) (if qmode (recur r2 (not= c2 \E)) (recur r2 (= c2 \Q)))) (= c \") (do (if qmode (.write w "\\E\\\"\\Q") (.write w "\\\"")) (recur r qmode)) :else (do (.append w c) (recur r qmode))))) (.append w \"))
//---
(function __clojure_core_fn_4328(){
return ((clojure.core.print_method).addMethod(java.util.regex.Pattern,(function __clojure_core_fn_4328_fn_4330(p_1,w_2){
var r2_18,c_5,s_7,s_14,vec__4335_11,qmode_10,r_6,G__4333_3,vec__4336_16,qmode_8,G__4333_9,r_13,vec__4334_4,c_12,c2_17,qmode_15;
return ((w_2).write("#\""),
((G__4333_3=clojure.core.seq.apply(null,[(p_1).pattern()])),
(vec__4334_4=G__4333_3),
(c_5=clojure.core.nth.apply(null,[vec__4334_4,(0),null])),
(r_6=clojure.core.nthrest.apply(null,[vec__4334_4,(1)])),
(s_7=vec__4334_4),
(qmode_8=false),
((function __loop(){var _rtn,_cnt;(G__4333_9=G__4333_3),
(qmode_10=qmode_8);do{_cnt=0;
_rtn=((vec__4335_11=G__4333_9),
(c_12=clojure.core.nth.apply(null,[vec__4335_11,(0),null])),
(r_13=clojure.core.nthrest.apply(null,[vec__4335_11,(1)])),
(s_14=vec__4335_11),
(qmode_15=qmode_10),
((s_14)?(((clojure.lang.Util.equiv(c_12,"\\"))?(((vec__4336_16=r_13),
(c2_17=clojure.core.nth.apply(null,[vec__4336_16,(0),null])),
(r2_18=clojure.core.nthrest.apply(null,[vec__4336_16,(1)])),
(w_2).append("\\"),
(w_2).append(c2_17),
((qmode_15)?((_cnt=1,_rtn=[r2_18,clojure.core.not_EQ_.apply(null,[c2_17,"E"])],G__4333_9=_rtn[0],qmode_10=_rtn[1])):((_cnt=1,_rtn=[r2_18,clojure.lang.Util.equiv(c2_17,"Q")],G__4333_9=_rtn[0],qmode_10=_rtn[1]))))):(((clojure.lang.Util.equiv(c_12,"\""))?(((qmode_15)?((w_2).write("\\E\\\"\\Q")):((w_2).write("\\\""))),
(_cnt=1,_rtn=[r_13,qmode_15],G__4333_9=_rtn[0],qmode_10=_rtn[1])):(((clojure.core.keyword("","else"))?((w_2).append(c_12),
(_cnt=1,_rtn=[r_13,qmode_15],G__4333_9=_rtn[0],qmode_10=_rtn[1])):(null))))))):(null)))}while(_cnt);return _rtn;})())),
(w_2).append("\""))})))}).apply(null,[]);

//======
//(defmethod print-dup java.util.regex.Pattern [p w] (print-method p w))
//---
(function __clojure_core_fn_4339(){
return ((clojure.core.print_dup).addMethod(java.util.regex.Pattern,(function __clojure_core_fn_4339_fn_4341(p_1,w_2){
return (clojure.core.print_method.apply(null,[p_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-dup clojure.lang.Namespace [n w] (.write w "#=(find-ns ") (print-dup (.name n) w) (.write w ")"))
//---
(function __clojure_core_fn_4345(){
return ((clojure.core.print_dup).addMethod(clojure.lang.Namespace,(function __clojure_core_fn_4345_fn_4347(n_1,w_2){
return ((w_2).write("#=(find-ns "),
clojure.core.print_dup.apply(null,[clojure.JS.getOrRun(n_1,"name"),w_2]),
(w_2).write(")"))})))}).apply(null,[]);

//======
//(def print-initialized true)
//---
(function __clojure_core_fn_4351(){
return (clojure.JS.def(clojure.core,"print_initialized",true))}).apply(null,[]);
