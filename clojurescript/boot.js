
//======
//(in-ns (quote clojure))
//---
(function __tmp_fn_2674(){
return (clojure.in_ns.apply(null,["'clojure"]))}).apply(null,[]);

//======
//(def list (. clojure.lang.PersistentList creator))
//---
(function __clojure_fn_2683(){
return (clojure.JS.def(clojure,"list",clojure.lang.PersistentList.creator))}).apply(null,[]);

//======
//(def cons (fn* cons [x seq] (. clojure.lang.RT (cons x seq))))
//---
(function __clojure_fn_2691(){
return (clojure.JS.def(clojure,"cons",(function __clojure_fn_2691_cons_2693(x_1,seq_2){
var cons_0=arguments.callee;
return (clojure.lang.RT.cons(x_1,seq_2))})))}).apply(null,[]);
// Skipping: (def let (fn* let [& decl] (cons (quote let*) decl)))
// Skipping: (def loop (fn* loop [& decl] (cons (quote loop*) decl)))
// Skipping: (def fn (fn* fn [& decl] (cons (quote fn*) decl)))
// Skipping: (def first (fn first [coll] (. clojure.lang.RT (first coll))))
// Skipping: (def rest (fn rest [x] (. clojure.lang.RT (rest x))))

//======
//(def conj (fn conj ([coll x] (. clojure.lang.RT (conj coll x))) ([coll x & xs] (if xs (recur (conj coll x) (first xs) (rest xs)) (conj coll x)))))
//---
(function __clojure_fn_2726(){
return (clojure.JS.def(clojure,"conj",clojure.JS.variadic(2,(function __clojure_fn_2726_conj_2728(coll_1,x_2){switch(arguments.length){
case 2:var conj_0=arguments.callee;
return (clojure.lang.RT.conj(coll_1,x_2))}
var _cnt,_rtn,xs_3,conj_0=arguments.callee,xs_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((xs_3)?((_cnt=1,_rtn=[conj_0.apply(null,[coll_1,x_2]),clojure.first.apply(null,[xs_3]),clojure.rest.apply(null,[xs_3])],coll_1=_rtn[0],x_2=_rtn[1],xs_3=_rtn[2])):(conj_0.apply(null,[coll_1,x_2])))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(def second (fn second [x] (first (rest x))))
//---
(function __clojure_fn_2736(){
return (clojure.JS.def(clojure,"second",(function __clojure_fn_2736_second_2738(x_1){
var second_0=arguments.callee;
return (clojure.first.apply(null,[clojure.rest.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def ffirst (fn ffirst [x] (first (first x))))
//---
(function __clojure_fn_2746(){
return (clojure.JS.def(clojure,"ffirst",(function __clojure_fn_2746_ffirst_2748(x_1){
var ffirst_0=arguments.callee;
return (clojure.first.apply(null,[clojure.first.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def rfirst (fn rfirst [x] (rest (first x))))
//---
(function __clojure_fn_2756(){
return (clojure.JS.def(clojure,"rfirst",(function __clojure_fn_2756_rfirst_2758(x_1){
var rfirst_0=arguments.callee;
return (clojure.rest.apply(null,[clojure.first.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def frest (fn frest [x] (first (rest x))))
//---
(function __clojure_fn_2766(){
return (clojure.JS.def(clojure,"frest",(function __clojure_fn_2766_frest_2768(x_1){
var frest_0=arguments.callee;
return (clojure.first.apply(null,[clojure.rest.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def rrest (fn rrest [x] (rest (rest x))))
//---
(function __clojure_fn_2776(){
return (clojure.JS.def(clojure,"rrest",(function __clojure_fn_2776_rrest_2778(x_1){
var rrest_0=arguments.callee;
return (clojure.rest.apply(null,[clojure.rest.apply(null,[x_1])]))})))}).apply(null,[]);
// Skipping: (def seq (fn seq [coll] (. clojure.lang.RT (seq coll))))
// Skipping: (def instance? (fn instance? [c x] (. c (isInstance x))))

//======
//(def seq? (fn seq? [x] (instance? clojure.lang.ISeq x)))
//---
(function __clojure_fn_2796(){
return (clojure.JS.def(clojure,"seq_QMARK_",(function __clojure_fn_2796_seq_QMARK_2798(x_1){
var seq_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_.apply(null,[clojure.lang.ISeq,x_1]))})))}).apply(null,[]);
// Skipping: (def string? (fn string? [x] (instance? String x)))

//======
//(def map? (fn map? [x] (instance? clojure.lang.IPersistentMap x)))
//---
(function __clojure_fn_2811(){
return (clojure.JS.def(clojure,"map_QMARK_",(function __clojure_fn_2811_map_QMARK_2813(x_1){
var map_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentMap,x_1]))})))}).apply(null,[]);

//======
//(def vector? (fn vector? [x] (instance? clojure.lang.IPersistentVector x)))
//---
(function __clojure_fn_2821(){
return (clojure.JS.def(clojure,"vector_QMARK_",(function __clojure_fn_2821_vector_QMARK_2823(x_1){
var vector_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentVector,x_1]))})))}).apply(null,[]);

//======
//(def sigs (fn [fdecl] (if (seq? (first fdecl)) (loop [ret [] fdecl fdecl] (if fdecl (recur (conj ret (first (first fdecl))) (rest fdecl)) (seq ret))) (list (first fdecl)))))
//---
(function __clojure_fn_2832(){
return (clojure.JS.def(clojure,"sigs",(function __clojure_fn_2832_sigs_2834(fdecl_1){
var fdecl_3,ret_2;
return (((clojure.seq_QMARK_.apply(null,[clojure.first.apply(null,[fdecl_1])]))?(((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentVector.EMPTY),
(fdecl_3=fdecl_1);do{_cnt=0;
_rtn=((fdecl_3)?((_cnt=1,_rtn=[clojure.conj.apply(null,[ret_2,clojure.first.apply(null,[clojure.first.apply(null,[fdecl_3])])]),clojure.rest.apply(null,[fdecl_3])],ret_2=_rtn[0],fdecl_3=_rtn[1])):(clojure.seq.apply(null,[ret_2])))}while(_cnt);return _rtn;})())):(clojure.list.apply(null,[clojure.first.apply(null,[fdecl_1])]))))})))}).apply(null,[]);
// Skipping: (def assoc (fn assoc ([map key val] (. clojure.lang.RT (assoc map key val))) ([map key val & kvs] (let [ret (assoc map key val)] (if kvs (recur ret (first kvs) (second kvs) (rrest kvs)) ret)))))

//======
//(def meta (fn meta [x] (if (instance? clojure.lang.IObj x) (. x (meta)))))
//---
(function __clojure_fn_2848(){
return (clojure.JS.def(clojure,"meta",(function __clojure_fn_2848_meta_2850(x_1){
var meta_0=arguments.callee;
return (((clojure.instance_QMARK_.apply(null,[clojure.lang.IObj,x_1]))?((x_1).meta()):(null)))})))}).apply(null,[]);

//======
//(def with-meta (fn with-meta [x m] (. x (withMeta m))))
//---
(function __clojure_fn_2858(){
return (clojure.JS.def(clojure,"with_meta",(function __clojure_fn_2858_with_meta_2860(x_1,m_2){
var with_meta_0=arguments.callee;
return ((x_1).withMeta(m_2))})))}).apply(null,[]);

//======
//(def last (fn last [s] (if (rest s) (recur (rest s)) (first s))))
//---
(function __clojure_fn_2868(){
return (clojure.JS.def(clojure,"last",(function __clojure_fn_2868_last_2870(s_1){
var _cnt,_rtn,last_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.rest.apply(null,[s_1]))?((_cnt=1,_rtn=[clojure.rest.apply(null,[s_1])],s_1=_rtn[0])):(clojure.first.apply(null,[s_1])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def butlast (fn butlast [s] (loop [ret [] s s] (if (rest s) (recur (conj ret (first s)) (rest s)) (seq ret)))))
//---
(function __clojure_fn_2878(){
return (clojure.JS.def(clojure,"butlast",(function __clojure_fn_2878_butlast_2880(s_1){
var s_3,ret_2,butlast_0=arguments.callee;
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentVector.EMPTY),
(s_3=s_1);do{_cnt=0;
_rtn=((clojure.rest.apply(null,[s_3]))?((_cnt=1,_rtn=[clojure.conj.apply(null,[ret_2,clojure.first.apply(null,[s_3])]),clojure.rest.apply(null,[s_3])],ret_2=_rtn[0],s_3=_rtn[1])):(clojure.seq.apply(null,[ret_2])))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);
// Skipping: (def defn (fn defn [name & fdecl] (let [m (if (string? (first fdecl)) {:doc (first fdecl)} {}) fdecl (if (string? (first fdecl)) (rest fdecl) fdecl) m (if (map? (first fdecl)) (conj m (first fdecl)) m) fdecl (if (map? (first fdecl)) (rest fdecl) fdecl) fdecl (if (vector? (first fdecl)) (list fdecl) fdecl) m (if (map? (last fdecl)) (conj m (last fdecl)) m) fdecl (if (map? (last fdecl)) (butlast fdecl) fdecl) m (conj {:arglists (list (quote quote) (sigs fdecl))} m)] (list (quote def) (with-meta name (conj (if (meta name) (meta name) {}) m)) (cons (quote clojure/fn) fdecl)))))
// Skipping: (. (var defn) (setMacro))

//======
//(defn cast "Throws a ClassCastException if x is not a c, else returns x." [c x] (. c (cast x)))
//---
(function __clojure_fn_2897(){
return (clojure.JS.def(clojure,"cast",(function __clojure_fn_2897_cast_2899(c_1,x_2){
return ((c_1).cast(x_2))})))}).apply(null,[]);

//======
//(defn to-array "Returns an array of Objects containing the contents of coll, which\n  can be any Collection.  Maps to java.util.Collection.toArray()." [coll] (. clojure.lang.RT (toArray coll)))
//---
(function __clojure_fn_2909(){
return (clojure.JS.def(clojure,"to_array",(function __clojure_fn_2909_to_array_2911(coll_1){
return (clojure.lang.RT.toArray(coll_1))})))}).apply(null,[]);

//======
//(defn vector "Creates a new vector containing the args." ([] []) ([& args] (. clojure.lang.LazilyPersistentVector (create args))))
//---
(function __clojure_fn_2922(){
return (clojure.JS.def(clojure,"vector",clojure.JS.variadic(0,(function __clojure_fn_2922_vector_2924(){switch(arguments.length){
case 0:return (clojure.lang.PersistentVector.EMPTY)}
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.LazilyPersistentVector.create(args_1))}))))}).apply(null,[]);

//======
//(defn vec "Creates a new vector containing the contents of coll." ([coll] (. clojure.lang.LazilyPersistentVector (createOwning (to-array coll)))))
//---
(function __clojure_fn_2935(){
return (clojure.JS.def(clojure,"vec",(function __clojure_fn_2935_vec_2937(coll_1){
return (clojure.lang.LazilyPersistentVector.createOwning(clojure.to_array.apply(null,[coll_1])))})))}).apply(null,[]);
// Skipping: (defn hash-map "keyval => key val\n  Returns a new hash map with supplied mappings." ([] {}) ([& keyvals] (. clojure.lang.PersistentHashMap (create keyvals))))

//======
//(defn hash-set "Returns a new hash set with supplied keys." ([] #{}) ([& keys] (. clojure.lang.PersistentHashSet (create keys))))
//---
(function __clojure_fn_2955(){
return (clojure.JS.def(clojure,"hash_set",clojure.JS.variadic(0,(function __clojure_fn_2955_hash_set_2957(){switch(arguments.length){
case 0:return (clojure.lang.PersistentHashSet.EMPTY)}
var keys_1,keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentHashSet.create(keys_1))}))))}).apply(null,[]);

//======
//(defn sorted-map "keyval => key val\n  Returns a new sorted map with supplied mappings." ([& keyvals] (. clojure.lang.PersistentTreeMap (create keyvals))))
//---
(function __clojure_fn_2968(){
return (clojure.JS.def(clojure,"sorted_map",clojure.JS.variadic(0,(function __clojure_fn_2968_sorted_map_2970(){
var keyvals_1,keyvals_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentTreeMap.create(keyvals_1))}))))}).apply(null,[]);

//======
//(defn sorted-set "Returns a new sorted set with supplied keys." ([& keys] (. clojure.lang.PersistentTreeSet (create keys))))
//---
(function __clojure_fn_2980(){
return (clojure.JS.def(clojure,"sorted_set",clojure.JS.variadic(0,(function __clojure_fn_2980_sorted_set_2982(){
var keys_1,keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentTreeSet.create(keys_1))}))))}).apply(null,[]);

//======
//(defn sorted-map-by "keyval => key val\n  Returns a new sorted map with supplied mappings, using the supplied comparator." ([comparator & keyvals] (. clojure.lang.PersistentTreeMap (create comparator keyvals))))
//---
(function __clojure_fn_2992(){
return (clojure.JS.def(clojure,"sorted_map_by",clojure.JS.variadic(1,(function __clojure_fn_2992_sorted_map_by_2994(comparator_1){
var keyvals_2,keyvals_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentTreeMap.create(comparator_1,keyvals_2))}))))}).apply(null,[]);
// Skipping: (def defmacro (fn [name & args] (list (quote do) (cons (quote clojure/defn) (cons name args)) (list (quote .) (list (quote var) name) (quote (setMacro))))))
// Skipping: (. (var defmacro) (setMacro))
// Skipping: (defmacro when "Evaluates test. If logical true, evaluates body in an implicit do." [test & body] (list (quote if) test (cons (quote do) body)))
// Skipping: (defmacro when-not "Evaluates test. If logical false, evaluates body in an implicit do." [test & body] (list (quote if) test nil (cons (quote do) body)))

//======
//(defn nil? "Returns true if x is nil, false otherwise." {:tag Boolean} [x] (identical? x nil))
//---
(function __clojure_fn_3025(){
return (clojure.JS.def(clojure,"nil_QMARK_",(function __clojure_fn_3025_nil_QMARK_3027(x_1){
return (clojure.identical_QMARK_.apply(null,[x_1,null]))})))}).apply(null,[]);

//======
//(defn false? "Returns true if x is the value false, false otherwise." {:tag Boolean} [x] (identical? x false))
//---
(function __clojure_fn_3037(){
return (clojure.JS.def(clojure,"false_QMARK_",(function __clojure_fn_3037_false_QMARK_3039(x_1){
return (clojure.identical_QMARK_.apply(null,[x_1,false]))})))}).apply(null,[]);

//======
//(defn true? "Returns true if x is the value true, false otherwise." {:tag Boolean} [x] (identical? x true))
//---
(function __clojure_fn_3049(){
return (clojure.JS.def(clojure,"true_QMARK_",(function __clojure_fn_3049_true_QMARK_3051(x_1){
return (clojure.identical_QMARK_.apply(null,[x_1,true]))})))}).apply(null,[]);

//======
//(defn not "Returns true if x is logical false, false otherwise." {:tag Boolean} [x] (if x false true))
//---
(function __clojure_fn_3061(){
return (clojure.JS.def(clojure,"not",(function __clojure_fn_3061_not_3063(x_1){
return (((x_1)?(false):(true)))})))}).apply(null,[]);

//======
//(defn str "With no args, returns the empty string. With one arg x, returns\n  x.toString().  (str nil) returns the empty string. With more than\n  one arg, returns the concatenation of the str values of the args." {:tag String} ([] "") ([x] (if (nil? x) "" (. x (toString)))) ([x & ys] (loop [sb (clojure.lang.RT/makeStringBuilder (str x)) more ys] (if more (recur (. sb (append (str (first more)))) (rest more)) (str sb)))))
//---
(function __clojure_fn_3075(){
return (clojure.JS.def(clojure,"str",clojure.JS.variadic(1,(function __clojure_fn_3075_str_3077(x_1){switch(arguments.length){
case 1:return (((clojure.nil_QMARK_.apply(null,[x_1]))?(""):((x_1).toString())))
case 0:return ("")}
var ys_2,sb_3,more_4,ys_2=clojure.JS.rest_args(this,arguments,1);
return (((function __loop(){var _rtn,_cnt;(sb_3=clojure.lang.RT.makeStringBuilder(clojure.str.apply(null,[x_1]))),
(more_4=ys_2);do{_cnt=0;
_rtn=((more_4)?((_cnt=1,_rtn=[(sb_3).append(clojure.str.apply(null,[clojure.first.apply(null,[more_4])])),clojure.rest.apply(null,[more_4])],sb_3=_rtn[0],more_4=_rtn[1])):(clojure.str.apply(null,[sb_3])))}while(_cnt);return _rtn;})()))}))))}).apply(null,[]);

//======
//(defn symbol "Returns a Symbol with the given namespace and name." ([name] (. clojure.lang.Symbol (intern name))) ([ns name] (. clojure.lang.Symbol (intern ns name))))
//---
(function __clojure_fn_3090(){
return (clojure.JS.def(clojure,"symbol",(function __clojure_fn_3090_symbol_3092(ns_1,name_2){switch(arguments.length){
case 1:var name_1=arguments[0];
return (clojure.lang.Symbol.intern(name_1))}
return (clojure.lang.Symbol.intern(ns_1,name_2))})))}).apply(null,[]);

//======
//(defn keyword "Returns a Keyword with the given namespace and name.  Do not use :\n  in the keyword strings, it will be added automatically." ([name] (. clojure.lang.Keyword (intern nil name))) ([ns name] (. clojure.lang.Keyword (intern ns name))))
//---
(function __clojure_fn_3104(){
return (clojure.JS.def(clojure,"keyword",(function __clojure_fn_3104_keyword_3106(ns_1,name_2){switch(arguments.length){
case 1:var name_1=arguments[0];
return (clojure.lang.Keyword.intern(null,name_1))}
return (clojure.lang.Keyword.intern(ns_1,name_2))})))}).apply(null,[]);

//======
//(defn gensym "Returns a new symbol with a unique name. If a prefix string is\n  supplied, the name is prefix# where # is some unique number. If\n  prefix is not supplied, the prefix is 'G'." ([] (gensym "G__")) ([prefix-string] (. clojure.lang.Symbol (intern (str prefix-string (str (. clojure.lang.RT (nextID))))))))
//---
(function __clojure_fn_3118(){
return (clojure.JS.def(clojure,"gensym",(function __clojure_fn_3118_gensym_3120(prefix_string_1){switch(arguments.length){
case 0:return (clojure.gensym.apply(null,["G__"]))}
return (clojure.lang.Symbol.intern(clojure.str.apply(null,[prefix_string_1,clojure.str.apply(null,[clojure.lang.RT.nextID()])])))})))}).apply(null,[]);
// Skipping: (defmacro cond "Takes a set of test/expr pairs. It evaluates each test one at a\n  time.  If a test returns logical true, cond evaluates and returns\n  the value of the corresponding expr and doesn't evaluate any of the\n  other tests or exprs. (cond) returns nil." [& clauses] (when clauses (list (quote if) (first clauses) (second clauses) (cons (quote cond) (rest (rest clauses))))))

//======
//(defn spread {:private true} [arglist] (cond (nil? arglist) nil (nil? (rest arglist)) (seq (first arglist)) :else (cons (first arglist) (spread (rest arglist)))))
//---
(function __clojure_fn_3137(){
return (clojure.JS.def(clojure,"spread",(function __clojure_fn_3137_spread_3139(arglist_1){
return (((clojure.nil_QMARK_.apply(null,[arglist_1]))?(null):(((clojure.nil_QMARK_.apply(null,[clojure.rest.apply(null,[arglist_1])]))?(clojure.seq.apply(null,[clojure.first.apply(null,[arglist_1])])):(((clojure.keyword("","else"))?(clojure.cons.apply(null,[clojure.first.apply(null,[arglist_1]),clojure.spread.apply(null,[clojure.rest.apply(null,[arglist_1])])])):(null)))))))})))}).apply(null,[]);
// Skipping: (defn apply "Applies fn f to the argument list formed by prepending args to argseq." {:arglists (quote ([f args* argseq]))} [f & args] (. f (applyTo (spread args))))

//======
//(defn list* "Creates a new list containing the item prepended to more." [item & more] (spread (cons item more)))
//---
(function __clojure_fn_3155(){
return (clojure.JS.def(clojure,"list_STAR_",clojure.JS.variadic(1,(function __clojure_fn_3155_list_STAR_3157(item_1){
var more_2,more_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.spread.apply(null,[clojure.cons.apply(null,[item_1,more_2])]))}))))}).apply(null,[]);
// Skipping: (defmacro delay "Takes a body of expressions and yields a Delay object than will\n  invoke the body only the first time it is forced (with force), and\n  will cache the result and return it on all subsequent force calls" [& body] (list (quote new) (quote clojure.lang.Delay) (list* (quote clojure/fn) [] body)))

//======
//(defn delay? "returns true if x is a Delay created with delay" [x] (instance? clojure.lang.Delay x))
//---
(function __clojure_fn_3173(){
return (clojure.JS.def(clojure,"delay_QMARK_",(function __clojure_fn_3173_delay_QMARK_3175(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Delay,x_1]))})))}).apply(null,[]);

//======
//(defn force "If x is a Delay, returns the (possibly cached) value of its expression, else returns x" [x] (. clojure.lang.Delay (force x)))
//---
(function __clojure_fn_3185(){
return (clojure.JS.def(clojure,"force",(function __clojure_fn_3185_force_3187(x_1){
return (clojure.lang.Delay.force(x_1))})))}).apply(null,[]);

//======
//(defn fnseq "Returns a seq object whose first is first and whose rest is the\n  value produced by calling restfn with no arguments. restfn will be\n  called at most once per step in the sequence, e.g. calling rest\n  repeatedly on the head of the seq calls restfn once - the value it\n  yields is cached." [first restfn] (new clojure.lang.FnSeq first restfn))
//---
(function __clojure_fn_3197(){
return (clojure.JS.def(clojure,"fnseq",(function __clojure_fn_3197_fnseq_3199(first_1,restfn_2){
return ((new clojure.lang.FnSeq(first_1,restfn_2)))})))}).apply(null,[]);
// Skipping: (defmacro lazy-cons "Expands to code which produces a seq object whose first is\n  first-expr and whose rest is rest-expr, neither of which is\n  evaluated until first/rest is called. Each expr will be evaluated at most\n  once per step in the sequence, e.g. calling first/rest repeatedly on the\n  same node of the seq evaluates first/rest-expr once - the values they yield are\n  cached." [first-expr & rest-expr] (list (quote new) (quote clojure.lang.LazyCons) (list (quote clojure/fn) (list [] first-expr) (list* [(gensym)] rest-expr))))

//======
//(defn cache-seq "Given a seq s, returns a lazy seq that will touch each element of s\n  at most once, caching the results." [s] (when s (clojure.lang.CachedSeq. s)))
//---
(function __clojure_fn_3215(){
return (clojure.JS.def(clojure,"cache_seq",(function __clojure_fn_3215_cache_seq_3217(s_1){
return (((s_1)?((new clojure.lang.CachedSeq(s_1))):(null)))})))}).apply(null,[]);

//======
//(defn concat "Returns a lazy seq representing the concatenation of\tthe elements in the supplied colls." ([] nil) ([x] (seq x)) ([x y] (if (seq x) (lazy-cons (first x) (concat (rest x) y)) (seq y))) ([x y & zs] (let [cat (fn cat [xys zs] (if (seq xys) (lazy-cons (first xys) (cat (rest xys) zs)) (when zs (recur (first zs) (rest zs)))))] (cat (concat x y) zs))))
//---
(function __clojure_fn_3242(){
return (clojure.JS.def(clojure,"concat",clojure.JS.variadic(2,(function __clojure_fn_3242_concat_3244(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.seq.apply(null,[x_1]))?((new clojure.lang.LazyCons((function __clojure_fn_3242_concat_3244_fn_3249(G__3248_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[x_1]))}
return (clojure.concat.apply(null,[clojure.rest.apply(null,[x_1]),y_2]))})))):(clojure.seq.apply(null,[y_2]))))
case 1:return (clojure.seq.apply(null,[x_1]))
case 0:return (null)}
var zs_3,cat_4,zs_3=clojure.JS.rest_args(this,arguments,2);
return (((cat_4=(function __clojure_fn_3242_concat_3244_cat_3254(xys_1,zs_2){
var _cnt,_rtn,cat_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[xys_1]))?((new clojure.lang.LazyCons((function __clojure_fn_3242_concat_3244_cat_3254_fn_3256(G__3255_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[xys_1]))}
return (cat_0.apply(null,[clojure.rest.apply(null,[xys_1]),zs_2]))})))):(((zs_2)?((_cnt=1,_rtn=[clojure.first.apply(null,[zs_2]),clojure.rest.apply(null,[zs_2])],xys_1=_rtn[0],zs_2=_rtn[1])):(null))))
}while(_cnt);return _rtn;})),
cat_4.apply(null,[clojure.concat.apply(null,[x_1,y_2]),zs_3])))}))))}).apply(null,[]);

//======
//(defn = "Equality. Returns true if x equals y, false if not. Same as\n  Java x.equals(y) except it also works for nil, and compares\n  numbers in a type-independent manner.  Clojure's immutable data\n  structures define equals() (and thus =) as a value, not an identity,\n  comparison." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Util)) (clojure/list (quote clojure/equal)) (clojure/list x) (clojure/list y))), :inline-arities #{2}, :tag Boolean} ([x] true) ([x y] (. clojure.lang.Util (equal x y))) ([x y & more] (if (= x y) (if (rest more) (recur y (first more) (rest more)) (= y (first more))) false)))
//---
(function __clojure_fn_3274(){
return (clojure.JS.def(clojure,"_EQ_",clojure.JS.variadic(2,(function __clojure_fn_3274_EQ_3279(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Util.equal(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Util.equal(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Util.equal(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn not= "Same as (not (= obj1 obj2))" {:tag Boolean} ([x] false) ([x y] (not (= x y))) ([x y & more] (not (apply = x y more))))
//---
(function __clojure_fn_3293(){
return (clojure.JS.def(clojure,"not_EQ_",clojure.JS.variadic(2,(function __clojure_fn_3293_not_EQ_3295(x_1,y_2){switch(arguments.length){
case 2:return (clojure.not.apply(null,[clojure.lang.Util.equal(x_1,y_2)]))
case 1:return (false)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.not.apply(null,[clojure.apply.apply(null,[clojure._EQ_,x_1,y_2,more_3])]))}))))}).apply(null,[]);

//======
//(defn compare "Comparator. Returns 0 if x equals y, -1 if x is logically 'less\n  than' y, else 1. Same as Java x.compareTo(y) except it also works\n  for nil, and compares numbers in a type-independent manner. x must\n  implement Comparable" {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Util)) (clojure/list (quote clojure/compare)) (clojure/list x) (clojure/list y))), :tag Integer} [x y] (. clojure.lang.Util (compare x y)))
//---
(function __clojure_fn_3310(){
return (clojure.JS.def(clojure,"compare",(function __clojure_fn_3310_compare_3315(x_1,y_2){
return (clojure.lang.Util.compare(x_1,y_2))})))}).apply(null,[]);
// Skipping: (defmacro and "Evaluates exprs one at a time, from left to right. If a form\n  returns logical false (nil or false), and returns that value and\n  doesn't evaluate any of the other expressions, otherwise it returns\n  the value of the last expr. (and) returns true." ([] true) ([x] x) ([x & rest] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote and__3319)) (clojure/list x)))) (clojure/list (clojure/concat (clojure/list (quote if)) (clojure/list (quote and__3319)) (clojure/list (clojure/concat (clojure/list (quote clojure/and)) rest)) (clojure/list (quote and__3319)))))))
// Skipping: (defmacro or "Evaluates exprs one at a time, from left to right. If a form\n  returns a logical true value, or returns that value and doesn't\n  evaluate any of the other expressions, otherwise it returns the\n  value of the last expression. (or) returns nil." ([] nil) ([x] x) ([x & rest] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote or__3328)) (clojure/list x)))) (clojure/list (clojure/concat (clojure/list (quote if)) (clojure/list (quote or__3328)) (clojure/list (quote or__3328)) (clojure/list (clojure/concat (clojure/list (quote clojure/or)) rest)))))))

//======
//(defn reduce "f should be a function of 2 arguments. If val is not supplied,\n  returns the result of applying f to the first 2 items in coll, then\n  applying f to that result and the 3rd item, etc. If coll contains no\n  items, f must accept no arguments as well, and reduce returns the\n  result of calling f with no arguments.  If coll has only 1 item, it\n  is returned and f is not called.  If val is supplied, returns the\n  result of applying f to val and the first item in coll, then\n  applying f to that result and the 2nd item, etc. If coll contains no\n  items, returns val and f is not called." ([f coll] (let [s (seq coll)] (if s (if (instance? clojure.lang.IReduce s) (. s (reduce f)) (reduce f (first s) (rest s))) (f)))) ([f val coll] (let [s (seq coll)] (if (instance? clojure.lang.IReduce s) (. s (reduce f val)) ((fn [f val s] (if s (recur f (f val (first s)) (rest s)) val)) f val s)))))
//---
(function __clojure_fn_3347(){
return (clojure.JS.def(clojure,"reduce",(function __clojure_fn_3347_reduce_3349(f_1,val_2,coll_3){switch(arguments.length){
case 2:var s_3,coll_2=arguments[1];
return (((s_3=clojure.seq.apply(null,[coll_2])),
((s_3)?(((clojure.instance_QMARK_.apply(null,[clojure.lang.IReduce,s_3]))?((s_3).reduce(f_1)):(clojure.reduce.apply(null,[f_1,clojure.first.apply(null,[s_3]),clojure.rest.apply(null,[s_3])])))):(f_1.apply(null,[])))))}
var s_4;
return (((s_4=clojure.seq.apply(null,[coll_3])),
((clojure.instance_QMARK_.apply(null,[clojure.lang.IReduce,s_4]))?((s_4).reduce(f_1,val_2)):((function __clojure_fn_3347_reduce_3349_fn_3352(f_1,val_2,s_3){
var _cnt,_rtn;
do{_cnt=0;_rtn=((s_3)?((_cnt=1,_rtn=[f_1,f_1.apply(null,[val_2,clojure.first.apply(null,[s_3])]),clojure.rest.apply(null,[s_3])],f_1=_rtn[0],val_2=_rtn[1],s_3=_rtn[2])):(val_2))
}while(_cnt);return _rtn;}).apply(null,[f_1,val_2,s_4])))))})))}).apply(null,[]);

//======
//(defn reverse "Returns a seq of the items in coll in reverse order. Not lazy." [coll] (reduce conj nil coll))
//---
(function __clojure_fn_3363(){
return (clojure.JS.def(clojure,"reverse",(function __clojure_fn_3363_reverse_3365(coll_1){
return (clojure.reduce.apply(null,[clojure.conj,null,coll_1]))})))}).apply(null,[]);

//======
//(defn + "Returns the sum of nums. (+) returns 0." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/add)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([] 0) ([x] (clojure.lang.RT/numberCast x)) ([x y] (. clojure.lang.Numbers (add x y))) ([x y & more] (reduce + (+ x y) more)))
//---
(function __clojure_fn_3381(){
return (clojure.JS.def(clojure,"_PLUS_",clojure.JS.variadic(2,(function __clojure_fn_3381_PLUS_3386(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.add(x_1,y_2))
case 1:return (clojure.lang.RT.numberCast(x_1))
case 0:return (0)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure._PLUS_,clojure.lang.Numbers.add(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn * "Returns the product of nums. (*) returns 1." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/multiply)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([] 1) ([x] (clojure.lang.RT/numberCast x)) ([x y] (. clojure.lang.Numbers (multiply x y))) ([x y & more] (reduce * (* x y) more)))
//---
(function __clojure_fn_3405(){
return (clojure.JS.def(clojure,"_STAR_",clojure.JS.variadic(2,(function __clojure_fn_3405_STAR_3410(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.multiply(x_1,y_2))
case 1:return (clojure.lang.RT.numberCast(x_1))
case 0:return (1)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure._STAR_,clojure.lang.Numbers.multiply(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn / "If no denominators are supplied, returns 1/numerator,\n  else returns numerator divided by all of the denominators." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/divide)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] (/ 1 x)) ([x y] (. clojure.lang.Numbers (divide x y))) ([x y & more] (reduce / (/ x y) more)))
//---
(function __clojure_fn_3428(){
return (clojure.JS.def(clojure,"_SLASH_",clojure.JS.variadic(2,(function __clojure_fn_3428_SLASH_3433(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.divide(x_1,y_2))
case 1:return (clojure.lang.Numbers.divide(1,x_1))}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure._SLASH_,clojure.lang.Numbers.divide(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn - "If no ys are supplied, returns the negation of x, else subtracts\n  the ys from x and returns the result." {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/minus)) args)))), :inline-arities #{1 2}} ([x] (. clojure.lang.Numbers (minus x))) ([x y] (. clojure.lang.Numbers (minus x y))) ([x y & more] (reduce - (- x y) more)))
//---
(function __clojure_fn_3450(){
return (clojure.JS.def(clojure,"_",clojure.JS.variadic(2,(function __clojure_fn_3450_3455(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.minus(x_1,y_2))
case 1:return (clojure.lang.Numbers.minus(x_1))}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure._,clojure.lang.Numbers.minus(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn < "Returns non-nil if nums are in monotonically increasing order,\n  otherwise false." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/lt)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (lt x y))) ([x y & more] (if (< x y) (if (rest more) (recur y (first more) (rest more)) (< y (first more))) false)))
//---
(function __clojure_fn_3472(){
return (clojure.JS.def(clojure,"_LT_",clojure.JS.variadic(2,(function __clojure_fn_3472_LT_3477(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.lt(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.lt(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.lt(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn <= "Returns non-nil if nums are in monotonically non-decreasing order,\n  otherwise false." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/lte)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (lte x y))) ([x y & more] (if (<= x y) (if (rest more) (recur y (first more) (rest more)) (<= y (first more))) false)))
//---
(function __clojure_fn_3494(){
return (clojure.JS.def(clojure,"_LT__EQ_",clojure.JS.variadic(2,(function __clojure_fn_3494_LT_EQ_3499(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.lte(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.lte(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.lte(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn > "Returns non-nil if nums are in monotonically decreasing order,\n  otherwise false." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/gt)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (gt x y))) ([x y & more] (if (> x y) (if (rest more) (recur y (first more) (rest more)) (> y (first more))) false)))
//---
(function __clojure_fn_3516(){
return (clojure.JS.def(clojure,"_GT_",clojure.JS.variadic(2,(function __clojure_fn_3516_GT_3521(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.gt(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.gt(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.gt(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn >= "Returns non-nil if nums are in monotonically non-increasing order,\n  otherwise false." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/gte)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (gte x y))) ([x y & more] (if (>= x y) (if (rest more) (recur y (first more) (rest more)) (>= y (first more))) false)))
//---
(function __clojure_fn_3538(){
return (clojure.JS.def(clojure,"_GT__EQ_",clojure.JS.variadic(2,(function __clojure_fn_3538_GT_EQ_3543(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.gte(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.gte(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.gte(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn == "Returns non-nil if nums all have the same value, otherwise false" {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/equiv)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (equiv x y))) ([x y & more] (if (== x y) (if (rest more) (recur y (first more) (rest more)) (== y (first more))) false)))
//---
(function __clojure_fn_3560(){
return (clojure.JS.def(clojure,"_EQ__EQ_",clojure.JS.variadic(2,(function __clojure_fn_3560_EQ_EQ_3565(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.equiv(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.equiv(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.equiv(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn max "Returns the greatest of the nums." ([x] x) ([x y] (if (> x y) x y)) ([x y & more] (reduce max (max x y) more)))
//---
(function __clojure_fn_3579(){
return (clojure.JS.def(clojure,"max",clojure.JS.variadic(2,(function __clojure_fn_3579_max_3581(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.lang.Numbers.gt(x_1,y_2))?(x_1):(y_2)))
case 1:return (x_1)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure.max,clojure.max.apply(null,[x_1,y_2]),more_3]))}))))}).apply(null,[]);

//======
//(defn min "Returns the least of the nums." ([x] x) ([x y] (if (< x y) x y)) ([x y & more] (reduce min (min x y) more)))
//---
(function __clojure_fn_3595(){
return (clojure.JS.def(clojure,"min",clojure.JS.variadic(2,(function __clojure_fn_3595_min_3597(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.lang.Numbers.lt(x_1,y_2))?(x_1):(y_2)))
case 1:return (x_1)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure.min,clojure.min.apply(null,[x_1,y_2]),more_3]))}))))}).apply(null,[]);

//======
//(defn inc "Returns a number one greater than num." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/inc)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (inc x)))
//---
(function __clojure_fn_3612(){
return (clojure.JS.def(clojure,"inc",(function __clojure_fn_3612_inc_3617(x_1){
return (clojure.lang.Numbers.inc(x_1))})))}).apply(null,[]);

//======
//(defn dec "Returns a number one less than num." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/dec)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (dec x)))
//---
(function __clojure_fn_3630(){
return (clojure.JS.def(clojure,"dec",(function __clojure_fn_3630_dec_3635(x_1){
return (clojure.lang.Numbers.dec(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-inc "Returns a number one greater than x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_inc)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (unchecked_inc x)))
//---
(function __clojure_fn_3648(){
return (clojure.JS.def(clojure,"unchecked_inc",(function __clojure_fn_3648_unchecked_inc_3653(x_1){
return (clojure.lang.Numbers.unchecked_inc(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-dec "Returns a number one less than x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_dec)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (unchecked_dec x)))
//---
(function __clojure_fn_3666(){
return (clojure.JS.def(clojure,"unchecked_dec",(function __clojure_fn_3666_unchecked_dec_3671(x_1){
return (clojure.lang.Numbers.unchecked_dec(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-negate "Returns the negation of x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_negate)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (unchecked_negate x)))
//---
(function __clojure_fn_3684(){
return (clojure.JS.def(clojure,"unchecked_negate",(function __clojure_fn_3684_unchecked_negate_3689(x_1){
return (clojure.lang.Numbers.unchecked_negate(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-add "Returns the sum of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_add)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_add x y)))
//---
(function __clojure_fn_3702(){
return (clojure.JS.def(clojure,"unchecked_add",(function __clojure_fn_3702_unchecked_add_3707(x_1,y_2){
return (clojure.lang.Numbers.unchecked_add(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-subtract "Returns the difference of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_subtract)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_subtract x y)))
//---
(function __clojure_fn_3720(){
return (clojure.JS.def(clojure,"unchecked_subtract",(function __clojure_fn_3720_unchecked_subtract_3725(x_1,y_2){
return (clojure.lang.Numbers.unchecked_subtract(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-multiply "Returns the product of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_multiply)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_multiply x y)))
//---
(function __clojure_fn_3738(){
return (clojure.JS.def(clojure,"unchecked_multiply",(function __clojure_fn_3738_unchecked_multiply_3743(x_1,y_2){
return (clojure.lang.Numbers.unchecked_multiply(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-divide "Returns the division of x by y, both int or long. \n  Note - uses a primitive operator subject to truncation." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_divide)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_divide x y)))
//---
(function __clojure_fn_3756(){
return (clojure.JS.def(clojure,"unchecked_divide",(function __clojure_fn_3756_unchecked_divide_3761(x_1,y_2){
return (clojure.lang.Numbers.unchecked_divide(x_1,y_2))})))}).apply(null,[]);

//======
//(defn pos? "Returns true if num is greater than zero, else false" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/isPos)) (clojure/list x))))), :tag Boolean} [x] (. clojure.lang.Numbers (isPos x)))
//---
(function __clojure_fn_3774(){
return (clojure.JS.def(clojure,"pos_QMARK_",(function __clojure_fn_3774_pos_QMARK_3779(x_1){
return (clojure.lang.Numbers.isPos(x_1))})))}).apply(null,[]);

//======
//(defn neg? "Returns true if num is less than zero, else false" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/isNeg)) (clojure/list x))))), :tag Boolean} [x] (. clojure.lang.Numbers (isNeg x)))
//---
(function __clojure_fn_3792(){
return (clojure.JS.def(clojure,"neg_QMARK_",(function __clojure_fn_3792_neg_QMARK_3797(x_1){
return (clojure.lang.Numbers.isNeg(x_1))})))}).apply(null,[]);

//======
//(defn zero? "Returns true if num is zero, else false" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/isZero)) (clojure/list x))))), :tag Boolean} [x] (. clojure.lang.Numbers (isZero x)))
//---
(function __clojure_fn_3810(){
return (clojure.JS.def(clojure,"zero_QMARK_",(function __clojure_fn_3810_zero_QMARK_3815(x_1){
return (clojure.lang.Numbers.isZero(x_1))})))}).apply(null,[]);

//======
//(defn quot "quot[ient] of dividing numerator by denominator." [num div] (. clojure.lang.Numbers (quotient num div)))
//---
(function __clojure_fn_3825(){
return (clojure.JS.def(clojure,"quot",(function __clojure_fn_3825_quot_3827(num_1,div_2){
return (clojure.lang.Numbers.quotient(num_1,div_2))})))}).apply(null,[]);

//======
//(defn rem "rem[ainder] of dividing numerator by denominator." [num div] (. clojure.lang.Numbers (remainder num div)))
//---
(function __clojure_fn_3837(){
return (clojure.JS.def(clojure,"rem",(function __clojure_fn_3837_rem_3839(num_1,div_2){
return (clojure.lang.Numbers.remainder(num_1,div_2))})))}).apply(null,[]);

//======
//(defn rationalize "returns the rational value of num" [num] (. clojure.lang.Numbers (rationalize num)))
//---
(function __clojure_fn_3849(){
return (clojure.JS.def(clojure,"rationalize",(function __clojure_fn_3849_rationalize_3851(num_1){
return (clojure.lang.Numbers.rationalize(num_1))})))}).apply(null,[]);

//======
//(defn bit-not "Bitwise complement" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/not)) (clojure/list x)))))} [x] (. clojure.lang.Numbers not x))
//---
(function __clojure_fn_3864(){
return (clojure.JS.def(clojure,"bit_not",(function __clojure_fn_3864_bit_not_3869(x_1){
return (clojure.lang.Numbers.not(x_1))})))}).apply(null,[]);

//======
//(defn bit-and "Bitwise and" {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/and)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers and x y))
//---
(function __clojure_fn_3882(){
return (clojure.JS.def(clojure,"bit_and",(function __clojure_fn_3882_bit_and_3887(x_1,y_2){
return (clojure.lang.Numbers.and(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-or "Bitwise or" {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/or)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers or x y))
//---
(function __clojure_fn_3900(){
return (clojure.JS.def(clojure,"bit_or",(function __clojure_fn_3900_bit_or_3905(x_1,y_2){
return (clojure.lang.Numbers.or(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-xor "Bitwise exclusive or" {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/xor)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers xor x y))
//---
(function __clojure_fn_3918(){
return (clojure.JS.def(clojure,"bit_xor",(function __clojure_fn_3918_bit_xor_3923(x_1,y_2){
return (clojure.lang.Numbers.xor(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-and-not "Bitwise and with complement" [x y] (. clojure.lang.Numbers andNot x y))
//---
(function __clojure_fn_3933(){
return (clojure.JS.def(clojure,"bit_and_not",(function __clojure_fn_3933_bit_and_not_3935(x_1,y_2){
return (clojure.lang.Numbers.andNot(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-clear "Clear bit at index n" [x n] (. clojure.lang.Numbers clearBit x n))
//---
(function __clojure_fn_3945(){
return (clojure.JS.def(clojure,"bit_clear",(function __clojure_fn_3945_bit_clear_3947(x_1,n_2){
return (clojure.lang.Numbers.clearBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-set "Set bit at index n" [x n] (. clojure.lang.Numbers setBit x n))
//---
(function __clojure_fn_3957(){
return (clojure.JS.def(clojure,"bit_set",(function __clojure_fn_3957_bit_set_3959(x_1,n_2){
return (clojure.lang.Numbers.setBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-flip "Flip bit at index n" [x n] (. clojure.lang.Numbers flipBit x n))
//---
(function __clojure_fn_3969(){
return (clojure.JS.def(clojure,"bit_flip",(function __clojure_fn_3969_bit_flip_3971(x_1,n_2){
return (clojure.lang.Numbers.flipBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-test "Test bit at index n" [x n] (. clojure.lang.Numbers testBit x n))
//---
(function __clojure_fn_3981(){
return (clojure.JS.def(clojure,"bit_test",(function __clojure_fn_3981_bit_test_3983(x_1,n_2){
return (clojure.lang.Numbers.testBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-shift-left "Bitwise shift left" [x n] (. clojure.lang.Numbers shiftLeft x n))
//---
(function __clojure_fn_3993(){
return (clojure.JS.def(clojure,"bit_shift_left",(function __clojure_fn_3993_bit_shift_left_3995(x_1,n_2){
return (clojure.lang.Numbers.shiftLeft(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-shift-right "Bitwise shift right" [x n] (. clojure.lang.Numbers shiftRight x n))
//---
(function __clojure_fn_4005(){
return (clojure.JS.def(clojure,"bit_shift_right",(function __clojure_fn_4005_bit_shift_right_4007(x_1,n_2){
return (clojure.lang.Numbers.shiftRight(x_1,n_2))})))}).apply(null,[]);

//======
//(defn even? "Returns true if n is even, throws an exception if n is not an integer" [n] (zero? (bit-and n 1)))
//---
(function __clojure_fn_4017(){
return (clojure.JS.def(clojure,"even_QMARK_",(function __clojure_fn_4017_even_QMARK_4019(n_1){
return (clojure.lang.Numbers.isZero(clojure.lang.Numbers.and(n_1,1)))})))}).apply(null,[]);

//======
//(defn odd? "Returns true if n is odd, throws an exception if n is not an integer" [n] (not (even? n)))
//---
(function __clojure_fn_4029(){
return (clojure.JS.def(clojure,"odd_QMARK_",(function __clojure_fn_4029_odd_QMARK_4031(n_1){
return (clojure.not.apply(null,[clojure.even_QMARK_.apply(null,[n_1])]))})))}).apply(null,[]);

//======
//(defn complement "Takes a fn f and returns a fn that takes the same arguments as f,\n  has the same effects, if any, and returns the opposite truth value." [f] (fn [& args] (not (apply f args))))
//---
(function __clojure_fn_4044(){
return (clojure.JS.def(clojure,"complement",(function __clojure_fn_4044_complement_4046(f_1){
return (clojure.JS.variadic(0,(function __clojure_fn_4044_complement_4046_fn_4048(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.not.apply(null,[clojure.apply.apply(null,[f_1,args_1])]))})))})))}).apply(null,[]);

//======
//(defn constantly "Returns a function that takes any number of arguments and returns x." [x] (fn [& args] x))
//---
(function __clojure_fn_4062(){
return (clojure.JS.def(clojure,"constantly",(function __clojure_fn_4062_constantly_4064(x_1){
return (clojure.JS.variadic(0,(function __clojure_fn_4062_constantly_4064_fn_4066(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (x_1)})))})))}).apply(null,[]);

//======
//(defn identity "Returns its argument." [x] x)
//---
(function __clojure_fn_4077(){
return (clojure.JS.def(clojure,"identity",(function __clojure_fn_4077_identity_4079(x_1){
return (x_1)})))}).apply(null,[]);
// Skipping: (defn count "Returns the number of items in the collection. (count nil) returns\n  0.  Also works on strings, arrays, and Java Collections and Maps" [coll] (. clojure.lang.RT (count coll)))

//======
//(defn peek "For a list or queue, same as first, for a vector, same as, but much\n  more efficient than, last. If the collection is empty, returns nil." [coll] (. clojure.lang.RT (peek coll)))
//---
(function __clojure_fn_4095(){
return (clojure.JS.def(clojure,"peek",(function __clojure_fn_4095_peek_4097(coll_1){
return (clojure.lang.RT.peek(coll_1))})))}).apply(null,[]);

//======
//(defn pop "For a list or queue, returns a new list/queue without the first\n  item, for a vector, returns a new vector without the last item. If\n  the collection is empty, throws an exception.  Note - not the same\n  as rest/butlast." [coll] (. clojure.lang.RT (pop coll)))
//---
(function __clojure_fn_4107(){
return (clojure.JS.def(clojure,"pop",(function __clojure_fn_4107_pop_4109(coll_1){
return (clojure.lang.RT.pop(coll_1))})))}).apply(null,[]);
// Skipping: (defn nth "Returns the value at the index. get returns nil if index out of\n  bounds, nth throws an exception unless not-found is supplied.  nth\n  also works for strings, Java arrays, regex Matchers and Lists, and,\n  in O(n) time, for sequences." ([coll index] (. clojure.lang.RT (nth coll index))) ([coll index not-found] (. clojure.lang.RT (nth coll index not-found))))
// Skipping: (defn contains? "Returns true if key is present, else false." [map key] (. clojure.lang.RT (contains map key)))
// Skipping: (defn get "Returns the value mapped to key, not-found or nil if key not present." ([map key] (. clojure.lang.RT (get map key))) ([map key not-found] (. clojure.lang.RT (get map key not-found))))

//======
//(defn dissoc "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,\n  that does not contain a mapping for key(s)." ([map] map) ([map key] (. clojure.lang.RT (dissoc map key))) ([map key & ks] (let [ret (dissoc map key)] (if ks (recur ret (first ks) (rest ks)) ret))))
//---
(function __clojure_fn_4141(){
return (clojure.JS.def(clojure,"dissoc",clojure.JS.variadic(2,(function __clojure_fn_4141_dissoc_4143(map_1,key_2){switch(arguments.length){
case 2:return (clojure.lang.RT.dissoc(map_1,key_2))
case 1:return (map_1)}
var _cnt,_rtn,ks_3,ret_4,ks_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((ret_4=clojure.dissoc.apply(null,[map_1,key_2])),
((ks_3)?((_cnt=1,_rtn=[ret_4,clojure.first.apply(null,[ks_3]),clojure.rest.apply(null,[ks_3])],map_1=_rtn[0],key_2=_rtn[1],ks_3=_rtn[2])):(ret_4)))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn disj "disj[oin]. Returns a new set of the same (hashed/sorted) type, that\n  does not contain key(s)." ([set] set) ([set key] (. set (disjoin key))) ([set key & ks] (let [ret (disj set key)] (if ks (recur ret (first ks) (rest ks)) ret))))
//---
(function __clojure_fn_4157(){
return (clojure.JS.def(clojure,"disj",clojure.JS.variadic(2,(function __clojure_fn_4157_disj_4159(set_1,key_2){switch(arguments.length){
case 2:return ((set_1).disjoin(key_2))
case 1:return (set_1)}
var _cnt,_rtn,ret_4,ks_3,ks_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((ret_4=clojure.disj.apply(null,[set_1,key_2])),
((ks_3)?((_cnt=1,_rtn=[ret_4,clojure.first.apply(null,[ks_3]),clojure.rest.apply(null,[ks_3])],set_1=_rtn[0],key_2=_rtn[1],ks_3=_rtn[2])):(ret_4)))
}while(_cnt);return _rtn;}))))}).apply(null,[]);
// Skipping: (defn find "Returns the map entry for key, or nil if key not present." [map key] (. clojure.lang.RT (find map key)))

//======
//(defn select-keys "Returns a map containing only those entries in map whose key is in keys" [map keyseq] (loop [ret {} keys (seq keyseq)] (if keys (let [entry (. clojure.lang.RT (find map (first keys)))] (recur (if entry (conj ret entry) ret) (rest keys))) ret)))
//---
(function __clojure_fn_4177(){
return (clojure.JS.def(clojure,"select_keys",(function __clojure_fn_4177_select_keys_4179(map_1,keyseq_2){
var keys_4,entry_5,ret_3;
return (((function __loop(){var _rtn,_cnt;(ret_3=clojure.lang.PersistentHashMap.EMPTY),
(keys_4=clojure.seq.apply(null,[keyseq_2]));do{_cnt=0;
_rtn=((keys_4)?(((entry_5=clojure.lang.RT.find(map_1,clojure.first.apply(null,[keys_4]))),
(_cnt=1,_rtn=[((entry_5)?(clojure.conj.apply(null,[ret_3,entry_5])):(ret_3)),clojure.rest.apply(null,[keys_4])],ret_3=_rtn[0],keys_4=_rtn[1]))):(ret_3))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);
// Skipping: (defn keys "Returns a sequence of the map's keys." [map] (. clojure.lang.RT (keys map)))
// Skipping: (defn vals "Returns a sequence of the map's values." [map] (. clojure.lang.RT (vals map)))

//======
//(defn key "Returns the key of the map entry." [e] (. e (getKey)))
//---
(function __clojure_fn_4201(){
return (clojure.JS.def(clojure,"key",(function __clojure_fn_4201_key_4203(e_1){
return ((e_1).getKey())})))}).apply(null,[]);

//======
//(defn val "Returns the value in the map entry." [e] (. e (getValue)))
//---
(function __clojure_fn_4213(){
return (clojure.JS.def(clojure,"val",(function __clojure_fn_4213_val_4215(e_1){
return ((e_1).getValue())})))}).apply(null,[]);

//======
//(defn rseq "Returns, in constant time, a sequence of the items in rev (which\n  can be a vector or sorted-map), in reverse order." [rev] (. rev (rseq)))
//---
(function __clojure_fn_4225(){
return (clojure.JS.def(clojure,"rseq",(function __clojure_fn_4225_rseq_4227(rev_1){
return ((rev_1).rseq())})))}).apply(null,[]);

//======
//(defn name "Returns the name String of a symbol or keyword." [x] (. x (getName)))
//---
(function __clojure_fn_4237(){
return (clojure.JS.def(clojure,"name",(function __clojure_fn_4237_name_4239(x_1){
return ((x_1).getName())})))}).apply(null,[]);

//======
//(defn namespace "Returns the namespace String of a symbol or keyword, or nil if not present." [x] (. x (getNamespace)))
//---
(function __clojure_fn_4249(){
return (clojure.JS.def(clojure,"namespace",(function __clojure_fn_4249_namespace_4251(x_1){
return ((x_1).getNamespace())})))}).apply(null,[]);
// Skipping: (defmacro locking "Executes exprs in an implicit do, while holding the monitor of x.\n  Will release the monitor of x in all circumstances." [x & body] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote lockee__4255)) (clojure/list x)))) (clojure/list (clojure/concat (clojure/list (quote try)) (clojure/list (clojure/concat (clojure/list (quote monitor-enter)) (clojure/list (quote lockee__4255)))) body (clojure/list (clojure/concat (clojure/list (quote finally)) (clojure/list (clojure/concat (clojure/list (quote monitor-exit)) (clojure/list (quote lockee__4255))))))))))
// Skipping: (defmacro .. "form => fieldName-symbol or (instanceMethodName-symbol args*)\n\n  Expands into a member access (.) of the first member on the first\n  argument, followed by the next member on the result, etc. For\n  instance:\n\n  (.. System (getProperties) (get \"os.name\"))\n\n  expands to:\n\n  (. (. System (getProperties)) (get \"os.name\"))\n\n  but is easier to write, read, and understand." ([x form] (clojure/concat (clojure/list (quote .)) (clojure/list x) (clojure/list form))) ([x form & more] (clojure/concat (clojure/list (quote clojure/..)) (clojure/list (clojure/concat (clojure/list (quote .)) (clojure/list x) (clojure/list form))) more)))
// Skipping: (defmacro -> "Macro. Threads the expr through the forms. Inserts x as the\n  second item in the first form, making a list of it if it is not a\n  list already. If there are more forms, inserts the first form as the\n  second item in second form, etc." ([x form] (if (seq? form) (clojure/concat (clojure/list (first form)) (clojure/list x) (rest form)) (list form x))) ([x form & more] (clojure/concat (clojure/list (quote clojure/->)) (clojure/list (clojure/concat (clojure/list (quote clojure/->)) (clojure/list x) (clojure/list form))) more)))
// Skipping: (defmacro defmulti "Creates a new multimethod with the associated dispatch function. If\n  default-dispatch-val is supplied it becomes the default dispatch\n  value of the multimethod, otherwise the default dispatch value\n  is :default." ([name dispatch-fn] (clojure/concat (clojure/list (quote clojure/defmulti)) (clojure/list name) (clojure/list dispatch-fn) (clojure/list :default))) ([name dispatch-fn default-val] (clojure/concat (clojure/list (quote def)) (clojure/list (with-meta name {:tag (quote clojure.lang.MultiFn)})) (clojure/list (clojure/concat (clojure/list (quote new)) (clojure/list (quote clojure.lang.MultiFn)) (clojure/list dispatch-fn) (clojure/list default-val))))))
// Skipping: (defmacro defmethod "Creates and installs a new method of multimethod associated with dispatch-value. " [multifn dispatch-val & fn-tail] (clojure/concat (clojure/list (quote .)) (clojure/list multifn) (clojure/list (quote clojure/addMethod)) (clojure/list dispatch-val) (clojure/list (clojure/concat (clojure/list (quote clojure/fn)) fn-tail))))
// Skipping: (defmacro remove-method "Removes the method of multimethod associated\twith dispatch-value." [multifn dispatch-val] (clojure/concat (clojure/list (quote .)) (clojure/list multifn) (clojure/list (quote clojure/removeMethod)) (clojure/list dispatch-val)))
// Skipping: (defmacro prefer-method "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y when there is a conflict" [multifn dispatch-val-x dispatch-val-y] (clojure/concat (clojure/list (quote .)) (clojure/list multifn) (clojure/list (quote clojure/preferMethod)) (clojure/list dispatch-val-x) (clojure/list dispatch-val-y)))
// Skipping: (defmacro binding "binding => var-symbol init-expr \n\n  Creates new bindings for the (already-existing) vars, with the\n  supplied initial values, executes the exprs in an implicit do, then\n  re-establishes the bindings that existed before." [bindings & body] (let [var-ize (fn [var-vals] (loop [ret [] vvs (seq var-vals)] (if vvs (recur (conj (conj ret (clojure/concat (clojure/list (quote var)) (clojure/list (first vvs)))) (second vvs)) (rest (rest vvs))) (seq ret))))] (clojure/concat (clojure/list (quote do)) (clojure/list (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Var)) (clojure/list (clojure/concat (clojure/list (quote clojure/pushThreadBindings)) (clojure/list (clojure/concat (clojure/list (quote clojure/hash-map)) (var-ize bindings))))))) (clojure/list (clojure/concat (clojure/list (quote try)) body (clojure/list (clojure/concat (clojure/list (quote finally)) (clojure/list (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Var)) (clojure/list (clojure/concat (clojure/list (quote clojure/popThreadBindings)))))))))))))

//======
//(defn find-var "Returns the global var named by the namespace-qualified symbol, or\n  nil if no var with that name." [sym] (. clojure.lang.Var (find sym)))
//---
(function __clojure_fn_4316(){
return (clojure.JS.def(clojure,"find_var",(function __clojure_fn_4316_find_var_4318(sym_1){
return (clojure.lang.Var.find(sym_1))})))}).apply(null,[]);

//======
//(defn agent "Creates and returns an agent with an initial value of state and an\n  optional validate fn. validate-fn must be nil or a side-effect-free fn of\n  one argument, which will be passed the intended new state on any state\n  change. If the new state is unacceptable, the validate-fn should\n  throw an exception." ([state] (new clojure.lang.Agent state)) ([state validate-fn] (new clojure.lang.Agent state validate-fn)))
//---
(function __clojure_fn_4329(){
return (clojure.JS.def(clojure,"agent",(function __clojure_fn_4329_agent_4331(state_1,validate_fn_2){switch(arguments.length){
case 1:return ((new clojure.lang.Agent(state_1)))}
return ((new clojure.lang.Agent(state_1,validate_fn_2)))})))}).apply(null,[]);

//======
//(defn ! [& args] (throw (clojure.lang.RT/makeException "! is now send. See also send-off")))
//---
(function __clojure_fn_4342(){
return (clojure.JS.def(clojure,"_BANG_",clojure.JS.variadic(0,(function __clojure_fn_4342_BANG_4344(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw clojure.lang.RT.makeException("! is now send. See also send-off")})())}))))}).apply(null,[]);

//======
//(defn send "Dispatch an action to an agent. Returns the agent immediately.\n  Subsequently, in a thread from a thread pool, the state of the agent\n  will be set to the value of:\n\n  (apply action-fn state-of-agent args)" [a f & args] (. a (dispatch f args false)))
//---
(function __clojure_fn_4354(){
return (clojure.JS.def(clojure,"send",clojure.JS.variadic(2,(function __clojure_fn_4354_send_4356(a_1,f_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((a_1).dispatch(f_2,args_3,false))}))))}).apply(null,[]);

//======
//(defn send-off "Dispatch a potentially blocking action to an agent. Returns the\n  agent immediately. Subsequently, in a separate thread, the state of\n  the agent will be set to the value of:\n\n  (apply action-fn state-of-agent args)" [a f & args] (. a (dispatch f args true)))
//---
(function __clojure_fn_4366(){
return (clojure.JS.def(clojure,"send_off",clojure.JS.variadic(2,(function __clojure_fn_4366_send_off_4368(a_1,f_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((a_1).dispatch(f_2,args_3,true))}))))}).apply(null,[]);

//======
//(defn add-watch "Experimental.\n  Adds a watcher to an agent. Whenever the agent runs an action, any\n  registered watchers will have their callback function called.  The\n  callback fn will be passed 3 args, the watcher, the agent and a boolean\n  which will be true if the agent's state was (potentially) changed by\n  the action. The callback fn is run synchronously with the action,\n  and thus derefs of the agent in the callback will see the value set\n  during that action. Because it is run on the action thread, the\n  callback should not block, but can send messages." [a watcher callback] (.addWatch a watcher callback))
//---
(function __clojure_fn_4378(){
return (clojure.JS.def(clojure,"add_watch",(function __clojure_fn_4378_add_watch_4380(a_1,watcher_2,callback_3){
return ((a_1).addWatch(watcher_2,callback_3))})))}).apply(null,[]);

//======
//(defn remove-watch "Experimental.\n  Removes a watcher (set by add-watch) from an agent" [a watcher] (.removeWatch a watcher))
//---
(function __clojure_fn_4390(){
return (clojure.JS.def(clojure,"remove_watch",(function __clojure_fn_4390_remove_watch_4392(a_1,watcher_2){
return ((a_1).removeWatch(watcher_2))})))}).apply(null,[]);

//======
//(defn agent-errors "Returns a sequence of the exceptions thrown during asynchronous\n  actions of the agent." [a] (. a (getErrors)))
//---
(function __clojure_fn_4402(){
return (clojure.JS.def(clojure,"agent_errors",(function __clojure_fn_4402_agent_errors_4404(a_1){
return ((a_1).getErrors())})))}).apply(null,[]);

//======
//(defn clear-agent-errors "Clears any exceptions thrown during asynchronous actions of the\n  agent, allowing subsequent actions to occur." [a] (. a (clearErrors)))
//---
(function __clojure_fn_4414(){
return (clojure.JS.def(clojure,"clear_agent_errors",(function __clojure_fn_4414_clear_agent_errors_4416(a_1){
return ((a_1).clearErrors())})))}).apply(null,[]);

//======
//(defn shutdown-agents "Initiates a shutdown of the thread pools that back the agent\n  system. Running actions will complete, but no new actions will be\n  accepted" [] (. clojure.lang.Agent shutdown))
//---
(function __clojure_fn_4426(){
return (clojure.JS.def(clojure,"shutdown_agents",(function __clojure_fn_4426_shutdown_agents_4428(){
return (clojure.lang.Agent.shutdown())})))}).apply(null,[]);

//======
//(defn ref "Creates and returns a Ref with an initial value of x and an optional validate fn.\n  validate-fn must be nil or a side-effect-free fn of one argument, which will\n  be passed the intended new state on any state change. If the new\n  state is unacceptable, the validate-fn should throw an\n  exception. validate-fn will be called on transaction commit, when\n  all refs have their final values." ([x] (new clojure.lang.Ref x)) ([x validate-fn] (new clojure.lang.Ref x validate-fn)))
//---
(function __clojure_fn_4439(){
return (clojure.JS.def(clojure,"ref",(function __clojure_fn_4439_ref_4441(x_1,validate_fn_2){switch(arguments.length){
case 1:return ((new clojure.lang.Ref(x_1)))}
return ((new clojure.lang.Ref(x_1,validate_fn_2)))})))}).apply(null,[]);

//======
//(defn deref "Also reader macro: @ref/@agent Within a transaction, returns the\n  in-transaction-value of ref, else returns the\n  most-recently-committed value of ref. When applied to an agent,\n  returns its current state." [ref] (. ref (get)))
//---
(function __clojure_fn_4452(){
return (clojure.JS.def(clojure,"deref",(function __clojure_fn_4452_deref_4454(ref_1){
return ((ref_1).get())})))}).apply(null,[]);

//======
//(defn set-validator "Sets the validator-fn for a var/ref/agent. validator-fn must be nil or a\n  side-effect-free fn of one argument, which will be passed the intended\n  new state on any state change. If the new state is unacceptable, the\n  validator-fn should throw an exception. If the current state (root\n  value if var) is not acceptable to the new validator, an exception\n  will be thrown and the validator will not be changed." [iref validator-fn] (. iref (setValidator validator-fn)))
//---
(function __clojure_fn_4464(){
return (clojure.JS.def(clojure,"set_validator",(function __clojure_fn_4464_set_validator_4466(iref_1,validator_fn_2){
return ((iref_1).setValidator(validator_fn_2))})))}).apply(null,[]);

//======
//(defn get-validator "Gets the validator-fn for a var/ref/agent." [iref] (. iref (getValidator)))
//---
(function __clojure_fn_4476(){
return (clojure.JS.def(clojure,"get_validator",(function __clojure_fn_4476_get_validator_4478(iref_1){
return ((iref_1).getValidator())})))}).apply(null,[]);

//======
//(defn commute "Must be called in a transaction. Sets the in-transaction-value of\n  ref to:\n\n  (apply fun in-transaction-value-of-ref args)\n\n  and returns the in-transaction-value of ref.\n\n  At the commit point of the transaction, sets the value of ref to be:\n\n  (apply fun most-recently-committed-value-of-ref args)\n\n  Thus fun should be commutative, or, failing that, you must accept\n  last-one-in-wins behavior.  commute allows for more concurrency than\n  ref-set." [ref fun & args] (. ref (commute fun args)))
//---
(function __clojure_fn_4488(){
return (clojure.JS.def(clojure,"commute",clojure.JS.variadic(2,(function __clojure_fn_4488_commute_4490(ref_1,fun_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((ref_1).commute(fun_2,args_3))}))))}).apply(null,[]);

//======
//(defn alter "Must be called in a transaction. Sets the in-transaction-value of\n  ref to:\n\n  (apply fun in-transaction-value-of-ref args)\n\n  and returns the in-transaction-value of ref." [ref fun & args] (. ref (alter fun args)))
//---
(function __clojure_fn_4500(){
return (clojure.JS.def(clojure,"alter",clojure.JS.variadic(2,(function __clojure_fn_4500_alter_4502(ref_1,fun_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((ref_1).alter(fun_2,args_3))}))))}).apply(null,[]);

//======
//(defn ref-set "Must be called in a transaction. Sets the value of ref.\n  Returns val." [ref val] (. ref (set val)))
//---
(function __clojure_fn_4512(){
return (clojure.JS.def(clojure,"ref_set",(function __clojure_fn_4512_ref_set_4514(ref_1,val_2){
return ((ref_1).set(val_2))})))}).apply(null,[]);

//======
//(defn ensure "Must be called in a transaction. Protects the ref from modification\n  by other transactions.  Returns the in-transaction-value of\n  ref. Allows for more concurrency than (ref-set ref @ref)" [ref] (. ref (touch)) (. ref (get)))
//---
(function __clojure_fn_4524(){
return (clojure.JS.def(clojure,"ensure",(function __clojure_fn_4524_ensure_4526(ref_1){
return ((ref_1).touch(),
(ref_1).get())})))}).apply(null,[]);
// Skipping: (defmacro sync "transaction-flags => TBD, pass nil for now\n\n  Runs the exprs (in an implicit do) in a transaction that encompasses\n  exprs and any nested calls.  Starts a transaction if none is already\n  running on this thread. Any uncaught exception will abort the\n  transaction and flow out of sync. The exprs may be run more than\n  once, but any effects on Refs will be atomic." [flags-ignored-for-now & body] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.LockingTransaction)) (clojure/list (clojure/concat (clojure/list (quote clojure/runInTransaction)) (clojure/list (clojure/concat (clojure/list (quote clojure/fn)) (clojure/list (clojure/apply clojure/vector (clojure/concat))) body))))))

//======
//(defn comp "Takes a set of functions and returns a fn that is the composition\n  of those fns.  The returned fn takes a variable number of args,\n  applies the rightmost of fns to the args, the next\n  fn (right-to-left) to the result, etc." [& fs] (let [fs (reverse fs)] (fn [& args] (loop [ret (apply (first fs) args) fs (rest fs)] (if fs (recur ((first fs) ret) (rest fs)) ret)))))
//---
(function __clojure_fn_4545(){
return (clojure.JS.def(clojure,"comp",clojure.JS.variadic(0,(function __clojure_fn_4545_comp_4547(){
var fs_2,fs_1,fs_1=clojure.JS.rest_args(this,arguments,0);
return (((fs_2=clojure.reverse.apply(null,[fs_1])),
clojure.JS.variadic(0,(function __clojure_fn_4545_comp_4547_fn_4549(){
var fs_3,args_1,ret_2,args_1=clojure.JS.rest_args(this,arguments,0);
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.apply.apply(null,[clojure.first.apply(null,[fs_2]),args_1])),
(fs_3=clojure.rest.apply(null,[fs_2]));do{_cnt=0;
_rtn=((fs_3)?((_cnt=1,_rtn=[clojure.first.apply(null,[fs_3]).apply(null,[ret_2]),clojure.rest.apply(null,[fs_3])],ret_2=_rtn[0],fs_3=_rtn[1])):(ret_2))}while(_cnt);return _rtn;})()))}))))}))))}).apply(null,[]);

//======
//(defn partial "Takes a function f and fewer than the normal arguments to f, and\n  returns a fn that takes a variable number of additional args. When\n  called, the returned function calls f with args + additional args." ([f arg1] (fn [& args] (apply f arg1 args))) ([f arg1 arg2] (fn [& args] (apply f arg1 arg2 args))) ([f arg1 arg2 arg3] (fn [& args] (apply f arg1 arg2 arg3 args))) ([f arg1 arg2 arg3 & more] (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))
//---
(function __clojure_fn_4575(){
return (clojure.JS.def(clojure,"partial",clojure.JS.variadic(4,(function __clojure_fn_4575_partial_4577(f_1,arg1_2,arg2_3,arg3_4){switch(arguments.length){
case 4:return (clojure.JS.variadic(0,(function __clojure_fn_4575_partial_4577_fn_4587(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[f_1,arg1_2,arg2_3,arg3_4,args_1]))})))
case 3:return (clojure.JS.variadic(0,(function __clojure_fn_4575_partial_4577_fn_4583(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[f_1,arg1_2,arg2_3,args_1]))})))
case 2:return (clojure.JS.variadic(0,(function __clojure_fn_4575_partial_4577_fn_4579(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[f_1,arg1_2,args_1]))})))}
var more_5,more_5=clojure.JS.rest_args(this,arguments,4);
return (clojure.JS.variadic(0,(function __clojure_fn_4575_partial_4577_fn_4591(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[f_1,arg1_2,arg2_3,arg3_4,clojure.concat.apply(null,[more_5,args_1])]))})))}))))}).apply(null,[]);

//======
//(defn every? "Returns true if (pred x) is logical true for every x in coll, else\n  false." {:tag Boolean} [pred coll] (if (seq coll) (and (pred (first coll)) (recur pred (rest coll))) true))
//---
(function __clojure_fn_4602(){
return (clojure.JS.def(clojure,"every_QMARK_",(function __clojure_fn_4602_every_QMARK_4604(pred_1,coll_2){
var _cnt,_rtn,and__196_3;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[coll_2]))?(((and__196_3=pred_1.apply(null,[clojure.first.apply(null,[coll_2])])),
((and__196_3)?((_cnt=1,_rtn=[pred_1,clojure.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])):(and__196_3)))):(true))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def not-every? (comp not every?))
//---
(function __clojure_fn_4611(){
return (clojure.JS.def(clojure,"not_every_QMARK_",clojure.comp.apply(null,[clojure.not,clojure.every_QMARK_])))}).apply(null,[]);

//======
//(defn some "Returns the first logical true value of (pred x) for any x in coll,\n  else nil." [pred coll] (when (seq coll) (or (pred (first coll)) (recur pred (rest coll)))))
//---
(function __clojure_fn_4620(){
return (clojure.JS.def(clojure,"some",(function __clojure_fn_4620_some_4622(pred_1,coll_2){
var _cnt,_rtn,or__202_3;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[coll_2]))?(((or__202_3=pred_1.apply(null,[clojure.first.apply(null,[coll_2])])),
((or__202_3)?(or__202_3):((_cnt=1,_rtn=[pred_1,clojure.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1]))))):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def not-any? (comp not some))
//---
(function __clojure_fn_4629(){
return (clojure.JS.def(clojure,"not_any_QMARK_",clojure.comp.apply(null,[clojure.not,clojure.some])))}).apply(null,[]);

//======
//(defn map "Returns a lazy seq consisting of the result of applying f to the\n  set of first items of each coll, followed by applying f to the set\n  of second items in each coll, until any one of the colls is\n  exhausted.  Any remaining items in other colls are ignored. Function\n  f should accept number-of-colls arguments." ([f coll] (when (seq coll) (lazy-cons (f (first coll)) (map f (rest coll))))) ([f c1 c2] (when (and (seq c1) (seq c2)) (lazy-cons (f (first c1) (first c2)) (map f (rest c1) (rest c2))))) ([f c1 c2 c3] (when (and (seq c1) (seq c2) (seq c3)) (lazy-cons (f (first c1) (first c2) (first c3)) (map f (rest c1) (rest c2) (rest c3))))) ([f c1 c2 c3 & colls] (let [step (fn step [cs] (when (every? seq cs) (lazy-cons (map first cs) (step (map rest cs)))))] (map (fn* [p1__4632] (apply f p1__4632)) (step (conj colls c3 c2 c1))))))
//---
(function __clojure_fn_4667(){
return (clojure.JS.def(clojure,"map",clojure.JS.variadic(4,(function __clojure_fn_4667_map_4669(f_1,c1_2,c2_3,c3_4){switch(arguments.length){
case 4:var and__196_5,and__196_6;
return (((((and__196_5=clojure.seq.apply(null,[c1_2])),
((and__196_5)?(((and__196_6=clojure.seq.apply(null,[c2_3])),
((and__196_6)?(clojure.seq.apply(null,[c3_4])):(and__196_6)))):(and__196_5))))?((new clojure.lang.LazyCons((function __clojure_fn_4667_map_4669_fn_4684(G__4683_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.first.apply(null,[c1_2]),clojure.first.apply(null,[c2_3]),clojure.first.apply(null,[c3_4])]))}
return (clojure.map.apply(null,[f_1,clojure.rest.apply(null,[c1_2]),clojure.rest.apply(null,[c2_3]),clojure.rest.apply(null,[c3_4])]))})))):(null)))
case 3:var and__196_4;
return (((((and__196_4=clojure.seq.apply(null,[c1_2])),
((and__196_4)?(clojure.seq.apply(null,[c2_3])):(and__196_4))))?((new clojure.lang.LazyCons((function __clojure_fn_4667_map_4669_fn_4678(G__4677_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.first.apply(null,[c1_2]),clojure.first.apply(null,[c2_3])]))}
return (clojure.map.apply(null,[f_1,clojure.rest.apply(null,[c1_2]),clojure.rest.apply(null,[c2_3])]))})))):(null)))
case 2:var coll_2=arguments[1];
return (((clojure.seq.apply(null,[coll_2]))?((new clojure.lang.LazyCons((function __clojure_fn_4667_map_4669_fn_4672(G__4671_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.first.apply(null,[coll_2])]))}
return (clojure.map.apply(null,[f_1,clojure.rest.apply(null,[coll_2])]))})))):(null)))}
var colls_5,step_6,colls_5=clojure.JS.rest_args(this,arguments,4);
return (((step_6=(function __clojure_fn_4667_map_4669_step_4689(cs_1){
var step_0=arguments.callee;
return (((clojure.every_QMARK_.apply(null,[clojure.seq,cs_1]))?((new clojure.lang.LazyCons((function __clojure_fn_4667_map_4669_step_4689_fn_4691(G__4690_1){switch(arguments.length){
case 0:return (clojure.map.apply(null,[clojure.first,cs_1]))}
return (step_0.apply(null,[clojure.map.apply(null,[clojure.rest,cs_1])]))})))):(null)))})),
clojure.map.apply(null,[(function __clojure_fn_4667_map_4669_fn_4696(p1__4632_1){
return (clojure.apply.apply(null,[f_1,p1__4632_1]))}),step_6.apply(null,[clojure.conj.apply(null,[colls_5,c3_4,c2_3,c1_2])])])))}))))}).apply(null,[]);

//======
//(defn mapcat "Returns the result of applying concat to the result of applying map\n  to f and colls.  Thus function f should return a collection." [f & colls] (apply concat (apply map f colls)))
//---
(function __clojure_fn_4707(){
return (clojure.JS.def(clojure,"mapcat",clojure.JS.variadic(1,(function __clojure_fn_4707_mapcat_4709(f_1){
var colls_2,colls_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.apply.apply(null,[clojure.concat,clojure.apply.apply(null,[clojure.map,f_1,colls_2])]))}))))}).apply(null,[]);

//======
//(defn filter "Returns a lazy seq of the items in coll for which\n  (pred item) returns true. pred must be free of side-effects." [pred coll] (when (seq coll) (if (pred (first coll)) (lazy-cons (first coll) (filter pred (rest coll))) (recur pred (rest coll)))))
//---
(function __clojure_fn_4724(){
return (clojure.JS.def(clojure,"filter",(function __clojure_fn_4724_filter_4726(pred_1,coll_2){
var _cnt,_rtn;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[coll_2]))?(((pred_1.apply(null,[clojure.first.apply(null,[coll_2])]))?((new clojure.lang.LazyCons((function __clojure_fn_4724_filter_4726_fn_4729(G__4728_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll_2]))}
return (clojure.filter.apply(null,[pred_1,clojure.rest.apply(null,[coll_2])]))})))):((_cnt=1,_rtn=[pred_1,clojure.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])))):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn take "Returns a lazy seq of the first n items in coll, or all items if\n  there are fewer than n." [n coll] (when (and (pos? n) (seq coll)) (lazy-cons (first coll) (take (dec n) (rest coll)))))
//---
(function __clojure_fn_4746(){
return (clojure.JS.def(clojure,"take",(function __clojure_fn_4746_take_4748(n_1,coll_2){
var and__196_3;
return (((((and__196_3=clojure.lang.Numbers.isPos(n_1)),
((and__196_3)?(clojure.seq.apply(null,[coll_2])):(and__196_3))))?((new clojure.lang.LazyCons((function __clojure_fn_4746_take_4748_fn_4751(G__4750_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll_2]))}
return (clojure.take.apply(null,[clojure.lang.Numbers.dec(n_1),clojure.rest.apply(null,[coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn take-while "Returns a lazy seq of successive items from coll while\n  (pred item) returns true. pred must be free of side-effects." [pred coll] (when (and (seq coll) (pred (first coll))) (lazy-cons (first coll) (take-while pred (rest coll)))))
//---
(function __clojure_fn_4768(){
return (clojure.JS.def(clojure,"take_while",(function __clojure_fn_4768_take_while_4770(pred_1,coll_2){
var and__196_3;
return (((((and__196_3=clojure.seq.apply(null,[coll_2])),
((and__196_3)?(pred_1.apply(null,[clojure.first.apply(null,[coll_2])])):(and__196_3))))?((new clojure.lang.LazyCons((function __clojure_fn_4768_take_while_4770_fn_4773(G__4772_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll_2]))}
return (clojure.take_while.apply(null,[pred_1,clojure.rest.apply(null,[coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn drop "Returns a lazy seq of all but the first n items in coll." [n coll] (if (and (pos? n) (seq coll)) (recur (dec n) (rest coll)) (seq coll)))
//---
(function __clojure_fn_4785(){
return (clojure.JS.def(clojure,"drop",(function __clojure_fn_4785_drop_4787(n_1,coll_2){
var _cnt,_rtn,and__196_3;
do{_cnt=0;_rtn=((((and__196_3=clojure.lang.Numbers.isPos(n_1)),
((and__196_3)?(clojure.seq.apply(null,[coll_2])):(and__196_3))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_1),clojure.rest.apply(null,[coll_2])],n_1=_rtn[0],coll_2=_rtn[1])):(clojure.seq.apply(null,[coll_2])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn drop-last "Return a lazy seq of all but the last n (default 1) items in coll" ([s] (drop-last 1 s)) ([n s] (map (fn [x _] x) (seq s) (drop n s))))
//---
(function __clojure_fn_4801(){
return (clojure.JS.def(clojure,"drop_last",(function __clojure_fn_4801_drop_last_4803(n_1,s_2){switch(arguments.length){
case 1:var s_1=arguments[0];
return (clojure.drop_last.apply(null,[1,s_1]))}
return (clojure.map.apply(null,[(function __clojure_fn_4801_drop_last_4803_fn_4806(x_1,__2){
return (x_1)}),clojure.seq.apply(null,[s_2]),clojure.drop.apply(null,[n_1,s_2])]))})))}).apply(null,[]);

//======
//(defn drop-while "Returns a lazy seq of the items in coll starting from the first\n  item for which (pred item) returns nil." [pred coll] (if (and (seq coll) (pred (first coll))) (recur pred (rest coll)) (seq coll)))
//---
(function __clojure_fn_4817(){
return (clojure.JS.def(clojure,"drop_while",(function __clojure_fn_4817_drop_while_4819(pred_1,coll_2){
var _cnt,_rtn,and__196_3;
do{_cnt=0;_rtn=((((and__196_3=clojure.seq.apply(null,[coll_2])),
((and__196_3)?(pred_1.apply(null,[clojure.first.apply(null,[coll_2])])):(and__196_3))))?((_cnt=1,_rtn=[pred_1,clojure.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])):(clojure.seq.apply(null,[coll_2])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn cycle "Returns a lazy (infinite!) seq of repetitions of the items in\n  coll." [coll] (when (seq coll) (let [rep (fn thisfn [xs] (if xs (lazy-cons (first xs) (thisfn (rest xs))) (recur (seq coll))))] (rep (seq coll)))))
//---
(function __clojure_fn_4836(){
return (clojure.JS.def(clojure,"cycle",(function __clojure_fn_4836_cycle_4838(coll_1){
var rep_2;
return (((clojure.seq.apply(null,[coll_1]))?(((rep_2=(function __clojure_fn_4836_cycle_4838_thisfn_4840(xs_1){
var _cnt,_rtn,thisfn_0=arguments.callee;
do{_cnt=0;_rtn=((xs_1)?((new clojure.lang.LazyCons((function __clojure_fn_4836_cycle_4838_thisfn_4840_fn_4842(G__4841_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[xs_1]))}
return (thisfn_0.apply(null,[clojure.rest.apply(null,[xs_1])]))})))):((_cnt=1,_rtn=[clojure.seq.apply(null,[coll_1])],xs_1=_rtn[0])))
}while(_cnt);return _rtn;})),
rep_2.apply(null,[clojure.seq.apply(null,[coll_1])]))):(null)))})))}).apply(null,[]);

//======
//(defn split-at "Returns a vector of [(take n coll) (drop n coll)]" [n coll] [(take n coll) (drop n coll)])
//---
(function __clojure_fn_4855(){
return (clojure.JS.def(clojure,"split_at",(function __clojure_fn_4855_split_at_4857(n_1,coll_2){
return (clojure.JS.lit_vector([clojure.take.apply(null,[n_1,coll_2]),clojure.drop.apply(null,[n_1,coll_2])]))})))}).apply(null,[]);

//======
//(defn split-with "Returns a vector of [(take-while pred coll) (drop-while pred coll)]" [pred coll] [(take-while pred coll) (drop-while pred coll)])
//---
(function __clojure_fn_4867(){
return (clojure.JS.def(clojure,"split_with",(function __clojure_fn_4867_split_with_4869(pred_1,coll_2){
return (clojure.JS.lit_vector([clojure.take_while.apply(null,[pred_1,coll_2]),clojure.drop_while.apply(null,[pred_1,coll_2])]))})))}).apply(null,[]);

//======
//(defn repeat "Returns a lazy (infinite!) seq of xs." [x] (lazy-cons x (repeat x)))
//---
(function __clojure_fn_4884(){
return (clojure.JS.def(clojure,"repeat",(function __clojure_fn_4884_repeat_4886(x_1){
return ((new clojure.lang.LazyCons((function __clojure_fn_4884_repeat_4886_fn_4889(G__4888_1){switch(arguments.length){
case 0:return (x_1)}
return (clojure.repeat.apply(null,[x_1]))}))))})))}).apply(null,[]);

//======
//(defn replicate "Returns a lazy seq of n xs." [n x] (take n (repeat x)))
//---
(function __clojure_fn_4901(){
return (clojure.JS.def(clojure,"replicate",(function __clojure_fn_4901_replicate_4903(n_1,x_2){
return (clojure.take.apply(null,[n_1,clojure.repeat.apply(null,[x_2])]))})))}).apply(null,[]);

//======
//(defn iterate "Returns a lazy seq of x, (f x), (f (f x)) etc. f must be free of side-effects" [f x] (lazy-cons x (iterate f (f x))))
//---
(function __clojure_fn_4918(){
return (clojure.JS.def(clojure,"iterate",(function __clojure_fn_4918_iterate_4920(f_1,x_2){
return ((new clojure.lang.LazyCons((function __clojure_fn_4918_iterate_4920_fn_4923(G__4922_1){switch(arguments.length){
case 0:return (x_2)}
return (clojure.iterate.apply(null,[f_1,f_1.apply(null,[x_2])]))}))))})))}).apply(null,[]);

//======
//(defn range "Returns a lazy seq of nums from start (inclusive) to end\n  (exclusive), by step, where start defaults to 0 and step to 1." ([end] (if (and (> end 0) (< end clojure.lang.RT/IntegerMaxValue)) (new clojure.lang.Range 0 end) (take end (iterate inc 0)))) ([start end] (if (and (< start end) (< end clojure.lang.RT/IntegerMaxValue)) (new clojure.lang.Range start end) (take (- end start) (iterate inc start)))) ([start end step] (take-while (partial (if (pos? step) > <) end) (iterate (partial + step) start))))
//---
(function __clojure_fn_4937(){
return (clojure.JS.def(clojure,"range",(function __clojure_fn_4937_range_4939(start_1,end_2,step_3){switch(arguments.length){
case 2:var and__196_3;
return (((((and__196_3=clojure.lang.Numbers.lt(start_1,end_2)),
((and__196_3)?(clojure.lang.Numbers.lt(end_2,clojure.lang.RT.IntegerMaxValue)):(and__196_3))))?((new clojure.lang.Range(start_1,end_2))):(clojure.take.apply(null,[clojure.lang.Numbers.minus(end_2,start_1),clojure.iterate.apply(null,[clojure.inc,start_1])]))))
case 1:var and__196_2,end_1=arguments[0];
return (((((and__196_2=clojure.lang.Numbers.gt(end_1,0)),
((and__196_2)?(clojure.lang.Numbers.lt(end_1,clojure.lang.RT.IntegerMaxValue)):(and__196_2))))?((new clojure.lang.Range(0,end_1))):(clojure.take.apply(null,[end_1,clojure.iterate.apply(null,[clojure.inc,0])]))))}
return (clojure.take_while.apply(null,[clojure.partial.apply(null,[((clojure.lang.Numbers.isPos(step_3))?(clojure._GT_):(clojure._LT_)),end_2]),clojure.iterate.apply(null,[clojure.partial.apply(null,[clojure._PLUS_,step_3]),start_1])]))})))}).apply(null,[]);

//======
//(defn merge "Returns a map that consists of the rest of the maps conj-ed onto\n  the first.  If a key occurs in more than one map, the mapping from\n  the latter (left-to-right) will be the mapping in the result." [& maps] (reduce conj maps))
//---
(function __clojure_fn_4951(){
return (clojure.JS.def(clojure,"merge",clojure.JS.variadic(0,(function __clojure_fn_4951_merge_4953(){
var maps_1,maps_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.reduce.apply(null,[clojure.conj,maps_1]))}))))}).apply(null,[]);

//======
//(defn merge-with "Returns a map that consists of the rest of the maps conj-ed onto\n  the first.  If a key occurs in more than one map, the mapping(s)\n  from the latter (left-to-right) will be combined with the mapping in\n  the result by calling (f val-in-result val-in-latter)." [f & maps] (let [merge-entry (fn [m e] (let [k (key e) v (val e)] (if (contains? m k) (assoc m k (f (m k) v)) (assoc m k v)))) merge2 (fn [m1 m2] (reduce merge-entry m1 (seq m2)))] (reduce merge2 maps)))
//---
(function __clojure_fn_4969(){
return (clojure.JS.def(clojure,"merge_with",clojure.JS.variadic(1,(function __clojure_fn_4969_merge_with_4971(f_1){
var maps_2,merge2_4,merge_entry_3,maps_2=clojure.JS.rest_args(this,arguments,1);
return (((merge_entry_3=(function __clojure_fn_4969_merge_with_4971_merge_entry_4973(m_1,e_2){
var k_3,v_4;
return (((k_3=clojure.key.apply(null,[e_2])),
(v_4=clojure.val.apply(null,[e_2])),
((clojure.contains_QMARK_.apply(null,[m_1,k_3]))?(clojure.assoc.apply(null,[m_1,k_3,f_1.apply(null,[m_1.apply(null,[k_3]),v_4])])):(clojure.assoc.apply(null,[m_1,k_3,v_4])))))})),
(merge2_4=(function __clojure_fn_4969_merge_with_4971_merge2_4976(m1_1,m2_2){
return (clojure.reduce.apply(null,[merge_entry_3,m1_1,clojure.seq.apply(null,[m2_2])]))})),
clojure.reduce.apply(null,[merge2_4,maps_2])))}))))}).apply(null,[]);

//======
//(defn zipmap "Returns a map with the keys mapped to the corresponding vals." [keys vals] (loop [map {} ks (seq keys) vs (seq vals)] (if (and ks vs) (recur (assoc map (first ks) (first vs)) (rest ks) (rest vs)) map)))
//---
(function __clojure_fn_4987(){
return (clojure.JS.def(clojure,"zipmap",(function __clojure_fn_4987_zipmap_4989(keys_1,vals_2){
var ks_4,vs_5,and__196_6,map_3;
return (((function __loop(){var _rtn,_cnt;(map_3=clojure.lang.PersistentHashMap.EMPTY),
(ks_4=clojure.seq.apply(null,[keys_1])),
(vs_5=clojure.seq.apply(null,[vals_2]));do{_cnt=0;
_rtn=((((and__196_6=ks_4),
((and__196_6)?(vs_5):(and__196_6))))?((_cnt=1,_rtn=[clojure.assoc.apply(null,[map_3,clojure.first.apply(null,[ks_4]),clojure.first.apply(null,[vs_5])]),clojure.rest.apply(null,[ks_4]),clojure.rest.apply(null,[vs_5])],map_3=_rtn[0],ks_4=_rtn[1],vs_5=_rtn[2])):(map_3))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn line-seq "Returns the lines of text from rdr as a lazy sequence of strings.\n  rdr must implement java.io.BufferedReader." [rdr] (let [line (. rdr (readLine))] (when line (lazy-cons line (line-seq rdr)))))
//---
(function __clojure_fn_5004(){
return (clojure.JS.def(clojure,"line_seq",(function __clojure_fn_5004_line_seq_5006(rdr_1){
var line_2;
return (((line_2=(rdr_1).readLine()),
((line_2)?((new clojure.lang.LazyCons((function __clojure_fn_5004_line_seq_5006_fn_5009(G__5008_1){switch(arguments.length){
case 0:return (line_2)}
return (clojure.line_seq.apply(null,[rdr_1]))})))):(null))))})))}).apply(null,[]);

//======
//(defn comparator "Returns an implementation of java.util.Comparator based upon pred." [pred] (fn [x y] (cond (pred x y) -1 (pred y x) 1 :else 0)))
//---
(function __clojure_fn_5024(){
return (clojure.JS.def(clojure,"comparator",(function __clojure_fn_5024_comparator_5026(pred_1){
return ((function __clojure_fn_5024_comparator_5026_fn_5028(x_1,y_2){
return (((pred_1.apply(null,[x_1,y_2]))?(-1):(((pred_1.apply(null,[y_2,x_1]))?(1):(((clojure.keyword("","else"))?(0):(null)))))))}))})))}).apply(null,[]);

//======
//(defn sort "Returns a sorted sequence of the items in coll. If no comparator is\n  supplied, uses compare. comparator must\n  implement java.util.Comparator." ([coll] (sort compare coll)) ([comp coll] (when (and coll (not (. coll (isEmpty)))) (let [a (. coll (toArray))] (clojure.lang.RT/sortArray a comp) (seq a)))))
//---
(function __clojure_fn_5040(){
return (clojure.JS.def(clojure,"sort",(function __clojure_fn_5040_sort_5042(comp_1,coll_2){switch(arguments.length){
case 1:var coll_1=arguments[0];
return (clojure.sort.apply(null,[clojure.compare,coll_1]))}
var a_3,and__196_3;
return (((((and__196_3=coll_2),
((and__196_3)?(clojure.not.apply(null,[(coll_2).isEmpty()])):(and__196_3))))?(((a_3=(coll_2).toArray()),
clojure.lang.RT.sortArray(a_3,comp_1),
clojure.seq.apply(null,[a_3]))):(null)))})))}).apply(null,[]);

//======
//(defn sort-by "Returns a sorted sequence of the items in coll, where the sort\n  order is determined by comparing (keyfn item).  If no comparator is\n  supplied, uses compare. comparator must\n  implement java.util.Comparator." ([keyfn coll] (sort-by keyfn compare coll)) ([keyfn comp coll] (sort (fn [x y] (. comp (compare (keyfn x) (keyfn y)))) coll)))
//---
(function __clojure_fn_5057(){
return (clojure.JS.def(clojure,"sort_by",(function __clojure_fn_5057_sort_by_5059(keyfn_1,comp_2,coll_3){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (clojure.sort_by.apply(null,[keyfn_1,clojure.compare,coll_2]))}
return (clojure.sort.apply(null,[(function __clojure_fn_5057_sort_by_5059_fn_5062(x_1,y_2){
return ((comp_2).compare(keyfn_1.apply(null,[x_1]),keyfn_1.apply(null,[y_2])))}),coll_3]))})))}).apply(null,[]);
// Skipping: (defn eval "Evaluates the form data structure (not text!) and returns the result." [form] (. clojure.lang.Compiler (eval form)))
// Skipping: (defmacro doseq "Repeatedly executes body (presumably for side-effects) with\n  binding-form bound to successive items from coll.  Does not retain\n  the head of the sequence. Returns nil." [item list & body] (clojure/concat (clojure/list (quote clojure/loop)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote list__5073)) (clojure/list (clojure/concat (clojure/list (quote clojure/seq)) (clojure/list list)))))) (clojure/list (clojure/concat (clojure/list (quote clojure/when)) (clojure/list (quote list__5073)) (clojure/list (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list item) (clojure/list (clojure/concat (clojure/list (quote clojure/first)) (clojure/list (quote list__5073))))))) body)) (clojure/list (clojure/concat (clojure/list (quote recur)) (clojure/list (clojure/concat (clojure/list (quote clojure/rest)) (clojure/list (quote list__5073))))))))))

//======
//(defn scan [& args] (throw (clojure.lang.RT/makeException "scan is now called dorun")))
//---
(function __clojure_fn_5086(){
return (clojure.JS.def(clojure,"scan",clojure.JS.variadic(0,(function __clojure_fn_5086_scan_5088(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw clojure.lang.RT.makeException("scan is now called dorun")})())}))))}).apply(null,[]);

//======
//(defn touch [& args] (throw (clojure.lang.RT/makeException "touch is now called doall")))
//---
(function __clojure_fn_5098(){
return (clojure.JS.def(clojure,"touch",clojure.JS.variadic(0,(function __clojure_fn_5098_touch_5100(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw clojure.lang.RT.makeException("touch is now called doall")})())}))))}).apply(null,[]);

//======
//(defn dorun "When lazy sequences are produced via functions that have side\n  effects, any effects other than those needed to produce the first\n  element in the seq do not occur until the seq is consumed. dorun can\n  be used to force any effects. Walks through the successive rests of\n  the seq, does not retain the head and returns nil." ([coll] (when (and (seq coll) (or (first coll) true)) (recur (rest coll)))) ([n coll] (when (and (seq coll) (pos? n) (or (first coll) true)) (recur (dec n) (rest coll)))))
//---
(function __clojure_fn_5111(){
return (clojure.JS.def(clojure,"dorun",(function __clojure_fn_5111_dorun_5113(n_1,coll_2){switch(arguments.length){
case 1:var _cnt,_rtn,or__202_3,and__196_2,coll_1=arguments[0];
do{_cnt=0;_rtn=((((and__196_2=clojure.seq.apply(null,[coll_1])),
((and__196_2)?(((or__202_3=clojure.first.apply(null,[coll_1])),
((or__202_3)?(or__202_3):(true)))):(and__196_2))))?((_cnt=1,_rtn=[clojure.rest.apply(null,[coll_1])],coll_1=_rtn[0])):(null))
}while(_cnt);return _rtn;}
var _cnt,_rtn,or__202_5,and__196_3,and__196_4;
do{_cnt=0;_rtn=((((and__196_3=clojure.seq.apply(null,[coll_2])),
((and__196_3)?(((and__196_4=clojure.lang.Numbers.isPos(n_1)),
((and__196_4)?(((or__202_5=clojure.first.apply(null,[coll_2])),
((or__202_5)?(or__202_5):(true)))):(and__196_4)))):(and__196_3))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_1),clojure.rest.apply(null,[coll_2])],n_1=_rtn[0],coll_2=_rtn[1])):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn doall "When lazy sequences are produced via functions that have side\n  effects, any effects other than those needed to produce the first\n  element in the seq do not occur until the seq is consumed. doall can\n  be used to force any effects. Walks through the successive rests of\n  the seq, retains the head and returns it, thus causing the entire\n  seq to reside in memory at one time." ([coll] (dorun coll) coll) ([n coll] (dorun n coll) coll))
//---
(function __clojure_fn_5125(){
return (clojure.JS.def(clojure,"doall",(function __clojure_fn_5125_doall_5127(n_1,coll_2){switch(arguments.length){
case 1:var coll_1=arguments[0];
return (clojure.dorun.apply(null,[coll_1]),
coll_1)}
return (clojure.dorun.apply(null,[n_1,coll_2]),
coll_2)})))}).apply(null,[]);
// Skipping: (defn await "Blocks the current thread (indefinitely!) until all actions\n  dispatched thus far, from this thread or agent, to the agent(s) have\n  occurred." [& agents] (when *agent* (throw (clojure.lang.RT/makeException "Can't await in agent action"))) (let [latch (new java.util.concurrent.CountDownLatch (count agents)) count-down (fn [agent] (. latch (countDown)) agent)] (doseq agent agents (send agent count-down)) (. latch (await))))

//======
//(defn await1 [a] (when (pos? (.getQueueCount a)) (await a)) a)
//---
(function __clojure_fn_5147(){
return (clojure.JS.def(clojure,"await1",(function __clojure_fn_5147_await1_5149(a_1){
return (((clojure.lang.Numbers.isPos((a_1).getQueueCount()))?(clojure.await.apply(null,[a_1])):(null)),
a_1)})))}).apply(null,[]);
// Skipping: (defn await-for "Blocks the current thread until all actions dispatched thus\n  far (from this thread or agent) to the agents have occurred, or the\n  timeout (in milliseconds) has elapsed. Returns nil if returning due\n  to timeout, non-nil otherwise." [timeout-ms & agents] (when *agent* (throw (clojure.lang.RT/makeException "Can't await in agent action"))) (let [latch (new java.util.concurrent.CountDownLatch (count agents)) count-down (fn [agent] (. latch (countDown)) agent)] (doseq agent agents (send agent count-down)) (. latch (await timeout-ms (. java.util.concurrent.TimeUnit MILLISECONDS)))))
// Skipping: (defmacro dotimes "Repeatedly executes body (presumably for side-effects) with name\n  bound to integers from 0 through n-1." [i n & body] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote n__5162)) (clojure/list (clojure/concat (clojure/list (quote clojure/int)) (clojure/list n)))))) (clojure/list (clojure/concat (clojure/list (quote clojure/loop)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list i) (clojure/list (clojure/concat (clojure/list (quote clojure/int)) (clojure/list 0)))))) (clojure/list (clojure/concat (clojure/list (quote clojure/when)) (clojure/list (clojure/concat (clojure/list (quote clojure/<)) (clojure/list i) (clojure/list (quote n__5162)))) body (clojure/list (clojure/concat (clojure/list (quote recur)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked-inc)) (clojure/list i)))))))))))
// Skipping: (defn import "import-list => (package-symbol class-name-symbols*)\n\n  For each name in class-name-symbols, adds a mapping from name to the\n  class named by package.name to the current namespace. Use :import in the ns \n  macro in preference to calling this directly." [& import-lists] (when import-lists (let [ns *ns* pkg (ffirst import-lists) classes (rfirst import-lists)] (doseq c classes (. ns (importClass c (. Class (forName (str pkg "." c))))))) (apply import (rest import-lists))))

//======
//(import (quote (clojure.lang RT)))
//---
(function __clojure_fn_5178(){
return (clojure.import_.apply(null,[clojure.JS.lit_list(["'clojure.lang","'RT"])]))}).apply(null,[]);

//======
//(defn into-array "Returns an array of the type of the first element in coll,\n  containing the contents of coll, which must be of a compatible\n  type." [aseq] (. clojure.lang.RT (seqToTypedArray (seq aseq))))
//---
(function __clojure_fn_5187(){
return (clojure.JS.def(clojure,"into_array",(function __clojure_fn_5187_into_array_5189(aseq_1){
return (clojure.lang.RT.seqToTypedArray(clojure.seq.apply(null,[aseq_1])))})))}).apply(null,[]);

//======
//(defn into "Returns a new coll consisting of to-coll with all of the items of\n  from-coll conjoined." [to from] (let [ret to items (seq from)] (if items (recur (conj ret (first items)) (rest items)) ret)))
//---
(function __clojure_fn_5199(){
return (clojure.JS.def(clojure,"into",(function __clojure_fn_5199_into_5201(to_1,from_2){
var _cnt,_rtn,ret_3,items_4;
do{_cnt=0;_rtn=((ret_3=to_1),
(items_4=clojure.seq.apply(null,[from_2])),
((items_4)?((_cnt=1,_rtn=[clojure.conj.apply(null,[ret_3,clojure.first.apply(null,[items_4])]),clojure.rest.apply(null,[items_4])],to_1=_rtn[0],from_2=_rtn[1])):(ret_3)))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn array [& items] (into-array items))
//---
(function __clojure_fn_5211(){
return (clojure.JS.def(clojure,"array",clojure.JS.variadic(0,(function __clojure_fn_5211_array_5213(){
var items_1,items_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.into_array.apply(null,[items_1]))}))))}).apply(null,[]);
// Skipping: (defn class "Returns the Class of x" [x] (if (nil? x) x (. x (getClass))))
// Skipping: (defmulti print-method (fn [x writer] (class x)))

//======
//(defn pr "Prints the object(s) to the output stream that is the current value\n  of *out*.  Prints the object(s), separated by spaces if there is\n  more than one.  By default, pr and prn print in a way that objects\n  can be read by the reader" ([] nil) ([x] (print-method x *out*) nil) ([x & more] (pr x) (. *out* (append \space)) (apply pr more)))
//---
(function __clojure_fn_5237(){
return (clojure.JS.def(clojure,"pr",clojure.JS.variadic(1,(function __clojure_fn_5237_pr_5239(x_1){switch(arguments.length){
case 1:return (clojure.print_method.apply(null,[x_1,clojure._STAR_out_STAR_]),
null)
case 0:return (null)}
var more_2,more_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.pr.apply(null,[x_1]),
(clojure._STAR_out_STAR_).append(" "),
clojure.apply.apply(null,[clojure.pr,more_2]))}))))}).apply(null,[]);

//======
//(defn newline "Writes a newline to the output stream that is the current value of\n  *out*" [] (. *out* (append \newline)) nil)
//---
(function __clojure_fn_5251(){
return (clojure.JS.def(clojure,"newline",(function __clojure_fn_5251_newline_5253(){
return ((clojure._STAR_out_STAR_).append("\n"),
null)})))}).apply(null,[]);

//======
//(defn flush "Flushes the output stream that is the current value of\n  *out*" [] (. *out* (flush)) nil)
//---
(function __clojure_fn_5263(){
return (clojure.JS.def(clojure,"flush",(function __clojure_fn_5263_flush_5265(){
return ((clojure._STAR_out_STAR_).flush(),
null)})))}).apply(null,[]);

//======
//(defn prn "Same as pr followed by (newline). Observes *flush-on-newline*" [& more] (apply pr more) (newline) (when *flush-on-newline* (flush)))
//---
(function __clojure_fn_5275(){
return (clojure.JS.def(clojure,"prn",clojure.JS.variadic(0,(function __clojure_fn_5275_prn_5277(){
var more_1,more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[clojure.pr,more_1]),
clojure.newline.apply(null,[]),
((clojure._STAR_flush_on_newline_STAR_)?(clojure.flush.apply(null,[])):(null)))}))))}).apply(null,[]);

//======
//(defn print "Prints the object(s) to the output stream that is the current value\n  of *out*.  print and println produce output for human consumption." [& more] (binding [*print-readably* nil] (apply pr more)))
//---
(function __clojure_fn_5287(){
return (clojure.JS.def(clojure,"print",clojure.JS.variadic(0,(function __clojure_fn_5287_print_5289(){
var more_1,more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_print_readably_STAR_,null])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.pr,more_1]))}
finally{clojure.lang.Var.popThreadBindings()}})())}))))}).apply(null,[]);

//======
//(defn println "Same as print followed by (newline)" [& more] (binding [*print-readably* nil] (apply prn more)))
//---
(function __clojure_fn_5299(){
return (clojure.JS.def(clojure,"println",clojure.JS.variadic(0,(function __clojure_fn_5299_println_5301(){
var more_1,more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_print_readably_STAR_,null])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.prn,more_1]))}
finally{clojure.lang.Var.popThreadBindings()}})())}))))}).apply(null,[]);

//======
//(defn read "Reads the next object from stream, which must be an instance of\n  java.io.PushbackReader or some derivee.  stream defaults to the\n  current value of *in* ." ([] (read *in*)) ([stream] (read stream true nil)) ([stream eof-error? eof-value] (read stream eof-error? eof-value false)) ([stream eof-error? eof-value recursive?] (. clojure.lang.LispReader (read stream eof-error? eof-value recursive?))))
//---
(function __clojure_fn_5314(){
return (clojure.JS.def(clojure,"read",(function __clojure_fn_5314_read_5316(stream_1,eof_error_QMARK__2,eof_value_3,recursive_QMARK__4){switch(arguments.length){
case 3:return (clojure.read.apply(null,[stream_1,eof_error_QMARK__2,eof_value_3,false]))
case 1:return (clojure.read.apply(null,[stream_1,true,null]))
case 0:return (clojure.read.apply(null,[clojure._STAR_in_STAR_]))}
return (clojure.lang.LispReader.read(stream_1,eof_error_QMARK__2,eof_value_3,recursive_QMARK__4))})))}).apply(null,[]);

//======
//(defn read-line "Reads the next line from stream that is the current value of *in* ." [] (. *in* (readLine)))
//---
(function __clojure_fn_5329(){
return (clojure.JS.def(clojure,"read_line",(function __clojure_fn_5329_read_line_5331(){
return ((clojure._STAR_in_STAR_).readLine())})))}).apply(null,[]);
// Skipping: (defmacro with-open "Evaluates body in a try expression with name bound to the value of\n  init, and a finally clause that calls (. name (close))." [name init & body] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list name) (clojure/list init)))) (clojure/list (clojure/concat (clojure/list (quote try)) body (clojure/list (clojure/concat (clojure/list (quote finally)) (clojure/list (clojure/concat (clojure/list (quote .)) (clojure/list name) (clojure/list (clojure/concat (clojure/list (quote clojure/close))))))))))))
// Skipping: (defmacro doto "Evaluates x then calls all of the methods with the supplied\n  arguments in succession on the resulting object, returning it.\n\n  (doto (new java.util.HashMap) (put \"a\" 1) (put \"b\" 2))" [x & members] (let [gx (gensym)] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list gx) (clojure/list x)))) (clojure/list (clojure/concat (clojure/list (quote do)) (map (fn [m] (list (quote .) gx m)) members))) (clojure/list gx))))
// Skipping: (defmacro memfn "Expands into code that creates a fn that expects to be passed an\n  object and any args and calls the named instance method on the\n  object passing the args. Use when you want to treat a Java method as\n  a first-class fn." [name & args] (clojure/concat (clojure/list (quote clojure/fn)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote target__5350)) args))) (clojure/list (clojure/concat (clojure/list (quote .)) (clojure/list (quote target__5350)) (clojure/list (clojure/concat (clojure/list name) args))))))
// Skipping: (defmacro time "Evaluates expr and prints the time it took.  Returns the value of\n expr." [expr] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote start__5357)) (clojure/list (clojure/concat (clojure/list (quote .)) (clojure/list (quote java.lang.System)) (clojure/list (clojure/concat (clojure/list (quote clojure/nanoTime)))))) (clojure/list (quote ret__5358)) (clojure/list expr)))) (clojure/list (clojure/concat (clojure/list (quote clojure/prn)) (clojure/list (clojure/concat (clojure/list (quote clojure/str)) (clojure/list "Elapsed time: ") (clojure/list (clojure/concat (clojure/list (quote clojure//)) (clojure/list (clojure/concat (clojure/list (quote clojure/double)) (clojure/list (clojure/concat (clojure/list (quote clojure/-)) (clojure/list (clojure/concat (clojure/list (quote .)) (clojure/list (quote java.lang.System)) (clojure/list (clojure/concat (clojure/list (quote clojure/nanoTime)))))) (clojure/list (quote start__5357)))))) (clojure/list 1000000.0))) (clojure/list " msecs"))))) (clojure/list (quote ret__5358))))

//======
//(defn num "Coerce to Number" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/num)) (clojure/list x))))), :tag Number} [x] (. clojure.lang.Numbers (num x)))
//---
(function __clojure_fn_5374(){
return (clojure.JS.def(clojure,"num",(function __clojure_fn_5374_num_5379(x_1){
return (clojure.lang.Numbers.num(x_1))})))}).apply(null,[]);

//======
//(defn int "Coerce to int" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/intCast)) (clojure/list x))))), :tag Integer} [x] (. clojure.lang.RT (intCast x)))
//---
(function __clojure_fn_5392(){
return (clojure.JS.def(clojure,"int",(function __clojure_fn_5392_int_5397(x_1){
return (clojure.lang.RT.intCast(x_1))})))}).apply(null,[]);

//======
//(defn long "Coerce to long" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/longCast)) (clojure/list x))))), :tag Long} [x] (. x (longValue)))
//---
(function __clojure_fn_5410(){
return (clojure.JS.def(clojure,"long",(function __clojure_fn_5410_long_5415(x_1){
return ((x_1).longValue())})))}).apply(null,[]);

//======
//(defn float "Coerce to float" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/floatCast)) (clojure/list x))))), :tag Float} [x] (. x (floatValue)))
//---
(function __clojure_fn_5428(){
return (clojure.JS.def(clojure,"float",(function __clojure_fn_5428_float_5433(x_1){
return ((x_1).floatValue())})))}).apply(null,[]);

//======
//(defn double "Coerce to double" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/doubleCast)) (clojure/list x))))), :tag Double} [x] (. x (doubleValue)))
//---
(function __clojure_fn_5446(){
return (clojure.JS.def(clojure,"double",(function __clojure_fn_5446_double_5451(x_1){
return ((x_1).doubleValue())})))}).apply(null,[]);

//======
//(defn short "Coerce to short" {:tag Short} [x] (. x (shortValue)))
//---
(function __clojure_fn_5461(){
return (clojure.JS.def(clojure,"short_",(function __clojure_fn_5461_short_5463(x_1){
return ((x_1).shortValue())})))}).apply(null,[]);

//======
//(defn byte "Coerce to byte" {:tag Byte} [x] (. x (byteValue)))
//---
(function __clojure_fn_5473(){
return (clojure.JS.def(clojure,"byte_",(function __clojure_fn_5473_byte_5475(x_1){
return ((x_1).byteValue())})))}).apply(null,[]);

//======
//(defn char "Coerce to char" {:tag Character} [x] (. clojure.lang.RT (charCast x)))
//---
(function __clojure_fn_5485(){
return (clojure.JS.def(clojure,"char_",(function __clojure_fn_5485_char_5487(x_1){
return (clojure.lang.RT.charCast(x_1))})))}).apply(null,[]);

//======
//(defn boolean "Coerce to boolean" {:tag Boolean} [x] (if x true false))
//---
(function __clojure_fn_5497(){
return (clojure.JS.def(clojure,"boolean_",(function __clojure_fn_5497_boolean_5499(x_1){
return (((x_1)?(true):(false)))})))}).apply(null,[]);
// Skipping: (defn bigint "Coerce to BigInteger" {:tag BigInteger} [x] (. BigInteger valueOf x))
// Skipping: (defn bigdec "Coerce to BigDecimal" {:tag BigDecimal} [x] (. BigDecimal valueOf x))

//======
//(import (quote (java.lang.reflect Array)))
//---
(function __clojure_fn_5518(){
return (clojure.import_.apply(null,[clojure.JS.lit_list(["'java.lang.reflect","'Array"])]))}).apply(null,[]);

//======
//(defn alength "Returns the length of the Java array. Works on arrays of all\n  types." {:inline (fn [a] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/alength)) (clojure/list a)))))} [array] (. clojure.lang.RT (alength array)))
//---
(function __clojure_fn_5530(){
return (clojure.JS.def(clojure,"alength",(function __clojure_fn_5530_alength_5535(array_1){
return (clojure.lang.RT.alength(array_1))})))}).apply(null,[]);

//======
//(defn aclone "Returns a clone of the Java array. Works on arrays of known\n  types." {:inline (fn [a] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/aclone)) (clojure/list a)))))} [array] (. clojure.lang.RT (aclone array)))
//---
(function __clojure_fn_5548(){
return (clojure.JS.def(clojure,"aclone",(function __clojure_fn_5548_aclone_5553(array_1){
return (clojure.lang.RT.aclone(array_1))})))}).apply(null,[]);

//======
//(defn aget "Returns the value at the index/indices. Works on Java arrays of all\n  types." {:inline (fn [a i] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/aget)) (clojure/list a) (clojure/list i))))), :inline-arities #{2}} ([array idx] (RT/aget array idx)) ([array idx & idxs] (apply aget (aget array idx) idxs)))
//---
(function __clojure_fn_5567(){
return (clojure.JS.def(clojure,"aget",clojure.JS.variadic(2,(function __clojure_fn_5567_aget_5572(array_1,idx_2){switch(arguments.length){
case 2:return (clojure.lang.RT.aget(array_1,idx_2))}
var idxs_3,idxs_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.apply.apply(null,[clojure.aget,clojure.lang.RT.aget(array_1,idx_2),idxs_3]))}))))}).apply(null,[]);

//======
//(defn aset "Sets the value at the index/indices. Works on Java arrays of\n  reference types. Returns val." {:inline (fn [a i v] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/aset)) (clojure/list a) (clojure/list i) (clojure/list v))))), :inline-arities #{3}} ([array idx val] (RT/aset array idx val) val) ([array idx idx2 & idxv] (apply aset (aget array idx) idx2 idxv)))
//---
(function __clojure_fn_5587(){
return (clojure.JS.def(clojure,"aset",clojure.JS.variadic(3,(function __clojure_fn_5587_aset_5592(array_1,idx_2,idx2_3){switch(arguments.length){
case 3:var val_3=arguments[2];
return (clojure.lang.RT.aset(array_1,idx_2,val_3),
val_3)}
var idxv_4,idxv_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply.apply(null,[clojure.aset,clojure.lang.RT.aget(array_1,idx_2),idx2_3,idxv_4]))}))))}).apply(null,[]);
// Skipping: (defmacro def-aset [name method coerce] (clojure/concat (clojure/list (quote clojure/defn)) (clojure/list name) (clojure/list (clojure/apply clojure/hash-map (clojure/concat (clojure/list :arglists) (clojure/list (clojure/concat (clojure/list (quote quote)) (clojure/list (clojure/concat (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote array)) (clojure/list (quote idx)) (clojure/list (quote val))))) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote array)) (clojure/list (quote idx)) (clojure/list (quote idx2)) (clojure/list (quote &)) (clojure/list (quote idxv)))))))))))) (clojure/list (clojure/concat (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote array__5597)) (clojure/list (quote idx__5598)) (clojure/list (quote val__5599))))) (clojure/list (clojure/concat (clojure/list (quote .)) (clojure/list (quote java.lang.reflect.Array)) (clojure/list (clojure/concat (clojure/list method) (clojure/list (quote array__5597)) (clojure/list (quote idx__5598)) (clojure/list (clojure/concat (clojure/list coerce) (clojure/list (quote val__5599)))))))) (clojure/list (quote val__5599)))) (clojure/list (clojure/concat (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote array__5597)) (clojure/list (quote idx__5598)) (clojure/list (quote idx2__5600)) (clojure/list (quote &)) (clojure/list (quote idxv__5601))))) (clojure/list (clojure/concat (clojure/list (quote clojure/apply)) (clojure/list name) (clojure/list (clojure/concat (clojure/list (quote clojure/aget)) (clojure/list (quote array__5597)) (clojure/list (quote idx__5598)))) (clojure/list (quote idx2__5600)) (clojure/list (quote idxv__5601))))))))
// Skipping: (def-aset aset-int setInt int)
// Skipping: (def-aset aset-long setLong long)
// Skipping: (def-aset aset-boolean setBoolean boolean)
// Skipping: (def-aset aset-float setFloat float)
// Skipping: (def-aset aset-double setDouble double)
// Skipping: (def-aset aset-short setShort short)
// Skipping: (def-aset aset-byte setByte byte)
// Skipping: (def-aset aset-char setChar char)
// Skipping: (defn make-array "Creates and returns an array of instances of the specified class of\n  the specified dimension(s).  Note that a class object is required.\n  Class objects can be obtained by using their imported or\n  fully-qualified name.  Class objects for the primitive types can be\n  obtained using, e.g., (. Integer TYPE)." ([type len] (. Array (newInstance type (int len)))) ([type dim & more-dims] (let [dims (cons dim more-dims) dimarray (make-array (. Integer TYPE) (count dims))] (dotimes i (alength dimarray) (aset-int dimarray i (nth dims i))) (. Array (newInstance type dimarray)))))
// Skipping: (defn to-array-2d "Returns a (potentially-ragged) 2-dimensional array of Objects\n  containing the contents of coll, which can be any Collection of any\n  Collection." [coll] (let [ret (make-array (. Class (forName "[Ljava.lang.Object;")) (. coll (size)))] (loop [i 0 xs (seq coll)] (when xs (aset ret i (to-array (first xs))) (recur (inc i) (rest xs)))) ret))
// Skipping: (defn macroexpand-1 "If form represents a macro form, returns its expansion,\n  else returns form." [form] (. clojure.lang.Compiler (macroexpand1 form)))
// Skipping: (defn macroexpand "Repeatedly calls macroexpand-1 on form until it no longer\n  represents a macro form, then returns it.  Note neither\n  macroexpand-1 nor macroexpand expand macros in subforms." [form] (let [ex (macroexpand-1 form)] (if (identical? ex form) form (macroexpand ex))))

//======
//(defn create-struct "Returns a structure basis object." [& keys] (. clojure.lang.PersistentStructMap (createSlotMap keys)))
//---
(function __clojure_fn_5695(){
return (clojure.JS.def(clojure,"create_struct",clojure.JS.variadic(0,(function __clojure_fn_5695_create_struct_5697(){
var keys_1,keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentStructMap.createSlotMap(keys_1))}))))}).apply(null,[]);
// Skipping: (defmacro defstruct "Same as (def name (create-struct keys...))" [name & keys] (clojure/concat (clojure/list (quote def)) (clojure/list name) (clojure/list (clojure/concat (clojure/list (quote clojure/create-struct)) keys))))

//======
//(defn struct-map "Returns a new structmap instance with the keys of the\n  structure-basis. keyvals may contain all, some or none of the basis\n  keys - where values are not supplied they will default to nil.\n  keyvals can also contain keys not in the basis." [s & inits] (. clojure.lang.PersistentStructMap (create s inits)))
//---
(function __clojure_fn_5713(){
return (clojure.JS.def(clojure,"struct_map",clojure.JS.variadic(1,(function __clojure_fn_5713_struct_map_5715(s_1){
var inits_2,inits_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentStructMap.create(s_1,inits_2))}))))}).apply(null,[]);

//======
//(defn struct "Returns a new structmap instance with the keys of the\n  structure-basis. vals must be supplied for basis keys in order -\n  where values are not supplied they will default to nil." [s & vals] (. clojure.lang.PersistentStructMap (construct s vals)))
//---
(function __clojure_fn_5725(){
return (clojure.JS.def(clojure,"struct",clojure.JS.variadic(1,(function __clojure_fn_5725_struct_5727(s_1){
var vals_2,vals_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentStructMap.construct(s_1,vals_2))}))))}).apply(null,[]);

//======
//(defn accessor "Returns a fn that, given an instance of a structmap with the basis,\n  returns the value at the key.  The key must be in the basis. The\n  returned function should be (slightly) more efficient than using\n  get, but such use of accessors should be limited to known\n  performance-critical areas." [s key] (. clojure.lang.PersistentStructMap (getAccessor s key)))
//---
(function __clojure_fn_5737(){
return (clojure.JS.def(clojure,"accessor",(function __clojure_fn_5737_accessor_5739(s_1,key_2){
return (clojure.lang.PersistentStructMap.getAccessor(s_1,key_2))})))}).apply(null,[]);

//======
//(defn subvec "Returns a persistent vector of the items in vector from\n  start (inclusive) to end (exclusive).  If end is not supplied,\n  defaults to (count vector). This operation is O(1) and very fast, as\n  the resulting vector shares structure with the original and no\n  trimming is done." ([v start] (subvec v start (count v))) ([v start end] (. clojure.lang.RT (subvec v start end))))
//---
(function __clojure_fn_5750(){
return (clojure.JS.def(clojure,"subvec",(function __clojure_fn_5750_subvec_5752(v_1,start_2,end_3){switch(arguments.length){
case 2:return (clojure.subvec.apply(null,[v_1,start_2,clojure.count.apply(null,[v_1])]))}
return (clojure.lang.RT.subvec(v_1,start_2,end_3))})))}).apply(null,[]);
// Skipping: (defn load-reader "Sequentially read and evaluate the set of forms contained in the\n  stream/file" [rdr] (. clojure.lang.Compiler (load rdr)))
// Skipping: (defn load-string "Sequentially read and evaluate the set of forms contained in the\n  string" [s] (let [rdr (-> (java.io.StringReader. s) (clojure.lang.LineNumberingPushbackReader.))] (load-reader rdr)))

//======
//(defn resultset-seq "Creates and returns a lazy sequence of structmaps corresponding to\n  the rows in the java.sql.ResultSet rs" [rs] (let [rsmeta (. rs (getMetaData)) idxs (range 1 (inc (. rsmeta (getColumnCount)))) keys (map (comp keyword (memfn toLowerCase)) (map (fn [i] (. rsmeta (getColumnName i))) idxs)) row-struct (apply create-struct keys) row-values (fn [] (map (fn [i] (. rs (getObject i))) idxs)) rows (fn thisfn [] (when (. rs (next)) (lazy-cons (apply struct row-struct (row-values)) (thisfn))))] (rows)))
//---
(function __clojure_fn_5794(){
return (clojure.JS.def(clojure,"resultset_seq",(function __clojure_fn_5794_resultset_seq_5796(rs_1){
var rows_7,row_struct_5,keys_4,row_values_6,rsmeta_2,idxs_3;
return (((rsmeta_2=(rs_1).getMetaData()),
(idxs_3=clojure.range.apply(null,[1,clojure.lang.Numbers.inc((rsmeta_2).getColumnCount())])),
(keys_4=clojure.map.apply(null,[clojure.comp.apply(null,[clojure.keyword,(function __clojure_fn_5794_resultset_seq_5796_fn_5798(target__877_1){
return ((target__877_1).toLowerCase())})]),clojure.map.apply(null,[(function __clojure_fn_5794_resultset_seq_5796_fn_5801(i_1){
return ((rsmeta_2).getColumnName(i_1))}),idxs_3])])),
(row_struct_5=clojure.apply.apply(null,[clojure.create_struct,keys_4])),
(row_values_6=(function __clojure_fn_5794_resultset_seq_5796_row_values_5804(){
return (clojure.map.apply(null,[(function __clojure_fn_5794_resultset_seq_5796_row_values_5804_fn_5806(i_1){
return ((rs_1).getObject(i_1))}),idxs_3]))})),
(rows_7=(function __clojure_fn_5794_resultset_seq_5796_thisfn_5810(){
var thisfn_0=arguments.callee;
return ((((rs_1).next())?((new clojure.lang.LazyCons((function __clojure_fn_5794_resultset_seq_5796_thisfn_5810_fn_5812(G__5811_1){switch(arguments.length){
case 0:return (clojure.apply.apply(null,[clojure.struct,row_struct_5,row_values_6.apply(null,[])]))}
return (thisfn_0.apply(null,[]))})))):(null)))})),
rows_7.apply(null,[])))})))}).apply(null,[]);

//======
//(defn set "Returns a set of the distinct elements of coll." [coll] (apply hash-set coll))
//---
(function __clojure_fn_5825(){
return (clojure.JS.def(clojure,"set",(function __clojure_fn_5825_set_5827(coll_1){
return (clojure.apply.apply(null,[clojure.hash_set,coll_1]))})))}).apply(null,[]);
// Skipping: (defn class? "Returns true if x is an instance of Class" [x] (instance? Class x))

//======
//(defn filter-key [keyfn pred amap] (loop [ret {} es (seq amap)] (if es (if (pred (keyfn (first es))) (recur (assoc ret (key (first es)) (val (first es))) (rest es)) (recur ret (rest es))) ret)))
//---
(function __clojure_fn_5843(){
return (clojure.JS.def(clojure,"filter_key",(function __clojure_fn_5843_filter_key_5845(keyfn_1,pred_2,amap_3){
var es_5,ret_4;
return (((function __loop(){var _rtn,_cnt;(ret_4=clojure.lang.PersistentHashMap.EMPTY),
(es_5=clojure.seq.apply(null,[amap_3]));do{_cnt=0;
_rtn=((es_5)?(((pred_2.apply(null,[keyfn_1.apply(null,[clojure.first.apply(null,[es_5])])]))?((_cnt=1,_rtn=[clojure.assoc.apply(null,[ret_4,clojure.key.apply(null,[clojure.first.apply(null,[es_5])]),clojure.val.apply(null,[clojure.first.apply(null,[es_5])])]),clojure.rest.apply(null,[es_5])],ret_4=_rtn[0],es_5=_rtn[1])):((_cnt=1,_rtn=[ret_4,clojure.rest.apply(null,[es_5])],ret_4=_rtn[0],es_5=_rtn[1])))):(ret_4))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn find-ns "Returns the namespace named by the symbol or nil if it doesn't exist." [sym] (clojure.lang.Namespace/find sym))
//---
(function __clojure_fn_5855(){
return (clojure.JS.def(clojure,"find_ns",(function __clojure_fn_5855_find_ns_5857(sym_1){
return (clojure.lang.Namespace.find(sym_1))})))}).apply(null,[]);

//======
//(defn create-ns "Create a new namespace named by the symbol if one doesn't already\n  exist, returns it or the already-existing namespace of the same\n  name." [sym] (clojure.lang.Namespace/findOrCreate sym))
//---
(function __clojure_fn_5867(){
return (clojure.JS.def(clojure,"create_ns",(function __clojure_fn_5867_create_ns_5869(sym_1){
return (clojure.lang.Namespace.findOrCreate(sym_1))})))}).apply(null,[]);

//======
//(defn remove-ns "Removes the namespace named by the symbol. Use with caution.\n  Cannot be used to remove the clojure namespace." [sym] (clojure.lang.Namespace/remove sym))
//---
(function __clojure_fn_5879(){
return (clojure.JS.def(clojure,"remove_ns",(function __clojure_fn_5879_remove_ns_5881(sym_1){
return (clojure.lang.Namespace.remove(sym_1))})))}).apply(null,[]);

//======
//(defn all-ns "Returns a sequence of all namespaces." [] (clojure.lang.Namespace/all))
//---
(function __clojure_fn_5891(){
return (clojure.JS.def(clojure,"all_ns",(function __clojure_fn_5891_all_ns_5893(){
return (clojure.lang.Namespace.all())})))}).apply(null,[]);

//======
//(defn the-ns [x] (if (instance? clojure.lang.Namespace x) x (or (find-ns x) (throw (RT/makeException (str "No namespace: " x " found"))))))
//---
(function __clojure_fn_5903(){
return (clojure.JS.def(clojure,"the_ns",(function __clojure_fn_5903_the_ns_5905(x_1){
var or__202_2;
return (((clojure.instance_QMARK_.apply(null,[clojure.lang.Namespace,x_1]))?(x_1):(((or__202_2=clojure.find_ns.apply(null,[x_1])),
((or__202_2)?(or__202_2):((function __throw(){throw clojure.lang.RT.makeException(clojure.str.apply(null,["No namespace: ",x_1," found"]))})()))))))})))}).apply(null,[]);

//======
//(defn ns-name "Returns the name of the namespace, a symbol." [ns] (.getName (the-ns ns)))
//---
(function __clojure_fn_5915(){
return (clojure.JS.def(clojure,"ns_name",(function __clojure_fn_5915_ns_name_5917(ns_1){
return ((clojure.the_ns.apply(null,[ns_1])).getName())})))}).apply(null,[]);

//======
//(defn ns-map "Returns a map of all the mappings for the namespace." [ns] (.getMappings (the-ns ns)))
//---
(function __clojure_fn_5927(){
return (clojure.JS.def(clojure,"ns_map",(function __clojure_fn_5927_ns_map_5929(ns_1){
return ((clojure.the_ns.apply(null,[ns_1])).getMappings())})))}).apply(null,[]);

//======
//(defn ns-unmap "Removes the mappings for the symbol from the namespace." [ns sym] (.unmap (the-ns ns) sym))
//---
(function __clojure_fn_5939(){
return (clojure.JS.def(clojure,"ns_unmap",(function __clojure_fn_5939_ns_unmap_5941(ns_1,sym_2){
return ((clojure.the_ns.apply(null,[ns_1])).unmap(sym_2))})))}).apply(null,[]);

//======
//(defn ns-publics "Returns a map of the public intern mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (= ns (.ns v)) (.isPublic v))) (ns-map ns))))
//---
(function __clojure_fn_5954(){
return (clojure.JS.def(clojure,"ns_publics",(function __clojure_fn_5954_ns_publics_5956(ns_1){
var ns_2;
return (((ns_2=clojure.the_ns.apply(null,[ns_1])),
clojure.filter_key.apply(null,[clojure.val,(function __clojure_fn_5954_ns_publics_5956_fn_5958(v_1){
var and__196_2,and__196_3;
return (((and__196_2=clojure.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__196_2)?(((and__196_3=clojure.lang.Util.equal(ns_2,(v_1).ns)),
((and__196_3)?((v_1).isPublic()):(and__196_3)))):(and__196_2))))}),clojure.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn ns-imports "Returns a map of the import mappings for the namespace." [ns] (filter-key val class? (ns-map ns)))
//---
(function __clojure_fn_5969(){
return (clojure.JS.def(clojure,"ns_imports",(function __clojure_fn_5969_ns_imports_5971(ns_1){
return (clojure.filter_key.apply(null,[clojure.val,clojure.class_QMARK_,clojure.ns_map.apply(null,[ns_1])]))})))}).apply(null,[]);
// Skipping: (defn refer "refers to all public vars of ns, subject to filters.\n  filters can include at most one each of:\n\n  :exclude list-of-symbols\n  :only list-of-symbols\n  :rename map-of-fromsymbol-tosymbol\n\n  For each public interned var in the namespace named by the symbol,\n  adds a mapping from the name of the var to the var to the current\n  namespace.  Throws an exception if name is already mapped to\n  something else in the current namespace. Filters can be used to\n  select a subset, via inclusion or exclusion, or to provide a mapping\n  to a symbol different from the var's name, in order to prevent\n  clashes. Use :use in the ns macro in preference to calling this directly." [ns-sym & filters] (let [ns (or (find-ns ns-sym) (throw (RT/makeException (str "No namespace: " ns-sym)))) fs (apply hash-map filters) nspublics (ns-publics ns) rename (or (:rename fs) {}) exclude (set (:exclude fs)) to-do (or (:only fs) (keys nspublics))] (doseq sym to-do (when-not (exclude sym) (let [v (nspublics sym)] (when-not v (throw (new java.lang.IllegalAccessError (str sym " is not public")))) (. *ns* (refer (or (rename sym) sym) v)))))))

//======
//(defn ns-refers "Returns a map of the refer mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (not= ns (.ns v)))) (ns-map ns))))
//---
(function __clojure_fn_5990(){
return (clojure.JS.def(clojure,"ns_refers",(function __clojure_fn_5990_ns_refers_5992(ns_1){
var ns_2;
return (((ns_2=clojure.the_ns.apply(null,[ns_1])),
clojure.filter_key.apply(null,[clojure.val,(function __clojure_fn_5990_ns_refers_5992_fn_5994(v_1){
var and__196_2;
return (((and__196_2=clojure.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__196_2)?(clojure.not_EQ_.apply(null,[ns_2,(v_1).ns])):(and__196_2))))}),clojure.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn ns-interns "Returns a map of the intern mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (= ns (.ns v)))) (ns-map ns))))
//---
(function __clojure_fn_6008(){
return (clojure.JS.def(clojure,"ns_interns",(function __clojure_fn_6008_ns_interns_6010(ns_1){
var ns_2;
return (((ns_2=clojure.the_ns.apply(null,[ns_1])),
clojure.filter_key.apply(null,[clojure.val,(function __clojure_fn_6008_ns_interns_6010_fn_6012(v_1){
var and__196_2;
return (((and__196_2=clojure.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__196_2)?(clojure.lang.Util.equal(ns_2,(v_1).ns)):(and__196_2))))}),clojure.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn alias "Add an alias in the current namespace to another\n  namespace. Arguments are two symbols: the alias to be used, and\n  the symbolic name of the target namespace. Use :as in the ns macro in preference \n  to calling this directly." [alias namespace-sym] (.addAlias *ns* alias (find-ns namespace-sym)))
//---
(function __clojure_fn_6023(){
return (clojure.JS.def(clojure,"alias",(function __clojure_fn_6023_alias_6025(alias_1,namespace_sym_2){
return ((clojure._STAR_ns_STAR_).addAlias(alias_1,clojure.find_ns.apply(null,[namespace_sym_2])))})))}).apply(null,[]);

//======
//(defn ns-aliases "Returns a map of the aliases for the namespace." [ns] (.getAliases (the-ns ns)))
//---
(function __clojure_fn_6035(){
return (clojure.JS.def(clojure,"ns_aliases",(function __clojure_fn_6035_ns_aliases_6037(ns_1){
return ((clojure.the_ns.apply(null,[ns_1])).getAliases())})))}).apply(null,[]);

//======
//(defn ns-unalias "Removes the alias for the symbol from the namespace." [ns sym] (.removeAlias (the-ns ns) sym))
//---
(function __clojure_fn_6047(){
return (clojure.JS.def(clojure,"ns_unalias",(function __clojure_fn_6047_ns_unalias_6049(ns_1,sym_2){
return ((clojure.the_ns.apply(null,[ns_1])).removeAlias(sym_2))})))}).apply(null,[]);

//======
//(defn take-nth "Returns a lazy seq of every nth item in coll." [n coll] (when (seq coll) (lazy-cons (first coll) (take-nth n (drop n coll)))))
//---
(function __clojure_fn_6064(){
return (clojure.JS.def(clojure,"take_nth",(function __clojure_fn_6064_take_nth_6066(n_1,coll_2){
return (((clojure.seq.apply(null,[coll_2]))?((new clojure.lang.LazyCons((function __clojure_fn_6064_take_nth_6066_fn_6069(G__6068_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll_2]))}
return (clojure.take_nth.apply(null,[n_1,clojure.drop.apply(null,[n_1,coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn interleave "Returns a lazy seq of the first item in each coll, then the second\n  etc." [& colls] (apply concat (apply map list colls)))
//---
(function __clojure_fn_6081(){
return (clojure.JS.def(clojure,"interleave",clojure.JS.variadic(0,(function __clojure_fn_6081_interleave_6083(){
var colls_1,colls_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[clojure.concat,clojure.apply.apply(null,[clojure.map,clojure.list,colls_1])]))}))))}).apply(null,[]);

//======
//(defn var-get "Gets the value in the var object" [x] (. x (get)))
//---
(function __clojure_fn_6093(){
return (clojure.JS.def(clojure,"var_get",(function __clojure_fn_6093_var_get_6095(x_1){
return ((x_1).get())})))}).apply(null,[]);

//======
//(defn var-set "Sets the value in the var object to val. The var must be\n thread-locally bound." [x val] (. x (set val)))
//---
(function __clojure_fn_6105(){
return (clojure.JS.def(clojure,"var_set",(function __clojure_fn_6105_var_set_6107(x_1,val_2){
return ((x_1).set(val_2))})))}).apply(null,[]);
// Skipping: (defmacro with-local-vars "varbinding=> symbol init-expr\n\n  Executes the exprs in a context in which the symbols are bound to\n  vars with per-thread bindings to the init-exprs.  The symbols refer\n  to the var objects themselves, and must be accessed with var-get and\n  var-set" [name-vals-vec & body] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (interleave (take-nth 2 name-vals-vec) (repeat (quote (. clojure.lang.Var (create)))))))) (clojure/list (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Var)) (clojure/list (clojure/concat (clojure/list (quote clojure/pushThreadBindings)) (clojure/list (clojure/concat (clojure/list (quote clojure/hash-map)) name-vals-vec)))))) (clojure/list (clojure/concat (clojure/list (quote try)) body (clojure/list (clojure/concat (clojure/list (quote finally)) (clojure/list (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Var)) (clojure/list (clojure/concat (clojure/list (quote clojure/popThreadBindings))))))))))))
// Skipping: (defn ns-resolve "Returns the var or Class to which a symbol will be resolved in the\n  namespace, else nil.  Note that if the symbol is fully qualified,\n  the var/Class to which it resolves need not be present in the\n  namespace." [ns sym] (clojure.lang.Compiler/maybeResolveIn (the-ns ns) sym))
// Skipping: (defn resolve "same as (ns-resolve *ns* symbol)" [sym] (ns-resolve *ns* sym))

//======
//(defn array-map "Constructs an array-map." ([] (. clojure.lang.PersistentArrayMap EMPTY)) ([& keyvals] (new clojure.lang.PersistentArrayMap (to-array keyvals))))
//---
(function __clojure_fn_6136(){
return (clojure.JS.def(clojure,"array_map",clojure.JS.variadic(0,(function __clojure_fn_6136_array_map_6138(){switch(arguments.length){
case 0:return (clojure.lang.PersistentArrayMap.EMPTY)}
var keyvals_1,keyvals_1=clojure.JS.rest_args(this,arguments,0);
return ((new clojure.lang.PersistentArrayMap(clojure.to_array.apply(null,[keyvals_1]))))}))))}).apply(null,[]);

//======
//(defn nthrest "Returns the nth rest of coll, (seq coll) when n is 0." [coll n] (loop [n n xs (seq coll)] (if (and xs (pos? n)) (recur (dec n) (rest xs)) xs)))
//---
(function __clojure_fn_6149(){
return (clojure.JS.def(clojure,"nthrest",(function __clojure_fn_6149_nthrest_6151(coll_1,n_2){
var xs_4,n_3,and__196_5;
return (((function __loop(){var _rtn,_cnt;(n_3=n_2),
(xs_4=clojure.seq.apply(null,[coll_1]));do{_cnt=0;
_rtn=((((and__196_5=xs_4),
((and__196_5)?(clojure.lang.Numbers.isPos(n_3)):(and__196_5))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_3),clojure.rest.apply(null,[xs_4])],n_3=_rtn[0],xs_4=_rtn[1])):(xs_4))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn symbol? "Return true if x is a Symbol" [x] (instance? clojure.lang.Symbol x))
//---
(function __clojure_fn_6161(){
return (clojure.JS.def(clojure,"symbol_QMARK_",(function __clojure_fn_6161_symbol_QMARK_6163(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Symbol,x_1]))})))}).apply(null,[]);

//======
//(defn keyword? "Return true if x is a Keyword" [x] (instance? clojure.lang.Keyword x))
//---
(function __clojure_fn_6173(){
return (clojure.JS.def(clojure,"keyword_QMARK_",(function __clojure_fn_6173_keyword_QMARK_6175(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Keyword,x_1]))})))}).apply(null,[]);

//======
//(defn destructure [bindings] (let [bmap (apply array-map bindings) pb (fn pb [bvec b v] (let [pvec (fn [bvec b val] (let [gvec (gensym "vec__")] (loop [ret (-> bvec (conj gvec) (conj val)) n 0 bs b seen-rest? false] (if bs (let [firstb (first bs)] (cond (= firstb (quote &)) (recur (pb ret (second bs) (list (quote clojure/nthrest) gvec n)) n (rrest bs) true) (= firstb :as) (pb ret (second bs) gvec) :else (if seen-rest? (throw (RT/makeException "Unsupported binding form, only :as can follow & parameter")) (recur (pb ret firstb (list (quote clojure/nth) gvec n nil)) (inc n) (rest bs) seen-rest?)))) ret)))) pmap (fn [bvec b v] (let [gmap (or (:as b) (gensym "map__")) defaults (:or b)] (loop [ret (-> bvec (conj gmap) (conj v)) bes (reduce (fn [bes entry] (reduce (fn* [p1__6179 p2__6180] (assoc p1__6179 p2__6180 ((val entry) p2__6180))) (dissoc bes (key entry)) ((key entry) bes))) (dissoc b :as :or) {:keys (fn* [p1__6181] (keyword (str p1__6181))), :syms (fn* [p1__6182] (list (quote quote) p1__6182)), :strs str})] (if bes (let [bb (key (first bes)) bk (val (first bes)) has-default (contains? defaults bb)] (recur (pb ret bb (if has-default (list (quote clojure/get) gmap bk (defaults bb)) (list (quote clojure/get) gmap bk))) (rest bes))) ret))))] (cond (symbol? b) (-> bvec (conj b) (conj v)) (vector? b) (pvec bvec b v) (map? b) (pmap bvec b v) :else (throw (RT/makeException (str "Unsupported binding form: " b)))))) process-entry (fn [bvec b] (pb bvec (key b) (val b)))] (if (every? symbol? (keys bmap)) bindings (reduce process-entry [] bmap))))
//---
(function __clojure_fn_6212(){
return (clojure.JS.def(clojure,"destructure",(function __clojure_fn_6212_destructure_6214(bindings_1){
var bmap_2,pb_3,process_entry_4;
return (((bmap_2=clojure.apply.apply(null,[clojure.array_map,bindings_1])),
(pb_3=(function __clojure_fn_6212_destructure_6214_pb_6216(bvec_1,b_2,v_3){
var pmap_5,pvec_4,pb_0=arguments.callee;
return (((pvec_4=(function __clojure_fn_6212_destructure_6214_pb_6216_pvec_6217(bvec_1,b_2,val_3){
var gvec_4,firstb_9,n_6,bs_7,ret_5,seen_rest_QMARK__8;
return (((gvec_4=clojure.gensym.apply(null,["vec__"])),
((function __loop(){var _rtn,_cnt;(ret_5=clojure.conj.apply(null,[clojure.conj.apply(null,[bvec_1,gvec_4]),val_3])),
(n_6=0),
(bs_7=b_2),
(seen_rest_QMARK__8=false);do{_cnt=0;
_rtn=((bs_7)?(((firstb_9=clojure.first.apply(null,[bs_7])),
((clojure.lang.Util.equal(firstb_9,"'&"))?((_cnt=1,_rtn=[pb_0.apply(null,[ret_5,clojure.second.apply(null,[bs_7]),clojure.list.apply(null,["'clojure/nthrest",gvec_4,n_6])]),n_6,clojure.rrest.apply(null,[bs_7]),true],ret_5=_rtn[0],n_6=_rtn[1],bs_7=_rtn[2],seen_rest_QMARK__8=_rtn[3])):(((clojure.lang.Util.equal(firstb_9,clojure.keyword("","as")))?(pb_0.apply(null,[ret_5,clojure.second.apply(null,[bs_7]),gvec_4])):(((clojure.keyword("","else"))?(((seen_rest_QMARK__8)?((function __throw(){throw clojure.lang.RT.makeException("Unsupported binding form, only :as can follow & parameter")})()):((_cnt=1,_rtn=[pb_0.apply(null,[ret_5,firstb_9,clojure.list.apply(null,["'clojure/nth",gvec_4,n_6,null])]),clojure.lang.Numbers.inc(n_6),clojure.rest.apply(null,[bs_7]),seen_rest_QMARK__8],ret_5=_rtn[0],n_6=_rtn[1],bs_7=_rtn[2],seen_rest_QMARK__8=_rtn[3])))):(null)))))))):(ret_5))}while(_cnt);return _rtn;})())))})),
(pmap_5=(function __clojure_fn_6212_destructure_6214_pb_6216_pmap_6220(bvec_1,b_2,v_3){
var has_default_10,ret_6,gmap_4,bb_8,or__202_4,defaults_5,bes_7,bk_9;
return (((gmap_4=((or__202_4=clojure.keyword("","as").apply(null,[b_2])),
((or__202_4)?(or__202_4):(clojure.gensym.apply(null,["map__"]))))),
(defaults_5=clojure.keyword("","or").apply(null,[b_2])),
((function __loop(){var _rtn,_cnt;(ret_6=clojure.conj.apply(null,[clojure.conj.apply(null,[bvec_1,gmap_4]),v_3])),
(bes_7=clojure.reduce.apply(null,[(function __clojure_fn_6212_destructure_6214_pb_6216_pmap_6220_fn_6222(bes_1,entry_2){
return (clojure.reduce.apply(null,[(function __clojure_fn_6212_destructure_6214_pb_6216_pmap_6220_fn_6222_fn_6224(p1__6179_1,p2__6180_2){
return (clojure.assoc.apply(null,[p1__6179_1,p2__6180_2,clojure.val.apply(null,[entry_2]).apply(null,[p2__6180_2])]))}),clojure.dissoc.apply(null,[bes_1,clojure.key.apply(null,[entry_2])]),clojure.key.apply(null,[entry_2]).apply(null,[bes_1])]))}),clojure.dissoc.apply(null,[b_2,clojure.keyword("","as"),clojure.keyword("","or")]),clojure.hash_map(clojure.keyword("","keys"),(function __clojure_fn_6212_destructure_6214_pb_6216_pmap_6220_fn_6228(p1__6181_1){
return (clojure.keyword.apply(null,[clojure.str.apply(null,[p1__6181_1])]))}),clojure.keyword("","syms"),(function __clojure_fn_6212_destructure_6214_pb_6216_pmap_6220_fn_6231(p1__6182_1){
return (clojure.list.apply(null,["'quote",p1__6182_1]))}),clojure.keyword("","strs"),clojure.str)]));do{_cnt=0;
_rtn=((bes_7)?(((bb_8=clojure.key.apply(null,[clojure.first.apply(null,[bes_7])])),
(bk_9=clojure.val.apply(null,[clojure.first.apply(null,[bes_7])])),
(has_default_10=clojure.contains_QMARK_.apply(null,[defaults_5,bb_8])),
(_cnt=1,_rtn=[pb_0.apply(null,[ret_6,bb_8,((has_default_10)?(clojure.list.apply(null,["'clojure/get",gmap_4,bk_9,defaults_5.apply(null,[bb_8])])):(clojure.list.apply(null,["'clojure/get",gmap_4,bk_9])))]),clojure.rest.apply(null,[bes_7])],ret_6=_rtn[0],bes_7=_rtn[1]))):(ret_6))}while(_cnt);return _rtn;})())))})),
((clojure.symbol_QMARK_.apply(null,[b_2]))?(clojure.conj.apply(null,[clojure.conj.apply(null,[bvec_1,b_2]),v_3])):(((clojure.vector_QMARK_.apply(null,[b_2]))?(pvec_4.apply(null,[bvec_1,b_2,v_3])):(((clojure.map_QMARK_.apply(null,[b_2]))?(pmap_5.apply(null,[bvec_1,b_2,v_3])):(((clojure.keyword("","else"))?((function __throw(){throw clojure.lang.RT.makeException(clojure.str.apply(null,["Unsupported binding form: ",b_2]))})()):(null))))))))))})),
(process_entry_4=(function __clojure_fn_6212_destructure_6214_process_entry_6236(bvec_1,b_2){
return (pb_3.apply(null,[bvec_1,clojure.key.apply(null,[b_2]),clojure.val.apply(null,[b_2])]))})),
((clojure.every_QMARK_.apply(null,[clojure.symbol_QMARK_,clojure.keys.apply(null,[bmap_2])]))?(bindings_1):(clojure.reduce.apply(null,[process_entry_4,clojure.lang.PersistentVector.EMPTY,bmap_2])))))})))}).apply(null,[]);
// Skipping: (defmacro let "Evaluates the exprs in a lexical context in which the symbols in\n  the binding-forms are bound to their respective init-exprs or parts\n  therein." [bindings & body] (when (odd? (count bindings)) (throw (Exception. "Odd number of elements in let bindings"))) (clojure/concat (clojure/list (quote let*)) (clojure/list (destructure bindings)) body))
// Skipping: (defmacro fn "(fn name? [params* ] exprs*)\n  (fn name? ([params* ] exprs*)+)\n\n  params => positional-params* , or positional-params* & rest-param\n  positional-param => binding-form\n  rest-param => binding-form\n  name => symbol\n\n  Defines a function" [& sigs] (let [name (if (symbol? (first sigs)) (first sigs) nil) sigs (if name (rest sigs) sigs) sigs (if (vector? (first sigs)) (list sigs) sigs) psig (fn [sig] (let [[params & body] sig] (if (every? symbol? params) sig (loop [params params new-params [] lets []] (if params (if (symbol? (first params)) (recur (rest params) (conj new-params (first params)) lets) (let [gparam (gensym "p__")] (recur (rest params) (conj new-params gparam) (-> lets (conj (first params)) (conj gparam))))) (clojure/concat (clojure/list new-params) (clojure/list (clojure/concat (clojure/list (quote clojure/let)) (clojure/list lets) body)))))))) new-sigs (map psig sigs)] (with-meta (if name (list* (quote fn*) name new-sigs) (cons (quote fn*) new-sigs)) *macro-meta*)))
// Skipping: (defmacro loop "Evaluates the exprs in a lexical context in which the symbols in\n  the binding-forms are bound to their respective init-exprs or parts\n  therein. Acts as a recur target." [bindings & body] (let [db (destructure bindings)] (if (= db bindings) (clojure/concat (clojure/list (quote loop*)) (clojure/list bindings) body) (let [vs (take-nth 2 (drop 1 bindings)) bs (take-nth 2 bindings) gs (map (fn [b] (if (symbol? b) b (gensym))) bs) bfs (reduce (fn [ret [b v g]] (if (symbol? b) (conj ret g v) (conj ret g v b g))) [] (map vector bs vs gs))] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list bfs) (clojure/list (clojure/concat (clojure/list (quote loop*)) (clojure/list (vec (interleave gs gs))) (clojure/list (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (vec (interleave bs gs))) body)))))))))
// Skipping: (defmacro when-first "Same as (when (seq xs) (let [x (first xs)] body))" [x xs & body] (clojure/concat (clojure/list (quote clojure/when)) (clojure/list (clojure/concat (clojure/list (quote clojure/seq)) (clojure/list xs))) (clojure/list (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list x) (clojure/list (clojure/concat (clojure/list (quote clojure/first)) (clojure/list xs)))))) body))))
// Skipping: (defmacro lazy-cat "Expands to code which yields a lazy sequence of the concatenation\n  of the supplied colls.  Each coll expr is not evaluated until it is\n  needed." ([coll] (clojure/concat (clojure/list (quote clojure/seq)) (clojure/list coll))) ([coll & colls] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote iter__6277)) (clojure/list (clojure/concat (clojure/list (quote clojure/fn)) (clojure/list (quote iter__6277)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote coll__6278))))) (clojure/list (clojure/concat (clojure/list (quote if)) (clojure/list (clojure/concat (clojure/list (quote clojure/seq)) (clojure/list (quote coll__6278)))) (clojure/list (clojure/concat (clojure/list (quote clojure/lazy-cons)) (clojure/list (clojure/concat (clojure/list (quote clojure/first)) (clojure/list (quote coll__6278)))) (clojure/list (clojure/concat (clojure/list (quote iter__6277)) (clojure/list (clojure/concat (clojure/list (quote clojure/rest)) (clojure/list (quote coll__6278)))))))) (clojure/list (clojure/concat (clojure/list (quote clojure/lazy-cat)) colls))))))))) (clojure/list (clojure/concat (clojure/list (quote iter__6277)) (clojure/list coll))))))
// Skipping: (defmacro for "List comprehension. Takes a vector of one or more\n binding-form/collection-expr pairs, each followed by an optional filtering\n :when/:while expression (:when test or :while test), and yields a\n lazy sequence of evaluations of expr. Collections are iterated in a\n nested fashion, rightmost fastest, and nested coll-exprs can refer to\n bindings created in prior binding-forms.\n\n (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)]  [x y]))" ([seq-exprs expr] (let [pargs (fn [xs] (loop [ret [] [b e & [w f & wr :as r] :as xs] (seq xs)] (if xs (cond (= w :when) (recur (conj ret {:e e, :w :when, :b b, :f f}) wr) (= w :while) (recur (conj ret {:e e, :w :while, :b b, :f f}) wr) :else (recur (conj ret {:e e, :w :while, :b b, :f true}) r)) (seq ret)))) emit (fn emit [[{w :w, b :b, f :f} & [{ys :e} :as rses]]] (let [giter (gensym "iter__") gxs (gensym "s__")] (clojure/concat (clojure/list (quote clojure/fn)) (clojure/list giter) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list gxs)))) (clojure/list (clojure/concat (clojure/list (quote clojure/when-first)) (clojure/list b) (clojure/list gxs) (clojure/list (clojure/concat (clojure/list (quote if)) (clojure/list f) (clojure/list (if rses (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote iterys__6286)) (clojure/list (emit rses)) (clojure/list (quote fs__6287)) (clojure/list (clojure/concat (clojure/list (quote iterys__6286)) (clojure/list ys)))))) (clojure/list (clojure/concat (clojure/list (quote if)) (clojure/list (quote fs__6287)) (clojure/list (clojure/concat (clojure/list (quote clojure/lazy-cat)) (clojure/list (quote fs__6287)) (clojure/list (clojure/concat (clojure/list giter) (clojure/list (clojure/concat (clojure/list (quote clojure/rest)) (clojure/list gxs))))))) (clojure/list (clojure/concat (clojure/list (quote recur)) (clojure/list (clojure/concat (clojure/list (quote clojure/rest)) (clojure/list gxs)))))))) (clojure/concat (clojure/list (quote clojure/lazy-cons)) (clojure/list expr) (clojure/list (clojure/concat (clojure/list giter) (clojure/list (clojure/concat (clojure/list (quote clojure/rest)) (clojure/list gxs)))))))) (clojure/list (if (= w :when) (clojure/concat (clojure/list (quote recur)) (clojure/list (clojure/concat (clojure/list (quote clojure/rest)) (clojure/list gxs)))) nil)))))))))] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote iter__6288)) (clojure/list (emit (pargs seq-exprs)))))) (clojure/list (clojure/concat (clojure/list (quote iter__6288)) (clojure/list (second seq-exprs))))))))
// Skipping: (defmacro comment "Ignores body, yields nil" [& body])
// Skipping: (defmacro with-out-str "Evaluates exprs in a context in which *out* is bound to a fresh\n  StringWriter.  Returns the string created by any nested printing\n  calls." [& body] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote s__6318)) (clojure/list (clojure/concat (clojure/list (quote RT/makeStringWriter))))))) (clojure/list (clojure/concat (clojure/list (quote clojure/binding)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote clojure/*out*)) (clojure/list (quote s__6318))))) body (clojure/list (clojure/concat (clojure/list (quote clojure/str)) (clojure/list (quote s__6318))))))))
// Skipping: (defmacro with-in-str "Evaluates body in a context in which *in* is bound to a fresh\n  StringReader initialized with the string s." [s & body] (clojure/concat (clojure/list (quote clojure/with-open)) (clojure/list (quote s__6325)) (clojure/list (clojure/concat (clojure/list (quote clojure/->)) (clojure/list (clojure/concat (clojure/list (quote java.io.StringReader.)) (clojure/list s))) (clojure/list (quote clojure.lang.LineNumberingPushbackReader.)))) (clojure/list (clojure/concat (clojure/list (quote clojure/binding)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote clojure/*in*)) (clojure/list (quote s__6325))))) body))))

//======
//(defn pr-str "pr to a string, returning it" {:tag String} [& xs] (with-out-str (apply pr xs)))
//---
(function __clojure_fn_6338(){
return (clojure.JS.def(clojure,"pr_str",clojure.JS.variadic(0,(function __clojure_fn_6338_pr_str_6340(){
var s__1254_2,xs_1,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1254_2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_out_STAR_,s__1254_2])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.pr,xs_1]),
clojure.str.apply(null,[s__1254_2]))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn prn-str "prn to a string, returning it" {:tag String} [& xs] (with-out-str (apply prn xs)))
//---
(function __clojure_fn_6350(){
return (clojure.JS.def(clojure,"prn_str",clojure.JS.variadic(0,(function __clojure_fn_6350_prn_str_6352(){
var s__1254_2,xs_1,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1254_2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_out_STAR_,s__1254_2])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.prn,xs_1]),
clojure.str.apply(null,[s__1254_2]))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn print-str "print to a string, returning it" {:tag String} [& xs] (with-out-str (apply print xs)))
//---
(function __clojure_fn_6362(){
return (clojure.JS.def(clojure,"print_str",clojure.JS.variadic(0,(function __clojure_fn_6362_print_str_6364(){
var s__1254_2,xs_1,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1254_2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_out_STAR_,s__1254_2])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.print,xs_1]),
clojure.str.apply(null,[s__1254_2]))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn println-str "println to a string, returning it" {:tag String} [& xs] (with-out-str (apply println xs)))
//---
(function __clojure_fn_6374(){
return (clojure.JS.def(clojure,"println_str",clojure.JS.variadic(0,(function __clojure_fn_6374_println_str_6376(){
var xs_1,s__1254_2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1254_2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_out_STAR_,s__1254_2])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.println,xs_1]),
clojure.str.apply(null,[s__1254_2]))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);
// Skipping: (defmacro assert "Evaluates expr and throws an exception if it does not evaluate to\n logical true." [x] (clojure/concat (clojure/list (quote clojure/when-not)) (clojure/list x) (clojure/list (clojure/concat (clojure/list (quote throw)) (clojure/list (clojure/concat (clojure/list (quote RT/makeException)) (clojure/list (clojure/concat (clojure/list (quote clojure/str)) (clojure/list "Assert failed: ") (clojure/list (clojure/concat (clojure/list (quote clojure/pr-str)) (clojure/list (clojure/concat (clojure/list (quote quote)) (clojure/list x)))))))))))))

//======
//(defn test "test [v] finds fn at key :test in var metadata and calls it,\n  presuming failure will throw exception" [v] (let [f (:test (clojure/meta v))] (if f (do (f) :ok) :no-test)))
//---
(function __clojure_fn_6392(){
return (clojure.JS.def(clojure,"test",(function __clojure_fn_6392_test_6394(v_1){
var f_2;
return (((f_2=clojure.keyword("","test").apply(null,[clojure.meta.apply(null,[v_1])])),
((f_2)?(f_2.apply(null,[]),
clojure.keyword("","ok")):(clojure.keyword("","no-test")))))})))}).apply(null,[]);
// Skipping: (defn re-pattern "Returns an instance of java.util.regex.Pattern, for use, e.g. in\n  re-matcher." {:tag java.util.regex.Pattern} [s] (. java.util.regex.Pattern (compile s)))
// Skipping: (defn re-matcher "Returns an instance of java.util.regex.Matcher, for use, e.g. in\n  re-find." {:tag java.util.regex.Matcher} [re s] (. re (matcher s)))
// Skipping: (defn re-groups "Returns the groups from the most recent match/find. If there are no\n  nested groups, returns a string of the entire match. If there are\n  nested groups, returns a vector of the groups, the first element\n  being the entire match." [m] (let [gc (. m (groupCount))] (if (zero? gc) (. m (group)) (loop [ret [] c 0] (if (<= c gc) (recur (conj ret (. m (group c))) (inc c)) ret)))))
// Skipping: (defn re-seq "Returns a lazy sequence of successive matches of pattern in string,\n  using java.util.regex.Matcher.find(), each such match processed with\n  re-groups." [re s] (let [m (re-matcher re s)] ((fn step [] (when (. m (find)) (lazy-cons (re-groups m) (step)))))))
// Skipping: (defn re-matches "Returns the match, if any, of string to pattern, using\n  java.util.regex.Matcher.matches().  Uses re-groups to return the\n  groups." [re s] (let [m (re-matcher re s)] (when (. m (matches)) (re-groups m))))
// Skipping: (defn re-find "Returns the next regex match, if any, of string to pattern, using\n  java.util.regex.Matcher.find().  Uses re-groups to return the\n  groups." ([m] (when (. m (find)) (re-groups m))) ([re s] (let [m (re-matcher re s)] (re-find m))))

//======
//(defn rand "Returns a random floating point number between 0 (inclusive) and\n  1 (exclusive)." ([] (RT/random)) ([n] (* n (rand))))
//---
(function __clojure_fn_6449(){
return (clojure.JS.def(clojure,"rand",(function __clojure_fn_6449_rand_6451(n_1){switch(arguments.length){
case 0:return (clojure.lang.RT.random())}
return (clojure.lang.Numbers.multiply(n_1,clojure.rand.apply(null,[])))})))}).apply(null,[]);

//======
//(defn rand-int "Returns a random integer between 0 (inclusive) and n (exclusive)." [n] (int (rand n)))
//---
(function __clojure_fn_6462(){
return (clojure.JS.def(clojure,"rand_int",(function __clojure_fn_6462_rand_int_6464(n_1){
return (clojure.lang.RT.intCast(clojure.rand.apply(null,[n_1])))})))}).apply(null,[]);
// Skipping: (defmacro defn- "same as defn, yielding non-public def" [name & decls] (list* (quote clojure/defn) (with-meta name (assoc (meta name) :private true)) decls))

//======
//(defn print-doc [v] (println "-------------------------") (println (str (ns-name (:ns (clojure/meta v))) "/" (:name (clojure/meta v)))) (prn (:arglists (clojure/meta v))) (when (:macro (clojure/meta v)) (println "Macro")) (println " " (:doc (clojure/meta v))))
//---
(function __clojure_fn_6480(){
return (clojure.JS.def(clojure,"print_doc",(function __clojure_fn_6480_print_doc_6482(v_1){
return (clojure.println.apply(null,["-------------------------"]),
clojure.println.apply(null,[clojure.str.apply(null,[clojure.ns_name.apply(null,[clojure.keyword("","ns").apply(null,[clojure.meta.apply(null,[v_1])])]),"/",clojure.keyword("","name").apply(null,[clojure.meta.apply(null,[v_1])])])]),
clojure.prn.apply(null,[clojure.keyword("","arglists").apply(null,[clojure.meta.apply(null,[v_1])])]),
((clojure.keyword("","macro").apply(null,[clojure.meta.apply(null,[v_1])]))?(clojure.println.apply(null,["Macro"])):(null)),
clojure.println.apply(null,[" ",clojure.keyword("","doc").apply(null,[clojure.meta.apply(null,[v_1])])]))})))}).apply(null,[]);

//======
//(defn find-doc "Prints documentation for any var whose documentation or name\n contains a match for re-string" [re-string] (let [re (re-pattern re-string)] (dorun (for [ns (all-ns) v (sort-by (comp :name meta) (vals (ns-interns ns))) :when (and (:doc (clojure/meta v)) (or (re-find (re-matcher re (:doc (clojure/meta v)))) (re-find (re-matcher re (str (:name (clojure/meta v)))))))] (print-doc v)))))
//---
(function __clojure_fn_6512(){
return (clojure.JS.def(clojure,"find_doc",(function __clojure_fn_6512_find_doc_6514(re_string_1){
var iter__1230_3,re_2;
return (((re_2=clojure.re_pattern.apply(null,[re_string_1])),
clojure.dorun.apply(null,[((iter__1230_3=(function __clojure_fn_6512_find_doc_6514_iter_6516_6520(s__6517_1){
var _cnt,_rtn,iterys__1228_3,iter__1222_5,ns_2,fs__1229_4,iter__6516_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[s__6517_1]))?(((ns_2=clojure.first.apply(null,[s__6517_1])),
((true)?(((iterys__1228_3=(function __clojure_fn_6512_find_doc_6514_iter_6516_6520_iter_6518_6521(s__6519_1){
var _cnt,_rtn,and__196_3,v_2,or__202_4,iter__6518_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[s__6519_1]))?(((v_2=clojure.first.apply(null,[s__6519_1])),
((((and__196_3=clojure.keyword("","doc").apply(null,[clojure.meta.apply(null,[v_2])])),
((and__196_3)?(((or__202_4=clojure.re_find.apply(null,[clojure.re_matcher.apply(null,[re_2,clojure.keyword("","doc").apply(null,[clojure.meta.apply(null,[v_2])])])])),
((or__202_4)?(or__202_4):(clojure.re_find.apply(null,[clojure.re_matcher.apply(null,[re_2,clojure.str.apply(null,[clojure.keyword("","name").apply(null,[clojure.meta.apply(null,[v_2])])])])]))))):(and__196_3))))?((new clojure.lang.LazyCons((function __clojure_fn_6512_find_doc_6514_iter_6516_6520_iter_6518_6521_fn_6523(G__6522_1){switch(arguments.length){
case 0:return (clojure.print_doc.apply(null,[v_2]))}
return (iter__6518_0.apply(null,[clojure.rest.apply(null,[s__6519_1])]))})))):((_cnt=1,_rtn=[clojure.rest.apply(null,[s__6519_1])],s__6519_1=_rtn[0]))))):(null))
}while(_cnt);return _rtn;})),
(fs__1229_4=iterys__1228_3.apply(null,[clojure.sort_by.apply(null,[clojure.comp.apply(null,[clojure.keyword("","name"),clojure.meta]),clojure.vals.apply(null,[clojure.ns_interns.apply(null,[ns_2])])])])),
((fs__1229_4)?(((iter__1222_5=(function __clojure_fn_6512_find_doc_6514_iter_6516_6520_iter_1222_6528(coll__1223_1){
var iter__1222_0=arguments.callee;
return (((clojure.seq.apply(null,[coll__1223_1]))?((new clojure.lang.LazyCons((function __clojure_fn_6512_find_doc_6514_iter_6516_6520_iter_1222_6528_fn_6530(G__6529_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll__1223_1]))}
return (iter__1222_0.apply(null,[clojure.rest.apply(null,[coll__1223_1])]))})))):(clojure.seq.apply(null,[iter__6516_0.apply(null,[clojure.rest.apply(null,[s__6517_1])])]))))})),
iter__1222_5.apply(null,[fs__1229_4]))):((_cnt=1,_rtn=[clojure.rest.apply(null,[s__6517_1])],s__6517_1=_rtn[0]))))):(null)))):(null))
}while(_cnt);return _rtn;})),
iter__1230_3.apply(null,[clojure.all_ns.apply(null,[])]))])))})))}).apply(null,[]);

//======
//(defn special-form-anchor "Returns the anchor tag on http://clojure.org/special_forms for the\n  special form x, or nil" [x] (#{(quote recur) (quote .) (quote var) (quote let) (quote quote) (quote set!) (quote monitor-enter) (quote loop) (quote new) (quote fn) (quote if) (quote try) (quote def) (quote monitor-exit) (quote throw) (quote do)} x))
//---
(function __clojure_fn_6544(){
return (clojure.JS.def(clojure,"special_form_anchor",(function __clojure_fn_6544_special_form_anchor_6546(x_1){
return (clojure.hash_set("'recur","'.","'var","'let","'quote","'set!","'monitor-enter","'loop","'new","'fn","'if","'try","'def","'monitor-exit","'throw","'do").apply(null,[x_1]))})))}).apply(null,[]);

//======
//(defn syntax-symbol-anchor "Returns the anchor tag on http://clojure.org/special_forms for the\n  special form that uses syntax symbol x, or nil" [x] ({(quote &) (quote fn), (quote catch) (quote try), (quote finally) (quote try)} x))
//---
(function __clojure_fn_6556(){
return (clojure.JS.def(clojure,"syntax_symbol_anchor",(function __clojure_fn_6556_syntax_symbol_anchor_6558(x_1){
return (clojure.hash_map("'&","'fn","'catch","'try","'finally","'try").apply(null,[x_1]))})))}).apply(null,[]);

//======
//(defn print-special-doc [name type anchor] (println "-------------------------") (println name) (println type) (println (str "  Please see http://clojure.org/special_forms#" anchor)))
//---
(function __clojure_fn_6568(){
return (clojure.JS.def(clojure,"print_special_doc",(function __clojure_fn_6568_print_special_doc_6570(name_1,type_2,anchor_3){
return (clojure.println.apply(null,["-------------------------"]),
clojure.println.apply(null,[name_1]),
clojure.println.apply(null,[type_2]),
clojure.println.apply(null,[clojure.str.apply(null,["  Please see http://clojure.org/special_forms#",anchor_3])]))})))}).apply(null,[]);
// Skipping: (defmacro doc "Prints documentation for a var or special form given its name" [name] (cond (special-form-anchor name) (clojure/concat (clojure/list (quote clojure/print-special-doc)) (clojure/list (clojure/concat (clojure/list (quote quote)) (clojure/list name))) (clojure/list "Special Form") (clojure/list (clojure/concat (clojure/list (quote clojure/special-form-anchor)) (clojure/list (clojure/concat (clojure/list (quote quote)) (clojure/list name)))))) (syntax-symbol-anchor name) (clojure/concat (clojure/list (quote clojure/print-special-doc)) (clojure/list (clojure/concat (clojure/list (quote quote)) (clojure/list name))) (clojure/list "Syntax Symbol") (clojure/list (clojure/concat (clojure/list (quote clojure/syntax-symbol-anchor)) (clojure/list (clojure/concat (clojure/list (quote quote)) (clojure/list name)))))) :else (clojure/concat (clojure/list (quote clojure/print-doc)) (clojure/list (clojure/concat (clojure/list (quote var)) (clojure/list name))))))

//======
//(defn tree-seq "returns a lazy sequence of the nodes in a tree, via a depth-first walk.\n  branch? must be a fn of one arg that returns true if passed a node\n  that can have children (but may not).  children must be a fn of one\n  arg that returns a sequence of the children. Will only be called on\n  nodes for which branch? returns true. Root is the root node of the\n  tree, must be a branch." [branch? children root] (let [walk (fn walk [nodes] (when-first node nodes (lazy-cons node (if (branch? node) (lazy-cat (walk (children node)) (walk (rest nodes))) (walk (rest nodes))))))] (lazy-cons root (walk (children root)))))
//---
(function __clojure_fn_6605(){
return (clojure.JS.def(clojure,"tree_seq",(function __clojure_fn_6605_tree_seq_6607(branch_QMARK__1,children_2,root_3){
var walk_4;
return (((walk_4=(function __clojure_fn_6605_tree_seq_6607_walk_6609(nodes_1){
var node_2,walk_0=arguments.callee;
return (((clojure.seq.apply(null,[nodes_1]))?(((node_2=clojure.first.apply(null,[nodes_1])),
(new clojure.lang.LazyCons((function __clojure_fn_6605_tree_seq_6607_walk_6609_fn_6611(G__6610_1){switch(arguments.length){
case 0:return (node_2)}
var iter__1222_2;
return (((branch_QMARK__1.apply(null,[node_2]))?(((iter__1222_2=(function __clojure_fn_6605_tree_seq_6607_walk_6609_fn_6611_iter_1222_6614(coll__1223_1){
var iter__1222_0=arguments.callee;
return (((clojure.seq.apply(null,[coll__1223_1]))?((new clojure.lang.LazyCons((function __clojure_fn_6605_tree_seq_6607_walk_6609_fn_6611_iter_1222_6614_fn_6616(G__6615_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll__1223_1]))}
return (iter__1222_0.apply(null,[clojure.rest.apply(null,[coll__1223_1])]))})))):(clojure.seq.apply(null,[walk_0.apply(null,[clojure.rest.apply(null,[nodes_1])])]))))})),
iter__1222_2.apply(null,[walk_0.apply(null,[children_2.apply(null,[node_2])])]))):(walk_0.apply(null,[clojure.rest.apply(null,[nodes_1])]))))}))))):(null)))})),
(new clojure.lang.LazyCons((function __clojure_fn_6605_tree_seq_6607_fn_6624(G__6623_1){switch(arguments.length){
case 0:return (root_3)}
return (walk_4.apply(null,[children_2.apply(null,[root_3])]))})))))})))}).apply(null,[]);

//======
//(defn file-seq "A tree seq on java.io.Files" [dir] (tree-seq (fn [f] (. f (isDirectory))) (fn [d] (seq (. d (listFiles)))) dir))
//---
(function __clojure_fn_6642(){
return (clojure.JS.def(clojure,"file_seq",(function __clojure_fn_6642_file_seq_6644(dir_1){
return (clojure.tree_seq.apply(null,[(function __clojure_fn_6642_file_seq_6644_fn_6646(f_1){
return ((f_1).isDirectory())}),(function __clojure_fn_6642_file_seq_6644_fn_6649(d_1){
return (clojure.seq.apply(null,[(d_1).listFiles()]))}),dir_1]))})))}).apply(null,[]);

//======
//(defn xml-seq "A tree seq on the xml elements as per xml/parse" [root] (tree-seq (complement string?) (comp seq :content) root))
//---
(function __clojure_fn_6660(){
return (clojure.JS.def(clojure,"xml_seq",(function __clojure_fn_6660_xml_seq_6662(root_1){
return (clojure.tree_seq.apply(null,[clojure.complement.apply(null,[clojure.string_QMARK_]),clojure.comp.apply(null,[clojure.seq,clojure.keyword("","content")]),root_1]))})))}).apply(null,[]);
// Skipping: (defn special-symbol? "Returns true if s names a special form" [s] (contains? (. clojure.lang.Compiler specials) s))

//======
//(defn var? "Returns true if v is of type clojure.lang.Var" [v] (instance? clojure.lang.Var v))
//---
(function __clojure_fn_6678(){
return (clojure.JS.def(clojure,"var_QMARK_",(function __clojure_fn_6678_var_QMARK_6680(v_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Var,v_1]))})))}).apply(null,[]);
// Skipping: (defn slurp "Reads the file named by f into a string and returns it." [f] (with-open r (new java.io.BufferedReader (new java.io.FileReader f)) (let [sb (RT/makeStringBuilder)] (loop [c (. r (read))] (if (neg? c) (str sb) (do (. sb (append (char c))) (recur (. r (read)))))))))

//======
//(defn subs "Returns the substring of s beginning at start inclusive, and ending\n  at end (defaults to length of string), exclusive." ([s start] (. s (substring start))) ([s start end] (. s (substring start end))))
//---
(function __clojure_fn_6697(){
return (clojure.JS.def(clojure,"subs",(function __clojure_fn_6697_subs_6699(s_1,start_2,end_3){switch(arguments.length){
case 2:return ((s_1).substring(start_2))}
return ((s_1).substring(start_2,end_3))})))}).apply(null,[]);

//======
//(defn max-key "Returns the x for which (k x), a number, is greatest." ([k x] x) ([k x y] (if (> (k x) (k y)) x y)) ([k x y & more] (reduce (fn* [p1__6704 p2__6705] (max-key k p1__6704 p2__6705)) (max-key k x y) more)))
//---
(function __clojure_fn_6717(){
return (clojure.JS.def(clojure,"max_key",clojure.JS.variadic(3,(function __clojure_fn_6717_max_key_6719(k_1,x_2,y_3){switch(arguments.length){
case 3:return (((clojure.lang.Numbers.gt(k_1.apply(null,[x_2]),k_1.apply(null,[y_3])))?(x_2):(y_3)))
case 2:return (x_2)}
var more_4,more_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.reduce.apply(null,[(function __clojure_fn_6717_max_key_6719_fn_6723(p1__6704_1,p2__6705_2){
return (clojure.max_key.apply(null,[k_1,p1__6704_1,p2__6705_2]))}),clojure.max_key.apply(null,[k_1,x_2,y_3]),more_4]))}))))}).apply(null,[]);

//======
//(defn min-key "Returns the x for which (k x), a number, is least." ([k x] x) ([k x y] (if (< (k x) (k y)) x y)) ([k x y & more] (reduce (fn* [p1__6728 p2__6729] (min-key k p1__6728 p2__6729)) (min-key k x y) more)))
//---
(function __clojure_fn_6741(){
return (clojure.JS.def(clojure,"min_key",clojure.JS.variadic(3,(function __clojure_fn_6741_min_key_6743(k_1,x_2,y_3){switch(arguments.length){
case 3:return (((clojure.lang.Numbers.lt(k_1.apply(null,[x_2]),k_1.apply(null,[y_3])))?(x_2):(y_3)))
case 2:return (x_2)}
var more_4,more_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.reduce.apply(null,[(function __clojure_fn_6741_min_key_6743_fn_6747(p1__6728_1,p2__6729_2){
return (clojure.min_key.apply(null,[k_1,p1__6728_1,p2__6729_2]))}),clojure.min_key.apply(null,[k_1,x_2,y_3]),more_4]))}))))}).apply(null,[]);

//======
//(defn distinct "Returns a lazy sequence of the elements of coll with duplicates removed" [coll] (let [step (fn step [[f & r :as xs] seen] (when xs (if (seen f) (recur r seen) (lazy-cons f (step r (conj seen f))))))] (step (seq coll) #{})))
//---
(function __clojure_fn_6767(){
return (clojure.JS.def(clojure,"distinct",(function __clojure_fn_6767_distinct_6769(coll_1){
var step_2;
return (((step_2=(function __clojure_fn_6767_distinct_6769_step_6772(p__6771_1,seen_2){
var _cnt,_rtn,f_4,vec__6773_3,r_5,xs_6,step_0=arguments.callee;
do{_cnt=0;_rtn=((vec__6773_3=p__6771_1),
(f_4=clojure.nth.apply(null,[vec__6773_3,0,null])),
(r_5=clojure.nthrest.apply(null,[vec__6773_3,1])),
(xs_6=vec__6773_3),
((xs_6)?(((seen_2.apply(null,[f_4]))?((_cnt=1,_rtn=[r_5,seen_2],p__6771_1=_rtn[0],seen_2=_rtn[1])):((new clojure.lang.LazyCons((function __clojure_fn_6767_distinct_6769_step_6772_fn_6775(G__6774_1){switch(arguments.length){
case 0:return (f_4)}
return (step_0.apply(null,[r_5,clojure.conj.apply(null,[seen_2,f_4])]))})))))):(null)))
}while(_cnt);return _rtn;})),
step_2.apply(null,[clojure.seq.apply(null,[coll_1]),clojure.lang.PersistentHashSet.EMPTY])))})))}).apply(null,[]);
// Skipping: (defmacro if-let "if test is true, evaluates then with binding-form bound to the value of test, if not, yields else" ([binding-form test then] (clojure/concat (clojure/list (quote clojure/if-let)) (clojure/list binding-form) (clojure/list test) (clojure/list then) (clojure/list (quote nil)))) ([binding-form test then else] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote temp__6782)) (clojure/list test)))) (clojure/list (clojure/concat (clojure/list (quote if)) (clojure/list (quote temp__6782)) (clojure/list (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list binding-form) (clojure/list (quote temp__6782))))) (clojure/list then))) (clojure/list else))))))
// Skipping: (defmacro when-let "when test is true, evaluates body with binding-form bound to the value of test" [binding-form test & body] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote temp__6790)) (clojure/list test)))) (clojure/list (clojure/concat (clojure/list (quote clojure/when)) (clojure/list (quote temp__6790)) (clojure/list (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list binding-form) (clojure/list (quote temp__6790))))) body))))))

//======
//(defn replace "Given a map of replacement pairs and a vector/collection, returns a\n  vector/seq with any elements = a key in smap replaced with the\n  corresponding val in smap" [smap coll] (if (vector? coll) (reduce (fn [v i] (if-let e (find smap (nth v i)) (assoc v i (val e)) v)) coll (range (count coll))) (map (fn* [p1__6797] (if-let e (find smap p1__6797) (val e) p1__6797)) coll)))
//---
(function __clojure_fn_6810(){
return (clojure.JS.def(clojure,"replace",(function __clojure_fn_6810_replace_6812(smap_1,coll_2){
return (((clojure.vector_QMARK_.apply(null,[coll_2]))?(clojure.reduce.apply(null,[(function __clojure_fn_6810_replace_6812_fn_6814(v_1,i_2){
var temp__1433_3,e_4;
return (((temp__1433_3=clojure.find.apply(null,[smap_1,clojure.nth.apply(null,[v_1,i_2])])),
((temp__1433_3)?(((e_4=temp__1433_3),
clojure.assoc.apply(null,[v_1,i_2,clojure.val.apply(null,[e_4])]))):(v_1))))}),coll_2,clojure.range.apply(null,[clojure.count.apply(null,[coll_2])])])):(clojure.map.apply(null,[(function __clojure_fn_6810_replace_6812_fn_6817(p1__6797_1){
var e_3,temp__1433_2;
return (((temp__1433_2=clojure.find.apply(null,[smap_1,p1__6797_1])),
((temp__1433_2)?(((e_3=temp__1433_2),
clojure.val.apply(null,[e_3]))):(p1__6797_1))))}),coll_2]))))})))}).apply(null,[]);
// Skipping: (defmacro dosync "Runs the exprs (in an implicit do) in a transaction that encompasses\n  exprs and any nested calls.  Starts a transaction if none is already\n  running on this thread. Any uncaught exception will abort the\n  transaction and flow out of dosync. The exprs may be run more than\n  once, but any effects on Refs will be atomic." [& exprs] (clojure/concat (clojure/list (quote clojure/sync)) (clojure/list (quote nil)) exprs))
// Skipping: (defmacro with-precision "Sets the precision and rounding mode to be used for BigDecimal operations.\n\n  Usage: (with-precision 10 (/ 1M 3))\n  or:    (with-precision 10 :rounding HALF_DOWN (/ 1M 3))\n  \n  The rounding mode is one of CEILING, FLOOR, HALF_UP, HALF_DOWN,\n  HALF_EVEN, UP, DOWN and UNNECESSARY; it defaults to HALF_UP." [precision & exprs] (let [[body rm] (if (= (first exprs) :rounding) [(rest (rest exprs)) (clojure/concat (clojure/list (clojure/concat (clojure/list (quote .)) (clojure/list (quote java.math.RoundingMode)) (clojure/list (second exprs)))))] [exprs nil])] (clojure/concat (clojure/list (quote clojure/binding)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote clojure/*math-context*)) (clojure/list (clojure/concat (clojure/list (quote java.math.MathContext.)) (clojure/list precision) rm))))) body)))

//======
//(defn bound-fn {:private true} [sc test key] (fn [e] (test (.. sc comparator (compare (. sc entryKey e) key)) 0)))
//---
(function __clojure_fn_6844(){
return (clojure.JS.def(clojure,"bound_fn",(function __clojure_fn_6844_bound_fn_6846(sc_1,test_2,key_3){
return ((function __clojure_fn_6844_bound_fn_6846_fn_6848(e_1){
return (test_2.apply(null,[((sc_1).comparator()).compare((sc_1).entryKey(e_1),key_3),0]))}))})))}).apply(null,[]);

//======
//(defn subseq "sc must be a sorted collection, test(s) one of <, <=, > or\n  >=. Returns a seq of those entries with keys ek for\n  which (test (.. sc comparator (compare ek key)) 0) is true" ([sc test key] (let [include (bound-fn sc test key)] (if (#{> >=} test) (when-let [e :as s] (. sc seqFrom key true) (if (include e) s (rest s))) (take-while include (. sc seq true))))) ([sc start-test start-key end-test end-key] (when-let [e :as s] (. sc seqFrom start-key true) (take-while (bound-fn sc end-test end-key) (if ((bound-fn sc start-test start-key) e) s (rest s))))))
//---
(function __clojure_fn_6862(){
return (clojure.JS.def(clojure,"subseq",(function __clojure_fn_6862_subseq_6864(sc_1,start_test_2,start_key_3,end_test_4,end_key_5){switch(arguments.length){
case 3:var vec__6866_6,include_4,temp__1438_5,e_7,s_8,test_2=arguments[1],key_3=arguments[2];
return (((include_4=clojure.bound_fn.apply(null,[sc_1,test_2,key_3])),
((clojure.hash_set(clojure._GT_,clojure._GT__EQ_).apply(null,[test_2]))?(((temp__1438_5=(sc_1).seqFrom(key_3,true)),
((temp__1438_5)?(((vec__6866_6=temp__1438_5),
(e_7=clojure.nth.apply(null,[vec__6866_6,0,null])),
(s_8=vec__6866_6),
((include_4.apply(null,[e_7]))?(s_8):(clojure.rest.apply(null,[s_8]))))):(null)))):(clojure.take_while.apply(null,[include_4,(sc_1).seq(true)])))))}
var temp__1438_6,s_9,e_8,vec__6868_7;
return (((temp__1438_6=(sc_1).seqFrom(start_key_3,true)),
((temp__1438_6)?(((vec__6868_7=temp__1438_6),
(e_8=clojure.nth.apply(null,[vec__6868_7,0,null])),
(s_9=vec__6868_7),
clojure.take_while.apply(null,[clojure.bound_fn.apply(null,[sc_1,end_test_4,end_key_5]),((clojure.bound_fn.apply(null,[sc_1,start_test_2,start_key_3]).apply(null,[e_8]))?(s_9):(clojure.rest.apply(null,[s_9])))]))):(null))))})))}).apply(null,[]);

//======
//(defn rsubseq "sc must be a sorted collection, test(s) one of <, <=, > or\n  >=. Returns a reverse seq of those entries with keys ek for\n  which (test (.. sc comparator (compare ek key)) 0) is true" ([sc test key] (let [include (bound-fn sc test key)] (if (#{< <=} test) (when-let [e :as s] (. sc seqFrom key false) (if (include e) s (rest s))) (take-while include (. sc seq false))))) ([sc start-test start-key end-test end-key] (when-let [e :as s] (. sc seqFrom end-key false) (take-while (bound-fn sc start-test start-key) (if ((bound-fn sc end-test end-key) e) s (rest s))))))
//---
(function __clojure_fn_6880(){
return (clojure.JS.def(clojure,"rsubseq",(function __clojure_fn_6880_rsubseq_6882(sc_1,start_test_2,start_key_3,end_test_4,end_key_5){switch(arguments.length){
case 3:var include_4,vec__6884_6,e_7,s_8,temp__1438_5,test_2=arguments[1],key_3=arguments[2];
return (((include_4=clojure.bound_fn.apply(null,[sc_1,test_2,key_3])),
((clojure.hash_set(clojure._LT_,clojure._LT__EQ_).apply(null,[test_2]))?(((temp__1438_5=(sc_1).seqFrom(key_3,false)),
((temp__1438_5)?(((vec__6884_6=temp__1438_5),
(e_7=clojure.nth.apply(null,[vec__6884_6,0,null])),
(s_8=vec__6884_6),
((include_4.apply(null,[e_7]))?(s_8):(clojure.rest.apply(null,[s_8]))))):(null)))):(clojure.take_while.apply(null,[include_4,(sc_1).seq(false)])))))}
var s_9,e_8,temp__1438_6,vec__6886_7;
return (((temp__1438_6=(sc_1).seqFrom(end_key_5,false)),
((temp__1438_6)?(((vec__6886_7=temp__1438_6),
(e_8=clojure.nth.apply(null,[vec__6886_7,0,null])),
(s_9=vec__6886_7),
clojure.take_while.apply(null,[clojure.bound_fn.apply(null,[sc_1,start_test_2,start_key_3]),((clojure.bound_fn.apply(null,[sc_1,end_test_4,end_key_5]).apply(null,[e_8]))?(s_9):(clojure.rest.apply(null,[s_9])))]))):(null))))})))}).apply(null,[]);

//======
//(defn repeatedly "Takes a function of no args, presumably with side effects, and returns an infinite\n  lazy sequence of calls to it" [f] (lazy-cons (f) (repeatedly f)))
//---
(function __clojure_fn_6900(){
return (clojure.JS.def(clojure,"repeatedly",(function __clojure_fn_6900_repeatedly_6902(f_1){
return ((new clojure.lang.LazyCons((function __clojure_fn_6900_repeatedly_6902_fn_6905(G__6904_1){switch(arguments.length){
case 0:return (f_1.apply(null,[]))}
return (clojure.repeatedly.apply(null,[f_1]))}))))})))}).apply(null,[]);

//======
//(defn add-classpath "Adds the url (String or URL object) to the classpath per URLClassLoader.addURL" [url] (. clojure.lang.RT addURL url))
//---
(function __clojure_fn_6917(){
return (clojure.JS.def(clojure,"add_classpath",(function __clojure_fn_6917_add_classpath_6919(url_1){
return (clojure.lang.RT.addURL(url_1))})))}).apply(null,[]);

//======
//(defn hash "Returns the hash code of its argument" [x] (. clojure.lang.Util (hash x)))
//---
(function __clojure_fn_6929(){
return (clojure.JS.def(clojure,"hash",(function __clojure_fn_6929_hash_6931(x_1){
return (clojure.lang.Util.hash(x_1))})))}).apply(null,[]);

//======
//(defn interpose "Returns a lazy seq of the elements of coll separated by sep" [sep coll] (drop 1 (interleave (repeat sep) coll)))
//---
(function __clojure_fn_6941(){
return (clojure.JS.def(clojure,"interpose",(function __clojure_fn_6941_interpose_6943(sep_1,coll_2){
return (clojure.drop.apply(null,[1,clojure.interleave.apply(null,[clojure.repeat.apply(null,[sep_1]),coll_2])]))})))}).apply(null,[]);

//======
//(defn partition "Returns a lazy sequence of lists of n items each, at offsets step\n  apart. If step is not supplied, defaults to n, i.e. the partitions\n  do not overlap." ([n coll] (partition n n coll)) ([n step coll] (when (seq coll) (let [p (take n coll)] (when (= n (count p)) (lazy-cons p (partition n step (drop step coll))))))))
//---
(function __clojure_fn_6959(){
return (clojure.JS.def(clojure,"partition",(function __clojure_fn_6959_partition_6961(n_1,step_2,coll_3){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (clojure.partition.apply(null,[n_1,n_1,coll_2]))}
var p_4;
return (((clojure.seq.apply(null,[coll_3]))?(((p_4=clojure.take.apply(null,[n_1,coll_3])),
((clojure.lang.Util.equal(n_1,clojure.count.apply(null,[p_4])))?((new clojure.lang.LazyCons((function __clojure_fn_6959_partition_6961_fn_6965(G__6964_1){switch(arguments.length){
case 0:return (p_4)}
return (clojure.partition.apply(null,[n_1,step_2,clojure.drop.apply(null,[step_2,coll_3])]))})))):(null)))):(null)))})))}).apply(null,[]);
// Skipping: (defmacro definline "Experimental - like defmacro, except defines a named function whose\n  body is the expansion, calls to which may be expanded inline as if\n  it were a macro. Cannot be used with variadic (&) args." [name & decl] (let [[args expr] (drop-while (comp not vector?) decl) inline (eval (list (quote fn) args expr))] (clojure/concat (clojure/list (quote do)) (clojure/list (clojure/concat (clojure/list (quote clojure/defn)) (clojure/list name) (clojure/list args) (clojure/list (apply inline args)))) (clojure/list (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote v__6971)) (clojure/list (clojure/concat (clojure/list (quote var)) (clojure/list name)))))) (clojure/list (clojure/concat (clojure/list (quote clojure/.setMeta)) (clojure/list (quote v__6971)) (clojure/list (clojure/concat (clojure/list (quote clojure/assoc)) (clojure/list (clojure/concat (clojure/list (quote clojure/meta)) (clojure/list (quote v__6971)))) (clojure/list :inline) (clojure/list inline))))))))))

//======
//(defn empty "Returns an empty collection of the same category as coll, or nil" [coll] (.empty coll))
//---
(function __clojure_fn_6985(){
return (clojure.JS.def(clojure,"empty",(function __clojure_fn_6985_empty_6987(coll_1){
return ((coll_1).empty())})))}).apply(null,[]);
// Skipping: (defmacro amap "Maps an expression across an array a, using an index named idx, and\n  return value named ret, initialized to a clone of a, then setting each element of\n  ret to the evaluation of expr, returning the new array ret." [a idx ret expr] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote a__6991)) (clojure/list a) (clojure/list ret) (clojure/list (clojure/concat (clojure/list (quote clojure/aclone)) (clojure/list (quote a__6991))))))) (clojure/list (clojure/concat (clojure/list (quote clojure/loop)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list idx) (clojure/list (clojure/concat (clojure/list (quote clojure/int)) (clojure/list 0)))))) (clojure/list (clojure/concat (clojure/list (quote if)) (clojure/list (clojure/concat (clojure/list (quote clojure/<)) (clojure/list idx) (clojure/list (clojure/concat (clojure/list (quote clojure/alength)) (clojure/list (quote a__6991)))))) (clojure/list (clojure/concat (clojure/list (quote do)) (clojure/list (clojure/concat (clojure/list (quote clojure/aset)) (clojure/list ret) (clojure/list idx) (clojure/list expr))) (clojure/list (clojure/concat (clojure/list (quote recur)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked-inc)) (clojure/list idx))))))) (clojure/list ret)))))))
// Skipping: (defmacro areduce "Reduces an expression across an array a, using an index named idx,\n  and return value named ret, initialized to init, setting ret to the evaluation of expr at\n  each step, returning ret." [a idx ret init expr] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote a__6998)) (clojure/list a)))) (clojure/list (clojure/concat (clojure/list (quote clojure/loop)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list idx) (clojure/list (clojure/concat (clojure/list (quote clojure/int)) (clojure/list 0))) (clojure/list ret) (clojure/list init)))) (clojure/list (clojure/concat (clojure/list (quote if)) (clojure/list (clojure/concat (clojure/list (quote clojure/<)) (clojure/list idx) (clojure/list (clojure/concat (clojure/list (quote clojure/alength)) (clojure/list (quote a__6998)))))) (clojure/list (clojure/concat (clojure/list (quote recur)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked-inc)) (clojure/list idx))) (clojure/list expr))) (clojure/list ret)))))))

//======
//(defn float-array "Creates an array of floats" {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/float_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers float_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers float_array size init-val-or-seq)))
//---
(function __clojure_fn_7015(){
return (clojure.JS.def(clojure,"float_array",(function __clojure_fn_7015_float_array_7020(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.float_array(size_or_seq_1))}
return (clojure.lang.Numbers.float_array(size_1,init_val_or_seq_2))})))}).apply(null,[]);

//======
//(defn double-array "Creates an array of doubles" {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/double_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers double_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers double_array size init-val-or-seq)))
//---
(function __clojure_fn_7035(){
return (clojure.JS.def(clojure,"double_array",(function __clojure_fn_7035_double_array_7040(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.double_array(size_or_seq_1))}
return (clojure.lang.Numbers.double_array(size_1,init_val_or_seq_2))})))}).apply(null,[]);

//======
//(defn int-array "Creates an array of ints" {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/int_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers int_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers int_array size init-val-or-seq)))
//---
(function __clojure_fn_7055(){
return (clojure.JS.def(clojure,"int_array",(function __clojure_fn_7055_int_array_7060(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.int_array(size_or_seq_1))}
return (clojure.lang.Numbers.int_array(size_1,init_val_or_seq_2))})))}).apply(null,[]);

//======
//(defn long-array "Creates an array of ints" {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/long_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers long_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers long_array size init-val-or-seq)))
//---
(function __clojure_fn_7075(){
return (clojure.JS.def(clojure,"long_array",(function __clojure_fn_7075_long_array_7080(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.long_array(size_or_seq_1))}
return (clojure.lang.Numbers.long_array(size_1,init_val_or_seq_2))})))}).apply(null,[]);
// Skipping: (definline floats "Casts to float[]" [xs] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/floats)) (clojure/list xs)))
// Skipping: (definline ints "Casts to int[]" [xs] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/ints)) (clojure/list xs)))
// Skipping: (definline doubles "Casts to double[]" [xs] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/doubles)) (clojure/list xs)))
// Skipping: (definline longs "Casts to long[]" [xs] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/longs)) (clojure/list xs)))

//======
//(import (quote (java.util.concurrent BlockingQueue LinkedBlockingQueue)))
//---
(function __clojure_fn_7136(){
return (clojure.import_.apply(null,[clojure.JS.lit_list(["'java.util.concurrent","'BlockingQueue","'LinkedBlockingQueue"])]))}).apply(null,[]);
// Skipping: (defn seque "Creates a queued seq on another (presumably lazy) seq s. The queued\n  seq will produce a concrete seq in the background, and can get up to\n  n items ahead of the consumer. n-or-q can be an integer n buffer\n  size, or an instance of java.util.concurrent BlockingQueue. Note\n  that reading from a seque can block if the reader gets ahead of the\n  producer." ([s] (seque 100 s)) ([n-or-q s] (let [q (if (instance? BlockingQueue n-or-q) n-or-q (LinkedBlockingQueue. (int n-or-q))) NIL (Object.) agt (agent (seq s)) fill (fn [s] (try (loop [[x & xs :as s] s] (if s (if (.offer q (if (nil? x) NIL x)) (recur xs) s) (.put q q))) (catch Exception e (.put q q) (throw e)))) drain (fn drain [] (let [x (.take q)] (if (identical? x q) (clojure/deref agt) (do (send-off agt fill) (lazy-cons (if (identical? x NIL) nil x) (drain))))))] (send-off agt fill) (drain))))

//======
//(defn alter-var-root "Atomically alters the root binding of var v by applying f to its\n  current value plus any args" [v f & args] (.alterRoot v f args))
//---
(function __clojure_fn_7166(){
return (clojure.JS.def(clojure,"alter_var_root",clojure.JS.variadic(2,(function __clojure_fn_7166_alter_var_root_7168(v_1,f_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((v_1).alterRoot(f_2,args_3))}))))}).apply(null,[]);

//======
//(defn make-hierarchy "Creates a hierarchy object for use with derive, isa? etc." [] {:parents {}, :ancestors {}, :descendants {}})
//---
(function __clojure_fn_7178(){
return (clojure.JS.def(clojure,"make_hierarchy",(function __clojure_fn_7178_make_hierarchy_7180(){
return (clojure.hash_map(clojure.keyword("","parents"),clojure.lang.PersistentHashMap.EMPTY,clojure.keyword("","ancestors"),clojure.lang.PersistentHashMap.EMPTY,clojure.keyword("","descendants"),clojure.lang.PersistentHashMap.EMPTY))})))}).apply(null,[]);

//======
//(def global-hierarchy (make-hierarchy))
//---
(function __clojure_fn_7187(){
return (clojure.JS.def(clojure,"global_hierarchy",clojure.make_hierarchy.apply(null,[])))}).apply(null,[]);

//======
//(defn not-empty "If coll is empty, returns nil, else coll" [coll] (when (seq coll) coll))
//---
(function __clojure_fn_7196(){
return (clojure.JS.def(clojure,"not_empty",(function __clojure_fn_7196_not_empty_7198(coll_1){
return (((clojure.seq.apply(null,[coll_1]))?(coll_1):(null)))})))}).apply(null,[]);

//======
//(defn bases "Returns the immediate superclass and direct interfaces of c, if any" [c] (let [i (.getInterfaces c) s (.getSuperclass c)] (not-empty (if s (cons s i) i))))
//---
(function __clojure_fn_7208(){
return (clojure.JS.def(clojure,"bases",(function __clojure_fn_7208_bases_7210(c_1){
var s_3,i_2;
return (((i_2=(c_1).getInterfaces()),
(s_3=(c_1).getSuperclass()),
clojure.not_empty.apply(null,[((s_3)?(clojure.cons.apply(null,[s_3,i_2])):(i_2))])))})))}).apply(null,[]);

//======
//(defn supers "Returns the immediate and indirect superclasses and interfaces of c, if any" [class] (loop [ret #{} c class] (if c (recur (into ret (bases c)) (.getSuperclass c)) (not-empty ret))))
//---
(function __clojure_fn_7220(){
return (clojure.JS.def(clojure,"supers",(function __clojure_fn_7220_supers_7222(class_1){
var ret_2,c_3;
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentHashSet.EMPTY),
(c_3=class_1);do{_cnt=0;
_rtn=((c_3)?((_cnt=1,_rtn=[clojure.into.apply(null,[ret_2,clojure.bases.apply(null,[c_3])]),(c_3).getSuperclass()],ret_2=_rtn[0],c_3=_rtn[1])):(clojure.not_empty.apply(null,[ret_2])))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn isa? "Returns true if (= child parent), or child is directly or indirectly derived from\n  parent, either via a Java type inheritance relationship or a\n  relationship established via derive. h must be a hierarchy obtained\n  from make-hierarchy, if not supplied defaults to the global\n  hierarchy" ([child parent] (isa? global-hierarchy child parent)) ([h child parent] (or (= child parent) (and (class? parent) (class? child) (. parent isAssignableFrom child)) (contains? ((:ancestors h) child) parent) (and (class? child) (some (fn* [p1__7226] (contains? ((:ancestors h) p1__7226) parent)) (supers child))) (and (vector? parent) (vector? child) (= (count parent) (count child)) (loop [ret true i 0] (if (= i (count parent)) ret (recur (and (isa? (child i) (parent i)) ret) (inc i))))))))
//---
(function __clojure_fn_7237(){
return (clojure.JS.def(clojure,"isa_QMARK_",(function __clojure_fn_7237_isa_QMARK_7239(h_1,child_2,parent_3){switch(arguments.length){
case 2:var child_1=arguments[0],parent_2=arguments[1];
return (clojure.isa_QMARK_.apply(null,[clojure.global_hierarchy,child_1,parent_2]))}
var ret_11,and__196_5,or__202_4,and__196_8,or__202_6,and__196_10,or__202_5,and__196_13,and__196_6,and__196_7,and__196_9,i_12,or__202_7;
return (((or__202_4=clojure.lang.Util.equal(child_2,parent_3)),
((or__202_4)?(or__202_4):(((or__202_5=((and__196_5=clojure.class_QMARK_.apply(null,[parent_3])),
((and__196_5)?(((and__196_6=clojure.class_QMARK_.apply(null,[child_2])),
((and__196_6)?((parent_3).isAssignableFrom(child_2)):(and__196_6)))):(and__196_5)))),
((or__202_5)?(or__202_5):(((or__202_6=clojure.contains_QMARK_.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]).apply(null,[child_2]),parent_3])),
((or__202_6)?(or__202_6):(((or__202_7=((and__196_7=clojure.class_QMARK_.apply(null,[child_2])),
((and__196_7)?(clojure.some.apply(null,[(function __clojure_fn_7237_isa_QMARK_7239_fn_7242(p1__7226_1){
return (clojure.contains_QMARK_.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]).apply(null,[p1__7226_1]),parent_3]))}),clojure.supers.apply(null,[child_2])])):(and__196_7)))),
((or__202_7)?(or__202_7):(((and__196_8=clojure.vector_QMARK_.apply(null,[parent_3])),
((and__196_8)?(((and__196_9=clojure.vector_QMARK_.apply(null,[child_2])),
((and__196_9)?(((and__196_10=clojure.lang.Util.equal(clojure.count.apply(null,[parent_3]),clojure.count.apply(null,[child_2]))),
((and__196_10)?(((function __loop(){var _rtn,_cnt;(ret_11=true),
(i_12=0);do{_cnt=0;
_rtn=((clojure.lang.Util.equal(i_12,clojure.count.apply(null,[parent_3])))?(ret_11):((_cnt=1,_rtn=[((and__196_13=clojure.isa_QMARK_.apply(null,[child_2.apply(null,[i_12]),parent_3.apply(null,[i_12])])),
((and__196_13)?(ret_11):(and__196_13))),clojure.lang.Numbers.inc(i_12)],ret_11=_rtn[0],i_12=_rtn[1])))}while(_cnt);return _rtn;})())):(and__196_10)))):(and__196_9)))):(and__196_8))))))))))))))))})))}).apply(null,[]);

//======
//(defn parents "Returns the immediate parents of tag, either via a Java type\n  inheritance relationship or a relationship established via derive. h\n  must be a hierarchy obtained from make-hierarchy, if not supplied\n  defaults to the global hierarchy" ([tag] (parents global-hierarchy tag)) ([h tag] (not-empty (let [tp (get (:parents h) tag)] (if (class? tag) (into (set (bases tag)) tp) tp)))))
//---
(function __clojure_fn_7254(){
return (clojure.JS.def(clojure,"parents",(function __clojure_fn_7254_parents_7256(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.parents.apply(null,[clojure.global_hierarchy,tag_1]))}
var tp_3;
return (clojure.not_empty.apply(null,[((tp_3=clojure.get.apply(null,[clojure.keyword("","parents").apply(null,[h_1]),tag_2])),
((clojure.class_QMARK_.apply(null,[tag_2]))?(clojure.into.apply(null,[clojure.set.apply(null,[clojure.bases.apply(null,[tag_2])]),tp_3])):(tp_3)))]))})))}).apply(null,[]);

//======
//(defn ancestors "Returns the immediate and indirect parents of tag, either via a Java type\n  inheritance relationship or a relationship established via derive. h\n  must be a hierarchy obtained from make-hierarchy, if not supplied\n  defaults to the global hierarchy" ([tag] (ancestors global-hierarchy tag)) ([h tag] (not-empty (let [ta (get (:ancestors h) tag)] (if (class? tag) (into (set (supers tag)) ta) ta)))))
//---
(function __clojure_fn_7268(){
return (clojure.JS.def(clojure,"ancestors",(function __clojure_fn_7268_ancestors_7270(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.ancestors.apply(null,[clojure.global_hierarchy,tag_1]))}
var ta_3;
return (clojure.not_empty.apply(null,[((ta_3=clojure.get.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]),tag_2])),
((clojure.class_QMARK_.apply(null,[tag_2]))?(clojure.into.apply(null,[clojure.set.apply(null,[clojure.supers.apply(null,[tag_2])]),ta_3])):(ta_3)))]))})))}).apply(null,[]);

//======
//(defn descendants "Returns the immediate and indirect children of tag, through a\n  relationship established via derive. h must be a hierarchy obtained\n  from make-hierarchy, if not supplied defaults to the global\n  hierarchy. Note: does not work on Java type inheritance\n  relationships." ([tag] (descendants global-hierarchy tag)) ([h tag] (if (class? tag) (throw (RT/makeUnsupportedException "Can't get descendants of classes")) (not-empty (get (:descendants h) tag)))))
//---
(function __clojure_fn_7282(){
return (clojure.JS.def(clojure,"descendants",(function __clojure_fn_7282_descendants_7284(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.descendants.apply(null,[clojure.global_hierarchy,tag_1]))}
return (((clojure.class_QMARK_.apply(null,[tag_2]))?((function __throw(){throw clojure.lang.RT.makeUnsupportedException("Can't get descendants of classes")})()):(clojure.not_empty.apply(null,[clojure.get.apply(null,[clojure.keyword("","descendants").apply(null,[h_1]),tag_2])]))))})))}).apply(null,[]);

//======
//(defn derive "Establishes a parent/child relationship between parent and\n  tag. Parent must be a namespace-qualified symbol or keyword and\n  child can be either a namespace-qualified symbol or keyword or a\n  class. h must be a hierarchy obtained from make-hierarchy, if not\n  supplied defaults to, and modifies, the global hierarchy." ([tag parent] (alter-var-root (var global-hierarchy) derive tag parent) nil) ([h tag parent] (assert (not= tag parent)) (assert (or (class? tag) (and (instance? clojure.lang.Named tag) (namespace tag)))) (assert (instance? clojure.lang.Named parent)) (assert (namespace parent)) (let [tp (:parents h) td (:descendants h) ta (:ancestors h) tf (fn [m source sources target targets] (reduce (fn [ret k] (assoc ret k (reduce conj (get targets k #{}) (cons target (targets target))))) m (cons source (sources source))))] (or (when-not (contains? (tp tag) parent) (when (contains? (ta tag) parent) (throw (RT/makeException (print-str tag "already has" parent "as ancestor")))) (when (contains? (ta parent) tag) (throw (RT/makeException (print-str "Cyclic derivation:" parent "has" tag "as ancestor")))) {:parents (assoc (:parents h) tag (conj (get tp tag #{}) parent)), :ancestors (tf (:ancestors h) tag td parent ta), :descendants (tf (:descendants h) parent ta tag td)}) h))))
//---
(function __clojure_fn_7302(){
return (clojure.JS.def(clojure,"derive",(function __clojure_fn_7302_derive_7304(h_1,tag_2,parent_3){switch(arguments.length){
case 2:var tag_1=arguments[0],parent_2=arguments[1];
return (clojure.alter_var_root.apply(null,[clojure._var_global_hierarchy,clojure.derive,tag_1,parent_2]),
null)}
var tf_7,td_5,ta_6,or__202_4,or__202_8,tp_4,and__196_5;
return (((clojure.not_EQ_.apply(null,[tag_2,parent_3]))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.str.apply(null,["Assert failed: ",clojure.pr_str.apply(null,[clojure.JS.lit_list(["'not=","'tag","'parent"])])]))})())),
((((or__202_4=clojure.class_QMARK_.apply(null,[tag_2])),
((or__202_4)?(or__202_4):(((and__196_5=clojure.instance_QMARK_.apply(null,[clojure.lang.Named,tag_2])),
((and__196_5)?(clojure.namespace.apply(null,[tag_2])):(and__196_5)))))))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.str.apply(null,["Assert failed: ",clojure.pr_str.apply(null,[clojure.JS.lit_list(["'or",clojure.JS.lit_list(["'class?","'tag"]),clojure.JS.lit_list(["'and",clojure.JS.lit_list(["'instance?","'clojure.lang.Named","'tag"]),clojure.JS.lit_list(["'namespace","'tag"])])])])]))})())),
((clojure.instance_QMARK_.apply(null,[clojure.lang.Named,parent_3]))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.str.apply(null,["Assert failed: ",clojure.pr_str.apply(null,[clojure.JS.lit_list(["'instance?","'clojure.lang.Named","'parent"])])]))})())),
((clojure.namespace.apply(null,[parent_3]))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.str.apply(null,["Assert failed: ",clojure.pr_str.apply(null,[clojure.JS.lit_list(["'namespace","'parent"])])]))})())),
((tp_4=clojure.keyword("","parents").apply(null,[h_1])),
(td_5=clojure.keyword("","descendants").apply(null,[h_1])),
(ta_6=clojure.keyword("","ancestors").apply(null,[h_1])),
(tf_7=(function __clojure_fn_7302_derive_7304_tf_7307(m_1,source_2,sources_3,target_4,targets_5){
return (clojure.reduce.apply(null,[(function __clojure_fn_7302_derive_7304_tf_7307_fn_7309(ret_1,k_2){
return (clojure.assoc.apply(null,[ret_1,k_2,clojure.reduce.apply(null,[clojure.conj,clojure.get.apply(null,[targets_5,k_2,clojure.lang.PersistentHashSet.EMPTY]),clojure.cons.apply(null,[target_4,targets_5.apply(null,[target_4])])])]))}),m_1,clojure.cons.apply(null,[source_2,sources_3.apply(null,[source_2])])]))})),
((or__202_8=((clojure.contains_QMARK_.apply(null,[tp_4.apply(null,[tag_2]),parent_3]))?(null):(((clojure.contains_QMARK_.apply(null,[ta_6.apply(null,[tag_2]),parent_3]))?((function __throw(){throw clojure.lang.RT.makeException(clojure.print_str.apply(null,[tag_2,"already has",parent_3,"as ancestor"]))})()):(null)),
((clojure.contains_QMARK_.apply(null,[ta_6.apply(null,[parent_3]),tag_2]))?((function __throw(){throw clojure.lang.RT.makeException(clojure.print_str.apply(null,["Cyclic derivation:",parent_3,"has",tag_2,"as ancestor"]))})()):(null)),
clojure.hash_map(clojure.keyword("","parents"),clojure.assoc.apply(null,[clojure.keyword("","parents").apply(null,[h_1]),tag_2,clojure.conj.apply(null,[clojure.get.apply(null,[tp_4,tag_2,clojure.lang.PersistentHashSet.EMPTY]),parent_3])]),clojure.keyword("","ancestors"),tf_7.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]),tag_2,td_5,parent_3,ta_6]),clojure.keyword("","descendants"),tf_7.apply(null,[clojure.keyword("","descendants").apply(null,[h_1]),parent_3,ta_6,tag_2,td_5]))))),
((or__202_8)?(or__202_8):(h_1)))))})))}).apply(null,[]);

//======
//(defn underive "Removes a parent/child relationship between parent and\n  tag. h must be a hierarchy obtained from make-hierarchy, if not\n  supplied defaults to, and modifies, the global hierarchy." ([tag parent] (alter-var-root (var global-hierarchy) underive tag parent) nil) ([h tag parent] (let [tp (:parents h) td (:descendants h) ta (:ancestors h) tf (fn [m source sources target targets] (reduce (fn [ret k] (assoc ret k (reduce disj (get targets k) (cons target (targets target))))) m (cons source (sources source))))] (if (contains? (tp tag) parent) {:parent (assoc (:parents h) tag (disj (get tp tag) parent)), :ancestors (tf (:ancestors h) tag td parent ta), :descendants (tf (:descendants h) parent ta tag td)} h))))
//---
(function __clojure_fn_7328(){
return (clojure.JS.def(clojure,"underive",(function __clojure_fn_7328_underive_7330(h_1,tag_2,parent_3){switch(arguments.length){
case 2:var tag_1=arguments[0],parent_2=arguments[1];
return (clojure.alter_var_root.apply(null,[clojure._var_global_hierarchy,clojure.underive,tag_1,parent_2]),
null)}
var td_5,tf_7,ta_6,tp_4;
return (((tp_4=clojure.keyword("","parents").apply(null,[h_1])),
(td_5=clojure.keyword("","descendants").apply(null,[h_1])),
(ta_6=clojure.keyword("","ancestors").apply(null,[h_1])),
(tf_7=(function __clojure_fn_7328_underive_7330_tf_7333(m_1,source_2,sources_3,target_4,targets_5){
return (clojure.reduce.apply(null,[(function __clojure_fn_7328_underive_7330_tf_7333_fn_7335(ret_1,k_2){
return (clojure.assoc.apply(null,[ret_1,k_2,clojure.reduce.apply(null,[clojure.disj,clojure.get.apply(null,[targets_5,k_2]),clojure.cons.apply(null,[target_4,targets_5.apply(null,[target_4])])])]))}),m_1,clojure.cons.apply(null,[source_2,sources_3.apply(null,[source_2])])]))})),
((clojure.contains_QMARK_.apply(null,[tp_4.apply(null,[tag_2]),parent_3]))?(clojure.hash_map(clojure.keyword("","parent"),clojure.assoc.apply(null,[clojure.keyword("","parents").apply(null,[h_1]),tag_2,clojure.disj.apply(null,[clojure.get.apply(null,[tp_4,tag_2]),parent_3])]),clojure.keyword("","ancestors"),tf_7.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]),tag_2,td_5,parent_3,ta_6]),clojure.keyword("","descendants"),tf_7.apply(null,[clojure.keyword("","descendants").apply(null,[h_1]),parent_3,ta_6,tag_2,td_5]))):(h_1))))})))}).apply(null,[]);

//======
//(defn distinct? "Returns true if no two of the arguments are equal" {:tag Boolean} ([x] true) ([x y] (not (= x y))) ([x y & more] (if (not= x y) (loop [s #{y x} [x & etc :as xs] more] (if xs (if (contains? s x) false (recur (conj s x) etc)) true)) false)))
//---
(function __clojure_fn_7353(){
return (clojure.JS.def(clojure,"distinct_QMARK_",clojure.JS.variadic(2,(function __clojure_fn_7353_distinct_QMARK_7355(x_1,y_2){switch(arguments.length){
case 2:return (clojure.not.apply(null,[clojure.lang.Util.equal(x_1,y_2)]))
case 1:return (true)}
var s_10,G__7360_5,xs_9,xs_16,G__7360_11,more_3,x_7,vec__7362_13,s_12,etc_8,etc_15,x_14,s_4,vec__7361_6,more_3=clojure.JS.rest_args(this,arguments,2);
return (((clojure.not_EQ_.apply(null,[x_1,y_2]))?(((s_4=clojure.hash_set(y_2,x_1)),
(G__7360_5=more_3),
(vec__7361_6=G__7360_5),
(x_7=clojure.nth.apply(null,[vec__7361_6,0,null])),
(etc_8=clojure.nthrest.apply(null,[vec__7361_6,1])),
(xs_9=vec__7361_6),
((function __loop(){var _rtn,_cnt;(s_10=s_4),
(G__7360_11=G__7360_5);do{_cnt=0;
_rtn=((s_12=s_10),
(vec__7362_13=G__7360_11),
(x_14=clojure.nth.apply(null,[vec__7362_13,0,null])),
(etc_15=clojure.nthrest.apply(null,[vec__7362_13,1])),
(xs_16=vec__7362_13),
((xs_16)?(((clojure.contains_QMARK_.apply(null,[s_12,x_14]))?(false):((_cnt=1,_rtn=[clojure.conj.apply(null,[s_12,x_14]),etc_15],s_10=_rtn[0],G__7360_11=_rtn[1])))):(true)))}while(_cnt);return _rtn;})()))):(false)))}))))}).apply(null,[]);

//======
//(defn iterator-seq "Returns a seq on a java.util.Iterator. Note that most collections\n  providing iterators implement Iterable and thus support seq directly." [iter] (clojure.lang.IteratorSeq/create iter))
//---
(function __clojure_fn_7371(){
return (clojure.JS.def(clojure,"iterator_seq",(function __clojure_fn_7371_iterator_seq_7373(iter_1){
return (clojure.lang.IteratorSeq.create(iter_1))})))}).apply(null,[]);

//======
//(defn enumeration-seq "Returns a seq on a java.lang.Enumeration" [e] (clojure.lang.EnumerationSeq/create e))
//---
(function __clojure_fn_7383(){
return (clojure.JS.def(clojure,"enumeration_seq",(function __clojure_fn_7383_enumeration_seq_7385(e_1){
return (clojure.lang.EnumerationSeq.create(e_1))})))}).apply(null,[]);
// Skipping: (defn format "Formats a string using java.lang.String.format, see java.util.Formatter for format\n  string syntax" [fmt & args] (String/format fmt (to-array args)))

//======
//(defn printf "Prints formatted output, as per format" [fmt & args] (print (apply format fmt args)))
//---
(function __clojure_fn_7401(){
return (clojure.JS.def(clojure,"printf",clojure.JS.variadic(1,(function __clojure_fn_7401_printf_7403(fmt_1){
var args_2,args_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.print.apply(null,[clojure.apply.apply(null,[clojure.format,fmt_1,args_2])]))}))))}).apply(null,[]);
// Skipping: (defmacro ns "Sets *ns* to the namespace named by name (unevaluated), creating it\n  if needed.  references can be zero or more of: (:refer-clojure ...)\n  (:require ...) (:use ...) (:import ...) (:load ...) with the syntax\n  of refer-clojure/require/use/import/load respectively, except the\n  arguments are unevaluated and need not be quoted.  If :refer-clojure\n  is not used, a default (refer 'clojure) is used. Use of ns is preferred\n  to individual calls to in-ns/require/use/import:\n\n  (ns foo\n    (:refer-clojure :exclude [ancestors printf])\n    (:require (clojure.contrib sql sql.tests))\n    (:use (my.lib this that))\n    (:import (java.util Date Timer Random)\n              (java.sql Connection Statement))\n    (:load \"/mystuff/foo.clj\"))" [name & references] (let [process-reference (fn [[kname & args]] (clojure/concat (clojure/list (symbol "clojure" (clojure/name kname))) (map (fn* [p1__7407] (list (quote quote) p1__7407)) args)))] (clojure/concat (clojure/list (quote do)) (clojure/list (clojure/concat (clojure/list (quote clojure/in-ns)) (clojure/list (clojure/concat (clojure/list (quote quote)) (clojure/list name))))) (when (not-any? (fn* [p1__7408] (= :refer-clojure (first p1__7408))) references) (clojure/concat (clojure/list (clojure/concat (clojure/list (quote clojure/refer)) (clojure/list (clojure/concat (clojure/list (quote quote)) (clojure/list (quote clojure)))))))) (map process-reference references))))
// Skipping: (defmacro refer-clojure "Same as (refer 'clojure <filters>)" [& filters] (clojure/concat (clojure/list (quote clojure/refer)) (clojure/list (clojure/concat (clojure/list (quote quote)) (clojure/list (quote clojure)))) filters))
// Skipping: (defmacro defonce "defs name to have the root value of the expr iff the named var has no root value, \n  else expr is unevaluated" [name expr] (clojure/concat (clojure/list (quote clojure/let)) (clojure/list (clojure/apply clojure/vector (clojure/concat (clojure/list (quote v__7432)) (clojure/list (clojure/concat (clojure/list (quote def)) (clojure/list name)))))) (clojure/list (clojure/concat (clojure/list (quote clojure/when-not)) (clojure/list (clojure/concat (clojure/list (quote clojure/.hasRoot)) (clojure/list (quote v__7432)))) (clojure/list (clojure/concat (clojure/list (quote def)) (clojure/list name) (clojure/list expr)))))))

//======
//(defonce *loaded-libs* (ref (sorted-set)))
//---
(function __clojure_fn_7442(){
var v__1711_1;
return (((v__1711_1=clojure.JS.def(clojure,"_STAR_loaded_libs_STAR_",null)),
(((v__1711_1).hasRoot())?(null):(clojure.JS.def(clojure,"_STAR_loaded_libs_STAR_",clojure.ref.apply(null,[clojure.sorted_set.apply(null,[])]))))))}).apply(null,[]);

//======
//(defonce *pending-paths* #{})
//---
(function __clojure_fn_7448(){
var v__1711_1;
return (((v__1711_1=clojure.JS.def(clojure,"_STAR_pending_paths_STAR_",null)),
(((v__1711_1).hasRoot())?(null):(clojure.JS.def(clojure,"_STAR_pending_paths_STAR_",clojure.lang.PersistentHashSet.EMPTY)))))}).apply(null,[]);

//======
//(defonce *loading-verbosely* false)
//---
(function __clojure_fn_7454(){
var v__1711_1;
return (((v__1711_1=clojure.JS.def(clojure,"_STAR_loading_verbosely_STAR_",null)),
(((v__1711_1).hasRoot())?(null):(clojure.JS.def(clojure,"_STAR_loading_verbosely_STAR_",false)))))}).apply(null,[]);

//======
//(defn- throw-if "Throws an exception with a message if pred is true" [pred fmt & args] (when pred (let [message (apply format fmt args) exception (RT/makeException message) raw-trace (.getStackTrace exception) boring? (fn* [p1__7457] (not= (.getMethodName p1__7457) "doInvoke")) trace (into-array (drop 2 (drop-while boring? raw-trace)))] (.setStackTrace exception trace) (throw exception))))
//---
(function __clojure_fn_7467(){
return (clojure.JS.def(clojure,"throw_if",clojure.JS.variadic(2,(function __clojure_fn_7467_throw_if_7469(pred_1,fmt_2){
var args_3,message_4,raw_trace_6,trace_8,boring_QMARK__7,exception_5,args_3=clojure.JS.rest_args(this,arguments,2);
return (((pred_1)?(((message_4=clojure.apply.apply(null,[clojure.format,fmt_2,args_3])),
(exception_5=clojure.lang.RT.makeException(message_4)),
(raw_trace_6=(exception_5).getStackTrace()),
(boring_QMARK__7=(function __clojure_fn_7467_throw_if_7469_boring_QMARK_7471(p1__7457_1){
return (clojure.not_EQ_.apply(null,[(p1__7457_1).getMethodName,"doInvoke"]))})),
(trace_8=clojure.into_array.apply(null,[clojure.drop.apply(null,[2,clojure.drop_while.apply(null,[boring_QMARK__7,raw_trace_6])])])),
(exception_5).setStackTrace(trace_8),
(function __throw(){throw exception_5})())):(null)))}))))}).apply(null,[]);

//======
//(defn- libspec? "Returns true if x is a libspec" [x] (or (symbol? x) (and (vector? x) (or (nil? (second x)) (keyword? (second x))))))
//---
(function __clojure_fn_7482(){
return (clojure.JS.def(clojure,"libspec_QMARK_",(function __clojure_fn_7482_libspec_QMARK_7484(x_1){
var and__196_3,or__202_2,or__202_4;
return (((or__202_2=clojure.symbol_QMARK_.apply(null,[x_1])),
((or__202_2)?(or__202_2):(((and__196_3=clojure.vector_QMARK_.apply(null,[x_1])),
((and__196_3)?(((or__202_4=clojure.nil_QMARK_.apply(null,[clojure.second.apply(null,[x_1])])),
((or__202_4)?(or__202_4):(clojure.keyword_QMARK_.apply(null,[clojure.second.apply(null,[x_1])]))))):(and__196_3)))))))})))}).apply(null,[]);

//======
//(defn- prependss "Prepends a symbol or a seq to coll" [x coll] (if (symbol? x) (cons x coll) (concat x coll)))
//---
(function __clojure_fn_7494(){
return (clojure.JS.def(clojure,"prependss",(function __clojure_fn_7494_prependss_7496(x_1,coll_2){
return (((clojure.symbol_QMARK_.apply(null,[x_1]))?(clojure.cons.apply(null,[x_1,coll_2])):(clojure.concat.apply(null,[x_1,coll_2]))))})))}).apply(null,[]);

//======
//(defn- root-directory "Returns the root directory path for a lib" [lib] (str \/ (.. (name lib) (replace \- \_) (replace \. \/))))
//---
(function __clojure_fn_7506(){
return (clojure.JS.def(clojure,"root_directory",(function __clojure_fn_7506_root_directory_7508(lib_1){
return (clojure.str.apply(null,["/",((clojure.name.apply(null,[lib_1])).replace("-","_")).replace(".","/")]))})))}).apply(null,[]);

//======
//(defn- root-resource "Returns the root resource path for a lib" [lib] (let [d (root-directory lib) i (inc (.lastIndexOf d (int \/))) leaf (.substring d i)] (str d \/ leaf ".clj")))
//---
(function __clojure_fn_7518(){
return (clojure.JS.def(clojure,"root_resource",(function __clojure_fn_7518_root_resource_7520(lib_1){
var leaf_4,d_2,i_3;
return (((d_2=clojure.root_directory.apply(null,[lib_1])),
(i_3=clojure.lang.Numbers.inc((d_2).lastIndexOf(clojure.lang.RT.intCast("/")))),
(leaf_4=(d_2).substring(i_3)),
clojure.str.apply(null,[d_2,"/",leaf_4,".clj"])))})))}).apply(null,[]);

//======
//(def load)
//---
(function __clojure_fn_7527(){
return (clojure.JS.def(clojure,"load",null))}).apply(null,[]);

//======
//(defn- load-one "Loads a lib given its name. If need-ns, ensures that the associated\n  namespace exists after loading. If require, records the load so any\n  duplicate loads can be skipped." [lib need-ns require] (load (root-resource lib)) (throw-if (and need-ns (not (find-ns lib))) "namespace '%s' not found after loading '%s'" lib (root-resource lib)) (when require (dosync (commute *loaded-libs* conj lib))))
//---
(function __clojure_fn_7539(){
return (clojure.JS.def(clojure,"load_one",(function __clojure_fn_7539_load_one_7541(lib_1,need_ns_2,require_3){
var and__196_4;
return (clojure.load.apply(null,[clojure.root_resource.apply(null,[lib_1])]),
clojure.throw_if.apply(null,[((and__196_4=need_ns_2),
((and__196_4)?(clojure.not.apply(null,[clojure.find_ns.apply(null,[lib_1])])):(and__196_4))),"namespace '%s' not found after loading '%s'",lib_1,clojure.root_resource.apply(null,[lib_1])]),
((require_3)?(clojure.lang.LockingTransaction.runInTransaction((function __clojure_fn_7539_load_one_7541_fn_7543(){
return (clojure.commute.apply(null,[clojure._STAR_loaded_libs_STAR_,clojure.conj,lib_1]))}))):(null)))})))}).apply(null,[]);

//======
//(defn- load-all "Loads a lib given its name and forces a load of any libs it directly or\n  indirectly loads. If need-ns, ensures that the associated namespace\n  exists after loading. If require, records the load so any duplicate loads\n  can be skipped." [lib need-ns require] (dosync (commute *loaded-libs* (fn* [p1__7548 p2__7549] (reduce conj p1__7548 p2__7549)) (binding [*loaded-libs* (ref (sorted-set))] (load-one lib need-ns require) (clojure/deref *loaded-libs*)))))
//---
(function __clojure_fn_7565(){
return (clojure.JS.def(clojure,"load_all",(function __clojure_fn_7565_load_all_7567(lib_1,need_ns_2,require_3){
return (clojure.lang.LockingTransaction.runInTransaction((function __clojure_fn_7565_load_all_7567_fn_7569(){
return (clojure.commute.apply(null,[clojure._STAR_loaded_libs_STAR_,(function __clojure_fn_7565_load_all_7567_fn_7569_fn_7571(p1__7548_1,p2__7549_2){
return (clojure.reduce.apply(null,[clojure.conj,p1__7548_1,p2__7549_2]))}),clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_loaded_libs_STAR_,clojure.ref.apply(null,[clojure.sorted_set.apply(null,[])])])),
(function __clojure_fn_7565_load_all_7567_fn_7569_fn_7574(){
return ((function __try(){try{var _rtn=(clojure.load_one.apply(null,[lib_1,need_ns_2,require_3]),
clojure.deref.apply(null,[clojure._STAR_loaded_libs_STAR_]))}
finally{clojure.lang.Var.popThreadBindings()}})())}).apply(null,[])]))})))})))}).apply(null,[]);

//======
//(defn- load-lib "Loads a lib with options" [prefix lib & options] (throw-if (and prefix (pos? (.indexOf (name lib) (int \.)))) "lib names inside prefix lists must not contain periods") (let [lib (if prefix (symbol (str prefix \. lib)) lib) opts (apply hash-map options) {:keys [as reload reload-all require use verbose]} opts loaded (contains? (clojure/deref *loaded-libs*) lib) load (cond reload-all load-all (or reload (not require) (not loaded)) load-one) need-ns (or as use) filter-opts (select-keys opts (quote (:exclude :only :rename)))] (binding [*loading-verbosely* (or *loading-verbosely* verbose)] (if load (load lib need-ns require) (throw-if (and need-ns (not (find-ns lib))) "namespace '%s' not found" lib)) (when (and need-ns *loading-verbosely*) (printf "(clojure/in-ns '%s)\n" (ns-name *ns*))) (when as (when *loading-verbosely* (printf "(clojure/alias '%s '%s)\n" as lib)) (alias as lib)) (when use (when *loading-verbosely* (printf "(clojure/refer '%s" lib) (doseq opt filter-opts (printf " %s '%s" (key opt) (print-str (val opt)))) (printf ")\n")) (apply refer lib (mapcat seq filter-opts))))))
//---
(function __clojure_fn_7587(){
return (clojure.JS.def(clojure,"load_lib",clojure.JS.variadic(2,(function __clojure_fn_7587_load_lib_7589(prefix_1,lib_2){
var use_12,and__196_19,filter_opts_16,options_3,need_ns_15,lib_4,or__202_17,require_9,or__202_14,and__196_4,opts_5,reload_7,reload_all_10,as_8,opt_20,or__202_15,loaded_13,and__196_19,verbose_11,list__781_19,load_14,or__202_15,map__7591_6,options_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.throw_if.apply(null,[((and__196_4=prefix_1),
((and__196_4)?(clojure.lang.Numbers.isPos((clojure.name.apply(null,[lib_2])).indexOf(clojure.lang.RT.intCast(".")))):(and__196_4))),"lib names inside prefix lists must not contain periods"]),
((lib_4=((prefix_1)?(clojure.symbol.apply(null,[clojure.str.apply(null,[prefix_1,".",lib_2])])):(lib_2))),
(opts_5=clojure.apply.apply(null,[clojure.hash_map,options_3])),
(map__7591_6=opts_5),
(reload_7=clojure.get.apply(null,[map__7591_6,clojure.keyword("","reload")])),
(as_8=clojure.get.apply(null,[map__7591_6,clojure.keyword("","as")])),
(require_9=clojure.get.apply(null,[map__7591_6,clojure.keyword("","require")])),
(reload_all_10=clojure.get.apply(null,[map__7591_6,clojure.keyword("","reload-all")])),
(verbose_11=clojure.get.apply(null,[map__7591_6,clojure.keyword("","verbose")])),
(use_12=clojure.get.apply(null,[map__7591_6,clojure.keyword("","use")])),
(loaded_13=clojure.contains_QMARK_.apply(null,[clojure.deref.apply(null,[clojure._STAR_loaded_libs_STAR_]),lib_4])),
(load_14=((reload_all_10)?(clojure.load_all):(((((or__202_14=reload_7),
((or__202_14)?(or__202_14):(((or__202_15=clojure.not.apply(null,[require_9])),
((or__202_15)?(or__202_15):(clojure.not.apply(null,[loaded_13]))))))))?(clojure.load_one):(null))))),
(need_ns_15=((or__202_15=as_8),
((or__202_15)?(or__202_15):(use_12)))),
(filter_opts_16=clojure.select_keys.apply(null,[opts_5,clojure.JS.lit_list([clojure.keyword("","exclude"),clojure.keyword("","only"),clojure.keyword("","rename")])])),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_loading_verbosely_STAR_,((or__202_17=clojure._STAR_loading_verbosely_STAR_),
((or__202_17)?(or__202_17):(verbose_11)))])),
(function __try(){try{var _rtn=(((load_14)?(load_14.apply(null,[lib_4,need_ns_15,require_9])):(clojure.throw_if.apply(null,[((and__196_19=need_ns_15),
((and__196_19)?(clojure.not.apply(null,[clojure.find_ns.apply(null,[lib_4])])):(and__196_19))),"namespace '%s' not found",lib_4]))),
((((and__196_19=need_ns_15),
((and__196_19)?(clojure._STAR_loading_verbosely_STAR_):(and__196_19))))?(clojure.printf.apply(null,["(clojure/in-ns '%s)\n",clojure.ns_name.apply(null,[clojure._STAR_ns_STAR_])])):(null)),
((as_8)?(((clojure._STAR_loading_verbosely_STAR_)?(clojure.printf.apply(null,["(clojure/alias '%s '%s)\n",as_8,lib_4])):(null)),
clojure.alias.apply(null,[as_8,lib_4])):(null)),
((use_12)?(((clojure._STAR_loading_verbosely_STAR_)?(clojure.printf.apply(null,["(clojure/refer '%s",lib_4]),
((function __loop(){var _rtn,_cnt;(list__781_19=clojure.seq.apply(null,[filter_opts_16]));do{_cnt=0;
_rtn=((list__781_19)?(((opt_20=clojure.first.apply(null,[list__781_19])),
clojure.printf.apply(null,[" %s '%s",clojure.key.apply(null,[opt_20]),clojure.print_str.apply(null,[clojure.val.apply(null,[opt_20])])])),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__781_19])],list__781_19=_rtn[0])):(null))}while(_cnt);return _rtn;})()),
clojure.printf.apply(null,[")\n"])):(null)),
clojure.apply.apply(null,[clojure.refer,lib_4,clojure.mapcat.apply(null,[clojure.seq,filter_opts_16])])):(null)))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn- load-libs "Loads libs, interpreting libspecs, prefix lists, and flags for\n  forwarding to load-lib" [& args] (let [flags (filter keyword? args) opts (interleave flags (repeat true)) args (filter (complement keyword?) args)] (doseq arg args (if (libspec? arg) (apply load-lib nil (prependss arg opts)) (let [[prefix & args] arg] (throw-if (nil? prefix) "prefix cannot be nil") (doseq arg args (apply load-lib prefix (prependss arg opts))))))))
//---
(function __clojure_fn_7601(){
return (clojure.JS.def(clojure,"load_libs",clojure.JS.variadic(0,(function __clojure_fn_7601_load_libs_7603(){
var vec__7605_7,list__781_5,args_1,prefix_8,flags_2,args_9,arg_11,opts_3,list__781_10,arg_6,args_4,args_1=clojure.JS.rest_args(this,arguments,0);
return (((flags_2=clojure.filter.apply(null,[clojure.keyword_QMARK_,args_1])),
(opts_3=clojure.interleave.apply(null,[flags_2,clojure.repeat.apply(null,[true])])),
(args_4=clojure.filter.apply(null,[clojure.complement.apply(null,[clojure.keyword_QMARK_]),args_1])),
((function __loop(){var _rtn,_cnt;(list__781_5=clojure.seq.apply(null,[args_4]));do{_cnt=0;
_rtn=((list__781_5)?(((arg_6=clojure.first.apply(null,[list__781_5])),
((clojure.libspec_QMARK_.apply(null,[arg_6]))?(clojure.apply.apply(null,[clojure.load_lib,null,clojure.prependss.apply(null,[arg_6,opts_3])])):(((vec__7605_7=arg_6),
(prefix_8=clojure.nth.apply(null,[vec__7605_7,0,null])),
(args_9=clojure.nthrest.apply(null,[vec__7605_7,1])),
clojure.throw_if.apply(null,[clojure.nil_QMARK_.apply(null,[prefix_8]),"prefix cannot be nil"]),
((function __loop(){var _rtn,_cnt;(list__781_10=clojure.seq.apply(null,[args_9]));do{_cnt=0;
_rtn=((list__781_10)?(((arg_11=clojure.first.apply(null,[list__781_10])),
clojure.apply.apply(null,[clojure.load_lib,prefix_8,clojure.prependss.apply(null,[arg_11,opts_3])])),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__781_10])],list__781_10=_rtn[0])):(null))}while(_cnt);return _rtn;})()))))),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__781_5])],list__781_5=_rtn[0])):(null))}while(_cnt);return _rtn;})())))}))))}).apply(null,[]);

//======
//(defn require "Loads libs, skipping any that are already loaded. Each argument is\n  either a libspec that identifies a lib, a prefix list that identifies\n  multiple libs whose names share a common prefix, or a flag that modifies\n  how all the identified libs are loaded. Use :require in the ns macro \n  in preference to calling this directly.\n\n  Libs\n\n  A 'lib' is a named set of resources in classpath whose contents define a\n  library of Clojure code. Lib names are symbols and each lib is associated\n  with a Clojure namespace and a Java package that share its name. A lib's\n  name also locates its root directory within classpath using Java's\n  package name to classpath-relative path mapping. All resources in a lib\n  should be contained in the directory structure under its root directory.\n  All definitions a lib makes should be in its associated namespace.\n\n  'require loads a lib by loading its root resource. The root resource path\n  is derived from the root directory path by repeating its last component\n  and appending '.clj'. For example, the lib 'x.y.z has root directory\n  <classpath>/x/y/z; root resource <classpath>/x/y/z/z.clj. The root\n  resource should contain code to create the lib's namespace and load any\n  additional lib resources.\n\n  Libspecs\n\n  A libspec is a lib name or a vector containing a lib name followed by\n  options expressed as sequential keywords and arguments.\n\n  Recognized options: :as\n  :as takes a symbol as its argument and makes that symbol an alias to the\n    lib's namespace in the current namespace.\n\n  Prefix Lists\n\n  It's common for Clojure code to depend on several libs whose names have\n  the same prefix. When specifying libs, prefix lists can be used to reduce\n  repetition. A prefix list contains the shared prefix followed by libspecs\n  with the shared prefix removed from the lib names. After removing the\n  prefix, the names that remain must not contain any periods.\n\n  Flags\n\n  A flag is a keyword.\n  Recognized flags: :reload, :reload-all, :verbose\n  :reload forces loading of all the identified libs even if they are\n    already loaded\n  :reload-all implies :reload and also forces loading of all libs that the\n    identified libs directly or indirectly load via require or use\n  :verbose triggers printing information about each load, alias, and refer" [& args] (apply load-libs :require args))
//---
(function __clojure_fn_7614(){
return (clojure.JS.def(clojure,"require",clojure.JS.variadic(0,(function __clojure_fn_7614_require_7616(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[clojure.load_libs,clojure.keyword("","require"),args_1]))}))))}).apply(null,[]);

//======
//(defn use "Like 'require, but also refers to each lib's namespace using\n  clojure/refer. Use :use in the ns macro in preference to calling\n  this directly.\n\n  'use accepts additional options in libspecs: :exclude, :only, :rename.\n  The arguments and semantics for :exclude, :only, and :rename are the same\n  as those documented for clojure/refer." [& args] (apply load-libs :require :use args))
//---
(function __clojure_fn_7626(){
return (clojure.JS.def(clojure,"use",clojure.JS.variadic(0,(function __clojure_fn_7626_use_7628(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[clojure.load_libs,clojure.keyword("","require"),clojure.keyword("","use"),args_1]))}))))}).apply(null,[]);

//======
//(defn loaded-libs "Returns a sorted set of symbols naming the currently loaded libs" [] (clojure/deref *loaded-libs*))
//---
(function __clojure_fn_7638(){
return (clojure.JS.def(clojure,"loaded_libs",(function __clojure_fn_7638_loaded_libs_7640(){
return (clojure.deref.apply(null,[clojure._STAR_loaded_libs_STAR_]))})))}).apply(null,[]);

//======
//(defn load "Loads Clojure code from resources in classpath. A path is interpreted as\n  classpath-relative if it begins with a slash or relative to the root\n  directory for the current namespace otherwise." [& paths] (doseq path paths (let [path (if (.startsWith path "/") path (str (root-directory (ns-name *ns*)) \/ path))] (when *loading-verbosely* (printf "(clojure/load \"%s\")\n" path) (flush)) (throw-if (*pending-paths* path) "cannot load '%s' again while it is loading" path) (binding [*pending-paths* (conj *pending-paths* path)] (.loadResourceScript clojure.lang.RT (.substring path 1))))))
//---
(function __clojure_fn_7653(){
return (clojure.JS.def(clojure,"load",clojure.JS.variadic(0,(function __clojure_fn_7653_load_7655(){
var paths_1,list__781_2,path_3,path_4,paths_1=clojure.JS.rest_args(this,arguments,0);
return (((function __loop(){var _rtn,_cnt;(list__781_2=clojure.seq.apply(null,[paths_1]));do{_cnt=0;
_rtn=((list__781_2)?(((path_3=clojure.first.apply(null,[list__781_2])),
((path_4=(((path_3).startsWith("/"))?(path_3):(clojure.str.apply(null,[clojure.root_directory.apply(null,[clojure.ns_name.apply(null,[clojure._STAR_ns_STAR_])]),"/",path_3])))),
((clojure._STAR_loading_verbosely_STAR_)?(clojure.printf.apply(null,["(clojure/load \"%s\")\n",path_4]),
clojure.flush.apply(null,[])):(null)),
clojure.throw_if.apply(null,[clojure._STAR_pending_paths_STAR_.apply(null,[path_4]),"cannot load '%s' again while it is loading",path_4]),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_pending_paths_STAR_,clojure.conj.apply(null,[clojure._STAR_pending_paths_STAR_,path_4])])),
(function __clojure_fn_7653_load_7655_fn_7657(){
return ((function __try(){try{var _rtn=(clojure.lang.RT.loadResourceScript((path_4).substring(1)))}
finally{clojure.lang.Var.popThreadBindings()}})())}).apply(null,[]))),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__781_2])],list__781_2=_rtn[0])):(null))}while(_cnt);return _rtn;})()))}))))}).apply(null,[]);

//======
//(defn get-in "returns the value in a nested associative structure, where ks is a sequence of keys" [m ks] (reduce get m ks))
//---
(function __clojure_fn_7668(){
return (clojure.JS.def(clojure,"get_in",(function __clojure_fn_7668_get_in_7670(m_1,ks_2){
return (clojure.reduce.apply(null,[clojure.get,m_1,ks_2]))})))}).apply(null,[]);

//======
//(defn assoc-in "Associates a value in a nested associative structure, where ks is a\n  sequence of keys and v is the new value and returns a new nested structure.  \n  If any levels do not exist, hash-maps will be created." [m [k & ks] v] (if ks (assoc m k (assoc-in (get m k) ks v)) (assoc m k v)))
//---
(function __clojure_fn_7682(){
return (clojure.JS.def(clojure,"assoc_in",(function __clojure_fn_7682_assoc_in_7685(m_1,p__7684_2,v_3){
var k_5,vec__7687_4,ks_6;
return (((vec__7687_4=p__7684_2),
(k_5=clojure.nth.apply(null,[vec__7687_4,0,null])),
(ks_6=clojure.nthrest.apply(null,[vec__7687_4,1])),
((ks_6)?(clojure.assoc.apply(null,[m_1,k_5,clojure.assoc_in.apply(null,[clojure.get.apply(null,[m_1,k_5]),ks_6,v_3])])):(clojure.assoc.apply(null,[m_1,k_5,v_3])))))})))}).apply(null,[]);

//======
//(defn update-in "'Updates' a value in a nested associative structure, where ks is a\n  sequence of keys and f is a function that will take the old value\n  and return the new value, and returns a new nested structure.  \n  If any levels do not exist, hash-maps will be created." ([m [k & ks] f] (if ks (assoc m k (update-in (get m k) ks f)) (assoc m k (f (get m k))))))
//---
(function __clojure_fn_7698(){
return (clojure.JS.def(clojure,"update_in",(function __clojure_fn_7698_update_in_7701(m_1,p__7700_2,f_3){
var k_5,vec__7703_4,ks_6;
return (((vec__7703_4=p__7700_2),
(k_5=clojure.nth.apply(null,[vec__7703_4,0,null])),
(ks_6=clojure.nthrest.apply(null,[vec__7703_4,1])),
((ks_6)?(clojure.assoc.apply(null,[m_1,k_5,clojure.update_in.apply(null,[clojure.get.apply(null,[m_1,k_5]),ks_6,f_3])])):(clojure.assoc.apply(null,[m_1,k_5,f_3.apply(null,[clojure.get.apply(null,[m_1,k_5])])])))))})))}).apply(null,[]);

//======
//(defn empty? "Returns true if coll has no items - same as (not (seq coll)). \n  Please use the idiom (seq x) rather than (not (empty? x))" [coll] (not (seq coll)))
//---
(function __clojure_fn_7712(){
return (clojure.JS.def(clojure,"empty_QMARK_",(function __clojure_fn_7712_empty_QMARK_7714(coll_1){
return (clojure.not.apply(null,[clojure.seq.apply(null,[coll_1])]))})))}).apply(null,[]);

//======
//(defn coll? "Returns true if x implements IPersistentCollection" [x] (instance? clojure.lang.IPersistentCollection x))
//---
(function __clojure_fn_7724(){
return (clojure.JS.def(clojure,"coll_QMARK_",(function __clojure_fn_7724_coll_QMARK_7726(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentCollection,x_1]))})))}).apply(null,[]);

//======
//(defn list? "Returns true if x implements IPersistentList" [x] (instance? clojure.lang.IPersistentList x))
//---
(function __clojure_fn_7736(){
return (clojure.JS.def(clojure,"list_QMARK_",(function __clojure_fn_7736_list_QMARK_7738(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentList,x_1]))})))}).apply(null,[]);

//======
//(defn set? "Returns true if x implements IPersistentSet" [x] (instance? clojure.lang.IPersistentSet x))
//---
(function __clojure_fn_7748(){
return (clojure.JS.def(clojure,"set_QMARK_",(function __clojure_fn_7748_set_QMARK_7750(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentSet,x_1]))})))}).apply(null,[]);
// Skipping: (defn number? "Returns true if x is a Number" [x] (instance? Number x))

//======
//(defn fn? "Returns true if x implements IFn. Note that many data structures \n  (e.g. sets and maps) implement IFn" [x] (instance? clojure.lang.IFn x))
//---
(function __clojure_fn_7766(){
return (clojure.JS.def(clojure,"fn_QMARK_",(function __clojure_fn_7766_fn_QMARK_7768(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IFn,x_1]))})))}).apply(null,[]);
// Skipping: (defn integer? "Returns true if n is an integer" [n] (or (instance? Integer n) (instance? Long n) (instance? BigInteger n)))

//======
//(defn ratio? "Returns true if n is a Ratio" [n] (instance? clojure.lang.Ratio n))
//---
(function __clojure_fn_7784(){
return (clojure.JS.def(clojure,"ratio_QMARK_",(function __clojure_fn_7784_ratio_QMARK_7786(n_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Ratio,n_1]))})))}).apply(null,[]);
// Skipping: (defn decimal? "Returns true if n is a BigDecimal" [n] (instance? BigDecimal n))
// Skipping: (defn float? "Returns true if n is a floating point number" [n] (or (instance? Double n) (instance? Float n)))

//======
//(defn rational? [n] "Returns true if n is a rational number" (or (integer? n) (ratio? n) (decimal? n)))
//---
(function __clojure_fn_7808(){
return (clojure.JS.def(clojure,"rational_QMARK_",(function __clojure_fn_7808_rational_QMARK_7810(n_1){
var or__202_3,or__202_2;
return ("Returns true if n is a rational number",
((or__202_2=clojure.integer_QMARK_.apply(null,[n_1])),
((or__202_2)?(or__202_2):(((or__202_3=clojure.ratio_QMARK_.apply(null,[n_1])),
((or__202_3)?(or__202_3):(clojure.decimal_QMARK_.apply(null,[n_1]))))))))})))}).apply(null,[]);

//======
//(defn associative? "Returns true if coll implements Associative" [coll] (instance? clojure.lang.Associative coll))
//---
(function __clojure_fn_7820(){
return (clojure.JS.def(clojure,"associative_QMARK_",(function __clojure_fn_7820_associative_QMARK_7822(coll_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Associative,coll_1]))})))}).apply(null,[]);

//======
//(defn sequential? "Returns true if coll implements Sequential" [coll] (instance? clojure.lang.Sequential coll))
//---
(function __clojure_fn_7832(){
return (clojure.JS.def(clojure,"sequential_QMARK_",(function __clojure_fn_7832_sequential_QMARK_7834(coll_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Sequential,coll_1]))})))}).apply(null,[]);

//======
//(defn sorted? "Returns true if coll implements Sorted" [coll] (instance? clojure.lang.Sorted coll))
//---
(function __clojure_fn_7844(){
return (clojure.JS.def(clojure,"sorted_QMARK_",(function __clojure_fn_7844_sorted_QMARK_7846(coll_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Sorted,coll_1]))})))}).apply(null,[]);

//======
//(defn reversible? "Returns true if coll implements Reversible" [coll] (instance? clojure.lang.Reversible coll))
//---
(function __clojure_fn_7856(){
return (clojure.JS.def(clojure,"reversible_QMARK_",(function __clojure_fn_7856_reversible_QMARK_7858(coll_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Reversible,coll_1]))})))}).apply(null,[]);
// Skipping: (defn pmap "Like map, except f is applied in parallel. Semi-lazy in that the\n  parallel computation stays ahead of the consumption, but doesn't\n  realize the entire result unless required. Only useful for\n  computationally intensive functions where the time of f dominates\n  the coordination overhead." ([f coll] (let [n (inc (.. Runtime getRuntime availableProcessors)) agents (doall (map (fn* [p1__7862] (agent (f p1__7862))) (take n coll))) wget (fn [a] (await1 a) (clojure/deref a)) step (fn step [[x & xs :as s] [a & as :as acycle]] (if s (let [v (wget a)] (send a (fn [_] (f x))) (lazy-cons v (step xs as))) (map wget (take (count agents) acycle))))] (step (drop n coll) (cycle agents)))) ([f coll & colls] (let [step (fn step [cs] (when (every? seq cs) (lazy-cons (map first cs) (step (map rest cs)))))] (pmap (fn* [p1__7863] (apply f p1__7863)) (step (cons coll colls))))))

//======
//(def *1)
//---
(function __clojure_fn_7904(){
return (clojure.JS.def(clojure,"_STAR_1",null))}).apply(null,[]);

//======
//(def *2)
//---
(function __clojure_fn_7910(){
return (clojure.JS.def(clojure,"_STAR_2",null))}).apply(null,[]);

//======
//(def *3)
//---
(function __clojure_fn_7916(){
return (clojure.JS.def(clojure,"_STAR_3",null))}).apply(null,[]);

//======
//(def *e)
//---
(function __clojure_fn_7922(){
return (clojure.JS.def(clojure,"_STAR_e",null))}).apply(null,[]);

//======
//(import (quote (java.io Writer)))
//---
(function __clojure_fn_7928(){
return (clojure.import_.apply(null,[clojure.JS.lit_list(["'java.io","'Writer"])]))}).apply(null,[]);

//======
//(defn- print-sequential [begin print-one sep end sequence w] (.write w begin) (loop [s (seq sequence)] (if (rest s) (do (print-one (first s) w) (.write w sep) (recur (rest s))) (when s (print-one (first s) w)))) (.write w end))
//---
(function __clojure_fn_7937(){
return (clojure.JS.def(clojure,"print_sequential",(function __clojure_fn_7937_print_sequential_7939(begin_1,print_one_2,sep_3,end_4,sequence_5,w_6){
var s_7;
return ((w_6).write(begin_1),
((function __loop(){var _rtn,_cnt;(s_7=clojure.seq.apply(null,[sequence_5]));do{_cnt=0;
_rtn=((clojure.rest.apply(null,[s_7]))?(print_one_2.apply(null,[clojure.first.apply(null,[s_7]),w_6]),
(w_6).write(sep_3),
(_cnt=1,_rtn=[clojure.rest.apply(null,[s_7])],s_7=_rtn[0])):(((s_7)?(print_one_2.apply(null,[clojure.first.apply(null,[s_7]),w_6])):(null))))}while(_cnt);return _rtn;})()),
(w_6).write(end_4))})))}).apply(null,[]);

//======
//(defn- print-meta [o w] (when-let m (meta o) (when (and *print-meta* *print-readably* (pos? (count m))) (.write w "#^") (if (and (= (count m) 1) (:tag m)) (print-method (:tag m) w) (print-method m w)) (.write w " "))))
//---
(function __clojure_fn_7949(){
return (clojure.JS.def(clojure,"print_meta",(function __clojure_fn_7949_print_meta_7951(o_1,w_2){
var m_4,and__196_5,temp__1438_3,and__196_5,and__196_6;
return (((temp__1438_3=clojure.meta.apply(null,[o_1])),
((temp__1438_3)?(((m_4=temp__1438_3),
((((and__196_5=clojure._STAR_print_meta_STAR_),
((and__196_5)?(((and__196_6=clojure._STAR_print_readably_STAR_),
((and__196_6)?(clojure.lang.Numbers.isPos(clojure.count.apply(null,[m_4]))):(and__196_6)))):(and__196_5))))?((w_2).write("#^"),
((((and__196_5=clojure.lang.Util.equal(clojure.count.apply(null,[m_4]),1)),
((and__196_5)?(clojure.keyword("","tag").apply(null,[m_4])):(and__196_5))))?(clojure.print_method.apply(null,[clojure.keyword("","tag").apply(null,[m_4]),w_2])):(clojure.print_method.apply(null,[m_4,w_2]))),
(w_2).write(" ")):(null)))):(null))))})))}).apply(null,[]);

//======
//(defmethod print-method nil [o w] (.write w "nil"))
//---
(function __clojure_fn_7961(){
return ((clojure.print_method).addMethod(null,(function __clojure_fn_7961_fn_7963(o_1,w_2){
return ((w_2).write("nil"))})))}).apply(null,[]);

//======
//(defmethod print-method :default [o w] (print-meta o w) (.write w (str o)))
//---
(function __clojure_fn_7973(){
return ((clojure.print_method).addMethod(clojure.keyword("","default"),(function __clojure_fn_7973_fn_7975(o_1,w_2){
return (clojure.print_meta.apply(null,[o_1,w_2]),
(w_2).write(clojure.str.apply(null,[o_1])))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.ISeq [o w] (print-meta o w) (print-sequential "(" print-method " " ")" o w))
//---
(function __clojure_fn_7985(){
return ((clojure.print_method).addMethod(clojure.lang.ISeq,(function __clojure_fn_7985_fn_7987(o_1,w_2){
return (clojure.print_meta.apply(null,[o_1,w_2]),
clojure.print_sequential.apply(null,["(",clojure.print_method," ",")",o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentList [o w] (print-meta o w) (print-sequential "(" print-method " " ")" o w))
//---
(function __clojure_fn_7997(){
return ((clojure.print_method).addMethod(clojure.lang.IPersistentList,(function __clojure_fn_7997_fn_7999(o_1,w_2){
return (clojure.print_meta.apply(null,[o_1,w_2]),
clojure.print_sequential.apply(null,["(",clojure.print_method," ",")",o_1,w_2]))})))}).apply(null,[]);

//======
//(prefer-method print-method clojure.lang.IPersistentList clojure.lang.ISeq)
//---
(function __clojure_fn_8006(){
return ((clojure.print_method).preferMethod(clojure.lang.IPersistentList,clojure.lang.ISeq))}).apply(null,[]);

//======
//(def char-escape-string {\" "\\\"", \backspace "\\b", \tab "\\t", \newline "\\n", \formfeed "\\f", \return "\\r", \\ "\\\\"})
//---
(function __clojure_fn_8012(){
return (clojure.JS.def(clojure,"char_escape_string",clojure.hash_map("\"","\\\"","\b","\\b","\t","\\t","\n","\\n","\f","\\f","\r","\\r","\\","\\\\")))}).apply(null,[]);

//======
//(defmethod print-method String [s w] (if *print-readably* (do (.append w \") (dotimes n (count s) (let [c (.charAt s n) e (char-escape-string c)] (if e (.write w e) (.append w c)))) (.append w \")) (.write w s)) nil)
//---
(function __clojure_fn_8021(){
return ((clojure.print_method).addMethod(java.lang.String,(function __clojure_fn_8021_fn_8023(s_1,w_2){
var c_5,n_4,n__814_3,e_6;
return (((clojure._STAR_print_readably_STAR_)?((w_2).append("\""),
((n__814_3=clojure.lang.RT.intCast(clojure.count.apply(null,[s_1]))),
((function __loop(){var _rtn,_cnt;(n_4=clojure.lang.RT.intCast(0));do{_cnt=0;
_rtn=((clojure.lang.Numbers.lt(n_4,n__814_3))?(((c_5=(s_1).charAt(n_4)),
(e_6=clojure.char_escape_string.apply(null,[c_5])),
((e_6)?((w_2).write(e_6)):((w_2).append(c_5)))),
(_cnt=1,_rtn=[clojure.lang.Numbers.unchecked_inc(n_4)],n_4=_rtn[0])):(null))}while(_cnt);return _rtn;})())),
(w_2).append("\"")):((w_2).write(s_1))),
null)})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentVector [v w] (print-meta v w) (.append w \[) (dotimes n (count v) (print-method (nth v n) w) (when (< n (dec (count v))) (.append w \space))) (.append w \]) nil)
//---
(function __clojure_fn_8033(){
return ((clojure.print_method).addMethod(clojure.lang.IPersistentVector,(function __clojure_fn_8033_fn_8035(v_1,w_2){
var n_4,n__814_3;
return (clojure.print_meta.apply(null,[v_1,w_2]),
(w_2).append("["),
((n__814_3=clojure.lang.RT.intCast(clojure.count.apply(null,[v_1]))),
((function __loop(){var _rtn,_cnt;(n_4=clojure.lang.RT.intCast(0));do{_cnt=0;
_rtn=((clojure.lang.Numbers.lt(n_4,n__814_3))?(clojure.print_method.apply(null,[clojure.nth.apply(null,[v_1,n_4]),w_2]),
((clojure.lang.Numbers.lt(n_4,clojure.lang.Numbers.dec(clojure.count.apply(null,[v_1]))))?((w_2).append(" ")):(null)),
(_cnt=1,_rtn=[clojure.lang.Numbers.unchecked_inc(n_4)],n_4=_rtn[0])):(null))}while(_cnt);return _rtn;})())),
(w_2).append("]"),
null)})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentMap [m w] (print-meta m w) (print-sequential "{" (fn [e w] (do (print-method (key e) w) (.append w \space) (print-method (val e) w))) ", " "}" (seq m) w))
//---
(function __clojure_fn_8048(){
return ((clojure.print_method).addMethod(clojure.lang.IPersistentMap,(function __clojure_fn_8048_fn_8050(m_1,w_2){
return (clojure.print_meta.apply(null,[m_1,w_2]),
clojure.print_sequential.apply(null,["{",(function __clojure_fn_8048_fn_8050_fn_8052(e_1,w_2){
return (clojure.print_method.apply(null,[clojure.key.apply(null,[e_1]),w_2]),
(w_2).append(" "),
clojure.print_method.apply(null,[clojure.val.apply(null,[e_1]),w_2]))}),", ","}",clojure.seq.apply(null,[m_1]),w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentSet [s w] (print-meta s w) (print-sequential "#{" print-method " " "}" (seq s) w))
//---
(function __clojure_fn_8063(){
return ((clojure.print_method).addMethod(clojure.lang.IPersistentSet,(function __clojure_fn_8063_fn_8065(s_1,w_2){
return (clojure.print_meta.apply(null,[s_1,w_2]),
clojure.print_sequential.apply(null,["#{",clojure.print_method," ","}",clojure.seq.apply(null,[s_1]),w_2]))})))}).apply(null,[]);

//======
//(def char-name-string {\space "space", \backspace "backspace", \tab "tab", \newline "newline", \formfeed "formfeed", \return "return"})
//---
(function __clojure_fn_8072(){
return (clojure.JS.def(clojure,"char_name_string",clojure.hash_map(" ","space","\b","backspace","\t","tab","\n","newline","\f","formfeed","\r","return")))}).apply(null,[]);

//======
//(defmethod print-method java.lang.Character [c w] (if *print-readably* (do (.append w \\) (let [n (char-name-string c)] (if n (.write w n) (.append w c)))) (.append w c)) nil)
//---
(function __clojure_fn_8081(){
return ((clojure.print_method).addMethod(java.lang.Character,(function __clojure_fn_8081_fn_8083(c_1,w_2){
var n_3;
return (((clojure._STAR_print_readably_STAR_)?((w_2).append("\\"),
((n_3=clojure.char_name_string.apply(null,[c_1])),
((n_3)?((w_2).write(n_3)):((w_2).append(c_1))))):((w_2).append(c_1))),
null)})))}).apply(null,[]);
// Skipping: (defmethod print-method Class [c w] (.write w (.getName c)))

//======
//(defmethod print-method java.math.BigDecimal [b w] (.write w (str b)) (.write w "M"))
//---
(function __clojure_fn_8099(){
return ((clojure.print_method).addMethod(java.math.BigDecimal,(function __clojure_fn_8099_fn_8101(b_1,w_2){
return ((w_2).write(clojure.str.apply(null,[b_1])),
(w_2).write("M"))})))}).apply(null,[]);

//======
//(defmethod print-method java.util.regex.Pattern [p w] (.append w \#) (print-method (str p) w))
//---
(function __clojure_fn_8111(){
return ((clojure.print_method).addMethod(java.util.regex.Pattern,(function __clojure_fn_8111_fn_8113(p_1,w_2){
return ((w_2).append("#"),
clojure.print_method.apply(null,[clojure.str.apply(null,[p_1]),w_2]))})))}).apply(null,[]);
