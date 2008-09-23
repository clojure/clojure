
//======
//(in-ns (quote clojure))
//---
(function __tmp_fn_2581(){
return (clojure.in_ns("'clojure"))})();

//======
//(def list (. clojure.lang.PersistentList creator))
//---
(function __clojure_fn_2590(){
return (clojure.JS.def(clojure,"list",clojure.lang.PersistentList.creator))})();

//======
//(def cons (fn* cons [x seq] (. clojure.lang.RT (cons x seq))))
//---
(function __clojure_fn_2598(){
return (clojure.JS.def(clojure,"cons",(function __clojure_fn_2598_cons_2600(x_1,seq_2){
var cons_0=arguments.callee;
return (clojure.lang.RT.cons(x_1,seq_2))})))})();

//======
//(def conj (fn conj ([coll x] (. clojure.lang.RT (conj coll x))) ([coll x & xs] (if xs (recur (conj coll x) (first xs) (rest xs)) (conj coll x)))))
//---
(function __clojure_fn_2633(){
return (clojure.JS.def(clojure,"conj",clojure.JS.variatic(2,(function __clojure_fn_2633_conj_2635(coll_1,x_2){switch(arguments.length){
case 2:var conj_0=arguments.callee;
return (clojure.lang.RT.conj(coll_1,x_2))}
var _cnt,_rtn,xs_3,conj_0=arguments.callee,xs_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((xs_3)?((_cnt=1,_t0=conj_0(coll_1,x_2),_t1=clojure.first(xs_3),_t2=clojure.rest(xs_3),coll_1=_t0,x_2=_t1,xs_3=_t2)):(conj_0(coll_1,x_2)))
}while(_cnt);return _rtn;}))))})();

//======
//(def second (fn second [x] (first (rest x))))
//---
(function __clojure_fn_2643(){
return (clojure.JS.def(clojure,"second",(function __clojure_fn_2643_second_2645(x_1){
var second_0=arguments.callee;
return (clojure.first(clojure.rest(x_1)))})))})();

//======
//(def ffirst (fn ffirst [x] (first (first x))))
//---
(function __clojure_fn_2653(){
return (clojure.JS.def(clojure,"ffirst",(function __clojure_fn_2653_ffirst_2655(x_1){
var ffirst_0=arguments.callee;
return (clojure.first(clojure.first(x_1)))})))})();

//======
//(def rfirst (fn rfirst [x] (rest (first x))))
//---
(function __clojure_fn_2663(){
return (clojure.JS.def(clojure,"rfirst",(function __clojure_fn_2663_rfirst_2665(x_1){
var rfirst_0=arguments.callee;
return (clojure.rest(clojure.first(x_1)))})))})();

//======
//(def frest (fn frest [x] (first (rest x))))
//---
(function __clojure_fn_2673(){
return (clojure.JS.def(clojure,"frest",(function __clojure_fn_2673_frest_2675(x_1){
var frest_0=arguments.callee;
return (clojure.first(clojure.rest(x_1)))})))})();

//======
//(def rrest (fn rrest [x] (rest (rest x))))
//---
(function __clojure_fn_2683(){
return (clojure.JS.def(clojure,"rrest",(function __clojure_fn_2683_rrest_2685(x_1){
var rrest_0=arguments.callee;
return (clojure.rest(clojure.rest(x_1)))})))})();

//======
//(def seq? (fn seq? [x] (instance? clojure.lang.ISeq x)))
//---
(function __clojure_fn_2703(){
return (clojure.JS.def(clojure,"seq_QMARK_",(function __clojure_fn_2703_seq_QMARK_2705(x_1){
var seq_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_(clojure.lang.ISeq,x_1))})))})();

//======
//(def string? (fn string? [x] (instance? String x)))
//---
(function __clojure_fn_2713(){
return (clojure.JS.def(clojure,"string_QMARK_",(function __clojure_fn_2713_string_QMARK_2715(x_1){
var string_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_(java.lang.String,x_1))})))})();

//======
//(def map? (fn map? [x] (instance? clojure.lang.IPersistentMap x)))
//---
(function __clojure_fn_2723(){
return (clojure.JS.def(clojure,"map_QMARK_",(function __clojure_fn_2723_map_QMARK_2725(x_1){
var map_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_(clojure.lang.IPersistentMap,x_1))})))})();

//======
//(def vector? (fn vector? [x] (instance? clojure.lang.IPersistentVector x)))
//---
(function __clojure_fn_2733(){
return (clojure.JS.def(clojure,"vector_QMARK_",(function __clojure_fn_2733_vector_QMARK_2735(x_1){
var vector_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_(clojure.lang.IPersistentVector,x_1))})))})();

//======
//(def sigs (fn [fdecl] (if (seq? (first fdecl)) (loop [ret [] fdecl fdecl] (if fdecl (recur (conj ret (first (first fdecl))) (rest fdecl)) (seq ret))) (list (first fdecl)))))
//---
(function __clojure_fn_2744(){
return (clojure.JS.def(clojure,"sigs",(function __clojure_fn_2744_sigs_2746(fdecl_1){
var ret_2,fdecl_3;
return (((clojure.seq_QMARK_(clojure.first(fdecl_1)))?(((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentVector.EMPTY),
(fdecl_3=fdecl_1);do{_cnt=0;
_rtn=((fdecl_3)?((_cnt=1,_t0=clojure.conj(ret_2,clojure.first(clojure.first(fdecl_3))),_t1=clojure.rest(fdecl_3),ret_2=_t0,fdecl_3=_t1)):(clojure.seq(ret_2)))}while(_cnt);return _rtn;})())):(clojure.list(clojure.first(fdecl_1)))))})))})();

//======
//(def meta (fn meta [x] (if (instance? clojure.lang.IObj x) (. x (meta)))))
//---
(function __clojure_fn_2760(){
return (clojure.JS.def(clojure,"meta",(function __clojure_fn_2760_meta_2762(x_1){
var meta_0=arguments.callee;
return (((clojure.instance_QMARK_(clojure.lang.IObj,x_1))?((x_1).meta()):(null)))})))})();

//======
//(def with-meta (fn with-meta [x m] (. x (withMeta m))))
//---
(function __clojure_fn_2770(){
return (clojure.JS.def(clojure,"with_meta",(function __clojure_fn_2770_with_meta_2772(x_1,m_2){
var with_meta_0=arguments.callee;
return ((x_1).withMeta(m_2))})))})();

//======
//(def last (fn last [s] (if (rest s) (recur (rest s)) (first s))))
//---
(function __clojure_fn_2780(){
return (clojure.JS.def(clojure,"last",(function __clojure_fn_2780_last_2782(s_1){
var _cnt,_rtn,last_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.rest(s_1))?((_cnt=1,_t0=clojure.rest(s_1),s_1=_t0)):(clojure.first(s_1)))
}while(_cnt);return _rtn;})))})();

//======
//(def butlast (fn butlast [s] (loop [ret [] s s] (if (rest s) (recur (conj ret (first s)) (rest s)) (seq ret)))))
//---
(function __clojure_fn_2790(){
return (clojure.JS.def(clojure,"butlast",(function __clojure_fn_2790_butlast_2792(s_1){
var ret_2,s_3,butlast_0=arguments.callee;
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentVector.EMPTY),
(s_3=s_1);do{_cnt=0;
_rtn=((clojure.rest(s_3))?((_cnt=1,_t0=clojure.conj(ret_2,clojure.first(s_3)),_t1=clojure.rest(s_3),ret_2=_t0,s_3=_t1)):(clojure.seq(ret_2)))}while(_cnt);return _rtn;})()))})))})();

//======
//(defn cast "Throws a ClassCastException if x is not a c, else returns x." [c x] (. c (cast x)))
//---
(function __clojure_fn_2809(){
return (clojure.JS.def(clojure,"cast",(function __clojure_fn_2809_cast_2811(c_1,x_2){
return ((c_1).cast(x_2))})))})();

//======
//(defn to-array "Returns an array of Objects containing the contents of coll, which\n  can be any Collection.  Maps to java.util.Collection.toArray()." [coll] (. clojure.lang.RT (toArray coll)))
//---
(function __clojure_fn_2821(){
return (clojure.JS.def(clojure,"to_array",(function __clojure_fn_2821_to_array_2823(coll_1){
return (clojure.lang.RT.toArray(coll_1))})))})();

//======
//(defn vector "Creates a new vector containing the args." ([] []) ([& args] (. clojure.lang.LazilyPersistentVector (create args))))
//---
(function __clojure_fn_2834(){
return (clojure.JS.def(clojure,"vector",clojure.JS.variatic(0,(function __clojure_fn_2834_vector_2836(){switch(arguments.length){
case 0:return (clojure.lang.PersistentVector.EMPTY)}
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.LazilyPersistentVector.create(args_1))}))))})();

//======
//(defn vec "Creates a new vector containing the contents of coll." ([coll] (. clojure.lang.LazilyPersistentVector (createOwning (to-array coll)))))
//---
(function __clojure_fn_2847(){
return (clojure.JS.def(clojure,"vec",(function __clojure_fn_2847_vec_2849(coll_1){
return (clojure.lang.LazilyPersistentVector.createOwning(clojure.to_array(coll_1)))})))})();

//======
//(defn hash-set "Returns a new hash set with supplied keys." ([] #{}) ([& keys] (. clojure.lang.PersistentHashSet (create keys))))
//---
(function __clojure_fn_2867(){
return (clojure.JS.def(clojure,"hash_set",clojure.JS.variatic(0,(function __clojure_fn_2867_hash_set_2869(){switch(arguments.length){
case 0:return (clojure.lang.PersistentHashSet.EMPTY)}
var keys_1,keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentHashSet.create(keys_1))}))))})();

//======
//(defn sorted-map "keyval => key val\n  Returns a new sorted map with supplied mappings." ([& keyvals] (. clojure.lang.PersistentTreeMap (create keyvals))))
//---
(function __clojure_fn_2880(){
return (clojure.JS.def(clojure,"sorted_map",clojure.JS.variatic(0,(function __clojure_fn_2880_sorted_map_2882(){
var keyvals_1,keyvals_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentTreeMap.create(keyvals_1))}))))})();

//======
//(defn sorted-set "Returns a new sorted set with supplied keys." ([& keys] (. clojure.lang.PersistentTreeSet (create keys))))
//---
(function __clojure_fn_2892(){
return (clojure.JS.def(clojure,"sorted_set",clojure.JS.variatic(0,(function __clojure_fn_2892_sorted_set_2894(){
var keys_1,keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentTreeSet.create(keys_1))}))))})();

//======
//(defn sorted-map-by "keyval => key val\n  Returns a new sorted map with supplied mappings, using the supplied comparator." ([comparator & keyvals] (. clojure.lang.PersistentTreeMap (create comparator keyvals))))
//---
(function __clojure_fn_2904(){
return (clojure.JS.def(clojure,"sorted_map_by",clojure.JS.variatic(1,(function __clojure_fn_2904_sorted_map_by_2906(comparator_1){
var keyvals_2,keyvals_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentTreeMap.create(comparator_1,keyvals_2))}))))})();

//======
//(defn nil? "Returns true if x is nil, false otherwise." {:tag Boolean} [x] (identical? x nil))
//---
(function __clojure_fn_2937(){
return (clojure.JS.def(clojure,"nil_QMARK_",(function __clojure_fn_2937_nil_QMARK_2939(x_1){
return (clojure.identical_QMARK_(x_1,null))})))})();

//======
//(defn false? "Returns true if x is the value false, false otherwise." {:tag Boolean} [x] (identical? x false))
//---
(function __clojure_fn_2949(){
return (clojure.JS.def(clojure,"false_QMARK_",(function __clojure_fn_2949_false_QMARK_2951(x_1){
return (clojure.identical_QMARK_(x_1,false))})))})();

//======
//(defn true? "Returns true if x is the value true, false otherwise." {:tag Boolean} [x] (identical? x true))
//---
(function __clojure_fn_2961(){
return (clojure.JS.def(clojure,"true_QMARK_",(function __clojure_fn_2961_true_QMARK_2963(x_1){
return (clojure.identical_QMARK_(x_1,true))})))})();

//======
//(defn not "Returns true if x is logical false, false otherwise." {:tag Boolean} [x] (if x false true))
//---
(function __clojure_fn_2973(){
return (clojure.JS.def(clojure,"not",(function __clojure_fn_2973_not_2975(x_1){
return (((x_1)?(false):(true)))})))})();

//======
//(defn str "With no args, returns the empty string. With one arg x, returns\n  x.toString().  (str nil) returns the empty string. With more than\n  one arg, returns the concatenation of the str values of the args." {:tag String} ([] "") ([x] (if (nil? x) "" (. x (toString)))) ([x & ys] (loop [sb (new StringBuilder (str x)) more ys] (if more (recur (. sb (append (str (first more)))) (rest more)) (str sb)))))
//---
(function __clojure_fn_2987(){
return (clojure.JS.def(clojure,"str",clojure.JS.variatic(1,(function __clojure_fn_2987_str_2989(x_1){switch(arguments.length){
case 1:return (((clojure.nil_QMARK_(x_1))?(""):((x_1).toString())))
case 0:return ("")}
var ys_2,sb_3,more_4,ys_2=clojure.JS.rest_args(this,arguments,1);
return (((function __loop(){var _rtn,_cnt;(sb_3=(new java.lang.StringBuilder(clojure.str(x_1)))),
(more_4=ys_2);do{_cnt=0;
_rtn=((more_4)?((_cnt=1,_t0=(sb_3).append(clojure.str(clojure.first(more_4))),_t1=clojure.rest(more_4),sb_3=_t0,more_4=_t1)):(clojure.str(sb_3)))}while(_cnt);return _rtn;})()))}))))})();

//======
//(defn symbol "Returns a Symbol with the given namespace and name." ([name] (. clojure.lang.Symbol (intern name))) ([ns name] (. clojure.lang.Symbol (intern ns name))))
//---
(function __clojure_fn_3002(){
return (clojure.JS.def(clojure,"symbol",(function __clojure_fn_3002_symbol_3004(ns_1,name_2){switch(arguments.length){
case 1:var name_1=arguments[0];
return (clojure.lang.Symbol.intern(name_1))}
return (clojure.lang.Symbol.intern(ns_1,name_2))})))})();

//======
//(defn keyword "Returns a Keyword with the given namespace and name.  Do not use :\n  in the keyword strings, it will be added automatically." ([name] (. clojure.lang.Keyword (intern nil name))) ([ns name] (. clojure.lang.Keyword (intern ns name))))
//---
(function __clojure_fn_3016(){
return (clojure.JS.def(clojure,"keyword",(function __clojure_fn_3016_keyword_3018(ns_1,name_2){switch(arguments.length){
case 1:var name_1=arguments[0];
return (clojure.lang.Keyword.intern(null,name_1))}
return (clojure.lang.Keyword.intern(ns_1,name_2))})))})();

//======
//(defn gensym "Returns a new symbol with a unique name. If a prefix string is\n  supplied, the name is prefix# where # is some unique number. If\n  prefix is not supplied, the prefix is 'G'." ([] (gensym "G__")) ([prefix-string] (. clojure.lang.Symbol (intern (str prefix-string (str (. clojure.lang.RT (nextID))))))))
//---
(function __clojure_fn_3030(){
return (clojure.JS.def(clojure,"gensym",(function __clojure_fn_3030_gensym_3032(prefix_string_1){switch(arguments.length){
case 0:return (clojure.gensym("G__"))}
return (clojure.lang.Symbol.intern(clojure.str(prefix_string_1,clojure.str(clojure.lang.RT.nextID()))))})))})();

//======
//(defn spread {:private true} [arglist] (cond (nil? arglist) nil (nil? (rest arglist)) (seq (first arglist)) :else (cons (first arglist) (spread (rest arglist)))))
//---
(function __clojure_fn_3049(){
return (clojure.JS.def(clojure,"spread",(function __clojure_fn_3049_spread_3051(arglist_1){
return (((clojure.nil_QMARK_(arglist_1))?(null):(((clojure.nil_QMARK_(clojure.rest(arglist_1)))?(clojure.seq(clojure.first(arglist_1))):(((":else")?(clojure.cons(clojure.first(arglist_1),clojure.spread(clojure.rest(arglist_1)))):(null)))))))})))})();

//======
//(defn list* "Creates a new list containing the item prepended to more." [item & more] (spread (cons item more)))
//---
(function __clojure_fn_3067(){
return (clojure.JS.def(clojure,"list_STAR_",clojure.JS.variatic(1,(function __clojure_fn_3067_list_STAR_3069(item_1){
var more_2,more_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.spread(clojure.cons(item_1,more_2)))}))))})();

//======
//(defn delay? "returns true if x is a Delay created with delay" [x] (instance? clojure.lang.Delay x))
//---
(function __clojure_fn_3085(){
return (clojure.JS.def(clojure,"delay_QMARK_",(function __clojure_fn_3085_delay_QMARK_3087(x_1){
return (clojure.instance_QMARK_(clojure.lang.Delay,x_1))})))})();

//======
//(defn force "If x is a Delay, returns the (possibly cached) value of its expression, else returns x" [x] (. clojure.lang.Delay (force x)))
//---
(function __clojure_fn_3097(){
return (clojure.JS.def(clojure,"force",(function __clojure_fn_3097_force_3099(x_1){
return (clojure.lang.Delay.force(x_1))})))})();

//======
//(defn fnseq "Returns a seq object whose first is first and whose rest is the\n  value produced by calling restfn with no arguments. restfn will be\n  called at most once per step in the sequence, e.g. calling rest\n  repeatedly on the head of the seq calls restfn once - the value it\n  yields is cached." [first restfn] (new clojure.lang.FnSeq first restfn))
//---
(function __clojure_fn_3109(){
return (clojure.JS.def(clojure,"fnseq",(function __clojure_fn_3109_fnseq_3111(first_1,restfn_2){
return ((new clojure.lang.FnSeq(first_1,restfn_2)))})))})();

//======
//(defn cache-seq "Given a seq s, returns a lazy seq that will touch each element of s\n  at most once, caching the results." [s] (when s (clojure.lang.CachedSeq. s)))
//---
(function __clojure_fn_3127(){
return (clojure.JS.def(clojure,"cache_seq",(function __clojure_fn_3127_cache_seq_3129(s_1){
return (((s_1)?((new clojure.lang.CachedSeq(s_1))):(null)))})))})();

//======
//(defn concat "Returns a lazy seq representing the concatenation of\tthe elements in the supplied colls." ([] nil) ([x] (seq x)) ([x y] (if (seq x) (lazy-cons (first x) (concat (rest x) y)) (seq y))) ([x y & zs] (let [cat (fn cat [xys zs] (if (seq xys) (lazy-cons (first xys) (cat (rest xys) zs)) (when zs (recur (first zs) (rest zs)))))] (cat (concat x y) zs))))
//---
(function __clojure_fn_3154(){
return (clojure.JS.def(clojure,"concat",clojure.JS.variatic(2,(function __clojure_fn_3154_concat_3156(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.seq(x_1))?((new clojure.lang.LazyCons((function __clojure_fn_3154_concat_3156_fn_3161(G__3160_1){switch(arguments.length){
case 0:return (clojure.first(x_1))}
return (clojure.concat(clojure.rest(x_1),y_2))})))):(clojure.seq(y_2))))
case 1:return (clojure.seq(x_1))
case 0:return (null)}
var zs_3,cat_4,zs_3=clojure.JS.rest_args(this,arguments,2);
return (((cat_4=(function __clojure_fn_3154_concat_3156_cat_3166(xys_1,zs_2){
var _cnt,_rtn,cat_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.seq(xys_1))?((new clojure.lang.LazyCons((function __clojure_fn_3154_concat_3156_cat_3166_fn_3168(G__3167_1){switch(arguments.length){
case 0:return (clojure.first(xys_1))}
return (cat_0(clojure.rest(xys_1),zs_2))})))):(((zs_2)?((_cnt=1,_t0=clojure.first(zs_2),_t1=clojure.rest(zs_2),xys_1=_t0,zs_2=_t1)):(null))))
}while(_cnt);return _rtn;})),
cat_4(clojure.concat(x_1,y_2),zs_3)))}))))})();

//======
//(defn = "Equality. Returns true if x equals y, false if not. Same as\n  Java x.equals(y) except it also works for nil, and compares\n  numbers in a type-independent manner.  Clojure's immutable data\n  structures define equals() (and thus =) as a value, not an identity,\n  comparison." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Util)) (clojure/list (quote clojure/equal)) (clojure/list x) (clojure/list y))), :inline-arities #{2}, :tag Boolean} ([x] true) ([x y] (. clojure.lang.Util (equal x y))) ([x y & more] (if (= x y) (if (rest more) (recur y (first more) (rest more)) (= y (first more))) false)))
//---
(function __clojure_fn_3186(){
return (clojure.JS.def(clojure,"_EQ_",clojure.JS.variatic(2,(function __clojure_fn_3186_EQ_3191(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Util.equal(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Util.equal(x_1,y_2))?(((clojure.rest(more_3))?((_cnt=1,_t0=y_2,_t1=clojure.first(more_3),_t2=clojure.rest(more_3),x_1=_t0,y_2=_t1,more_3=_t2)):(clojure.lang.Util.equal(y_2,clojure.first(more_3))))):(false))
}while(_cnt);return _rtn;}))))})();

//======
//(defn not= "Same as (not (= obj1 obj2))" {:tag Boolean} ([x] false) ([x y] (not (= x y))) ([x y & more] (not (apply = x y more))))
//---
(function __clojure_fn_3205(){
return (clojure.JS.def(clojure,"not_EQ_",clojure.JS.variatic(2,(function __clojure_fn_3205_not_EQ_3207(x_1,y_2){switch(arguments.length){
case 2:return (clojure.not(clojure.lang.Util.equal(x_1,y_2)))
case 1:return (false)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.not(clojure.apply(clojure._EQ_,x_1,y_2,more_3)))}))))})();

//======
//(defn compare "Comparator. Returns 0 if x equals y, -1 if x is logically 'less\n  than' y, else 1. Same as Java x.compareTo(y) except it also works\n  for nil, and compares numbers in a type-independent manner. x must\n  implement Comparable" {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Util)) (clojure/list (quote clojure/compare)) (clojure/list x) (clojure/list y))), :tag Integer} [x y] (. clojure.lang.Util (compare x y)))
//---
(function __clojure_fn_3222(){
return (clojure.JS.def(clojure,"compare",(function __clojure_fn_3222_compare_3227(x_1,y_2){
return (clojure.lang.Util.compare(x_1,y_2))})))})();

//======
//(defn reduce "f should be a function of 2 arguments. If val is not supplied,\n  returns the result of applying f to the first 2 items in coll, then\n  applying f to that result and the 3rd item, etc. If coll contains no\n  items, f must accept no arguments as well, and reduce returns the\n  result of calling f with no arguments.  If coll has only 1 item, it\n  is returned and f is not called.  If val is supplied, returns the\n  result of applying f to val and the first item in coll, then\n  applying f to that result and the 2nd item, etc. If coll contains no\n  items, returns val and f is not called." ([f coll] (let [s (seq coll)] (if s (if (instance? clojure.lang.IReduce s) (. s (reduce f)) (reduce f (first s) (rest s))) (f)))) ([f val coll] (let [s (seq coll)] (if (instance? clojure.lang.IReduce s) (. s (reduce f val)) ((fn [f val s] (if s (recur f (f val (first s)) (rest s)) val)) f val s)))))
//---
(function __clojure_fn_3259(){
return (clojure.JS.def(clojure,"reduce",(function __clojure_fn_3259_reduce_3261(f_1,val_2,coll_3){switch(arguments.length){
case 2:var s_3,coll_2=arguments[1];
return (((s_3=clojure.seq(coll_2)),
((s_3)?(((clojure.instance_QMARK_(clojure.lang.IReduce,s_3))?((s_3).reduce(f_1)):(clojure.reduce(f_1,clojure.first(s_3),clojure.rest(s_3))))):(f_1()))))}
var s_4;
return (((s_4=clojure.seq(coll_3)),
((clojure.instance_QMARK_(clojure.lang.IReduce,s_4))?((s_4).reduce(f_1,val_2)):((function __clojure_fn_3259_reduce_3261_fn_3264(f_1,val_2,s_3){
var _cnt,_rtn;
do{_cnt=0;_rtn=((s_3)?((_cnt=1,_t0=f_1,_t1=f_1(val_2,clojure.first(s_3)),_t2=clojure.rest(s_3),f_1=_t0,val_2=_t1,s_3=_t2)):(val_2))
}while(_cnt);return _rtn;})(f_1,val_2,s_4)))))})))})();

//======
//(defn reverse "Returns a seq of the items in coll in reverse order. Not lazy." [coll] (reduce conj nil coll))
//---
(function __clojure_fn_3275(){
return (clojure.JS.def(clojure,"reverse",(function __clojure_fn_3275_reverse_3277(coll_1){
return (clojure.reduce(clojure.conj,null,coll_1))})))})();

//======
//(defn + "Returns the sum of nums. (+) returns 0." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/add)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([] 0) ([x] (cast Number x)) ([x y] (. clojure.lang.Numbers (add x y))) ([x y & more] (reduce + (+ x y) more)))
//---
(function __clojure_fn_3293(){
return (clojure.JS.def(clojure,"_PLUS_",clojure.JS.variatic(2,(function __clojure_fn_3293_PLUS_3298(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.add(x_1,y_2))
case 1:return (clojure.cast(java.lang.Number,x_1))
case 0:return (0)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce(clojure._PLUS_,clojure.lang.Numbers.add(x_1,y_2),more_3))}))))})();

//======
//(defn * "Returns the product of nums. (*) returns 1." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/multiply)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([] 1) ([x] (cast Number x)) ([x y] (. clojure.lang.Numbers (multiply x y))) ([x y & more] (reduce * (* x y) more)))
//---
(function __clojure_fn_3317(){
return (clojure.JS.def(clojure,"_STAR_",clojure.JS.variatic(2,(function __clojure_fn_3317_STAR_3322(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.multiply(x_1,y_2))
case 1:return (clojure.cast(java.lang.Number,x_1))
case 0:return (1)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce(clojure._STAR_,clojure.lang.Numbers.multiply(x_1,y_2),more_3))}))))})();

//======
//(defn / "If no denominators are supplied, returns 1/numerator,\n  else returns numerator divided by all of the denominators." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/divide)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] (/ 1 x)) ([x y] (. clojure.lang.Numbers (divide x y))) ([x y & more] (reduce / (/ x y) more)))
//---
(function __clojure_fn_3340(){
return (clojure.JS.def(clojure,"_SLASH_",clojure.JS.variatic(2,(function __clojure_fn_3340_SLASH_3345(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.divide(x_1,y_2))
case 1:return (clojure.lang.Numbers.divide(1,x_1))}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce(clojure._SLASH_,clojure.lang.Numbers.divide(x_1,y_2),more_3))}))))})();

//======
//(defn - "If no ys are supplied, returns the negation of x, else subtracts\n  the ys from x and returns the result." {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/minus)) args)))), :inline-arities #{1 2}} ([x] (. clojure.lang.Numbers (minus x))) ([x y] (. clojure.lang.Numbers (minus x y))) ([x y & more] (reduce - (- x y) more)))
//---
(function __clojure_fn_3362(){
return (clojure.JS.def(clojure,"_",clojure.JS.variatic(2,(function __clojure_fn_3362_3367(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.minus(x_1,y_2))
case 1:return (clojure.lang.Numbers.minus(x_1))}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce(clojure._,clojure.lang.Numbers.minus(x_1,y_2),more_3))}))))})();

//======
//(defn < "Returns non-nil if nums are in monotonically increasing order,\n  otherwise false." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/lt)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (lt x y))) ([x y & more] (if (< x y) (if (rest more) (recur y (first more) (rest more)) (< y (first more))) false)))
//---
(function __clojure_fn_3384(){
return (clojure.JS.def(clojure,"_LT_",clojure.JS.variatic(2,(function __clojure_fn_3384_LT_3389(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.lt(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.lt(x_1,y_2))?(((clojure.rest(more_3))?((_cnt=1,_t0=y_2,_t1=clojure.first(more_3),_t2=clojure.rest(more_3),x_1=_t0,y_2=_t1,more_3=_t2)):(clojure.lang.Numbers.lt(y_2,clojure.first(more_3))))):(false))
}while(_cnt);return _rtn;}))))})();

//======
//(defn <= "Returns non-nil if nums are in monotonically non-decreasing order,\n  otherwise false." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/lte)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (lte x y))) ([x y & more] (if (<= x y) (if (rest more) (recur y (first more) (rest more)) (<= y (first more))) false)))
//---
(function __clojure_fn_3406(){
return (clojure.JS.def(clojure,"_LT__EQ_",clojure.JS.variatic(2,(function __clojure_fn_3406_LT_EQ_3411(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.lte(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.lte(x_1,y_2))?(((clojure.rest(more_3))?((_cnt=1,_t0=y_2,_t1=clojure.first(more_3),_t2=clojure.rest(more_3),x_1=_t0,y_2=_t1,more_3=_t2)):(clojure.lang.Numbers.lte(y_2,clojure.first(more_3))))):(false))
}while(_cnt);return _rtn;}))))})();

//======
//(defn > "Returns non-nil if nums are in monotonically decreasing order,\n  otherwise false." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/gt)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (gt x y))) ([x y & more] (if (> x y) (if (rest more) (recur y (first more) (rest more)) (> y (first more))) false)))
//---
(function __clojure_fn_3428(){
return (clojure.JS.def(clojure,"_GT_",clojure.JS.variatic(2,(function __clojure_fn_3428_GT_3433(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.gt(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.gt(x_1,y_2))?(((clojure.rest(more_3))?((_cnt=1,_t0=y_2,_t1=clojure.first(more_3),_t2=clojure.rest(more_3),x_1=_t0,y_2=_t1,more_3=_t2)):(clojure.lang.Numbers.gt(y_2,clojure.first(more_3))))):(false))
}while(_cnt);return _rtn;}))))})();

//======
//(defn >= "Returns non-nil if nums are in monotonically non-increasing order,\n  otherwise false." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/gte)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (gte x y))) ([x y & more] (if (>= x y) (if (rest more) (recur y (first more) (rest more)) (>= y (first more))) false)))
//---
(function __clojure_fn_3450(){
return (clojure.JS.def(clojure,"_GT__EQ_",clojure.JS.variatic(2,(function __clojure_fn_3450_GT_EQ_3455(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.gte(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.gte(x_1,y_2))?(((clojure.rest(more_3))?((_cnt=1,_t0=y_2,_t1=clojure.first(more_3),_t2=clojure.rest(more_3),x_1=_t0,y_2=_t1,more_3=_t2)):(clojure.lang.Numbers.gte(y_2,clojure.first(more_3))))):(false))
}while(_cnt);return _rtn;}))))})();

//======
//(defn == "Returns non-nil if nums all have the same value, otherwise false" {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/equiv)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (equiv x y))) ([x y & more] (if (== x y) (if (rest more) (recur y (first more) (rest more)) (== y (first more))) false)))
//---
(function __clojure_fn_3472(){
return (clojure.JS.def(clojure,"_EQ__EQ_",clojure.JS.variatic(2,(function __clojure_fn_3472_EQ_EQ_3477(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.equiv(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.equiv(x_1,y_2))?(((clojure.rest(more_3))?((_cnt=1,_t0=y_2,_t1=clojure.first(more_3),_t2=clojure.rest(more_3),x_1=_t0,y_2=_t1,more_3=_t2)):(clojure.lang.Numbers.equiv(y_2,clojure.first(more_3))))):(false))
}while(_cnt);return _rtn;}))))})();

//======
//(defn max "Returns the greatest of the nums." ([x] x) ([x y] (if (> x y) x y)) ([x y & more] (reduce max (max x y) more)))
//---
(function __clojure_fn_3491(){
return (clojure.JS.def(clojure,"max",clojure.JS.variatic(2,(function __clojure_fn_3491_max_3493(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.lang.Numbers.gt(x_1,y_2))?(x_1):(y_2)))
case 1:return (x_1)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce(clojure.max,clojure.max(x_1,y_2),more_3))}))))})();

//======
//(defn min "Returns the least of the nums." ([x] x) ([x y] (if (< x y) x y)) ([x y & more] (reduce min (min x y) more)))
//---
(function __clojure_fn_3507(){
return (clojure.JS.def(clojure,"min",clojure.JS.variatic(2,(function __clojure_fn_3507_min_3509(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.lang.Numbers.lt(x_1,y_2))?(x_1):(y_2)))
case 1:return (x_1)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce(clojure.min,clojure.min(x_1,y_2),more_3))}))))})();

//======
//(defn inc "Returns a number one greater than num." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/inc)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (inc x)))
//---
(function __clojure_fn_3524(){
return (clojure.JS.def(clojure,"inc",(function __clojure_fn_3524_inc_3529(x_1){
return (clojure.lang.Numbers.inc(x_1))})))})();

//======
//(defn dec "Returns a number one less than num." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/dec)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (dec x)))
//---
(function __clojure_fn_3542(){
return (clojure.JS.def(clojure,"dec",(function __clojure_fn_3542_dec_3547(x_1){
return (clojure.lang.Numbers.dec(x_1))})))})();

//======
//(defn unchecked-inc "Returns a number one greater than x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_inc)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (unchecked_inc x)))
//---
(function __clojure_fn_3560(){
return (clojure.JS.def(clojure,"unchecked_inc",(function __clojure_fn_3560_unchecked_inc_3565(x_1){
return (clojure.lang.Numbers.unchecked_inc(x_1))})))})();

//======
//(defn unchecked-dec "Returns a number one less than x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_dec)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (unchecked_dec x)))
//---
(function __clojure_fn_3578(){
return (clojure.JS.def(clojure,"unchecked_dec",(function __clojure_fn_3578_unchecked_dec_3583(x_1){
return (clojure.lang.Numbers.unchecked_dec(x_1))})))})();

//======
//(defn unchecked-negate "Returns the negation of x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_negate)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (unchecked_negate x)))
//---
(function __clojure_fn_3596(){
return (clojure.JS.def(clojure,"unchecked_negate",(function __clojure_fn_3596_unchecked_negate_3601(x_1){
return (clojure.lang.Numbers.unchecked_negate(x_1))})))})();

//======
//(defn unchecked-add "Returns the sum of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_add)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_add x y)))
//---
(function __clojure_fn_3614(){
return (clojure.JS.def(clojure,"unchecked_add",(function __clojure_fn_3614_unchecked_add_3619(x_1,y_2){
return (clojure.lang.Numbers.unchecked_add(x_1,y_2))})))})();

//======
//(defn unchecked-subtract "Returns the difference of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_subtract)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_subtract x y)))
//---
(function __clojure_fn_3632(){
return (clojure.JS.def(clojure,"unchecked_subtract",(function __clojure_fn_3632_unchecked_subtract_3637(x_1,y_2){
return (clojure.lang.Numbers.unchecked_subtract(x_1,y_2))})))})();

//======
//(defn unchecked-multiply "Returns the product of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_multiply)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_multiply x y)))
//---
(function __clojure_fn_3650(){
return (clojure.JS.def(clojure,"unchecked_multiply",(function __clojure_fn_3650_unchecked_multiply_3655(x_1,y_2){
return (clojure.lang.Numbers.unchecked_multiply(x_1,y_2))})))})();

//======
//(defn unchecked-divide "Returns the division of x by y, both int or long. \n  Note - uses a primitive operator subject to truncation." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_divide)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_divide x y)))
//---
(function __clojure_fn_3668(){
return (clojure.JS.def(clojure,"unchecked_divide",(function __clojure_fn_3668_unchecked_divide_3673(x_1,y_2){
return (clojure.lang.Numbers.unchecked_divide(x_1,y_2))})))})();

//======
//(defn pos? "Returns true if num is greater than zero, else false" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/isPos)) (clojure/list x))))), :tag Boolean} [x] (. clojure.lang.Numbers (isPos x)))
//---
(function __clojure_fn_3686(){
return (clojure.JS.def(clojure,"pos_QMARK_",(function __clojure_fn_3686_pos_QMARK_3691(x_1){
return (clojure.lang.Numbers.isPos(x_1))})))})();

//======
//(defn neg? "Returns true if num is less than zero, else false" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/isNeg)) (clojure/list x))))), :tag Boolean} [x] (. clojure.lang.Numbers (isNeg x)))
//---
(function __clojure_fn_3704(){
return (clojure.JS.def(clojure,"neg_QMARK_",(function __clojure_fn_3704_neg_QMARK_3709(x_1){
return (clojure.lang.Numbers.isNeg(x_1))})))})();

//======
//(defn zero? "Returns true if num is zero, else false" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/isZero)) (clojure/list x))))), :tag Boolean} [x] (. clojure.lang.Numbers (isZero x)))
//---
(function __clojure_fn_3722(){
return (clojure.JS.def(clojure,"zero_QMARK_",(function __clojure_fn_3722_zero_QMARK_3727(x_1){
return (clojure.lang.Numbers.isZero(x_1))})))})();

//======
//(defn quot "quot[ient] of dividing numerator by denominator." [num div] (. clojure.lang.Numbers (quotient num div)))
//---
(function __clojure_fn_3737(){
return (clojure.JS.def(clojure,"quot",(function __clojure_fn_3737_quot_3739(num_1,div_2){
return (clojure.lang.Numbers.quotient(num_1,div_2))})))})();

//======
//(defn rem "rem[ainder] of dividing numerator by denominator." [num div] (. clojure.lang.Numbers (remainder num div)))
//---
(function __clojure_fn_3749(){
return (clojure.JS.def(clojure,"rem",(function __clojure_fn_3749_rem_3751(num_1,div_2){
return (clojure.lang.Numbers.remainder(num_1,div_2))})))})();

//======
//(defn rationalize "returns the rational value of num" [num] (. clojure.lang.Numbers (rationalize num)))
//---
(function __clojure_fn_3761(){
return (clojure.JS.def(clojure,"rationalize",(function __clojure_fn_3761_rationalize_3763(num_1){
return (clojure.lang.Numbers.rationalize(num_1))})))})();

//======
//(defn bit-not "Bitwise complement" [x] (. clojure.lang.Numbers not x))
//---
(function __clojure_fn_3773(){
return (clojure.JS.def(clojure,"bit_not",(function __clojure_fn_3773_bit_not_3775(x_1){
return (clojure.lang.Numbers.not(x_1))})))})();

//======
//(defn bit-and "Bitwise and" [x y] (. clojure.lang.Numbers and x y))
//---
(function __clojure_fn_3785(){
return (clojure.JS.def(clojure,"bit_and",(function __clojure_fn_3785_bit_and_3787(x_1,y_2){
return (clojure.lang.Numbers.and(x_1,y_2))})))})();

//======
//(defn bit-or "Bitwise or" [x y] (. clojure.lang.Numbers or x y))
//---
(function __clojure_fn_3797(){
return (clojure.JS.def(clojure,"bit_or",(function __clojure_fn_3797_bit_or_3799(x_1,y_2){
return (clojure.lang.Numbers.or(x_1,y_2))})))})();

//======
//(defn bit-xor "Bitwise exclusive or" [x y] (. clojure.lang.Numbers xor x y))
//---
(function __clojure_fn_3809(){
return (clojure.JS.def(clojure,"bit_xor",(function __clojure_fn_3809_bit_xor_3811(x_1,y_2){
return (clojure.lang.Numbers.xor(x_1,y_2))})))})();

//======
//(defn bit-and-not "Bitwise and with complement" [x y] (. clojure.lang.Numbers andNot x y))
//---
(function __clojure_fn_3821(){
return (clojure.JS.def(clojure,"bit_and_not",(function __clojure_fn_3821_bit_and_not_3823(x_1,y_2){
return (clojure.lang.Numbers.andNot(x_1,y_2))})))})();

//======
//(defn bit-clear "Clear bit at index n" [x n] (. clojure.lang.Numbers clearBit x n))
//---
(function __clojure_fn_3833(){
return (clojure.JS.def(clojure,"bit_clear",(function __clojure_fn_3833_bit_clear_3835(x_1,n_2){
return (clojure.lang.Numbers.clearBit(x_1,n_2))})))})();

//======
//(defn bit-set "Set bit at index n" [x n] (. clojure.lang.Numbers setBit x n))
//---
(function __clojure_fn_3845(){
return (clojure.JS.def(clojure,"bit_set",(function __clojure_fn_3845_bit_set_3847(x_1,n_2){
return (clojure.lang.Numbers.setBit(x_1,n_2))})))})();

//======
//(defn bit-flip "Flip bit at index n" [x n] (. clojure.lang.Numbers flipBit x n))
//---
(function __clojure_fn_3857(){
return (clojure.JS.def(clojure,"bit_flip",(function __clojure_fn_3857_bit_flip_3859(x_1,n_2){
return (clojure.lang.Numbers.flipBit(x_1,n_2))})))})();

//======
//(defn bit-test "Test bit at index n" [x n] (. clojure.lang.Numbers testBit x n))
//---
(function __clojure_fn_3869(){
return (clojure.JS.def(clojure,"bit_test",(function __clojure_fn_3869_bit_test_3871(x_1,n_2){
return (clojure.lang.Numbers.testBit(x_1,n_2))})))})();

//======
//(defn bit-shift-left "Bitwise shift left" [x n] (. clojure.lang.Numbers shiftLeft x n))
//---
(function __clojure_fn_3881(){
return (clojure.JS.def(clojure,"bit_shift_left",(function __clojure_fn_3881_bit_shift_left_3883(x_1,n_2){
return (clojure.lang.Numbers.shiftLeft(x_1,n_2))})))})();

//======
//(defn bit-shift-right "Bitwise shift right" [x n] (. clojure.lang.Numbers shiftRight x n))
//---
(function __clojure_fn_3893(){
return (clojure.JS.def(clojure,"bit_shift_right",(function __clojure_fn_3893_bit_shift_right_3895(x_1,n_2){
return (clojure.lang.Numbers.shiftRight(x_1,n_2))})))})();

//======
//(defn complement "Takes a fn f and returns a fn that takes the same arguments as f,\n  has the same effects, if any, and returns the opposite truth value." [f] (fn [& args] (not (apply f args))))
//---
(function __clojure_fn_3908(){
return (clojure.JS.def(clojure,"complement",(function __clojure_fn_3908_complement_3910(f_1){
return (clojure.JS.variatic(0,(function __clojure_fn_3908_complement_3910_fn_3912(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.not(clojure.apply(f_1,args_1)))})))})))})();

//======
//(defn constantly "Returns a function that takes any number of arguments and returns x." [x] (fn [& args] x))
//---
(function __clojure_fn_3926(){
return (clojure.JS.def(clojure,"constantly",(function __clojure_fn_3926_constantly_3928(x_1){
return (clojure.JS.variatic(0,(function __clojure_fn_3926_constantly_3928_fn_3930(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (x_1)})))})))})();

//======
//(defn identity "Returns its argument." [x] x)
//---
(function __clojure_fn_3941(){
return (clojure.JS.def(clojure,"identity",(function __clojure_fn_3941_identity_3943(x_1){
return (x_1)})))})();

//======
//(defn peek "For a list or queue, same as first, for a vector, same as, but much\n  more efficient than, last. If the collection is empty, returns nil." [coll] (. clojure.lang.RT (peek coll)))
//---
(function __clojure_fn_3959(){
return (clojure.JS.def(clojure,"peek",(function __clojure_fn_3959_peek_3961(coll_1){
return (clojure.lang.RT.peek(coll_1))})))})();

//======
//(defn pop "For a list or queue, returns a new list/queue without the first\n  item, for a vector, returns a new vector without the last item. If\n  the collection is empty, throws an exception.  Note - not the same\n  as rest/butlast." [coll] (. clojure.lang.RT (pop coll)))
//---
(function __clojure_fn_3971(){
return (clojure.JS.def(clojure,"pop",(function __clojure_fn_3971_pop_3973(coll_1){
return (clojure.lang.RT.pop(coll_1))})))})();

//======
//(defn nth "Returns the value at the index. get returns nil if index out of\n  bounds, nth throws an exception unless not-found is supplied.  nth\n  also works for strings, Java arrays, regex Matchers and Lists, and,\n  in O(n) time, for sequences." ([coll index] (. clojure.lang.RT (nth coll index))) ([coll index not-found] (. clojure.lang.RT (nth coll index not-found))))
//---
(function __clojure_fn_3984(){
return (clojure.JS.def(clojure,"nth",(function __clojure_fn_3984_nth_3986(coll_1,index_2,not_found_3){switch(arguments.length){
case 2:return (clojure.lang.RT.nth(coll_1,index_2))}
return (clojure.lang.RT.nth(coll_1,index_2,not_found_3))})))})();

//======
//(defn contains? "Returns true if key is present, else false." [map key] (. clojure.lang.RT (contains map key)))
//---
(function __clojure_fn_3997(){
return (clojure.JS.def(clojure,"contains_QMARK_",(function __clojure_fn_3997_contains_QMARK_3999(map_1,key_2){
return (clojure.lang.RT.contains(map_1,key_2))})))})();

//======
//(defn get "Returns the value mapped to key, not-found or nil if key not present." ([map key] (. clojure.lang.RT (get map key))) ([map key not-found] (. clojure.lang.RT (get map key not-found))))
//---
(function __clojure_fn_4010(){
return (clojure.JS.def(clojure,"get",(function __clojure_fn_4010_get_4012(map_1,key_2,not_found_3){switch(arguments.length){
case 2:return (clojure.lang.RT.get(map_1,key_2))}
return (clojure.lang.RT.get(map_1,key_2,not_found_3))})))})();

//======
//(defn dissoc "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,\n  that does not contain a mapping for key(s)." ([map] map) ([map key] (. clojure.lang.RT (dissoc map key))) ([map key & ks] (let [ret (dissoc map key)] (if ks (recur ret (first ks) (rest ks)) ret))))
//---
(function __clojure_fn_4025(){
return (clojure.JS.def(clojure,"dissoc",clojure.JS.variatic(2,(function __clojure_fn_4025_dissoc_4027(map_1,key_2){switch(arguments.length){
case 2:return (clojure.lang.RT.dissoc(map_1,key_2))
case 1:return (map_1)}
var _cnt,_rtn,ks_3,ret_4,ks_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((ret_4=clojure.dissoc(map_1,key_2)),
((ks_3)?((_cnt=1,_t0=ret_4,_t1=clojure.first(ks_3),_t2=clojure.rest(ks_3),map_1=_t0,key_2=_t1,ks_3=_t2)):(ret_4)))
}while(_cnt);return _rtn;}))))})();

//======
//(defn disj "disj[oin]. Returns a new set of the same (hashed/sorted) type, that\n  does not contain key(s)." ([set] set) ([set key] (. set (disjoin key))) ([set key & ks] (let [ret (disj set key)] (if ks (recur ret (first ks) (rest ks)) ret))))
//---
(function __clojure_fn_4041(){
return (clojure.JS.def(clojure,"disj",clojure.JS.variatic(2,(function __clojure_fn_4041_disj_4043(set_1,key_2){switch(arguments.length){
case 2:return ((set_1).disjoin(key_2))
case 1:return (set_1)}
var _cnt,_rtn,ks_3,ret_4,ks_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((ret_4=clojure.disj(set_1,key_2)),
((ks_3)?((_cnt=1,_t0=ret_4,_t1=clojure.first(ks_3),_t2=clojure.rest(ks_3),set_1=_t0,key_2=_t1,ks_3=_t2)):(ret_4)))
}while(_cnt);return _rtn;}))))})();

//======
//(defn select-keys "Returns a map containing only those entries in map whose key is in keys" [map keyseq] (loop [ret {} keys (seq keyseq)] (if keys (let [entry (. clojure.lang.RT (find map (first keys)))] (recur (if entry (conj ret entry) ret) (rest keys))) ret)))
//---
(function __clojure_fn_4061(){
return (clojure.JS.def(clojure,"select_keys",(function __clojure_fn_4061_select_keys_4063(map_1,keyseq_2){
var ret_3,entry_5,keys_4;
return (((function __loop(){var _rtn,_cnt;(ret_3=clojure.lang.PersistentHashMap.EMPTY),
(keys_4=clojure.seq(keyseq_2));do{_cnt=0;
_rtn=((keys_4)?(((entry_5=clojure.lang.RT.find(map_1,clojure.first(keys_4))),
(_cnt=1,_t0=((entry_5)?(clojure.conj(ret_3,entry_5)):(ret_3)),_t1=clojure.rest(keys_4),ret_3=_t0,keys_4=_t1))):(ret_3))}while(_cnt);return _rtn;})()))})))})();

//======
//(defn keys "Returns a sequence of the map's keys." [map] (. clojure.lang.RT (keys map)))
//---
(function __clojure_fn_4073(){
return (clojure.JS.def(clojure,"keys",(function __clojure_fn_4073_keys_4075(map_1){
return (clojure.lang.RT.keys(map_1))})))})();

//======
//(defn vals "Returns a sequence of the map's values." [map] (. clojure.lang.RT (vals map)))
//---
(function __clojure_fn_4085(){
return (clojure.JS.def(clojure,"vals",(function __clojure_fn_4085_vals_4087(map_1){
return (clojure.lang.RT.vals(map_1))})))})();

//======
//(defn key "Returns the key of the map entry." [e] (. e (getKey)))
//---
(function __clojure_fn_4097(){
return (clojure.JS.def(clojure,"key",(function __clojure_fn_4097_key_4099(e_1){
return ((e_1).getKey())})))})();

//======
//(defn val "Returns the value in the map entry." [e] (. e (getValue)))
//---
(function __clojure_fn_4109(){
return (clojure.JS.def(clojure,"val",(function __clojure_fn_4109_val_4111(e_1){
return ((e_1).getValue())})))})();

//======
//(defn rseq "Returns, in constant time, a sequence of the items in rev (which\n  can be a vector or sorted-map), in reverse order." [rev] (. rev (rseq)))
//---
(function __clojure_fn_4121(){
return (clojure.JS.def(clojure,"rseq",(function __clojure_fn_4121_rseq_4123(rev_1){
return ((rev_1).rseq())})))})();

//======
//(defn name "Returns the name String of a symbol or keyword." [x] (. x (getName)))
//---
(function __clojure_fn_4133(){
return (clojure.JS.def(clojure,"name",(function __clojure_fn_4133_name_4135(x_1){
return ((x_1).getName())})))})();

//======
//(defn namespace "Returns the namespace String of a symbol or keyword, or nil if not present." [x] (. x (getNamespace)))
//---
(function __clojure_fn_4145(){
return (clojure.JS.def(clojure,"namespace",(function __clojure_fn_4145_namespace_4147(x_1){
return ((x_1).getNamespace())})))})();

//======
//(defn find-var "Returns the global var named by the namespace-qualified symbol, or\n  nil if no var with that name." [sym] (. clojure.lang.Var (find sym)))
//---
(function __clojure_fn_4212(){
return (clojure.JS.def(clojure,"find_var",(function __clojure_fn_4212_find_var_4214(sym_1){
return (clojure.lang.Var.find(sym_1))})))})();

//======
//(defn agent "Creates and returns an agent with an initial value of state and an\n  optional validate fn. validate-fn must be nil or a side-effect-free fn of\n  one argument, which will be passed the intended new state on any state\n  change. If the new state is unacceptable, the validate-fn should\n  throw an exception." ([state] (new clojure.lang.Agent state)) ([state validate-fn] (new clojure.lang.Agent state validate-fn)))
//---
(function __clojure_fn_4225(){
return (clojure.JS.def(clojure,"agent",(function __clojure_fn_4225_agent_4227(state_1,validate_fn_2){switch(arguments.length){
case 1:return ((new clojure.lang.Agent(state_1)))}
return ((new clojure.lang.Agent(state_1,validate_fn_2)))})))})();

//======
//(defn ! [& args] (throw (new Exception "! is now send. See also send-off")))
//---
(function __clojure_fn_4238(){
return (clojure.JS.def(clojure,"_BANG_",clojure.JS.variatic(0,(function __clojure_fn_4238_BANG_4240(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw (new java.lang.Exception("! is now send. See also send-off"))})())}))))})();

//======
//(defn send "Dispatch an action to an agent. Returns the agent immediately.\n  Subsequently, in a thread from a thread pool, the state of the agent\n  will be set to the value of:\n\n  (apply action-fn state-of-agent args)" [a f & args] (. a (dispatch f args false)))
//---
(function __clojure_fn_4250(){
return (clojure.JS.def(clojure,"send",clojure.JS.variatic(2,(function __clojure_fn_4250_send_4252(a_1,f_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((a_1).dispatch(f_2,args_3,false))}))))})();

//======
//(defn send-off "Dispatch a potentially blocking action to an agent. Returns the\n  agent immediately. Subsequently, in a separate thread, the state of\n  the agent will be set to the value of:\n\n  (apply action-fn state-of-agent args)" [a f & args] (. a (dispatch f args true)))
//---
(function __clojure_fn_4262(){
return (clojure.JS.def(clojure,"send_off",clojure.JS.variatic(2,(function __clojure_fn_4262_send_off_4264(a_1,f_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((a_1).dispatch(f_2,args_3,true))}))))})();

//======
//(defn agent-errors "Returns a sequence of the exceptions thrown during asynchronous\n  actions of the agent." [a] (. a (getErrors)))
//---
(function __clojure_fn_4274(){
return (clojure.JS.def(clojure,"agent_errors",(function __clojure_fn_4274_agent_errors_4276(a_1){
return ((a_1).getErrors())})))})();

//======
//(defn clear-agent-errors "Clears any exceptions thrown during asynchronous actions of the\n  agent, allowing subsequent actions to occur." [a] (. a (clearErrors)))
//---
(function __clojure_fn_4286(){
return (clojure.JS.def(clojure,"clear_agent_errors",(function __clojure_fn_4286_clear_agent_errors_4288(a_1){
return ((a_1).clearErrors())})))})();

//======
//(defn shutdown-agents "Initiates a shutdown of the thread pools that back the agent\n  system. Running actions will complete, but no new actions will be\n  accepted" [] (. clojure.lang.Agent shutdown))
//---
(function __clojure_fn_4298(){
return (clojure.JS.def(clojure,"shutdown_agents",(function __clojure_fn_4298_shutdown_agents_4300(){
return (clojure.lang.Agent.shutdown())})))})();

//======
//(defn ref "Creates and returns a Ref with an initial value of x and an optional validate fn.\n  validate-fn must be nil or a side-effect-free fn of one argument, which will\n  be passed the intended new state on any state change. If the new\n  state is unacceptable, the validate-fn should throw an\n  exception. validate-fn will be called on transaction commit, when\n  all refs have their final values." ([x] (new clojure.lang.Ref x)) ([x validate-fn] (new clojure.lang.Ref x validate-fn)))
//---
(function __clojure_fn_4311(){
return (clojure.JS.def(clojure,"ref",(function __clojure_fn_4311_ref_4313(x_1,validate_fn_2){switch(arguments.length){
case 1:return ((new clojure.lang.Ref(x_1)))}
return ((new clojure.lang.Ref(x_1,validate_fn_2)))})))})();

//======
//(defn deref "Also reader macro: @ref/@agent Within a transaction, returns the\n  in-transaction-value of ref, else returns the\n  most-recently-committed value of ref. When applied to an agent,\n  returns its current state." [ref] (. ref (get)))
//---
(function __clojure_fn_4324(){
return (clojure.JS.def(clojure,"deref",(function __clojure_fn_4324_deref_4326(ref_1){
return ((ref_1).get())})))})();

//======
//(defn set-validator "Sets the validator-fn for a var/ref/agent. validator-fn must be nil or a\n  side-effect-free fn of one argument, which will be passed the intended\n  new state on any state change. If the new state is unacceptable, the\n  validator-fn should throw an exception. If the current state (root\n  value if var) is not acceptable to the new validator, an exception\n  will be thrown and the validator will not be changed." [iref validator-fn] (. iref (setValidator validator-fn)))
//---
(function __clojure_fn_4336(){
return (clojure.JS.def(clojure,"set_validator",(function __clojure_fn_4336_set_validator_4338(iref_1,validator_fn_2){
return ((iref_1).setValidator(validator_fn_2))})))})();

//======
//(defn get-validator "Gets the validator-fn for a var/ref/agent." [iref] (. iref (getValidator)))
//---
(function __clojure_fn_4348(){
return (clojure.JS.def(clojure,"get_validator",(function __clojure_fn_4348_get_validator_4350(iref_1){
return ((iref_1).getValidator())})))})();

//======
//(defn commute "Must be called in a transaction. Sets the in-transaction-value of\n  ref to:\n\n  (apply fun in-transaction-value-of-ref args)\n\n  and returns the in-transaction-value of ref.\n\n  At the commit point of the transaction, sets the value of ref to be:\n\n  (apply fun most-recently-committed-value-of-ref args)\n\n  Thus fun should be commutative, or, failing that, you must accept\n  last-one-in-wins behavior.  commute allows for more concurrency than\n  ref-set." [ref fun & args] (. ref (commute fun args)))
//---
(function __clojure_fn_4360(){
return (clojure.JS.def(clojure,"commute",clojure.JS.variatic(2,(function __clojure_fn_4360_commute_4362(ref_1,fun_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((ref_1).commute(fun_2,args_3))}))))})();

//======
//(defn alter "Must be called in a transaction. Sets the in-transaction-value of\n  ref to:\n\n  (apply fun in-transaction-value-of-ref args)\n\n  and returns the in-transaction-value of ref." [ref fun & args] (. ref (alter fun args)))
//---
(function __clojure_fn_4372(){
return (clojure.JS.def(clojure,"alter",clojure.JS.variatic(2,(function __clojure_fn_4372_alter_4374(ref_1,fun_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((ref_1).alter(fun_2,args_3))}))))})();

//======
//(defn ref-set "Must be called in a transaction. Sets the value of ref.\n  Returns val." [ref val] (. ref (set val)))
//---
(function __clojure_fn_4384(){
return (clojure.JS.def(clojure,"ref_set",(function __clojure_fn_4384_ref_set_4386(ref_1,val_2){
return ((ref_1).set(val_2))})))})();

//======
//(defn ensure "Must be called in a transaction. Protects the ref from modification\n  by other transactions.  Returns the in-transaction-value of\n  ref. Allows for more concurrency than (ref-set ref @ref)" [ref] (. ref (touch)) (. ref (get)))
//---
(function __clojure_fn_4396(){
return (clojure.JS.def(clojure,"ensure",(function __clojure_fn_4396_ensure_4398(ref_1){
return ((ref_1).touch(),
(ref_1).get())})))})();

//======
//(defn comp "Takes a set of functions and returns a fn that is the composition\n  of those fns.  The returned fn takes a variable number of args,\n  applies the rightmost of fns to the args, the next\n  fn (right-to-left) to the result, etc." [& fs] (let [fs (reverse fs)] (fn [& args] (loop [ret (apply (first fs) args) fs (rest fs)] (if fs (recur ((first fs) ret) (rest fs)) ret)))))
//---
(function __clojure_fn_4417(){
return (clojure.JS.def(clojure,"comp",clojure.JS.variatic(0,(function __clojure_fn_4417_comp_4419(){
var fs_2,fs_1,fs_1=clojure.JS.rest_args(this,arguments,0);
return (((fs_2=clojure.reverse(fs_1)),
clojure.JS.variatic(0,(function __clojure_fn_4417_comp_4419_fn_4421(){
var fs_3,args_1,ret_2,args_1=clojure.JS.rest_args(this,arguments,0);
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.apply(clojure.first(fs_2),args_1)),
(fs_3=clojure.rest(fs_2));do{_cnt=0;
_rtn=((fs_3)?((_cnt=1,_t0=clojure.first(fs_3)(ret_2),_t1=clojure.rest(fs_3),ret_2=_t0,fs_3=_t1)):(ret_2))}while(_cnt);return _rtn;})()))}))))}))))})();

//======
//(defn partial "Takes a function f and fewer than the normal arguments to f, and\n  returns a fn that takes a variable number of additional args. When\n  called, the returned function calls f with args + additional args." ([f arg1] (fn [& args] (apply f arg1 args))) ([f arg1 arg2] (fn [& args] (apply f arg1 arg2 args))) ([f arg1 arg2 arg3] (fn [& args] (apply f arg1 arg2 arg3 args))) ([f arg1 arg2 arg3 & more] (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))
//---
(function __clojure_fn_4447(){
return (clojure.JS.def(clojure,"partial",clojure.JS.variatic(4,(function __clojure_fn_4447_partial_4449(f_1,arg1_2,arg2_3,arg3_4){switch(arguments.length){
case 4:return (clojure.JS.variatic(0,(function __clojure_fn_4447_partial_4449_fn_4459(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply(f_1,arg1_2,arg2_3,arg3_4,args_1))})))
case 3:return (clojure.JS.variatic(0,(function __clojure_fn_4447_partial_4449_fn_4455(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply(f_1,arg1_2,arg2_3,args_1))})))
case 2:return (clojure.JS.variatic(0,(function __clojure_fn_4447_partial_4449_fn_4451(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply(f_1,arg1_2,args_1))})))}
var more_5,more_5=clojure.JS.rest_args(this,arguments,4);
return (clojure.JS.variatic(0,(function __clojure_fn_4447_partial_4449_fn_4463(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply(f_1,arg1_2,arg2_3,arg3_4,clojure.concat(more_5,args_1)))})))}))))})();

//======
//(defn every? "Returns true if (pred x) is logical true for every x in coll, else\n  false." {:tag Boolean} [pred coll] (if (seq coll) (and (pred (first coll)) (recur pred (rest coll))) true))
//---
(function __clojure_fn_4474(){
return (clojure.JS.def(clojure,"every_QMARK_",(function __clojure_fn_4474_every_QMARK_4476(pred_1,coll_2){
var _cnt,_rtn,and__196_3;
do{_cnt=0;_rtn=((clojure.seq(coll_2))?(((and__196_3=pred_1(clojure.first(coll_2))),
((and__196_3)?((_cnt=1,_t0=pred_1,_t1=clojure.rest(coll_2),pred_1=_t0,coll_2=_t1)):(and__196_3)))):(true))
}while(_cnt);return _rtn;})))})();

//======
//(def not-every? (comp not every?))
//---
(function __clojure_fn_4483(){
return (clojure.JS.def(clojure,"not_every_QMARK_",clojure.comp(clojure.not,clojure.every_QMARK_)))})();

//======
//(defn some "Returns the first logical true value of (pred x) for any x in coll,\n  else nil." [pred coll] (when (seq coll) (or (pred (first coll)) (recur pred (rest coll)))))
//---
(function __clojure_fn_4492(){
return (clojure.JS.def(clojure,"some",(function __clojure_fn_4492_some_4494(pred_1,coll_2){
var _cnt,_rtn,or__202_3;
do{_cnt=0;_rtn=((clojure.seq(coll_2))?(((or__202_3=pred_1(clojure.first(coll_2))),
((or__202_3)?(or__202_3):((_cnt=1,_t0=pred_1,_t1=clojure.rest(coll_2),pred_1=_t0,coll_2=_t1))))):(null))
}while(_cnt);return _rtn;})))})();

//======
//(def not-any? (comp not some))
//---
(function __clojure_fn_4501(){
return (clojure.JS.def(clojure,"not_any_QMARK_",clojure.comp(clojure.not,clojure.some)))})();

//======
//(defn map "Returns a lazy seq consisting of the result of applying f to the\n  set of first items of each coll, followed by applying f to the set\n  of second items in each coll, until any one of the colls is\n  exhausted.  Any remaining items in other colls are ignored. Function\n  f should accept number-of-colls arguments." ([f coll] (when (seq coll) (lazy-cons (f (first coll)) (map f (rest coll))))) ([f coll & colls] (when (and (seq coll) (every? seq colls)) (lazy-cons (apply f (first coll) (map first colls)) (apply map f (rest coll) (map rest colls))))))
//---
(function __clojure_fn_4521(){
return (clojure.JS.def(clojure,"map",clojure.JS.variatic(2,(function __clojure_fn_4521_map_4523(f_1,coll_2){switch(arguments.length){
case 2:return (((clojure.seq(coll_2))?((new clojure.lang.LazyCons((function __clojure_fn_4521_map_4523_fn_4526(G__4525_1){switch(arguments.length){
case 0:return (f_1(clojure.first(coll_2)))}
return (clojure.map(f_1,clojure.rest(coll_2)))})))):(null)))}
var and__196_4,colls_3,colls_3=clojure.JS.rest_args(this,arguments,2);
return (((((and__196_4=clojure.seq(coll_2)),
((and__196_4)?(clojure.every_QMARK_(clojure.seq,colls_3)):(and__196_4))))?((new clojure.lang.LazyCons((function __clojure_fn_4521_map_4523_fn_4532(G__4531_1){switch(arguments.length){
case 0:return (clojure.apply(f_1,clojure.first(coll_2),clojure.map(clojure.first,colls_3)))}
return (clojure.apply(clojure.map,f_1,clojure.rest(coll_2),clojure.map(clojure.rest,colls_3)))})))):(null)))}))))})();

//======
//(defn mapcat "Returns the result of applying concat to the result of applying map\n  to f and colls.  Thus function f should return a collection." [f & colls] (apply concat (apply map f colls)))
//---
(function __clojure_fn_4544(){
return (clojure.JS.def(clojure,"mapcat",clojure.JS.variatic(1,(function __clojure_fn_4544_mapcat_4546(f_1){
var colls_2,colls_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.apply(clojure.concat,clojure.apply(clojure.map,f_1,colls_2)))}))))})();

//======
//(defn filter "Returns a lazy seq of the items in coll for which\n  (pred item) returns true. pred must be free of side-effects." [pred coll] (when (seq coll) (if (pred (first coll)) (lazy-cons (first coll) (filter pred (rest coll))) (recur pred (rest coll)))))
//---
(function __clojure_fn_4561(){
return (clojure.JS.def(clojure,"filter",(function __clojure_fn_4561_filter_4563(pred_1,coll_2){
var _cnt,_rtn;
do{_cnt=0;_rtn=((clojure.seq(coll_2))?(((pred_1(clojure.first(coll_2)))?((new clojure.lang.LazyCons((function __clojure_fn_4561_filter_4563_fn_4566(G__4565_1){switch(arguments.length){
case 0:return (clojure.first(coll_2))}
return (clojure.filter(pred_1,clojure.rest(coll_2)))})))):((_cnt=1,_t0=pred_1,_t1=clojure.rest(coll_2),pred_1=_t0,coll_2=_t1)))):(null))
}while(_cnt);return _rtn;})))})();

//======
//(defn take "Returns a lazy seq of the first n items in coll, or all items if\n  there are fewer than n." [n coll] (when (and (pos? n) (seq coll)) (lazy-cons (first coll) (take (dec n) (rest coll)))))
//---
(function __clojure_fn_4583(){
return (clojure.JS.def(clojure,"take",(function __clojure_fn_4583_take_4585(n_1,coll_2){
var and__196_3;
return (((((and__196_3=clojure.lang.Numbers.isPos(n_1)),
((and__196_3)?(clojure.seq(coll_2)):(and__196_3))))?((new clojure.lang.LazyCons((function __clojure_fn_4583_take_4585_fn_4588(G__4587_1){switch(arguments.length){
case 0:return (clojure.first(coll_2))}
return (clojure.take(clojure.lang.Numbers.dec(n_1),clojure.rest(coll_2)))})))):(null)))})))})();

//======
//(defn take-while "Returns a lazy seq of successive items from coll while\n  (pred item) returns true. pred must be free of side-effects." [pred coll] (when (and (seq coll) (pred (first coll))) (lazy-cons (first coll) (take-while pred (rest coll)))))
//---
(function __clojure_fn_4605(){
return (clojure.JS.def(clojure,"take_while",(function __clojure_fn_4605_take_while_4607(pred_1,coll_2){
var and__196_3;
return (((((and__196_3=clojure.seq(coll_2)),
((and__196_3)?(pred_1(clojure.first(coll_2))):(and__196_3))))?((new clojure.lang.LazyCons((function __clojure_fn_4605_take_while_4607_fn_4610(G__4609_1){switch(arguments.length){
case 0:return (clojure.first(coll_2))}
return (clojure.take_while(pred_1,clojure.rest(coll_2)))})))):(null)))})))})();

//======
//(defn drop "Returns a lazy seq of all but the first n items in coll." [n coll] (if (and (pos? n) (seq coll)) (recur (dec n) (rest coll)) (seq coll)))
//---
(function __clojure_fn_4622(){
return (clojure.JS.def(clojure,"drop",(function __clojure_fn_4622_drop_4624(n_1,coll_2){
var _cnt,_rtn,and__196_3;
do{_cnt=0;_rtn=((((and__196_3=clojure.lang.Numbers.isPos(n_1)),
((and__196_3)?(clojure.seq(coll_2)):(and__196_3))))?((_cnt=1,_t0=clojure.lang.Numbers.dec(n_1),_t1=clojure.rest(coll_2),n_1=_t0,coll_2=_t1)):(clojure.seq(coll_2)))
}while(_cnt);return _rtn;})))})();

//======
//(defn drop-last "Return a lazy seq of all but the last n (default 1) items in coll" ([s] (drop-last 1 s)) ([n s] (map (fn [x _] x) (seq s) (drop n s))))
//---
(function __clojure_fn_4638(){
return (clojure.JS.def(clojure,"drop_last",(function __clojure_fn_4638_drop_last_4640(n_1,s_2){switch(arguments.length){
case 1:var s_1=arguments[0];
return (clojure.drop_last(1,s_1))}
return (clojure.map((function __clojure_fn_4638_drop_last_4640_fn_4643(x_1,__2){
return (x_1)}),clojure.seq(s_2),clojure.drop(n_1,s_2)))})))})();

//======
//(defn drop-while "Returns a lazy seq of the items in coll starting from the first\n  item for which (pred item) returns nil." [pred coll] (if (and (seq coll) (pred (first coll))) (recur pred (rest coll)) (seq coll)))
//---
(function __clojure_fn_4654(){
return (clojure.JS.def(clojure,"drop_while",(function __clojure_fn_4654_drop_while_4656(pred_1,coll_2){
var _cnt,_rtn,and__196_3;
do{_cnt=0;_rtn=((((and__196_3=clojure.seq(coll_2)),
((and__196_3)?(pred_1(clojure.first(coll_2))):(and__196_3))))?((_cnt=1,_t0=pred_1,_t1=clojure.rest(coll_2),pred_1=_t0,coll_2=_t1)):(clojure.seq(coll_2)))
}while(_cnt);return _rtn;})))})();

//======
//(defn cycle "Returns a lazy (infinite!) seq of repetitions of the items in\n  coll." [coll] (when (seq coll) (let [rep (fn thisfn [xs] (if xs (lazy-cons (first xs) (thisfn (rest xs))) (recur (seq coll))))] (rep (seq coll)))))
//---
(function __clojure_fn_4673(){
return (clojure.JS.def(clojure,"cycle",(function __clojure_fn_4673_cycle_4675(coll_1){
var rep_2;
return (((clojure.seq(coll_1))?(((rep_2=(function __clojure_fn_4673_cycle_4675_thisfn_4677(xs_1){
var _cnt,_rtn,thisfn_0=arguments.callee;
do{_cnt=0;_rtn=((xs_1)?((new clojure.lang.LazyCons((function __clojure_fn_4673_cycle_4675_thisfn_4677_fn_4679(G__4678_1){switch(arguments.length){
case 0:return (clojure.first(xs_1))}
return (thisfn_0(clojure.rest(xs_1)))})))):((_cnt=1,_t0=clojure.seq(coll_1),xs_1=_t0)))
}while(_cnt);return _rtn;})),
rep_2(clojure.seq(coll_1)))):(null)))})))})();

//======
//(defn split-at "Returns a vector of [(take n coll) (drop n coll)]" [n coll] [(take n coll) (drop n coll)])
//---
(function __clojure_fn_4692(){
return (clojure.JS.def(clojure,"split_at",(function __clojure_fn_4692_split_at_4694(n_1,coll_2){
return (clojure.JS.lit_vector([clojure.take(n_1,coll_2),clojure.drop(n_1,coll_2)]))})))})();

//======
//(defn split-with "Returns a vector of [(take-while pred coll) (drop-while pred coll)]" [pred coll] [(take-while pred coll) (drop-while pred coll)])
//---
(function __clojure_fn_4704(){
return (clojure.JS.def(clojure,"split_with",(function __clojure_fn_4704_split_with_4706(pred_1,coll_2){
return (clojure.JS.lit_vector([clojure.take_while(pred_1,coll_2),clojure.drop_while(pred_1,coll_2)]))})))})();

//======
//(defn repeat "Returns a lazy (infinite!) seq of xs." [x] (lazy-cons x (repeat x)))
//---
(function __clojure_fn_4721(){
return (clojure.JS.def(clojure,"repeat",(function __clojure_fn_4721_repeat_4723(x_1){
return ((new clojure.lang.LazyCons((function __clojure_fn_4721_repeat_4723_fn_4726(G__4725_1){switch(arguments.length){
case 0:return (x_1)}
return (clojure.repeat(x_1))}))))})))})();

//======
//(defn replicate "Returns a lazy seq of n xs." [n x] (take n (repeat x)))
//---
(function __clojure_fn_4738(){
return (clojure.JS.def(clojure,"replicate",(function __clojure_fn_4738_replicate_4740(n_1,x_2){
return (clojure.take(n_1,clojure.repeat(x_2)))})))})();

//======
//(defn iterate "Returns a lazy seq of x, (f x), (f (f x)) etc. f must be free of side-effects" [f x] (lazy-cons x (iterate f (f x))))
//---
(function __clojure_fn_4755(){
return (clojure.JS.def(clojure,"iterate",(function __clojure_fn_4755_iterate_4757(f_1,x_2){
return ((new clojure.lang.LazyCons((function __clojure_fn_4755_iterate_4757_fn_4760(G__4759_1){switch(arguments.length){
case 0:return (x_2)}
return (clojure.iterate(f_1,f_1(x_2)))}))))})))})();

//======
//(defn range "Returns a lazy seq of nums from start (inclusive) to end\n  (exclusive), by step, where start defaults to 0 and step to 1." ([end] (if (and (> end 0) (< end (. Integer MAX_VALUE))) (new clojure.lang.Range 0 end) (take end (iterate inc 0)))) ([start end] (if (and (< start end) (< end (. Integer MAX_VALUE))) (new clojure.lang.Range start end) (take (- end start) (iterate inc start)))) ([start end step] (take-while (partial (if (pos? step) > <) end) (iterate (partial + step) start))))
//---
(function __clojure_fn_4774(){
return (clojure.JS.def(clojure,"range",(function __clojure_fn_4774_range_4776(start_1,end_2,step_3){switch(arguments.length){
case 2:var and__196_3;
return (((((and__196_3=clojure.lang.Numbers.lt(start_1,end_2)),
((and__196_3)?(clojure.lang.Numbers.lt(end_2,java.lang.Integer.MAX_VALUE)):(and__196_3))))?((new clojure.lang.Range(start_1,end_2))):(clojure.take(clojure.lang.Numbers.minus(end_2,start_1),clojure.iterate(clojure.inc,start_1)))))
case 1:var and__196_2,end_1=arguments[0];
return (((((and__196_2=clojure.lang.Numbers.gt(end_1,0)),
((and__196_2)?(clojure.lang.Numbers.lt(end_1,java.lang.Integer.MAX_VALUE)):(and__196_2))))?((new clojure.lang.Range(0,end_1))):(clojure.take(end_1,clojure.iterate(clojure.inc,0)))))}
return (clojure.take_while(clojure.partial(((clojure.lang.Numbers.isPos(step_3))?(clojure._GT_):(clojure._LT_)),end_2),clojure.iterate(clojure.partial(clojure._PLUS_,step_3),start_1)))})))})();

//======
//(defn merge "Returns a map that consists of the rest of the maps conj-ed onto\n  the first.  If a key occurs in more than one map, the mapping from\n  the latter (left-to-right) will be the mapping in the result." [& maps] (reduce conj maps))
//---
(function __clojure_fn_4788(){
return (clojure.JS.def(clojure,"merge",clojure.JS.variatic(0,(function __clojure_fn_4788_merge_4790(){
var maps_1,maps_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.reduce(clojure.conj,maps_1))}))))})();

//======
//(defn merge-with "Returns a map that consists of the rest of the maps conj-ed onto\n  the first.  If a key occurs in more than one map, the mapping(s)\n  from the latter (left-to-right) will be combined with the mapping in\n  the result by calling (f val-in-result val-in-latter)." [f & maps] (let [merge-entry (fn [m e] (let [k (key e) v (val e)] (if (contains? m k) (assoc m k (f (m k) v)) (assoc m k v)))) merge2 (fn [m1 m2] (reduce merge-entry m1 (seq m2)))] (reduce merge2 maps)))
//---
(function __clojure_fn_4806(){
return (clojure.JS.def(clojure,"merge_with",clojure.JS.variatic(1,(function __clojure_fn_4806_merge_with_4808(f_1){
var merge2_4,merge_entry_3,maps_2,maps_2=clojure.JS.rest_args(this,arguments,1);
return (((merge_entry_3=(function __clojure_fn_4806_merge_with_4808_merge_entry_4810(m_1,e_2){
var k_3,v_4;
return (((k_3=clojure.key(e_2)),
(v_4=clojure.val(e_2)),
((clojure.contains_QMARK_(m_1,k_3))?(clojure.assoc(m_1,k_3,f_1(m_1(k_3),v_4))):(clojure.assoc(m_1,k_3,v_4)))))})),
(merge2_4=(function __clojure_fn_4806_merge_with_4808_merge2_4813(m1_1,m2_2){
return (clojure.reduce(merge_entry_3,m1_1,clojure.seq(m2_2)))})),
clojure.reduce(merge2_4,maps_2)))}))))})();

//======
//(defn zipmap "Returns a map with the keys mapped to the corresponding vals." [keys vals] (loop [map {} ks (seq keys) vs (seq vals)] (if (and ks vs) (recur (assoc map (first ks) (first vs)) (rest ks) (rest vs)) map)))
//---
(function __clojure_fn_4824(){
return (clojure.JS.def(clojure,"zipmap",(function __clojure_fn_4824_zipmap_4826(keys_1,vals_2){
var and__196_6,ks_4,vs_5,map_3;
return (((function __loop(){var _rtn,_cnt;(map_3=clojure.lang.PersistentHashMap.EMPTY),
(ks_4=clojure.seq(keys_1)),
(vs_5=clojure.seq(vals_2));do{_cnt=0;
_rtn=((((and__196_6=ks_4),
((and__196_6)?(vs_5):(and__196_6))))?((_cnt=1,_t0=clojure.assoc(map_3,clojure.first(ks_4),clojure.first(vs_5)),_t1=clojure.rest(ks_4),_t2=clojure.rest(vs_5),map_3=_t0,ks_4=_t1,vs_5=_t2)):(map_3))}while(_cnt);return _rtn;})()))})))})();

//======
//(defn line-seq "Returns the lines of text from rdr as a lazy sequence of strings.\n  rdr must implement java.io.BufferedReader." [rdr] (let [line (. rdr (readLine))] (when line (lazy-cons line (line-seq rdr)))))
//---
(function __clojure_fn_4841(){
return (clojure.JS.def(clojure,"line_seq",(function __clojure_fn_4841_line_seq_4843(rdr_1){
var line_2;
return (((line_2=(rdr_1).readLine()),
((line_2)?((new clojure.lang.LazyCons((function __clojure_fn_4841_line_seq_4843_fn_4846(G__4845_1){switch(arguments.length){
case 0:return (line_2)}
return (clojure.line_seq(rdr_1))})))):(null))))})))})();

//======
//(defn comparator "Returns an implementation of java.util.Comparator based upon pred." [pred] (fn [x y] (cond (pred x y) -1 (pred y x) 1 :else 0)))
//---
(function __clojure_fn_4861(){
return (clojure.JS.def(clojure,"comparator",(function __clojure_fn_4861_comparator_4863(pred_1){
return ((function __clojure_fn_4861_comparator_4863_fn_4865(x_1,y_2){
return (((pred_1(x_1,y_2))?(-1):(((pred_1(y_2,x_1))?(1):(((":else")?(0):(null)))))))}))})))})();

//======
//(defn sort "Returns a sorted sequence of the items in coll. If no comparator is\n  supplied, uses compare. comparator must\n  implement java.util.Comparator." ([coll] (sort compare coll)) ([comp coll] (when (and coll (not (. coll (isEmpty)))) (let [a (. coll (toArray))] (. java.util.Arrays (sort a comp)) (seq a)))))
//---
(function __clojure_fn_4877(){
return (clojure.JS.def(clojure,"sort",(function __clojure_fn_4877_sort_4879(comp_1,coll_2){switch(arguments.length){
case 1:var coll_1=arguments[0];
return (clojure.sort(clojure.compare,coll_1))}
var a_3,and__196_3;
return (((((and__196_3=coll_2),
((and__196_3)?(clojure.not((coll_2).isEmpty())):(and__196_3))))?(((a_3=(coll_2).toArray()),
java.util.Arrays.sort(a_3,comp_1),
clojure.seq(a_3))):(null)))})))})();

//======
//(defn sort-by "Returns a sorted sequence of the items in coll, where the sort\n  order is determined by comparing (keyfn item).  If no comparator is\n  supplied, uses compare. comparator must\n  implement java.util.Comparator." ([keyfn coll] (sort-by keyfn compare coll)) ([keyfn comp coll] (sort (fn [x y] (. comp (compare (keyfn x) (keyfn y)))) coll)))
//---
(function __clojure_fn_4894(){
return (clojure.JS.def(clojure,"sort_by",(function __clojure_fn_4894_sort_by_4896(keyfn_1,comp_2,coll_3){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (clojure.sort_by(keyfn_1,clojure.compare,coll_2))}
return (clojure.sort((function __clojure_fn_4894_sort_by_4896_fn_4899(x_1,y_2){
return ((comp_2).compare(keyfn_1(x_1),keyfn_1(y_2)))}),coll_3))})))})();

//======
//(defn eval "Evaluates the form data structure (not text!) and returns the result." [form] (. clojure.lang.Compiler (eval form)))
//---
(function __clojure_fn_4910(){
return (clojure.JS.def(clojure,"eval",(function __clojure_fn_4910_eval_4912(form_1){
return (clojure.lang.Compiler.eval(form_1))})))})();

//======
//(defn scan [& args] (throw (new Exception "scan is now called dorun")))
//---
(function __clojure_fn_4929(){
return (clojure.JS.def(clojure,"scan",clojure.JS.variatic(0,(function __clojure_fn_4929_scan_4931(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw (new java.lang.Exception("scan is now called dorun"))})())}))))})();

//======
//(defn touch [& args] (throw (new Exception "touch is now called doall")))
//---
(function __clojure_fn_4941(){
return (clojure.JS.def(clojure,"touch",clojure.JS.variatic(0,(function __clojure_fn_4941_touch_4943(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw (new java.lang.Exception("touch is now called doall"))})())}))))})();

//======
//(defn dorun "When lazy sequences are produced via functions that have side\n  effects, any effects other than those needed to produce the first\n  element in the seq do not occur until the seq is consumed. dorun can\n  be used to force any effects. Walks through the successive rests of\n  the seq, does not retain the head and returns nil." ([coll] (when (and (seq coll) (or (first coll) true)) (recur (rest coll)))) ([n coll] (when (and (seq coll) (pos? n) (or (first coll) true)) (recur (dec n) (rest coll)))))
//---
(function __clojure_fn_4954(){
return (clojure.JS.def(clojure,"dorun",(function __clojure_fn_4954_dorun_4956(n_1,coll_2){switch(arguments.length){
case 1:var _cnt,_rtn,or__202_3,and__196_2,coll_1=arguments[0];
do{_cnt=0;_rtn=((((and__196_2=clojure.seq(coll_1)),
((and__196_2)?(((or__202_3=clojure.first(coll_1)),
((or__202_3)?(or__202_3):(true)))):(and__196_2))))?((_cnt=1,_t0=clojure.rest(coll_1),coll_1=_t0)):(null))
}while(_cnt);return _rtn;}
var _cnt,_rtn,and__196_3,and__196_4,or__202_5;
do{_cnt=0;_rtn=((((and__196_3=clojure.seq(coll_2)),
((and__196_3)?(((and__196_4=clojure.lang.Numbers.isPos(n_1)),
((and__196_4)?(((or__202_5=clojure.first(coll_2)),
((or__202_5)?(or__202_5):(true)))):(and__196_4)))):(and__196_3))))?((_cnt=1,_t0=clojure.lang.Numbers.dec(n_1),_t1=clojure.rest(coll_2),n_1=_t0,coll_2=_t1)):(null))
}while(_cnt);return _rtn;})))})();

//======
//(defn doall "When lazy sequences are produced via functions that have side\n  effects, any effects other than those needed to produce the first\n  element in the seq do not occur until the seq is consumed. doall can\n  be used to force any effects. Walks through the successive rests of\n  the seq, retains the head and returns it, thus causing the entire\n  seq to reside in memory at one time." ([coll] (dorun coll) coll) ([n coll] (dorun n coll) coll))
//---
(function __clojure_fn_4968(){
return (clojure.JS.def(clojure,"doall",(function __clojure_fn_4968_doall_4970(n_1,coll_2){switch(arguments.length){
case 1:var coll_1=arguments[0];
return (clojure.dorun(coll_1),
coll_1)}
return (clojure.dorun(n_1,coll_2),
coll_2)})))})();

//======
//(defn await "Blocks the current thread (indefinitely!) until all actions\n  dispatched thus far, from this thread or agent, to the agent(s) have\n  occurred." [& agents] (when *agent* (throw (new Exception "Can't await in agent action"))) (let [latch (new java.util.concurrent.CountDownLatch (count agents)) count-down (fn [agent] (. latch (countDown)) agent)] (doseq agent agents (send agent count-down)) (. latch (await))))
//---
(function __clojure_fn_4984(){
return (clojure.JS.def(clojure,"await",clojure.JS.variatic(0,(function __clojure_fn_4984_await_4986(){
var list__739_4,latch_2,agent_5,agents_1,count_down_3,agents_1=clojure.JS.rest_args(this,arguments,0);
return (((clojure._STAR_agent_STAR_)?((function __throw(){throw (new java.lang.Exception("Can't await in agent action"))})()):(null)),
((latch_2=(new java.util.concurrent.CountDownLatch(clojure.count(agents_1)))),
(count_down_3=(function __clojure_fn_4984_await_4986_count_down_4988(agent_1){
return ((latch_2).countDown(),
agent_1)})),
((function __loop(){var _rtn,_cnt;(list__739_4=clojure.seq(agents_1));do{_cnt=0;
_rtn=((list__739_4)?(((agent_5=clojure.first(list__739_4)),
clojure.send(agent_5,count_down_3)),
(_cnt=1,_t0=clojure.rest(list__739_4),list__739_4=_t0)):(null))}while(_cnt);return _rtn;})()),
(latch_2).await()))}))))})();

//======
//(defn await1 [a] (when (pos? (.getQueueCount a)) (await a)) a)
//---
(function __clojure_fn_4999(){
return (clojure.JS.def(clojure,"await1",(function __clojure_fn_4999_await1_5001(a_1){
return (((clojure.lang.Numbers.isPos((a_1).getQueueCount()))?(clojure.await(a_1)):(null)),
a_1)})))})();

//======
//(defn await-for "Blocks the current thread until all actions dispatched thus\n  far (from this thread or agent) to the agents have occurred, or the\n  timeout (in milliseconds) has elapsed. Returns nil if returning due\n  to timeout, non-nil otherwise." [timeout-ms & agents] (when *agent* (throw (new Exception "Can't await in agent action"))) (let [latch (new java.util.concurrent.CountDownLatch (count agents)) count-down (fn [agent] (. latch (countDown)) agent)] (doseq agent agents (send agent count-down)) (. latch (await timeout-ms (. java.util.concurrent.TimeUnit MILLISECONDS)))))
//---
(function __clojure_fn_5014(){
return (clojure.JS.def(clojure,"await_for",clojure.JS.variatic(1,(function __clojure_fn_5014_await_for_5016(timeout_ms_1){
var latch_3,agents_2,agent_6,list__739_5,count_down_4,agents_2=clojure.JS.rest_args(this,arguments,1);
return (((clojure._STAR_agent_STAR_)?((function __throw(){throw (new java.lang.Exception("Can't await in agent action"))})()):(null)),
((latch_3=(new java.util.concurrent.CountDownLatch(clojure.count(agents_2)))),
(count_down_4=(function __clojure_fn_5014_await_for_5016_count_down_5018(agent_1){
return ((latch_3).countDown(),
agent_1)})),
((function __loop(){var _rtn,_cnt;(list__739_5=clojure.seq(agents_2));do{_cnt=0;
_rtn=((list__739_5)?(((agent_6=clojure.first(list__739_5)),
clojure.send(agent_6,count_down_4)),
(_cnt=1,_t0=clojure.rest(list__739_5),list__739_5=_t0)):(null))}while(_cnt);return _rtn;})()),
(latch_3).await(timeout_ms_1,java.util.concurrent.TimeUnit.MILLISECONDS)))}))))})();

//======
//(defn into-array "Returns an array of the type of the first element in coll,\n  containing the contents of coll, which must be of a compatible\n  type." [aseq] (. clojure.lang.RT (seqToTypedArray (seq aseq))))
//---
(function __clojure_fn_5042(){
return (clojure.JS.def(clojure,"into_array",(function __clojure_fn_5042_into_array_5044(aseq_1){
return (clojure.lang.RT.seqToTypedArray(clojure.seq(aseq_1)))})))})();

//======
//(defn into "Returns a new coll consisting of to-coll with all of the items of\n  from-coll conjoined." [to from] (let [ret to items (seq from)] (if items (recur (conj ret (first items)) (rest items)) ret)))
//---
(function __clojure_fn_5054(){
return (clojure.JS.def(clojure,"into",(function __clojure_fn_5054_into_5056(to_1,from_2){
var _cnt,_rtn,ret_3,items_4;
do{_cnt=0;_rtn=((ret_3=to_1),
(items_4=clojure.seq(from_2)),
((items_4)?((_cnt=1,_t0=clojure.conj(ret_3,clojure.first(items_4)),_t1=clojure.rest(items_4),to_1=_t0,from_2=_t1)):(ret_3)))
}while(_cnt);return _rtn;})))})();

//======
//(defn array [& items] (into-array items))
//---
(function __clojure_fn_5066(){
return (clojure.JS.def(clojure,"array",clojure.JS.variatic(0,(function __clojure_fn_5066_array_5068(){
var items_1,items_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.into_array(items_1))}))))})();

//======
//(defn pr "Prints the object(s) to the output stream that is the current value\n  of *out*.  Prints the object(s), separated by spaces if there is\n  more than one.  By default, pr and prn print in a way that objects\n  can be read by the reader" ([] nil) ([x] (. clojure.lang.RT (print x *out*)) nil) ([x & more] (pr x) (. *out* (append \space)) (apply pr more)))
//---
(function __clojure_fn_5080(){
return (clojure.JS.def(clojure,"pr",clojure.JS.variatic(1,(function __clojure_fn_5080_pr_5082(x_1){switch(arguments.length){
case 1:return (clojure.lang.RT.print(x_1,clojure._STAR_out_STAR_),
null)
case 0:return (null)}
var more_2,more_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.pr(x_1),
(clojure._STAR_out_STAR_).append(" "),
clojure.apply(clojure.pr,more_2))}))))})();

//======
//(defn newline "Writes a newline to the output stream that is the current value of\n  *out*" [] (. *out* (append \newline)) nil)
//---
(function __clojure_fn_5094(){
return (clojure.JS.def(clojure,"newline",(function __clojure_fn_5094_newline_5096(){
return ((clojure._STAR_out_STAR_).append("\n"),
null)})))})();

//======
//(defn flush "Flushes the output stream that is the current value of\n  *out*" [] (. *out* (flush)) nil)
//---
(function __clojure_fn_5106(){
return (clojure.JS.def(clojure,"flush",(function __clojure_fn_5106_flush_5108(){
return ((clojure._STAR_out_STAR_).flush(),
null)})))})();

//======
//(defn prn "Same as pr followed by (newline). Observes *flush-on-newline*" [& more] (apply pr more) (newline) (when *flush-on-newline* (flush)))
//---
(function __clojure_fn_5118(){
return (clojure.JS.def(clojure,"prn",clojure.JS.variatic(0,(function __clojure_fn_5118_prn_5120(){
var more_1,more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply(clojure.pr,more_1),
clojure.newline(),
((clojure._STAR_flush_on_newline_STAR_)?(clojure.flush()):(null)))}))))})();

//======
//(defn print "Prints the object(s) to the output stream that is the current value\n  of *out*.  print and println produce output for human consumption." [& more] (binding [*print-readably* nil] (apply pr more)))
//---
(function __clojure_fn_5130(){
return (clojure.JS.def(clojure,"print",clojure.JS.variatic(0,(function __clojure_fn_5130_print_5132(){
var more_1,more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.Var.pushThreadBindings(clojure.hash_map(clojure._var__STAR_print_readably_STAR_,null)),
(function __try(){try{var _rtn=(clojure.apply(clojure.pr,more_1))}
finally{clojure.lang.Var.popThreadBindings()}})())}))))})();

//======
//(defn println "Same as print followed by (newline)" [& more] (binding [*print-readably* nil] (apply prn more)))
//---
(function __clojure_fn_5142(){
return (clojure.JS.def(clojure,"println",clojure.JS.variatic(0,(function __clojure_fn_5142_println_5144(){
var more_1,more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.Var.pushThreadBindings(clojure.hash_map(clojure._var__STAR_print_readably_STAR_,null)),
(function __try(){try{var _rtn=(clojure.apply(clojure.prn,more_1))}
finally{clojure.lang.Var.popThreadBindings()}})())}))))})();

//======
//(defn read "Reads the next object from stream, which must be an instance of\n  java.io.PushbackReader or some derivee.  stream defaults to the\n  current value of *in* ." ([] (read *in*)) ([stream] (read stream true nil)) ([stream eof-error? eof-value] (read stream eof-error? eof-value false)) ([stream eof-error? eof-value recursive?] (. clojure.lang.LispReader (read stream eof-error? eof-value recursive?))))
//---
(function __clojure_fn_5157(){
return (clojure.JS.def(clojure,"read",(function __clojure_fn_5157_read_5159(stream_1,eof_error_QMARK__2,eof_value_3,recursive_QMARK__4){switch(arguments.length){
case 3:return (clojure.read(stream_1,eof_error_QMARK__2,eof_value_3,false))
case 1:return (clojure.read(stream_1,true,null))
case 0:return (clojure.read(clojure._STAR_in_STAR_))}
return (clojure.lang.LispReader.read(stream_1,eof_error_QMARK__2,eof_value_3,recursive_QMARK__4))})))})();

//======
//(defn read-line "Reads the next line from stream that is the current value of *in* ." [] (. *in* (readLine)))
//---
(function __clojure_fn_5172(){
return (clojure.JS.def(clojure,"read_line",(function __clojure_fn_5172_read_line_5174(){
return ((clojure._STAR_in_STAR_).readLine())})))})();

//======
//(defn num "Coerce to Number" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/num)) (clojure/list x))))), :tag Number} [x] (. clojure.lang.Numbers (num x)))
//---
(function __clojure_fn_5217(){
return (clojure.JS.def(clojure,"num",(function __clojure_fn_5217_num_5222(x_1){
return (clojure.lang.Numbers.num(x_1))})))})();

//======
//(defn int "Coerce to int" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/intCast)) (clojure/list x))))), :tag Integer} [x] (. clojure.lang.RT (intCast x)))
//---
(function __clojure_fn_5235(){
return (clojure.JS.def(clojure,"int",(function __clojure_fn_5235_int_5240(x_1){
return (clojure.lang.RT.intCast(x_1))})))})();

//======
//(defn long "Coerce to long" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/longCast)) (clojure/list x))))), :tag Long} [x] (. x (longValue)))
//---
(function __clojure_fn_5253(){
return (clojure.JS.def(clojure,"long",(function __clojure_fn_5253_long_5258(x_1){
return ((x_1).longValue())})))})();

//======
//(defn float "Coerce to float" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/floatCast)) (clojure/list x))))), :tag Float} [x] (. x (floatValue)))
//---
(function __clojure_fn_5271(){
return (clojure.JS.def(clojure,"float",(function __clojure_fn_5271_float_5276(x_1){
return ((x_1).floatValue())})))})();

//======
//(defn double "Coerce to double" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/doubleCast)) (clojure/list x))))), :tag Double} [x] (. x (doubleValue)))
//---
(function __clojure_fn_5289(){
return (clojure.JS.def(clojure,"double",(function __clojure_fn_5289_double_5294(x_1){
return ((x_1).doubleValue())})))})();

//======
//(defn short "Coerce to short" {:tag Short} [x] (. x (shortValue)))
//---
(function __clojure_fn_5304(){
return (clojure.JS.def(clojure,"short_",(function __clojure_fn_5304_short_5306(x_1){
return ((x_1).shortValue())})))})();

//======
//(defn byte "Coerce to byte" {:tag Byte} [x] (. x (byteValue)))
//---
(function __clojure_fn_5316(){
return (clojure.JS.def(clojure,"byte_",(function __clojure_fn_5316_byte_5318(x_1){
return ((x_1).byteValue())})))})();

//======
//(defn char "Coerce to char" {:tag Character} [x] (. clojure.lang.RT (charCast x)))
//---
(function __clojure_fn_5328(){
return (clojure.JS.def(clojure,"char_",(function __clojure_fn_5328_char_5330(x_1){
return (clojure.lang.RT.charCast(x_1))})))})();

//======
//(defn boolean "Coerce to boolean" {:tag Boolean} [x] (if x true false))
//---
(function __clojure_fn_5340(){
return (clojure.JS.def(clojure,"boolean_",(function __clojure_fn_5340_boolean_5342(x_1){
return (((x_1)?(true):(false)))})))})();

//======
//(defn bigint "Coerce to BigInteger" {:tag BigInteger} [x] (. BigInteger valueOf x))
//---
(function __clojure_fn_5352(){
return (clojure.JS.def(clojure,"bigint",(function __clojure_fn_5352_bigint_5354(x_1){
return (java.math.BigInteger.valueOf(x_1))})))})();

//======
//(defn bigdec "Coerce to BigDecimal" {:tag BigDecimal} [x] (. BigDecimal valueOf x))
//---
(function __clojure_fn_5364(){
return (clojure.JS.def(clojure,"bigdec",(function __clojure_fn_5364_bigdec_5366(x_1){
return (java.math.BigDecimal.valueOf(x_1))})))})();

//======
//(import (quote (java.lang.reflect Array)))
//---
(function __clojure_fn_5373(){
return (clojure.import_(clojure.JS.lit_list(["'java.lang.reflect","'Array"])))})();

//======
//(defn alength "Returns the length of the Java array. Works on arrays of all\n  types." {:inline (fn [a] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/alength)) (clojure/list a)))))} [array] (. clojure.lang.RT (alength array)))
//---
(function __clojure_fn_5385(){
return (clojure.JS.def(clojure,"alength",(function __clojure_fn_5385_alength_5390(array_1){
return (clojure.lang.RT.alength(array_1))})))})();

//======
//(defn aclone "Returns a clone of the Java array. Works on arrays of known\n  types." {:inline (fn [a] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/aclone)) (clojure/list a)))))} [array] (. clojure.lang.RT (aclone array)))
//---
(function __clojure_fn_5403(){
return (clojure.JS.def(clojure,"aclone",(function __clojure_fn_5403_aclone_5408(array_1){
return (clojure.lang.RT.aclone(array_1))})))})();

//======
//(defn aget "Returns the value at the index/indices. Works on Java arrays of all\n  types." {:inline (fn [a i] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/aget)) (clojure/list a) (clojure/list i))))), :inline-arities #{2}} ([array idx] (. Array (get array idx))) ([array idx & idxs] (apply aget (aget array idx) idxs)))
//---
(function __clojure_fn_5422(){
return (clojure.JS.def(clojure,"aget",clojure.JS.variatic(2,(function __clojure_fn_5422_aget_5427(array_1,idx_2){switch(arguments.length){
case 2:return (java.lang.reflect.Array.get(array_1,idx_2))}
var idxs_3,idxs_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.apply(clojure.aget,clojure.lang.RT.aget(array_1,idx_2),idxs_3))}))))})();

//======
//(defn aset "Sets the value at the index/indices. Works on Java arrays of\n  reference types. Returns val." {:inline (fn [a i v] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/aset)) (clojure/list a) (clojure/list i) (clojure/list v))))), :inline-arities #{3}} ([array idx val] (. Array (set array idx val)) val) ([array idx idx2 & idxv] (apply aset (aget array idx) idx2 idxv)))
//---
(function __clojure_fn_5442(){
return (clojure.JS.def(clojure,"aset",clojure.JS.variatic(3,(function __clojure_fn_5442_aset_5447(array_1,idx_2,idx2_3){switch(arguments.length){
case 3:var val_3=arguments[2];
return (java.lang.reflect.Array.set(array_1,idx_2,val_3),
val_3)}
var idxv_4,idxv_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply(clojure.aset,clojure.lang.RT.aget(array_1,idx_2),idx2_3,idxv_4))}))))})();

//======
//(def-aset aset-int setInt int)
//---
(function __clojure_fn_5470(){
return (clojure.JS.def(clojure,"aset_int",clojure.JS.variatic(3,(function __clojure_fn_5470_aset_int_5472(array__912_1,idx__913_2,idx2__915_3){switch(arguments.length){
case 3:var val__914_3=arguments[2];
return (java.lang.reflect.Array.setInt(array__912_1,idx__913_2,clojure.lang.RT.intCast(val__914_3)),
val__914_3)}
var idxv__916_4,idxv__916_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply(clojure.aset_int,clojure.lang.RT.aget(array__912_1,idx__913_2),idx2__915_3,idxv__916_4))}))))})();

//======
//(def-aset aset-long setLong long)
//---
(function __clojure_fn_5484(){
return (clojure.JS.def(clojure,"aset_long",clojure.JS.variatic(3,(function __clojure_fn_5484_aset_long_5486(array__912_1,idx__913_2,idx2__915_3){switch(arguments.length){
case 3:var val__914_3=arguments[2];
return (java.lang.reflect.Array.setLong(array__912_1,idx__913_2,clojure.lang.RT.longCast(val__914_3)),
val__914_3)}
var idxv__916_4,idxv__916_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply(clojure.aset_long,clojure.lang.RT.aget(array__912_1,idx__913_2),idx2__915_3,idxv__916_4))}))))})();

//======
//(def-aset aset-boolean setBoolean boolean)
//---
(function __clojure_fn_5498(){
return (clojure.JS.def(clojure,"aset_boolean",clojure.JS.variatic(3,(function __clojure_fn_5498_aset_boolean_5500(array__912_1,idx__913_2,idx2__915_3){switch(arguments.length){
case 3:var val__914_3=arguments[2];
return (java.lang.reflect.Array.setBoolean(array__912_1,idx__913_2,clojure.boolean_(val__914_3)),
val__914_3)}
var idxv__916_4,idxv__916_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply(clojure.aset_boolean,clojure.lang.RT.aget(array__912_1,idx__913_2),idx2__915_3,idxv__916_4))}))))})();

//======
//(def-aset aset-float setFloat float)
//---
(function __clojure_fn_5512(){
return (clojure.JS.def(clojure,"aset_float",clojure.JS.variatic(3,(function __clojure_fn_5512_aset_float_5514(array__912_1,idx__913_2,idx2__915_3){switch(arguments.length){
case 3:var val__914_3=arguments[2];
return (java.lang.reflect.Array.setFloat(array__912_1,idx__913_2,clojure.lang.RT.floatCast(val__914_3)),
val__914_3)}
var idxv__916_4,idxv__916_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply(clojure.aset_float,clojure.lang.RT.aget(array__912_1,idx__913_2),idx2__915_3,idxv__916_4))}))))})();

//======
//(def-aset aset-double setDouble double)
//---
(function __clojure_fn_5526(){
return (clojure.JS.def(clojure,"aset_double",clojure.JS.variatic(3,(function __clojure_fn_5526_aset_double_5528(array__912_1,idx__913_2,idx2__915_3){switch(arguments.length){
case 3:var val__914_3=arguments[2];
return (java.lang.reflect.Array.setDouble(array__912_1,idx__913_2,clojure.lang.RT.doubleCast(val__914_3)),
val__914_3)}
var idxv__916_4,idxv__916_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply(clojure.aset_double,clojure.lang.RT.aget(array__912_1,idx__913_2),idx2__915_3,idxv__916_4))}))))})();

//======
//(def-aset aset-short setShort short)
//---
(function __clojure_fn_5540(){
return (clojure.JS.def(clojure,"aset_short",clojure.JS.variatic(3,(function __clojure_fn_5540_aset_short_5542(array__912_1,idx__913_2,idx2__915_3){switch(arguments.length){
case 3:var val__914_3=arguments[2];
return (java.lang.reflect.Array.setShort(array__912_1,idx__913_2,clojure.short_(val__914_3)),
val__914_3)}
var idxv__916_4,idxv__916_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply(clojure.aset_short,clojure.lang.RT.aget(array__912_1,idx__913_2),idx2__915_3,idxv__916_4))}))))})();

//======
//(def-aset aset-byte setByte byte)
//---
(function __clojure_fn_5554(){
return (clojure.JS.def(clojure,"aset_byte",clojure.JS.variatic(3,(function __clojure_fn_5554_aset_byte_5556(array__912_1,idx__913_2,idx2__915_3){switch(arguments.length){
case 3:var val__914_3=arguments[2];
return (java.lang.reflect.Array.setByte(array__912_1,idx__913_2,clojure.byte_(val__914_3)),
val__914_3)}
var idxv__916_4,idxv__916_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply(clojure.aset_byte,clojure.lang.RT.aget(array__912_1,idx__913_2),idx2__915_3,idxv__916_4))}))))})();

//======
//(def-aset aset-char setChar char)
//---
(function __clojure_fn_5568(){
return (clojure.JS.def(clojure,"aset_char",clojure.JS.variatic(3,(function __clojure_fn_5568_aset_char_5570(array__912_1,idx__913_2,idx2__915_3){switch(arguments.length){
case 3:var val__914_3=arguments[2];
return (java.lang.reflect.Array.setChar(array__912_1,idx__913_2,clojure.char_(val__914_3)),
val__914_3)}
var idxv__916_4,idxv__916_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply(clojure.aset_char,clojure.lang.RT.aget(array__912_1,idx__913_2),idx2__915_3,idxv__916_4))}))))})();

//======
//(defn make-array "Creates and returns an array of instances of the specified class of\n  the specified dimension(s).  Note that a class object is required.\n  Class objects can be obtained by using their imported or\n  fully-qualified name.  Class objects for the primitive types can be\n  obtained using, e.g., (. Integer TYPE)." ([type len] (. Array (newInstance type (int len)))) ([type dim & more-dims] (let [dims (cons dim more-dims) dimarray (make-array (. Integer TYPE) (count dims))] (dotimes i (alength dimarray) (aset-int dimarray i (nth dims i))) (. Array (newInstance type dimarray)))))
//---
(function __clojure_fn_5582(){
return (clojure.JS.def(clojure,"make_array",clojure.JS.variatic(2,(function __clojure_fn_5582_make_array_5584(type_1,dim_2){switch(arguments.length){
case 2:var len_2=arguments[1];
return (java.lang.reflect.Array.newInstance(type_1,clojure.lang.RT.intCast(len_2)))}
var n__772_6,dimarray_5,dims_4,i_7,more_dims_3,more_dims_3=clojure.JS.rest_args(this,arguments,2);
return (((dims_4=clojure.cons(dim_2,more_dims_3)),
(dimarray_5=clojure.make_array(java.lang.Integer.TYPE,clojure.count(dims_4))),
((n__772_6=clojure.lang.RT.intCast(clojure.lang.RT.alength(dimarray_5))),
((function __loop(){var _rtn,_cnt;(i_7=clojure.lang.RT.intCast(0));do{_cnt=0;
_rtn=((clojure.lang.Numbers.lt(i_7,n__772_6))?(clojure.aset_int(dimarray_5,i_7,clojure.nth(dims_4,i_7)),
(_cnt=1,_t0=clojure.lang.Numbers.unchecked_inc(i_7),i_7=_t0)):(null))}while(_cnt);return _rtn;})())),
java.lang.reflect.Array.newInstance(type_1,dimarray_5)))}))))})();

//======
//(defn to-array-2d "Returns a (potentially-ragged) 2-dimensional array of Objects\n  containing the contents of coll, which can be any Collection of any\n  Collection." [coll] (let [ret (make-array (. Class (forName "[Ljava.lang.Object;")) (. coll (size)))] (loop [i 0 xs (seq coll)] (when xs (aset ret i (to-array (first xs))) (recur (inc i) (rest xs)))) ret))
//---
(function __clojure_fn_5595(){
return (clojure.JS.def(clojure,"to_array_2d",(function __clojure_fn_5595_to_array_2d_5597(coll_1){
var i_3,ret_2,xs_4;
return (((ret_2=clojure.make_array(java.lang.Class.forName("[Ljava.lang.Object;"),(coll_1).size())),
((function __loop(){var _rtn,_cnt;(i_3=0),
(xs_4=clojure.seq(coll_1));do{_cnt=0;
_rtn=((xs_4)?(clojure.lang.RT.aset(ret_2,i_3,clojure.to_array(clojure.first(xs_4))),
(_cnt=1,_t0=clojure.lang.Numbers.inc(i_3),_t1=clojure.rest(xs_4),i_3=_t0,xs_4=_t1)):(null))}while(_cnt);return _rtn;})()),
ret_2))})))})();

//======
//(defn macroexpand-1 "If form represents a macro form, returns its expansion,\n  else returns form." [form] (. clojure.lang.Compiler (macroexpand1 form)))
//---
(function __clojure_fn_5607(){
return (clojure.JS.def(clojure,"macroexpand_1",(function __clojure_fn_5607_macroexpand_1_5609(form_1){
return (clojure.lang.Compiler.macroexpand1(form_1))})))})();

//======
//(defn macroexpand "Repeatedly calls macroexpand-1 on form until it no longer\n  represents a macro form, then returns it.  Note neither\n  macroexpand-1 nor macroexpand expand macros in subforms." [form] (let [ex (macroexpand-1 form)] (if (identical? ex form) form (macroexpand ex))))
//---
(function __clojure_fn_5619(){
return (clojure.JS.def(clojure,"macroexpand",(function __clojure_fn_5619_macroexpand_5621(form_1){
var ex_2;
return (((ex_2=clojure.macroexpand_1(form_1)),
((clojure.identical_QMARK_(ex_2,form_1))?(form_1):(clojure.macroexpand(ex_2)))))})))})();

//======
//(defn create-struct "Returns a structure basis object." [& keys] (. clojure.lang.PersistentStructMap (createSlotMap keys)))
//---
(function __clojure_fn_5631(){
return (clojure.JS.def(clojure,"create_struct",clojure.JS.variatic(0,(function __clojure_fn_5631_create_struct_5633(){
var keys_1,keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentStructMap.createSlotMap(keys_1))}))))})();

//======
//(defn struct-map "Returns a new structmap instance with the keys of the\n  structure-basis. keyvals may contain all, some or none of the basis\n  keys - where values are not supplied they will default to nil.\n  keyvals can also contain keys not in the basis." [s & inits] (. clojure.lang.PersistentStructMap (create s inits)))
//---
(function __clojure_fn_5649(){
return (clojure.JS.def(clojure,"struct_map",clojure.JS.variatic(1,(function __clojure_fn_5649_struct_map_5651(s_1){
var inits_2,inits_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentStructMap.create(s_1,inits_2))}))))})();

//======
//(defn struct "Returns a new structmap instance with the keys of the\n  structure-basis. vals must be supplied for basis keys in order -\n  where values are not supplied they will default to nil." [s & vals] (. clojure.lang.PersistentStructMap (construct s vals)))
//---
(function __clojure_fn_5661(){
return (clojure.JS.def(clojure,"struct",clojure.JS.variatic(1,(function __clojure_fn_5661_struct_5663(s_1){
var vals_2,vals_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentStructMap.construct(s_1,vals_2))}))))})();

//======
//(defn accessor "Returns a fn that, given an instance of a structmap with the basis,\n  returns the value at the key.  The key must be in the basis. The\n  returned function should be (slightly) more efficient than using\n  get, but such use of accessors should be limited to known\n  performance-critical areas." [s key] (. clojure.lang.PersistentStructMap (getAccessor s key)))
//---
(function __clojure_fn_5673(){
return (clojure.JS.def(clojure,"accessor",(function __clojure_fn_5673_accessor_5675(s_1,key_2){
return (clojure.lang.PersistentStructMap.getAccessor(s_1,key_2))})))})();

//======
//(defn subvec "Returns a persistent vector of the items in vector from\n  start (inclusive) to end (exclusive).  If end is not supplied,\n  defaults to (count vector). This operation is O(1) and very fast, as\n  the resulting vector shares structure with the original and no\n  trimming is done." ([v start] (subvec v start (count v))) ([v start end] (. clojure.lang.RT (subvec v start end))))
//---
(function __clojure_fn_5686(){
return (clojure.JS.def(clojure,"subvec",(function __clojure_fn_5686_subvec_5688(v_1,start_2,end_3){switch(arguments.length){
case 2:return (clojure.subvec(v_1,start_2,clojure.count(v_1)))}
return (clojure.lang.RT.subvec(v_1,start_2,end_3))})))})();

//======
//(defn load-reader "Sequentially read and evaluate the set of forms contained in the\n  stream/file" [rdr] (. clojure.lang.Compiler (load rdr)))
//---
(function __clojure_fn_5699(){
return (clojure.JS.def(clojure,"load_reader",(function __clojure_fn_5699_load_reader_5701(rdr_1){
return (clojure.lang.Compiler.load(rdr_1))})))})();

//======
//(defn load-string "Sequentially read and evaluate the set of forms contained in the\n  string" [s] (let [rdr (-> (java.io.StringReader. s) (clojure.lang.LineNumberingPushbackReader.))] (load-reader rdr)))
//---
(function __clojure_fn_5711(){
return (clojure.JS.def(clojure,"load_string",(function __clojure_fn_5711_load_string_5713(s_1){
var rdr_2;
return (((rdr_2=(new clojure.lang.LineNumberingPushbackReader((new java.io.StringReader(s_1))))),
clojure.load_reader(rdr_2)))})))})();

//======
//(defn resultset-seq "Creates and returns a lazy sequence of structmaps corresponding to\n  the rows in the java.sql.ResultSet rs" [rs] (let [rsmeta (. rs (getMetaData)) idxs (range 1 (inc (. rsmeta (getColumnCount)))) keys (map (comp keyword (memfn toLowerCase)) (map (fn [i] (. rsmeta (getColumnName i))) idxs)) row-struct (apply create-struct keys) row-values (fn [] (map (fn [i] (. rs (getObject i))) idxs)) rows (fn thisfn [] (when (. rs (next)) (lazy-cons (apply struct row-struct (row-values)) (thisfn))))] (rows)))
//---
(function __clojure_fn_5742(){
return (clojure.JS.def(clojure,"resultset_seq",(function __clojure_fn_5742_resultset_seq_5744(rs_1){
var rsmeta_2,idxs_3,rows_7,keys_4,row_values_6,row_struct_5;
return (((rsmeta_2=(rs_1).getMetaData()),
(idxs_3=clojure.range(1,clojure.lang.Numbers.inc((rsmeta_2).getColumnCount()))),
(keys_4=clojure.map(clojure.comp(clojure.keyword,(function __clojure_fn_5742_resultset_seq_5744_fn_5746(target__826_1){
return ((target__826_1).toLowerCase())})),clojure.map((function __clojure_fn_5742_resultset_seq_5744_fn_5749(i_1){
return ((rsmeta_2).getColumnName(i_1))}),idxs_3))),
(row_struct_5=clojure.apply(clojure.create_struct,keys_4)),
(row_values_6=(function __clojure_fn_5742_resultset_seq_5744_row_values_5752(){
return (clojure.map((function __clojure_fn_5742_resultset_seq_5744_row_values_5752_fn_5754(i_1){
return ((rs_1).getObject(i_1))}),idxs_3))})),
(rows_7=(function __clojure_fn_5742_resultset_seq_5744_thisfn_5758(){
var thisfn_0=arguments.callee;
return ((((rs_1).next())?((new clojure.lang.LazyCons((function __clojure_fn_5742_resultset_seq_5744_thisfn_5758_fn_5760(G__5759_1){switch(arguments.length){
case 0:return (clojure.apply(clojure.struct,row_struct_5,row_values_6()))}
return (thisfn_0())})))):(null)))})),
rows_7()))})))})();

//======
//(defn set "Returns a set of the distinct elements of coll." [coll] (apply hash-set coll))
//---
(function __clojure_fn_5773(){
return (clojure.JS.def(clojure,"set",(function __clojure_fn_5773_set_5775(coll_1){
return (clojure.apply(clojure.hash_set,coll_1))})))})();

//======
//(defn filter-key [keyfn pred amap] (loop [ret {} es (seq amap)] (if es (if (pred (keyfn (first es))) (recur (assoc ret (key (first es)) (val (first es))) (rest es)) (recur ret (rest es))) ret)))
//---
(function __clojure_fn_5785(){
return (clojure.JS.def(clojure,"filter_key",(function __clojure_fn_5785_filter_key_5787(keyfn_1,pred_2,amap_3){
var es_5,ret_4;
return (((function __loop(){var _rtn,_cnt;(ret_4=clojure.lang.PersistentHashMap.EMPTY),
(es_5=clojure.seq(amap_3));do{_cnt=0;
_rtn=((es_5)?(((pred_2(keyfn_1(clojure.first(es_5))))?((_cnt=1,_t0=clojure.assoc(ret_4,clojure.key(clojure.first(es_5)),clojure.val(clojure.first(es_5))),_t1=clojure.rest(es_5),ret_4=_t0,es_5=_t1)):((_cnt=1,_t0=ret_4,_t1=clojure.rest(es_5),ret_4=_t0,es_5=_t1)))):(ret_4))}while(_cnt);return _rtn;})()))})))})();

//======
//(defn find-ns "Returns the namespace named by the symbol or nil if it doesn't exist." [sym] (clojure.lang.Namespace/find sym))
//---
(function __clojure_fn_5797(){
return (clojure.JS.def(clojure,"find_ns",(function __clojure_fn_5797_find_ns_5799(sym_1){
return (clojure.lang.Namespace.find(sym_1))})))})();

//======
//(defn create-ns "Create a new namespace named by the symbol if one doesn't already\n  exist, returns it or the already-existing namespace of the same\n  name." [sym] (clojure.lang.Namespace/findOrCreate sym))
//---
(function __clojure_fn_5809(){
return (clojure.JS.def(clojure,"create_ns",(function __clojure_fn_5809_create_ns_5811(sym_1){
return (clojure.lang.Namespace.findOrCreate(sym_1))})))})();

//======
//(defn remove-ns "Removes the namespace named by the symbol. Use with caution.\n  Cannot be used to remove the clojure namespace." [sym] (clojure.lang.Namespace/remove sym))
//---
(function __clojure_fn_5821(){
return (clojure.JS.def(clojure,"remove_ns",(function __clojure_fn_5821_remove_ns_5823(sym_1){
return (clojure.lang.Namespace.remove(sym_1))})))})();

//======
//(defn all-ns "Returns a sequence of all namespaces." [] (clojure.lang.Namespace/all))
//---
(function __clojure_fn_5833(){
return (clojure.JS.def(clojure,"all_ns",(function __clojure_fn_5833_all_ns_5835(){
return (clojure.lang.Namespace.all())})))})();

//======
//(defn the-ns [x] (if (instance? clojure.lang.Namespace x) x (or (find-ns x) (throw (Exception. (str "No namespace: " x " found"))))))
//---
(function __clojure_fn_5845(){
return (clojure.JS.def(clojure,"the_ns",(function __clojure_fn_5845_the_ns_5847(x_1){
var or__202_2;
return (((clojure.instance_QMARK_(clojure.lang.Namespace,x_1))?(x_1):(((or__202_2=clojure.find_ns(x_1)),
((or__202_2)?(or__202_2):((function __throw(){throw (new java.lang.Exception(clojure.str("No namespace: ",x_1," found")))})()))))))})))})();

//======
//(defn ns-name "Returns the name of the namespace, a symbol." [ns] (.getName (the-ns ns)))
//---
(function __clojure_fn_5857(){
return (clojure.JS.def(clojure,"ns_name",(function __clojure_fn_5857_ns_name_5859(ns_1){
return ((clojure.the_ns(ns_1)).getName())})))})();

//======
//(defn ns-map "Returns a map of all the mappings for the namespace." [ns] (.getMappings (the-ns ns)))
//---
(function __clojure_fn_5869(){
return (clojure.JS.def(clojure,"ns_map",(function __clojure_fn_5869_ns_map_5871(ns_1){
return ((clojure.the_ns(ns_1)).getMappings())})))})();

//======
//(defn ns-unmap "Removes the mappings for the symbol from the namespace." [ns sym] (.unmap (the-ns ns) sym))
//---
(function __clojure_fn_5881(){
return (clojure.JS.def(clojure,"ns_unmap",(function __clojure_fn_5881_ns_unmap_5883(ns_1,sym_2){
return ((clojure.the_ns(ns_1)).unmap(sym_2))})))})();

//======
//(defn ns-publics "Returns a map of the public intern mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (= ns (.ns v)) (.isPublic v))) (ns-map ns))))
//---
(function __clojure_fn_5896(){
return (clojure.JS.def(clojure,"ns_publics",(function __clojure_fn_5896_ns_publics_5898(ns_1){
var ns_2;
return (((ns_2=clojure.the_ns(ns_1)),
clojure.filter_key(clojure.val,(function __clojure_fn_5896_ns_publics_5898_fn_5900(v_1){
var and__196_2,and__196_3;
return (((and__196_2=clojure.instance_QMARK_(clojure.lang.Var,v_1)),
((and__196_2)?(((and__196_3=clojure.lang.Util.equal(ns_2,(v_1).ns)),
((and__196_3)?((v_1).isPublic()):(and__196_3)))):(and__196_2))))}),clojure.ns_map(ns_2))))})))})();

//======
//(defn ns-imports "Returns a map of the import mappings for the namespace." [ns] (filter-key val (partial instance? Class) (ns-map ns)))
//---
(function __clojure_fn_5911(){
return (clojure.JS.def(clojure,"ns_imports",(function __clojure_fn_5911_ns_imports_5913(ns_1){
return (clojure.filter_key(clojure.val,clojure.partial(clojure.instance_QMARK_,java.lang.Class),clojure.ns_map(ns_1)))})))})();

//======
//(defn ns-refers "Returns a map of the refer mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (not= ns (.ns v)))) (ns-map ns))))
//---
(function __clojure_fn_5932(){
return (clojure.JS.def(clojure,"ns_refers",(function __clojure_fn_5932_ns_refers_5934(ns_1){
var ns_2;
return (((ns_2=clojure.the_ns(ns_1)),
clojure.filter_key(clojure.val,(function __clojure_fn_5932_ns_refers_5934_fn_5936(v_1){
var and__196_2;
return (((and__196_2=clojure.instance_QMARK_(clojure.lang.Var,v_1)),
((and__196_2)?(clojure.not_EQ_(ns_2,(v_1).ns)):(and__196_2))))}),clojure.ns_map(ns_2))))})))})();

//======
//(defn ns-interns "Returns a map of the intern mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (= ns (.ns v)))) (ns-map ns))))
//---
(function __clojure_fn_5950(){
return (clojure.JS.def(clojure,"ns_interns",(function __clojure_fn_5950_ns_interns_5952(ns_1){
var ns_2;
return (((ns_2=clojure.the_ns(ns_1)),
clojure.filter_key(clojure.val,(function __clojure_fn_5950_ns_interns_5952_fn_5954(v_1){
var and__196_2;
return (((and__196_2=clojure.instance_QMARK_(clojure.lang.Var,v_1)),
((and__196_2)?(clojure.lang.Util.equal(ns_2,(v_1).ns)):(and__196_2))))}),clojure.ns_map(ns_2))))})))})();

//======
//(defn alias "Add an alias in the current namespace to another\n  namespace. Arguments are two symbols: the alias to be used, and\n  the symbolic name of the target namespace. Use :as in the ns macro in preference \n  to calling this directly." [alias namespace-sym] (.addAlias *ns* alias (find-ns namespace-sym)))
//---
(function __clojure_fn_5965(){
return (clojure.JS.def(clojure,"alias",(function __clojure_fn_5965_alias_5967(alias_1,namespace_sym_2){
return ((clojure._STAR_ns_STAR_).addAlias(alias_1,clojure.find_ns(namespace_sym_2)))})))})();

//======
//(defn ns-aliases "Returns a map of the aliases for the namespace." [ns] (.getAliases (the-ns ns)))
//---
(function __clojure_fn_5977(){
return (clojure.JS.def(clojure,"ns_aliases",(function __clojure_fn_5977_ns_aliases_5979(ns_1){
return ((clojure.the_ns(ns_1)).getAliases())})))})();

//======
//(defn ns-unalias "Removes the alias for the symbol from the namespace." [ns sym] (.removeAlias (the-ns ns) sym))
//---
(function __clojure_fn_5989(){
return (clojure.JS.def(clojure,"ns_unalias",(function __clojure_fn_5989_ns_unalias_5991(ns_1,sym_2){
return ((clojure.the_ns(ns_1)).removeAlias(sym_2))})))})();

//======
//(defn take-nth "Returns a lazy seq of every nth item in coll." [n coll] (when (seq coll) (lazy-cons (first coll) (take-nth n (drop n coll)))))
//---
(function __clojure_fn_6006(){
return (clojure.JS.def(clojure,"take_nth",(function __clojure_fn_6006_take_nth_6008(n_1,coll_2){
return (((clojure.seq(coll_2))?((new clojure.lang.LazyCons((function __clojure_fn_6006_take_nth_6008_fn_6011(G__6010_1){switch(arguments.length){
case 0:return (clojure.first(coll_2))}
return (clojure.take_nth(n_1,clojure.drop(n_1,coll_2)))})))):(null)))})))})();

//======
//(defn interleave "Returns a lazy seq of the first item in each coll, then the second\n  etc." [& colls] (apply concat (apply map list colls)))
//---
(function __clojure_fn_6023(){
return (clojure.JS.def(clojure,"interleave",clojure.JS.variatic(0,(function __clojure_fn_6023_interleave_6025(){
var colls_1,colls_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply(clojure.concat,clojure.apply(clojure.map,clojure.list,colls_1)))}))))})();

//======
//(defn var-get "Gets the value in the var object" [x] (. x (get)))
//---
(function __clojure_fn_6035(){
return (clojure.JS.def(clojure,"var_get",(function __clojure_fn_6035_var_get_6037(x_1){
return ((x_1).get())})))})();

//======
//(defn var-set "Sets the value in the var object to val. The var must be\n thread-locally bound." [x val] (. x (set val)))
//---
(function __clojure_fn_6047(){
return (clojure.JS.def(clojure,"var_set",(function __clojure_fn_6047_var_set_6049(x_1,val_2){
return ((x_1).set(val_2))})))})();

//======
//(defn ns-resolve "Returns the var or Class to which a symbol will be resolved in the\n  namespace, else nil.  Note that if the symbol is fully qualified,\n  the var/Class to which it resolves need not be present in the\n  namespace." [ns sym] (clojure.lang.Compiler/maybeResolveIn (the-ns ns) sym))
//---
(function __clojure_fn_6065(){
return (clojure.JS.def(clojure,"ns_resolve",(function __clojure_fn_6065_ns_resolve_6067(ns_1,sym_2){
return (clojure.lang.Compiler.maybeResolveIn(clojure.the_ns(ns_1),sym_2))})))})();

//======
//(defn resolve "same as (ns-resolve *ns* symbol)" [sym] (ns-resolve *ns* sym))
//---
(function __clojure_fn_6077(){
return (clojure.JS.def(clojure,"resolve",(function __clojure_fn_6077_resolve_6079(sym_1){
return (clojure.ns_resolve(clojure._STAR_ns_STAR_,sym_1))})))})();

//======
//(defn array-map "Constructs an array-map." ([] (. clojure.lang.PersistentArrayMap EMPTY)) ([& keyvals] (new clojure.lang.PersistentArrayMap (to-array keyvals))))
//---
(function __clojure_fn_6090(){
return (clojure.JS.def(clojure,"array_map",clojure.JS.variatic(0,(function __clojure_fn_6090_array_map_6092(){switch(arguments.length){
case 0:return (clojure.lang.PersistentArrayMap.EMPTY)}
var keyvals_1,keyvals_1=clojure.JS.rest_args(this,arguments,0);
return ((new clojure.lang.PersistentArrayMap(clojure.to_array(keyvals_1))))}))))})();

//======
//(defn nthrest "Returns the nth rest of coll, (seq coll) when n is 0." [coll n] (loop [n n xs (seq coll)] (if (and xs (pos? n)) (recur (dec n) (rest xs)) xs)))
//---
(function __clojure_fn_6103(){
return (clojure.JS.def(clojure,"nthrest",(function __clojure_fn_6103_nthrest_6105(coll_1,n_2){
var n_3,and__196_5,xs_4;
return (((function __loop(){var _rtn,_cnt;(n_3=n_2),
(xs_4=clojure.seq(coll_1));do{_cnt=0;
_rtn=((((and__196_5=xs_4),
((and__196_5)?(clojure.lang.Numbers.isPos(n_3)):(and__196_5))))?((_cnt=1,_t0=clojure.lang.Numbers.dec(n_3),_t1=clojure.rest(xs_4),n_3=_t0,xs_4=_t1)):(xs_4))}while(_cnt);return _rtn;})()))})))})();

//======
//(defn symbol? "Return true if x is a Symbol" [x] (instance? clojure.lang.Symbol x))
//---
(function __clojure_fn_6115(){
return (clojure.JS.def(clojure,"symbol_QMARK_",(function __clojure_fn_6115_symbol_QMARK_6117(x_1){
return (clojure.instance_QMARK_(clojure.lang.Symbol,x_1))})))})();

//======
//(defn keyword? "Return true if x is a Keyword" [x] (instance? clojure.lang.Keyword x))
//---
(function __clojure_fn_6127(){
return (clojure.JS.def(clojure,"keyword_QMARK_",(function __clojure_fn_6127_keyword_QMARK_6129(x_1){
return (clojure.instance_QMARK_(clojure.lang.Keyword,x_1))})))})();

//======
//(defn destructure [bindings] (let [bmap (apply array-map bindings) pb (fn pb [bvec b v] (let [pvec (fn [bvec b val] (let [gvec (gensym "vec__")] (loop [ret (-> bvec (conj gvec) (conj val)) n 0 bs b seen-rest? false] (if bs (let [firstb (first bs)] (cond (= firstb (quote &)) (recur (pb ret (second bs) (list (quote clojure/nthrest) gvec n)) n (rrest bs) true) (= firstb :as) (pb ret (second bs) gvec) :else (if seen-rest? (throw (new Exception "Unsupported binding form, only :as can follow & parameter")) (recur (pb ret firstb (list (quote clojure/nth) gvec n nil)) (inc n) (rest bs) seen-rest?)))) ret)))) pmap (fn [bvec b v] (let [gmap (or (:as b) (gensym "map__")) defaults (:or b)] (loop [ret (-> bvec (conj gmap) (conj v)) bes (reduce (fn [bes entry] (reduce (fn* [p1__6133 p2__6134] (assoc p1__6133 p2__6134 ((val entry) p2__6134))) (dissoc bes (key entry)) ((key entry) bes))) (dissoc b :as :or) {:keys (fn* [p1__6135] (keyword (str p1__6135))), :strs str, :syms (fn* [p1__6136] (list (quote quote) p1__6136))})] (if bes (let [bb (key (first bes)) bk (val (first bes)) has-default (contains? defaults bb)] (recur (pb ret bb (if has-default (list (quote clojure/get) gmap bk (defaults bb)) (list (quote clojure/get) gmap bk))) (rest bes))) ret))))] (cond (symbol? b) (-> bvec (conj b) (conj v)) (vector? b) (pvec bvec b v) (map? b) (pmap bvec b v) :else (throw (new Exception (str "Unsupported binding form: " b)))))) process-entry (fn [bvec b] (pb bvec (key b) (val b)))] (if (every? symbol? (keys bmap)) bindings (reduce process-entry [] bmap))))
//---
(function __clojure_fn_6166(){
return (clojure.JS.def(clojure,"destructure",(function __clojure_fn_6166_destructure_6168(bindings_1){
var process_entry_4,bmap_2,pb_3;
return (((bmap_2=clojure.apply(clojure.array_map,bindings_1)),
(pb_3=(function __clojure_fn_6166_destructure_6168_pb_6170(bvec_1,b_2,v_3){
var pvec_4,pmap_5,pb_0=arguments.callee;
return (((pvec_4=(function __clojure_fn_6166_destructure_6168_pb_6170_pvec_6171(bvec_1,b_2,val_3){
var gvec_4,ret_5,seen_rest_QMARK__8,firstb_9,bs_7,n_6;
return (((gvec_4=clojure.gensym("vec__")),
((function __loop(){var _rtn,_cnt;(ret_5=clojure.conj(clojure.conj(bvec_1,gvec_4),val_3)),
(n_6=0),
(bs_7=b_2),
(seen_rest_QMARK__8=false);do{_cnt=0;
_rtn=((bs_7)?(((firstb_9=clojure.first(bs_7)),
((clojure.lang.Util.equal(firstb_9,"'&"))?((_cnt=1,_t0=pb_0(ret_5,clojure.second(bs_7),clojure.list("'clojure/nthrest",gvec_4,n_6)),_t1=n_6,_t2=clojure.rrest(bs_7),_t3=true,ret_5=_t0,n_6=_t1,bs_7=_t2,seen_rest_QMARK__8=_t3)):(((clojure.lang.Util.equal(firstb_9,":as"))?(pb_0(ret_5,clojure.second(bs_7),gvec_4)):(((":else")?(((seen_rest_QMARK__8)?((function __throw(){throw (new java.lang.Exception("Unsupported binding form, only :as can follow & parameter"))})()):((_cnt=1,_t0=pb_0(ret_5,firstb_9,clojure.list("'clojure/nth",gvec_4,n_6,null)),_t1=clojure.lang.Numbers.inc(n_6),_t2=clojure.rest(bs_7),_t3=seen_rest_QMARK__8,ret_5=_t0,n_6=_t1,bs_7=_t2,seen_rest_QMARK__8=_t3)))):(null)))))))):(ret_5))}while(_cnt);return _rtn;})())))})),
(pmap_5=(function __clojure_fn_6166_destructure_6168_pb_6170_pmap_6174(bvec_1,b_2,v_3){
var bes_7,or__202_4,ret_6,has_default_10,bk_9,bb_8,defaults_5,gmap_4;
return (((gmap_4=((or__202_4=":as"(b_2)),
((or__202_4)?(or__202_4):(clojure.gensym("map__"))))),
(defaults_5=":or"(b_2)),
((function __loop(){var _rtn,_cnt;(ret_6=clojure.conj(clojure.conj(bvec_1,gmap_4),v_3)),
(bes_7=clojure.reduce((function __clojure_fn_6166_destructure_6168_pb_6170_pmap_6174_fn_6176(bes_1,entry_2){
return (clojure.reduce((function __clojure_fn_6166_destructure_6168_pb_6170_pmap_6174_fn_6176_fn_6178(p1__6133_1,p2__6134_2){
return (clojure.assoc(p1__6133_1,p2__6134_2,clojure.val(entry_2)(p2__6134_2)))}),clojure.dissoc(bes_1,clojure.key(entry_2)),clojure.key(entry_2)(bes_1)))}),clojure.dissoc(b_2,":as",":or"),clojure.lang.HashMap.create([":keys",(function __clojure_fn_6166_destructure_6168_pb_6170_pmap_6174_fn_6182(p1__6135_1){
return (clojure.keyword(clojure.str(p1__6135_1)))}),":strs",clojure.str,":syms",(function __clojure_fn_6166_destructure_6168_pb_6170_pmap_6174_fn_6185(p1__6136_1){
return (clojure.list("'quote",p1__6136_1))})])));do{_cnt=0;
_rtn=((bes_7)?(((bb_8=clojure.key(clojure.first(bes_7))),
(bk_9=clojure.val(clojure.first(bes_7))),
(has_default_10=clojure.contains_QMARK_(defaults_5,bb_8)),
(_cnt=1,_t0=pb_0(ret_6,bb_8,((has_default_10)?(clojure.list("'clojure/get",gmap_4,bk_9,defaults_5(bb_8))):(clojure.list("'clojure/get",gmap_4,bk_9)))),_t1=clojure.rest(bes_7),ret_6=_t0,bes_7=_t1))):(ret_6))}while(_cnt);return _rtn;})())))})),
((clojure.symbol_QMARK_(b_2))?(clojure.conj(clojure.conj(bvec_1,b_2),v_3)):(((clojure.vector_QMARK_(b_2))?(pvec_4(bvec_1,b_2,v_3)):(((clojure.map_QMARK_(b_2))?(pmap_5(bvec_1,b_2,v_3)):(((":else")?((function __throw(){throw (new java.lang.Exception(clojure.str("Unsupported binding form: ",b_2)))})()):(null))))))))))})),
(process_entry_4=(function __clojure_fn_6166_destructure_6168_process_entry_6190(bvec_1,b_2){
return (pb_3(bvec_1,clojure.key(b_2),clojure.val(b_2)))})),
((clojure.every_QMARK_(clojure.symbol_QMARK_,clojure.keys(bmap_2)))?(bindings_1):(clojure.reduce(process_entry_4,clojure.lang.PersistentVector.EMPTY,bmap_2)))))})))})();

//======
//(defn pr-str "pr to a string, returning it" {:tag String} [& xs] (with-out-str (apply pr xs)))
//---
(function __clojure_fn_6291(){
return (clojure.JS.def(clojure,"pr_str",clojure.JS.variatic(0,(function __clojure_fn_6291_pr_str_6293(){
var s__1199_2,xs_1,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1199_2=(new java.io.StringWriter())),
clojure.lang.Var.pushThreadBindings(clojure.hash_map(clojure._var__STAR_out_STAR_,s__1199_2)),
(function __try(){try{var _rtn=(clojure.apply(clojure.pr,xs_1),
clojure.str(s__1199_2))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))})();

//======
//(defn prn-str "prn to a string, returning it" {:tag String} [& xs] (with-out-str (apply prn xs)))
//---
(function __clojure_fn_6303(){
return (clojure.JS.def(clojure,"prn_str",clojure.JS.variatic(0,(function __clojure_fn_6303_prn_str_6305(){
var s__1199_2,xs_1,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1199_2=(new java.io.StringWriter())),
clojure.lang.Var.pushThreadBindings(clojure.hash_map(clojure._var__STAR_out_STAR_,s__1199_2)),
(function __try(){try{var _rtn=(clojure.apply(clojure.prn,xs_1),
clojure.str(s__1199_2))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))})();

//======
//(defn print-str "print to a string, returning it" {:tag String} [& xs] (with-out-str (apply print xs)))
//---
(function __clojure_fn_6315(){
return (clojure.JS.def(clojure,"print_str",clojure.JS.variatic(0,(function __clojure_fn_6315_print_str_6317(){
var xs_1,s__1199_2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1199_2=(new java.io.StringWriter())),
clojure.lang.Var.pushThreadBindings(clojure.hash_map(clojure._var__STAR_out_STAR_,s__1199_2)),
(function __try(){try{var _rtn=(clojure.apply(clojure.print,xs_1),
clojure.str(s__1199_2))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))})();

//======
//(defn println-str "println to a string, returning it" {:tag String} [& xs] (with-out-str (apply println xs)))
//---
(function __clojure_fn_6327(){
return (clojure.JS.def(clojure,"println_str",clojure.JS.variatic(0,(function __clojure_fn_6327_println_str_6329(){
var s__1199_2,xs_1,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1199_2=(new java.io.StringWriter())),
clojure.lang.Var.pushThreadBindings(clojure.hash_map(clojure._var__STAR_out_STAR_,s__1199_2)),
(function __try(){try{var _rtn=(clojure.apply(clojure.println,xs_1),
clojure.str(s__1199_2))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))})();

//======
//(defn test "test [v] finds fn at key :test in var metadata and calls it,\n  presuming failure will throw exception" [v] (let [f (:test (clojure/meta v))] (if f (do (f) :ok) :no-test)))
//---
(function __clojure_fn_6345(){
return (clojure.JS.def(clojure,"test",(function __clojure_fn_6345_test_6347(v_1){
var f_2;
return (((f_2=":test"(clojure.meta(v_1))),
((f_2)?(f_2(),
":ok"):(":no-test"))))})))})();

//======
//(defn re-pattern "Returns an instance of java.util.regex.Pattern, for use, e.g. in\n  re-matcher." {:tag java.util.regex.Pattern} [s] (. java.util.regex.Pattern (compile s)))
//---
(function __clojure_fn_6357(){
return (clojure.JS.def(clojure,"re_pattern",(function __clojure_fn_6357_re_pattern_6359(s_1){
return (java.util.regex.Pattern.compile(s_1))})))})();

//======
//(defn re-matcher "Returns an instance of java.util.regex.Matcher, for use, e.g. in\n  re-find." {:tag java.util.regex.Matcher} [re s] (. re (matcher s)))
//---
(function __clojure_fn_6369(){
return (clojure.JS.def(clojure,"re_matcher",(function __clojure_fn_6369_re_matcher_6371(re_1,s_2){
return ((re_1).matcher(s_2))})))})();

//======
//(defn re-groups "Returns the groups from the most recent match/find. If there are no\n  nested groups, returns a string of the entire match. If there are\n  nested groups, returns a vector of the groups, the first element\n  being the entire match." [m] (let [gc (. m (groupCount))] (if (zero? gc) (. m (group)) (loop [ret [] c 0] (if (<= c gc) (recur (conj ret (. m (group c))) (inc c)) ret)))))
//---
(function __clojure_fn_6381(){
return (clojure.JS.def(clojure,"re_groups",(function __clojure_fn_6381_re_groups_6383(m_1){
var ret_3,gc_2,c_4;
return (((gc_2=(m_1).groupCount()),
((clojure.lang.Numbers.isZero(gc_2))?((m_1).group()):(((function __loop(){var _rtn,_cnt;(ret_3=clojure.lang.PersistentVector.EMPTY),
(c_4=0);do{_cnt=0;
_rtn=((clojure.lang.Numbers.lte(c_4,gc_2))?((_cnt=1,_t0=clojure.conj(ret_3,(m_1).group(c_4)),_t1=clojure.lang.Numbers.inc(c_4),ret_3=_t0,c_4=_t1)):(ret_3))}while(_cnt);return _rtn;})())))))})))})();

//======
//(defn re-seq "Returns a lazy sequence of successive matches of pattern in string,\n  using java.util.regex.Matcher.find(), each such match processed with\n  re-groups." [re s] (let [m (re-matcher re s)] ((fn step [] (when (. m (find)) (lazy-cons (re-groups m) (step)))))))
//---
(function __clojure_fn_6400(){
return (clojure.JS.def(clojure,"re_seq",(function __clojure_fn_6400_re_seq_6402(re_1,s_2){
var m_3;
return (((m_3=clojure.re_matcher(re_1,s_2)),
(function __clojure_fn_6400_re_seq_6402_step_6404(){
var step_0=arguments.callee;
return ((((m_3).find())?((new clojure.lang.LazyCons((function __clojure_fn_6400_re_seq_6402_step_6404_fn_6406(G__6405_1){switch(arguments.length){
case 0:return (clojure.re_groups(m_3))}
return (step_0())})))):(null)))})()))})))})();

//======
//(defn re-matches "Returns the match, if any, of string to pattern, using\n  java.util.regex.Matcher.matches().  Uses re-groups to return the\n  groups." [re s] (let [m (re-matcher re s)] (when (. m (matches)) (re-groups m))))
//---
(function __clojure_fn_6419(){
return (clojure.JS.def(clojure,"re_matches",(function __clojure_fn_6419_re_matches_6421(re_1,s_2){
var m_3;
return (((m_3=clojure.re_matcher(re_1,s_2)),
(((m_3).matches())?(clojure.re_groups(m_3)):(null))))})))})();

//======
//(defn re-find "Returns the next regex match, if any, of string to pattern, using\n  java.util.regex.Matcher.find().  Uses re-groups to return the\n  groups." ([m] (when (. m (find)) (re-groups m))) ([re s] (let [m (re-matcher re s)] (re-find m))))
//---
(function __clojure_fn_6432(){
return (clojure.JS.def(clojure,"re_find",(function __clojure_fn_6432_re_find_6434(re_1,s_2){switch(arguments.length){
case 1:var m_1=arguments[0];
return ((((m_1).find())?(clojure.re_groups(m_1)):(null)))}
var m_3;
return (((m_3=clojure.re_matcher(re_1,s_2)),
clojure.re_find(m_3)))})))})();

//======
//(defn rand "Returns a random floating point number between 0 (inclusive) and\n  1 (exclusive)." ([] (. Math (random))) ([n] (* n (rand))))
//---
(function __clojure_fn_6446(){
return (clojure.JS.def(clojure,"rand",(function __clojure_fn_6446_rand_6448(n_1){switch(arguments.length){
case 0:return (java.lang.Math.random())}
return (clojure.lang.Numbers.multiply(n_1,clojure.rand()))})))})();

//======
//(defn rand-int "Returns a random integer between 0 (inclusive) and n (exclusive)." [n] (int (rand n)))
//---
(function __clojure_fn_6459(){
return (clojure.JS.def(clojure,"rand_int",(function __clojure_fn_6459_rand_int_6461(n_1){
return (clojure.lang.RT.intCast(clojure.rand(n_1)))})))})();

//======
//(defn print-doc [v] (println "-------------------------") (println (str (ns-name (:ns (clojure/meta v))) "/" (:name (clojure/meta v)))) (prn (:arglists (clojure/meta v))) (when (:macro (clojure/meta v)) (println "Macro")) (println " " (:doc (clojure/meta v))))
//---
(function __clojure_fn_6477(){
return (clojure.JS.def(clojure,"print_doc",(function __clojure_fn_6477_print_doc_6479(v_1){
return (clojure.println("-------------------------"),
clojure.println(clojure.str(clojure.ns_name(":ns"(clojure.meta(v_1))),"/",":name"(clojure.meta(v_1)))),
clojure.prn(":arglists"(clojure.meta(v_1))),
((":macro"(clojure.meta(v_1)))?(clojure.println("Macro")):(null)),
clojure.println(" ",":doc"(clojure.meta(v_1))))})))})();

//======
//(defn find-doc "Prints documentation for any var whose documentation or name\n contains a match for re-string" [re-string] (let [re (re-pattern re-string)] (dorun (for [ns (all-ns) v (sort-by (comp :name meta) (vals (ns-interns ns))) :when (and (:doc (clojure/meta v)) (or (re-find (re-matcher re (:doc (clojure/meta v)))) (re-find (re-matcher re (str (:name (clojure/meta v)))))))] (print-doc v)))))
//---
(function __clojure_fn_6509(){
return (clojure.JS.def(clojure,"find_doc",(function __clojure_fn_6509_find_doc_6511(re_string_1){
var re_2,iter__1175_3;
return (((re_2=clojure.re_pattern(re_string_1)),
clojure.dorun(((iter__1175_3=(function __clojure_fn_6509_find_doc_6511_iter_6513_6517(s__6514_1){
var iter__1168_4,ns_2,iterys__1174_3,iter__6513_0=arguments.callee;
return (((clojure.seq(s__6514_1))?(((ns_2=clojure.first(s__6514_1)),
((true)?(((iterys__1174_3=(function __clojure_fn_6509_find_doc_6511_iter_6513_6517_iter_6515_6518(s__6516_1){
var _cnt,_rtn,v_2,and__196_3,or__202_4,iter__6515_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.seq(s__6516_1))?(((v_2=clojure.first(s__6516_1)),
((((and__196_3=":doc"(clojure.meta(v_2))),
((and__196_3)?(((or__202_4=clojure.re_find(clojure.re_matcher(re_2,":doc"(clojure.meta(v_2))))),
((or__202_4)?(or__202_4):(clojure.re_find(clojure.re_matcher(re_2,clojure.str(":name"(clojure.meta(v_2))))))))):(and__196_3))))?((new clojure.lang.LazyCons((function __clojure_fn_6509_find_doc_6511_iter_6513_6517_iter_6515_6518_fn_6520(G__6519_1){switch(arguments.length){
case 0:return (clojure.print_doc(v_2))}
return (iter__6515_0(clojure.rest(s__6516_1)))})))):((_cnt=1,_t0=clojure.rest(s__6516_1),s__6516_1=_t0))))):(null))
}while(_cnt);return _rtn;})),
((iter__1168_4=(function __clojure_fn_6509_find_doc_6511_iter_6513_6517_iter_1168_6525(coll__1169_1){
var iter__1168_0=arguments.callee;
return (((clojure.seq(coll__1169_1))?((new clojure.lang.LazyCons((function __clojure_fn_6509_find_doc_6511_iter_6513_6517_iter_1168_6525_fn_6527(G__6526_1){switch(arguments.length){
case 0:return (clojure.first(coll__1169_1))}
return (iter__1168_0(clojure.rest(coll__1169_1)))})))):(clojure.seq(iter__6513_0(clojure.rest(s__6514_1))))))})),
iter__1168_4(iterys__1174_3(clojure.sort_by(clojure.comp(":name",clojure.meta),clojure.vals(clojure.ns_interns(ns_2)))))))):(null)))):(null)))})),
iter__1175_3(clojure.all_ns())))))})))})();

//======
//(defn special-form-anchor "Returns the anchor tag on http://clojure.org/special_forms for the\n  special form x, or nil" [x] (#{(quote recur) (quote .) (quote var) (quote let) (quote quote) (quote set!) (quote monitor-enter) (quote loop) (quote new) (quote fn) (quote if) (quote try) (quote def) (quote monitor-exit) (quote throw) (quote do)} x))
//---
(function __clojure_fn_6541(){
return (clojure.JS.def(clojure,"special_form_anchor",(function __clojure_fn_6541_special_form_anchor_6543(x_1){
return (clojure.lang.HashSet.create(["'recur","'.","'var","'let","'quote","'set!","'monitor-enter","'loop","'new","'fn","'if","'try","'def","'monitor-exit","'throw","'do"])(x_1))})))})();

//======
//(defn syntax-symbol-anchor "Returns the anchor tag on http://clojure.org/special_forms for the\n  special form that uses syntax symbol x, or nil" [x] ({(quote &) (quote fn), (quote catch) (quote try), (quote finally) (quote try)} x))
//---
(function __clojure_fn_6553(){
return (clojure.JS.def(clojure,"syntax_symbol_anchor",(function __clojure_fn_6553_syntax_symbol_anchor_6555(x_1){
return (clojure.lang.HashMap.create(["'&","'fn","'catch","'try","'finally","'try"])(x_1))})))})();

//======
//(defn print-special-doc [name type anchor] (println "-------------------------") (println name) (println type) (println (str "  Please see http://clojure.org/special_forms#" anchor)))
//---
(function __clojure_fn_6565(){
return (clojure.JS.def(clojure,"print_special_doc",(function __clojure_fn_6565_print_special_doc_6567(name_1,type_2,anchor_3){
return (clojure.println("-------------------------"),
clojure.println(name_1),
clojure.println(type_2),
clojure.println(clojure.str("  Please see http://clojure.org/special_forms#",anchor_3)))})))})();

//======
//(defn tree-seq "returns a lazy sequence of the nodes in a tree, via a depth-first walk.\n  branch? must be a fn of one arg that returns true if passed a node\n  that can have children (but may not).  children must be a fn of one\n  arg that returns a sequence of the children. Will only be called on\n  nodes for which branch? returns true. Root is the root node of the\n  tree, must be a branch." [branch? children root] (let [walk (fn walk [nodes] (when-first node nodes (lazy-cons node (if (branch? node) (lazy-cat (walk (children node)) (walk (rest nodes))) (walk (rest nodes))))))] (lazy-cons root (walk (children root)))))
//---
(function __clojure_fn_6602(){
return (clojure.JS.def(clojure,"tree_seq",(function __clojure_fn_6602_tree_seq_6604(branch_QMARK__1,children_2,root_3){
var walk_4;
return (((walk_4=(function __clojure_fn_6602_tree_seq_6604_walk_6606(nodes_1){
var node_2,walk_0=arguments.callee;
return (((clojure.seq(nodes_1))?(((node_2=clojure.first(nodes_1)),
(new clojure.lang.LazyCons((function __clojure_fn_6602_tree_seq_6604_walk_6606_fn_6608(G__6607_1){switch(arguments.length){
case 0:return (node_2)}
var iter__1168_2;
return (((branch_QMARK__1(node_2))?(((iter__1168_2=(function __clojure_fn_6602_tree_seq_6604_walk_6606_fn_6608_iter_1168_6611(coll__1169_1){
var iter__1168_0=arguments.callee;
return (((clojure.seq(coll__1169_1))?((new clojure.lang.LazyCons((function __clojure_fn_6602_tree_seq_6604_walk_6606_fn_6608_iter_1168_6611_fn_6613(G__6612_1){switch(arguments.length){
case 0:return (clojure.first(coll__1169_1))}
return (iter__1168_0(clojure.rest(coll__1169_1)))})))):(clojure.seq(walk_0(clojure.rest(nodes_1))))))})),
iter__1168_2(walk_0(children_2(node_2))))):(walk_0(clojure.rest(nodes_1)))))}))))):(null)))})),
(new clojure.lang.LazyCons((function __clojure_fn_6602_tree_seq_6604_fn_6621(G__6620_1){switch(arguments.length){
case 0:return (root_3)}
return (walk_4(children_2(root_3)))})))))})))})();

//======
//(defn file-seq "A tree seq on java.io.Files" [dir] (tree-seq (fn [f] (. f (isDirectory))) (fn [d] (seq (. d (listFiles)))) dir))
//---
(function __clojure_fn_6639(){
return (clojure.JS.def(clojure,"file_seq",(function __clojure_fn_6639_file_seq_6641(dir_1){
return (clojure.tree_seq((function __clojure_fn_6639_file_seq_6641_fn_6643(f_1){
return ((f_1).isDirectory())}),(function __clojure_fn_6639_file_seq_6641_fn_6646(d_1){
return (clojure.seq((d_1).listFiles()))}),dir_1))})))})();

//======
//(defn xml-seq "A tree seq on the xml elements as per xml/parse" [root] (tree-seq (complement string?) (comp seq :content) root))
//---
(function __clojure_fn_6657(){
return (clojure.JS.def(clojure,"xml_seq",(function __clojure_fn_6657_xml_seq_6659(root_1){
return (clojure.tree_seq(clojure.complement(clojure.string_QMARK_),clojure.comp(clojure.seq,":content"),root_1))})))})();

//======
//(defn special-symbol? "Returns true if s names a special form" [s] (contains? (. clojure.lang.Compiler specials) s))
//---
(function __clojure_fn_6669(){
return (clojure.JS.def(clojure,"special_symbol_QMARK_",(function __clojure_fn_6669_special_symbol_QMARK_6671(s_1){
return (clojure.contains_QMARK_(clojure.lang.Compiler.specials,s_1))})))})();

//======
//(defn var? "Returns true if v is of type clojure.lang.Var" [v] (instance? clojure.lang.Var v))
//---
(function __clojure_fn_6681(){
return (clojure.JS.def(clojure,"var_QMARK_",(function __clojure_fn_6681_var_QMARK_6683(v_1){
return (clojure.instance_QMARK_(clojure.lang.Var,v_1))})))})();

//======
//(defn class "Returns the Class of x" [x] (if (nil? x) x (. x (getClass))))
//---
(function __clojure_fn_6693(){
return (clojure.JS.def(clojure,"class",(function __clojure_fn_6693_class_6695(x_1){
return (((clojure.nil_QMARK_(x_1))?(x_1):((x_1).getClass())))})))})();

//======
//(defn slurp "Reads the file named by f into a string and returns it." [f] (with-open r (new java.io.BufferedReader (new java.io.FileReader f)) (let [sb (new StringBuilder)] (loop [c (. r (read))] (if (neg? c) (str sb) (do (. sb (append (char c))) (recur (. r (read)))))))))
//---
(function __clojure_fn_6705(){
return (clojure.JS.def(clojure,"slurp",(function __clojure_fn_6705_slurp_6707(f_1){
var r_2,sb_5,c_6;
return (((r_2=(new java.io.BufferedReader((new java.io.FileReader(f_1))))),
(function __try(){try{var _rtn=(((sb_5=(new java.lang.StringBuilder())),
((function __loop(){var _rtn,_cnt;(c_6=(r_2).read());do{_cnt=0;
_rtn=((clojure.lang.Numbers.isNeg(c_6))?(clojure.str(sb_5)):((sb_5).append(clojure.char_(c_6)),
(_cnt=1,_t0=(r_2).read(),c_6=_t0)))}while(_cnt);return _rtn;})())))}
finally{(r_2).close()}})()))})))})();

//======
//(defn subs "Returns the substring of s beginning at start inclusive, and ending\n  at end (defaults to length of string), exclusive." ([s start] (. s (substring start))) ([s start end] (. s (substring start end))))
//---
(function __clojure_fn_6718(){
return (clojure.JS.def(clojure,"subs",(function __clojure_fn_6718_subs_6720(s_1,start_2,end_3){switch(arguments.length){
case 2:return ((s_1).substring(start_2))}
return ((s_1).substring(start_2,end_3))})))})();

//======
//(defn max-key "Returns the x for which (k x), a number, is greatest." ([k x] x) ([k x y] (if (> (k x) (k y)) x y)) ([k x y & more] (reduce (fn* [p1__6725 p2__6726] (max-key k p1__6725 p2__6726)) (max-key k x y) more)))
//---
(function __clojure_fn_6738(){
return (clojure.JS.def(clojure,"max_key",clojure.JS.variatic(3,(function __clojure_fn_6738_max_key_6740(k_1,x_2,y_3){switch(arguments.length){
case 3:return (((clojure.lang.Numbers.gt(k_1(x_2),k_1(y_3)))?(x_2):(y_3)))
case 2:return (x_2)}
var more_4,more_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.reduce((function __clojure_fn_6738_max_key_6740_fn_6744(p1__6725_1,p2__6726_2){
return (clojure.max_key(k_1,p1__6725_1,p2__6726_2))}),clojure.max_key(k_1,x_2,y_3),more_4))}))))})();

//======
//(defn min-key "Returns the x for which (k x), a number, is least." ([k x] x) ([k x y] (if (< (k x) (k y)) x y)) ([k x y & more] (reduce (fn* [p1__6749 p2__6750] (min-key k p1__6749 p2__6750)) (min-key k x y) more)))
//---
(function __clojure_fn_6762(){
return (clojure.JS.def(clojure,"min_key",clojure.JS.variatic(3,(function __clojure_fn_6762_min_key_6764(k_1,x_2,y_3){switch(arguments.length){
case 3:return (((clojure.lang.Numbers.lt(k_1(x_2),k_1(y_3)))?(x_2):(y_3)))
case 2:return (x_2)}
var more_4,more_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.reduce((function __clojure_fn_6762_min_key_6764_fn_6768(p1__6749_1,p2__6750_2){
return (clojure.min_key(k_1,p1__6749_1,p2__6750_2))}),clojure.min_key(k_1,x_2,y_3),more_4))}))))})();

//======
//(defn distinct "Returns a lazy sequence of the elements of coll with duplicates removed" [coll] (let [step (fn step [[f & r :as xs] seen] (when xs (if (seen f) (recur r seen) (lazy-cons f (step r (conj seen f))))))] (step (seq coll) #{})))
//---
(function __clojure_fn_6788(){
return (clojure.JS.def(clojure,"distinct",(function __clojure_fn_6788_distinct_6790(coll_1){
var step_2;
return (((step_2=(function __clojure_fn_6788_distinct_6790_step_6793(p__6792_1,seen_2){
var _cnt,_rtn,xs_6,vec__6794_3,f_4,r_5,step_0=arguments.callee;
do{_cnt=0;_rtn=((vec__6794_3=p__6792_1),
(f_4=clojure.nth(vec__6794_3,0,null)),
(r_5=clojure.nthrest(vec__6794_3,1)),
(xs_6=vec__6794_3),
((xs_6)?(((seen_2(f_4))?((_cnt=1,_t0=r_5,_t1=seen_2,p__6792_1=_t0,seen_2=_t1)):((new clojure.lang.LazyCons((function __clojure_fn_6788_distinct_6790_step_6793_fn_6796(G__6795_1){switch(arguments.length){
case 0:return (f_4)}
return (step_0(r_5,clojure.conj(seen_2,f_4)))})))))):(null)))
}while(_cnt);return _rtn;})),
step_2(clojure.seq(coll_1),clojure.lang.PersistentHashSet.EMPTY)))})))})();

//======
//(defn replace "Given a map of replacement pairs and a vector/collection, returns a\n  vector/seq with any elements = a key in smap replaced with the\n  corresponding val in smap" [smap coll] (if (vector? coll) (reduce (fn [v i] (if-let e (find smap (nth v i)) (assoc v i (val e)) v)) coll (range (count coll))) (map (fn* [p1__6818] (if-let e (find smap p1__6818) (val e) p1__6818)) coll)))
//---
(function __clojure_fn_6831(){
return (clojure.JS.def(clojure,"replace",(function __clojure_fn_6831_replace_6833(smap_1,coll_2){
return (((clojure.vector_QMARK_(coll_2))?(clojure.reduce((function __clojure_fn_6831_replace_6833_fn_6835(v_1,i_2){
var e_4,temp__1381_3;
return (((temp__1381_3=clojure.find(smap_1,clojure.nth(v_1,i_2))),
((temp__1381_3)?(((e_4=temp__1381_3),
clojure.assoc(v_1,i_2,clojure.val(e_4)))):(v_1))))}),coll_2,clojure.range(clojure.count(coll_2)))):(clojure.map((function __clojure_fn_6831_replace_6833_fn_6838(p1__6818_1){
var temp__1381_2,e_3;
return (((temp__1381_2=clojure.find(smap_1,p1__6818_1)),
((temp__1381_2)?(((e_3=temp__1381_2),
clojure.val(e_3))):(p1__6818_1))))}),coll_2))))})))})();

//======
//(defn bound-fn {:private true} [sc test key] (fn [e] (test (.. sc comparator (compare (. sc entryKey e) key)) 0)))
//---
(function __clojure_fn_6865(){
return (clojure.JS.def(clojure,"bound_fn",(function __clojure_fn_6865_bound_fn_6867(sc_1,test_2,key_3){
return ((function __clojure_fn_6865_bound_fn_6867_fn_6869(e_1){
return (test_2(((sc_1).comparator()).compare((sc_1).entryKey(e_1),key_3),0))}))})))})();

//======
//(defn subseq "sc must be a sorted collection, test(s) one of <, <=, > or\n  >=. Returns a seq of those entries with keys ek for\n  which (test (.. sc comparator (compare ek key)) 0) is true" ([sc test key] (let [include (bound-fn sc test key)] (if (#{> >=} test) (when-let [e :as s] (. sc seqFrom key true) (if (include e) s (rest s))) (take-while include (. sc seq true))))) ([sc start-test start-key end-test end-key] (when-let [e :as s] (. sc seqFrom start-key true) (take-while (bound-fn sc end-test end-key) (if ((bound-fn sc start-test start-key) e) s (rest s))))))
//---
(function __clojure_fn_6883(){
return (clojure.JS.def(clojure,"subseq",(function __clojure_fn_6883_subseq_6885(sc_1,start_test_2,start_key_3,end_test_4,end_key_5){switch(arguments.length){
case 3:var temp__1386_5,include_4,e_7,s_8,vec__6887_6,test_2=arguments[1],key_3=arguments[2];
return (((include_4=clojure.bound_fn(sc_1,test_2,key_3)),
((clojure.lang.HashSet.create([clojure._GT_,clojure._GT__EQ_])(test_2))?(((temp__1386_5=(sc_1).seqFrom(key_3,true)),
((temp__1386_5)?(((vec__6887_6=temp__1386_5),
(e_7=clojure.nth(vec__6887_6,0,null)),
(s_8=vec__6887_6),
((include_4(e_7))?(s_8):(clojure.rest(s_8))))):(null)))):(clojure.take_while(include_4,(sc_1).seq(true))))))}
var vec__6889_7,temp__1386_6,s_9,e_8;
return (((temp__1386_6=(sc_1).seqFrom(start_key_3,true)),
((temp__1386_6)?(((vec__6889_7=temp__1386_6),
(e_8=clojure.nth(vec__6889_7,0,null)),
(s_9=vec__6889_7),
clojure.take_while(clojure.bound_fn(sc_1,end_test_4,end_key_5),((clojure.bound_fn(sc_1,start_test_2,start_key_3)(e_8))?(s_9):(clojure.rest(s_9)))))):(null))))})))})();

//======
//(defn rsubseq "sc must be a sorted collection, test(s) one of <, <=, > or\n  >=. Returns a reverse seq of those entries with keys ek for\n  which (test (.. sc comparator (compare ek key)) 0) is true" ([sc test key] (let [include (bound-fn sc test key)] (if (#{< <=} test) (when-let [e :as s] (. sc seqFrom key false) (if (include e) s (rest s))) (take-while include (. sc seq false))))) ([sc start-test start-key end-test end-key] (when-let [e :as s] (. sc seqFrom end-key false) (take-while (bound-fn sc start-test start-key) (if ((bound-fn sc end-test end-key) e) s (rest s))))))
//---
(function __clojure_fn_6901(){
return (clojure.JS.def(clojure,"rsubseq",(function __clojure_fn_6901_rsubseq_6903(sc_1,start_test_2,start_key_3,end_test_4,end_key_5){switch(arguments.length){
case 3:var s_8,e_7,temp__1386_5,include_4,vec__6905_6,test_2=arguments[1],key_3=arguments[2];
return (((include_4=clojure.bound_fn(sc_1,test_2,key_3)),
((clojure.lang.HashSet.create([clojure._LT_,clojure._LT__EQ_])(test_2))?(((temp__1386_5=(sc_1).seqFrom(key_3,false)),
((temp__1386_5)?(((vec__6905_6=temp__1386_5),
(e_7=clojure.nth(vec__6905_6,0,null)),
(s_8=vec__6905_6),
((include_4(e_7))?(s_8):(clojure.rest(s_8))))):(null)))):(clojure.take_while(include_4,(sc_1).seq(false))))))}
var vec__6907_7,e_8,temp__1386_6,s_9;
return (((temp__1386_6=(sc_1).seqFrom(end_key_5,false)),
((temp__1386_6)?(((vec__6907_7=temp__1386_6),
(e_8=clojure.nth(vec__6907_7,0,null)),
(s_9=vec__6907_7),
clojure.take_while(clojure.bound_fn(sc_1,start_test_2,start_key_3),((clojure.bound_fn(sc_1,end_test_4,end_key_5)(e_8))?(s_9):(clojure.rest(s_9)))))):(null))))})))})();

//======
//(defn repeatedly "Takes a function of no args, presumably with side effects, and returns an infinite\n  lazy sequence of calls to it" [f] (lazy-cons (f) (repeatedly f)))
//---
(function __clojure_fn_6921(){
return (clojure.JS.def(clojure,"repeatedly",(function __clojure_fn_6921_repeatedly_6923(f_1){
return ((new clojure.lang.LazyCons((function __clojure_fn_6921_repeatedly_6923_fn_6926(G__6925_1){switch(arguments.length){
case 0:return (f_1())}
return (clojure.repeatedly(f_1))}))))})))})();

//======
//(defn add-classpath "Adds the url (String or URL object) to the classpath per URLClassLoader.addURL" [url] (. clojure.lang.RT addURL url))
//---
(function __clojure_fn_6938(){
return (clojure.JS.def(clojure,"add_classpath",(function __clojure_fn_6938_add_classpath_6940(url_1){
return (clojure.lang.RT.addURL(url_1))})))})();

//======
//(defn hash "Returns the hash code of its argument" [x] (. clojure.lang.Util (hash x)))
//---
(function __clojure_fn_6950(){
return (clojure.JS.def(clojure,"hash",(function __clojure_fn_6950_hash_6952(x_1){
return (clojure.lang.Util.hash(x_1))})))})();

//======
//(defn interpose "Returns a lazy seq of the elements of coll separated by sep" [sep coll] (drop 1 (interleave (repeat sep) coll)))
//---
(function __clojure_fn_6962(){
return (clojure.JS.def(clojure,"interpose",(function __clojure_fn_6962_interpose_6964(sep_1,coll_2){
return (clojure.drop(1,clojure.interleave(clojure.repeat(sep_1),coll_2)))})))})();

//======
//(defn partition "Returns a lazy sequence of lists of n items each, at offsets step\n  apart. If step is not supplied, defaults to n, i.e. the partitions\n  do not overlap." ([n coll] (partition n n coll)) ([n step coll] (when (seq coll) (let [p (take n coll)] (when (= n (count p)) (lazy-cons p (partition n step (drop step coll))))))))
//---
(function __clojure_fn_6980(){
return (clojure.JS.def(clojure,"partition",(function __clojure_fn_6980_partition_6982(n_1,step_2,coll_3){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (clojure.partition(n_1,n_1,coll_2))}
var p_4;
return (((clojure.seq(coll_3))?(((p_4=clojure.take(n_1,coll_3)),
((clojure.lang.Util.equal(n_1,clojure.count(p_4)))?((new clojure.lang.LazyCons((function __clojure_fn_6980_partition_6982_fn_6986(G__6985_1){switch(arguments.length){
case 0:return (p_4)}
return (clojure.partition(n_1,step_2,clojure.drop(step_2,coll_3)))})))):(null)))):(null)))})))})();

//======
//(defn empty "Returns an empty collection of the same category as coll, or nil" [coll] (.empty coll))
//---
(function __clojure_fn_7006(){
return (clojure.JS.def(clojure,"empty",(function __clojure_fn_7006_empty_7008(coll_1){
return ((coll_1).empty())})))})();

//======
//(defn float-array "Creates an array of floats" {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/float_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers float_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers float_array size init-val-or-seq)))
//---
(function __clojure_fn_7036(){
return (clojure.JS.def(clojure,"float_array",(function __clojure_fn_7036_float_array_7041(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.float_array(size_or_seq_1))}
return (clojure.lang.Numbers.float_array(size_1,init_val_or_seq_2))})))})();

//======
//(defn double-array "Creates an array of doubles" {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/double_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers double_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers double_array size init-val-or-seq)))
//---
(function __clojure_fn_7056(){
return (clojure.JS.def(clojure,"double_array",(function __clojure_fn_7056_double_array_7061(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.double_array(size_or_seq_1))}
return (clojure.lang.Numbers.double_array(size_1,init_val_or_seq_2))})))})();

//======
//(defn int-array "Creates an array of ints" {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/int_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers int_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers int_array size init-val-or-seq)))
//---
(function __clojure_fn_7076(){
return (clojure.JS.def(clojure,"int_array",(function __clojure_fn_7076_int_array_7081(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.int_array(size_or_seq_1))}
return (clojure.lang.Numbers.int_array(size_1,init_val_or_seq_2))})))})();

//======
//(defn long-array "Creates an array of ints" {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/long_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers long_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers long_array size init-val-or-seq)))
//---
(function __clojure_fn_7096(){
return (clojure.JS.def(clojure,"long_array",(function __clojure_fn_7096_long_array_7101(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.long_array(size_or_seq_1))}
return (clojure.lang.Numbers.long_array(size_1,init_val_or_seq_2))})))})();

//======
//(import (quote (java.util.concurrent BlockingQueue LinkedBlockingQueue)))
//---
(function __clojure_fn_7157(){
return (clojure.import_(clojure.JS.lit_list(["'java.util.concurrent","'BlockingQueue","'LinkedBlockingQueue"])))})();

//======
//(defn seque "Creates a queued seq on another (presumably lazy) seq s. The queued\n  seq will produce a concrete seq in the background, and can get up to\n  n items ahead of the consumer. n-or-q can be an integer n buffer\n  size, or an instance of java.util.concurrent BlockingQueue. Note\n  that reading from a seque can block if the reader gets ahead of the\n  producer." ([s] (seque 100 s)) ([n-or-q s] (let [q (if (instance? BlockingQueue n-or-q) n-or-q (LinkedBlockingQueue. (int n-or-q))) NIL (Object.) agt (agent (seq s)) fill (fn [s] (try (loop [[x & xs :as s] s] (if s (if (.offer q (if (nil? x) NIL x)) (recur xs) s) (.put q q))) (catch Exception e (.put q q) (throw e)))) drain (fn drain [] (let [x (.take q)] (if (identical? x q) (clojure/deref agt) (do (send-off agt fill) (lazy-cons (if (identical? x NIL) nil x) (drain))))))] (send-off agt fill) (drain))))
//---
(function __clojure_fn_7181(){
return (clojure.JS.def(clojure,"seque",(function __clojure_fn_7181_seque_7183(n_or_q_1,s_2){switch(arguments.length){
case 1:var s_1=arguments[0];
return (clojure.seque(100,s_1))}
var NIL_4,agt_5,drain_7,q_3,fill_6;
return (((q_3=((clojure.instance_QMARK_(java.util.concurrent.BlockingQueue,n_or_q_1))?(n_or_q_1):((new java.util.concurrent.LinkedBlockingQueue(clojure.lang.RT.intCast(n_or_q_1)))))),
(NIL_4=(new java.lang.Object())),
(agt_5=clojure.agent(clojure.seq(s_2))),
(fill_6=(function __clojure_fn_7181_seque_7183_fill_7186(s_1){
var e_4,vec__7191_10,x_6,xs_7,vec__7190_5,G__7189_4,s_13,x_11,s_8,xs_12,G__7189_9;
return ((function __try(){try{var _rtn=(((G__7189_4=s_1),
(vec__7190_5=G__7189_4),
(x_6=clojure.nth(vec__7190_5,0,null)),
(xs_7=clojure.nthrest(vec__7190_5,1)),
(s_8=vec__7190_5),
((function __loop(){var _rtn,_cnt;(G__7189_9=G__7189_4);do{_cnt=0;
_rtn=((vec__7191_10=G__7189_9),
(x_11=clojure.nth(vec__7191_10,0,null)),
(xs_12=clojure.nthrest(vec__7191_10,1)),
(s_13=vec__7191_10),
((s_13)?((((q_3).offer(((clojure.nil_QMARK_(x_11))?(NIL_4):(x_11))))?((_cnt=1,_t0=xs_12,G__7189_9=_t0)):(s_13))):((q_3).put(q_3))))}while(_cnt);return _rtn;})())))}
catch(e_4){_rtn=(q_3).put(q_3),
(function __throw(){throw e_4})()}})())})),
(drain_7=(function __clojure_fn_7181_seque_7183_drain_7193(){
var x_1,drain_0=arguments.callee;
return (((x_1=(q_3).take()),
((clojure.identical_QMARK_(x_1,q_3))?(clojure.deref(agt_5)):(clojure.send_off(agt_5,fill_6),
(new clojure.lang.LazyCons((function __clojure_fn_7181_seque_7183_drain_7193_fn_7195(G__7194_1){switch(arguments.length){
case 0:return (((clojure.identical_QMARK_(x_1,NIL_4))?(null):(x_1)))}
return (drain_0())})))))))})),
clojure.send_off(agt_5,fill_6),
drain_7()))})))})();

//======
//(defn class? "Returns true if x is an instance of Class" [x] (instance? Class x))
//---
(function __clojure_fn_7208(){
return (clojure.JS.def(clojure,"class_QMARK_",(function __clojure_fn_7208_class_QMARK_7210(x_1){
return (clojure.instance_QMARK_(java.lang.Class,x_1))})))})();

//======
//(defn alter-var-root "Atomically alters the root binding of var v by applying f to its\n  current value plus any args" [v f & args] (.alterRoot v f args))
//---
(function __clojure_fn_7220(){
return (clojure.JS.def(clojure,"alter_var_root",clojure.JS.variatic(2,(function __clojure_fn_7220_alter_var_root_7222(v_1,f_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((v_1).alterRoot(f_2,args_3))}))))})();

//======
//(defn make-hierarchy "Creates a hierarchy object for use with derive, isa? etc." [] {:descendants {}, :parents {}, :ancestors {}})
//---
(function __clojure_fn_7232(){
return (clojure.JS.def(clojure,"make_hierarchy",(function __clojure_fn_7232_make_hierarchy_7234(){
return (clojure.lang.HashMap.create([":descendants",clojure.lang.PersistentHashMap.EMPTY,":parents",clojure.lang.PersistentHashMap.EMPTY,":ancestors",clojure.lang.PersistentHashMap.EMPTY]))})))})();

//======
//(defn not-empty "If coll is empty, returns nil, else coll" [coll] (when (seq coll) coll))
//---
(function __clojure_fn_7247(){
return (clojure.JS.def(clojure,"not_empty",(function __clojure_fn_7247_not_empty_7249(coll_1){
return (((clojure.seq(coll_1))?(coll_1):(null)))})))})();

//======
//(defn bases "Returns the immediate superclass and direct interfaces of c, if any" [c] (let [i (.getInterfaces c) s (.getSuperclass c)] (not-empty (if s (cons s i) i))))
//---
(function __clojure_fn_7259(){
return (clojure.JS.def(clojure,"bases",(function __clojure_fn_7259_bases_7261(c_1){
var i_2,s_3;
return (((i_2=(c_1).getInterfaces()),
(s_3=(c_1).getSuperclass()),
clojure.not_empty(((s_3)?(clojure.cons(s_3,i_2)):(i_2)))))})))})();

//======
//(defn supers "Returns the immediate and indirect superclasses and interfaces of c, if any" [class] (loop [ret #{} c class] (if c (recur (into ret (bases c)) (.getSuperclass c)) (not-empty ret))))
//---
(function __clojure_fn_7271(){
return (clojure.JS.def(clojure,"supers",(function __clojure_fn_7271_supers_7273(class_1){
var c_3,ret_2;
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentHashSet.EMPTY),
(c_3=class_1);do{_cnt=0;
_rtn=((c_3)?((_cnt=1,_t0=clojure.into(ret_2,clojure.bases(c_3)),_t1=(c_3).getSuperclass(),ret_2=_t0,c_3=_t1)):(clojure.not_empty(ret_2)))}while(_cnt);return _rtn;})()))})))})();

//======
//(defn isa? "Returns true if (= child parent), or child is directly or indirectly derived from\n  parent, either via a Java type inheritance relationship or a\n  relationship established via derive. h must be a hierarchy obtained\n  from make-hierarchy, if not supplied defaults to the global\n  hierarchy" ([child parent] (isa? global-hierarchy child parent)) ([h child parent] (or (= child parent) (and (class? parent) (class? child) (. parent isAssignableFrom child)) (contains? ((:ancestors h) child) parent) (and (class? child) (some (fn* [p1__7277] (contains? ((:ancestors h) p1__7277) parent)) (supers child))) (and (vector? parent) (vector? child) (= (count parent) (count child)) (loop [ret true i 0] (if (= i (count parent)) ret (recur (and (isa? (child i) (parent i)) ret) (inc i))))))))
//---
(function __clojure_fn_7288(){
return (clojure.JS.def(clojure,"isa_QMARK_",(function __clojure_fn_7288_isa_QMARK_7290(h_1,child_2,parent_3){switch(arguments.length){
case 2:var child_1=arguments[0],parent_2=arguments[1];
return (clojure.isa_QMARK_(clojure.global_hierarchy,child_1,parent_2))}
var or__202_4,and__196_9,and__196_5,or__202_5,or__202_6,and__196_7,i_12,and__196_10,or__202_7,and__196_13,ret_11,and__196_8,and__196_6;
return (((or__202_4=clojure.lang.Util.equal(child_2,parent_3)),
((or__202_4)?(or__202_4):(((or__202_5=((and__196_5=clojure.class_QMARK_(parent_3)),
((and__196_5)?(((and__196_6=clojure.class_QMARK_(child_2)),
((and__196_6)?((parent_3).isAssignableFrom(child_2)):(and__196_6)))):(and__196_5)))),
((or__202_5)?(or__202_5):(((or__202_6=clojure.contains_QMARK_(":ancestors"(h_1)(child_2),parent_3)),
((or__202_6)?(or__202_6):(((or__202_7=((and__196_7=clojure.class_QMARK_(child_2)),
((and__196_7)?(clojure.some((function __clojure_fn_7288_isa_QMARK_7290_fn_7293(p1__7277_1){
return (clojure.contains_QMARK_(":ancestors"(h_1)(p1__7277_1),parent_3))}),clojure.supers(child_2))):(and__196_7)))),
((or__202_7)?(or__202_7):(((and__196_8=clojure.vector_QMARK_(parent_3)),
((and__196_8)?(((and__196_9=clojure.vector_QMARK_(child_2)),
((and__196_9)?(((and__196_10=clojure.lang.Util.equal(clojure.count(parent_3),clojure.count(child_2))),
((and__196_10)?(((function __loop(){var _rtn,_cnt;(ret_11=true),
(i_12=0);do{_cnt=0;
_rtn=((clojure.lang.Util.equal(i_12,clojure.count(parent_3)))?(ret_11):((_cnt=1,_t0=((and__196_13=clojure.isa_QMARK_(child_2(i_12),parent_3(i_12))),
((and__196_13)?(ret_11):(and__196_13))),_t1=clojure.lang.Numbers.inc(i_12),ret_11=_t0,i_12=_t1)))}while(_cnt);return _rtn;})())):(and__196_10)))):(and__196_9)))):(and__196_8))))))))))))))))})))})();

//======
//(defn parents "Returns the immediate parents of tag, either via a Java type\n  inheritance relationship or a relationship established via derive. h\n  must be a hierarchy obtained from make-hierarchy, if not supplied\n  defaults to the global hierarchy" ([tag] (parents global-hierarchy tag)) ([h tag] (not-empty (let [tp (get (:parents h) tag)] (if (class? tag) (into (set (bases tag)) tp) tp)))))
//---
(function __clojure_fn_7305(){
return (clojure.JS.def(clojure,"parents",(function __clojure_fn_7305_parents_7307(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.parents(clojure.global_hierarchy,tag_1))}
var tp_3;
return (clojure.not_empty(((tp_3=clojure.get(":parents"(h_1),tag_2)),
((clojure.class_QMARK_(tag_2))?(clojure.into(clojure.set(clojure.bases(tag_2)),tp_3)):(tp_3)))))})))})();

//======
//(defn ancestors "Returns the immediate and indirect parents of tag, either via a Java type\n  inheritance relationship or a relationship established via derive. h\n  must be a hierarchy obtained from make-hierarchy, if not supplied\n  defaults to the global hierarchy" ([tag] (ancestors global-hierarchy tag)) ([h tag] (not-empty (let [ta (get (:ancestors h) tag)] (if (class? tag) (into (set (supers tag)) ta) ta)))))
//---
(function __clojure_fn_7319(){
return (clojure.JS.def(clojure,"ancestors",(function __clojure_fn_7319_ancestors_7321(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.ancestors(clojure.global_hierarchy,tag_1))}
var ta_3;
return (clojure.not_empty(((ta_3=clojure.get(":ancestors"(h_1),tag_2)),
((clojure.class_QMARK_(tag_2))?(clojure.into(clojure.set(clojure.supers(tag_2)),ta_3)):(ta_3)))))})))})();

//======
//(defn descendants "Returns the immediate and indirect children of tag, through a\n  relationship established via derive. h must be a hierarchy obtained\n  from make-hierarchy, if not supplied defaults to the global\n  hierarchy. Note: does not work on Java type inheritance\n  relationships." ([tag] (descendants global-hierarchy tag)) ([h tag] (if (class? tag) (throw (java.lang.UnsupportedOperationException. "Can't get descendants of classes")) (not-empty (get (:descendants h) tag)))))
//---
(function __clojure_fn_7333(){
return (clojure.JS.def(clojure,"descendants",(function __clojure_fn_7333_descendants_7335(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.descendants(clojure.global_hierarchy,tag_1))}
return (((clojure.class_QMARK_(tag_2))?((function __throw(){throw (new java.lang.UnsupportedOperationException("Can't get descendants of classes"))})()):(clojure.not_empty(clojure.get(":descendants"(h_1),tag_2)))))})))})();

//======
//(defn derive "Establishes a parent/child relationship between parent and\n  tag. Parent must be a namespace-qualified symbol or keyword and\n  child can be either a namespace-qualified symbol or keyword or a\n  class. h must be a hierarchy obtained from make-hierarchy, if not\n  supplied defaults to, and modifies, the global hierarchy." ([tag parent] (alter-var-root (var global-hierarchy) derive tag parent) nil) ([h tag parent] (assert (not= tag parent)) (assert (or (class? tag) (and (instance? clojure.lang.Named tag) (namespace tag)))) (assert (instance? clojure.lang.Named parent)) (assert (namespace parent)) (let [tp (:parents h) td (:descendants h) ta (:ancestors h) tf (fn [m source sources target targets] (reduce (fn [ret k] (assoc ret k (reduce conj (get targets k #{}) (cons target (targets target))))) m (cons source (sources source))))] (or (when-not (contains? (tp tag) parent) (when (contains? (ta tag) parent) (throw (Exception. (print-str tag "already has" parent "as ancestor")))) (when (contains? (ta parent) tag) (throw (Exception. (print-str "Cyclic derivation:" parent "has" tag "as ancestor")))) {:descendants (tf (:descendants h) parent ta tag td), :parents (assoc (:parents h) tag (conj (get tp tag #{}) parent)), :ancestors (tf (:ancestors h) tag td parent ta)}) h))))
//---
(function __clojure_fn_7353(){
return (clojure.JS.def(clojure,"derive",(function __clojure_fn_7353_derive_7355(h_1,tag_2,parent_3){switch(arguments.length){
case 2:var tag_1=arguments[0],parent_2=arguments[1];
return (clojure.alter_var_root(clojure._var_global_hierarchy,clojure.derive,tag_1,parent_2),
null)}
var td_5,ta_6,tf_7,or__202_4,and__196_5,or__202_8,tp_4;
return (((clojure.not_EQ_(tag_2,parent_3))?(null):((function __throw(){throw (new java.lang.Exception(clojure.str("Assert failed: ",clojure.pr_str(clojure.JS.lit_list(["'not=","'tag","'parent"])))))})())),
((((or__202_4=clojure.class_QMARK_(tag_2)),
((or__202_4)?(or__202_4):(((and__196_5=clojure.instance_QMARK_(clojure.lang.Named,tag_2)),
((and__196_5)?(clojure.namespace(tag_2)):(and__196_5)))))))?(null):((function __throw(){throw (new java.lang.Exception(clojure.str("Assert failed: ",clojure.pr_str(clojure.JS.lit_list(["'or",clojure.JS.lit_list(["'class?","'tag"]),clojure.JS.lit_list(["'and",clojure.JS.lit_list(["'instance?","'clojure.lang.Named","'tag"]),clojure.JS.lit_list(["'namespace","'tag"])])])))))})())),
((clojure.instance_QMARK_(clojure.lang.Named,parent_3))?(null):((function __throw(){throw (new java.lang.Exception(clojure.str("Assert failed: ",clojure.pr_str(clojure.JS.lit_list(["'instance?","'clojure.lang.Named","'parent"])))))})())),
((clojure.namespace(parent_3))?(null):((function __throw(){throw (new java.lang.Exception(clojure.str("Assert failed: ",clojure.pr_str(clojure.JS.lit_list(["'namespace","'parent"])))))})())),
((tp_4=":parents"(h_1)),
(td_5=":descendants"(h_1)),
(ta_6=":ancestors"(h_1)),
(tf_7=(function __clojure_fn_7353_derive_7355_tf_7358(m_1,source_2,sources_3,target_4,targets_5){
return (clojure.reduce((function __clojure_fn_7353_derive_7355_tf_7358_fn_7360(ret_1,k_2){
return (clojure.assoc(ret_1,k_2,clojure.reduce(clojure.conj,clojure.get(targets_5,k_2,clojure.lang.PersistentHashSet.EMPTY),clojure.cons(target_4,targets_5(target_4)))))}),m_1,clojure.cons(source_2,sources_3(source_2))))})),
((or__202_8=((clojure.contains_QMARK_(tp_4(tag_2),parent_3))?(null):(((clojure.contains_QMARK_(ta_6(tag_2),parent_3))?((function __throw(){throw (new java.lang.Exception(clojure.print_str(tag_2,"already has",parent_3,"as ancestor")))})()):(null)),
((clojure.contains_QMARK_(ta_6(parent_3),tag_2))?((function __throw(){throw (new java.lang.Exception(clojure.print_str("Cyclic derivation:",parent_3,"has",tag_2,"as ancestor")))})()):(null)),
clojure.lang.HashMap.create([":descendants",tf_7(":descendants"(h_1),parent_3,ta_6,tag_2,td_5),":parents",clojure.assoc(":parents"(h_1),tag_2,clojure.conj(clojure.get(tp_4,tag_2,clojure.lang.PersistentHashSet.EMPTY),parent_3)),":ancestors",tf_7(":ancestors"(h_1),tag_2,td_5,parent_3,ta_6)])))),
((or__202_8)?(or__202_8):(h_1)))))})))})();

//======
//(defn underive "Removes a parent/child relationship between parent and\n  tag. h must be a hierarchy obtained from make-hierarchy, if not\n  supplied defaults to, and modifies, the global hierarchy." ([tag parent] (alter-var-root (var global-hierarchy) underive tag parent) nil) ([h tag parent] (let [tp (:parents h) td (:descendants h) ta (:ancestors h) tf (fn [m source sources target targets] (reduce (fn [ret k] (assoc ret k (reduce disj (get targets k) (cons target (targets target))))) m (cons source (sources source))))] (if (contains? (tp tag) parent) {:parent (assoc (:parents h) tag (disj (get tp tag) parent)), :descendants (tf (:descendants h) parent ta tag td), :ancestors (tf (:ancestors h) tag td parent ta)} h))))
//---
(function __clojure_fn_7379(){
return (clojure.JS.def(clojure,"underive",(function __clojure_fn_7379_underive_7381(h_1,tag_2,parent_3){switch(arguments.length){
case 2:var tag_1=arguments[0],parent_2=arguments[1];
return (clojure.alter_var_root(clojure._var_global_hierarchy,clojure.underive,tag_1,parent_2),
null)}
var tp_4,tf_7,td_5,ta_6;
return (((tp_4=":parents"(h_1)),
(td_5=":descendants"(h_1)),
(ta_6=":ancestors"(h_1)),
(tf_7=(function __clojure_fn_7379_underive_7381_tf_7384(m_1,source_2,sources_3,target_4,targets_5){
return (clojure.reduce((function __clojure_fn_7379_underive_7381_tf_7384_fn_7386(ret_1,k_2){
return (clojure.assoc(ret_1,k_2,clojure.reduce(clojure.disj,clojure.get(targets_5,k_2),clojure.cons(target_4,targets_5(target_4)))))}),m_1,clojure.cons(source_2,sources_3(source_2))))})),
((clojure.contains_QMARK_(tp_4(tag_2),parent_3))?(clojure.lang.HashMap.create([":parent",clojure.assoc(":parents"(h_1),tag_2,clojure.disj(clojure.get(tp_4,tag_2),parent_3)),":descendants",tf_7(":descendants"(h_1),parent_3,ta_6,tag_2,td_5),":ancestors",tf_7(":ancestors"(h_1),tag_2,td_5,parent_3,ta_6)])):(h_1))))})))})();

//======
//(defn distinct? "Returns true if no two of the arguments are equal" {:tag Boolean} ([x] true) ([x y] (not (= x y))) ([x y & more] (if (not= x y) (loop [s #{y x} [x & etc :as xs] more] (if xs (if (contains? s x) false (recur (conj s x) etc)) true)) false)))
//---
(function __clojure_fn_7404(){
return (clojure.JS.def(clojure,"distinct_QMARK_",clojure.JS.variatic(2,(function __clojure_fn_7404_distinct_QMARK_7406(x_1,y_2){switch(arguments.length){
case 2:return (clojure.not(clojure.lang.Util.equal(x_1,y_2)))
case 1:return (true)}
var etc_15,etc_8,xs_9,more_3,vec__7413_13,x_7,xs_16,s_12,s_10,x_14,G__7411_5,G__7411_11,vec__7412_6,s_4,more_3=clojure.JS.rest_args(this,arguments,2);
return (((clojure.not_EQ_(x_1,y_2))?(((s_4=clojure.lang.HashSet.create([y_2,x_1])),
(G__7411_5=more_3),
(vec__7412_6=G__7411_5),
(x_7=clojure.nth(vec__7412_6,0,null)),
(etc_8=clojure.nthrest(vec__7412_6,1)),
(xs_9=vec__7412_6),
((function __loop(){var _rtn,_cnt;(s_10=s_4),
(G__7411_11=G__7411_5);do{_cnt=0;
_rtn=((s_12=s_10),
(vec__7413_13=G__7411_11),
(x_14=clojure.nth(vec__7413_13,0,null)),
(etc_15=clojure.nthrest(vec__7413_13,1)),
(xs_16=vec__7413_13),
((xs_16)?(((clojure.contains_QMARK_(s_12,x_14))?(false):((_cnt=1,_t0=clojure.conj(s_12,x_14),_t1=etc_15,s_10=_t0,G__7411_11=_t1)))):(true)))}while(_cnt);return _rtn;})()))):(false)))}))))})();

//======
//(defn iterator-seq "Returns a seq on a java.util.Iterator. Note that most collections\n  providing iterators implement Iterable and thus support seq directly." [iter] (clojure.lang.IteratorSeq/create iter))
//---
(function __clojure_fn_7422(){
return (clojure.JS.def(clojure,"iterator_seq",(function __clojure_fn_7422_iterator_seq_7424(iter_1){
return (clojure.lang.IteratorSeq.create(iter_1))})))})();

//======
//(defn enumeration-seq "Returns a seq on a java.lang.Enumeration" [e] (clojure.lang.EnumerationSeq/create e))
//---
(function __clojure_fn_7434(){
return (clojure.JS.def(clojure,"enumeration_seq",(function __clojure_fn_7434_enumeration_seq_7436(e_1){
return (clojure.lang.EnumerationSeq.create(e_1))})))})();

//======
//(defn format "Formats a string using java.lang.String.format, see java.util.Formatter for format\n  string syntax" [fmt & args] (String/format fmt (to-array args)))
//---
(function __clojure_fn_7446(){
return (clojure.JS.def(clojure,"format",clojure.JS.variatic(1,(function __clojure_fn_7446_format_7448(fmt_1){
var args_2,args_2=clojure.JS.rest_args(this,arguments,1);
return (java.lang.String.format(fmt_1,clojure.to_array(args_2)))}))))})();

//======
//(defn printf "Prints formatted output, as per format" [fmt & args] (print (apply format fmt args)))
//---
(function __clojure_fn_7458(){
return (clojure.JS.def(clojure,"printf",clojure.JS.variatic(1,(function __clojure_fn_7458_printf_7460(fmt_1){
var args_2,args_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.print(clojure.apply(clojure.format,fmt_1,args_2)))}))))})();

//======
//(defonce *loaded-libs* (ref (sorted-set)))
//---
(function __clojure_fn_7499(){
var v__1662_1;
return (((v__1662_1=clojure.JS.def(clojure,"_STAR_loaded_libs_STAR_",null)),
(((v__1662_1).hasRoot())?(null):(clojure.JS.def(clojure,"_STAR_loaded_libs_STAR_",clojure.ref(clojure.sorted_set()))))))})();

//======
//(defonce *pending-paths* #{})
//---
(function __clojure_fn_7505(){
var v__1662_1;
return (((v__1662_1=clojure.JS.def(clojure,"_STAR_pending_paths_STAR_",null)),
(((v__1662_1).hasRoot())?(null):(clojure.JS.def(clojure,"_STAR_pending_paths_STAR_",clojure.lang.PersistentHashSet.EMPTY)))))})();

//======
//(defonce *loading-verbosely* false)
//---
(function __clojure_fn_7511(){
var v__1662_1;
return (((v__1662_1=clojure.JS.def(clojure,"_STAR_loading_verbosely_STAR_",null)),
(((v__1662_1).hasRoot())?(null):(clojure.JS.def(clojure,"_STAR_loading_verbosely_STAR_",false)))))})();

//======
//(defn- throw-if "Throws an exception with a message if pred is true" [pred fmt & args] (when pred (let [message (apply format fmt args) exception (Exception. message) raw-trace (.getStackTrace exception) boring? (fn* [p1__7514] (not= (.getMethodName p1__7514) "doInvoke")) trace (into-array (drop 2 (drop-while boring? raw-trace)))] (.setStackTrace exception trace) (throw exception))))
//---
(function __clojure_fn_7524(){
return (clojure.JS.def(clojure,"throw_if",clojure.JS.variatic(2,(function __clojure_fn_7524_throw_if_7526(pred_1,fmt_2){
var message_4,args_3,trace_8,exception_5,raw_trace_6,boring_QMARK__7,args_3=clojure.JS.rest_args(this,arguments,2);
return (((pred_1)?(((message_4=clojure.apply(clojure.format,fmt_2,args_3)),
(exception_5=(new java.lang.Exception(message_4))),
(raw_trace_6=(exception_5).getStackTrace()),
(boring_QMARK__7=(function __clojure_fn_7524_throw_if_7526_boring_QMARK_7528(p1__7514_1){
return (clojure.not_EQ_((p1__7514_1).getMethodName,"doInvoke"))})),
(trace_8=clojure.into_array(clojure.drop(2,clojure.drop_while(boring_QMARK__7,raw_trace_6)))),
(exception_5).setStackTrace(trace_8),
(function __throw(){throw exception_5})())):(null)))}))))})();

//======
//(defn- libspec? "Returns true if x is a libspec" [x] (or (symbol? x) (and (vector? x) (or (nil? (second x)) (keyword? (second x))))))
//---
(function __clojure_fn_7539(){
return (clojure.JS.def(clojure,"libspec_QMARK_",(function __clojure_fn_7539_libspec_QMARK_7541(x_1){
var and__196_3,or__202_2,or__202_4;
return (((or__202_2=clojure.symbol_QMARK_(x_1)),
((or__202_2)?(or__202_2):(((and__196_3=clojure.vector_QMARK_(x_1)),
((and__196_3)?(((or__202_4=clojure.nil_QMARK_(clojure.second(x_1))),
((or__202_4)?(or__202_4):(clojure.keyword_QMARK_(clojure.second(x_1)))))):(and__196_3)))))))})))})();

//======
//(defn- prependss "Prepends a symbol or a seq to coll" [x coll] (if (symbol? x) (cons x coll) (concat x coll)))
//---
(function __clojure_fn_7551(){
return (clojure.JS.def(clojure,"prependss",(function __clojure_fn_7551_prependss_7553(x_1,coll_2){
return (((clojure.symbol_QMARK_(x_1))?(clojure.cons(x_1,coll_2)):(clojure.concat(x_1,coll_2))))})))})();

//======
//(defn- root-directory "Returns the root directory path for a lib" [lib] (str \/ (.. (name lib) (replace \- \_) (replace \. \/))))
//---
(function __clojure_fn_7563(){
return (clojure.JS.def(clojure,"root_directory",(function __clojure_fn_7563_root_directory_7565(lib_1){
return (clojure.str("/",((clojure.name(lib_1)).replace("-","_")).replace(".","/")))})))})();

//======
//(defn- root-resource "Returns the root resource path for a lib" [lib] (let [d (root-directory lib) i (inc (.lastIndexOf d (int \/))) leaf (.substring d i)] (str d \/ leaf ".clj")))
//---
(function __clojure_fn_7575(){
return (clojure.JS.def(clojure,"root_resource",(function __clojure_fn_7575_root_resource_7577(lib_1){
var leaf_4,i_3,d_2;
return (((d_2=clojure.root_directory(lib_1)),
(i_3=clojure.lang.Numbers.inc((d_2).lastIndexOf(clojure.lang.RT.intCast("/")))),
(leaf_4=(d_2).substring(i_3)),
clojure.str(d_2,"/",leaf_4,".clj")))})))})();

//======
//(def load)
//---
(function __clojure_fn_7584(){
return (clojure.JS.def(clojure,"load",null))})();

//======
//(defn- load-one "Loads a lib given its name. If need-ns, ensures that the associated\n  namespace exists after loading. If require, records the load so any\n  duplicate loads can be skipped." [lib need-ns require] (load (root-resource lib)) (throw-if (and need-ns (not (find-ns lib))) "namespace '%s' not found after loading '%s'" lib (root-resource lib)) (when require (dosync (commute *loaded-libs* conj lib))))
//---
(function __clojure_fn_7596(){
return (clojure.JS.def(clojure,"load_one",(function __clojure_fn_7596_load_one_7598(lib_1,need_ns_2,require_3){
var and__196_4;
return (clojure.load(clojure.root_resource(lib_1)),
clojure.throw_if(((and__196_4=need_ns_2),
((and__196_4)?(clojure.not(clojure.find_ns(lib_1))):(and__196_4))),"namespace '%s' not found after loading '%s'",lib_1,clojure.root_resource(lib_1)),
((require_3)?(clojure.lang.LockingTransaction.runInTransaction((function __clojure_fn_7596_load_one_7598_fn_7600(){
return (clojure.commute(clojure._STAR_loaded_libs_STAR_,clojure.conj,lib_1))}))):(null)))})))})();

//======
//(defn- load-all "Loads a lib given its name and forces a load of any libs it directly or\n  indirectly loads. If need-ns, ensures that the associated namespace\n  exists after loading. If require, records the load so any duplicate loads\n  can be skipped." [lib need-ns require] (dosync (commute *loaded-libs* (fn* [p1__7605 p2__7606] (reduce conj p1__7605 p2__7606)) (binding [*loaded-libs* (ref (sorted-set))] (load-one lib need-ns require) (clojure/deref *loaded-libs*)))))
//---
(function __clojure_fn_7622(){
return (clojure.JS.def(clojure,"load_all",(function __clojure_fn_7622_load_all_7624(lib_1,need_ns_2,require_3){
return (clojure.lang.LockingTransaction.runInTransaction((function __clojure_fn_7622_load_all_7624_fn_7626(){
return (clojure.commute(clojure._STAR_loaded_libs_STAR_,(function __clojure_fn_7622_load_all_7624_fn_7626_fn_7628(p1__7605_1,p2__7606_2){
return (clojure.reduce(clojure.conj,p1__7605_1,p2__7606_2))}),clojure.lang.Var.pushThreadBindings(clojure.hash_map(clojure._var__STAR_loaded_libs_STAR_,clojure.ref(clojure.sorted_set()))),
(function __clojure_fn_7622_load_all_7624_fn_7626_fn_7631(){
return ((function __try(){try{var _rtn=(clojure.load_one(lib_1,need_ns_2,require_3),
clojure.deref(clojure._STAR_loaded_libs_STAR_))}
finally{clojure.lang.Var.popThreadBindings()}})())})()))})))})))})();

//======
//(defn- load-lib "Loads a lib with options" [prefix lib & options] (throw-if (and prefix (pos? (.indexOf (name lib) (int \.)))) "lib names inside prefix lists must not contain periods") (let [lib (if prefix (symbol (str prefix \. lib)) lib) opts (apply hash-map options) {:keys [as reload reload-all require use verbose]} opts loaded (contains? (clojure/deref *loaded-libs*) lib) load (cond reload-all load-all (or reload (not require) (not loaded)) load-one) need-ns (or as use) filter-opts (select-keys opts (quote (:exclude :only :rename)))] (binding [*loading-verbosely* (or *loading-verbosely* verbose)] (if load (load lib need-ns require) (throw-if (and need-ns (not (find-ns lib))) "namespace '%s' not found" lib)) (when (and need-ns *loading-verbosely*) (printf "(clojure/in-ns '%s)\n" (ns-name *ns*))) (when as (when *loading-verbosely* (printf "(clojure/alias '%s '%s)\n" as lib)) (alias as lib)) (when use (when *loading-verbosely* (printf "(clojure/refer '%s" lib) (doseq opt filter-opts (printf " %s '%s" (key opt) (print-str (val opt)))) (printf ")\n")) (apply refer lib (mapcat seq filter-opts))))))
//---
(function __clojure_fn_7644(){
return (clojure.JS.def(clojure,"load_lib",clojure.JS.variatic(2,(function __clojure_fn_7644_load_lib_7646(prefix_1,lib_2){
var or__202_15,options_3,map__7648_6,require_9,as_8,list__739_19,load_14,opts_5,filter_opts_16,need_ns_15,verbose_11,lib_4,opt_20,and__196_19,and__196_4,reload_all_10,use_12,loaded_13,reload_7,or__202_15,or__202_14,and__196_19,or__202_17,options_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.throw_if(((and__196_4=prefix_1),
((and__196_4)?(clojure.lang.Numbers.isPos((clojure.name(lib_2)).indexOf(clojure.lang.RT.intCast(".")))):(and__196_4))),"lib names inside prefix lists must not contain periods"),
((lib_4=((prefix_1)?(clojure.symbol(clojure.str(prefix_1,".",lib_2))):(lib_2))),
(opts_5=clojure.apply(clojure.hash_map,options_3)),
(map__7648_6=opts_5),
(reload_7=clojure.get(map__7648_6,":reload")),
(as_8=clojure.get(map__7648_6,":as")),
(require_9=clojure.get(map__7648_6,":require")),
(reload_all_10=clojure.get(map__7648_6,":reload-all")),
(verbose_11=clojure.get(map__7648_6,":verbose")),
(use_12=clojure.get(map__7648_6,":use")),
(loaded_13=clojure.contains_QMARK_(clojure.deref(clojure._STAR_loaded_libs_STAR_),lib_4)),
(load_14=((reload_all_10)?(clojure.load_all):(((((or__202_14=reload_7),
((or__202_14)?(or__202_14):(((or__202_15=clojure.not(require_9)),
((or__202_15)?(or__202_15):(clojure.not(loaded_13))))))))?(clojure.load_one):(null))))),
(need_ns_15=((or__202_15=as_8),
((or__202_15)?(or__202_15):(use_12)))),
(filter_opts_16=clojure.select_keys(opts_5,clojure.JS.lit_list([":exclude",":only",":rename"]))),
clojure.lang.Var.pushThreadBindings(clojure.hash_map(clojure._var__STAR_loading_verbosely_STAR_,((or__202_17=clojure._STAR_loading_verbosely_STAR_),
((or__202_17)?(or__202_17):(verbose_11))))),
(function __try(){try{var _rtn=(((load_14)?(load_14(lib_4,need_ns_15,require_9)):(clojure.throw_if(((and__196_19=need_ns_15),
((and__196_19)?(clojure.not(clojure.find_ns(lib_4))):(and__196_19))),"namespace '%s' not found",lib_4))),
((((and__196_19=need_ns_15),
((and__196_19)?(clojure._STAR_loading_verbosely_STAR_):(and__196_19))))?(clojure.printf("(clojure/in-ns '%s)\n",clojure.ns_name(clojure._STAR_ns_STAR_))):(null)),
((as_8)?(((clojure._STAR_loading_verbosely_STAR_)?(clojure.printf("(clojure/alias '%s '%s)\n",as_8,lib_4)):(null)),
clojure.alias(as_8,lib_4)):(null)),
((use_12)?(((clojure._STAR_loading_verbosely_STAR_)?(clojure.printf("(clojure/refer '%s",lib_4),
((function __loop(){var _rtn,_cnt;(list__739_19=clojure.seq(filter_opts_16));do{_cnt=0;
_rtn=((list__739_19)?(((opt_20=clojure.first(list__739_19)),
clojure.printf(" %s '%s",clojure.key(opt_20),clojure.print_str(clojure.val(opt_20)))),
(_cnt=1,_t0=clojure.rest(list__739_19),list__739_19=_t0)):(null))}while(_cnt);return _rtn;})()),
clojure.printf(")\n")):(null)),
clojure.apply(clojure.refer,lib_4,clojure.mapcat(clojure.seq,filter_opts_16))):(null)))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))})();

//======
//(defn- load-libs "Loads libs, interpreting libspecs, prefix lists, and flags for\n  forwarding to load-lib" [& args] (let [flags (filter keyword? args) opts (interleave flags (repeat true)) args (filter (complement keyword?) args)] (doseq arg args (if (libspec? arg) (apply load-lib nil (prependss arg opts)) (let [[prefix & args] arg] (throw-if (nil? prefix) "prefix cannot be nil") (doseq arg args (apply load-lib prefix (prependss arg opts))))))))
//---
(function __clojure_fn_7658(){
return (clojure.JS.def(clojure,"load_libs",clojure.JS.variatic(0,(function __clojure_fn_7658_load_libs_7660(){
var opts_3,args_9,args_1,args_4,prefix_8,flags_2,arg_6,list__739_10,vec__7662_7,arg_11,list__739_5,args_1=clojure.JS.rest_args(this,arguments,0);
return (((flags_2=clojure.filter(clojure.keyword_QMARK_,args_1)),
(opts_3=clojure.interleave(flags_2,clojure.repeat(true))),
(args_4=clojure.filter(clojure.complement(clojure.keyword_QMARK_),args_1)),
((function __loop(){var _rtn,_cnt;(list__739_5=clojure.seq(args_4));do{_cnt=0;
_rtn=((list__739_5)?(((arg_6=clojure.first(list__739_5)),
((clojure.libspec_QMARK_(arg_6))?(clojure.apply(clojure.load_lib,null,clojure.prependss(arg_6,opts_3))):(((vec__7662_7=arg_6),
(prefix_8=clojure.nth(vec__7662_7,0,null)),
(args_9=clojure.nthrest(vec__7662_7,1)),
clojure.throw_if(clojure.nil_QMARK_(prefix_8),"prefix cannot be nil"),
((function __loop(){var _rtn,_cnt;(list__739_10=clojure.seq(args_9));do{_cnt=0;
_rtn=((list__739_10)?(((arg_11=clojure.first(list__739_10)),
clojure.apply(clojure.load_lib,prefix_8,clojure.prependss(arg_11,opts_3))),
(_cnt=1,_t0=clojure.rest(list__739_10),list__739_10=_t0)):(null))}while(_cnt);return _rtn;})()))))),
(_cnt=1,_t0=clojure.rest(list__739_5),list__739_5=_t0)):(null))}while(_cnt);return _rtn;})())))}))))})();

//======
//(defn require "Loads libs, skipping any that are already loaded. Each argument is\n  either a libspec that identifies a lib, a prefix list that identifies\n  multiple libs whose names share a common prefix, or a flag that modifies\n  how all the identified libs are loaded. Use :require in the ns macro \n  in preference to calling this directly.\n\n  Libs\n\n  A 'lib' is a named set of resources in classpath whose contents define a\n  library of Clojure code. Lib names are symbols and each lib is associated\n  with a Clojure namespace and a Java package that share its name. A lib's\n  name also locates its root directory within classpath using Java's\n  package name to classpath-relative path mapping. All resources in a lib\n  should be contained in the directory structure under its root directory.\n  All definitions a lib makes should be in its associated namespace.\n\n  'require loads a lib by loading its root resource. The root resource path\n  is derived from the root directory path by repeating its last component\n  and appending '.clj'. For example, the lib 'x.y.z has root directory\n  <classpath>/x/y/z; root resource <classpath>/x/y/z/z.clj. The root\n  resource should contain code to create the lib's namespace and load any\n  additional lib resources.\n\n  Libspecs\n\n  A libspec is a lib name or a vector containing a lib name followed by\n  options expressed as sequential keywords and arguments.\n\n  Recognized options: :as\n  :as takes a symbol as its argument and makes that symbol an alias to the\n    lib's namespace in the current namespace.\n\n  Prefix Lists\n\n  It's common for Clojure code to depend on several libs whose names have\n  the same prefix. When specifying libs, prefix lists can be used to reduce\n  repetition. A prefix list contains the shared prefix followed by libspecs\n  with the shared prefix removed from the lib names. After removing the\n  prefix, the names that remain must not contain any periods.\n\n  Flags\n\n  A flag is a keyword.\n  Recognized flags: :reload, :reload-all, :verbose\n  :reload forces loading of all the identified libs even if they are\n    already loaded\n  :reload-all implies :reload and also forces loading of all libs that the\n    identified libs directly or indirectly load via require or use\n  :verbose triggers printing information about each load, alias, and refer" [& args] (apply load-libs :require args))
//---
(function __clojure_fn_7671(){
return (clojure.JS.def(clojure,"require",clojure.JS.variatic(0,(function __clojure_fn_7671_require_7673(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply(clojure.load_libs,":require",args_1))}))))})();

//======
//(defn use "Like 'require, but also refers to each lib's namespace using\n  clojure/refer. Use :use in the ns macro in preference to calling\n  this directly.\n\n  'use accepts additional options in libspecs: :exclude, :only, :rename.\n  The arguments and semantics for :exclude, :only, and :rename are the same\n  as those documented for clojure/refer." [& args] (apply load-libs :require :use args))
//---
(function __clojure_fn_7683(){
return (clojure.JS.def(clojure,"use",clojure.JS.variatic(0,(function __clojure_fn_7683_use_7685(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply(clojure.load_libs,":require",":use",args_1))}))))})();

//======
//(defn loaded-libs "Returns a sorted set of symbols naming the currently loaded libs" [] (clojure/deref *loaded-libs*))
//---
(function __clojure_fn_7695(){
return (clojure.JS.def(clojure,"loaded_libs",(function __clojure_fn_7695_loaded_libs_7697(){
return (clojure.deref(clojure._STAR_loaded_libs_STAR_))})))})();

//======
//(defn load "Loads Clojure code from resources in classpath. A path is interpreted as\n  classpath-relative if it begins with a slash or relative to the root\n  directory for the current namespace otherwise." [& paths] (doseq path paths (let [path (if (.startsWith path "/") path (str (root-directory (ns-name *ns*)) \/ path))] (when *loading-verbosely* (printf "(clojure/load \"%s\")\n" path) (flush)) (throw-if (*pending-paths* path) "cannot load '%s' again while it is loading" path) (binding [*pending-paths* (conj *pending-paths* path)] (.loadResourceScript clojure.lang.RT (.substring path 1))))))
//---
(function __clojure_fn_7710(){
return (clojure.JS.def(clojure,"load",clojure.JS.variatic(0,(function __clojure_fn_7710_load_7712(){
var path_4,path_3,paths_1,list__739_2,paths_1=clojure.JS.rest_args(this,arguments,0);
return (((function __loop(){var _rtn,_cnt;(list__739_2=clojure.seq(paths_1));do{_cnt=0;
_rtn=((list__739_2)?(((path_3=clojure.first(list__739_2)),
((path_4=(((path_3).startsWith("/"))?(path_3):(clojure.str(clojure.root_directory(clojure.ns_name(clojure._STAR_ns_STAR_)),"/",path_3)))),
((clojure._STAR_loading_verbosely_STAR_)?(clojure.printf("(clojure/load \"%s\")\n",path_4),
clojure.flush()):(null)),
clojure.throw_if(clojure._STAR_pending_paths_STAR_(path_4),"cannot load '%s' again while it is loading",path_4),
clojure.lang.Var.pushThreadBindings(clojure.hash_map(clojure._var__STAR_pending_paths_STAR_,clojure.conj(clojure._STAR_pending_paths_STAR_,path_4))),
(function __clojure_fn_7710_load_7712_fn_7714(){
return ((function __try(){try{var _rtn=(clojure.lang.RT.loadResourceScript((path_4).substring(1)))}
finally{clojure.lang.Var.popThreadBindings()}})())})())),
(_cnt=1,_t0=clojure.rest(list__739_2),list__739_2=_t0)):(null))}while(_cnt);return _rtn;})()))}))))})();

//======
//(defn get-in "returns the value in a nested associative structure, where ks is a sequence of keys" [m ks] (reduce get m ks))
//---
(function __clojure_fn_7725(){
return (clojure.JS.def(clojure,"get_in",(function __clojure_fn_7725_get_in_7727(m_1,ks_2){
return (clojure.reduce(clojure.get,m_1,ks_2))})))})();

//======
//(defn assoc-in "Associates a value in a nested associative structure, where ks is a\n  sequence of keys and v is the new value and returns a new nested structure.  \n  If any levels do not exist, hash-maps will be created." [m [k & ks] v] (if ks (assoc m k (assoc-in (get m k) ks v)) (assoc m k v)))
//---
(function __clojure_fn_7739(){
return (clojure.JS.def(clojure,"assoc_in",(function __clojure_fn_7739_assoc_in_7742(m_1,p__7741_2,v_3){
var vec__7744_4,k_5,ks_6;
return (((vec__7744_4=p__7741_2),
(k_5=clojure.nth(vec__7744_4,0,null)),
(ks_6=clojure.nthrest(vec__7744_4,1)),
((ks_6)?(clojure.assoc(m_1,k_5,clojure.assoc_in(clojure.get(m_1,k_5),ks_6,v_3))):(clojure.assoc(m_1,k_5,v_3)))))})))})();

//======
//(defn update-in "'Updates' a value in a nested associative structure, where ks is a\n  sequence of keys and f is a function that will take the old value\n  and return the new value, and returns a new nested structure.  \n  If any levels do not exist, hash-maps will be created." ([m [k & ks] f] (if ks (assoc m k (update-in (get m k) ks f)) (assoc m k (f (get m k))))))
//---
(function __clojure_fn_7755(){
return (clojure.JS.def(clojure,"update_in",(function __clojure_fn_7755_update_in_7758(m_1,p__7757_2,f_3){
var vec__7760_4,ks_6,k_5;
return (((vec__7760_4=p__7757_2),
(k_5=clojure.nth(vec__7760_4,0,null)),
(ks_6=clojure.nthrest(vec__7760_4,1)),
((ks_6)?(clojure.assoc(m_1,k_5,clojure.update_in(clojure.get(m_1,k_5),ks_6,f_3))):(clojure.assoc(m_1,k_5,f_3(clojure.get(m_1,k_5)))))))})))})();

//======
//(defn empty? "Returns true if coll has no items - same as (not (seq coll)). \n  Please use the idiom (seq x) rather than (not (empty? x))" [coll] (not (seq coll)))
//---
(function __clojure_fn_7769(){
return (clojure.JS.def(clojure,"empty_QMARK_",(function __clojure_fn_7769_empty_QMARK_7771(coll_1){
return (clojure.not(clojure.seq(coll_1)))})))})();

//======
//(defn coll? "Returns true if x implements IPersistentCollection" [x] (instance? clojure.lang.IPersistentCollection x))
//---
(function __clojure_fn_7781(){
return (clojure.JS.def(clojure,"coll_QMARK_",(function __clojure_fn_7781_coll_QMARK_7783(x_1){
return (clojure.instance_QMARK_(clojure.lang.IPersistentCollection,x_1))})))})();

//======
//(defn list? "Returns true if x implements IPersistentList" [x] (instance? clojure.lang.IPersistentList x))
//---
(function __clojure_fn_7793(){
return (clojure.JS.def(clojure,"list_QMARK_",(function __clojure_fn_7793_list_QMARK_7795(x_1){
return (clojure.instance_QMARK_(clojure.lang.IPersistentList,x_1))})))})();

//======
//(defn set? "Returns true if x implements IPersistentSet" [x] (instance? clojure.lang.IPersistentSet x))
//---
(function __clojure_fn_7805(){
return (clojure.JS.def(clojure,"set_QMARK_",(function __clojure_fn_7805_set_QMARK_7807(x_1){
return (clojure.instance_QMARK_(clojure.lang.IPersistentSet,x_1))})))})();

//======
//(defn number? "Returns true if x is a Number" [x] (instance? Number x))
//---
(function __clojure_fn_7817(){
return (clojure.JS.def(clojure,"number_QMARK_",(function __clojure_fn_7817_number_QMARK_7819(x_1){
return (clojure.instance_QMARK_(java.lang.Number,x_1))})))})();

//======
//(defn fn? "Returns true if x implements IFn. Note that many data structures \n  (e.g. sets and maps) implement IFn" [x] (instance? clojure.lang.IFn x))
//---
(function __clojure_fn_7829(){
return (clojure.JS.def(clojure,"fn_QMARK_",(function __clojure_fn_7829_fn_QMARK_7831(x_1){
return (clojure.instance_QMARK_(clojure.lang.IFn,x_1))})))})();

//======
//(defn integer? "Returns true if n is an integer" [n] (or (instance? Integer n) (instance? Long n) (instance? BigInteger n)))
//---
(function __clojure_fn_7841(){
return (clojure.JS.def(clojure,"integer_QMARK_",(function __clojure_fn_7841_integer_QMARK_7843(n_1){
var or__202_3,or__202_2;
return (((or__202_2=clojure.instance_QMARK_(java.lang.Integer,n_1)),
((or__202_2)?(or__202_2):(((or__202_3=clojure.instance_QMARK_(java.lang.Long,n_1)),
((or__202_3)?(or__202_3):(clojure.instance_QMARK_(java.math.BigInteger,n_1))))))))})))})();

//======
//(defn ratio? "Returns true if n is a Ratio" [n] (instance? clojure.lang.Ratio n))
//---
(function __clojure_fn_7853(){
return (clojure.JS.def(clojure,"ratio_QMARK_",(function __clojure_fn_7853_ratio_QMARK_7855(n_1){
return (clojure.instance_QMARK_(clojure.lang.Ratio,n_1))})))})();

//======
//(defn decimal? "Returns true if n is a BigDecimal" [n] (instance? BigDecimal n))
//---
(function __clojure_fn_7865(){
return (clojure.JS.def(clojure,"decimal_QMARK_",(function __clojure_fn_7865_decimal_QMARK_7867(n_1){
return (clojure.instance_QMARK_(java.math.BigDecimal,n_1))})))})();

//======
//(defn float? "Returns true if n is a floating point number" [n] (or (instance? Double n) (instance? Float n)))
//---
(function __clojure_fn_7877(){
return (clojure.JS.def(clojure,"float_QMARK_",(function __clojure_fn_7877_float_QMARK_7879(n_1){
var or__202_2;
return (((or__202_2=clojure.instance_QMARK_(java.lang.Double,n_1)),
((or__202_2)?(or__202_2):(clojure.instance_QMARK_(java.lang.Float,n_1)))))})))})();

//======
//(defn rational? [n] "Returns true if n is a rational number" (or (integer? n) (ratio? n) (decimal? n)))
//---
(function __clojure_fn_7889(){
return (clojure.JS.def(clojure,"rational_QMARK_",(function __clojure_fn_7889_rational_QMARK_7891(n_1){
var or__202_3,or__202_2;
return ("Returns true if n is a rational number",
((or__202_2=clojure.integer_QMARK_(n_1)),
((or__202_2)?(or__202_2):(((or__202_3=clojure.ratio_QMARK_(n_1)),
((or__202_3)?(or__202_3):(clojure.decimal_QMARK_(n_1))))))))})))})();

//======
//(defn associative? "Returns true if coll implements Associative" [coll] (instance? clojure.lang.Associative coll))
//---
(function __clojure_fn_7901(){
return (clojure.JS.def(clojure,"associative_QMARK_",(function __clojure_fn_7901_associative_QMARK_7903(coll_1){
return (clojure.instance_QMARK_(clojure.lang.Associative,coll_1))})))})();

//======
//(defn sequential? "Returns true if coll implements Sequential" [coll] (instance? clojure.lang.Sequential coll))
//---
(function __clojure_fn_7913(){
return (clojure.JS.def(clojure,"sequential_QMARK_",(function __clojure_fn_7913_sequential_QMARK_7915(coll_1){
return (clojure.instance_QMARK_(clojure.lang.Sequential,coll_1))})))})();

//======
//(defn sorted? "Returns true if coll implements Sorted" [coll] (instance? clojure.lang.Sorted coll))
//---
(function __clojure_fn_7925(){
return (clojure.JS.def(clojure,"sorted_QMARK_",(function __clojure_fn_7925_sorted_QMARK_7927(coll_1){
return (clojure.instance_QMARK_(clojure.lang.Sorted,coll_1))})))})();

//======
//(defn reversible? "Returns true if coll implements Reversible" [coll] (instance? clojure.lang.Reversible coll))
//---
(function __clojure_fn_7937(){
return (clojure.JS.def(clojure,"reversible_QMARK_",(function __clojure_fn_7937_reversible_QMARK_7939(coll_1){
return (clojure.instance_QMARK_(clojure.lang.Reversible,coll_1))})))})();

//======
//(defn even? "Returns true if n is even, throws an exception if n is not an integer" [n] (zero? (bit-and n 1)))
//---
(function __clojure_fn_7949(){
return (clojure.JS.def(clojure,"even_QMARK_",(function __clojure_fn_7949_even_QMARK_7951(n_1){
return (clojure.lang.Numbers.isZero(clojure.bit_and(n_1,1)))})))})();

//======
//(defn odd? "Returns true if n is odd, throws an exception if n is not an integer" [n] (not (even? n)))
//---
(function __clojure_fn_7961(){
return (clojure.JS.def(clojure,"odd_QMARK_",(function __clojure_fn_7961_odd_QMARK_7963(n_1){
return (clojure.not(clojure.even_QMARK_(n_1)))})))})();

//======
//(defn pmap "Like map, except f is applied in parallel. Semi-lazy in that the\n  parallel computation stays ahead of the consumption, but doesn't\n  realize the entire result unless required. Only useful for\n  computationally intensive functions where the time of f dominates\n  the coordination overhead." ([f coll] (let [n (inc (.. Runtime getRuntime availableProcessors)) agents (doall (map (fn* [p1__7967] (agent (f p1__7967))) (take n coll))) wget (fn [a] (await1 a) (clojure/deref a)) step (fn step [[x & xs :as s] [a & as :as acycle]] (if s (let [v (wget a)] (send a (fn [_] (f x))) (lazy-cons v (step xs as))) (map wget (take (count agents) acycle))))] (step (drop n coll) (cycle agents)))) ([f coll & colls] (let [step (fn step [cs] (when (every? seq cs) (lazy-cons (map first cs) (step (map rest cs)))))] (pmap (fn* [p1__7968] (apply f p1__7968)) (step (cons coll colls))))))
//---
(function __clojure_fn_8006(){
return (clojure.JS.def(clojure,"pmap",clojure.JS.variatic(2,(function __clojure_fn_8006_pmap_8008(f_1,coll_2){switch(arguments.length){
case 2:var agents_4,wget_5,n_3,step_6;
return (((n_3=clojure.lang.Numbers.inc((java.lang.Runtime.getRuntime()).availableProcessors())),
(agents_4=clojure.doall(clojure.map((function __clojure_fn_8006_pmap_8008_fn_8010(p1__7967_1){
return (clojure.agent(f_1(p1__7967_1)))}),clojure.take(n_3,coll_2)))),
(wget_5=(function __clojure_fn_8006_pmap_8008_wget_8013(a_1){
return (clojure.await1(a_1),
clojure.deref(a_1))})),
(step_6=(function __clojure_fn_8006_pmap_8008_step_8018(p__8016_1,p__8017_2){
var xs_5,as_9,v_11,vec__8020_7,a_8,x_4,s_6,vec__8019_3,acycle_10,step_0=arguments.callee;
return (((vec__8019_3=p__8016_1),
(x_4=clojure.nth(vec__8019_3,0,null)),
(xs_5=clojure.nthrest(vec__8019_3,1)),
(s_6=vec__8019_3),
(vec__8020_7=p__8017_2),
(a_8=clojure.nth(vec__8020_7,0,null)),
(as_9=clojure.nthrest(vec__8020_7,1)),
(acycle_10=vec__8020_7),
((s_6)?(((v_11=wget_5(a_8)),
clojure.send(a_8,(function __clojure_fn_8006_pmap_8008_step_8018_fn_8021(__1){
return (f_1(x_4))})),
(new clojure.lang.LazyCons((function __clojure_fn_8006_pmap_8008_step_8018_fn_8025(G__8024_1){switch(arguments.length){
case 0:return (v_11)}
return (step_0(xs_5,as_9))}))))):(clojure.map(wget_5,clojure.take(clojure.count(agents_4),acycle_10))))))})),
step_6(clojure.drop(n_3,coll_2),clojure.cycle(agents_4))))}
var step_4,colls_3,colls_3=clojure.JS.rest_args(this,arguments,2);
return (((step_4=(function __clojure_fn_8006_pmap_8008_step_8031(cs_1){
var step_0=arguments.callee;
return (((clojure.every_QMARK_(clojure.seq,cs_1))?((new clojure.lang.LazyCons((function __clojure_fn_8006_pmap_8008_step_8031_fn_8033(G__8032_1){switch(arguments.length){
case 0:return (clojure.map(clojure.first,cs_1))}
return (step_0(clojure.map(clojure.rest,cs_1)))})))):(null)))})),
clojure.pmap((function __clojure_fn_8006_pmap_8008_fn_8038(p1__7968_1){
return (clojure.apply(f_1,p1__7968_1))}),step_4(clojure.cons(coll_2,colls_3)))))}))))})();

//======
//(def *1)
//---
(function __clojure_fn_8046(){
return (clojure.JS.def(clojure,"_STAR_1",null))})();

//======
//(def *2)
//---
(function __clojure_fn_8052(){
return (clojure.JS.def(clojure,"_STAR_2",null))})();

//======
//(def *3)
//---
(function __clojure_fn_8058(){
return (clojure.JS.def(clojure,"_STAR_3",null))})();

//======
//(def *e)
//---
(function __clojure_fn_8064(){
return (clojure.JS.def(clojure,"_STAR_e",null))})();
