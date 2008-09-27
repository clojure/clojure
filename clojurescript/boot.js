
//======
//(in-ns (quote clojure))
//---
(function __tmp_fn_2662(){
return (clojure.in_ns.apply(null,["'clojure"]))}).apply(null,[]);

//======
//(def list (. clojure.lang.PersistentList creator))
//---
(function __clojure_fn_2671(){
return (clojure.JS.def(clojure,"list",clojure.lang.PersistentList.creator))}).apply(null,[]);

//======
//(def cons (fn* cons [x seq] (. clojure.lang.RT (cons x seq))))
//---
(function __clojure_fn_2679(){
return (clojure.JS.def(clojure,"cons",(function __clojure_fn_2679_cons_2681(x_1,seq_2){
var cons_0=arguments.callee;
return (clojure.lang.RT.cons(x_1,seq_2))})))}).apply(null,[]);

//======
//(def conj (fn conj ([coll x] (. clojure.lang.RT (conj coll x))) ([coll x & xs] (if xs (recur (conj coll x) (first xs) (rest xs)) (conj coll x)))))
//---
(function __clojure_fn_2714(){
return (clojure.JS.def(clojure,"conj",clojure.JS.variadic(2,(function __clojure_fn_2714_conj_2716(coll_1,x_2){switch(arguments.length){
case 2:var conj_0=arguments.callee;
return (clojure.lang.RT.conj(coll_1,x_2))}
var _cnt,_rtn,xs_3,conj_0=arguments.callee,xs_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((xs_3)?((_cnt=1,_rtn=[conj_0.apply(null,[coll_1,x_2]),clojure.first.apply(null,[xs_3]),clojure.rest.apply(null,[xs_3])],coll_1=_rtn[0],x_2=_rtn[1],xs_3=_rtn[2])):(conj_0.apply(null,[coll_1,x_2])))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(def second (fn second [x] (first (rest x))))
//---
(function __clojure_fn_2724(){
return (clojure.JS.def(clojure,"second",(function __clojure_fn_2724_second_2726(x_1){
var second_0=arguments.callee;
return (clojure.first.apply(null,[clojure.rest.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def ffirst (fn ffirst [x] (first (first x))))
//---
(function __clojure_fn_2734(){
return (clojure.JS.def(clojure,"ffirst",(function __clojure_fn_2734_ffirst_2736(x_1){
var ffirst_0=arguments.callee;
return (clojure.first.apply(null,[clojure.first.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def rfirst (fn rfirst [x] (rest (first x))))
//---
(function __clojure_fn_2744(){
return (clojure.JS.def(clojure,"rfirst",(function __clojure_fn_2744_rfirst_2746(x_1){
var rfirst_0=arguments.callee;
return (clojure.rest.apply(null,[clojure.first.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def frest (fn frest [x] (first (rest x))))
//---
(function __clojure_fn_2754(){
return (clojure.JS.def(clojure,"frest",(function __clojure_fn_2754_frest_2756(x_1){
var frest_0=arguments.callee;
return (clojure.first.apply(null,[clojure.rest.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def rrest (fn rrest [x] (rest (rest x))))
//---
(function __clojure_fn_2764(){
return (clojure.JS.def(clojure,"rrest",(function __clojure_fn_2764_rrest_2766(x_1){
var rrest_0=arguments.callee;
return (clojure.rest.apply(null,[clojure.rest.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def seq? (fn seq? [x] (instance? clojure.lang.ISeq x)))
//---
(function __clojure_fn_2784(){
return (clojure.JS.def(clojure,"seq_QMARK_",(function __clojure_fn_2784_seq_QMARK_2786(x_1){
var seq_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_.apply(null,[clojure.lang.ISeq,x_1]))})))}).apply(null,[]);

//======
//(def string? (fn string? [x] (instance? String x)))
//---
(function __clojure_fn_2794(){
return (clojure.JS.def(clojure,"string_QMARK_",(function __clojure_fn_2794_string_QMARK_2796(x_1){
var string_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_.apply(null,[java.lang.String,x_1]))})))}).apply(null,[]);

//======
//(def map? (fn map? [x] (instance? clojure.lang.IPersistentMap x)))
//---
(function __clojure_fn_2804(){
return (clojure.JS.def(clojure,"map_QMARK_",(function __clojure_fn_2804_map_QMARK_2806(x_1){
var map_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentMap,x_1]))})))}).apply(null,[]);

//======
//(def vector? (fn vector? [x] (instance? clojure.lang.IPersistentVector x)))
//---
(function __clojure_fn_2814(){
return (clojure.JS.def(clojure,"vector_QMARK_",(function __clojure_fn_2814_vector_QMARK_2816(x_1){
var vector_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentVector,x_1]))})))}).apply(null,[]);

//======
//(def sigs (fn [fdecl] (if (seq? (first fdecl)) (loop [ret [] fdecl fdecl] (if fdecl (recur (conj ret (first (first fdecl))) (rest fdecl)) (seq ret))) (list (first fdecl)))))
//---
(function __clojure_fn_2825(){
return (clojure.JS.def(clojure,"sigs",(function __clojure_fn_2825_sigs_2827(fdecl_1){
var ret_2,fdecl_3;
return (((clojure.seq_QMARK_.apply(null,[clojure.first.apply(null,[fdecl_1])]))?(((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentVector.EMPTY),
(fdecl_3=fdecl_1);do{_cnt=0;
_rtn=((fdecl_3)?((_cnt=1,_rtn=[clojure.conj.apply(null,[ret_2,clojure.first.apply(null,[clojure.first.apply(null,[fdecl_3])])]),clojure.rest.apply(null,[fdecl_3])],ret_2=_rtn[0],fdecl_3=_rtn[1])):(clojure.seq.apply(null,[ret_2])))}while(_cnt);return _rtn;})())):(clojure.list.apply(null,[clojure.first.apply(null,[fdecl_1])]))))})))}).apply(null,[]);

//======
//(def meta (fn meta [x] (if (instance? clojure.lang.IObj x) (. x (meta)))))
//---
(function __clojure_fn_2841(){
return (clojure.JS.def(clojure,"meta",(function __clojure_fn_2841_meta_2843(x_1){
var meta_0=arguments.callee;
return (((clojure.instance_QMARK_.apply(null,[clojure.lang.IObj,x_1]))?((x_1).meta()):(null)))})))}).apply(null,[]);

//======
//(def with-meta (fn with-meta [x m] (. x (withMeta m))))
//---
(function __clojure_fn_2851(){
return (clojure.JS.def(clojure,"with_meta",(function __clojure_fn_2851_with_meta_2853(x_1,m_2){
var with_meta_0=arguments.callee;
return ((x_1).withMeta(m_2))})))}).apply(null,[]);

//======
//(def last (fn last [s] (if (rest s) (recur (rest s)) (first s))))
//---
(function __clojure_fn_2861(){
return (clojure.JS.def(clojure,"last",(function __clojure_fn_2861_last_2863(s_1){
var _cnt,_rtn,last_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.rest.apply(null,[s_1]))?((_cnt=1,_rtn=[clojure.rest.apply(null,[s_1])],s_1=_rtn[0])):(clojure.first.apply(null,[s_1])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def butlast (fn butlast [s] (loop [ret [] s s] (if (rest s) (recur (conj ret (first s)) (rest s)) (seq ret)))))
//---
(function __clojure_fn_2871(){
return (clojure.JS.def(clojure,"butlast",(function __clojure_fn_2871_butlast_2873(s_1){
var ret_2,s_3,butlast_0=arguments.callee;
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentVector.EMPTY),
(s_3=s_1);do{_cnt=0;
_rtn=((clojure.rest.apply(null,[s_3]))?((_cnt=1,_rtn=[clojure.conj.apply(null,[ret_2,clojure.first.apply(null,[s_3])]),clojure.rest.apply(null,[s_3])],ret_2=_rtn[0],s_3=_rtn[1])):(clojure.seq.apply(null,[ret_2])))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn cast "Throws a ClassCastException if x is not a c, else returns x." [c x] (. c (cast x)))
//---
(function __clojure_fn_2890(){
return (clojure.JS.def(clojure,"cast",(function __clojure_fn_2890_cast_2892(c_1,x_2){
return ((c_1).cast(x_2))})))}).apply(null,[]);

//======
//(defn to-array "Returns an array of Objects containing the contents of coll, which\n  can be any Collection.  Maps to java.util.Collection.toArray()." [coll] (. clojure.lang.RT (toArray coll)))
//---
(function __clojure_fn_2902(){
return (clojure.JS.def(clojure,"to_array",(function __clojure_fn_2902_to_array_2904(coll_1){
return (clojure.lang.RT.toArray(coll_1))})))}).apply(null,[]);

//======
//(defn vector "Creates a new vector containing the args." ([] []) ([& args] (. clojure.lang.LazilyPersistentVector (create args))))
//---
(function __clojure_fn_2915(){
return (clojure.JS.def(clojure,"vector",clojure.JS.variadic(0,(function __clojure_fn_2915_vector_2917(){switch(arguments.length){
case 0:return (clojure.lang.PersistentVector.EMPTY)}
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.LazilyPersistentVector.create(args_1))}))))}).apply(null,[]);

//======
//(defn vec "Creates a new vector containing the contents of coll." ([coll] (. clojure.lang.LazilyPersistentVector (createOwning (to-array coll)))))
//---
(function __clojure_fn_2928(){
return (clojure.JS.def(clojure,"vec",(function __clojure_fn_2928_vec_2930(coll_1){
return (clojure.lang.LazilyPersistentVector.createOwning(clojure.to_array.apply(null,[coll_1])))})))}).apply(null,[]);

//======
//(defn hash-set "Returns a new hash set with supplied keys." ([] #{}) ([& keys] (. clojure.lang.PersistentHashSet (create keys))))
//---
(function __clojure_fn_2948(){
return (clojure.JS.def(clojure,"hash_set",clojure.JS.variadic(0,(function __clojure_fn_2948_hash_set_2950(){switch(arguments.length){
case 0:return (clojure.lang.PersistentHashSet.EMPTY)}
var keys_1,keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentHashSet.create(keys_1))}))))}).apply(null,[]);

//======
//(defn sorted-map "keyval => key val\n  Returns a new sorted map with supplied mappings." ([& keyvals] (. clojure.lang.PersistentTreeMap (create keyvals))))
//---
(function __clojure_fn_2961(){
return (clojure.JS.def(clojure,"sorted_map",clojure.JS.variadic(0,(function __clojure_fn_2961_sorted_map_2963(){
var keyvals_1,keyvals_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentTreeMap.create(keyvals_1))}))))}).apply(null,[]);

//======
//(defn sorted-set "Returns a new sorted set with supplied keys." ([& keys] (. clojure.lang.PersistentTreeSet (create keys))))
//---
(function __clojure_fn_2973(){
return (clojure.JS.def(clojure,"sorted_set",clojure.JS.variadic(0,(function __clojure_fn_2973_sorted_set_2975(){
var keys_1,keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentTreeSet.create(keys_1))}))))}).apply(null,[]);

//======
//(defn sorted-map-by "keyval => key val\n  Returns a new sorted map with supplied mappings, using the supplied comparator." ([comparator & keyvals] (. clojure.lang.PersistentTreeMap (create comparator keyvals))))
//---
(function __clojure_fn_2985(){
return (clojure.JS.def(clojure,"sorted_map_by",clojure.JS.variadic(1,(function __clojure_fn_2985_sorted_map_by_2987(comparator_1){
var keyvals_2,keyvals_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentTreeMap.create(comparator_1,keyvals_2))}))))}).apply(null,[]);

//======
//(defn nil? "Returns true if x is nil, false otherwise." {:tag Boolean} [x] (identical? x nil))
//---
(function __clojure_fn_3018(){
return (clojure.JS.def(clojure,"nil_QMARK_",(function __clojure_fn_3018_nil_QMARK_3020(x_1){
return (clojure.identical_QMARK_.apply(null,[x_1,null]))})))}).apply(null,[]);

//======
//(defn false? "Returns true if x is the value false, false otherwise." {:tag Boolean} [x] (identical? x false))
//---
(function __clojure_fn_3030(){
return (clojure.JS.def(clojure,"false_QMARK_",(function __clojure_fn_3030_false_QMARK_3032(x_1){
return (clojure.identical_QMARK_.apply(null,[x_1,false]))})))}).apply(null,[]);

//======
//(defn true? "Returns true if x is the value true, false otherwise." {:tag Boolean} [x] (identical? x true))
//---
(function __clojure_fn_3042(){
return (clojure.JS.def(clojure,"true_QMARK_",(function __clojure_fn_3042_true_QMARK_3044(x_1){
return (clojure.identical_QMARK_.apply(null,[x_1,true]))})))}).apply(null,[]);

//======
//(defn not "Returns true if x is logical false, false otherwise." {:tag Boolean} [x] (if x false true))
//---
(function __clojure_fn_3054(){
return (clojure.JS.def(clojure,"not",(function __clojure_fn_3054_not_3056(x_1){
return (((x_1)?(false):(true)))})))}).apply(null,[]);

//======
//(defn str "With no args, returns the empty string. With one arg x, returns\n  x.toString().  (str nil) returns the empty string. With more than\n  one arg, returns the concatenation of the str values of the args." {:tag String} ([] "") ([x] (if (nil? x) "" (. x (toString)))) ([x & ys] (loop [sb (new StringBuilder (str x)) more ys] (if more (recur (. sb (append (str (first more)))) (rest more)) (str sb)))))
//---
(function __clojure_fn_3068(){
return (clojure.JS.def(clojure,"str",clojure.JS.variadic(1,(function __clojure_fn_3068_str_3070(x_1){switch(arguments.length){
case 1:return (((clojure.nil_QMARK_.apply(null,[x_1]))?(""):((x_1).toString())))
case 0:return ("")}
var more_4,ys_2,sb_3,ys_2=clojure.JS.rest_args(this,arguments,1);
return (((function __loop(){var _rtn,_cnt;(sb_3=(new java.lang.StringBuilder(clojure.str.apply(null,[x_1])))),
(more_4=ys_2);do{_cnt=0;
_rtn=((more_4)?((_cnt=1,_rtn=[(sb_3).append(clojure.str.apply(null,[clojure.first.apply(null,[more_4])])),clojure.rest.apply(null,[more_4])],sb_3=_rtn[0],more_4=_rtn[1])):(clojure.str.apply(null,[sb_3])))}while(_cnt);return _rtn;})()))}))))}).apply(null,[]);

//======
//(defn symbol "Returns a Symbol with the given namespace and name." ([name] (. clojure.lang.Symbol (intern name))) ([ns name] (. clojure.lang.Symbol (intern ns name))))
//---
(function __clojure_fn_3083(){
return (clojure.JS.def(clojure,"symbol",(function __clojure_fn_3083_symbol_3085(ns_1,name_2){switch(arguments.length){
case 1:var name_1=arguments[0];
return (clojure.lang.Symbol.intern(name_1))}
return (clojure.lang.Symbol.intern(ns_1,name_2))})))}).apply(null,[]);

//======
//(defn keyword "Returns a Keyword with the given namespace and name.  Do not use :\n  in the keyword strings, it will be added automatically." ([name] (. clojure.lang.Keyword (intern nil name))) ([ns name] (. clojure.lang.Keyword (intern ns name))))
//---
(function __clojure_fn_3097(){
return (clojure.JS.def(clojure,"keyword",(function __clojure_fn_3097_keyword_3099(ns_1,name_2){switch(arguments.length){
case 1:var name_1=arguments[0];
return (clojure.lang.Keyword.intern(null,name_1))}
return (clojure.lang.Keyword.intern(ns_1,name_2))})))}).apply(null,[]);

//======
//(defn gensym "Returns a new symbol with a unique name. If a prefix string is\n  supplied, the name is prefix# where # is some unique number. If\n  prefix is not supplied, the prefix is 'G'." ([] (gensym "G__")) ([prefix-string] (. clojure.lang.Symbol (intern (str prefix-string (str (. clojure.lang.RT (nextID))))))))
//---
(function __clojure_fn_3111(){
return (clojure.JS.def(clojure,"gensym",(function __clojure_fn_3111_gensym_3113(prefix_string_1){switch(arguments.length){
case 0:return (clojure.gensym.apply(null,["G__"]))}
return (clojure.lang.Symbol.intern(clojure.str.apply(null,[prefix_string_1,clojure.str.apply(null,[clojure.lang.RT.nextID()])])))})))}).apply(null,[]);

//======
//(defn spread {:private true} [arglist] (cond (nil? arglist) nil (nil? (rest arglist)) (seq (first arglist)) :else (cons (first arglist) (spread (rest arglist)))))
//---
(function __clojure_fn_3130(){
return (clojure.JS.def(clojure,"spread",(function __clojure_fn_3130_spread_3132(arglist_1){
return (((clojure.nil_QMARK_.apply(null,[arglist_1]))?(null):(((clojure.nil_QMARK_.apply(null,[clojure.rest.apply(null,[arglist_1])]))?(clojure.seq.apply(null,[clojure.first.apply(null,[arglist_1])])):(((clojure.keyword("","else"))?(clojure.cons.apply(null,[clojure.first.apply(null,[arglist_1]),clojure.spread.apply(null,[clojure.rest.apply(null,[arglist_1])])])):(null)))))))})))}).apply(null,[]);

//======
//(defn list* "Creates a new list containing the item prepended to more." [item & more] (spread (cons item more)))
//---
(function __clojure_fn_3148(){
return (clojure.JS.def(clojure,"list_STAR_",clojure.JS.variadic(1,(function __clojure_fn_3148_list_STAR_3150(item_1){
var more_2,more_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.spread.apply(null,[clojure.cons.apply(null,[item_1,more_2])]))}))))}).apply(null,[]);

//======
//(defn delay? "returns true if x is a Delay created with delay" [x] (instance? clojure.lang.Delay x))
//---
(function __clojure_fn_3166(){
return (clojure.JS.def(clojure,"delay_QMARK_",(function __clojure_fn_3166_delay_QMARK_3168(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Delay,x_1]))})))}).apply(null,[]);

//======
//(defn force "If x is a Delay, returns the (possibly cached) value of its expression, else returns x" [x] (. clojure.lang.Delay (force x)))
//---
(function __clojure_fn_3178(){
return (clojure.JS.def(clojure,"force",(function __clojure_fn_3178_force_3180(x_1){
return (clojure.lang.Delay.force(x_1))})))}).apply(null,[]);

//======
//(defn fnseq "Returns a seq object whose first is first and whose rest is the\n  value produced by calling restfn with no arguments. restfn will be\n  called at most once per step in the sequence, e.g. calling rest\n  repeatedly on the head of the seq calls restfn once - the value it\n  yields is cached." [first restfn] (new clojure.lang.FnSeq first restfn))
//---
(function __clojure_fn_3190(){
return (clojure.JS.def(clojure,"fnseq",(function __clojure_fn_3190_fnseq_3192(first_1,restfn_2){
return ((new clojure.lang.FnSeq(first_1,restfn_2)))})))}).apply(null,[]);

//======
//(defn cache-seq "Given a seq s, returns a lazy seq that will touch each element of s\n  at most once, caching the results." [s] (when s (clojure.lang.CachedSeq. s)))
//---
(function __clojure_fn_3208(){
return (clojure.JS.def(clojure,"cache_seq",(function __clojure_fn_3208_cache_seq_3210(s_1){
return (((s_1)?((new clojure.lang.CachedSeq(s_1))):(null)))})))}).apply(null,[]);

//======
//(defn concat "Returns a lazy seq representing the concatenation of\tthe elements in the supplied colls." ([] nil) ([x] (seq x)) ([x y] (if (seq x) (lazy-cons (first x) (concat (rest x) y)) (seq y))) ([x y & zs] (let [cat (fn cat [xys zs] (if (seq xys) (lazy-cons (first xys) (cat (rest xys) zs)) (when zs (recur (first zs) (rest zs)))))] (cat (concat x y) zs))))
//---
(function __clojure_fn_3235(){
return (clojure.JS.def(clojure,"concat",clojure.JS.variadic(2,(function __clojure_fn_3235_concat_3237(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.seq.apply(null,[x_1]))?((new clojure.lang.LazyCons((function __clojure_fn_3235_concat_3237_fn_3242(G__3241_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[x_1]))}
return (clojure.concat.apply(null,[clojure.rest.apply(null,[x_1]),y_2]))})))):(clojure.seq.apply(null,[y_2]))))
case 1:return (clojure.seq.apply(null,[x_1]))
case 0:return (null)}
var cat_4,zs_3,zs_3=clojure.JS.rest_args(this,arguments,2);
return (((cat_4=(function __clojure_fn_3235_concat_3237_cat_3247(xys_1,zs_2){
var _cnt,_rtn,cat_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[xys_1]))?((new clojure.lang.LazyCons((function __clojure_fn_3235_concat_3237_cat_3247_fn_3249(G__3248_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[xys_1]))}
return (cat_0.apply(null,[clojure.rest.apply(null,[xys_1]),zs_2]))})))):(((zs_2)?((_cnt=1,_rtn=[clojure.first.apply(null,[zs_2]),clojure.rest.apply(null,[zs_2])],xys_1=_rtn[0],zs_2=_rtn[1])):(null))))
}while(_cnt);return _rtn;})),
cat_4.apply(null,[clojure.concat.apply(null,[x_1,y_2]),zs_3])))}))))}).apply(null,[]);

//======
//(defn = "Equality. Returns true if x equals y, false if not. Same as\n  Java x.equals(y) except it also works for nil, and compares\n  numbers in a type-independent manner.  Clojure's immutable data\n  structures define equals() (and thus =) as a value, not an identity,\n  comparison." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Util)) (clojure/list (quote clojure/equal)) (clojure/list x) (clojure/list y))), :inline-arities #{2}, :tag Boolean} ([x] true) ([x y] (. clojure.lang.Util (equal x y))) ([x y & more] (if (= x y) (if (rest more) (recur y (first more) (rest more)) (= y (first more))) false)))
//---
(function __clojure_fn_3267(){
return (clojure.JS.def(clojure,"_EQ_",clojure.JS.variadic(2,(function __clojure_fn_3267_EQ_3272(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Util.equal(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Util.equal(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Util.equal(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn not= "Same as (not (= obj1 obj2))" {:tag Boolean} ([x] false) ([x y] (not (= x y))) ([x y & more] (not (apply = x y more))))
//---
(function __clojure_fn_3286(){
return (clojure.JS.def(clojure,"not_EQ_",clojure.JS.variadic(2,(function __clojure_fn_3286_not_EQ_3288(x_1,y_2){switch(arguments.length){
case 2:return (clojure.not.apply(null,[clojure.lang.Util.equal(x_1,y_2)]))
case 1:return (false)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.not.apply(null,[clojure.apply.apply(null,[clojure._EQ_,x_1,y_2,more_3])]))}))))}).apply(null,[]);

//======
//(defn compare "Comparator. Returns 0 if x equals y, -1 if x is logically 'less\n  than' y, else 1. Same as Java x.compareTo(y) except it also works\n  for nil, and compares numbers in a type-independent manner. x must\n  implement Comparable" {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Util)) (clojure/list (quote clojure/compare)) (clojure/list x) (clojure/list y))), :tag Integer} [x y] (. clojure.lang.Util (compare x y)))
//---
(function __clojure_fn_3303(){
return (clojure.JS.def(clojure,"compare",(function __clojure_fn_3303_compare_3308(x_1,y_2){
return (clojure.lang.Util.compare(x_1,y_2))})))}).apply(null,[]);

//======
//(defn reduce "f should be a function of 2 arguments. If val is not supplied,\n  returns the result of applying f to the first 2 items in coll, then\n  applying f to that result and the 3rd item, etc. If coll contains no\n  items, f must accept no arguments as well, and reduce returns the\n  result of calling f with no arguments.  If coll has only 1 item, it\n  is returned and f is not called.  If val is supplied, returns the\n  result of applying f to val and the first item in coll, then\n  applying f to that result and the 2nd item, etc. If coll contains no\n  items, returns val and f is not called." ([f coll] (let [s (seq coll)] (if s (if (instance? clojure.lang.IReduce s) (. s (reduce f)) (reduce f (first s) (rest s))) (f)))) ([f val coll] (let [s (seq coll)] (if (instance? clojure.lang.IReduce s) (. s (reduce f val)) ((fn [f val s] (if s (recur f (f val (first s)) (rest s)) val)) f val s)))))
//---
(function __clojure_fn_3340(){
return (clojure.JS.def(clojure,"reduce",(function __clojure_fn_3340_reduce_3342(f_1,val_2,coll_3){switch(arguments.length){
case 2:var s_3,coll_2=arguments[1];
return (((s_3=clojure.seq.apply(null,[coll_2])),
((s_3)?(((clojure.instance_QMARK_.apply(null,[clojure.lang.IReduce,s_3]))?((s_3).reduce(f_1)):(clojure.reduce.apply(null,[f_1,clojure.first.apply(null,[s_3]),clojure.rest.apply(null,[s_3])])))):(f_1.apply(null,[])))))}
var s_4;
return (((s_4=clojure.seq.apply(null,[coll_3])),
((clojure.instance_QMARK_.apply(null,[clojure.lang.IReduce,s_4]))?((s_4).reduce(f_1,val_2)):((function __clojure_fn_3340_reduce_3342_fn_3345(f_1,val_2,s_3){
var _cnt,_rtn;
do{_cnt=0;_rtn=((s_3)?((_cnt=1,_rtn=[f_1,f_1.apply(null,[val_2,clojure.first.apply(null,[s_3])]),clojure.rest.apply(null,[s_3])],f_1=_rtn[0],val_2=_rtn[1],s_3=_rtn[2])):(val_2))
}while(_cnt);return _rtn;}).apply(null,[f_1,val_2,s_4])))))})))}).apply(null,[]);

//======
//(defn reverse "Returns a seq of the items in coll in reverse order. Not lazy." [coll] (reduce conj nil coll))
//---
(function __clojure_fn_3356(){
return (clojure.JS.def(clojure,"reverse",(function __clojure_fn_3356_reverse_3358(coll_1){
return (clojure.reduce.apply(null,[clojure.conj,null,coll_1]))})))}).apply(null,[]);

//======
//(defn + "Returns the sum of nums. (+) returns 0." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/add)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([] 0) ([x] (cast Number x)) ([x y] (. clojure.lang.Numbers (add x y))) ([x y & more] (reduce + (+ x y) more)))
//---
(function __clojure_fn_3374(){
return (clojure.JS.def(clojure,"_PLUS_",clojure.JS.variadic(2,(function __clojure_fn_3374_PLUS_3379(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.add(x_1,y_2))
case 1:return (clojure.cast.apply(null,[java.lang.Number,x_1]))
case 0:return (0)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure._PLUS_,clojure.lang.Numbers.add(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn * "Returns the product of nums. (*) returns 1." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/multiply)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([] 1) ([x] (cast Number x)) ([x y] (. clojure.lang.Numbers (multiply x y))) ([x y & more] (reduce * (* x y) more)))
//---
(function __clojure_fn_3398(){
return (clojure.JS.def(clojure,"_STAR_",clojure.JS.variadic(2,(function __clojure_fn_3398_STAR_3403(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.multiply(x_1,y_2))
case 1:return (clojure.cast.apply(null,[java.lang.Number,x_1]))
case 0:return (1)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure._STAR_,clojure.lang.Numbers.multiply(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn / "If no denominators are supplied, returns 1/numerator,\n  else returns numerator divided by all of the denominators." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/divide)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] (/ 1 x)) ([x y] (. clojure.lang.Numbers (divide x y))) ([x y & more] (reduce / (/ x y) more)))
//---
(function __clojure_fn_3421(){
return (clojure.JS.def(clojure,"_SLASH_",clojure.JS.variadic(2,(function __clojure_fn_3421_SLASH_3426(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.divide(x_1,y_2))
case 1:return (clojure.lang.Numbers.divide(1,x_1))}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure._SLASH_,clojure.lang.Numbers.divide(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn - "If no ys are supplied, returns the negation of x, else subtracts\n  the ys from x and returns the result." {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/minus)) args)))), :inline-arities #{1 2}} ([x] (. clojure.lang.Numbers (minus x))) ([x y] (. clojure.lang.Numbers (minus x y))) ([x y & more] (reduce - (- x y) more)))
//---
(function __clojure_fn_3443(){
return (clojure.JS.def(clojure,"_",clojure.JS.variadic(2,(function __clojure_fn_3443_3448(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.minus(x_1,y_2))
case 1:return (clojure.lang.Numbers.minus(x_1))}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure._,clojure.lang.Numbers.minus(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn < "Returns non-nil if nums are in monotonically increasing order,\n  otherwise false." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/lt)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (lt x y))) ([x y & more] (if (< x y) (if (rest more) (recur y (first more) (rest more)) (< y (first more))) false)))
//---
(function __clojure_fn_3465(){
return (clojure.JS.def(clojure,"_LT_",clojure.JS.variadic(2,(function __clojure_fn_3465_LT_3470(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.lt(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.lt(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.lt(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn <= "Returns non-nil if nums are in monotonically non-decreasing order,\n  otherwise false." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/lte)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (lte x y))) ([x y & more] (if (<= x y) (if (rest more) (recur y (first more) (rest more)) (<= y (first more))) false)))
//---
(function __clojure_fn_3487(){
return (clojure.JS.def(clojure,"_LT__EQ_",clojure.JS.variadic(2,(function __clojure_fn_3487_LT_EQ_3492(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.lte(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.lte(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.lte(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn > "Returns non-nil if nums are in monotonically decreasing order,\n  otherwise false." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/gt)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (gt x y))) ([x y & more] (if (> x y) (if (rest more) (recur y (first more) (rest more)) (> y (first more))) false)))
//---
(function __clojure_fn_3509(){
return (clojure.JS.def(clojure,"_GT_",clojure.JS.variadic(2,(function __clojure_fn_3509_GT_3514(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.gt(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.gt(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.gt(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn >= "Returns non-nil if nums are in monotonically non-increasing order,\n  otherwise false." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/gte)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (gte x y))) ([x y & more] (if (>= x y) (if (rest more) (recur y (first more) (rest more)) (>= y (first more))) false)))
//---
(function __clojure_fn_3531(){
return (clojure.JS.def(clojure,"_GT__EQ_",clojure.JS.variadic(2,(function __clojure_fn_3531_GT_EQ_3536(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.gte(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.gte(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.gte(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn == "Returns non-nil if nums all have the same value, otherwise false" {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/equiv)) (clojure/list x) (clojure/list y))))), :inline-arities #{2}} ([x] true) ([x y] (. clojure.lang.Numbers (equiv x y))) ([x y & more] (if (== x y) (if (rest more) (recur y (first more) (rest more)) (== y (first more))) false)))
//---
(function __clojure_fn_3553(){
return (clojure.JS.def(clojure,"_EQ__EQ_",clojure.JS.variadic(2,(function __clojure_fn_3553_EQ_EQ_3558(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.equiv(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.equiv(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.equiv(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn max "Returns the greatest of the nums." ([x] x) ([x y] (if (> x y) x y)) ([x y & more] (reduce max (max x y) more)))
//---
(function __clojure_fn_3572(){
return (clojure.JS.def(clojure,"max",clojure.JS.variadic(2,(function __clojure_fn_3572_max_3574(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.lang.Numbers.gt(x_1,y_2))?(x_1):(y_2)))
case 1:return (x_1)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure.max,clojure.max.apply(null,[x_1,y_2]),more_3]))}))))}).apply(null,[]);

//======
//(defn min "Returns the least of the nums." ([x] x) ([x y] (if (< x y) x y)) ([x y & more] (reduce min (min x y) more)))
//---
(function __clojure_fn_3588(){
return (clojure.JS.def(clojure,"min",clojure.JS.variadic(2,(function __clojure_fn_3588_min_3590(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.lang.Numbers.lt(x_1,y_2))?(x_1):(y_2)))
case 1:return (x_1)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure.min,clojure.min.apply(null,[x_1,y_2]),more_3]))}))))}).apply(null,[]);

//======
//(defn inc "Returns a number one greater than num." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/inc)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (inc x)))
//---
(function __clojure_fn_3605(){
return (clojure.JS.def(clojure,"inc",(function __clojure_fn_3605_inc_3610(x_1){
return (clojure.lang.Numbers.inc(x_1))})))}).apply(null,[]);

//======
//(defn dec "Returns a number one less than num." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/dec)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (dec x)))
//---
(function __clojure_fn_3623(){
return (clojure.JS.def(clojure,"dec",(function __clojure_fn_3623_dec_3628(x_1){
return (clojure.lang.Numbers.dec(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-inc "Returns a number one greater than x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_inc)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (unchecked_inc x)))
//---
(function __clojure_fn_3641(){
return (clojure.JS.def(clojure,"unchecked_inc",(function __clojure_fn_3641_unchecked_inc_3646(x_1){
return (clojure.lang.Numbers.unchecked_inc(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-dec "Returns a number one less than x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_dec)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (unchecked_dec x)))
//---
(function __clojure_fn_3659(){
return (clojure.JS.def(clojure,"unchecked_dec",(function __clojure_fn_3659_unchecked_dec_3664(x_1){
return (clojure.lang.Numbers.unchecked_dec(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-negate "Returns the negation of x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_negate)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (unchecked_negate x)))
//---
(function __clojure_fn_3677(){
return (clojure.JS.def(clojure,"unchecked_negate",(function __clojure_fn_3677_unchecked_negate_3682(x_1){
return (clojure.lang.Numbers.unchecked_negate(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-add "Returns the sum of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_add)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_add x y)))
//---
(function __clojure_fn_3695(){
return (clojure.JS.def(clojure,"unchecked_add",(function __clojure_fn_3695_unchecked_add_3700(x_1,y_2){
return (clojure.lang.Numbers.unchecked_add(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-subtract "Returns the difference of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_subtract)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_subtract x y)))
//---
(function __clojure_fn_3713(){
return (clojure.JS.def(clojure,"unchecked_subtract",(function __clojure_fn_3713_unchecked_subtract_3718(x_1,y_2){
return (clojure.lang.Numbers.unchecked_subtract(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-multiply "Returns the product of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_multiply)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_multiply x y)))
//---
(function __clojure_fn_3731(){
return (clojure.JS.def(clojure,"unchecked_multiply",(function __clojure_fn_3731_unchecked_multiply_3736(x_1,y_2){
return (clojure.lang.Numbers.unchecked_multiply(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-divide "Returns the division of x by y, both int or long. \n  Note - uses a primitive operator subject to truncation." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_divide)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_divide x y)))
//---
(function __clojure_fn_3749(){
return (clojure.JS.def(clojure,"unchecked_divide",(function __clojure_fn_3749_unchecked_divide_3754(x_1,y_2){
return (clojure.lang.Numbers.unchecked_divide(x_1,y_2))})))}).apply(null,[]);

//======
//(defn pos? "Returns true if num is greater than zero, else false" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/isPos)) (clojure/list x))))), :tag Boolean} [x] (. clojure.lang.Numbers (isPos x)))
//---
(function __clojure_fn_3767(){
return (clojure.JS.def(clojure,"pos_QMARK_",(function __clojure_fn_3767_pos_QMARK_3772(x_1){
return (clojure.lang.Numbers.isPos(x_1))})))}).apply(null,[]);

//======
//(defn neg? "Returns true if num is less than zero, else false" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/isNeg)) (clojure/list x))))), :tag Boolean} [x] (. clojure.lang.Numbers (isNeg x)))
//---
(function __clojure_fn_3785(){
return (clojure.JS.def(clojure,"neg_QMARK_",(function __clojure_fn_3785_neg_QMARK_3790(x_1){
return (clojure.lang.Numbers.isNeg(x_1))})))}).apply(null,[]);

//======
//(defn zero? "Returns true if num is zero, else false" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/isZero)) (clojure/list x))))), :tag Boolean} [x] (. clojure.lang.Numbers (isZero x)))
//---
(function __clojure_fn_3803(){
return (clojure.JS.def(clojure,"zero_QMARK_",(function __clojure_fn_3803_zero_QMARK_3808(x_1){
return (clojure.lang.Numbers.isZero(x_1))})))}).apply(null,[]);

//======
//(defn quot "quot[ient] of dividing numerator by denominator." [num div] (. clojure.lang.Numbers (quotient num div)))
//---
(function __clojure_fn_3818(){
return (clojure.JS.def(clojure,"quot",(function __clojure_fn_3818_quot_3820(num_1,div_2){
return (clojure.lang.Numbers.quotient(num_1,div_2))})))}).apply(null,[]);

//======
//(defn rem "rem[ainder] of dividing numerator by denominator." [num div] (. clojure.lang.Numbers (remainder num div)))
//---
(function __clojure_fn_3830(){
return (clojure.JS.def(clojure,"rem",(function __clojure_fn_3830_rem_3832(num_1,div_2){
return (clojure.lang.Numbers.remainder(num_1,div_2))})))}).apply(null,[]);

//======
//(defn rationalize "returns the rational value of num" [num] (. clojure.lang.Numbers (rationalize num)))
//---
(function __clojure_fn_3842(){
return (clojure.JS.def(clojure,"rationalize",(function __clojure_fn_3842_rationalize_3844(num_1){
return (clojure.lang.Numbers.rationalize(num_1))})))}).apply(null,[]);

//======
//(defn bit-not "Bitwise complement" [x] (. clojure.lang.Numbers not x))
//---
(function __clojure_fn_3854(){
return (clojure.JS.def(clojure,"bit_not",(function __clojure_fn_3854_bit_not_3856(x_1){
return (clojure.lang.Numbers.not(x_1))})))}).apply(null,[]);

//======
//(defn bit-and "Bitwise and" [x y] (. clojure.lang.Numbers and x y))
//---
(function __clojure_fn_3866(){
return (clojure.JS.def(clojure,"bit_and",(function __clojure_fn_3866_bit_and_3868(x_1,y_2){
return (clojure.lang.Numbers.and(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-or "Bitwise or" [x y] (. clojure.lang.Numbers or x y))
//---
(function __clojure_fn_3878(){
return (clojure.JS.def(clojure,"bit_or",(function __clojure_fn_3878_bit_or_3880(x_1,y_2){
return (clojure.lang.Numbers.or(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-xor "Bitwise exclusive or" [x y] (. clojure.lang.Numbers xor x y))
//---
(function __clojure_fn_3890(){
return (clojure.JS.def(clojure,"bit_xor",(function __clojure_fn_3890_bit_xor_3892(x_1,y_2){
return (clojure.lang.Numbers.xor(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-and-not "Bitwise and with complement" [x y] (. clojure.lang.Numbers andNot x y))
//---
(function __clojure_fn_3902(){
return (clojure.JS.def(clojure,"bit_and_not",(function __clojure_fn_3902_bit_and_not_3904(x_1,y_2){
return (clojure.lang.Numbers.andNot(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-clear "Clear bit at index n" [x n] (. clojure.lang.Numbers clearBit x n))
//---
(function __clojure_fn_3914(){
return (clojure.JS.def(clojure,"bit_clear",(function __clojure_fn_3914_bit_clear_3916(x_1,n_2){
return (clojure.lang.Numbers.clearBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-set "Set bit at index n" [x n] (. clojure.lang.Numbers setBit x n))
//---
(function __clojure_fn_3926(){
return (clojure.JS.def(clojure,"bit_set",(function __clojure_fn_3926_bit_set_3928(x_1,n_2){
return (clojure.lang.Numbers.setBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-flip "Flip bit at index n" [x n] (. clojure.lang.Numbers flipBit x n))
//---
(function __clojure_fn_3938(){
return (clojure.JS.def(clojure,"bit_flip",(function __clojure_fn_3938_bit_flip_3940(x_1,n_2){
return (clojure.lang.Numbers.flipBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-test "Test bit at index n" [x n] (. clojure.lang.Numbers testBit x n))
//---
(function __clojure_fn_3950(){
return (clojure.JS.def(clojure,"bit_test",(function __clojure_fn_3950_bit_test_3952(x_1,n_2){
return (clojure.lang.Numbers.testBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-shift-left "Bitwise shift left" [x n] (. clojure.lang.Numbers shiftLeft x n))
//---
(function __clojure_fn_3962(){
return (clojure.JS.def(clojure,"bit_shift_left",(function __clojure_fn_3962_bit_shift_left_3964(x_1,n_2){
return (clojure.lang.Numbers.shiftLeft(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-shift-right "Bitwise shift right" [x n] (. clojure.lang.Numbers shiftRight x n))
//---
(function __clojure_fn_3974(){
return (clojure.JS.def(clojure,"bit_shift_right",(function __clojure_fn_3974_bit_shift_right_3976(x_1,n_2){
return (clojure.lang.Numbers.shiftRight(x_1,n_2))})))}).apply(null,[]);

//======
//(defn complement "Takes a fn f and returns a fn that takes the same arguments as f,\n  has the same effects, if any, and returns the opposite truth value." [f] (fn [& args] (not (apply f args))))
//---
(function __clojure_fn_3989(){
return (clojure.JS.def(clojure,"complement",(function __clojure_fn_3989_complement_3991(f_1){
return (clojure.JS.variadic(0,(function __clojure_fn_3989_complement_3991_fn_3993(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.not.apply(null,[clojure.apply.apply(null,[f_1,args_1])]))})))})))}).apply(null,[]);

//======
//(defn constantly "Returns a function that takes any number of arguments and returns x." [x] (fn [& args] x))
//---
(function __clojure_fn_4007(){
return (clojure.JS.def(clojure,"constantly",(function __clojure_fn_4007_constantly_4009(x_1){
return (clojure.JS.variadic(0,(function __clojure_fn_4007_constantly_4009_fn_4011(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (x_1)})))})))}).apply(null,[]);

//======
//(defn identity "Returns its argument." [x] x)
//---
(function __clojure_fn_4022(){
return (clojure.JS.def(clojure,"identity",(function __clojure_fn_4022_identity_4024(x_1){
return (x_1)})))}).apply(null,[]);

//======
//(defn peek "For a list or queue, same as first, for a vector, same as, but much\n  more efficient than, last. If the collection is empty, returns nil." [coll] (. clojure.lang.RT (peek coll)))
//---
(function __clojure_fn_4040(){
return (clojure.JS.def(clojure,"peek",(function __clojure_fn_4040_peek_4042(coll_1){
return (clojure.lang.RT.peek(coll_1))})))}).apply(null,[]);

//======
//(defn pop "For a list or queue, returns a new list/queue without the first\n  item, for a vector, returns a new vector without the last item. If\n  the collection is empty, throws an exception.  Note - not the same\n  as rest/butlast." [coll] (. clojure.lang.RT (pop coll)))
//---
(function __clojure_fn_4052(){
return (clojure.JS.def(clojure,"pop",(function __clojure_fn_4052_pop_4054(coll_1){
return (clojure.lang.RT.pop(coll_1))})))}).apply(null,[]);

//======
//(defn nth "Returns the value at the index. get returns nil if index out of\n  bounds, nth throws an exception unless not-found is supplied.  nth\n  also works for strings, Java arrays, regex Matchers and Lists, and,\n  in O(n) time, for sequences." ([coll index] (. clojure.lang.RT (nth coll index))) ([coll index not-found] (. clojure.lang.RT (nth coll index not-found))))
//---
(function __clojure_fn_4065(){
return (clojure.JS.def(clojure,"nth",(function __clojure_fn_4065_nth_4067(coll_1,index_2,not_found_3){switch(arguments.length){
case 2:return (clojure.lang.RT.nth(coll_1,index_2))}
return (clojure.lang.RT.nth(coll_1,index_2,not_found_3))})))}).apply(null,[]);

//======
//(defn dissoc "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,\n  that does not contain a mapping for key(s)." ([map] map) ([map key] (. clojure.lang.RT (dissoc map key))) ([map key & ks] (let [ret (dissoc map key)] (if ks (recur ret (first ks) (rest ks)) ret))))
//---
(function __clojure_fn_4093(){
return (clojure.JS.def(clojure,"dissoc",clojure.JS.variadic(2,(function __clojure_fn_4093_dissoc_4095(map_1,key_2){switch(arguments.length){
case 2:return (clojure.lang.RT.dissoc(map_1,key_2))
case 1:return (map_1)}
var _cnt,_rtn,ks_3,ret_4,ks_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((ret_4=clojure.dissoc.apply(null,[map_1,key_2])),
((ks_3)?((_cnt=1,_rtn=[ret_4,clojure.first.apply(null,[ks_3]),clojure.rest.apply(null,[ks_3])],map_1=_rtn[0],key_2=_rtn[1],ks_3=_rtn[2])):(ret_4)))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn disj "disj[oin]. Returns a new set of the same (hashed/sorted) type, that\n  does not contain key(s)." ([set] set) ([set key] (. set (disjoin key))) ([set key & ks] (let [ret (disj set key)] (if ks (recur ret (first ks) (rest ks)) ret))))
//---
(function __clojure_fn_4109(){
return (clojure.JS.def(clojure,"disj",clojure.JS.variadic(2,(function __clojure_fn_4109_disj_4111(set_1,key_2){switch(arguments.length){
case 2:return ((set_1).disjoin(key_2))
case 1:return (set_1)}
var _cnt,_rtn,ret_4,ks_3,ks_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((ret_4=clojure.disj.apply(null,[set_1,key_2])),
((ks_3)?((_cnt=1,_rtn=[ret_4,clojure.first.apply(null,[ks_3]),clojure.rest.apply(null,[ks_3])],set_1=_rtn[0],key_2=_rtn[1],ks_3=_rtn[2])):(ret_4)))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn select-keys "Returns a map containing only those entries in map whose key is in keys" [map keyseq] (loop [ret {} keys (seq keyseq)] (if keys (let [entry (. clojure.lang.RT (find map (first keys)))] (recur (if entry (conj ret entry) ret) (rest keys))) ret)))
//---
(function __clojure_fn_4129(){
return (clojure.JS.def(clojure,"select_keys",(function __clojure_fn_4129_select_keys_4131(map_1,keyseq_2){
var keys_4,ret_3,entry_5;
return (((function __loop(){var _rtn,_cnt;(ret_3=clojure.lang.PersistentHashMap.EMPTY),
(keys_4=clojure.seq.apply(null,[keyseq_2]));do{_cnt=0;
_rtn=((keys_4)?(((entry_5=clojure.lang.RT.find(map_1,clojure.first.apply(null,[keys_4]))),
(_cnt=1,_rtn=[((entry_5)?(clojure.conj.apply(null,[ret_3,entry_5])):(ret_3)),clojure.rest.apply(null,[keys_4])],ret_3=_rtn[0],keys_4=_rtn[1]))):(ret_3))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn key "Returns the key of the map entry." [e] (. e (getKey)))
//---
(function __clojure_fn_4153(){
return (clojure.JS.def(clojure,"key",(function __clojure_fn_4153_key_4155(e_1){
return ((e_1).getKey())})))}).apply(null,[]);

//======
//(defn val "Returns the value in the map entry." [e] (. e (getValue)))
//---
(function __clojure_fn_4165(){
return (clojure.JS.def(clojure,"val",(function __clojure_fn_4165_val_4167(e_1){
return ((e_1).getValue())})))}).apply(null,[]);

//======
//(defn rseq "Returns, in constant time, a sequence of the items in rev (which\n  can be a vector or sorted-map), in reverse order." [rev] (. rev (rseq)))
//---
(function __clojure_fn_4177(){
return (clojure.JS.def(clojure,"rseq",(function __clojure_fn_4177_rseq_4179(rev_1){
return ((rev_1).rseq())})))}).apply(null,[]);

//======
//(defn name "Returns the name String of a symbol or keyword." [x] (. x (getName)))
//---
(function __clojure_fn_4189(){
return (clojure.JS.def(clojure,"name",(function __clojure_fn_4189_name_4191(x_1){
return ((x_1).getName())})))}).apply(null,[]);

//======
//(defn namespace "Returns the namespace String of a symbol or keyword, or nil if not present." [x] (. x (getNamespace)))
//---
(function __clojure_fn_4201(){
return (clojure.JS.def(clojure,"namespace",(function __clojure_fn_4201_namespace_4203(x_1){
return ((x_1).getNamespace())})))}).apply(null,[]);

//======
//(defn find-var "Returns the global var named by the namespace-qualified symbol, or\n  nil if no var with that name." [sym] (. clojure.lang.Var (find sym)))
//---
(function __clojure_fn_4268(){
return (clojure.JS.def(clojure,"find_var",(function __clojure_fn_4268_find_var_4270(sym_1){
return (clojure.lang.Var.find(sym_1))})))}).apply(null,[]);

//======
//(defn agent "Creates and returns an agent with an initial value of state and an\n  optional validate fn. validate-fn must be nil or a side-effect-free fn of\n  one argument, which will be passed the intended new state on any state\n  change. If the new state is unacceptable, the validate-fn should\n  throw an exception." ([state] (new clojure.lang.Agent state)) ([state validate-fn] (new clojure.lang.Agent state validate-fn)))
//---
(function __clojure_fn_4281(){
return (clojure.JS.def(clojure,"agent",(function __clojure_fn_4281_agent_4283(state_1,validate_fn_2){switch(arguments.length){
case 1:return ((new clojure.lang.Agent(state_1)))}
return ((new clojure.lang.Agent(state_1,validate_fn_2)))})))}).apply(null,[]);

//======
//(defn ! [& args] (throw (new Exception "! is now send. See also send-off")))
//---
(function __clojure_fn_4294(){
return (clojure.JS.def(clojure,"_BANG_",clojure.JS.variadic(0,(function __clojure_fn_4294_BANG_4296(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw (new java.lang.Exception("! is now send. See also send-off"))})())}))))}).apply(null,[]);

//======
//(defn send "Dispatch an action to an agent. Returns the agent immediately.\n  Subsequently, in a thread from a thread pool, the state of the agent\n  will be set to the value of:\n\n  (apply action-fn state-of-agent args)" [a f & args] (. a (dispatch f args false)))
//---
(function __clojure_fn_4306(){
return (clojure.JS.def(clojure,"send",clojure.JS.variadic(2,(function __clojure_fn_4306_send_4308(a_1,f_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((a_1).dispatch(f_2,args_3,false))}))))}).apply(null,[]);

//======
//(defn send-off "Dispatch a potentially blocking action to an agent. Returns the\n  agent immediately. Subsequently, in a separate thread, the state of\n  the agent will be set to the value of:\n\n  (apply action-fn state-of-agent args)" [a f & args] (. a (dispatch f args true)))
//---
(function __clojure_fn_4318(){
return (clojure.JS.def(clojure,"send_off",clojure.JS.variadic(2,(function __clojure_fn_4318_send_off_4320(a_1,f_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((a_1).dispatch(f_2,args_3,true))}))))}).apply(null,[]);

//======
//(defn add-watch "Experimental.\n  Adds a watcher to an agent. Whenever the agent runs an action, any\n  registered watchers will have their callback function called.  The\n  callback fn will be passed 3 args, the watcher, the agent and a boolean\n  which will be true if the agent's state was (potentially) changed by\n  the action. The callback fn is run synchronously with the action,\n  and thus derefs of the agent in the callback will see the value set\n  during that action. Because it is run on the action thread, the\n  callback should not block, but can send messages." [a watcher callback] (.addWatch a watcher callback))
//---
(function __clojure_fn_4330(){
return (clojure.JS.def(clojure,"add_watch",(function __clojure_fn_4330_add_watch_4332(a_1,watcher_2,callback_3){
return ((a_1).addWatch(watcher_2,callback_3))})))}).apply(null,[]);

//======
//(defn remove-watch "Experimental.\n  Removes a watcher (set by add-watch) from an agent" [a watcher] (.removeWatch a watcher))
//---
(function __clojure_fn_4342(){
return (clojure.JS.def(clojure,"remove_watch",(function __clojure_fn_4342_remove_watch_4344(a_1,watcher_2){
return ((a_1).removeWatch(watcher_2))})))}).apply(null,[]);

//======
//(defn agent-errors "Returns a sequence of the exceptions thrown during asynchronous\n  actions of the agent." [a] (. a (getErrors)))
//---
(function __clojure_fn_4354(){
return (clojure.JS.def(clojure,"agent_errors",(function __clojure_fn_4354_agent_errors_4356(a_1){
return ((a_1).getErrors())})))}).apply(null,[]);

//======
//(defn clear-agent-errors "Clears any exceptions thrown during asynchronous actions of the\n  agent, allowing subsequent actions to occur." [a] (. a (clearErrors)))
//---
(function __clojure_fn_4366(){
return (clojure.JS.def(clojure,"clear_agent_errors",(function __clojure_fn_4366_clear_agent_errors_4368(a_1){
return ((a_1).clearErrors())})))}).apply(null,[]);

//======
//(defn shutdown-agents "Initiates a shutdown of the thread pools that back the agent\n  system. Running actions will complete, but no new actions will be\n  accepted" [] (. clojure.lang.Agent shutdown))
//---
(function __clojure_fn_4378(){
return (clojure.JS.def(clojure,"shutdown_agents",(function __clojure_fn_4378_shutdown_agents_4380(){
return (clojure.lang.Agent.shutdown())})))}).apply(null,[]);

//======
//(defn ref "Creates and returns a Ref with an initial value of x and an optional validate fn.\n  validate-fn must be nil or a side-effect-free fn of one argument, which will\n  be passed the intended new state on any state change. If the new\n  state is unacceptable, the validate-fn should throw an\n  exception. validate-fn will be called on transaction commit, when\n  all refs have their final values." ([x] (new clojure.lang.Ref x)) ([x validate-fn] (new clojure.lang.Ref x validate-fn)))
//---
(function __clojure_fn_4391(){
return (clojure.JS.def(clojure,"ref",(function __clojure_fn_4391_ref_4393(x_1,validate_fn_2){switch(arguments.length){
case 1:return ((new clojure.lang.Ref(x_1)))}
return ((new clojure.lang.Ref(x_1,validate_fn_2)))})))}).apply(null,[]);

//======
//(defn deref "Also reader macro: @ref/@agent Within a transaction, returns the\n  in-transaction-value of ref, else returns the\n  most-recently-committed value of ref. When applied to an agent,\n  returns its current state." [ref] (. ref (get)))
//---
(function __clojure_fn_4404(){
return (clojure.JS.def(clojure,"deref",(function __clojure_fn_4404_deref_4406(ref_1){
return ((ref_1).get())})))}).apply(null,[]);

//======
//(defn set-validator "Sets the validator-fn for a var/ref/agent. validator-fn must be nil or a\n  side-effect-free fn of one argument, which will be passed the intended\n  new state on any state change. If the new state is unacceptable, the\n  validator-fn should throw an exception. If the current state (root\n  value if var) is not acceptable to the new validator, an exception\n  will be thrown and the validator will not be changed." [iref validator-fn] (. iref (setValidator validator-fn)))
//---
(function __clojure_fn_4416(){
return (clojure.JS.def(clojure,"set_validator",(function __clojure_fn_4416_set_validator_4418(iref_1,validator_fn_2){
return ((iref_1).setValidator(validator_fn_2))})))}).apply(null,[]);

//======
//(defn get-validator "Gets the validator-fn for a var/ref/agent." [iref] (. iref (getValidator)))
//---
(function __clojure_fn_4428(){
return (clojure.JS.def(clojure,"get_validator",(function __clojure_fn_4428_get_validator_4430(iref_1){
return ((iref_1).getValidator())})))}).apply(null,[]);

//======
//(defn commute "Must be called in a transaction. Sets the in-transaction-value of\n  ref to:\n\n  (apply fun in-transaction-value-of-ref args)\n\n  and returns the in-transaction-value of ref.\n\n  At the commit point of the transaction, sets the value of ref to be:\n\n  (apply fun most-recently-committed-value-of-ref args)\n\n  Thus fun should be commutative, or, failing that, you must accept\n  last-one-in-wins behavior.  commute allows for more concurrency than\n  ref-set." [ref fun & args] (. ref (commute fun args)))
//---
(function __clojure_fn_4440(){
return (clojure.JS.def(clojure,"commute",clojure.JS.variadic(2,(function __clojure_fn_4440_commute_4442(ref_1,fun_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((ref_1).commute(fun_2,args_3))}))))}).apply(null,[]);

//======
//(defn alter "Must be called in a transaction. Sets the in-transaction-value of\n  ref to:\n\n  (apply fun in-transaction-value-of-ref args)\n\n  and returns the in-transaction-value of ref." [ref fun & args] (. ref (alter fun args)))
//---
(function __clojure_fn_4452(){
return (clojure.JS.def(clojure,"alter",clojure.JS.variadic(2,(function __clojure_fn_4452_alter_4454(ref_1,fun_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((ref_1).alter(fun_2,args_3))}))))}).apply(null,[]);

//======
//(defn ref-set "Must be called in a transaction. Sets the value of ref.\n  Returns val." [ref val] (. ref (set val)))
//---
(function __clojure_fn_4464(){
return (clojure.JS.def(clojure,"ref_set",(function __clojure_fn_4464_ref_set_4466(ref_1,val_2){
return ((ref_1).set(val_2))})))}).apply(null,[]);

//======
//(defn ensure "Must be called in a transaction. Protects the ref from modification\n  by other transactions.  Returns the in-transaction-value of\n  ref. Allows for more concurrency than (ref-set ref @ref)" [ref] (. ref (touch)) (. ref (get)))
//---
(function __clojure_fn_4476(){
return (clojure.JS.def(clojure,"ensure",(function __clojure_fn_4476_ensure_4478(ref_1){
return ((ref_1).touch(),
(ref_1).get())})))}).apply(null,[]);

//======
//(defn comp "Takes a set of functions and returns a fn that is the composition\n  of those fns.  The returned fn takes a variable number of args,\n  applies the rightmost of fns to the args, the next\n  fn (right-to-left) to the result, etc." [& fs] (let [fs (reverse fs)] (fn [& args] (loop [ret (apply (first fs) args) fs (rest fs)] (if fs (recur ((first fs) ret) (rest fs)) ret)))))
//---
(function __clojure_fn_4497(){
return (clojure.JS.def(clojure,"comp",clojure.JS.variadic(0,(function __clojure_fn_4497_comp_4499(){
var fs_1,fs_2,fs_1=clojure.JS.rest_args(this,arguments,0);
return (((fs_2=clojure.reverse.apply(null,[fs_1])),
clojure.JS.variadic(0,(function __clojure_fn_4497_comp_4499_fn_4501(){
var args_1,ret_2,fs_3,args_1=clojure.JS.rest_args(this,arguments,0);
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.apply.apply(null,[clojure.first.apply(null,[fs_2]),args_1])),
(fs_3=clojure.rest.apply(null,[fs_2]));do{_cnt=0;
_rtn=((fs_3)?((_cnt=1,_rtn=[clojure.first.apply(null,[fs_3]).apply(null,[ret_2]),clojure.rest.apply(null,[fs_3])],ret_2=_rtn[0],fs_3=_rtn[1])):(ret_2))}while(_cnt);return _rtn;})()))}))))}))))}).apply(null,[]);

//======
//(defn partial "Takes a function f and fewer than the normal arguments to f, and\n  returns a fn that takes a variable number of additional args. When\n  called, the returned function calls f with args + additional args." ([f arg1] (fn [& args] (apply f arg1 args))) ([f arg1 arg2] (fn [& args] (apply f arg1 arg2 args))) ([f arg1 arg2 arg3] (fn [& args] (apply f arg1 arg2 arg3 args))) ([f arg1 arg2 arg3 & more] (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))
//---
(function __clojure_fn_4527(){
return (clojure.JS.def(clojure,"partial",clojure.JS.variadic(4,(function __clojure_fn_4527_partial_4529(f_1,arg1_2,arg2_3,arg3_4){switch(arguments.length){
case 4:return (clojure.JS.variadic(0,(function __clojure_fn_4527_partial_4529_fn_4539(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[f_1,arg1_2,arg2_3,arg3_4,args_1]))})))
case 3:return (clojure.JS.variadic(0,(function __clojure_fn_4527_partial_4529_fn_4535(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[f_1,arg1_2,arg2_3,args_1]))})))
case 2:return (clojure.JS.variadic(0,(function __clojure_fn_4527_partial_4529_fn_4531(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[f_1,arg1_2,args_1]))})))}
var more_5,more_5=clojure.JS.rest_args(this,arguments,4);
return (clojure.JS.variadic(0,(function __clojure_fn_4527_partial_4529_fn_4543(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[f_1,arg1_2,arg2_3,arg3_4,clojure.concat.apply(null,[more_5,args_1])]))})))}))))}).apply(null,[]);

//======
//(defn every? "Returns true if (pred x) is logical true for every x in coll, else\n  false." {:tag Boolean} [pred coll] (if (seq coll) (and (pred (first coll)) (recur pred (rest coll))) true))
//---
(function __clojure_fn_4554(){
return (clojure.JS.def(clojure,"every_QMARK_",(function __clojure_fn_4554_every_QMARK_4556(pred_1,coll_2){
var _cnt,_rtn,and__196_3;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[coll_2]))?(((and__196_3=pred_1.apply(null,[clojure.first.apply(null,[coll_2])])),
((and__196_3)?((_cnt=1,_rtn=[pred_1,clojure.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])):(and__196_3)))):(true))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def not-every? (comp not every?))
//---
(function __clojure_fn_4563(){
return (clojure.JS.def(clojure,"not_every_QMARK_",clojure.comp.apply(null,[clojure.not,clojure.every_QMARK_])))}).apply(null,[]);

//======
//(defn some "Returns the first logical true value of (pred x) for any x in coll,\n  else nil." [pred coll] (when (seq coll) (or (pred (first coll)) (recur pred (rest coll)))))
//---
(function __clojure_fn_4572(){
return (clojure.JS.def(clojure,"some",(function __clojure_fn_4572_some_4574(pred_1,coll_2){
var _cnt,_rtn,or__202_3;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[coll_2]))?(((or__202_3=pred_1.apply(null,[clojure.first.apply(null,[coll_2])])),
((or__202_3)?(or__202_3):((_cnt=1,_rtn=[pred_1,clojure.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1]))))):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def not-any? (comp not some))
//---
(function __clojure_fn_4581(){
return (clojure.JS.def(clojure,"not_any_QMARK_",clojure.comp.apply(null,[clojure.not,clojure.some])))}).apply(null,[]);

//======
//(defn map "Returns a lazy seq consisting of the result of applying f to the\n  set of first items of each coll, followed by applying f to the set\n  of second items in each coll, until any one of the colls is\n  exhausted.  Any remaining items in other colls are ignored. Function\n  f should accept number-of-colls arguments." ([f coll] (when (seq coll) (lazy-cons (f (first coll)) (map f (rest coll))))) ([f c1 c2] (when (and (seq c1) (seq c2)) (lazy-cons (f (first c1) (first c2)) (map f (rest c1) (rest c2))))) ([f c1 c2 c3] (when (and (seq c1) (seq c2) (seq c3)) (lazy-cons (f (first c1) (first c2) (first c3)) (map f (rest c1) (rest c2) (rest c3))))) ([f c1 c2 c3 & colls] (let [step (fn step [cs] (when (every? seq cs) (lazy-cons (map first cs) (step (map rest cs)))))] (map (fn* [p1__4584] (apply f p1__4584)) (step (conj colls c3 c2 c1))))))
//---
(function __clojure_fn_4619(){
return (clojure.JS.def(clojure,"map",clojure.JS.variadic(4,(function __clojure_fn_4619_map_4621(f_1,c1_2,c2_3,c3_4){switch(arguments.length){
case 4:var and__196_5,and__196_6;
return (((((and__196_5=clojure.seq.apply(null,[c1_2])),
((and__196_5)?(((and__196_6=clojure.seq.apply(null,[c2_3])),
((and__196_6)?(clojure.seq.apply(null,[c3_4])):(and__196_6)))):(and__196_5))))?((new clojure.lang.LazyCons((function __clojure_fn_4619_map_4621_fn_4636(G__4635_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.first.apply(null,[c1_2]),clojure.first.apply(null,[c2_3]),clojure.first.apply(null,[c3_4])]))}
return (clojure.map.apply(null,[f_1,clojure.rest.apply(null,[c1_2]),clojure.rest.apply(null,[c2_3]),clojure.rest.apply(null,[c3_4])]))})))):(null)))
case 3:var and__196_4;
return (((((and__196_4=clojure.seq.apply(null,[c1_2])),
((and__196_4)?(clojure.seq.apply(null,[c2_3])):(and__196_4))))?((new clojure.lang.LazyCons((function __clojure_fn_4619_map_4621_fn_4630(G__4629_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.first.apply(null,[c1_2]),clojure.first.apply(null,[c2_3])]))}
return (clojure.map.apply(null,[f_1,clojure.rest.apply(null,[c1_2]),clojure.rest.apply(null,[c2_3])]))})))):(null)))
case 2:var coll_2=arguments[1];
return (((clojure.seq.apply(null,[coll_2]))?((new clojure.lang.LazyCons((function __clojure_fn_4619_map_4621_fn_4624(G__4623_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.first.apply(null,[coll_2])]))}
return (clojure.map.apply(null,[f_1,clojure.rest.apply(null,[coll_2])]))})))):(null)))}
var step_6,colls_5,colls_5=clojure.JS.rest_args(this,arguments,4);
return (((step_6=(function __clojure_fn_4619_map_4621_step_4641(cs_1){
var step_0=arguments.callee;
return (((clojure.every_QMARK_.apply(null,[clojure.seq,cs_1]))?((new clojure.lang.LazyCons((function __clojure_fn_4619_map_4621_step_4641_fn_4643(G__4642_1){switch(arguments.length){
case 0:return (clojure.map.apply(null,[clojure.first,cs_1]))}
return (step_0.apply(null,[clojure.map.apply(null,[clojure.rest,cs_1])]))})))):(null)))})),
clojure.map.apply(null,[(function __clojure_fn_4619_map_4621_fn_4648(p1__4584_1){
return (clojure.apply.apply(null,[f_1,p1__4584_1]))}),step_6.apply(null,[clojure.conj.apply(null,[colls_5,c3_4,c2_3,c1_2])])])))}))))}).apply(null,[]);

//======
//(defn mapcat "Returns the result of applying concat to the result of applying map\n  to f and colls.  Thus function f should return a collection." [f & colls] (apply concat (apply map f colls)))
//---
(function __clojure_fn_4659(){
return (clojure.JS.def(clojure,"mapcat",clojure.JS.variadic(1,(function __clojure_fn_4659_mapcat_4661(f_1){
var colls_2,colls_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.apply.apply(null,[clojure.concat,clojure.apply.apply(null,[clojure.map,f_1,colls_2])]))}))))}).apply(null,[]);

//======
//(defn filter "Returns a lazy seq of the items in coll for which\n  (pred item) returns true. pred must be free of side-effects." [pred coll] (when (seq coll) (if (pred (first coll)) (lazy-cons (first coll) (filter pred (rest coll))) (recur pred (rest coll)))))
//---
(function __clojure_fn_4676(){
return (clojure.JS.def(clojure,"filter",(function __clojure_fn_4676_filter_4678(pred_1,coll_2){
var _cnt,_rtn;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[coll_2]))?(((pred_1.apply(null,[clojure.first.apply(null,[coll_2])]))?((new clojure.lang.LazyCons((function __clojure_fn_4676_filter_4678_fn_4681(G__4680_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll_2]))}
return (clojure.filter.apply(null,[pred_1,clojure.rest.apply(null,[coll_2])]))})))):((_cnt=1,_rtn=[pred_1,clojure.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])))):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn take "Returns a lazy seq of the first n items in coll, or all items if\n  there are fewer than n." [n coll] (when (and (pos? n) (seq coll)) (lazy-cons (first coll) (take (dec n) (rest coll)))))
//---
(function __clojure_fn_4698(){
return (clojure.JS.def(clojure,"take",(function __clojure_fn_4698_take_4700(n_1,coll_2){
var and__196_3;
return (((((and__196_3=clojure.lang.Numbers.isPos(n_1)),
((and__196_3)?(clojure.seq.apply(null,[coll_2])):(and__196_3))))?((new clojure.lang.LazyCons((function __clojure_fn_4698_take_4700_fn_4703(G__4702_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll_2]))}
return (clojure.take.apply(null,[clojure.lang.Numbers.dec(n_1),clojure.rest.apply(null,[coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn take-while "Returns a lazy seq of successive items from coll while\n  (pred item) returns true. pred must be free of side-effects." [pred coll] (when (and (seq coll) (pred (first coll))) (lazy-cons (first coll) (take-while pred (rest coll)))))
//---
(function __clojure_fn_4720(){
return (clojure.JS.def(clojure,"take_while",(function __clojure_fn_4720_take_while_4722(pred_1,coll_2){
var and__196_3;
return (((((and__196_3=clojure.seq.apply(null,[coll_2])),
((and__196_3)?(pred_1.apply(null,[clojure.first.apply(null,[coll_2])])):(and__196_3))))?((new clojure.lang.LazyCons((function __clojure_fn_4720_take_while_4722_fn_4725(G__4724_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll_2]))}
return (clojure.take_while.apply(null,[pred_1,clojure.rest.apply(null,[coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn drop "Returns a lazy seq of all but the first n items in coll." [n coll] (if (and (pos? n) (seq coll)) (recur (dec n) (rest coll)) (seq coll)))
//---
(function __clojure_fn_4737(){
return (clojure.JS.def(clojure,"drop",(function __clojure_fn_4737_drop_4739(n_1,coll_2){
var _cnt,_rtn,and__196_3;
do{_cnt=0;_rtn=((((and__196_3=clojure.lang.Numbers.isPos(n_1)),
((and__196_3)?(clojure.seq.apply(null,[coll_2])):(and__196_3))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_1),clojure.rest.apply(null,[coll_2])],n_1=_rtn[0],coll_2=_rtn[1])):(clojure.seq.apply(null,[coll_2])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn drop-last "Return a lazy seq of all but the last n (default 1) items in coll" ([s] (drop-last 1 s)) ([n s] (map (fn [x _] x) (seq s) (drop n s))))
//---
(function __clojure_fn_4753(){
return (clojure.JS.def(clojure,"drop_last",(function __clojure_fn_4753_drop_last_4755(n_1,s_2){switch(arguments.length){
case 1:var s_1=arguments[0];
return (clojure.drop_last.apply(null,[1,s_1]))}
return (clojure.map.apply(null,[(function __clojure_fn_4753_drop_last_4755_fn_4758(x_1,__2){
return (x_1)}),clojure.seq.apply(null,[s_2]),clojure.drop.apply(null,[n_1,s_2])]))})))}).apply(null,[]);

//======
//(defn drop-while "Returns a lazy seq of the items in coll starting from the first\n  item for which (pred item) returns nil." [pred coll] (if (and (seq coll) (pred (first coll))) (recur pred (rest coll)) (seq coll)))
//---
(function __clojure_fn_4769(){
return (clojure.JS.def(clojure,"drop_while",(function __clojure_fn_4769_drop_while_4771(pred_1,coll_2){
var _cnt,_rtn,and__196_3;
do{_cnt=0;_rtn=((((and__196_3=clojure.seq.apply(null,[coll_2])),
((and__196_3)?(pred_1.apply(null,[clojure.first.apply(null,[coll_2])])):(and__196_3))))?((_cnt=1,_rtn=[pred_1,clojure.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])):(clojure.seq.apply(null,[coll_2])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn cycle "Returns a lazy (infinite!) seq of repetitions of the items in\n  coll." [coll] (when (seq coll) (let [rep (fn thisfn [xs] (if xs (lazy-cons (first xs) (thisfn (rest xs))) (recur (seq coll))))] (rep (seq coll)))))
//---
(function __clojure_fn_4788(){
return (clojure.JS.def(clojure,"cycle",(function __clojure_fn_4788_cycle_4790(coll_1){
var rep_2;
return (((clojure.seq.apply(null,[coll_1]))?(((rep_2=(function __clojure_fn_4788_cycle_4790_thisfn_4792(xs_1){
var _cnt,_rtn,thisfn_0=arguments.callee;
do{_cnt=0;_rtn=((xs_1)?((new clojure.lang.LazyCons((function __clojure_fn_4788_cycle_4790_thisfn_4792_fn_4794(G__4793_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[xs_1]))}
return (thisfn_0.apply(null,[clojure.rest.apply(null,[xs_1])]))})))):((_cnt=1,_rtn=[clojure.seq.apply(null,[coll_1])],xs_1=_rtn[0])))
}while(_cnt);return _rtn;})),
rep_2.apply(null,[clojure.seq.apply(null,[coll_1])]))):(null)))})))}).apply(null,[]);

//======
//(defn split-at "Returns a vector of [(take n coll) (drop n coll)]" [n coll] [(take n coll) (drop n coll)])
//---
(function __clojure_fn_4807(){
return (clojure.JS.def(clojure,"split_at",(function __clojure_fn_4807_split_at_4809(n_1,coll_2){
return (clojure.JS.lit_vector([clojure.take.apply(null,[n_1,coll_2]),clojure.drop.apply(null,[n_1,coll_2])]))})))}).apply(null,[]);

//======
//(defn split-with "Returns a vector of [(take-while pred coll) (drop-while pred coll)]" [pred coll] [(take-while pred coll) (drop-while pred coll)])
//---
(function __clojure_fn_4819(){
return (clojure.JS.def(clojure,"split_with",(function __clojure_fn_4819_split_with_4821(pred_1,coll_2){
return (clojure.JS.lit_vector([clojure.take_while.apply(null,[pred_1,coll_2]),clojure.drop_while.apply(null,[pred_1,coll_2])]))})))}).apply(null,[]);

//======
//(defn repeat "Returns a lazy (infinite!) seq of xs." [x] (lazy-cons x (repeat x)))
//---
(function __clojure_fn_4836(){
return (clojure.JS.def(clojure,"repeat",(function __clojure_fn_4836_repeat_4838(x_1){
return ((new clojure.lang.LazyCons((function __clojure_fn_4836_repeat_4838_fn_4841(G__4840_1){switch(arguments.length){
case 0:return (x_1)}
return (clojure.repeat.apply(null,[x_1]))}))))})))}).apply(null,[]);

//======
//(defn replicate "Returns a lazy seq of n xs." [n x] (take n (repeat x)))
//---
(function __clojure_fn_4853(){
return (clojure.JS.def(clojure,"replicate",(function __clojure_fn_4853_replicate_4855(n_1,x_2){
return (clojure.take.apply(null,[n_1,clojure.repeat.apply(null,[x_2])]))})))}).apply(null,[]);

//======
//(defn iterate "Returns a lazy seq of x, (f x), (f (f x)) etc. f must be free of side-effects" [f x] (lazy-cons x (iterate f (f x))))
//---
(function __clojure_fn_4870(){
return (clojure.JS.def(clojure,"iterate",(function __clojure_fn_4870_iterate_4872(f_1,x_2){
return ((new clojure.lang.LazyCons((function __clojure_fn_4870_iterate_4872_fn_4875(G__4874_1){switch(arguments.length){
case 0:return (x_2)}
return (clojure.iterate.apply(null,[f_1,f_1.apply(null,[x_2])]))}))))})))}).apply(null,[]);

//======
//(defn range "Returns a lazy seq of nums from start (inclusive) to end\n  (exclusive), by step, where start defaults to 0 and step to 1." ([end] (if (and (> end 0) (< end (. Integer MAX_VALUE))) (new clojure.lang.Range 0 end) (take end (iterate inc 0)))) ([start end] (if (and (< start end) (< end (. Integer MAX_VALUE))) (new clojure.lang.Range start end) (take (- end start) (iterate inc start)))) ([start end step] (take-while (partial (if (pos? step) > <) end) (iterate (partial + step) start))))
//---
(function __clojure_fn_4889(){
return (clojure.JS.def(clojure,"range",(function __clojure_fn_4889_range_4891(start_1,end_2,step_3){switch(arguments.length){
case 2:var and__196_3;
return (((((and__196_3=clojure.lang.Numbers.lt(start_1,end_2)),
((and__196_3)?(clojure.lang.Numbers.lt(end_2,java.lang.Integer.MAX_VALUE)):(and__196_3))))?((new clojure.lang.Range(start_1,end_2))):(clojure.take.apply(null,[clojure.lang.Numbers.minus(end_2,start_1),clojure.iterate.apply(null,[clojure.inc,start_1])]))))
case 1:var and__196_2,end_1=arguments[0];
return (((((and__196_2=clojure.lang.Numbers.gt(end_1,0)),
((and__196_2)?(clojure.lang.Numbers.lt(end_1,java.lang.Integer.MAX_VALUE)):(and__196_2))))?((new clojure.lang.Range(0,end_1))):(clojure.take.apply(null,[end_1,clojure.iterate.apply(null,[clojure.inc,0])]))))}
return (clojure.take_while.apply(null,[clojure.partial.apply(null,[((clojure.lang.Numbers.isPos(step_3))?(clojure._GT_):(clojure._LT_)),end_2]),clojure.iterate.apply(null,[clojure.partial.apply(null,[clojure._PLUS_,step_3]),start_1])]))})))}).apply(null,[]);

//======
//(defn merge "Returns a map that consists of the rest of the maps conj-ed onto\n  the first.  If a key occurs in more than one map, the mapping from\n  the latter (left-to-right) will be the mapping in the result." [& maps] (reduce conj maps))
//---
(function __clojure_fn_4903(){
return (clojure.JS.def(clojure,"merge",clojure.JS.variadic(0,(function __clojure_fn_4903_merge_4905(){
var maps_1,maps_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.reduce.apply(null,[clojure.conj,maps_1]))}))))}).apply(null,[]);

//======
//(defn merge-with "Returns a map that consists of the rest of the maps conj-ed onto\n  the first.  If a key occurs in more than one map, the mapping(s)\n  from the latter (left-to-right) will be combined with the mapping in\n  the result by calling (f val-in-result val-in-latter)." [f & maps] (let [merge-entry (fn [m e] (let [k (key e) v (val e)] (if (contains? m k) (assoc m k (f (m k) v)) (assoc m k v)))) merge2 (fn [m1 m2] (reduce merge-entry m1 (seq m2)))] (reduce merge2 maps)))
//---
(function __clojure_fn_4921(){
return (clojure.JS.def(clojure,"merge_with",clojure.JS.variadic(1,(function __clojure_fn_4921_merge_with_4923(f_1){
var maps_2,merge_entry_3,merge2_4,maps_2=clojure.JS.rest_args(this,arguments,1);
return (((merge_entry_3=(function __clojure_fn_4921_merge_with_4923_merge_entry_4925(m_1,e_2){
var k_3,v_4;
return (((k_3=clojure.key.apply(null,[e_2])),
(v_4=clojure.val.apply(null,[e_2])),
((clojure.contains_QMARK_.apply(null,[m_1,k_3]))?(clojure.assoc.apply(null,[m_1,k_3,f_1.apply(null,[m_1.apply(null,[k_3]),v_4])])):(clojure.assoc.apply(null,[m_1,k_3,v_4])))))})),
(merge2_4=(function __clojure_fn_4921_merge_with_4923_merge2_4928(m1_1,m2_2){
return (clojure.reduce.apply(null,[merge_entry_3,m1_1,clojure.seq.apply(null,[m2_2])]))})),
clojure.reduce.apply(null,[merge2_4,maps_2])))}))))}).apply(null,[]);

//======
//(defn zipmap "Returns a map with the keys mapped to the corresponding vals." [keys vals] (loop [map {} ks (seq keys) vs (seq vals)] (if (and ks vs) (recur (assoc map (first ks) (first vs)) (rest ks) (rest vs)) map)))
//---
(function __clojure_fn_4939(){
return (clojure.JS.def(clojure,"zipmap",(function __clojure_fn_4939_zipmap_4941(keys_1,vals_2){
var ks_4,map_3,and__196_6,vs_5;
return (((function __loop(){var _rtn,_cnt;(map_3=clojure.lang.PersistentHashMap.EMPTY),
(ks_4=clojure.seq.apply(null,[keys_1])),
(vs_5=clojure.seq.apply(null,[vals_2]));do{_cnt=0;
_rtn=((((and__196_6=ks_4),
((and__196_6)?(vs_5):(and__196_6))))?((_cnt=1,_rtn=[clojure.assoc.apply(null,[map_3,clojure.first.apply(null,[ks_4]),clojure.first.apply(null,[vs_5])]),clojure.rest.apply(null,[ks_4]),clojure.rest.apply(null,[vs_5])],map_3=_rtn[0],ks_4=_rtn[1],vs_5=_rtn[2])):(map_3))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn line-seq "Returns the lines of text from rdr as a lazy sequence of strings.\n  rdr must implement java.io.BufferedReader." [rdr] (let [line (. rdr (readLine))] (when line (lazy-cons line (line-seq rdr)))))
//---
(function __clojure_fn_4956(){
return (clojure.JS.def(clojure,"line_seq",(function __clojure_fn_4956_line_seq_4958(rdr_1){
var line_2;
return (((line_2=(rdr_1).readLine()),
((line_2)?((new clojure.lang.LazyCons((function __clojure_fn_4956_line_seq_4958_fn_4961(G__4960_1){switch(arguments.length){
case 0:return (line_2)}
return (clojure.line_seq.apply(null,[rdr_1]))})))):(null))))})))}).apply(null,[]);

//======
//(defn comparator "Returns an implementation of java.util.Comparator based upon pred." [pred] (fn [x y] (cond (pred x y) -1 (pred y x) 1 :else 0)))
//---
(function __clojure_fn_4976(){
return (clojure.JS.def(clojure,"comparator",(function __clojure_fn_4976_comparator_4978(pred_1){
return ((function __clojure_fn_4976_comparator_4978_fn_4980(x_1,y_2){
return (((pred_1.apply(null,[x_1,y_2]))?(-1):(((pred_1.apply(null,[y_2,x_1]))?(1):(((clojure.keyword("","else"))?(0):(null)))))))}))})))}).apply(null,[]);

//======
//(defn sort "Returns a sorted sequence of the items in coll. If no comparator is\n  supplied, uses compare. comparator must\n  implement java.util.Comparator." ([coll] (sort compare coll)) ([comp coll] (when (and coll (not (. coll (isEmpty)))) (let [a (. coll (toArray))] (. java.util.Arrays (sort a comp)) (seq a)))))
//---
(function __clojure_fn_4992(){
return (clojure.JS.def(clojure,"sort",(function __clojure_fn_4992_sort_4994(comp_1,coll_2){switch(arguments.length){
case 1:var coll_1=arguments[0];
return (clojure.sort.apply(null,[clojure.compare,coll_1]))}
var a_3,and__196_3;
return (((((and__196_3=coll_2),
((and__196_3)?(clojure.not.apply(null,[(coll_2).isEmpty()])):(and__196_3))))?(((a_3=(coll_2).toArray()),
java.util.Arrays.sort(a_3,comp_1),
clojure.seq.apply(null,[a_3]))):(null)))})))}).apply(null,[]);

//======
//(defn sort-by "Returns a sorted sequence of the items in coll, where the sort\n  order is determined by comparing (keyfn item).  If no comparator is\n  supplied, uses compare. comparator must\n  implement java.util.Comparator." ([keyfn coll] (sort-by keyfn compare coll)) ([keyfn comp coll] (sort (fn [x y] (. comp (compare (keyfn x) (keyfn y)))) coll)))
//---
(function __clojure_fn_5009(){
return (clojure.JS.def(clojure,"sort_by",(function __clojure_fn_5009_sort_by_5011(keyfn_1,comp_2,coll_3){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (clojure.sort_by.apply(null,[keyfn_1,clojure.compare,coll_2]))}
return (clojure.sort.apply(null,[(function __clojure_fn_5009_sort_by_5011_fn_5014(x_1,y_2){
return ((comp_2).compare(keyfn_1.apply(null,[x_1]),keyfn_1.apply(null,[y_2])))}),coll_3]))})))}).apply(null,[]);

//======
//(defn eval "Evaluates the form data structure (not text!) and returns the result." [form] (. clojure.lang.Compiler (eval form)))
//---
(function __clojure_fn_5025(){
return (clojure.JS.def(clojure,"eval",(function __clojure_fn_5025_eval_5027(form_1){
return (clojure.lang.Compiler.eval(form_1))})))}).apply(null,[]);

//======
//(defn scan [& args] (throw (new Exception "scan is now called dorun")))
//---
(function __clojure_fn_5044(){
return (clojure.JS.def(clojure,"scan",clojure.JS.variadic(0,(function __clojure_fn_5044_scan_5046(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw (new java.lang.Exception("scan is now called dorun"))})())}))))}).apply(null,[]);

//======
//(defn touch [& args] (throw (new Exception "touch is now called doall")))
//---
(function __clojure_fn_5056(){
return (clojure.JS.def(clojure,"touch",clojure.JS.variadic(0,(function __clojure_fn_5056_touch_5058(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw (new java.lang.Exception("touch is now called doall"))})())}))))}).apply(null,[]);

//======
//(defn dorun "When lazy sequences are produced via functions that have side\n  effects, any effects other than those needed to produce the first\n  element in the seq do not occur until the seq is consumed. dorun can\n  be used to force any effects. Walks through the successive rests of\n  the seq, does not retain the head and returns nil." ([coll] (when (and (seq coll) (or (first coll) true)) (recur (rest coll)))) ([n coll] (when (and (seq coll) (pos? n) (or (first coll) true)) (recur (dec n) (rest coll)))))
//---
(function __clojure_fn_5069(){
return (clojure.JS.def(clojure,"dorun",(function __clojure_fn_5069_dorun_5071(n_1,coll_2){switch(arguments.length){
case 1:var _cnt,_rtn,and__196_2,or__202_3,coll_1=arguments[0];
do{_cnt=0;_rtn=((((and__196_2=clojure.seq.apply(null,[coll_1])),
((and__196_2)?(((or__202_3=clojure.first.apply(null,[coll_1])),
((or__202_3)?(or__202_3):(true)))):(and__196_2))))?((_cnt=1,_rtn=[clojure.rest.apply(null,[coll_1])],coll_1=_rtn[0])):(null))
}while(_cnt);return _rtn;}
var _cnt,_rtn,and__196_3,or__202_5,and__196_4;
do{_cnt=0;_rtn=((((and__196_3=clojure.seq.apply(null,[coll_2])),
((and__196_3)?(((and__196_4=clojure.lang.Numbers.isPos(n_1)),
((and__196_4)?(((or__202_5=clojure.first.apply(null,[coll_2])),
((or__202_5)?(or__202_5):(true)))):(and__196_4)))):(and__196_3))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_1),clojure.rest.apply(null,[coll_2])],n_1=_rtn[0],coll_2=_rtn[1])):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn doall "When lazy sequences are produced via functions that have side\n  effects, any effects other than those needed to produce the first\n  element in the seq do not occur until the seq is consumed. doall can\n  be used to force any effects. Walks through the successive rests of\n  the seq, retains the head and returns it, thus causing the entire\n  seq to reside in memory at one time." ([coll] (dorun coll) coll) ([n coll] (dorun n coll) coll))
//---
(function __clojure_fn_5083(){
return (clojure.JS.def(clojure,"doall",(function __clojure_fn_5083_doall_5085(n_1,coll_2){switch(arguments.length){
case 1:var coll_1=arguments[0];
return (clojure.dorun.apply(null,[coll_1]),
coll_1)}
return (clojure.dorun.apply(null,[n_1,coll_2]),
coll_2)})))}).apply(null,[]);

//======
//(defn await "Blocks the current thread (indefinitely!) until all actions\n  dispatched thus far, from this thread or agent, to the agent(s) have\n  occurred." [& agents] (when *agent* (throw (new Exception "Can't await in agent action"))) (let [latch (new java.util.concurrent.CountDownLatch (count agents)) count-down (fn [agent] (. latch (countDown)) agent)] (doseq agent agents (send agent count-down)) (. latch (await))))
//---
(function __clojure_fn_5099(){
return (clojure.JS.def(clojure,"await",clojure.JS.variadic(0,(function __clojure_fn_5099_await_5101(){
var list__763_4,count_down_3,latch_2,agents_1,agent_5,agents_1=clojure.JS.rest_args(this,arguments,0);
return (((clojure._STAR_agent_STAR_)?((function __throw(){throw (new java.lang.Exception("Can't await in agent action"))})()):(null)),
((latch_2=(new java.util.concurrent.CountDownLatch(clojure.count.apply(null,[agents_1])))),
(count_down_3=(function __clojure_fn_5099_await_5101_count_down_5103(agent_1){
return ((latch_2).countDown(),
agent_1)})),
((function __loop(){var _rtn,_cnt;(list__763_4=clojure.seq.apply(null,[agents_1]));do{_cnt=0;
_rtn=((list__763_4)?(((agent_5=clojure.first.apply(null,[list__763_4])),
clojure.send.apply(null,[agent_5,count_down_3])),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__763_4])],list__763_4=_rtn[0])):(null))}while(_cnt);return _rtn;})()),
(latch_2).await()))}))))}).apply(null,[]);

//======
//(defn await1 [a] (when (pos? (.getQueueCount a)) (await a)) a)
//---
(function __clojure_fn_5114(){
return (clojure.JS.def(clojure,"await1",(function __clojure_fn_5114_await1_5116(a_1){
return (((clojure.lang.Numbers.isPos((a_1).getQueueCount()))?(clojure.await.apply(null,[a_1])):(null)),
a_1)})))}).apply(null,[]);

//======
//(defn await-for "Blocks the current thread until all actions dispatched thus\n  far (from this thread or agent) to the agents have occurred, or the\n  timeout (in milliseconds) has elapsed. Returns nil if returning due\n  to timeout, non-nil otherwise." [timeout-ms & agents] (when *agent* (throw (new Exception "Can't await in agent action"))) (let [latch (new java.util.concurrent.CountDownLatch (count agents)) count-down (fn [agent] (. latch (countDown)) agent)] (doseq agent agents (send agent count-down)) (. latch (await timeout-ms (. java.util.concurrent.TimeUnit MILLISECONDS)))))
//---
(function __clojure_fn_5129(){
return (clojure.JS.def(clojure,"await_for",clojure.JS.variadic(1,(function __clojure_fn_5129_await_for_5131(timeout_ms_1){
var list__763_5,agent_6,agents_2,count_down_4,latch_3,agents_2=clojure.JS.rest_args(this,arguments,1);
return (((clojure._STAR_agent_STAR_)?((function __throw(){throw (new java.lang.Exception("Can't await in agent action"))})()):(null)),
((latch_3=(new java.util.concurrent.CountDownLatch(clojure.count.apply(null,[agents_2])))),
(count_down_4=(function __clojure_fn_5129_await_for_5131_count_down_5133(agent_1){
return ((latch_3).countDown(),
agent_1)})),
((function __loop(){var _rtn,_cnt;(list__763_5=clojure.seq.apply(null,[agents_2]));do{_cnt=0;
_rtn=((list__763_5)?(((agent_6=clojure.first.apply(null,[list__763_5])),
clojure.send.apply(null,[agent_6,count_down_4])),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__763_5])],list__763_5=_rtn[0])):(null))}while(_cnt);return _rtn;})()),
(latch_3).await(timeout_ms_1,java.util.concurrent.TimeUnit.MILLISECONDS)))}))))}).apply(null,[]);

//======
//(defn into-array "Returns an array of the type of the first element in coll,\n  containing the contents of coll, which must be of a compatible\n  type." [aseq] (. clojure.lang.RT (seqToTypedArray (seq aseq))))
//---
(function __clojure_fn_5157(){
return (clojure.JS.def(clojure,"into_array",(function __clojure_fn_5157_into_array_5159(aseq_1){
return (clojure.lang.RT.seqToTypedArray(clojure.seq.apply(null,[aseq_1])))})))}).apply(null,[]);

//======
//(defn into "Returns a new coll consisting of to-coll with all of the items of\n  from-coll conjoined." [to from] (let [ret to items (seq from)] (if items (recur (conj ret (first items)) (rest items)) ret)))
//---
(function __clojure_fn_5169(){
return (clojure.JS.def(clojure,"into",(function __clojure_fn_5169_into_5171(to_1,from_2){
var _cnt,_rtn,items_4,ret_3;
do{_cnt=0;_rtn=((ret_3=to_1),
(items_4=clojure.seq.apply(null,[from_2])),
((items_4)?((_cnt=1,_rtn=[clojure.conj.apply(null,[ret_3,clojure.first.apply(null,[items_4])]),clojure.rest.apply(null,[items_4])],to_1=_rtn[0],from_2=_rtn[1])):(ret_3)))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn array [& items] (into-array items))
//---
(function __clojure_fn_5181(){
return (clojure.JS.def(clojure,"array",clojure.JS.variadic(0,(function __clojure_fn_5181_array_5183(){
var items_1,items_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.into_array.apply(null,[items_1]))}))))}).apply(null,[]);

//======
//(defn pr "Prints the object(s) to the output stream that is the current value\n  of *out*.  Prints the object(s), separated by spaces if there is\n  more than one.  By default, pr and prn print in a way that objects\n  can be read by the reader" ([] nil) ([x] (print-method x *out*) nil) ([x & more] (pr x) (. *out* (append \space)) (apply pr more)))
//---
(function __clojure_fn_5207(){
return (clojure.JS.def(clojure,"pr",clojure.JS.variadic(1,(function __clojure_fn_5207_pr_5209(x_1){switch(arguments.length){
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
(function __clojure_fn_5221(){
return (clojure.JS.def(clojure,"newline",(function __clojure_fn_5221_newline_5223(){
return ((clojure._STAR_out_STAR_).append("\n"),
null)})))}).apply(null,[]);

//======
//(defn flush "Flushes the output stream that is the current value of\n  *out*" [] (. *out* (flush)) nil)
//---
(function __clojure_fn_5233(){
return (clojure.JS.def(clojure,"flush",(function __clojure_fn_5233_flush_5235(){
return ((clojure._STAR_out_STAR_).flush(),
null)})))}).apply(null,[]);

//======
//(defn prn "Same as pr followed by (newline). Observes *flush-on-newline*" [& more] (apply pr more) (newline) (when *flush-on-newline* (flush)))
//---
(function __clojure_fn_5245(){
return (clojure.JS.def(clojure,"prn",clojure.JS.variadic(0,(function __clojure_fn_5245_prn_5247(){
var more_1,more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[clojure.pr,more_1]),
clojure.newline.apply(null,[]),
((clojure._STAR_flush_on_newline_STAR_)?(clojure.flush.apply(null,[])):(null)))}))))}).apply(null,[]);

//======
//(defn print "Prints the object(s) to the output stream that is the current value\n  of *out*.  print and println produce output for human consumption." [& more] (binding [*print-readably* nil] (apply pr more)))
//---
(function __clojure_fn_5257(){
return (clojure.JS.def(clojure,"print",clojure.JS.variadic(0,(function __clojure_fn_5257_print_5259(){
var more_1,more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_print_readably_STAR_,null])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.pr,more_1]))}
finally{clojure.lang.Var.popThreadBindings()}})())}))))}).apply(null,[]);

//======
//(defn println "Same as print followed by (newline)" [& more] (binding [*print-readably* nil] (apply prn more)))
//---
(function __clojure_fn_5269(){
return (clojure.JS.def(clojure,"println",clojure.JS.variadic(0,(function __clojure_fn_5269_println_5271(){
var more_1,more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_print_readably_STAR_,null])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.prn,more_1]))}
finally{clojure.lang.Var.popThreadBindings()}})())}))))}).apply(null,[]);

//======
//(defn read "Reads the next object from stream, which must be an instance of\n  java.io.PushbackReader or some derivee.  stream defaults to the\n  current value of *in* ." ([] (read *in*)) ([stream] (read stream true nil)) ([stream eof-error? eof-value] (read stream eof-error? eof-value false)) ([stream eof-error? eof-value recursive?] (. clojure.lang.LispReader (read stream eof-error? eof-value recursive?))))
//---
(function __clojure_fn_5284(){
return (clojure.JS.def(clojure,"read",(function __clojure_fn_5284_read_5286(stream_1,eof_error_QMARK__2,eof_value_3,recursive_QMARK__4){switch(arguments.length){
case 3:return (clojure.read.apply(null,[stream_1,eof_error_QMARK__2,eof_value_3,false]))
case 1:return (clojure.read.apply(null,[stream_1,true,null]))
case 0:return (clojure.read.apply(null,[clojure._STAR_in_STAR_]))}
return (clojure.lang.LispReader.read(stream_1,eof_error_QMARK__2,eof_value_3,recursive_QMARK__4))})))}).apply(null,[]);

//======
//(defn read-line "Reads the next line from stream that is the current value of *in* ." [] (. *in* (readLine)))
//---
(function __clojure_fn_5299(){
return (clojure.JS.def(clojure,"read_line",(function __clojure_fn_5299_read_line_5301(){
return ((clojure._STAR_in_STAR_).readLine())})))}).apply(null,[]);

//======
//(defn num "Coerce to Number" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/num)) (clojure/list x))))), :tag Number} [x] (. clojure.lang.Numbers (num x)))
//---
(function __clojure_fn_5344(){
return (clojure.JS.def(clojure,"num",(function __clojure_fn_5344_num_5349(x_1){
return (clojure.lang.Numbers.num(x_1))})))}).apply(null,[]);

//======
//(defn int "Coerce to int" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/intCast)) (clojure/list x))))), :tag Integer} [x] (. clojure.lang.RT (intCast x)))
//---
(function __clojure_fn_5362(){
return (clojure.JS.def(clojure,"int",(function __clojure_fn_5362_int_5367(x_1){
return (clojure.lang.RT.intCast(x_1))})))}).apply(null,[]);

//======
//(defn long "Coerce to long" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/longCast)) (clojure/list x))))), :tag Long} [x] (. x (longValue)))
//---
(function __clojure_fn_5380(){
return (clojure.JS.def(clojure,"long",(function __clojure_fn_5380_long_5385(x_1){
return ((x_1).longValue())})))}).apply(null,[]);

//======
//(defn float "Coerce to float" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/floatCast)) (clojure/list x))))), :tag Float} [x] (. x (floatValue)))
//---
(function __clojure_fn_5398(){
return (clojure.JS.def(clojure,"float",(function __clojure_fn_5398_float_5403(x_1){
return ((x_1).floatValue())})))}).apply(null,[]);

//======
//(defn double "Coerce to double" {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/doubleCast)) (clojure/list x))))), :tag Double} [x] (. x (doubleValue)))
//---
(function __clojure_fn_5416(){
return (clojure.JS.def(clojure,"double",(function __clojure_fn_5416_double_5421(x_1){
return ((x_1).doubleValue())})))}).apply(null,[]);

//======
//(defn short "Coerce to short" {:tag Short} [x] (. x (shortValue)))
//---
(function __clojure_fn_5431(){
return (clojure.JS.def(clojure,"short_",(function __clojure_fn_5431_short_5433(x_1){
return ((x_1).shortValue())})))}).apply(null,[]);

//======
//(defn byte "Coerce to byte" {:tag Byte} [x] (. x (byteValue)))
//---
(function __clojure_fn_5443(){
return (clojure.JS.def(clojure,"byte_",(function __clojure_fn_5443_byte_5445(x_1){
return ((x_1).byteValue())})))}).apply(null,[]);

//======
//(defn char "Coerce to char" {:tag Character} [x] (. clojure.lang.RT (charCast x)))
//---
(function __clojure_fn_5455(){
return (clojure.JS.def(clojure,"char_",(function __clojure_fn_5455_char_5457(x_1){
return (clojure.lang.RT.charCast(x_1))})))}).apply(null,[]);

//======
//(defn boolean "Coerce to boolean" {:tag Boolean} [x] (if x true false))
//---
(function __clojure_fn_5467(){
return (clojure.JS.def(clojure,"boolean_",(function __clojure_fn_5467_boolean_5469(x_1){
return (((x_1)?(true):(false)))})))}).apply(null,[]);

//======
//(defn bigint "Coerce to BigInteger" {:tag BigInteger} [x] (. BigInteger valueOf x))
//---
(function __clojure_fn_5479(){
return (clojure.JS.def(clojure,"bigint",(function __clojure_fn_5479_bigint_5481(x_1){
return (java.math.BigInteger.valueOf(x_1))})))}).apply(null,[]);

//======
//(defn bigdec "Coerce to BigDecimal" {:tag BigDecimal} [x] (. BigDecimal valueOf x))
//---
(function __clojure_fn_5491(){
return (clojure.JS.def(clojure,"bigdec",(function __clojure_fn_5491_bigdec_5493(x_1){
return (java.math.BigDecimal.valueOf(x_1))})))}).apply(null,[]);

//======
//(import (quote (java.lang.reflect Array)))
//---
(function __clojure_fn_5500(){
return (clojure.import_.apply(null,[clojure.JS.lit_list(["'java.lang.reflect","'Array"])]))}).apply(null,[]);

//======
//(defn alength "Returns the length of the Java array. Works on arrays of all\n  types." {:inline (fn [a] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/alength)) (clojure/list a)))))} [array] (. clojure.lang.RT (alength array)))
//---
(function __clojure_fn_5512(){
return (clojure.JS.def(clojure,"alength",(function __clojure_fn_5512_alength_5517(array_1){
return (clojure.lang.RT.alength(array_1))})))}).apply(null,[]);

//======
//(defn aclone "Returns a clone of the Java array. Works on arrays of known\n  types." {:inline (fn [a] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/aclone)) (clojure/list a)))))} [array] (. clojure.lang.RT (aclone array)))
//---
(function __clojure_fn_5530(){
return (clojure.JS.def(clojure,"aclone",(function __clojure_fn_5530_aclone_5535(array_1){
return (clojure.lang.RT.aclone(array_1))})))}).apply(null,[]);

//======
//(defn aget "Returns the value at the index/indices. Works on Java arrays of all\n  types." {:inline (fn [a i] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/aget)) (clojure/list a) (clojure/list i))))), :inline-arities #{2}} ([array idx] (. Array (get array idx))) ([array idx & idxs] (apply aget (aget array idx) idxs)))
//---
(function __clojure_fn_5549(){
return (clojure.JS.def(clojure,"aget",clojure.JS.variadic(2,(function __clojure_fn_5549_aget_5554(array_1,idx_2){switch(arguments.length){
case 2:return (java.lang.reflect.Array.get(array_1,idx_2))}
var idxs_3,idxs_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.apply.apply(null,[clojure.aget,clojure.lang.RT.aget(array_1,idx_2),idxs_3]))}))))}).apply(null,[]);

//======
//(defn aset "Sets the value at the index/indices. Works on Java arrays of\n  reference types. Returns val." {:inline (fn [a i v] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/aset)) (clojure/list a) (clojure/list i) (clojure/list v))))), :inline-arities #{3}} ([array idx val] (. Array (set array idx val)) val) ([array idx idx2 & idxv] (apply aset (aget array idx) idx2 idxv)))
//---
(function __clojure_fn_5569(){
return (clojure.JS.def(clojure,"aset",clojure.JS.variadic(3,(function __clojure_fn_5569_aset_5574(array_1,idx_2,idx2_3){switch(arguments.length){
case 3:var val_3=arguments[2];
return (java.lang.reflect.Array.set(array_1,idx_2,val_3),
val_3)}
var idxv_4,idxv_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply.apply(null,[clojure.aset,clojure.lang.RT.aget(array_1,idx_2),idx2_3,idxv_4]))}))))}).apply(null,[]);

//======
//(def-aset aset-int setInt int)
//---
(function __clojure_fn_5597(){
return (clojure.JS.def(clojure,"aset_int",clojure.JS.variadic(3,(function __clojure_fn_5597_aset_int_5599(array__942_1,idx__943_2,idx2__945_3){switch(arguments.length){
case 3:var val__944_3=arguments[2];
return (java.lang.reflect.Array.setInt(array__942_1,idx__943_2,clojure.lang.RT.intCast(val__944_3)),
val__944_3)}
var idxv__946_4,idxv__946_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply.apply(null,[clojure.aset_int,clojure.lang.RT.aget(array__942_1,idx__943_2),idx2__945_3,idxv__946_4]))}))))}).apply(null,[]);

//======
//(def-aset aset-long setLong long)
//---
(function __clojure_fn_5611(){
return (clojure.JS.def(clojure,"aset_long",clojure.JS.variadic(3,(function __clojure_fn_5611_aset_long_5613(array__942_1,idx__943_2,idx2__945_3){switch(arguments.length){
case 3:var val__944_3=arguments[2];
return (java.lang.reflect.Array.setLong(array__942_1,idx__943_2,clojure.lang.RT.longCast(val__944_3)),
val__944_3)}
var idxv__946_4,idxv__946_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply.apply(null,[clojure.aset_long,clojure.lang.RT.aget(array__942_1,idx__943_2),idx2__945_3,idxv__946_4]))}))))}).apply(null,[]);

//======
//(def-aset aset-boolean setBoolean boolean)
//---
(function __clojure_fn_5625(){
return (clojure.JS.def(clojure,"aset_boolean",clojure.JS.variadic(3,(function __clojure_fn_5625_aset_boolean_5627(array__942_1,idx__943_2,idx2__945_3){switch(arguments.length){
case 3:var val__944_3=arguments[2];
return (java.lang.reflect.Array.setBoolean(array__942_1,idx__943_2,clojure.boolean_.apply(null,[val__944_3])),
val__944_3)}
var idxv__946_4,idxv__946_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply.apply(null,[clojure.aset_boolean,clojure.lang.RT.aget(array__942_1,idx__943_2),idx2__945_3,idxv__946_4]))}))))}).apply(null,[]);

//======
//(def-aset aset-float setFloat float)
//---
(function __clojure_fn_5639(){
return (clojure.JS.def(clojure,"aset_float",clojure.JS.variadic(3,(function __clojure_fn_5639_aset_float_5641(array__942_1,idx__943_2,idx2__945_3){switch(arguments.length){
case 3:var val__944_3=arguments[2];
return (java.lang.reflect.Array.setFloat(array__942_1,idx__943_2,clojure.lang.RT.floatCast(val__944_3)),
val__944_3)}
var idxv__946_4,idxv__946_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply.apply(null,[clojure.aset_float,clojure.lang.RT.aget(array__942_1,idx__943_2),idx2__945_3,idxv__946_4]))}))))}).apply(null,[]);

//======
//(def-aset aset-double setDouble double)
//---
(function __clojure_fn_5653(){
return (clojure.JS.def(clojure,"aset_double",clojure.JS.variadic(3,(function __clojure_fn_5653_aset_double_5655(array__942_1,idx__943_2,idx2__945_3){switch(arguments.length){
case 3:var val__944_3=arguments[2];
return (java.lang.reflect.Array.setDouble(array__942_1,idx__943_2,clojure.lang.RT.doubleCast(val__944_3)),
val__944_3)}
var idxv__946_4,idxv__946_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply.apply(null,[clojure.aset_double,clojure.lang.RT.aget(array__942_1,idx__943_2),idx2__945_3,idxv__946_4]))}))))}).apply(null,[]);

//======
//(def-aset aset-short setShort short)
//---
(function __clojure_fn_5667(){
return (clojure.JS.def(clojure,"aset_short",clojure.JS.variadic(3,(function __clojure_fn_5667_aset_short_5669(array__942_1,idx__943_2,idx2__945_3){switch(arguments.length){
case 3:var val__944_3=arguments[2];
return (java.lang.reflect.Array.setShort(array__942_1,idx__943_2,clojure.short_.apply(null,[val__944_3])),
val__944_3)}
var idxv__946_4,idxv__946_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply.apply(null,[clojure.aset_short,clojure.lang.RT.aget(array__942_1,idx__943_2),idx2__945_3,idxv__946_4]))}))))}).apply(null,[]);

//======
//(def-aset aset-byte setByte byte)
//---
(function __clojure_fn_5681(){
return (clojure.JS.def(clojure,"aset_byte",clojure.JS.variadic(3,(function __clojure_fn_5681_aset_byte_5683(array__942_1,idx__943_2,idx2__945_3){switch(arguments.length){
case 3:var val__944_3=arguments[2];
return (java.lang.reflect.Array.setByte(array__942_1,idx__943_2,clojure.byte_.apply(null,[val__944_3])),
val__944_3)}
var idxv__946_4,idxv__946_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply.apply(null,[clojure.aset_byte,clojure.lang.RT.aget(array__942_1,idx__943_2),idx2__945_3,idxv__946_4]))}))))}).apply(null,[]);

//======
//(def-aset aset-char setChar char)
//---
(function __clojure_fn_5695(){
return (clojure.JS.def(clojure,"aset_char",clojure.JS.variadic(3,(function __clojure_fn_5695_aset_char_5697(array__942_1,idx__943_2,idx2__945_3){switch(arguments.length){
case 3:var val__944_3=arguments[2];
return (java.lang.reflect.Array.setChar(array__942_1,idx__943_2,clojure.char_.apply(null,[val__944_3])),
val__944_3)}
var idxv__946_4,idxv__946_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply.apply(null,[clojure.aset_char,clojure.lang.RT.aget(array__942_1,idx__943_2),idx2__945_3,idxv__946_4]))}))))}).apply(null,[]);

//======
//(defn make-array "Creates and returns an array of instances of the specified class of\n  the specified dimension(s).  Note that a class object is required.\n  Class objects can be obtained by using their imported or\n  fully-qualified name.  Class objects for the primitive types can be\n  obtained using, e.g., (. Integer TYPE)." ([type len] (. Array (newInstance type (int len)))) ([type dim & more-dims] (let [dims (cons dim more-dims) dimarray (make-array (. Integer TYPE) (count dims))] (dotimes i (alength dimarray) (aset-int dimarray i (nth dims i))) (. Array (newInstance type dimarray)))))
//---
(function __clojure_fn_5709(){
return (clojure.JS.def(clojure,"make_array",clojure.JS.variadic(2,(function __clojure_fn_5709_make_array_5711(type_1,dim_2){switch(arguments.length){
case 2:var len_2=arguments[1];
return (java.lang.reflect.Array.newInstance(type_1,clojure.lang.RT.intCast(len_2)))}
var n__796_6,dims_4,dimarray_5,more_dims_3,i_7,more_dims_3=clojure.JS.rest_args(this,arguments,2);
return (((dims_4=clojure.cons.apply(null,[dim_2,more_dims_3])),
(dimarray_5=clojure.make_array.apply(null,[java.lang.Integer.TYPE,clojure.count.apply(null,[dims_4])])),
((n__796_6=clojure.lang.RT.intCast(clojure.lang.RT.alength(dimarray_5))),
((function __loop(){var _rtn,_cnt;(i_7=clojure.lang.RT.intCast(0));do{_cnt=0;
_rtn=((clojure.lang.Numbers.lt(i_7,n__796_6))?(clojure.aset_int.apply(null,[dimarray_5,i_7,clojure.nth.apply(null,[dims_4,i_7])]),
(_cnt=1,_rtn=[clojure.lang.Numbers.unchecked_inc(i_7)],i_7=_rtn[0])):(null))}while(_cnt);return _rtn;})())),
java.lang.reflect.Array.newInstance(type_1,dimarray_5)))}))))}).apply(null,[]);

//======
//(defn to-array-2d "Returns a (potentially-ragged) 2-dimensional array of Objects\n  containing the contents of coll, which can be any Collection of any\n  Collection." [coll] (let [ret (make-array (. Class (forName "[Ljava.lang.Object;")) (. coll (size)))] (loop [i 0 xs (seq coll)] (when xs (aset ret i (to-array (first xs))) (recur (inc i) (rest xs)))) ret))
//---
(function __clojure_fn_5722(){
return (clojure.JS.def(clojure,"to_array_2d",(function __clojure_fn_5722_to_array_2d_5724(coll_1){
var i_3,xs_4,ret_2;
return (((ret_2=clojure.make_array.apply(null,[java.lang.Class.forName("[Ljava.lang.Object;"),(coll_1).size()])),
((function __loop(){var _rtn,_cnt;(i_3=0),
(xs_4=clojure.seq.apply(null,[coll_1]));do{_cnt=0;
_rtn=((xs_4)?(clojure.lang.RT.aset(ret_2,i_3,clojure.to_array.apply(null,[clojure.first.apply(null,[xs_4])])),
(_cnt=1,_rtn=[clojure.lang.Numbers.inc(i_3),clojure.rest.apply(null,[xs_4])],i_3=_rtn[0],xs_4=_rtn[1])):(null))}while(_cnt);return _rtn;})()),
ret_2))})))}).apply(null,[]);

//======
//(defn macroexpand-1 "If form represents a macro form, returns its expansion,\n  else returns form." [form] (. clojure.lang.Compiler (macroexpand1 form)))
//---
(function __clojure_fn_5734(){
return (clojure.JS.def(clojure,"macroexpand_1",(function __clojure_fn_5734_macroexpand_1_5736(form_1){
return (clojure.lang.Compiler.macroexpand1(form_1))})))}).apply(null,[]);

//======
//(defn macroexpand "Repeatedly calls macroexpand-1 on form until it no longer\n  represents a macro form, then returns it.  Note neither\n  macroexpand-1 nor macroexpand expand macros in subforms." [form] (let [ex (macroexpand-1 form)] (if (identical? ex form) form (macroexpand ex))))
//---
(function __clojure_fn_5746(){
return (clojure.JS.def(clojure,"macroexpand",(function __clojure_fn_5746_macroexpand_5748(form_1){
var ex_2;
return (((ex_2=clojure.macroexpand_1.apply(null,[form_1])),
((clojure.identical_QMARK_.apply(null,[ex_2,form_1]))?(form_1):(clojure.macroexpand.apply(null,[ex_2])))))})))}).apply(null,[]);

//======
//(defn create-struct "Returns a structure basis object." [& keys] (. clojure.lang.PersistentStructMap (createSlotMap keys)))
//---
(function __clojure_fn_5758(){
return (clojure.JS.def(clojure,"create_struct",clojure.JS.variadic(0,(function __clojure_fn_5758_create_struct_5760(){
var keys_1,keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentStructMap.createSlotMap(keys_1))}))))}).apply(null,[]);

//======
//(defn struct-map "Returns a new structmap instance with the keys of the\n  structure-basis. keyvals may contain all, some or none of the basis\n  keys - where values are not supplied they will default to nil.\n  keyvals can also contain keys not in the basis." [s & inits] (. clojure.lang.PersistentStructMap (create s inits)))
//---
(function __clojure_fn_5776(){
return (clojure.JS.def(clojure,"struct_map",clojure.JS.variadic(1,(function __clojure_fn_5776_struct_map_5778(s_1){
var inits_2,inits_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentStructMap.create(s_1,inits_2))}))))}).apply(null,[]);

//======
//(defn struct "Returns a new structmap instance with the keys of the\n  structure-basis. vals must be supplied for basis keys in order -\n  where values are not supplied they will default to nil." [s & vals] (. clojure.lang.PersistentStructMap (construct s vals)))
//---
(function __clojure_fn_5788(){
return (clojure.JS.def(clojure,"struct",clojure.JS.variadic(1,(function __clojure_fn_5788_struct_5790(s_1){
var vals_2,vals_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentStructMap.construct(s_1,vals_2))}))))}).apply(null,[]);

//======
//(defn accessor "Returns a fn that, given an instance of a structmap with the basis,\n  returns the value at the key.  The key must be in the basis. The\n  returned function should be (slightly) more efficient than using\n  get, but such use of accessors should be limited to known\n  performance-critical areas." [s key] (. clojure.lang.PersistentStructMap (getAccessor s key)))
//---
(function __clojure_fn_5800(){
return (clojure.JS.def(clojure,"accessor",(function __clojure_fn_5800_accessor_5802(s_1,key_2){
return (clojure.lang.PersistentStructMap.getAccessor(s_1,key_2))})))}).apply(null,[]);

//======
//(defn subvec "Returns a persistent vector of the items in vector from\n  start (inclusive) to end (exclusive).  If end is not supplied,\n  defaults to (count vector). This operation is O(1) and very fast, as\n  the resulting vector shares structure with the original and no\n  trimming is done." ([v start] (subvec v start (count v))) ([v start end] (. clojure.lang.RT (subvec v start end))))
//---
(function __clojure_fn_5813(){
return (clojure.JS.def(clojure,"subvec",(function __clojure_fn_5813_subvec_5815(v_1,start_2,end_3){switch(arguments.length){
case 2:return (clojure.subvec.apply(null,[v_1,start_2,clojure.count.apply(null,[v_1])]))}
return (clojure.lang.RT.subvec(v_1,start_2,end_3))})))}).apply(null,[]);

//======
//(defn load-reader "Sequentially read and evaluate the set of forms contained in the\n  stream/file" [rdr] (. clojure.lang.Compiler (load rdr)))
//---
(function __clojure_fn_5826(){
return (clojure.JS.def(clojure,"load_reader",(function __clojure_fn_5826_load_reader_5828(rdr_1){
return (clojure.lang.Compiler.load(rdr_1))})))}).apply(null,[]);

//======
//(defn load-string "Sequentially read and evaluate the set of forms contained in the\n  string" [s] (let [rdr (-> (java.io.StringReader. s) (clojure.lang.LineNumberingPushbackReader.))] (load-reader rdr)))
//---
(function __clojure_fn_5838(){
return (clojure.JS.def(clojure,"load_string",(function __clojure_fn_5838_load_string_5840(s_1){
var rdr_2;
return (((rdr_2=(new clojure.lang.LineNumberingPushbackReader((new java.io.StringReader(s_1))))),
clojure.load_reader.apply(null,[rdr_2])))})))}).apply(null,[]);

//======
//(defn resultset-seq "Creates and returns a lazy sequence of structmaps corresponding to\n  the rows in the java.sql.ResultSet rs" [rs] (let [rsmeta (. rs (getMetaData)) idxs (range 1 (inc (. rsmeta (getColumnCount)))) keys (map (comp keyword (memfn toLowerCase)) (map (fn [i] (. rsmeta (getColumnName i))) idxs)) row-struct (apply create-struct keys) row-values (fn [] (map (fn [i] (. rs (getObject i))) idxs)) rows (fn thisfn [] (when (. rs (next)) (lazy-cons (apply struct row-struct (row-values)) (thisfn))))] (rows)))
//---
(function __clojure_fn_5869(){
return (clojure.JS.def(clojure,"resultset_seq",(function __clojure_fn_5869_resultset_seq_5871(rs_1){
var row_struct_5,rsmeta_2,row_values_6,keys_4,idxs_3,rows_7;
return (((rsmeta_2=(rs_1).getMetaData()),
(idxs_3=clojure.range.apply(null,[1,clojure.lang.Numbers.inc((rsmeta_2).getColumnCount())])),
(keys_4=clojure.map.apply(null,[clojure.comp.apply(null,[clojure.keyword,(function __clojure_fn_5869_resultset_seq_5871_fn_5873(target__856_1){
return ((target__856_1).toLowerCase())})]),clojure.map.apply(null,[(function __clojure_fn_5869_resultset_seq_5871_fn_5876(i_1){
return ((rsmeta_2).getColumnName(i_1))}),idxs_3])])),
(row_struct_5=clojure.apply.apply(null,[clojure.create_struct,keys_4])),
(row_values_6=(function __clojure_fn_5869_resultset_seq_5871_row_values_5879(){
return (clojure.map.apply(null,[(function __clojure_fn_5869_resultset_seq_5871_row_values_5879_fn_5881(i_1){
return ((rs_1).getObject(i_1))}),idxs_3]))})),
(rows_7=(function __clojure_fn_5869_resultset_seq_5871_thisfn_5885(){
var thisfn_0=arguments.callee;
return ((((rs_1).next())?((new clojure.lang.LazyCons((function __clojure_fn_5869_resultset_seq_5871_thisfn_5885_fn_5887(G__5886_1){switch(arguments.length){
case 0:return (clojure.apply.apply(null,[clojure.struct,row_struct_5,row_values_6.apply(null,[])]))}
return (thisfn_0.apply(null,[]))})))):(null)))})),
rows_7.apply(null,[])))})))}).apply(null,[]);

//======
//(defn set "Returns a set of the distinct elements of coll." [coll] (apply hash-set coll))
//---
(function __clojure_fn_5900(){
return (clojure.JS.def(clojure,"set",(function __clojure_fn_5900_set_5902(coll_1){
return (clojure.apply.apply(null,[clojure.hash_set,coll_1]))})))}).apply(null,[]);

//======
//(defn filter-key [keyfn pred amap] (loop [ret {} es (seq amap)] (if es (if (pred (keyfn (first es))) (recur (assoc ret (key (first es)) (val (first es))) (rest es)) (recur ret (rest es))) ret)))
//---
(function __clojure_fn_5912(){
return (clojure.JS.def(clojure,"filter_key",(function __clojure_fn_5912_filter_key_5914(keyfn_1,pred_2,amap_3){
var ret_4,es_5;
return (((function __loop(){var _rtn,_cnt;(ret_4=clojure.lang.PersistentHashMap.EMPTY),
(es_5=clojure.seq.apply(null,[amap_3]));do{_cnt=0;
_rtn=((es_5)?(((pred_2.apply(null,[keyfn_1.apply(null,[clojure.first.apply(null,[es_5])])]))?((_cnt=1,_rtn=[clojure.assoc.apply(null,[ret_4,clojure.key.apply(null,[clojure.first.apply(null,[es_5])]),clojure.val.apply(null,[clojure.first.apply(null,[es_5])])]),clojure.rest.apply(null,[es_5])],ret_4=_rtn[0],es_5=_rtn[1])):((_cnt=1,_rtn=[ret_4,clojure.rest.apply(null,[es_5])],ret_4=_rtn[0],es_5=_rtn[1])))):(ret_4))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn find-ns "Returns the namespace named by the symbol or nil if it doesn't exist." [sym] (clojure.lang.Namespace/find sym))
//---
(function __clojure_fn_5924(){
return (clojure.JS.def(clojure,"find_ns",(function __clojure_fn_5924_find_ns_5926(sym_1){
return (clojure.lang.Namespace.find(sym_1))})))}).apply(null,[]);

//======
//(defn create-ns "Create a new namespace named by the symbol if one doesn't already\n  exist, returns it or the already-existing namespace of the same\n  name." [sym] (clojure.lang.Namespace/findOrCreate sym))
//---
(function __clojure_fn_5936(){
return (clojure.JS.def(clojure,"create_ns",(function __clojure_fn_5936_create_ns_5938(sym_1){
return (clojure.lang.Namespace.findOrCreate(sym_1))})))}).apply(null,[]);

//======
//(defn remove-ns "Removes the namespace named by the symbol. Use with caution.\n  Cannot be used to remove the clojure namespace." [sym] (clojure.lang.Namespace/remove sym))
//---
(function __clojure_fn_5948(){
return (clojure.JS.def(clojure,"remove_ns",(function __clojure_fn_5948_remove_ns_5950(sym_1){
return (clojure.lang.Namespace.remove(sym_1))})))}).apply(null,[]);

//======
//(defn all-ns "Returns a sequence of all namespaces." [] (clojure.lang.Namespace/all))
//---
(function __clojure_fn_5960(){
return (clojure.JS.def(clojure,"all_ns",(function __clojure_fn_5960_all_ns_5962(){
return (clojure.lang.Namespace.all())})))}).apply(null,[]);

//======
//(defn the-ns [x] (if (instance? clojure.lang.Namespace x) x (or (find-ns x) (throw (Exception. (str "No namespace: " x " found"))))))
//---
(function __clojure_fn_5972(){
return (clojure.JS.def(clojure,"the_ns",(function __clojure_fn_5972_the_ns_5974(x_1){
var or__202_2;
return (((clojure.instance_QMARK_.apply(null,[clojure.lang.Namespace,x_1]))?(x_1):(((or__202_2=clojure.find_ns.apply(null,[x_1])),
((or__202_2)?(or__202_2):((function __throw(){throw (new java.lang.Exception(clojure.str.apply(null,["No namespace: ",x_1," found"])))})()))))))})))}).apply(null,[]);

//======
//(defn ns-name "Returns the name of the namespace, a symbol." [ns] (.getName (the-ns ns)))
//---
(function __clojure_fn_5984(){
return (clojure.JS.def(clojure,"ns_name",(function __clojure_fn_5984_ns_name_5986(ns_1){
return ((clojure.the_ns.apply(null,[ns_1])).getName())})))}).apply(null,[]);

//======
//(defn ns-map "Returns a map of all the mappings for the namespace." [ns] (.getMappings (the-ns ns)))
//---
(function __clojure_fn_5996(){
return (clojure.JS.def(clojure,"ns_map",(function __clojure_fn_5996_ns_map_5998(ns_1){
return ((clojure.the_ns.apply(null,[ns_1])).getMappings())})))}).apply(null,[]);

//======
//(defn ns-unmap "Removes the mappings for the symbol from the namespace." [ns sym] (.unmap (the-ns ns) sym))
//---
(function __clojure_fn_6008(){
return (clojure.JS.def(clojure,"ns_unmap",(function __clojure_fn_6008_ns_unmap_6010(ns_1,sym_2){
return ((clojure.the_ns.apply(null,[ns_1])).unmap(sym_2))})))}).apply(null,[]);

//======
//(defn ns-publics "Returns a map of the public intern mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (= ns (.ns v)) (.isPublic v))) (ns-map ns))))
//---
(function __clojure_fn_6023(){
return (clojure.JS.def(clojure,"ns_publics",(function __clojure_fn_6023_ns_publics_6025(ns_1){
var ns_2;
return (((ns_2=clojure.the_ns.apply(null,[ns_1])),
clojure.filter_key.apply(null,[clojure.val,(function __clojure_fn_6023_ns_publics_6025_fn_6027(v_1){
var and__196_3,and__196_2;
return (((and__196_2=clojure.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__196_2)?(((and__196_3=clojure.lang.Util.equal(ns_2,(v_1).ns)),
((and__196_3)?((v_1).isPublic()):(and__196_3)))):(and__196_2))))}),clojure.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn ns-imports "Returns a map of the import mappings for the namespace." [ns] (filter-key val (partial instance? Class) (ns-map ns)))
//---
(function __clojure_fn_6038(){
return (clojure.JS.def(clojure,"ns_imports",(function __clojure_fn_6038_ns_imports_6040(ns_1){
return (clojure.filter_key.apply(null,[clojure.val,clojure.partial.apply(null,[clojure.instance_QMARK_,java.lang.Class]),clojure.ns_map.apply(null,[ns_1])]))})))}).apply(null,[]);

//======
//(defn ns-refers "Returns a map of the refer mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (not= ns (.ns v)))) (ns-map ns))))
//---
(function __clojure_fn_6059(){
return (clojure.JS.def(clojure,"ns_refers",(function __clojure_fn_6059_ns_refers_6061(ns_1){
var ns_2;
return (((ns_2=clojure.the_ns.apply(null,[ns_1])),
clojure.filter_key.apply(null,[clojure.val,(function __clojure_fn_6059_ns_refers_6061_fn_6063(v_1){
var and__196_2;
return (((and__196_2=clojure.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__196_2)?(clojure.not_EQ_.apply(null,[ns_2,(v_1).ns])):(and__196_2))))}),clojure.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn ns-interns "Returns a map of the intern mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (= ns (.ns v)))) (ns-map ns))))
//---
(function __clojure_fn_6077(){
return (clojure.JS.def(clojure,"ns_interns",(function __clojure_fn_6077_ns_interns_6079(ns_1){
var ns_2;
return (((ns_2=clojure.the_ns.apply(null,[ns_1])),
clojure.filter_key.apply(null,[clojure.val,(function __clojure_fn_6077_ns_interns_6079_fn_6081(v_1){
var and__196_2;
return (((and__196_2=clojure.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__196_2)?(clojure.lang.Util.equal(ns_2,(v_1).ns)):(and__196_2))))}),clojure.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn alias "Add an alias in the current namespace to another\n  namespace. Arguments are two symbols: the alias to be used, and\n  the symbolic name of the target namespace. Use :as in the ns macro in preference \n  to calling this directly." [alias namespace-sym] (.addAlias *ns* alias (find-ns namespace-sym)))
//---
(function __clojure_fn_6092(){
return (clojure.JS.def(clojure,"alias",(function __clojure_fn_6092_alias_6094(alias_1,namespace_sym_2){
return ((clojure._STAR_ns_STAR_).addAlias(alias_1,clojure.find_ns.apply(null,[namespace_sym_2])))})))}).apply(null,[]);

//======
//(defn ns-aliases "Returns a map of the aliases for the namespace." [ns] (.getAliases (the-ns ns)))
//---
(function __clojure_fn_6104(){
return (clojure.JS.def(clojure,"ns_aliases",(function __clojure_fn_6104_ns_aliases_6106(ns_1){
return ((clojure.the_ns.apply(null,[ns_1])).getAliases())})))}).apply(null,[]);

//======
//(defn ns-unalias "Removes the alias for the symbol from the namespace." [ns sym] (.removeAlias (the-ns ns) sym))
//---
(function __clojure_fn_6116(){
return (clojure.JS.def(clojure,"ns_unalias",(function __clojure_fn_6116_ns_unalias_6118(ns_1,sym_2){
return ((clojure.the_ns.apply(null,[ns_1])).removeAlias(sym_2))})))}).apply(null,[]);

//======
//(defn take-nth "Returns a lazy seq of every nth item in coll." [n coll] (when (seq coll) (lazy-cons (first coll) (take-nth n (drop n coll)))))
//---
(function __clojure_fn_6133(){
return (clojure.JS.def(clojure,"take_nth",(function __clojure_fn_6133_take_nth_6135(n_1,coll_2){
return (((clojure.seq.apply(null,[coll_2]))?((new clojure.lang.LazyCons((function __clojure_fn_6133_take_nth_6135_fn_6138(G__6137_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll_2]))}
return (clojure.take_nth.apply(null,[n_1,clojure.drop.apply(null,[n_1,coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn interleave "Returns a lazy seq of the first item in each coll, then the second\n  etc." [& colls] (apply concat (apply map list colls)))
//---
(function __clojure_fn_6150(){
return (clojure.JS.def(clojure,"interleave",clojure.JS.variadic(0,(function __clojure_fn_6150_interleave_6152(){
var colls_1,colls_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[clojure.concat,clojure.apply.apply(null,[clojure.map,clojure.list,colls_1])]))}))))}).apply(null,[]);

//======
//(defn var-get "Gets the value in the var object" [x] (. x (get)))
//---
(function __clojure_fn_6162(){
return (clojure.JS.def(clojure,"var_get",(function __clojure_fn_6162_var_get_6164(x_1){
return ((x_1).get())})))}).apply(null,[]);

//======
//(defn var-set "Sets the value in the var object to val. The var must be\n thread-locally bound." [x val] (. x (set val)))
//---
(function __clojure_fn_6174(){
return (clojure.JS.def(clojure,"var_set",(function __clojure_fn_6174_var_set_6176(x_1,val_2){
return ((x_1).set(val_2))})))}).apply(null,[]);

//======
//(defn ns-resolve "Returns the var or Class to which a symbol will be resolved in the\n  namespace, else nil.  Note that if the symbol is fully qualified,\n  the var/Class to which it resolves need not be present in the\n  namespace." [ns sym] (clojure.lang.Compiler/maybeResolveIn (the-ns ns) sym))
//---
(function __clojure_fn_6192(){
return (clojure.JS.def(clojure,"ns_resolve",(function __clojure_fn_6192_ns_resolve_6194(ns_1,sym_2){
return (clojure.lang.Compiler.maybeResolveIn(clojure.the_ns.apply(null,[ns_1]),sym_2))})))}).apply(null,[]);

//======
//(defn resolve "same as (ns-resolve *ns* symbol)" [sym] (ns-resolve *ns* sym))
//---
(function __clojure_fn_6204(){
return (clojure.JS.def(clojure,"resolve",(function __clojure_fn_6204_resolve_6206(sym_1){
return (clojure.ns_resolve.apply(null,[clojure._STAR_ns_STAR_,sym_1]))})))}).apply(null,[]);

//======
//(defn array-map "Constructs an array-map." ([] (. clojure.lang.PersistentArrayMap EMPTY)) ([& keyvals] (new clojure.lang.PersistentArrayMap (to-array keyvals))))
//---
(function __clojure_fn_6217(){
return (clojure.JS.def(clojure,"array_map",clojure.JS.variadic(0,(function __clojure_fn_6217_array_map_6219(){switch(arguments.length){
case 0:return (clojure.lang.PersistentArrayMap.EMPTY)}
var keyvals_1,keyvals_1=clojure.JS.rest_args(this,arguments,0);
return ((new clojure.lang.PersistentArrayMap(clojure.to_array.apply(null,[keyvals_1]))))}))))}).apply(null,[]);

//======
//(defn nthrest "Returns the nth rest of coll, (seq coll) when n is 0." [coll n] (loop [n n xs (seq coll)] (if (and xs (pos? n)) (recur (dec n) (rest xs)) xs)))
//---
(function __clojure_fn_6230(){
return (clojure.JS.def(clojure,"nthrest",(function __clojure_fn_6230_nthrest_6232(coll_1,n_2){
var xs_4,and__196_5,n_3;
return (((function __loop(){var _rtn,_cnt;(n_3=n_2),
(xs_4=clojure.seq.apply(null,[coll_1]));do{_cnt=0;
_rtn=((((and__196_5=xs_4),
((and__196_5)?(clojure.lang.Numbers.isPos(n_3)):(and__196_5))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_3),clojure.rest.apply(null,[xs_4])],n_3=_rtn[0],xs_4=_rtn[1])):(xs_4))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn symbol? "Return true if x is a Symbol" [x] (instance? clojure.lang.Symbol x))
//---
(function __clojure_fn_6242(){
return (clojure.JS.def(clojure,"symbol_QMARK_",(function __clojure_fn_6242_symbol_QMARK_6244(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Symbol,x_1]))})))}).apply(null,[]);

//======
//(defn keyword? "Return true if x is a Keyword" [x] (instance? clojure.lang.Keyword x))
//---
(function __clojure_fn_6254(){
return (clojure.JS.def(clojure,"keyword_QMARK_",(function __clojure_fn_6254_keyword_QMARK_6256(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Keyword,x_1]))})))}).apply(null,[]);

//======
//(defn destructure [bindings] (let [bmap (apply array-map bindings) pb (fn pb [bvec b v] (let [pvec (fn [bvec b val] (let [gvec (gensym "vec__")] (loop [ret (-> bvec (conj gvec) (conj val)) n 0 bs b seen-rest? false] (if bs (let [firstb (first bs)] (cond (= firstb (quote &)) (recur (pb ret (second bs) (list (quote clojure/nthrest) gvec n)) n (rrest bs) true) (= firstb :as) (pb ret (second bs) gvec) :else (if seen-rest? (throw (new Exception "Unsupported binding form, only :as can follow & parameter")) (recur (pb ret firstb (list (quote clojure/nth) gvec n nil)) (inc n) (rest bs) seen-rest?)))) ret)))) pmap (fn [bvec b v] (let [gmap (or (:as b) (gensym "map__")) defaults (:or b)] (loop [ret (-> bvec (conj gmap) (conj v)) bes (reduce (fn [bes entry] (reduce (fn* [p1__6260 p2__6261] (assoc p1__6260 p2__6261 ((val entry) p2__6261))) (dissoc bes (key entry)) ((key entry) bes))) (dissoc b :as :or) {:syms (fn* [p1__6263] (list (quote quote) p1__6263)), :strs str, :keys (fn* [p1__6262] (keyword (str p1__6262)))})] (if bes (let [bb (key (first bes)) bk (val (first bes)) has-default (contains? defaults bb)] (recur (pb ret bb (if has-default (list (quote clojure/get) gmap bk (defaults bb)) (list (quote clojure/get) gmap bk))) (rest bes))) ret))))] (cond (symbol? b) (-> bvec (conj b) (conj v)) (vector? b) (pvec bvec b v) (map? b) (pmap bvec b v) :else (throw (new Exception (str "Unsupported binding form: " b)))))) process-entry (fn [bvec b] (pb bvec (key b) (val b)))] (if (every? symbol? (keys bmap)) bindings (reduce process-entry [] bmap))))
//---
(function __clojure_fn_6293(){
return (clojure.JS.def(clojure,"destructure",(function __clojure_fn_6293_destructure_6295(bindings_1){
var pb_3,bmap_2,process_entry_4;
return (((bmap_2=clojure.apply.apply(null,[clojure.array_map,bindings_1])),
(pb_3=(function __clojure_fn_6293_destructure_6295_pb_6297(bvec_1,b_2,v_3){
var pmap_5,pvec_4,pb_0=arguments.callee;
return (((pvec_4=(function __clojure_fn_6293_destructure_6295_pb_6297_pvec_6298(bvec_1,b_2,val_3){
var seen_rest_QMARK__8,firstb_9,n_6,gvec_4,bs_7,ret_5;
return (((gvec_4=clojure.gensym.apply(null,["vec__"])),
((function __loop(){var _rtn,_cnt;(ret_5=clojure.conj.apply(null,[clojure.conj.apply(null,[bvec_1,gvec_4]),val_3])),
(n_6=0),
(bs_7=b_2),
(seen_rest_QMARK__8=false);do{_cnt=0;
_rtn=((bs_7)?(((firstb_9=clojure.first.apply(null,[bs_7])),
((clojure.lang.Util.equal(firstb_9,"'&"))?((_cnt=1,_rtn=[pb_0.apply(null,[ret_5,clojure.second.apply(null,[bs_7]),clojure.list.apply(null,["'clojure/nthrest",gvec_4,n_6])]),n_6,clojure.rrest.apply(null,[bs_7]),true],ret_5=_rtn[0],n_6=_rtn[1],bs_7=_rtn[2],seen_rest_QMARK__8=_rtn[3])):(((clojure.lang.Util.equal(firstb_9,clojure.keyword("","as")))?(pb_0.apply(null,[ret_5,clojure.second.apply(null,[bs_7]),gvec_4])):(((clojure.keyword("","else"))?(((seen_rest_QMARK__8)?((function __throw(){throw (new java.lang.Exception("Unsupported binding form, only :as can follow & parameter"))})()):((_cnt=1,_rtn=[pb_0.apply(null,[ret_5,firstb_9,clojure.list.apply(null,["'clojure/nth",gvec_4,n_6,null])]),clojure.lang.Numbers.inc(n_6),clojure.rest.apply(null,[bs_7]),seen_rest_QMARK__8],ret_5=_rtn[0],n_6=_rtn[1],bs_7=_rtn[2],seen_rest_QMARK__8=_rtn[3])))):(null)))))))):(ret_5))}while(_cnt);return _rtn;})())))})),
(pmap_5=(function __clojure_fn_6293_destructure_6295_pb_6297_pmap_6301(bvec_1,b_2,v_3){
var gmap_4,or__202_4,ret_6,bes_7,has_default_10,defaults_5,bb_8,bk_9;
return (((gmap_4=((or__202_4=clojure.keyword("","as").apply(null,[b_2])),
((or__202_4)?(or__202_4):(clojure.gensym.apply(null,["map__"]))))),
(defaults_5=clojure.keyword("","or").apply(null,[b_2])),
((function __loop(){var _rtn,_cnt;(ret_6=clojure.conj.apply(null,[clojure.conj.apply(null,[bvec_1,gmap_4]),v_3])),
(bes_7=clojure.reduce.apply(null,[(function __clojure_fn_6293_destructure_6295_pb_6297_pmap_6301_fn_6303(bes_1,entry_2){
return (clojure.reduce.apply(null,[(function __clojure_fn_6293_destructure_6295_pb_6297_pmap_6301_fn_6303_fn_6305(p1__6260_1,p2__6261_2){
return (clojure.assoc.apply(null,[p1__6260_1,p2__6261_2,clojure.val.apply(null,[entry_2]).apply(null,[p2__6261_2])]))}),clojure.dissoc.apply(null,[bes_1,clojure.key.apply(null,[entry_2])]),clojure.key.apply(null,[entry_2]).apply(null,[bes_1])]))}),clojure.dissoc.apply(null,[b_2,clojure.keyword("","as"),clojure.keyword("","or")]),clojure.hash_map(clojure.keyword("","syms"),(function __clojure_fn_6293_destructure_6295_pb_6297_pmap_6301_fn_6309(p1__6263_1){
return (clojure.list.apply(null,["'quote",p1__6263_1]))}),clojure.keyword("","strs"),clojure.str,clojure.keyword("","keys"),(function __clojure_fn_6293_destructure_6295_pb_6297_pmap_6301_fn_6312(p1__6262_1){
return (clojure.keyword.apply(null,[clojure.str.apply(null,[p1__6262_1])]))}))]));do{_cnt=0;
_rtn=((bes_7)?(((bb_8=clojure.key.apply(null,[clojure.first.apply(null,[bes_7])])),
(bk_9=clojure.val.apply(null,[clojure.first.apply(null,[bes_7])])),
(has_default_10=clojure.contains_QMARK_.apply(null,[defaults_5,bb_8])),
(_cnt=1,_rtn=[pb_0.apply(null,[ret_6,bb_8,((has_default_10)?(clojure.list.apply(null,["'clojure/get",gmap_4,bk_9,defaults_5.apply(null,[bb_8])])):(clojure.list.apply(null,["'clojure/get",gmap_4,bk_9])))]),clojure.rest.apply(null,[bes_7])],ret_6=_rtn[0],bes_7=_rtn[1]))):(ret_6))}while(_cnt);return _rtn;})())))})),
((clojure.symbol_QMARK_.apply(null,[b_2]))?(clojure.conj.apply(null,[clojure.conj.apply(null,[bvec_1,b_2]),v_3])):(((clojure.vector_QMARK_.apply(null,[b_2]))?(pvec_4.apply(null,[bvec_1,b_2,v_3])):(((clojure.map_QMARK_.apply(null,[b_2]))?(pmap_5.apply(null,[bvec_1,b_2,v_3])):(((clojure.keyword("","else"))?((function __throw(){throw (new java.lang.Exception(clojure.str.apply(null,["Unsupported binding form: ",b_2])))})()):(null))))))))))})),
(process_entry_4=(function __clojure_fn_6293_destructure_6295_process_entry_6317(bvec_1,b_2){
return (pb_3.apply(null,[bvec_1,clojure.key.apply(null,[b_2]),clojure.val.apply(null,[b_2])]))})),
((clojure.every_QMARK_.apply(null,[clojure.symbol_QMARK_,clojure.keys.apply(null,[bmap_2])]))?(bindings_1):(clojure.reduce.apply(null,[process_entry_4,clojure.lang.PersistentVector.EMPTY,bmap_2])))))})))}).apply(null,[]);

//======
//(defn pr-str "pr to a string, returning it" {:tag String} [& xs] (with-out-str (apply pr xs)))
//---
(function __clojure_fn_6418(){
return (clojure.JS.def(clojure,"pr_str",clojure.JS.variadic(0,(function __clojure_fn_6418_pr_str_6420(){
var xs_1,s__1229_2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1229_2=(new java.io.StringWriter())),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_out_STAR_,s__1229_2])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.pr,xs_1]),
clojure.str.apply(null,[s__1229_2]))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn prn-str "prn to a string, returning it" {:tag String} [& xs] (with-out-str (apply prn xs)))
//---
(function __clojure_fn_6430(){
return (clojure.JS.def(clojure,"prn_str",clojure.JS.variadic(0,(function __clojure_fn_6430_prn_str_6432(){
var xs_1,s__1229_2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1229_2=(new java.io.StringWriter())),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_out_STAR_,s__1229_2])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.prn,xs_1]),
clojure.str.apply(null,[s__1229_2]))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn print-str "print to a string, returning it" {:tag String} [& xs] (with-out-str (apply print xs)))
//---
(function __clojure_fn_6442(){
return (clojure.JS.def(clojure,"print_str",clojure.JS.variadic(0,(function __clojure_fn_6442_print_str_6444(){
var xs_1,s__1229_2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1229_2=(new java.io.StringWriter())),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_out_STAR_,s__1229_2])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.print,xs_1]),
clojure.str.apply(null,[s__1229_2]))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn println-str "println to a string, returning it" {:tag String} [& xs] (with-out-str (apply println xs)))
//---
(function __clojure_fn_6454(){
return (clojure.JS.def(clojure,"println_str",clojure.JS.variadic(0,(function __clojure_fn_6454_println_str_6456(){
var xs_1,s__1229_2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1229_2=(new java.io.StringWriter())),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_out_STAR_,s__1229_2])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.println,xs_1]),
clojure.str.apply(null,[s__1229_2]))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn test "test [v] finds fn at key :test in var metadata and calls it,\n  presuming failure will throw exception" [v] (let [f (:test (clojure/meta v))] (if f (do (f) :ok) :no-test)))
//---
(function __clojure_fn_6472(){
return (clojure.JS.def(clojure,"test",(function __clojure_fn_6472_test_6474(v_1){
var f_2;
return (((f_2=clojure.keyword("","test").apply(null,[clojure.meta.apply(null,[v_1])])),
((f_2)?(f_2.apply(null,[]),
clojure.keyword("","ok")):(clojure.keyword("","no-test")))))})))}).apply(null,[]);

//======
//(defn re-pattern "Returns an instance of java.util.regex.Pattern, for use, e.g. in\n  re-matcher." {:tag java.util.regex.Pattern} [s] (. java.util.regex.Pattern (compile s)))
//---
(function __clojure_fn_6484(){
return (clojure.JS.def(clojure,"re_pattern",(function __clojure_fn_6484_re_pattern_6486(s_1){
return (java.util.regex.Pattern.compile(s_1))})))}).apply(null,[]);

//======
//(defn re-matcher "Returns an instance of java.util.regex.Matcher, for use, e.g. in\n  re-find." {:tag java.util.regex.Matcher} [re s] (. re (matcher s)))
//---
(function __clojure_fn_6496(){
return (clojure.JS.def(clojure,"re_matcher",(function __clojure_fn_6496_re_matcher_6498(re_1,s_2){
return ((re_1).matcher(s_2))})))}).apply(null,[]);

//======
//(defn re-groups "Returns the groups from the most recent match/find. If there are no\n  nested groups, returns a string of the entire match. If there are\n  nested groups, returns a vector of the groups, the first element\n  being the entire match." [m] (let [gc (. m (groupCount))] (if (zero? gc) (. m (group)) (loop [ret [] c 0] (if (<= c gc) (recur (conj ret (. m (group c))) (inc c)) ret)))))
//---
(function __clojure_fn_6508(){
return (clojure.JS.def(clojure,"re_groups",(function __clojure_fn_6508_re_groups_6510(m_1){
var ret_3,gc_2,c_4;
return (((gc_2=(m_1).groupCount()),
((clojure.lang.Numbers.isZero(gc_2))?((m_1).group()):(((function __loop(){var _rtn,_cnt;(ret_3=clojure.lang.PersistentVector.EMPTY),
(c_4=0);do{_cnt=0;
_rtn=((clojure.lang.Numbers.lte(c_4,gc_2))?((_cnt=1,_rtn=[clojure.conj.apply(null,[ret_3,(m_1).group(c_4)]),clojure.lang.Numbers.inc(c_4)],ret_3=_rtn[0],c_4=_rtn[1])):(ret_3))}while(_cnt);return _rtn;})())))))})))}).apply(null,[]);

//======
//(defn re-seq "Returns a lazy sequence of successive matches of pattern in string,\n  using java.util.regex.Matcher.find(), each such match processed with\n  re-groups." [re s] (let [m (re-matcher re s)] ((fn step [] (when (. m (find)) (lazy-cons (re-groups m) (step)))))))
//---
(function __clojure_fn_6527(){
return (clojure.JS.def(clojure,"re_seq",(function __clojure_fn_6527_re_seq_6529(re_1,s_2){
var m_3;
return (((m_3=clojure.re_matcher.apply(null,[re_1,s_2])),
(function __clojure_fn_6527_re_seq_6529_step_6531(){
var step_0=arguments.callee;
return ((((m_3).find())?((new clojure.lang.LazyCons((function __clojure_fn_6527_re_seq_6529_step_6531_fn_6533(G__6532_1){switch(arguments.length){
case 0:return (clojure.re_groups.apply(null,[m_3]))}
return (step_0.apply(null,[]))})))):(null)))}).apply(null,[])))})))}).apply(null,[]);

//======
//(defn re-matches "Returns the match, if any, of string to pattern, using\n  java.util.regex.Matcher.matches().  Uses re-groups to return the\n  groups." [re s] (let [m (re-matcher re s)] (when (. m (matches)) (re-groups m))))
//---
(function __clojure_fn_6546(){
return (clojure.JS.def(clojure,"re_matches",(function __clojure_fn_6546_re_matches_6548(re_1,s_2){
var m_3;
return (((m_3=clojure.re_matcher.apply(null,[re_1,s_2])),
(((m_3).matches())?(clojure.re_groups.apply(null,[m_3])):(null))))})))}).apply(null,[]);

//======
//(defn re-find "Returns the next regex match, if any, of string to pattern, using\n  java.util.regex.Matcher.find().  Uses re-groups to return the\n  groups." ([m] (when (. m (find)) (re-groups m))) ([re s] (let [m (re-matcher re s)] (re-find m))))
//---
(function __clojure_fn_6559(){
return (clojure.JS.def(clojure,"re_find",(function __clojure_fn_6559_re_find_6561(re_1,s_2){switch(arguments.length){
case 1:var m_1=arguments[0];
return ((((m_1).find())?(clojure.re_groups.apply(null,[m_1])):(null)))}
var m_3;
return (((m_3=clojure.re_matcher.apply(null,[re_1,s_2])),
clojure.re_find.apply(null,[m_3])))})))}).apply(null,[]);

//======
//(defn rand "Returns a random floating point number between 0 (inclusive) and\n  1 (exclusive)." ([] (. Math (random))) ([n] (* n (rand))))
//---
(function __clojure_fn_6573(){
return (clojure.JS.def(clojure,"rand",(function __clojure_fn_6573_rand_6575(n_1){switch(arguments.length){
case 0:return (java.lang.Math.random())}
return (clojure.lang.Numbers.multiply(n_1,clojure.rand.apply(null,[])))})))}).apply(null,[]);

//======
//(defn rand-int "Returns a random integer between 0 (inclusive) and n (exclusive)." [n] (int (rand n)))
//---
(function __clojure_fn_6586(){
return (clojure.JS.def(clojure,"rand_int",(function __clojure_fn_6586_rand_int_6588(n_1){
return (clojure.lang.RT.intCast(clojure.rand.apply(null,[n_1])))})))}).apply(null,[]);

//======
//(defn print-doc [v] (println "-------------------------") (println (str (ns-name (:ns (clojure/meta v))) "/" (:name (clojure/meta v)))) (prn (:arglists (clojure/meta v))) (when (:macro (clojure/meta v)) (println "Macro")) (println " " (:doc (clojure/meta v))))
//---
(function __clojure_fn_6604(){
return (clojure.JS.def(clojure,"print_doc",(function __clojure_fn_6604_print_doc_6606(v_1){
return (clojure.println.apply(null,["-------------------------"]),
clojure.println.apply(null,[clojure.str.apply(null,[clojure.ns_name.apply(null,[clojure.keyword("","ns").apply(null,[clojure.meta.apply(null,[v_1])])]),"/",clojure.keyword("","name").apply(null,[clojure.meta.apply(null,[v_1])])])]),
clojure.prn.apply(null,[clojure.keyword("","arglists").apply(null,[clojure.meta.apply(null,[v_1])])]),
((clojure.keyword("","macro").apply(null,[clojure.meta.apply(null,[v_1])]))?(clojure.println.apply(null,["Macro"])):(null)),
clojure.println.apply(null,[" ",clojure.keyword("","doc").apply(null,[clojure.meta.apply(null,[v_1])])]))})))}).apply(null,[]);

//======
//(defn find-doc "Prints documentation for any var whose documentation or name\n contains a match for re-string" [re-string] (let [re (re-pattern re-string)] (dorun (for [ns (all-ns) v (sort-by (comp :name meta) (vals (ns-interns ns))) :when (and (:doc (clojure/meta v)) (or (re-find (re-matcher re (:doc (clojure/meta v)))) (re-find (re-matcher re (str (:name (clojure/meta v)))))))] (print-doc v)))))
//---
(function __clojure_fn_6636(){
return (clojure.JS.def(clojure,"find_doc",(function __clojure_fn_6636_find_doc_6638(re_string_1){
var iter__1205_3,re_2;
return (((re_2=clojure.re_pattern.apply(null,[re_string_1])),
clojure.dorun.apply(null,[((iter__1205_3=(function __clojure_fn_6636_find_doc_6638_iter_6640_6644(s__6641_1){
var iterys__1204_3,ns_2,iter__1198_4,iter__6640_0=arguments.callee;
return (((clojure.seq.apply(null,[s__6641_1]))?(((ns_2=clojure.first.apply(null,[s__6641_1])),
((true)?(((iterys__1204_3=(function __clojure_fn_6636_find_doc_6638_iter_6640_6644_iter_6642_6645(s__6643_1){
var _cnt,_rtn,and__196_3,v_2,or__202_4,iter__6642_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[s__6643_1]))?(((v_2=clojure.first.apply(null,[s__6643_1])),
((((and__196_3=clojure.keyword("","doc").apply(null,[clojure.meta.apply(null,[v_2])])),
((and__196_3)?(((or__202_4=clojure.re_find.apply(null,[clojure.re_matcher.apply(null,[re_2,clojure.keyword("","doc").apply(null,[clojure.meta.apply(null,[v_2])])])])),
((or__202_4)?(or__202_4):(clojure.re_find.apply(null,[clojure.re_matcher.apply(null,[re_2,clojure.str.apply(null,[clojure.keyword("","name").apply(null,[clojure.meta.apply(null,[v_2])])])])]))))):(and__196_3))))?((new clojure.lang.LazyCons((function __clojure_fn_6636_find_doc_6638_iter_6640_6644_iter_6642_6645_fn_6647(G__6646_1){switch(arguments.length){
case 0:return (clojure.print_doc.apply(null,[v_2]))}
return (iter__6642_0.apply(null,[clojure.rest.apply(null,[s__6643_1])]))})))):((_cnt=1,_rtn=[clojure.rest.apply(null,[s__6643_1])],s__6643_1=_rtn[0]))))):(null))
}while(_cnt);return _rtn;})),
((iter__1198_4=(function __clojure_fn_6636_find_doc_6638_iter_6640_6644_iter_1198_6652(coll__1199_1){
var iter__1198_0=arguments.callee;
return (((clojure.seq.apply(null,[coll__1199_1]))?((new clojure.lang.LazyCons((function __clojure_fn_6636_find_doc_6638_iter_6640_6644_iter_1198_6652_fn_6654(G__6653_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll__1199_1]))}
return (iter__1198_0.apply(null,[clojure.rest.apply(null,[coll__1199_1])]))})))):(clojure.seq.apply(null,[iter__6640_0.apply(null,[clojure.rest.apply(null,[s__6641_1])])]))))})),
iter__1198_4.apply(null,[iterys__1204_3.apply(null,[clojure.sort_by.apply(null,[clojure.comp.apply(null,[clojure.keyword("","name"),clojure.meta]),clojure.vals.apply(null,[clojure.ns_interns.apply(null,[ns_2])])])])])))):(null)))):(null)))})),
iter__1205_3.apply(null,[clojure.all_ns.apply(null,[])]))])))})))}).apply(null,[]);

//======
//(defn special-form-anchor "Returns the anchor tag on http://clojure.org/special_forms for the\n  special form x, or nil" [x] (#{(quote recur) (quote .) (quote var) (quote let) (quote quote) (quote set!) (quote monitor-enter) (quote loop) (quote new) (quote fn) (quote if) (quote try) (quote def) (quote monitor-exit) (quote throw) (quote do)} x))
//---
(function __clojure_fn_6668(){
return (clojure.JS.def(clojure,"special_form_anchor",(function __clojure_fn_6668_special_form_anchor_6670(x_1){
return (clojure.hash_set("'recur","'.","'var","'let","'quote","'set!","'monitor-enter","'loop","'new","'fn","'if","'try","'def","'monitor-exit","'throw","'do").apply(null,[x_1]))})))}).apply(null,[]);

//======
//(defn syntax-symbol-anchor "Returns the anchor tag on http://clojure.org/special_forms for the\n  special form that uses syntax symbol x, or nil" [x] ({(quote &) (quote fn), (quote catch) (quote try), (quote finally) (quote try)} x))
//---
(function __clojure_fn_6680(){
return (clojure.JS.def(clojure,"syntax_symbol_anchor",(function __clojure_fn_6680_syntax_symbol_anchor_6682(x_1){
return (clojure.hash_map("'&","'fn","'catch","'try","'finally","'try").apply(null,[x_1]))})))}).apply(null,[]);

//======
//(defn print-special-doc [name type anchor] (println "-------------------------") (println name) (println type) (println (str "  Please see http://clojure.org/special_forms#" anchor)))
//---
(function __clojure_fn_6692(){
return (clojure.JS.def(clojure,"print_special_doc",(function __clojure_fn_6692_print_special_doc_6694(name_1,type_2,anchor_3){
return (clojure.println.apply(null,["-------------------------"]),
clojure.println.apply(null,[name_1]),
clojure.println.apply(null,[type_2]),
clojure.println.apply(null,[clojure.str.apply(null,["  Please see http://clojure.org/special_forms#",anchor_3])]))})))}).apply(null,[]);

//======
//(defn tree-seq "returns a lazy sequence of the nodes in a tree, via a depth-first walk.\n  branch? must be a fn of one arg that returns true if passed a node\n  that can have children (but may not).  children must be a fn of one\n  arg that returns a sequence of the children. Will only be called on\n  nodes for which branch? returns true. Root is the root node of the\n  tree, must be a branch." [branch? children root] (let [walk (fn walk [nodes] (when-first node nodes (lazy-cons node (if (branch? node) (lazy-cat (walk (children node)) (walk (rest nodes))) (walk (rest nodes))))))] (lazy-cons root (walk (children root)))))
//---
(function __clojure_fn_6729(){
return (clojure.JS.def(clojure,"tree_seq",(function __clojure_fn_6729_tree_seq_6731(branch_QMARK__1,children_2,root_3){
var walk_4;
return (((walk_4=(function __clojure_fn_6729_tree_seq_6731_walk_6733(nodes_1){
var node_2,walk_0=arguments.callee;
return (((clojure.seq.apply(null,[nodes_1]))?(((node_2=clojure.first.apply(null,[nodes_1])),
(new clojure.lang.LazyCons((function __clojure_fn_6729_tree_seq_6731_walk_6733_fn_6735(G__6734_1){switch(arguments.length){
case 0:return (node_2)}
var iter__1198_2;
return (((branch_QMARK__1.apply(null,[node_2]))?(((iter__1198_2=(function __clojure_fn_6729_tree_seq_6731_walk_6733_fn_6735_iter_1198_6738(coll__1199_1){
var iter__1198_0=arguments.callee;
return (((clojure.seq.apply(null,[coll__1199_1]))?((new clojure.lang.LazyCons((function __clojure_fn_6729_tree_seq_6731_walk_6733_fn_6735_iter_1198_6738_fn_6740(G__6739_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll__1199_1]))}
return (iter__1198_0.apply(null,[clojure.rest.apply(null,[coll__1199_1])]))})))):(clojure.seq.apply(null,[walk_0.apply(null,[clojure.rest.apply(null,[nodes_1])])]))))})),
iter__1198_2.apply(null,[walk_0.apply(null,[children_2.apply(null,[node_2])])]))):(walk_0.apply(null,[clojure.rest.apply(null,[nodes_1])]))))}))))):(null)))})),
(new clojure.lang.LazyCons((function __clojure_fn_6729_tree_seq_6731_fn_6748(G__6747_1){switch(arguments.length){
case 0:return (root_3)}
return (walk_4.apply(null,[children_2.apply(null,[root_3])]))})))))})))}).apply(null,[]);

//======
//(defn file-seq "A tree seq on java.io.Files" [dir] (tree-seq (fn [f] (. f (isDirectory))) (fn [d] (seq (. d (listFiles)))) dir))
//---
(function __clojure_fn_6766(){
return (clojure.JS.def(clojure,"file_seq",(function __clojure_fn_6766_file_seq_6768(dir_1){
return (clojure.tree_seq.apply(null,[(function __clojure_fn_6766_file_seq_6768_fn_6770(f_1){
return ((f_1).isDirectory())}),(function __clojure_fn_6766_file_seq_6768_fn_6773(d_1){
return (clojure.seq.apply(null,[(d_1).listFiles()]))}),dir_1]))})))}).apply(null,[]);

//======
//(defn xml-seq "A tree seq on the xml elements as per xml/parse" [root] (tree-seq (complement string?) (comp seq :content) root))
//---
(function __clojure_fn_6784(){
return (clojure.JS.def(clojure,"xml_seq",(function __clojure_fn_6784_xml_seq_6786(root_1){
return (clojure.tree_seq.apply(null,[clojure.complement.apply(null,[clojure.string_QMARK_]),clojure.comp.apply(null,[clojure.seq,clojure.keyword("","content")]),root_1]))})))}).apply(null,[]);

//======
//(defn special-symbol? "Returns true if s names a special form" [s] (contains? (. clojure.lang.Compiler specials) s))
//---
(function __clojure_fn_6796(){
return (clojure.JS.def(clojure,"special_symbol_QMARK_",(function __clojure_fn_6796_special_symbol_QMARK_6798(s_1){
return (clojure.contains_QMARK_.apply(null,[clojure.lang.Compiler.specials,s_1]))})))}).apply(null,[]);

//======
//(defn var? "Returns true if v is of type clojure.lang.Var" [v] (instance? clojure.lang.Var v))
//---
(function __clojure_fn_6808(){
return (clojure.JS.def(clojure,"var_QMARK_",(function __clojure_fn_6808_var_QMARK_6810(v_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Var,v_1]))})))}).apply(null,[]);

//======
//(defn slurp "Reads the file named by f into a string and returns it." [f] (with-open r (new java.io.BufferedReader (new java.io.FileReader f)) (let [sb (new StringBuilder)] (loop [c (. r (read))] (if (neg? c) (str sb) (do (. sb (append (char c))) (recur (. r (read)))))))))
//---
(function __clojure_fn_6820(){
return (clojure.JS.def(clojure,"slurp",(function __clojure_fn_6820_slurp_6822(f_1){
var c_6,sb_5,r_2;
return (((r_2=(new java.io.BufferedReader((new java.io.FileReader(f_1))))),
(function __try(){try{var _rtn=(((sb_5=(new java.lang.StringBuilder())),
((function __loop(){var _rtn,_cnt;(c_6=(r_2).read());do{_cnt=0;
_rtn=((clojure.lang.Numbers.isNeg(c_6))?(clojure.str.apply(null,[sb_5])):((sb_5).append(clojure.char_.apply(null,[c_6])),
(_cnt=1,_rtn=[(r_2).read()],c_6=_rtn[0])))}while(_cnt);return _rtn;})())))}
finally{(r_2).close()}})()))})))}).apply(null,[]);

//======
//(defn subs "Returns the substring of s beginning at start inclusive, and ending\n  at end (defaults to length of string), exclusive." ([s start] (. s (substring start))) ([s start end] (. s (substring start end))))
//---
(function __clojure_fn_6833(){
return (clojure.JS.def(clojure,"subs",(function __clojure_fn_6833_subs_6835(s_1,start_2,end_3){switch(arguments.length){
case 2:return ((s_1).substring(start_2))}
return ((s_1).substring(start_2,end_3))})))}).apply(null,[]);

//======
//(defn max-key "Returns the x for which (k x), a number, is greatest." ([k x] x) ([k x y] (if (> (k x) (k y)) x y)) ([k x y & more] (reduce (fn* [p1__6840 p2__6841] (max-key k p1__6840 p2__6841)) (max-key k x y) more)))
//---
(function __clojure_fn_6853(){
return (clojure.JS.def(clojure,"max_key",clojure.JS.variadic(3,(function __clojure_fn_6853_max_key_6855(k_1,x_2,y_3){switch(arguments.length){
case 3:return (((clojure.lang.Numbers.gt(k_1.apply(null,[x_2]),k_1.apply(null,[y_3])))?(x_2):(y_3)))
case 2:return (x_2)}
var more_4,more_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.reduce.apply(null,[(function __clojure_fn_6853_max_key_6855_fn_6859(p1__6840_1,p2__6841_2){
return (clojure.max_key.apply(null,[k_1,p1__6840_1,p2__6841_2]))}),clojure.max_key.apply(null,[k_1,x_2,y_3]),more_4]))}))))}).apply(null,[]);

//======
//(defn min-key "Returns the x for which (k x), a number, is least." ([k x] x) ([k x y] (if (< (k x) (k y)) x y)) ([k x y & more] (reduce (fn* [p1__6864 p2__6865] (min-key k p1__6864 p2__6865)) (min-key k x y) more)))
//---
(function __clojure_fn_6877(){
return (clojure.JS.def(clojure,"min_key",clojure.JS.variadic(3,(function __clojure_fn_6877_min_key_6879(k_1,x_2,y_3){switch(arguments.length){
case 3:return (((clojure.lang.Numbers.lt(k_1.apply(null,[x_2]),k_1.apply(null,[y_3])))?(x_2):(y_3)))
case 2:return (x_2)}
var more_4,more_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.reduce.apply(null,[(function __clojure_fn_6877_min_key_6879_fn_6883(p1__6864_1,p2__6865_2){
return (clojure.min_key.apply(null,[k_1,p1__6864_1,p2__6865_2]))}),clojure.min_key.apply(null,[k_1,x_2,y_3]),more_4]))}))))}).apply(null,[]);

//======
//(defn distinct "Returns a lazy sequence of the elements of coll with duplicates removed" [coll] (let [step (fn step [[f & r :as xs] seen] (when xs (if (seen f) (recur r seen) (lazy-cons f (step r (conj seen f))))))] (step (seq coll) #{})))
//---
(function __clojure_fn_6903(){
return (clojure.JS.def(clojure,"distinct",(function __clojure_fn_6903_distinct_6905(coll_1){
var step_2;
return (((step_2=(function __clojure_fn_6903_distinct_6905_step_6908(p__6907_1,seen_2){
var _cnt,_rtn,xs_6,f_4,vec__6909_3,r_5,step_0=arguments.callee;
do{_cnt=0;_rtn=((vec__6909_3=p__6907_1),
(f_4=clojure.nth.apply(null,[vec__6909_3,0,null])),
(r_5=clojure.nthrest.apply(null,[vec__6909_3,1])),
(xs_6=vec__6909_3),
((xs_6)?(((seen_2.apply(null,[f_4]))?((_cnt=1,_rtn=[r_5,seen_2],p__6907_1=_rtn[0],seen_2=_rtn[1])):((new clojure.lang.LazyCons((function __clojure_fn_6903_distinct_6905_step_6908_fn_6911(G__6910_1){switch(arguments.length){
case 0:return (f_4)}
return (step_0.apply(null,[r_5,clojure.conj.apply(null,[seen_2,f_4])]))})))))):(null)))
}while(_cnt);return _rtn;})),
step_2.apply(null,[clojure.seq.apply(null,[coll_1]),clojure.lang.PersistentHashSet.EMPTY])))})))}).apply(null,[]);

//======
//(defn replace "Given a map of replacement pairs and a vector/collection, returns a\n  vector/seq with any elements = a key in smap replaced with the\n  corresponding val in smap" [smap coll] (if (vector? coll) (reduce (fn [v i] (if-let e (find smap (nth v i)) (assoc v i (val e)) v)) coll (range (count coll))) (map (fn* [p1__6933] (if-let e (find smap p1__6933) (val e) p1__6933)) coll)))
//---
(function __clojure_fn_6946(){
return (clojure.JS.def(clojure,"replace",(function __clojure_fn_6946_replace_6948(smap_1,coll_2){
return (((clojure.vector_QMARK_.apply(null,[coll_2]))?(clojure.reduce.apply(null,[(function __clojure_fn_6946_replace_6948_fn_6950(v_1,i_2){
var temp__1408_3,e_4;
return (((temp__1408_3=clojure.find.apply(null,[smap_1,clojure.nth.apply(null,[v_1,i_2])])),
((temp__1408_3)?(((e_4=temp__1408_3),
clojure.assoc.apply(null,[v_1,i_2,clojure.val.apply(null,[e_4])]))):(v_1))))}),coll_2,clojure.range.apply(null,[clojure.count.apply(null,[coll_2])])])):(clojure.map.apply(null,[(function __clojure_fn_6946_replace_6948_fn_6953(p1__6933_1){
var temp__1408_2,e_3;
return (((temp__1408_2=clojure.find.apply(null,[smap_1,p1__6933_1])),
((temp__1408_2)?(((e_3=temp__1408_2),
clojure.val.apply(null,[e_3]))):(p1__6933_1))))}),coll_2]))))})))}).apply(null,[]);

//======
//(defn bound-fn {:private true} [sc test key] (fn [e] (test (.. sc comparator (compare (. sc entryKey e) key)) 0)))
//---
(function __clojure_fn_6980(){
return (clojure.JS.def(clojure,"bound_fn",(function __clojure_fn_6980_bound_fn_6982(sc_1,test_2,key_3){
return ((function __clojure_fn_6980_bound_fn_6982_fn_6984(e_1){
return (test_2.apply(null,[((sc_1).comparator()).compare((sc_1).entryKey(e_1),key_3),0]))}))})))}).apply(null,[]);

//======
//(defn subseq "sc must be a sorted collection, test(s) one of <, <=, > or\n  >=. Returns a seq of those entries with keys ek for\n  which (test (.. sc comparator (compare ek key)) 0) is true" ([sc test key] (let [include (bound-fn sc test key)] (if (#{> >=} test) (when-let [e :as s] (. sc seqFrom key true) (if (include e) s (rest s))) (take-while include (. sc seq true))))) ([sc start-test start-key end-test end-key] (when-let [e :as s] (. sc seqFrom start-key true) (take-while (bound-fn sc end-test end-key) (if ((bound-fn sc start-test start-key) e) s (rest s))))))
//---
(function __clojure_fn_6998(){
return (clojure.JS.def(clojure,"subseq",(function __clojure_fn_6998_subseq_7000(sc_1,start_test_2,start_key_3,end_test_4,end_key_5){switch(arguments.length){
case 3:var temp__1413_5,e_7,s_8,vec__7002_6,include_4,test_2=arguments[1],key_3=arguments[2];
return (((include_4=clojure.bound_fn.apply(null,[sc_1,test_2,key_3])),
((clojure.hash_set(clojure._GT_,clojure._GT__EQ_).apply(null,[test_2]))?(((temp__1413_5=(sc_1).seqFrom(key_3,true)),
((temp__1413_5)?(((vec__7002_6=temp__1413_5),
(e_7=clojure.nth.apply(null,[vec__7002_6,0,null])),
(s_8=vec__7002_6),
((include_4.apply(null,[e_7]))?(s_8):(clojure.rest.apply(null,[s_8]))))):(null)))):(clojure.take_while.apply(null,[include_4,(sc_1).seq(true)])))))}
var temp__1413_6,vec__7004_7,e_8,s_9;
return (((temp__1413_6=(sc_1).seqFrom(start_key_3,true)),
((temp__1413_6)?(((vec__7004_7=temp__1413_6),
(e_8=clojure.nth.apply(null,[vec__7004_7,0,null])),
(s_9=vec__7004_7),
clojure.take_while.apply(null,[clojure.bound_fn.apply(null,[sc_1,end_test_4,end_key_5]),((clojure.bound_fn.apply(null,[sc_1,start_test_2,start_key_3]).apply(null,[e_8]))?(s_9):(clojure.rest.apply(null,[s_9])))]))):(null))))})))}).apply(null,[]);

//======
//(defn rsubseq "sc must be a sorted collection, test(s) one of <, <=, > or\n  >=. Returns a reverse seq of those entries with keys ek for\n  which (test (.. sc comparator (compare ek key)) 0) is true" ([sc test key] (let [include (bound-fn sc test key)] (if (#{< <=} test) (when-let [e :as s] (. sc seqFrom key false) (if (include e) s (rest s))) (take-while include (. sc seq false))))) ([sc start-test start-key end-test end-key] (when-let [e :as s] (. sc seqFrom end-key false) (take-while (bound-fn sc start-test start-key) (if ((bound-fn sc end-test end-key) e) s (rest s))))))
//---
(function __clojure_fn_7016(){
return (clojure.JS.def(clojure,"rsubseq",(function __clojure_fn_7016_rsubseq_7018(sc_1,start_test_2,start_key_3,end_test_4,end_key_5){switch(arguments.length){
case 3:var include_4,temp__1413_5,e_7,vec__7020_6,s_8,test_2=arguments[1],key_3=arguments[2];
return (((include_4=clojure.bound_fn.apply(null,[sc_1,test_2,key_3])),
((clojure.hash_set(clojure._LT_,clojure._LT__EQ_).apply(null,[test_2]))?(((temp__1413_5=(sc_1).seqFrom(key_3,false)),
((temp__1413_5)?(((vec__7020_6=temp__1413_5),
(e_7=clojure.nth.apply(null,[vec__7020_6,0,null])),
(s_8=vec__7020_6),
((include_4.apply(null,[e_7]))?(s_8):(clojure.rest.apply(null,[s_8]))))):(null)))):(clojure.take_while.apply(null,[include_4,(sc_1).seq(false)])))))}
var vec__7022_7,s_9,temp__1413_6,e_8;
return (((temp__1413_6=(sc_1).seqFrom(end_key_5,false)),
((temp__1413_6)?(((vec__7022_7=temp__1413_6),
(e_8=clojure.nth.apply(null,[vec__7022_7,0,null])),
(s_9=vec__7022_7),
clojure.take_while.apply(null,[clojure.bound_fn.apply(null,[sc_1,start_test_2,start_key_3]),((clojure.bound_fn.apply(null,[sc_1,end_test_4,end_key_5]).apply(null,[e_8]))?(s_9):(clojure.rest.apply(null,[s_9])))]))):(null))))})))}).apply(null,[]);

//======
//(defn repeatedly "Takes a function of no args, presumably with side effects, and returns an infinite\n  lazy sequence of calls to it" [f] (lazy-cons (f) (repeatedly f)))
//---
(function __clojure_fn_7036(){
return (clojure.JS.def(clojure,"repeatedly",(function __clojure_fn_7036_repeatedly_7038(f_1){
return ((new clojure.lang.LazyCons((function __clojure_fn_7036_repeatedly_7038_fn_7041(G__7040_1){switch(arguments.length){
case 0:return (f_1.apply(null,[]))}
return (clojure.repeatedly.apply(null,[f_1]))}))))})))}).apply(null,[]);

//======
//(defn add-classpath "Adds the url (String or URL object) to the classpath per URLClassLoader.addURL" [url] (. clojure.lang.RT addURL url))
//---
(function __clojure_fn_7053(){
return (clojure.JS.def(clojure,"add_classpath",(function __clojure_fn_7053_add_classpath_7055(url_1){
return (clojure.lang.RT.addURL(url_1))})))}).apply(null,[]);

//======
//(defn hash "Returns the hash code of its argument" [x] (. clojure.lang.Util (hash x)))
//---
(function __clojure_fn_7065(){
return (clojure.JS.def(clojure,"hash",(function __clojure_fn_7065_hash_7067(x_1){
return (clojure.lang.Util.hash(x_1))})))}).apply(null,[]);

//======
//(defn interpose "Returns a lazy seq of the elements of coll separated by sep" [sep coll] (drop 1 (interleave (repeat sep) coll)))
//---
(function __clojure_fn_7077(){
return (clojure.JS.def(clojure,"interpose",(function __clojure_fn_7077_interpose_7079(sep_1,coll_2){
return (clojure.drop.apply(null,[1,clojure.interleave.apply(null,[clojure.repeat.apply(null,[sep_1]),coll_2])]))})))}).apply(null,[]);

//======
//(defn partition "Returns a lazy sequence of lists of n items each, at offsets step\n  apart. If step is not supplied, defaults to n, i.e. the partitions\n  do not overlap." ([n coll] (partition n n coll)) ([n step coll] (when (seq coll) (let [p (take n coll)] (when (= n (count p)) (lazy-cons p (partition n step (drop step coll))))))))
//---
(function __clojure_fn_7095(){
return (clojure.JS.def(clojure,"partition",(function __clojure_fn_7095_partition_7097(n_1,step_2,coll_3){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (clojure.partition.apply(null,[n_1,n_1,coll_2]))}
var p_4;
return (((clojure.seq.apply(null,[coll_3]))?(((p_4=clojure.take.apply(null,[n_1,coll_3])),
((clojure.lang.Util.equal(n_1,clojure.count.apply(null,[p_4])))?((new clojure.lang.LazyCons((function __clojure_fn_7095_partition_7097_fn_7101(G__7100_1){switch(arguments.length){
case 0:return (p_4)}
return (clojure.partition.apply(null,[n_1,step_2,clojure.drop.apply(null,[step_2,coll_3])]))})))):(null)))):(null)))})))}).apply(null,[]);

//======
//(defn empty "Returns an empty collection of the same category as coll, or nil" [coll] (.empty coll))
//---
(function __clojure_fn_7121(){
return (clojure.JS.def(clojure,"empty",(function __clojure_fn_7121_empty_7123(coll_1){
return ((coll_1).empty())})))}).apply(null,[]);

//======
//(defn float-array "Creates an array of floats" {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/float_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers float_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers float_array size init-val-or-seq)))
//---
(function __clojure_fn_7151(){
return (clojure.JS.def(clojure,"float_array",(function __clojure_fn_7151_float_array_7156(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.float_array(size_or_seq_1))}
return (clojure.lang.Numbers.float_array(size_1,init_val_or_seq_2))})))}).apply(null,[]);

//======
//(defn double-array "Creates an array of doubles" {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/double_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers double_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers double_array size init-val-or-seq)))
//---
(function __clojure_fn_7171(){
return (clojure.JS.def(clojure,"double_array",(function __clojure_fn_7171_double_array_7176(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.double_array(size_or_seq_1))}
return (clojure.lang.Numbers.double_array(size_1,init_val_or_seq_2))})))}).apply(null,[]);

//======
//(defn int-array "Creates an array of ints" {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/int_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers int_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers int_array size init-val-or-seq)))
//---
(function __clojure_fn_7191(){
return (clojure.JS.def(clojure,"int_array",(function __clojure_fn_7191_int_array_7196(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.int_array(size_or_seq_1))}
return (clojure.lang.Numbers.int_array(size_1,init_val_or_seq_2))})))}).apply(null,[]);

//======
//(defn long-array "Creates an array of ints" {:inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/long_array)) args)), :inline-arities #{1 2}} ([size-or-seq] (. clojure.lang.Numbers long_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers long_array size init-val-or-seq)))
//---
(function __clojure_fn_7211(){
return (clojure.JS.def(clojure,"long_array",(function __clojure_fn_7211_long_array_7216(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.long_array(size_or_seq_1))}
return (clojure.lang.Numbers.long_array(size_1,init_val_or_seq_2))})))}).apply(null,[]);

//======
//(import (quote (java.util.concurrent BlockingQueue LinkedBlockingQueue)))
//---
(function __clojure_fn_7272(){
return (clojure.import_.apply(null,[clojure.JS.lit_list(["'java.util.concurrent","'BlockingQueue","'LinkedBlockingQueue"])]))}).apply(null,[]);

//======
//(defn seque "Creates a queued seq on another (presumably lazy) seq s. The queued\n  seq will produce a concrete seq in the background, and can get up to\n  n items ahead of the consumer. n-or-q can be an integer n buffer\n  size, or an instance of java.util.concurrent BlockingQueue. Note\n  that reading from a seque can block if the reader gets ahead of the\n  producer." ([s] (seque 100 s)) ([n-or-q s] (let [q (if (instance? BlockingQueue n-or-q) n-or-q (LinkedBlockingQueue. (int n-or-q))) NIL (Object.) agt (agent (seq s)) fill (fn [s] (try (loop [[x & xs :as s] s] (if s (if (.offer q (if (nil? x) NIL x)) (recur xs) s) (.put q q))) (catch Exception e (.put q q) (throw e)))) drain (fn drain [] (let [x (.take q)] (if (identical? x q) (clojure/deref agt) (do (send-off agt fill) (lazy-cons (if (identical? x NIL) nil x) (drain))))))] (send-off agt fill) (drain))))
//---
(function __clojure_fn_7296(){
return (clojure.JS.def(clojure,"seque",(function __clojure_fn_7296_seque_7298(n_or_q_1,s_2){switch(arguments.length){
case 1:var s_1=arguments[0];
return (clojure.seque.apply(null,[100,s_1]))}
var NIL_4,fill_6,agt_5,q_3,drain_7;
return (((q_3=((clojure.instance_QMARK_.apply(null,[java.util.concurrent.BlockingQueue,n_or_q_1]))?(n_or_q_1):((new java.util.concurrent.LinkedBlockingQueue(clojure.lang.RT.intCast(n_or_q_1)))))),
(NIL_4=(new java.lang.Object())),
(agt_5=clojure.agent.apply(null,[clojure.seq.apply(null,[s_2])])),
(fill_6=(function __clojure_fn_7296_seque_7298_fill_7301(s_1){
var x_6,e_4,s_13,s_8,vec__7306_10,G__7304_4,x_11,G__7304_9,vec__7305_5,xs_12,xs_7;
return ((function __try(){try{var _rtn=(((G__7304_4=s_1),
(vec__7305_5=G__7304_4),
(x_6=clojure.nth.apply(null,[vec__7305_5,0,null])),
(xs_7=clojure.nthrest.apply(null,[vec__7305_5,1])),
(s_8=vec__7305_5),
((function __loop(){var _rtn,_cnt;(G__7304_9=G__7304_4);do{_cnt=0;
_rtn=((vec__7306_10=G__7304_9),
(x_11=clojure.nth.apply(null,[vec__7306_10,0,null])),
(xs_12=clojure.nthrest.apply(null,[vec__7306_10,1])),
(s_13=vec__7306_10),
((s_13)?((((q_3).offer(((clojure.nil_QMARK_.apply(null,[x_11]))?(NIL_4):(x_11))))?((_cnt=1,_rtn=[xs_12],G__7304_9=_rtn[0])):(s_13))):((q_3).put(q_3))))}while(_cnt);return _rtn;})())))}
catch(e_4){_rtn=(q_3).put(q_3),
(function __throw(){throw e_4})()}})())})),
(drain_7=(function __clojure_fn_7296_seque_7298_drain_7308(){
var x_1,drain_0=arguments.callee;
return (((x_1=(q_3).take()),
((clojure.identical_QMARK_.apply(null,[x_1,q_3]))?(clojure.deref.apply(null,[agt_5])):(clojure.send_off.apply(null,[agt_5,fill_6]),
(new clojure.lang.LazyCons((function __clojure_fn_7296_seque_7298_drain_7308_fn_7310(G__7309_1){switch(arguments.length){
case 0:return (((clojure.identical_QMARK_.apply(null,[x_1,NIL_4]))?(null):(x_1)))}
return (drain_0.apply(null,[]))})))))))})),
clojure.send_off.apply(null,[agt_5,fill_6]),
drain_7.apply(null,[])))})))}).apply(null,[]);

//======
//(defn class? "Returns true if x is an instance of Class" [x] (instance? Class x))
//---
(function __clojure_fn_7323(){
return (clojure.JS.def(clojure,"class_QMARK_",(function __clojure_fn_7323_class_QMARK_7325(x_1){
return (clojure.instance_QMARK_.apply(null,[java.lang.Class,x_1]))})))}).apply(null,[]);

//======
//(defn alter-var-root "Atomically alters the root binding of var v by applying f to its\n  current value plus any args" [v f & args] (.alterRoot v f args))
//---
(function __clojure_fn_7335(){
return (clojure.JS.def(clojure,"alter_var_root",clojure.JS.variadic(2,(function __clojure_fn_7335_alter_var_root_7337(v_1,f_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((v_1).alterRoot(f_2,args_3))}))))}).apply(null,[]);

//======
//(defn make-hierarchy "Creates a hierarchy object for use with derive, isa? etc." [] {:ancestors {}, :descendants {}, :parents {}})
//---
(function __clojure_fn_7347(){
return (clojure.JS.def(clojure,"make_hierarchy",(function __clojure_fn_7347_make_hierarchy_7349(){
return (clojure.hash_map(clojure.keyword("","ancestors"),clojure.lang.PersistentHashMap.EMPTY,clojure.keyword("","descendants"),clojure.lang.PersistentHashMap.EMPTY,clojure.keyword("","parents"),clojure.lang.PersistentHashMap.EMPTY))})))}).apply(null,[]);

//======
//(def global-hierarchy (make-hierarchy))
//---
(function __clojure_fn_7356(){
return (clojure.JS.def(clojure,"global_hierarchy",clojure.make_hierarchy.apply(null,[])))}).apply(null,[]);

//======
//(defn not-empty "If coll is empty, returns nil, else coll" [coll] (when (seq coll) coll))
//---
(function __clojure_fn_7365(){
return (clojure.JS.def(clojure,"not_empty",(function __clojure_fn_7365_not_empty_7367(coll_1){
return (((clojure.seq.apply(null,[coll_1]))?(coll_1):(null)))})))}).apply(null,[]);

//======
//(defn bases "Returns the immediate superclass and direct interfaces of c, if any" [c] (let [i (.getInterfaces c) s (.getSuperclass c)] (not-empty (if s (cons s i) i))))
//---
(function __clojure_fn_7377(){
return (clojure.JS.def(clojure,"bases",(function __clojure_fn_7377_bases_7379(c_1){
var s_3,i_2;
return (((i_2=(c_1).getInterfaces()),
(s_3=(c_1).getSuperclass()),
clojure.not_empty.apply(null,[((s_3)?(clojure.cons.apply(null,[s_3,i_2])):(i_2))])))})))}).apply(null,[]);

//======
//(defn supers "Returns the immediate and indirect superclasses and interfaces of c, if any" [class] (loop [ret #{} c class] (if c (recur (into ret (bases c)) (.getSuperclass c)) (not-empty ret))))
//---
(function __clojure_fn_7389(){
return (clojure.JS.def(clojure,"supers",(function __clojure_fn_7389_supers_7391(class_1){
var ret_2,c_3;
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentHashSet.EMPTY),
(c_3=class_1);do{_cnt=0;
_rtn=((c_3)?((_cnt=1,_rtn=[clojure.into.apply(null,[ret_2,clojure.bases.apply(null,[c_3])]),(c_3).getSuperclass()],ret_2=_rtn[0],c_3=_rtn[1])):(clojure.not_empty.apply(null,[ret_2])))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn isa? "Returns true if (= child parent), or child is directly or indirectly derived from\n  parent, either via a Java type inheritance relationship or a\n  relationship established via derive. h must be a hierarchy obtained\n  from make-hierarchy, if not supplied defaults to the global\n  hierarchy" ([child parent] (isa? global-hierarchy child parent)) ([h child parent] (or (= child parent) (and (class? parent) (class? child) (. parent isAssignableFrom child)) (contains? ((:ancestors h) child) parent) (and (class? child) (some (fn* [p1__7395] (contains? ((:ancestors h) p1__7395) parent)) (supers child))) (and (vector? parent) (vector? child) (= (count parent) (count child)) (loop [ret true i 0] (if (= i (count parent)) ret (recur (and (isa? (child i) (parent i)) ret) (inc i))))))))
//---
(function __clojure_fn_7406(){
return (clojure.JS.def(clojure,"isa_QMARK_",(function __clojure_fn_7406_isa_QMARK_7408(h_1,child_2,parent_3){switch(arguments.length){
case 2:var child_1=arguments[0],parent_2=arguments[1];
return (clojure.isa_QMARK_.apply(null,[clojure.global_hierarchy,child_1,parent_2]))}
var and__196_5,and__196_8,and__196_7,and__196_10,and__196_9,or__202_7,and__196_6,ret_11,or__202_5,or__202_4,or__202_6,and__196_13,i_12;
return (((or__202_4=clojure.lang.Util.equal(child_2,parent_3)),
((or__202_4)?(or__202_4):(((or__202_5=((and__196_5=clojure.class_QMARK_.apply(null,[parent_3])),
((and__196_5)?(((and__196_6=clojure.class_QMARK_.apply(null,[child_2])),
((and__196_6)?((parent_3).isAssignableFrom(child_2)):(and__196_6)))):(and__196_5)))),
((or__202_5)?(or__202_5):(((or__202_6=clojure.contains_QMARK_.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]).apply(null,[child_2]),parent_3])),
((or__202_6)?(or__202_6):(((or__202_7=((and__196_7=clojure.class_QMARK_.apply(null,[child_2])),
((and__196_7)?(clojure.some.apply(null,[(function __clojure_fn_7406_isa_QMARK_7408_fn_7411(p1__7395_1){
return (clojure.contains_QMARK_.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]).apply(null,[p1__7395_1]),parent_3]))}),clojure.supers.apply(null,[child_2])])):(and__196_7)))),
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
(function __clojure_fn_7423(){
return (clojure.JS.def(clojure,"parents",(function __clojure_fn_7423_parents_7425(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.parents.apply(null,[clojure.global_hierarchy,tag_1]))}
var tp_3;
return (clojure.not_empty.apply(null,[((tp_3=clojure.get.apply(null,[clojure.keyword("","parents").apply(null,[h_1]),tag_2])),
((clojure.class_QMARK_.apply(null,[tag_2]))?(clojure.into.apply(null,[clojure.set.apply(null,[clojure.bases.apply(null,[tag_2])]),tp_3])):(tp_3)))]))})))}).apply(null,[]);

//======
//(defn ancestors "Returns the immediate and indirect parents of tag, either via a Java type\n  inheritance relationship or a relationship established via derive. h\n  must be a hierarchy obtained from make-hierarchy, if not supplied\n  defaults to the global hierarchy" ([tag] (ancestors global-hierarchy tag)) ([h tag] (not-empty (let [ta (get (:ancestors h) tag)] (if (class? tag) (into (set (supers tag)) ta) ta)))))
//---
(function __clojure_fn_7437(){
return (clojure.JS.def(clojure,"ancestors",(function __clojure_fn_7437_ancestors_7439(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.ancestors.apply(null,[clojure.global_hierarchy,tag_1]))}
var ta_3;
return (clojure.not_empty.apply(null,[((ta_3=clojure.get.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]),tag_2])),
((clojure.class_QMARK_.apply(null,[tag_2]))?(clojure.into.apply(null,[clojure.set.apply(null,[clojure.supers.apply(null,[tag_2])]),ta_3])):(ta_3)))]))})))}).apply(null,[]);

//======
//(defn descendants "Returns the immediate and indirect children of tag, through a\n  relationship established via derive. h must be a hierarchy obtained\n  from make-hierarchy, if not supplied defaults to the global\n  hierarchy. Note: does not work on Java type inheritance\n  relationships." ([tag] (descendants global-hierarchy tag)) ([h tag] (if (class? tag) (throw (java.lang.UnsupportedOperationException. "Can't get descendants of classes")) (not-empty (get (:descendants h) tag)))))
//---
(function __clojure_fn_7451(){
return (clojure.JS.def(clojure,"descendants",(function __clojure_fn_7451_descendants_7453(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.descendants.apply(null,[clojure.global_hierarchy,tag_1]))}
return (((clojure.class_QMARK_.apply(null,[tag_2]))?((function __throw(){throw (new java.lang.UnsupportedOperationException("Can't get descendants of classes"))})()):(clojure.not_empty.apply(null,[clojure.get.apply(null,[clojure.keyword("","descendants").apply(null,[h_1]),tag_2])]))))})))}).apply(null,[]);

//======
//(defn derive "Establishes a parent/child relationship between parent and\n  tag. Parent must be a namespace-qualified symbol or keyword and\n  child can be either a namespace-qualified symbol or keyword or a\n  class. h must be a hierarchy obtained from make-hierarchy, if not\n  supplied defaults to, and modifies, the global hierarchy." ([tag parent] (alter-var-root (var global-hierarchy) derive tag parent) nil) ([h tag parent] (assert (not= tag parent)) (assert (or (class? tag) (and (instance? clojure.lang.Named tag) (namespace tag)))) (assert (instance? clojure.lang.Named parent)) (assert (namespace parent)) (let [tp (:parents h) td (:descendants h) ta (:ancestors h) tf (fn [m source sources target targets] (reduce (fn [ret k] (assoc ret k (reduce conj (get targets k #{}) (cons target (targets target))))) m (cons source (sources source))))] (or (when-not (contains? (tp tag) parent) (when (contains? (ta tag) parent) (throw (Exception. (print-str tag "already has" parent "as ancestor")))) (when (contains? (ta parent) tag) (throw (Exception. (print-str "Cyclic derivation:" parent "has" tag "as ancestor")))) {:ancestors (tf (:ancestors h) tag td parent ta), :descendants (tf (:descendants h) parent ta tag td), :parents (assoc (:parents h) tag (conj (get tp tag #{}) parent))}) h))))
//---
(function __clojure_fn_7471(){
return (clojure.JS.def(clojure,"derive",(function __clojure_fn_7471_derive_7473(h_1,tag_2,parent_3){switch(arguments.length){
case 2:var tag_1=arguments[0],parent_2=arguments[1];
return (clojure.alter_var_root.apply(null,[clojure._var_global_hierarchy,clojure.derive,tag_1,parent_2]),
null)}
var td_5,tp_4,ta_6,or__202_4,or__202_8,and__196_5,tf_7;
return (((clojure.not_EQ_.apply(null,[tag_2,parent_3]))?(null):((function __throw(){throw (new java.lang.Exception(clojure.str.apply(null,["Assert failed: ",clojure.pr_str.apply(null,[clojure.JS.lit_list(["'not=","'tag","'parent"])])])))})())),
((((or__202_4=clojure.class_QMARK_.apply(null,[tag_2])),
((or__202_4)?(or__202_4):(((and__196_5=clojure.instance_QMARK_.apply(null,[clojure.lang.Named,tag_2])),
((and__196_5)?(clojure.namespace.apply(null,[tag_2])):(and__196_5)))))))?(null):((function __throw(){throw (new java.lang.Exception(clojure.str.apply(null,["Assert failed: ",clojure.pr_str.apply(null,[clojure.JS.lit_list(["'or",clojure.JS.lit_list(["'class?","'tag"]),clojure.JS.lit_list(["'and",clojure.JS.lit_list(["'instance?","'clojure.lang.Named","'tag"]),clojure.JS.lit_list(["'namespace","'tag"])])])])])))})())),
((clojure.instance_QMARK_.apply(null,[clojure.lang.Named,parent_3]))?(null):((function __throw(){throw (new java.lang.Exception(clojure.str.apply(null,["Assert failed: ",clojure.pr_str.apply(null,[clojure.JS.lit_list(["'instance?","'clojure.lang.Named","'parent"])])])))})())),
((clojure.namespace.apply(null,[parent_3]))?(null):((function __throw(){throw (new java.lang.Exception(clojure.str.apply(null,["Assert failed: ",clojure.pr_str.apply(null,[clojure.JS.lit_list(["'namespace","'parent"])])])))})())),
((tp_4=clojure.keyword("","parents").apply(null,[h_1])),
(td_5=clojure.keyword("","descendants").apply(null,[h_1])),
(ta_6=clojure.keyword("","ancestors").apply(null,[h_1])),
(tf_7=(function __clojure_fn_7471_derive_7473_tf_7476(m_1,source_2,sources_3,target_4,targets_5){
return (clojure.reduce.apply(null,[(function __clojure_fn_7471_derive_7473_tf_7476_fn_7478(ret_1,k_2){
return (clojure.assoc.apply(null,[ret_1,k_2,clojure.reduce.apply(null,[clojure.conj,clojure.get.apply(null,[targets_5,k_2,clojure.lang.PersistentHashSet.EMPTY]),clojure.cons.apply(null,[target_4,targets_5.apply(null,[target_4])])])]))}),m_1,clojure.cons.apply(null,[source_2,sources_3.apply(null,[source_2])])]))})),
((or__202_8=((clojure.contains_QMARK_.apply(null,[tp_4.apply(null,[tag_2]),parent_3]))?(null):(((clojure.contains_QMARK_.apply(null,[ta_6.apply(null,[tag_2]),parent_3]))?((function __throw(){throw (new java.lang.Exception(clojure.print_str.apply(null,[tag_2,"already has",parent_3,"as ancestor"])))})()):(null)),
((clojure.contains_QMARK_.apply(null,[ta_6.apply(null,[parent_3]),tag_2]))?((function __throw(){throw (new java.lang.Exception(clojure.print_str.apply(null,["Cyclic derivation:",parent_3,"has",tag_2,"as ancestor"])))})()):(null)),
clojure.hash_map(clojure.keyword("","ancestors"),tf_7.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]),tag_2,td_5,parent_3,ta_6]),clojure.keyword("","descendants"),tf_7.apply(null,[clojure.keyword("","descendants").apply(null,[h_1]),parent_3,ta_6,tag_2,td_5]),clojure.keyword("","parents"),clojure.assoc.apply(null,[clojure.keyword("","parents").apply(null,[h_1]),tag_2,clojure.conj.apply(null,[clojure.get.apply(null,[tp_4,tag_2,clojure.lang.PersistentHashSet.EMPTY]),parent_3])]))))),
((or__202_8)?(or__202_8):(h_1)))))})))}).apply(null,[]);

//======
//(defn underive "Removes a parent/child relationship between parent and\n  tag. h must be a hierarchy obtained from make-hierarchy, if not\n  supplied defaults to, and modifies, the global hierarchy." ([tag parent] (alter-var-root (var global-hierarchy) underive tag parent) nil) ([h tag parent] (let [tp (:parents h) td (:descendants h) ta (:ancestors h) tf (fn [m source sources target targets] (reduce (fn [ret k] (assoc ret k (reduce disj (get targets k) (cons target (targets target))))) m (cons source (sources source))))] (if (contains? (tp tag) parent) {:parent (assoc (:parents h) tag (disj (get tp tag) parent)), :ancestors (tf (:ancestors h) tag td parent ta), :descendants (tf (:descendants h) parent ta tag td)} h))))
//---
(function __clojure_fn_7497(){
return (clojure.JS.def(clojure,"underive",(function __clojure_fn_7497_underive_7499(h_1,tag_2,parent_3){switch(arguments.length){
case 2:var tag_1=arguments[0],parent_2=arguments[1];
return (clojure.alter_var_root.apply(null,[clojure._var_global_hierarchy,clojure.underive,tag_1,parent_2]),
null)}
var tp_4,td_5,tf_7,ta_6;
return (((tp_4=clojure.keyword("","parents").apply(null,[h_1])),
(td_5=clojure.keyword("","descendants").apply(null,[h_1])),
(ta_6=clojure.keyword("","ancestors").apply(null,[h_1])),
(tf_7=(function __clojure_fn_7497_underive_7499_tf_7502(m_1,source_2,sources_3,target_4,targets_5){
return (clojure.reduce.apply(null,[(function __clojure_fn_7497_underive_7499_tf_7502_fn_7504(ret_1,k_2){
return (clojure.assoc.apply(null,[ret_1,k_2,clojure.reduce.apply(null,[clojure.disj,clojure.get.apply(null,[targets_5,k_2]),clojure.cons.apply(null,[target_4,targets_5.apply(null,[target_4])])])]))}),m_1,clojure.cons.apply(null,[source_2,sources_3.apply(null,[source_2])])]))})),
((clojure.contains_QMARK_.apply(null,[tp_4.apply(null,[tag_2]),parent_3]))?(clojure.hash_map(clojure.keyword("","parent"),clojure.assoc.apply(null,[clojure.keyword("","parents").apply(null,[h_1]),tag_2,clojure.disj.apply(null,[clojure.get.apply(null,[tp_4,tag_2]),parent_3])]),clojure.keyword("","ancestors"),tf_7.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]),tag_2,td_5,parent_3,ta_6]),clojure.keyword("","descendants"),tf_7.apply(null,[clojure.keyword("","descendants").apply(null,[h_1]),parent_3,ta_6,tag_2,td_5]))):(h_1))))})))}).apply(null,[]);

//======
//(defn distinct? "Returns true if no two of the arguments are equal" {:tag Boolean} ([x] true) ([x y] (not (= x y))) ([x y & more] (if (not= x y) (loop [s #{y x} [x & etc :as xs] more] (if xs (if (contains? s x) false (recur (conj s x) etc)) true)) false)))
//---
(function __clojure_fn_7522(){
return (clojure.JS.def(clojure,"distinct_QMARK_",clojure.JS.variadic(2,(function __clojure_fn_7522_distinct_QMARK_7524(x_1,y_2){switch(arguments.length){
case 2:return (clojure.not.apply(null,[clojure.lang.Util.equal(x_1,y_2)]))
case 1:return (true)}
var etc_8,s_4,etc_15,x_14,G__7529_11,more_3,xs_9,vec__7531_13,x_7,vec__7530_6,s_12,xs_16,s_10,G__7529_5,more_3=clojure.JS.rest_args(this,arguments,2);
return (((clojure.not_EQ_.apply(null,[x_1,y_2]))?(((s_4=clojure.hash_set(y_2,x_1)),
(G__7529_5=more_3),
(vec__7530_6=G__7529_5),
(x_7=clojure.nth.apply(null,[vec__7530_6,0,null])),
(etc_8=clojure.nthrest.apply(null,[vec__7530_6,1])),
(xs_9=vec__7530_6),
((function __loop(){var _rtn,_cnt;(s_10=s_4),
(G__7529_11=G__7529_5);do{_cnt=0;
_rtn=((s_12=s_10),
(vec__7531_13=G__7529_11),
(x_14=clojure.nth.apply(null,[vec__7531_13,0,null])),
(etc_15=clojure.nthrest.apply(null,[vec__7531_13,1])),
(xs_16=vec__7531_13),
((xs_16)?(((clojure.contains_QMARK_.apply(null,[s_12,x_14]))?(false):((_cnt=1,_rtn=[clojure.conj.apply(null,[s_12,x_14]),etc_15],s_10=_rtn[0],G__7529_11=_rtn[1])))):(true)))}while(_cnt);return _rtn;})()))):(false)))}))))}).apply(null,[]);

//======
//(defn iterator-seq "Returns a seq on a java.util.Iterator. Note that most collections\n  providing iterators implement Iterable and thus support seq directly." [iter] (clojure.lang.IteratorSeq/create iter))
//---
(function __clojure_fn_7540(){
return (clojure.JS.def(clojure,"iterator_seq",(function __clojure_fn_7540_iterator_seq_7542(iter_1){
return (clojure.lang.IteratorSeq.create(iter_1))})))}).apply(null,[]);

//======
//(defn enumeration-seq "Returns a seq on a java.lang.Enumeration" [e] (clojure.lang.EnumerationSeq/create e))
//---
(function __clojure_fn_7552(){
return (clojure.JS.def(clojure,"enumeration_seq",(function __clojure_fn_7552_enumeration_seq_7554(e_1){
return (clojure.lang.EnumerationSeq.create(e_1))})))}).apply(null,[]);

//======
//(defn format "Formats a string using java.lang.String.format, see java.util.Formatter for format\n  string syntax" [fmt & args] (String/format fmt (to-array args)))
//---
(function __clojure_fn_7564(){
return (clojure.JS.def(clojure,"format",clojure.JS.variadic(1,(function __clojure_fn_7564_format_7566(fmt_1){
var args_2,args_2=clojure.JS.rest_args(this,arguments,1);
return (java.lang.String.format(fmt_1,clojure.to_array.apply(null,[args_2])))}))))}).apply(null,[]);

//======
//(defn printf "Prints formatted output, as per format" [fmt & args] (print (apply format fmt args)))
//---
(function __clojure_fn_7576(){
return (clojure.JS.def(clojure,"printf",clojure.JS.variadic(1,(function __clojure_fn_7576_printf_7578(fmt_1){
var args_2,args_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.print.apply(null,[clojure.apply.apply(null,[clojure.format,fmt_1,args_2])]))}))))}).apply(null,[]);

//======
//(defonce *loaded-libs* (ref (sorted-set)))
//---
(function __clojure_fn_7617(){
var v__1689_1;
return (((v__1689_1=clojure.JS.def(clojure,"_STAR_loaded_libs_STAR_",null)),
(((v__1689_1).hasRoot())?(null):(clojure.JS.def(clojure,"_STAR_loaded_libs_STAR_",clojure.ref.apply(null,[clojure.sorted_set.apply(null,[])]))))))}).apply(null,[]);

//======
//(defonce *pending-paths* #{})
//---
(function __clojure_fn_7623(){
var v__1689_1;
return (((v__1689_1=clojure.JS.def(clojure,"_STAR_pending_paths_STAR_",null)),
(((v__1689_1).hasRoot())?(null):(clojure.JS.def(clojure,"_STAR_pending_paths_STAR_",clojure.lang.PersistentHashSet.EMPTY)))))}).apply(null,[]);

//======
//(defonce *loading-verbosely* false)
//---
(function __clojure_fn_7629(){
var v__1689_1;
return (((v__1689_1=clojure.JS.def(clojure,"_STAR_loading_verbosely_STAR_",null)),
(((v__1689_1).hasRoot())?(null):(clojure.JS.def(clojure,"_STAR_loading_verbosely_STAR_",false)))))}).apply(null,[]);

//======
//(defn- throw-if "Throws an exception with a message if pred is true" [pred fmt & args] (when pred (let [message (apply format fmt args) exception (Exception. message) raw-trace (.getStackTrace exception) boring? (fn* [p1__7632] (not= (.getMethodName p1__7632) "doInvoke")) trace (into-array (drop 2 (drop-while boring? raw-trace)))] (.setStackTrace exception trace) (throw exception))))
//---
(function __clojure_fn_7642(){
return (clojure.JS.def(clojure,"throw_if",clojure.JS.variadic(2,(function __clojure_fn_7642_throw_if_7644(pred_1,fmt_2){
var args_3,trace_8,raw_trace_6,exception_5,message_4,boring_QMARK__7,args_3=clojure.JS.rest_args(this,arguments,2);
return (((pred_1)?(((message_4=clojure.apply.apply(null,[clojure.format,fmt_2,args_3])),
(exception_5=(new java.lang.Exception(message_4))),
(raw_trace_6=(exception_5).getStackTrace()),
(boring_QMARK__7=(function __clojure_fn_7642_throw_if_7644_boring_QMARK_7646(p1__7632_1){
return (clojure.not_EQ_.apply(null,[(p1__7632_1).getMethodName,"doInvoke"]))})),
(trace_8=clojure.into_array.apply(null,[clojure.drop.apply(null,[2,clojure.drop_while.apply(null,[boring_QMARK__7,raw_trace_6])])])),
(exception_5).setStackTrace(trace_8),
(function __throw(){throw exception_5})())):(null)))}))))}).apply(null,[]);

//======
//(defn- libspec? "Returns true if x is a libspec" [x] (or (symbol? x) (and (vector? x) (or (nil? (second x)) (keyword? (second x))))))
//---
(function __clojure_fn_7657(){
return (clojure.JS.def(clojure,"libspec_QMARK_",(function __clojure_fn_7657_libspec_QMARK_7659(x_1){
var or__202_4,and__196_3,or__202_2;
return (((or__202_2=clojure.symbol_QMARK_.apply(null,[x_1])),
((or__202_2)?(or__202_2):(((and__196_3=clojure.vector_QMARK_.apply(null,[x_1])),
((and__196_3)?(((or__202_4=clojure.nil_QMARK_.apply(null,[clojure.second.apply(null,[x_1])])),
((or__202_4)?(or__202_4):(clojure.keyword_QMARK_.apply(null,[clojure.second.apply(null,[x_1])]))))):(and__196_3)))))))})))}).apply(null,[]);

//======
//(defn- prependss "Prepends a symbol or a seq to coll" [x coll] (if (symbol? x) (cons x coll) (concat x coll)))
//---
(function __clojure_fn_7669(){
return (clojure.JS.def(clojure,"prependss",(function __clojure_fn_7669_prependss_7671(x_1,coll_2){
return (((clojure.symbol_QMARK_.apply(null,[x_1]))?(clojure.cons.apply(null,[x_1,coll_2])):(clojure.concat.apply(null,[x_1,coll_2]))))})))}).apply(null,[]);

//======
//(defn- root-directory "Returns the root directory path for a lib" [lib] (str \/ (.. (name lib) (replace \- \_) (replace \. \/))))
//---
(function __clojure_fn_7681(){
return (clojure.JS.def(clojure,"root_directory",(function __clojure_fn_7681_root_directory_7683(lib_1){
return (clojure.str.apply(null,["/",((clojure.name.apply(null,[lib_1])).replace("-","_")).replace(".","/")]))})))}).apply(null,[]);

//======
//(defn- root-resource "Returns the root resource path for a lib" [lib] (let [d (root-directory lib) i (inc (.lastIndexOf d (int \/))) leaf (.substring d i)] (str d \/ leaf ".clj")))
//---
(function __clojure_fn_7693(){
return (clojure.JS.def(clojure,"root_resource",(function __clojure_fn_7693_root_resource_7695(lib_1){
var leaf_4,i_3,d_2;
return (((d_2=clojure.root_directory.apply(null,[lib_1])),
(i_3=clojure.lang.Numbers.inc((d_2).lastIndexOf(clojure.lang.RT.intCast("/")))),
(leaf_4=(d_2).substring(i_3)),
clojure.str.apply(null,[d_2,"/",leaf_4,".clj"])))})))}).apply(null,[]);

//======
//(def load)
//---
(function __clojure_fn_7702(){
return (clojure.JS.def(clojure,"load",null))}).apply(null,[]);

//======
//(defn- load-one "Loads a lib given its name. If need-ns, ensures that the associated\n  namespace exists after loading. If require, records the load so any\n  duplicate loads can be skipped." [lib need-ns require] (load (root-resource lib)) (throw-if (and need-ns (not (find-ns lib))) "namespace '%s' not found after loading '%s'" lib (root-resource lib)) (when require (dosync (commute *loaded-libs* conj lib))))
//---
(function __clojure_fn_7714(){
return (clojure.JS.def(clojure,"load_one",(function __clojure_fn_7714_load_one_7716(lib_1,need_ns_2,require_3){
var and__196_4;
return (clojure.load.apply(null,[clojure.root_resource.apply(null,[lib_1])]),
clojure.throw_if.apply(null,[((and__196_4=need_ns_2),
((and__196_4)?(clojure.not.apply(null,[clojure.find_ns.apply(null,[lib_1])])):(and__196_4))),"namespace '%s' not found after loading '%s'",lib_1,clojure.root_resource.apply(null,[lib_1])]),
((require_3)?(clojure.lang.LockingTransaction.runInTransaction((function __clojure_fn_7714_load_one_7716_fn_7718(){
return (clojure.commute.apply(null,[clojure._STAR_loaded_libs_STAR_,clojure.conj,lib_1]))}))):(null)))})))}).apply(null,[]);

//======
//(defn- load-all "Loads a lib given its name and forces a load of any libs it directly or\n  indirectly loads. If need-ns, ensures that the associated namespace\n  exists after loading. If require, records the load so any duplicate loads\n  can be skipped." [lib need-ns require] (dosync (commute *loaded-libs* (fn* [p1__7723 p2__7724] (reduce conj p1__7723 p2__7724)) (binding [*loaded-libs* (ref (sorted-set))] (load-one lib need-ns require) (clojure/deref *loaded-libs*)))))
//---
(function __clojure_fn_7740(){
return (clojure.JS.def(clojure,"load_all",(function __clojure_fn_7740_load_all_7742(lib_1,need_ns_2,require_3){
return (clojure.lang.LockingTransaction.runInTransaction((function __clojure_fn_7740_load_all_7742_fn_7744(){
return (clojure.commute.apply(null,[clojure._STAR_loaded_libs_STAR_,(function __clojure_fn_7740_load_all_7742_fn_7744_fn_7746(p1__7723_1,p2__7724_2){
return (clojure.reduce.apply(null,[clojure.conj,p1__7723_1,p2__7724_2]))}),clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_loaded_libs_STAR_,clojure.ref.apply(null,[clojure.sorted_set.apply(null,[])])])),
(function __clojure_fn_7740_load_all_7742_fn_7744_fn_7749(){
return ((function __try(){try{var _rtn=(clojure.load_one.apply(null,[lib_1,need_ns_2,require_3]),
clojure.deref.apply(null,[clojure._STAR_loaded_libs_STAR_]))}
finally{clojure.lang.Var.popThreadBindings()}})())}).apply(null,[])]))})))})))}).apply(null,[]);

//======
//(defn- load-lib "Loads a lib with options" [prefix lib & options] (throw-if (and prefix (pos? (.indexOf (name lib) (int \.)))) "lib names inside prefix lists must not contain periods") (let [lib (if prefix (symbol (str prefix \. lib)) lib) opts (apply hash-map options) {:keys [as reload reload-all require use verbose]} opts loaded (contains? (clojure/deref *loaded-libs*) lib) load (cond reload-all load-all (or reload (not require) (not loaded)) load-one) need-ns (or as use) filter-opts (select-keys opts (quote (:exclude :only :rename)))] (binding [*loading-verbosely* (or *loading-verbosely* verbose)] (if load (load lib need-ns require) (throw-if (and need-ns (not (find-ns lib))) "namespace '%s' not found" lib)) (when (and need-ns *loading-verbosely*) (printf "(clojure/in-ns '%s)\n" (ns-name *ns*))) (when as (when *loading-verbosely* (printf "(clojure/alias '%s '%s)\n" as lib)) (alias as lib)) (when use (when *loading-verbosely* (printf "(clojure/refer '%s" lib) (doseq opt filter-opts (printf " %s '%s" (key opt) (print-str (val opt)))) (printf ")\n")) (apply refer lib (mapcat seq filter-opts))))))
//---
(function __clojure_fn_7762(){
return (clojure.JS.def(clojure,"load_lib",clojure.JS.variadic(2,(function __clojure_fn_7762_load_lib_7764(prefix_1,lib_2){
var list__763_19,as_8,or__202_15,verbose_11,options_3,and__196_4,reload_all_10,or__202_14,use_12,filter_opts_16,opts_5,require_9,map__7766_6,and__196_19,load_14,opt_20,and__196_19,lib_4,reload_7,or__202_17,or__202_15,loaded_13,need_ns_15,options_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.throw_if.apply(null,[((and__196_4=prefix_1),
((and__196_4)?(clojure.lang.Numbers.isPos((clojure.name.apply(null,[lib_2])).indexOf(clojure.lang.RT.intCast(".")))):(and__196_4))),"lib names inside prefix lists must not contain periods"]),
((lib_4=((prefix_1)?(clojure.symbol.apply(null,[clojure.str.apply(null,[prefix_1,".",lib_2])])):(lib_2))),
(opts_5=clojure.apply.apply(null,[clojure.hash_map,options_3])),
(map__7766_6=opts_5),
(reload_7=clojure.get.apply(null,[map__7766_6,clojure.keyword("","reload")])),
(as_8=clojure.get.apply(null,[map__7766_6,clojure.keyword("","as")])),
(require_9=clojure.get.apply(null,[map__7766_6,clojure.keyword("","require")])),
(reload_all_10=clojure.get.apply(null,[map__7766_6,clojure.keyword("","reload-all")])),
(verbose_11=clojure.get.apply(null,[map__7766_6,clojure.keyword("","verbose")])),
(use_12=clojure.get.apply(null,[map__7766_6,clojure.keyword("","use")])),
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
((function __loop(){var _rtn,_cnt;(list__763_19=clojure.seq.apply(null,[filter_opts_16]));do{_cnt=0;
_rtn=((list__763_19)?(((opt_20=clojure.first.apply(null,[list__763_19])),
clojure.printf.apply(null,[" %s '%s",clojure.key.apply(null,[opt_20]),clojure.print_str.apply(null,[clojure.val.apply(null,[opt_20])])])),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__763_19])],list__763_19=_rtn[0])):(null))}while(_cnt);return _rtn;})()),
clojure.printf.apply(null,[")\n"])):(null)),
clojure.apply.apply(null,[clojure.refer,lib_4,clojure.mapcat.apply(null,[clojure.seq,filter_opts_16])])):(null)))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn- load-libs "Loads libs, interpreting libspecs, prefix lists, and flags for\n  forwarding to load-lib" [& args] (let [flags (filter keyword? args) opts (interleave flags (repeat true)) args (filter (complement keyword?) args)] (doseq arg args (if (libspec? arg) (apply load-lib nil (prependss arg opts)) (let [[prefix & args] arg] (throw-if (nil? prefix) "prefix cannot be nil") (doseq arg args (apply load-lib prefix (prependss arg opts))))))))
//---
(function __clojure_fn_7776(){
return (clojure.JS.def(clojure,"load_libs",clojure.JS.variadic(0,(function __clojure_fn_7776_load_libs_7778(){
var list__763_10,args_9,arg_11,list__763_5,arg_6,args_4,flags_2,opts_3,vec__7780_7,prefix_8,args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (((flags_2=clojure.filter.apply(null,[clojure.keyword_QMARK_,args_1])),
(opts_3=clojure.interleave.apply(null,[flags_2,clojure.repeat.apply(null,[true])])),
(args_4=clojure.filter.apply(null,[clojure.complement.apply(null,[clojure.keyword_QMARK_]),args_1])),
((function __loop(){var _rtn,_cnt;(list__763_5=clojure.seq.apply(null,[args_4]));do{_cnt=0;
_rtn=((list__763_5)?(((arg_6=clojure.first.apply(null,[list__763_5])),
((clojure.libspec_QMARK_.apply(null,[arg_6]))?(clojure.apply.apply(null,[clojure.load_lib,null,clojure.prependss.apply(null,[arg_6,opts_3])])):(((vec__7780_7=arg_6),
(prefix_8=clojure.nth.apply(null,[vec__7780_7,0,null])),
(args_9=clojure.nthrest.apply(null,[vec__7780_7,1])),
clojure.throw_if.apply(null,[clojure.nil_QMARK_.apply(null,[prefix_8]),"prefix cannot be nil"]),
((function __loop(){var _rtn,_cnt;(list__763_10=clojure.seq.apply(null,[args_9]));do{_cnt=0;
_rtn=((list__763_10)?(((arg_11=clojure.first.apply(null,[list__763_10])),
clojure.apply.apply(null,[clojure.load_lib,prefix_8,clojure.prependss.apply(null,[arg_11,opts_3])])),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__763_10])],list__763_10=_rtn[0])):(null))}while(_cnt);return _rtn;})()))))),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__763_5])],list__763_5=_rtn[0])):(null))}while(_cnt);return _rtn;})())))}))))}).apply(null,[]);

//======
//(defn require "Loads libs, skipping any that are already loaded. Each argument is\n  either a libspec that identifies a lib, a prefix list that identifies\n  multiple libs whose names share a common prefix, or a flag that modifies\n  how all the identified libs are loaded. Use :require in the ns macro \n  in preference to calling this directly.\n\n  Libs\n\n  A 'lib' is a named set of resources in classpath whose contents define a\n  library of Clojure code. Lib names are symbols and each lib is associated\n  with a Clojure namespace and a Java package that share its name. A lib's\n  name also locates its root directory within classpath using Java's\n  package name to classpath-relative path mapping. All resources in a lib\n  should be contained in the directory structure under its root directory.\n  All definitions a lib makes should be in its associated namespace.\n\n  'require loads a lib by loading its root resource. The root resource path\n  is derived from the root directory path by repeating its last component\n  and appending '.clj'. For example, the lib 'x.y.z has root directory\n  <classpath>/x/y/z; root resource <classpath>/x/y/z/z.clj. The root\n  resource should contain code to create the lib's namespace and load any\n  additional lib resources.\n\n  Libspecs\n\n  A libspec is a lib name or a vector containing a lib name followed by\n  options expressed as sequential keywords and arguments.\n\n  Recognized options: :as\n  :as takes a symbol as its argument and makes that symbol an alias to the\n    lib's namespace in the current namespace.\n\n  Prefix Lists\n\n  It's common for Clojure code to depend on several libs whose names have\n  the same prefix. When specifying libs, prefix lists can be used to reduce\n  repetition. A prefix list contains the shared prefix followed by libspecs\n  with the shared prefix removed from the lib names. After removing the\n  prefix, the names that remain must not contain any periods.\n\n  Flags\n\n  A flag is a keyword.\n  Recognized flags: :reload, :reload-all, :verbose\n  :reload forces loading of all the identified libs even if they are\n    already loaded\n  :reload-all implies :reload and also forces loading of all libs that the\n    identified libs directly or indirectly load via require or use\n  :verbose triggers printing information about each load, alias, and refer" [& args] (apply load-libs :require args))
//---
(function __clojure_fn_7789(){
return (clojure.JS.def(clojure,"require",clojure.JS.variadic(0,(function __clojure_fn_7789_require_7791(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[clojure.load_libs,clojure.keyword("","require"),args_1]))}))))}).apply(null,[]);

//======
//(defn use "Like 'require, but also refers to each lib's namespace using\n  clojure/refer. Use :use in the ns macro in preference to calling\n  this directly.\n\n  'use accepts additional options in libspecs: :exclude, :only, :rename.\n  The arguments and semantics for :exclude, :only, and :rename are the same\n  as those documented for clojure/refer." [& args] (apply load-libs :require :use args))
//---
(function __clojure_fn_7801(){
return (clojure.JS.def(clojure,"use",clojure.JS.variadic(0,(function __clojure_fn_7801_use_7803(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[clojure.load_libs,clojure.keyword("","require"),clojure.keyword("","use"),args_1]))}))))}).apply(null,[]);

//======
//(defn loaded-libs "Returns a sorted set of symbols naming the currently loaded libs" [] (clojure/deref *loaded-libs*))
//---
(function __clojure_fn_7813(){
return (clojure.JS.def(clojure,"loaded_libs",(function __clojure_fn_7813_loaded_libs_7815(){
return (clojure.deref.apply(null,[clojure._STAR_loaded_libs_STAR_]))})))}).apply(null,[]);

//======
//(defn load "Loads Clojure code from resources in classpath. A path is interpreted as\n  classpath-relative if it begins with a slash or relative to the root\n  directory for the current namespace otherwise." [& paths] (doseq path paths (let [path (if (.startsWith path "/") path (str (root-directory (ns-name *ns*)) \/ path))] (when *loading-verbosely* (printf "(clojure/load \"%s\")\n" path) (flush)) (throw-if (*pending-paths* path) "cannot load '%s' again while it is loading" path) (binding [*pending-paths* (conj *pending-paths* path)] (.loadResourceScript clojure.lang.RT (.substring path 1))))))
//---
(function __clojure_fn_7828(){
return (clojure.JS.def(clojure,"load",clojure.JS.variadic(0,(function __clojure_fn_7828_load_7830(){
var paths_1,path_4,path_3,list__763_2,paths_1=clojure.JS.rest_args(this,arguments,0);
return (((function __loop(){var _rtn,_cnt;(list__763_2=clojure.seq.apply(null,[paths_1]));do{_cnt=0;
_rtn=((list__763_2)?(((path_3=clojure.first.apply(null,[list__763_2])),
((path_4=(((path_3).startsWith("/"))?(path_3):(clojure.str.apply(null,[clojure.root_directory.apply(null,[clojure.ns_name.apply(null,[clojure._STAR_ns_STAR_])]),"/",path_3])))),
((clojure._STAR_loading_verbosely_STAR_)?(clojure.printf.apply(null,["(clojure/load \"%s\")\n",path_4]),
clojure.flush.apply(null,[])):(null)),
clojure.throw_if.apply(null,[clojure._STAR_pending_paths_STAR_.apply(null,[path_4]),"cannot load '%s' again while it is loading",path_4]),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_pending_paths_STAR_,clojure.conj.apply(null,[clojure._STAR_pending_paths_STAR_,path_4])])),
(function __clojure_fn_7828_load_7830_fn_7832(){
return ((function __try(){try{var _rtn=(clojure.lang.RT.loadResourceScript((path_4).substring(1)))}
finally{clojure.lang.Var.popThreadBindings()}})())}).apply(null,[]))),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__763_2])],list__763_2=_rtn[0])):(null))}while(_cnt);return _rtn;})()))}))))}).apply(null,[]);

//======
//(defn get-in "returns the value in a nested associative structure, where ks is a sequence of keys" [m ks] (reduce get m ks))
//---
(function __clojure_fn_7843(){
return (clojure.JS.def(clojure,"get_in",(function __clojure_fn_7843_get_in_7845(m_1,ks_2){
return (clojure.reduce.apply(null,[clojure.get,m_1,ks_2]))})))}).apply(null,[]);

//======
//(defn assoc-in "Associates a value in a nested associative structure, where ks is a\n  sequence of keys and v is the new value and returns a new nested structure.  \n  If any levels do not exist, hash-maps will be created." [m [k & ks] v] (if ks (assoc m k (assoc-in (get m k) ks v)) (assoc m k v)))
//---
(function __clojure_fn_7857(){
return (clojure.JS.def(clojure,"assoc_in",(function __clojure_fn_7857_assoc_in_7860(m_1,p__7859_2,v_3){
var ks_6,vec__7862_4,k_5;
return (((vec__7862_4=p__7859_2),
(k_5=clojure.nth.apply(null,[vec__7862_4,0,null])),
(ks_6=clojure.nthrest.apply(null,[vec__7862_4,1])),
((ks_6)?(clojure.assoc.apply(null,[m_1,k_5,clojure.assoc_in.apply(null,[clojure.get.apply(null,[m_1,k_5]),ks_6,v_3])])):(clojure.assoc.apply(null,[m_1,k_5,v_3])))))})))}).apply(null,[]);

//======
//(defn update-in "'Updates' a value in a nested associative structure, where ks is a\n  sequence of keys and f is a function that will take the old value\n  and return the new value, and returns a new nested structure.  \n  If any levels do not exist, hash-maps will be created." ([m [k & ks] f] (if ks (assoc m k (update-in (get m k) ks f)) (assoc m k (f (get m k))))))
//---
(function __clojure_fn_7873(){
return (clojure.JS.def(clojure,"update_in",(function __clojure_fn_7873_update_in_7876(m_1,p__7875_2,f_3){
var vec__7878_4,ks_6,k_5;
return (((vec__7878_4=p__7875_2),
(k_5=clojure.nth.apply(null,[vec__7878_4,0,null])),
(ks_6=clojure.nthrest.apply(null,[vec__7878_4,1])),
((ks_6)?(clojure.assoc.apply(null,[m_1,k_5,clojure.update_in.apply(null,[clojure.get.apply(null,[m_1,k_5]),ks_6,f_3])])):(clojure.assoc.apply(null,[m_1,k_5,f_3.apply(null,[clojure.get.apply(null,[m_1,k_5])])])))))})))}).apply(null,[]);

//======
//(defn empty? "Returns true if coll has no items - same as (not (seq coll)). \n  Please use the idiom (seq x) rather than (not (empty? x))" [coll] (not (seq coll)))
//---
(function __clojure_fn_7887(){
return (clojure.JS.def(clojure,"empty_QMARK_",(function __clojure_fn_7887_empty_QMARK_7889(coll_1){
return (clojure.not.apply(null,[clojure.seq.apply(null,[coll_1])]))})))}).apply(null,[]);

//======
//(defn coll? "Returns true if x implements IPersistentCollection" [x] (instance? clojure.lang.IPersistentCollection x))
//---
(function __clojure_fn_7899(){
return (clojure.JS.def(clojure,"coll_QMARK_",(function __clojure_fn_7899_coll_QMARK_7901(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentCollection,x_1]))})))}).apply(null,[]);

//======
//(defn list? "Returns true if x implements IPersistentList" [x] (instance? clojure.lang.IPersistentList x))
//---
(function __clojure_fn_7911(){
return (clojure.JS.def(clojure,"list_QMARK_",(function __clojure_fn_7911_list_QMARK_7913(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentList,x_1]))})))}).apply(null,[]);

//======
//(defn set? "Returns true if x implements IPersistentSet" [x] (instance? clojure.lang.IPersistentSet x))
//---
(function __clojure_fn_7923(){
return (clojure.JS.def(clojure,"set_QMARK_",(function __clojure_fn_7923_set_QMARK_7925(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentSet,x_1]))})))}).apply(null,[]);

//======
//(defn number? "Returns true if x is a Number" [x] (instance? Number x))
//---
(function __clojure_fn_7935(){
return (clojure.JS.def(clojure,"number_QMARK_",(function __clojure_fn_7935_number_QMARK_7937(x_1){
return (clojure.instance_QMARK_.apply(null,[java.lang.Number,x_1]))})))}).apply(null,[]);

//======
//(defn fn? "Returns true if x implements IFn. Note that many data structures \n  (e.g. sets and maps) implement IFn" [x] (instance? clojure.lang.IFn x))
//---
(function __clojure_fn_7947(){
return (clojure.JS.def(clojure,"fn_QMARK_",(function __clojure_fn_7947_fn_QMARK_7949(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IFn,x_1]))})))}).apply(null,[]);

//======
//(defn integer? "Returns true if n is an integer" [n] (or (instance? Integer n) (instance? Long n) (instance? BigInteger n)))
//---
(function __clojure_fn_7959(){
return (clojure.JS.def(clojure,"integer_QMARK_",(function __clojure_fn_7959_integer_QMARK_7961(n_1){
var or__202_2,or__202_3;
return (((or__202_2=clojure.instance_QMARK_.apply(null,[java.lang.Integer,n_1])),
((or__202_2)?(or__202_2):(((or__202_3=clojure.instance_QMARK_.apply(null,[java.lang.Long,n_1])),
((or__202_3)?(or__202_3):(clojure.instance_QMARK_.apply(null,[java.math.BigInteger,n_1]))))))))})))}).apply(null,[]);

//======
//(defn ratio? "Returns true if n is a Ratio" [n] (instance? clojure.lang.Ratio n))
//---
(function __clojure_fn_7971(){
return (clojure.JS.def(clojure,"ratio_QMARK_",(function __clojure_fn_7971_ratio_QMARK_7973(n_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Ratio,n_1]))})))}).apply(null,[]);

//======
//(defn decimal? "Returns true if n is a BigDecimal" [n] (instance? BigDecimal n))
//---
(function __clojure_fn_7983(){
return (clojure.JS.def(clojure,"decimal_QMARK_",(function __clojure_fn_7983_decimal_QMARK_7985(n_1){
return (clojure.instance_QMARK_.apply(null,[java.math.BigDecimal,n_1]))})))}).apply(null,[]);

//======
//(defn float? "Returns true if n is a floating point number" [n] (or (instance? Double n) (instance? Float n)))
//---
(function __clojure_fn_7995(){
return (clojure.JS.def(clojure,"float_QMARK_",(function __clojure_fn_7995_float_QMARK_7997(n_1){
var or__202_2;
return (((or__202_2=clojure.instance_QMARK_.apply(null,[java.lang.Double,n_1])),
((or__202_2)?(or__202_2):(clojure.instance_QMARK_.apply(null,[java.lang.Float,n_1])))))})))}).apply(null,[]);

//======
//(defn rational? [n] "Returns true if n is a rational number" (or (integer? n) (ratio? n) (decimal? n)))
//---
(function __clojure_fn_8007(){
return (clojure.JS.def(clojure,"rational_QMARK_",(function __clojure_fn_8007_rational_QMARK_8009(n_1){
var or__202_2,or__202_3;
return ("Returns true if n is a rational number",
((or__202_2=clojure.integer_QMARK_.apply(null,[n_1])),
((or__202_2)?(or__202_2):(((or__202_3=clojure.ratio_QMARK_.apply(null,[n_1])),
((or__202_3)?(or__202_3):(clojure.decimal_QMARK_.apply(null,[n_1]))))))))})))}).apply(null,[]);

//======
//(defn associative? "Returns true if coll implements Associative" [coll] (instance? clojure.lang.Associative coll))
//---
(function __clojure_fn_8019(){
return (clojure.JS.def(clojure,"associative_QMARK_",(function __clojure_fn_8019_associative_QMARK_8021(coll_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Associative,coll_1]))})))}).apply(null,[]);

//======
//(defn sequential? "Returns true if coll implements Sequential" [coll] (instance? clojure.lang.Sequential coll))
//---
(function __clojure_fn_8031(){
return (clojure.JS.def(clojure,"sequential_QMARK_",(function __clojure_fn_8031_sequential_QMARK_8033(coll_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Sequential,coll_1]))})))}).apply(null,[]);

//======
//(defn sorted? "Returns true if coll implements Sorted" [coll] (instance? clojure.lang.Sorted coll))
//---
(function __clojure_fn_8043(){
return (clojure.JS.def(clojure,"sorted_QMARK_",(function __clojure_fn_8043_sorted_QMARK_8045(coll_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Sorted,coll_1]))})))}).apply(null,[]);

//======
//(defn reversible? "Returns true if coll implements Reversible" [coll] (instance? clojure.lang.Reversible coll))
//---
(function __clojure_fn_8055(){
return (clojure.JS.def(clojure,"reversible_QMARK_",(function __clojure_fn_8055_reversible_QMARK_8057(coll_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Reversible,coll_1]))})))}).apply(null,[]);

//======
//(defn even? "Returns true if n is even, throws an exception if n is not an integer" [n] (zero? (bit-and n 1)))
//---
(function __clojure_fn_8067(){
return (clojure.JS.def(clojure,"even_QMARK_",(function __clojure_fn_8067_even_QMARK_8069(n_1){
return (clojure.lang.Numbers.isZero(clojure.bit_and.apply(null,[n_1,1])))})))}).apply(null,[]);

//======
//(defn odd? "Returns true if n is odd, throws an exception if n is not an integer" [n] (not (even? n)))
//---
(function __clojure_fn_8079(){
return (clojure.JS.def(clojure,"odd_QMARK_",(function __clojure_fn_8079_odd_QMARK_8081(n_1){
return (clojure.not.apply(null,[clojure.even_QMARK_.apply(null,[n_1])]))})))}).apply(null,[]);

//======
//(defn pmap "Like map, except f is applied in parallel. Semi-lazy in that the\n  parallel computation stays ahead of the consumption, but doesn't\n  realize the entire result unless required. Only useful for\n  computationally intensive functions where the time of f dominates\n  the coordination overhead." ([f coll] (let [n (inc (.. Runtime getRuntime availableProcessors)) agents (doall (map (fn* [p1__8085] (agent (f p1__8085))) (take n coll))) wget (fn [a] (await1 a) (clojure/deref a)) step (fn step [[x & xs :as s] [a & as :as acycle]] (if s (let [v (wget a)] (send a (fn [_] (f x))) (lazy-cons v (step xs as))) (map wget (take (count agents) acycle))))] (step (drop n coll) (cycle agents)))) ([f coll & colls] (let [step (fn step [cs] (when (every? seq cs) (lazy-cons (map first cs) (step (map rest cs)))))] (pmap (fn* [p1__8086] (apply f p1__8086)) (step (cons coll colls))))))
//---
(function __clojure_fn_8124(){
return (clojure.JS.def(clojure,"pmap",clojure.JS.variadic(2,(function __clojure_fn_8124_pmap_8126(f_1,coll_2){switch(arguments.length){
case 2:var step_6,wget_5,n_3,agents_4;
return (((n_3=clojure.lang.Numbers.inc((java.lang.Runtime.getRuntime()).availableProcessors())),
(agents_4=clojure.doall.apply(null,[clojure.map.apply(null,[(function __clojure_fn_8124_pmap_8126_fn_8128(p1__8085_1){
return (clojure.agent.apply(null,[f_1.apply(null,[p1__8085_1])]))}),clojure.take.apply(null,[n_3,coll_2])])])),
(wget_5=(function __clojure_fn_8124_pmap_8126_wget_8131(a_1){
return (clojure.await1.apply(null,[a_1]),
clojure.deref.apply(null,[a_1]))})),
(step_6=(function __clojure_fn_8124_pmap_8126_step_8136(p__8134_1,p__8135_2){
var as_9,vec__8137_3,x_4,a_8,acycle_10,xs_5,vec__8138_7,v_11,s_6,step_0=arguments.callee;
return (((vec__8137_3=p__8134_1),
(x_4=clojure.nth.apply(null,[vec__8137_3,0,null])),
(xs_5=clojure.nthrest.apply(null,[vec__8137_3,1])),
(s_6=vec__8137_3),
(vec__8138_7=p__8135_2),
(a_8=clojure.nth.apply(null,[vec__8138_7,0,null])),
(as_9=clojure.nthrest.apply(null,[vec__8138_7,1])),
(acycle_10=vec__8138_7),
((s_6)?(((v_11=wget_5.apply(null,[a_8])),
clojure.send.apply(null,[a_8,(function __clojure_fn_8124_pmap_8126_step_8136_fn_8139(__1){
return (f_1.apply(null,[x_4]))})]),
(new clojure.lang.LazyCons((function __clojure_fn_8124_pmap_8126_step_8136_fn_8143(G__8142_1){switch(arguments.length){
case 0:return (v_11)}
return (step_0.apply(null,[xs_5,as_9]))}))))):(clojure.map.apply(null,[wget_5,clojure.take.apply(null,[clojure.count.apply(null,[agents_4]),acycle_10])])))))})),
step_6.apply(null,[clojure.drop.apply(null,[n_3,coll_2]),clojure.cycle.apply(null,[agents_4])])))}
var colls_3,step_4,colls_3=clojure.JS.rest_args(this,arguments,2);
return (((step_4=(function __clojure_fn_8124_pmap_8126_step_8149(cs_1){
var step_0=arguments.callee;
return (((clojure.every_QMARK_.apply(null,[clojure.seq,cs_1]))?((new clojure.lang.LazyCons((function __clojure_fn_8124_pmap_8126_step_8149_fn_8151(G__8150_1){switch(arguments.length){
case 0:return (clojure.map.apply(null,[clojure.first,cs_1]))}
return (step_0.apply(null,[clojure.map.apply(null,[clojure.rest,cs_1])]))})))):(null)))})),
clojure.pmap.apply(null,[(function __clojure_fn_8124_pmap_8126_fn_8156(p1__8086_1){
return (clojure.apply.apply(null,[f_1,p1__8086_1]))}),step_4.apply(null,[clojure.cons.apply(null,[coll_2,colls_3])])])))}))))}).apply(null,[]);

//======
//(def *1)
//---
(function __clojure_fn_8164(){
return (clojure.JS.def(clojure,"_STAR_1",null))}).apply(null,[]);

//======
//(def *2)
//---
(function __clojure_fn_8170(){
return (clojure.JS.def(clojure,"_STAR_2",null))}).apply(null,[]);

//======
//(def *3)
//---
(function __clojure_fn_8176(){
return (clojure.JS.def(clojure,"_STAR_3",null))}).apply(null,[]);

//======
//(def *e)
//---
(function __clojure_fn_8182(){
return (clojure.JS.def(clojure,"_STAR_e",null))}).apply(null,[]);

//======
//(import (quote (java.io Writer)))
//---
(function __clojure_fn_8188(){
return (clojure.import_.apply(null,[clojure.JS.lit_list(["'java.io","'Writer"])]))}).apply(null,[]);

//======
//(defn- print-sequential [begin print-one sep end sequence w] (.write w begin) (loop [[f & r :as s] (seq sequence)] (if r (do (print-one f w) (.write w sep) (recur r)) (when s (print-one f w)))) (.write w end))
//---
(function __clojure_fn_8201(){
return (clojure.JS.def(clojure,"print_sequential",(function __clojure_fn_8201_print_sequential_8203(begin_1,print_one_2,sep_3,end_4,sequence_5,w_6){
var vec__8208_13,G__8206_7,r_15,f_9,G__8206_12,vec__8207_8,r_10,f_14,s_11,s_16;
return ((w_6).write(begin_1),
((G__8206_7=clojure.seq.apply(null,[sequence_5])),
(vec__8207_8=G__8206_7),
(f_9=clojure.nth.apply(null,[vec__8207_8,0,null])),
(r_10=clojure.nthrest.apply(null,[vec__8207_8,1])),
(s_11=vec__8207_8),
((function __loop(){var _rtn,_cnt;(G__8206_12=G__8206_7);do{_cnt=0;
_rtn=((vec__8208_13=G__8206_12),
(f_14=clojure.nth.apply(null,[vec__8208_13,0,null])),
(r_15=clojure.nthrest.apply(null,[vec__8208_13,1])),
(s_16=vec__8208_13),
((r_15)?(print_one_2.apply(null,[f_14,w_6]),
(w_6).write(sep_3),
(_cnt=1,_rtn=[r_15],G__8206_12=_rtn[0])):(((s_16)?(print_one_2.apply(null,[f_14,w_6])):(null)))))}while(_cnt);return _rtn;})())),
(w_6).write(end_4))})))}).apply(null,[]);

//======
//(defn- print-meta [o w] (when-let m (meta o) (when (and *print-meta* *print-readably* (pos? (count m))) (.write w "#^") (if (and (= (count m) 1) (:tag m)) (print-method (:tag m) w) (print-method m w)) (.write w " "))))
//---
(function __clojure_fn_8217(){
return (clojure.JS.def(clojure,"print_meta",(function __clojure_fn_8217_print_meta_8219(o_1,w_2){
var and__196_6,and__196_5,m_4,temp__1413_3,and__196_5;
return (((temp__1413_3=clojure.meta.apply(null,[o_1])),
((temp__1413_3)?(((m_4=temp__1413_3),
((((and__196_5=clojure._STAR_print_meta_STAR_),
((and__196_5)?(((and__196_6=clojure._STAR_print_readably_STAR_),
((and__196_6)?(clojure.lang.Numbers.isPos(clojure.count.apply(null,[m_4]))):(and__196_6)))):(and__196_5))))?((w_2).write("#^"),
((((and__196_5=clojure.lang.Util.equal(clojure.count.apply(null,[m_4]),1)),
((and__196_5)?(clojure.keyword("","tag").apply(null,[m_4])):(and__196_5))))?(clojure.print_method.apply(null,[clojure.keyword("","tag").apply(null,[m_4]),w_2])):(clojure.print_method.apply(null,[m_4,w_2]))),
(w_2).write(" ")):(null)))):(null))))})))}).apply(null,[]);

//======
//(defmethod print-method nil [o w] (.write w "nil"))
//---
(function __clojure_fn_8229(){
return ((clojure.print_method).addMethod(null,(function __clojure_fn_8229_fn_8231(o_1,w_2){
return ((w_2).write("nil"))})))}).apply(null,[]);

//======
//(defmethod print-method :default [o w] (print-meta o w) (.write w (str o)))
//---
(function __clojure_fn_8241(){
return ((clojure.print_method).addMethod(clojure.keyword("","default"),(function __clojure_fn_8241_fn_8243(o_1,w_2){
return (clojure.print_meta.apply(null,[o_1,w_2]),
(w_2).write(clojure.str.apply(null,[o_1])))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.ISeq [o w] (print-meta o w) (print-sequential "(" print-method " " ")" o w))
//---
(function __clojure_fn_8253(){
return ((clojure.print_method).addMethod(clojure.lang.ISeq,(function __clojure_fn_8253_fn_8255(o_1,w_2){
return (clojure.print_meta.apply(null,[o_1,w_2]),
clojure.print_sequential.apply(null,["(",clojure.print_method," ",")",o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentList [o w] (print-meta o w) (print-sequential "(" print-method " " ")" o w))
//---
(function __clojure_fn_8265(){
return ((clojure.print_method).addMethod(clojure.lang.IPersistentList,(function __clojure_fn_8265_fn_8267(o_1,w_2){
return (clojure.print_meta.apply(null,[o_1,w_2]),
clojure.print_sequential.apply(null,["(",clojure.print_method," ",")",o_1,w_2]))})))}).apply(null,[]);

//======
//(prefer-method print-method clojure.lang.IPersistentList clojure.lang.ISeq)
//---
(function __clojure_fn_8274(){
return ((clojure.print_method).preferMethod(clojure.lang.IPersistentList,clojure.lang.ISeq))}).apply(null,[]);

//======
//(def char-escape-string {\" "\\\"", \backspace "\\b", \tab "\\t", \newline "\\n", \formfeed "\\f", \return "\\r", \\ "\\\\"})
//---
(function __clojure_fn_8280(){
return (clojure.JS.def(clojure,"char_escape_string",clojure.hash_map("\"","\\\"","\b","\\b","\t","\\t","\n","\\n","\f","\\f","\r","\\r","\\","\\\\")))}).apply(null,[]);

//======
//(defmethod print-method String [s w] (if *print-readably* (do (.append w \") (dotimes n (count s) (let [c (.charAt s n) e (char-escape-string c)] (if e (.write w e) (.append w c)))) (.append w \")) (.write w s)) nil)
//---
(function __clojure_fn_8289(){
return ((clojure.print_method).addMethod(java.lang.String,(function __clojure_fn_8289_fn_8291(s_1,w_2){
var n_4,e_6,c_5,n__796_3;
return (((clojure._STAR_print_readably_STAR_)?((w_2).append("\""),
((n__796_3=clojure.lang.RT.intCast(clojure.count.apply(null,[s_1]))),
((function __loop(){var _rtn,_cnt;(n_4=clojure.lang.RT.intCast(0));do{_cnt=0;
_rtn=((clojure.lang.Numbers.lt(n_4,n__796_3))?(((c_5=(s_1).charAt(n_4)),
(e_6=clojure.char_escape_string.apply(null,[c_5])),
((e_6)?((w_2).write(e_6)):((w_2).append(c_5)))),
(_cnt=1,_rtn=[clojure.lang.Numbers.unchecked_inc(n_4)],n_4=_rtn[0])):(null))}while(_cnt);return _rtn;})())),
(w_2).append("\"")):((w_2).write(s_1))),
null)})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentVector [v w] (print-meta v w) (.append w \[) (dotimes n (count v) (print-method (nth v n) w) (when (< n (dec (count v))) (.append w \space))) (.append w \]) nil)
//---
(function __clojure_fn_8301(){
return ((clojure.print_method).addMethod(clojure.lang.IPersistentVector,(function __clojure_fn_8301_fn_8303(v_1,w_2){
var n__796_3,n_4;
return (clojure.print_meta.apply(null,[v_1,w_2]),
(w_2).append("["),
((n__796_3=clojure.lang.RT.intCast(clojure.count.apply(null,[v_1]))),
((function __loop(){var _rtn,_cnt;(n_4=clojure.lang.RT.intCast(0));do{_cnt=0;
_rtn=((clojure.lang.Numbers.lt(n_4,n__796_3))?(clojure.print_method.apply(null,[clojure.nth.apply(null,[v_1,n_4]),w_2]),
((clojure.lang.Numbers.lt(n_4,clojure.lang.Numbers.dec(clojure.count.apply(null,[v_1]))))?((w_2).append(" ")):(null)),
(_cnt=1,_rtn=[clojure.lang.Numbers.unchecked_inc(n_4)],n_4=_rtn[0])):(null))}while(_cnt);return _rtn;})())),
(w_2).append("]"),
null)})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentMap [m w] (print-meta m w) (print-sequential "{" (fn [e w] (do (print-method (key e) w) (.append w \space) (print-method (val e) w))) ", " "}" (seq m) w))
//---
(function __clojure_fn_8316(){
return ((clojure.print_method).addMethod(clojure.lang.IPersistentMap,(function __clojure_fn_8316_fn_8318(m_1,w_2){
return (clojure.print_meta.apply(null,[m_1,w_2]),
clojure.print_sequential.apply(null,["{",(function __clojure_fn_8316_fn_8318_fn_8320(e_1,w_2){
return (clojure.print_method.apply(null,[clojure.key.apply(null,[e_1]),w_2]),
(w_2).append(" "),
clojure.print_method.apply(null,[clojure.val.apply(null,[e_1]),w_2]))}),", ","}",clojure.seq.apply(null,[m_1]),w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentSet [s w] (print-meta s w) (print-sequential "#{" print-method " " "}" (seq s) w))
//---
(function __clojure_fn_8331(){
return ((clojure.print_method).addMethod(clojure.lang.IPersistentSet,(function __clojure_fn_8331_fn_8333(s_1,w_2){
return (clojure.print_meta.apply(null,[s_1,w_2]),
clojure.print_sequential.apply(null,["#{",clojure.print_method," ","}",clojure.seq.apply(null,[s_1]),w_2]))})))}).apply(null,[]);

//======
//(def char-name-string {\space "space", \backspace "backspace", \tab "tab", \newline "newline", \formfeed "formfeed", \return "return"})
//---
(function __clojure_fn_8340(){
return (clojure.JS.def(clojure,"char_name_string",clojure.hash_map(" ","space","\b","backspace","\t","tab","\n","newline","\f","formfeed","\r","return")))}).apply(null,[]);

//======
//(defmethod print-method java.lang.Character [c w] (if *print-readably* (do (.append w \\) (let [n (char-name-string c)] (if n (.write w n) (.append w c)))) (.append w c)) nil)
//---
(function __clojure_fn_8349(){
return ((clojure.print_method).addMethod(java.lang.Character,(function __clojure_fn_8349_fn_8351(c_1,w_2){
var n_3;
return (((clojure._STAR_print_readably_STAR_)?((w_2).append("\\"),
((n_3=clojure.char_name_string.apply(null,[c_1])),
((n_3)?((w_2).write(n_3)):((w_2).append(c_1))))):((w_2).append(c_1))),
null)})))}).apply(null,[]);

//======
//(defmethod print-method Class [c w] (.write w (.getName c)))
//---
(function __clojure_fn_8361(){
return ((clojure.print_method).addMethod(java.lang.Class,(function __clojure_fn_8361_fn_8363(c_1,w_2){
return ((w_2).write((c_1).getName()))})))}).apply(null,[]);

//======
//(defmethod print-method java.math.BigDecimal [b w] (.write w (str b)) (.write w "M"))
//---
(function __clojure_fn_8373(){
return ((clojure.print_method).addMethod(java.math.BigDecimal,(function __clojure_fn_8373_fn_8375(b_1,w_2){
return ((w_2).write(clojure.str.apply(null,[b_1])),
(w_2).write("M"))})))}).apply(null,[]);

//======
//(defmethod print-method java.util.regex.Pattern [p w] (.append w \#) (print-method (str p) w))
//---
(function __clojure_fn_8385(){
return ((clojure.print_method).addMethod(java.util.regex.Pattern,(function __clojure_fn_8385_fn_8387(p_1,w_2){
return ((w_2).append("#"),
clojure.print_method.apply(null,[clojure.str.apply(null,[p_1]),w_2]))})))}).apply(null,[]);
