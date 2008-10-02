
//======
//(in-ns (quote clojure))
//---
(function __tmp_fn_2665(){
return (clojure.in_ns.apply(null,["'clojure"]))}).apply(null,[]);

//======
//(def list (. clojure.lang.PersistentList creator))
//---
(function __clojure_fn_2674(){
return (clojure.JS.def(clojure,"list",clojure.lang.PersistentList.creator))}).apply(null,[]);

//======
//(def cons (fn* cons [x seq] (. clojure.lang.RT (cons x seq))))
//---
(function __clojure_fn_2682(){
return (clojure.JS.def(clojure,"cons",(function __clojure_fn_2682_cons_2684(x_1,seq_2){
var cons_0=arguments.callee;
return (clojure.lang.RT.cons(x_1,seq_2))})))}).apply(null,[]);

//======
//(def conj (fn conj ([coll x] (. clojure.lang.RT (conj coll x))) ([coll x & xs] (if xs (recur (conj coll x) (first xs) (rest xs)) (conj coll x)))))
//---
(function __clojure_fn_2717(){
return (clojure.JS.def(clojure,"conj",clojure.JS.variadic(2,(function __clojure_fn_2717_conj_2719(coll_1,x_2){switch(arguments.length){
case 2:var conj_0=arguments.callee;
return (clojure.lang.RT.conj(coll_1,x_2))}
var _cnt,_rtn,xs_3,conj_0=arguments.callee,xs_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((xs_3)?((_cnt=1,_rtn=[conj_0.apply(null,[coll_1,x_2]),clojure.first.apply(null,[xs_3]),clojure.rest.apply(null,[xs_3])],coll_1=_rtn[0],x_2=_rtn[1],xs_3=_rtn[2])):(conj_0.apply(null,[coll_1,x_2])))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(def second (fn second [x] (first (rest x))))
//---
(function __clojure_fn_2727(){
return (clojure.JS.def(clojure,"second",(function __clojure_fn_2727_second_2729(x_1){
var second_0=arguments.callee;
return (clojure.first.apply(null,[clojure.rest.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def ffirst (fn ffirst [x] (first (first x))))
//---
(function __clojure_fn_2737(){
return (clojure.JS.def(clojure,"ffirst",(function __clojure_fn_2737_ffirst_2739(x_1){
var ffirst_0=arguments.callee;
return (clojure.first.apply(null,[clojure.first.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def rfirst (fn rfirst [x] (rest (first x))))
//---
(function __clojure_fn_2747(){
return (clojure.JS.def(clojure,"rfirst",(function __clojure_fn_2747_rfirst_2749(x_1){
var rfirst_0=arguments.callee;
return (clojure.rest.apply(null,[clojure.first.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def frest (fn frest [x] (first (rest x))))
//---
(function __clojure_fn_2757(){
return (clojure.JS.def(clojure,"frest",(function __clojure_fn_2757_frest_2759(x_1){
var frest_0=arguments.callee;
return (clojure.first.apply(null,[clojure.rest.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def rrest (fn rrest [x] (rest (rest x))))
//---
(function __clojure_fn_2767(){
return (clojure.JS.def(clojure,"rrest",(function __clojure_fn_2767_rrest_2769(x_1){
var rrest_0=arguments.callee;
return (clojure.rest.apply(null,[clojure.rest.apply(null,[x_1])]))})))}).apply(null,[]);

//======
//(def seq? (fn seq? [x] (instance? clojure.lang.ISeq x)))
//---
(function __clojure_fn_2787(){
return (clojure.JS.def(clojure,"seq_QMARK_",(function __clojure_fn_2787_seq_QMARK_2789(x_1){
var seq_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_.apply(null,[clojure.lang.ISeq,x_1]))})))}).apply(null,[]);

//======
//(def map? (fn map? [x] (instance? clojure.lang.IPersistentMap x)))
//---
(function __clojure_fn_2802(){
return (clojure.JS.def(clojure,"map_QMARK_",(function __clojure_fn_2802_map_QMARK_2804(x_1){
var map_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentMap,x_1]))})))}).apply(null,[]);

//======
//(def vector? (fn vector? [x] (instance? clojure.lang.IPersistentVector x)))
//---
(function __clojure_fn_2812(){
return (clojure.JS.def(clojure,"vector_QMARK_",(function __clojure_fn_2812_vector_QMARK_2814(x_1){
var vector_QMARK__0=arguments.callee;
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentVector,x_1]))})))}).apply(null,[]);

//======
//(def sigs (fn [fdecl] (if (seq? (first fdecl)) (loop [ret [] fdecl fdecl] (if fdecl (recur (conj ret (first (first fdecl))) (rest fdecl)) (seq ret))) (list (first fdecl)))))
//---
(function __clojure_fn_2823(){
return (clojure.JS.def(clojure,"sigs",(function __clojure_fn_2823_sigs_2825(fdecl_1){
var ret_2,fdecl_3;
return (((clojure.seq_QMARK_.apply(null,[clojure.first.apply(null,[fdecl_1])]))?(((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentVector.EMPTY),
(fdecl_3=fdecl_1);do{_cnt=0;
_rtn=((fdecl_3)?((_cnt=1,_rtn=[clojure.conj.apply(null,[ret_2,clojure.first.apply(null,[clojure.first.apply(null,[fdecl_3])])]),clojure.rest.apply(null,[fdecl_3])],ret_2=_rtn[0],fdecl_3=_rtn[1])):(clojure.seq.apply(null,[ret_2])))}while(_cnt);return _rtn;})())):(clojure.list.apply(null,[clojure.first.apply(null,[fdecl_1])]))))})))}).apply(null,[]);

//======
//(def meta (fn meta [x] (if (instance? clojure.lang.IObj x) (. x (meta)))))
//---
(function __clojure_fn_2839(){
return (clojure.JS.def(clojure,"meta",(function __clojure_fn_2839_meta_2841(x_1){
var meta_0=arguments.callee;
return (((clojure.instance_QMARK_.apply(null,[clojure.lang.IObj,x_1]))?((x_1).meta()):(null)))})))}).apply(null,[]);

//======
//(def with-meta (fn with-meta [x m] (. x (withMeta m))))
//---
(function __clojure_fn_2849(){
return (clojure.JS.def(clojure,"with_meta",(function __clojure_fn_2849_with_meta_2851(x_1,m_2){
var with_meta_0=arguments.callee;
return ((x_1).withMeta(m_2))})))}).apply(null,[]);

//======
//(def last (fn last [s] (if (rest s) (recur (rest s)) (first s))))
//---
(function __clojure_fn_2859(){
return (clojure.JS.def(clojure,"last",(function __clojure_fn_2859_last_2861(s_1){
var _cnt,_rtn,last_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.rest.apply(null,[s_1]))?((_cnt=1,_rtn=[clojure.rest.apply(null,[s_1])],s_1=_rtn[0])):(clojure.first.apply(null,[s_1])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def butlast (fn butlast [s] (loop [ret [] s s] (if (rest s) (recur (conj ret (first s)) (rest s)) (seq ret)))))
//---
(function __clojure_fn_2869(){
return (clojure.JS.def(clojure,"butlast",(function __clojure_fn_2869_butlast_2871(s_1){
var s_3,ret_2,butlast_0=arguments.callee;
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentVector.EMPTY),
(s_3=s_1);do{_cnt=0;
_rtn=((clojure.rest.apply(null,[s_3]))?((_cnt=1,_rtn=[clojure.conj.apply(null,[ret_2,clojure.first.apply(null,[s_3])]),clojure.rest.apply(null,[s_3])],ret_2=_rtn[0],s_3=_rtn[1])):(clojure.seq.apply(null,[ret_2])))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn cast "Throws a ClassCastException if x is not a c, else returns x." [c x] (. c (cast x)))
//---
(function __clojure_fn_2888(){
return (clojure.JS.def(clojure,"cast",(function __clojure_fn_2888_cast_2890(c_1,x_2){
return ((c_1).cast(x_2))})))}).apply(null,[]);

//======
//(defn to-array "Returns an array of Objects containing the contents of coll, which\n  can be any Collection.  Maps to java.util.Collection.toArray()." [coll] (. clojure.lang.RT (toArray coll)))
//---
(function __clojure_fn_2900(){
return (clojure.JS.def(clojure,"to_array",(function __clojure_fn_2900_to_array_2902(coll_1){
return (clojure.lang.RT.toArray(coll_1))})))}).apply(null,[]);

//======
//(defn vector "Creates a new vector containing the args." ([] []) ([& args] (. clojure.lang.LazilyPersistentVector (create args))))
//---
(function __clojure_fn_2913(){
return (clojure.JS.def(clojure,"vector",clojure.JS.variadic(0,(function __clojure_fn_2913_vector_2915(){switch(arguments.length){
case 0:return (clojure.lang.PersistentVector.EMPTY)}
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.LazilyPersistentVector.create(args_1))}))))}).apply(null,[]);

//======
//(defn vec "Creates a new vector containing the contents of coll." ([coll] (. clojure.lang.LazilyPersistentVector (createOwning (to-array coll)))))
//---
(function __clojure_fn_2926(){
return (clojure.JS.def(clojure,"vec",(function __clojure_fn_2926_vec_2928(coll_1){
return (clojure.lang.LazilyPersistentVector.createOwning(clojure.to_array.apply(null,[coll_1])))})))}).apply(null,[]);

//======
//(defn hash-set "Returns a new hash set with supplied keys." ([] #{}) ([& keys] (. clojure.lang.PersistentHashSet (create keys))))
//---
(function __clojure_fn_2946(){
return (clojure.JS.def(clojure,"hash_set",clojure.JS.variadic(0,(function __clojure_fn_2946_hash_set_2948(){switch(arguments.length){
case 0:return (clojure.lang.PersistentHashSet.EMPTY)}
var keys_1,keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentHashSet.create(keys_1))}))))}).apply(null,[]);

//======
//(defn sorted-map "keyval => key val\n  Returns a new sorted map with supplied mappings." ([& keyvals] (. clojure.lang.PersistentTreeMap (create keyvals))))
//---
(function __clojure_fn_2959(){
return (clojure.JS.def(clojure,"sorted_map",clojure.JS.variadic(0,(function __clojure_fn_2959_sorted_map_2961(){
var keyvals_1,keyvals_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentTreeMap.create(keyvals_1))}))))}).apply(null,[]);

//======
//(defn sorted-set "Returns a new sorted set with supplied keys." ([& keys] (. clojure.lang.PersistentTreeSet (create keys))))
//---
(function __clojure_fn_2971(){
return (clojure.JS.def(clojure,"sorted_set",clojure.JS.variadic(0,(function __clojure_fn_2971_sorted_set_2973(){
var keys_1,keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentTreeSet.create(keys_1))}))))}).apply(null,[]);

//======
//(defn sorted-map-by "keyval => key val\n  Returns a new sorted map with supplied mappings, using the supplied comparator." ([comparator & keyvals] (. clojure.lang.PersistentTreeMap (create comparator keyvals))))
//---
(function __clojure_fn_2983(){
return (clojure.JS.def(clojure,"sorted_map_by",clojure.JS.variadic(1,(function __clojure_fn_2983_sorted_map_by_2985(comparator_1){
var keyvals_2,keyvals_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentTreeMap.create(comparator_1,keyvals_2))}))))}).apply(null,[]);

//======
//(defn nil? "Returns true if x is nil, false otherwise." {:tag Boolean} [x] (identical? x nil))
//---
(function __clojure_fn_3016(){
return (clojure.JS.def(clojure,"nil_QMARK_",(function __clojure_fn_3016_nil_QMARK_3018(x_1){
return (clojure.identical_QMARK_.apply(null,[x_1,null]))})))}).apply(null,[]);

//======
//(defn false? "Returns true if x is the value false, false otherwise." {:tag Boolean} [x] (identical? x false))
//---
(function __clojure_fn_3028(){
return (clojure.JS.def(clojure,"false_QMARK_",(function __clojure_fn_3028_false_QMARK_3030(x_1){
return (clojure.identical_QMARK_.apply(null,[x_1,false]))})))}).apply(null,[]);

//======
//(defn true? "Returns true if x is the value true, false otherwise." {:tag Boolean} [x] (identical? x true))
//---
(function __clojure_fn_3040(){
return (clojure.JS.def(clojure,"true_QMARK_",(function __clojure_fn_3040_true_QMARK_3042(x_1){
return (clojure.identical_QMARK_.apply(null,[x_1,true]))})))}).apply(null,[]);

//======
//(defn not "Returns true if x is logical false, false otherwise." {:tag Boolean} [x] (if x false true))
//---
(function __clojure_fn_3052(){
return (clojure.JS.def(clojure,"not",(function __clojure_fn_3052_not_3054(x_1){
return (((x_1)?(false):(true)))})))}).apply(null,[]);

//======
//(defn str "With no args, returns the empty string. With one arg x, returns\n  x.toString().  (str nil) returns the empty string. With more than\n  one arg, returns the concatenation of the str values of the args." {:tag String} ([] "") ([x] (if (nil? x) "" (. x (toString)))) ([x & ys] (loop [sb (clojure.lang.RT/makeStringBuilder (str x)) more ys] (if more (recur (. sb (append (str (first more)))) (rest more)) (str sb)))))
//---
(function __clojure_fn_3066(){
return (clojure.JS.def(clojure,"str",clojure.JS.variadic(1,(function __clojure_fn_3066_str_3068(x_1){switch(arguments.length){
case 1:return (((clojure.nil_QMARK_.apply(null,[x_1]))?(""):((x_1).toString())))
case 0:return ("")}
var more_4,sb_3,ys_2,ys_2=clojure.JS.rest_args(this,arguments,1);
return (((function __loop(){var _rtn,_cnt;(sb_3=clojure.lang.RT.makeStringBuilder(clojure.str.apply(null,[x_1]))),
(more_4=ys_2);do{_cnt=0;
_rtn=((more_4)?((_cnt=1,_rtn=[(sb_3).append(clojure.str.apply(null,[clojure.first.apply(null,[more_4])])),clojure.rest.apply(null,[more_4])],sb_3=_rtn[0],more_4=_rtn[1])):(clojure.str.apply(null,[sb_3])))}while(_cnt);return _rtn;})()))}))))}).apply(null,[]);

//======
//(defn symbol "Returns a Symbol with the given namespace and name." ([name] (. clojure.lang.Symbol (intern name))) ([ns name] (. clojure.lang.Symbol (intern ns name))))
//---
(function __clojure_fn_3081(){
return (clojure.JS.def(clojure,"symbol",(function __clojure_fn_3081_symbol_3083(ns_1,name_2){switch(arguments.length){
case 1:var name_1=arguments[0];
return (clojure.lang.Symbol.intern(name_1))}
return (clojure.lang.Symbol.intern(ns_1,name_2))})))}).apply(null,[]);

//======
//(defn keyword "Returns a Keyword with the given namespace and name.  Do not use :\n  in the keyword strings, it will be added automatically." ([name] (. clojure.lang.Keyword (intern nil name))) ([ns name] (. clojure.lang.Keyword (intern ns name))))
//---
(function __clojure_fn_3095(){
return (clojure.JS.def(clojure,"keyword",(function __clojure_fn_3095_keyword_3097(ns_1,name_2){switch(arguments.length){
case 1:var name_1=arguments[0];
return (clojure.lang.Keyword.intern(null,name_1))}
return (clojure.lang.Keyword.intern(ns_1,name_2))})))}).apply(null,[]);

//======
//(defn gensym "Returns a new symbol with a unique name. If a prefix string is\n  supplied, the name is prefix# where # is some unique number. If\n  prefix is not supplied, the prefix is 'G'." ([] (gensym "G__")) ([prefix-string] (. clojure.lang.Symbol (intern (str prefix-string (str (. clojure.lang.RT (nextID))))))))
//---
(function __clojure_fn_3109(){
return (clojure.JS.def(clojure,"gensym",(function __clojure_fn_3109_gensym_3111(prefix_string_1){switch(arguments.length){
case 0:return (clojure.gensym.apply(null,["G__"]))}
return (clojure.lang.Symbol.intern(clojure.str.apply(null,[prefix_string_1,clojure.str.apply(null,[clojure.lang.RT.nextID()])])))})))}).apply(null,[]);

//======
//(defn spread {:private true} [arglist] (cond (nil? arglist) nil (nil? (rest arglist)) (seq (first arglist)) :else (cons (first arglist) (spread (rest arglist)))))
//---
(function __clojure_fn_3128(){
return (clojure.JS.def(clojure,"spread",(function __clojure_fn_3128_spread_3130(arglist_1){
return (((clojure.nil_QMARK_.apply(null,[arglist_1]))?(null):(((clojure.nil_QMARK_.apply(null,[clojure.rest.apply(null,[arglist_1])]))?(clojure.seq.apply(null,[clojure.first.apply(null,[arglist_1])])):(((clojure.keyword("","else"))?(clojure.cons.apply(null,[clojure.first.apply(null,[arglist_1]),clojure.spread.apply(null,[clojure.rest.apply(null,[arglist_1])])])):(null)))))))})))}).apply(null,[]);

//======
//(defn list* "Creates a new list containing the item prepended to more." [item & more] (spread (cons item more)))
//---
(function __clojure_fn_3146(){
return (clojure.JS.def(clojure,"list_STAR_",clojure.JS.variadic(1,(function __clojure_fn_3146_list_STAR_3148(item_1){
var more_2,more_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.spread.apply(null,[clojure.cons.apply(null,[item_1,more_2])]))}))))}).apply(null,[]);

//======
//(defn delay? "returns true if x is a Delay created with delay" [x] (instance? clojure.lang.Delay x))
//---
(function __clojure_fn_3164(){
return (clojure.JS.def(clojure,"delay_QMARK_",(function __clojure_fn_3164_delay_QMARK_3166(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Delay,x_1]))})))}).apply(null,[]);

//======
//(defn force "If x is a Delay, returns the (possibly cached) value of its expression, else returns x" [x] (. clojure.lang.Delay (force x)))
//---
(function __clojure_fn_3176(){
return (clojure.JS.def(clojure,"force",(function __clojure_fn_3176_force_3178(x_1){
return (clojure.lang.Delay.force(x_1))})))}).apply(null,[]);

//======
//(defn fnseq "Returns a seq object whose first is first and whose rest is the\n  value produced by calling restfn with no arguments. restfn will be\n  called at most once per step in the sequence, e.g. calling rest\n  repeatedly on the head of the seq calls restfn once - the value it\n  yields is cached." [first restfn] (new clojure.lang.FnSeq first restfn))
//---
(function __clojure_fn_3188(){
return (clojure.JS.def(clojure,"fnseq",(function __clojure_fn_3188_fnseq_3190(first_1,restfn_2){
return ((new clojure.lang.FnSeq(first_1,restfn_2)))})))}).apply(null,[]);

//======
//(defn cache-seq "Given a seq s, returns a lazy seq that will touch each element of s\n  at most once, caching the results." [s] (when s (clojure.lang.CachedSeq. s)))
//---
(function __clojure_fn_3206(){
return (clojure.JS.def(clojure,"cache_seq",(function __clojure_fn_3206_cache_seq_3208(s_1){
return (((s_1)?((new clojure.lang.CachedSeq(s_1))):(null)))})))}).apply(null,[]);

//======
//(defn concat "Returns a lazy seq representing the concatenation of\tthe elements in the supplied colls." ([] nil) ([x] (seq x)) ([x y] (if (seq x) (lazy-cons (first x) (concat (rest x) y)) (seq y))) ([x y & zs] (let [cat (fn cat [xys zs] (if (seq xys) (lazy-cons (first xys) (cat (rest xys) zs)) (when zs (recur (first zs) (rest zs)))))] (cat (concat x y) zs))))
//---
(function __clojure_fn_3233(){
return (clojure.JS.def(clojure,"concat",clojure.JS.variadic(2,(function __clojure_fn_3233_concat_3235(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.seq.apply(null,[x_1]))?((new clojure.lang.LazyCons((function __clojure_fn_3233_concat_3235_fn_3240(G__3239_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[x_1]))}
return (clojure.concat.apply(null,[clojure.rest.apply(null,[x_1]),y_2]))})))):(clojure.seq.apply(null,[y_2]))))
case 1:return (clojure.seq.apply(null,[x_1]))
case 0:return (null)}
var cat_4,zs_3,zs_3=clojure.JS.rest_args(this,arguments,2);
return (((cat_4=(function __clojure_fn_3233_concat_3235_cat_3245(xys_1,zs_2){
var _cnt,_rtn,cat_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[xys_1]))?((new clojure.lang.LazyCons((function __clojure_fn_3233_concat_3235_cat_3245_fn_3247(G__3246_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[xys_1]))}
return (cat_0.apply(null,[clojure.rest.apply(null,[xys_1]),zs_2]))})))):(((zs_2)?((_cnt=1,_rtn=[clojure.first.apply(null,[zs_2]),clojure.rest.apply(null,[zs_2])],xys_1=_rtn[0],zs_2=_rtn[1])):(null))))
}while(_cnt);return _rtn;})),
cat_4.apply(null,[clojure.concat.apply(null,[x_1,y_2]),zs_3])))}))))}).apply(null,[]);

//======
//(defn = "Equality. Returns true if x equals y, false if not. Same as\n  Java x.equals(y) except it also works for nil, and compares\n  numbers in a type-independent manner.  Clojure's immutable data\n  structures define equals() (and thus =) as a value, not an identity,\n  comparison." {:tag Boolean, :inline-arities #{2}, :inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Util)) (clojure/list (quote clojure/equal)) (clojure/list x) (clojure/list y)))} ([x] true) ([x y] (. clojure.lang.Util (equal x y))) ([x y & more] (if (= x y) (if (rest more) (recur y (first more) (rest more)) (= y (first more))) false)))
//---
(function __clojure_fn_3265(){
return (clojure.JS.def(clojure,"_EQ_",clojure.JS.variadic(2,(function __clojure_fn_3265_EQ_3270(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Util.equal(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Util.equal(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Util.equal(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn not= "Same as (not (= obj1 obj2))" {:tag Boolean} ([x] false) ([x y] (not (= x y))) ([x y & more] (not (apply = x y more))))
//---
(function __clojure_fn_3284(){
return (clojure.JS.def(clojure,"not_EQ_",clojure.JS.variadic(2,(function __clojure_fn_3284_not_EQ_3286(x_1,y_2){switch(arguments.length){
case 2:return (clojure.not.apply(null,[clojure.lang.Util.equal(x_1,y_2)]))
case 1:return (false)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.not.apply(null,[clojure.apply.apply(null,[clojure._EQ_,x_1,y_2,more_3])]))}))))}).apply(null,[]);

//======
//(defn compare "Comparator. Returns 0 if x equals y, -1 if x is logically 'less\n  than' y, else 1. Same as Java x.compareTo(y) except it also works\n  for nil, and compares numbers in a type-independent manner. x must\n  implement Comparable" {:tag Integer, :inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Util)) (clojure/list (quote clojure/compare)) (clojure/list x) (clojure/list y)))} [x y] (. clojure.lang.Util (compare x y)))
//---
(function __clojure_fn_3301(){
return (clojure.JS.def(clojure,"compare",(function __clojure_fn_3301_compare_3306(x_1,y_2){
return (clojure.lang.Util.compare(x_1,y_2))})))}).apply(null,[]);

//======
//(defn reduce "f should be a function of 2 arguments. If val is not supplied,\n  returns the result of applying f to the first 2 items in coll, then\n  applying f to that result and the 3rd item, etc. If coll contains no\n  items, f must accept no arguments as well, and reduce returns the\n  result of calling f with no arguments.  If coll has only 1 item, it\n  is returned and f is not called.  If val is supplied, returns the\n  result of applying f to val and the first item in coll, then\n  applying f to that result and the 2nd item, etc. If coll contains no\n  items, returns val and f is not called." ([f coll] (let [s (seq coll)] (if s (if (instance? clojure.lang.IReduce s) (. s (reduce f)) (reduce f (first s) (rest s))) (f)))) ([f val coll] (let [s (seq coll)] (if (instance? clojure.lang.IReduce s) (. s (reduce f val)) ((fn [f val s] (if s (recur f (f val (first s)) (rest s)) val)) f val s)))))
//---
(function __clojure_fn_3338(){
return (clojure.JS.def(clojure,"reduce",(function __clojure_fn_3338_reduce_3340(f_1,val_2,coll_3){switch(arguments.length){
case 2:var s_3,coll_2=arguments[1];
return (((s_3=clojure.seq.apply(null,[coll_2])),
((s_3)?(((clojure.instance_QMARK_.apply(null,[clojure.lang.IReduce,s_3]))?((s_3).reduce(f_1)):(clojure.reduce.apply(null,[f_1,clojure.first.apply(null,[s_3]),clojure.rest.apply(null,[s_3])])))):(f_1.apply(null,[])))))}
var s_4;
return (((s_4=clojure.seq.apply(null,[coll_3])),
((clojure.instance_QMARK_.apply(null,[clojure.lang.IReduce,s_4]))?((s_4).reduce(f_1,val_2)):((function __clojure_fn_3338_reduce_3340_fn_3343(f_1,val_2,s_3){
var _cnt,_rtn;
do{_cnt=0;_rtn=((s_3)?((_cnt=1,_rtn=[f_1,f_1.apply(null,[val_2,clojure.first.apply(null,[s_3])]),clojure.rest.apply(null,[s_3])],f_1=_rtn[0],val_2=_rtn[1],s_3=_rtn[2])):(val_2))
}while(_cnt);return _rtn;}).apply(null,[f_1,val_2,s_4])))))})))}).apply(null,[]);

//======
//(defn reverse "Returns a seq of the items in coll in reverse order. Not lazy." [coll] (reduce conj nil coll))
//---
(function __clojure_fn_3354(){
return (clojure.JS.def(clojure,"reverse",(function __clojure_fn_3354_reverse_3356(coll_1){
return (clojure.reduce.apply(null,[clojure.conj,null,coll_1]))})))}).apply(null,[]);

//======
//(defn + "Returns the sum of nums. (+) returns 0." {:inline-arities #{2}, :inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/add)) (clojure/list x) (clojure/list y)))))} ([] 0) ([x] (clojure.lang.RT/numberCast x)) ([x y] (. clojure.lang.Numbers (add x y))) ([x y & more] (reduce + (+ x y) more)))
//---
(function __clojure_fn_3372(){
return (clojure.JS.def(clojure,"_PLUS_",clojure.JS.variadic(2,(function __clojure_fn_3372_PLUS_3377(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.add(x_1,y_2))
case 1:return (clojure.lang.RT.numberCast(x_1))
case 0:return (0)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure._PLUS_,clojure.lang.Numbers.add(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn * "Returns the product of nums. (*) returns 1." {:inline-arities #{2}, :inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/multiply)) (clojure/list x) (clojure/list y)))))} ([] 1) ([x] (clojure.lang.RT/numberCast x)) ([x y] (. clojure.lang.Numbers (multiply x y))) ([x y & more] (reduce * (* x y) more)))
//---
(function __clojure_fn_3396(){
return (clojure.JS.def(clojure,"_STAR_",clojure.JS.variadic(2,(function __clojure_fn_3396_STAR_3401(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.multiply(x_1,y_2))
case 1:return (clojure.lang.RT.numberCast(x_1))
case 0:return (1)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure._STAR_,clojure.lang.Numbers.multiply(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn / "If no denominators are supplied, returns 1/numerator,\n  else returns numerator divided by all of the denominators." {:inline-arities #{2}, :inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/divide)) (clojure/list x) (clojure/list y)))))} ([x] (/ 1 x)) ([x y] (. clojure.lang.Numbers (divide x y))) ([x y & more] (reduce / (/ x y) more)))
//---
(function __clojure_fn_3419(){
return (clojure.JS.def(clojure,"_SLASH_",clojure.JS.variadic(2,(function __clojure_fn_3419_SLASH_3424(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.divide(x_1,y_2))
case 1:return (clojure.lang.Numbers.divide(1,x_1))}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure._SLASH_,clojure.lang.Numbers.divide(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn - "If no ys are supplied, returns the negation of x, else subtracts\n  the ys from x and returns the result." {:inline-arities #{1 2}, :inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/minus)) args))))} ([x] (. clojure.lang.Numbers (minus x))) ([x y] (. clojure.lang.Numbers (minus x y))) ([x y & more] (reduce - (- x y) more)))
//---
(function __clojure_fn_3441(){
return (clojure.JS.def(clojure,"_",clojure.JS.variadic(2,(function __clojure_fn_3441_3446(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.minus(x_1,y_2))
case 1:return (clojure.lang.Numbers.minus(x_1))}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure._,clojure.lang.Numbers.minus(x_1,y_2),more_3]))}))))}).apply(null,[]);

//======
//(defn < "Returns non-nil if nums are in monotonically increasing order,\n  otherwise false." {:inline-arities #{2}, :inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/lt)) (clojure/list x) (clojure/list y)))))} ([x] true) ([x y] (. clojure.lang.Numbers (lt x y))) ([x y & more] (if (< x y) (if (rest more) (recur y (first more) (rest more)) (< y (first more))) false)))
//---
(function __clojure_fn_3463(){
return (clojure.JS.def(clojure,"_LT_",clojure.JS.variadic(2,(function __clojure_fn_3463_LT_3468(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.lt(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.lt(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.lt(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn <= "Returns non-nil if nums are in monotonically non-decreasing order,\n  otherwise false." {:inline-arities #{2}, :inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/lte)) (clojure/list x) (clojure/list y)))))} ([x] true) ([x y] (. clojure.lang.Numbers (lte x y))) ([x y & more] (if (<= x y) (if (rest more) (recur y (first more) (rest more)) (<= y (first more))) false)))
//---
(function __clojure_fn_3485(){
return (clojure.JS.def(clojure,"_LT__EQ_",clojure.JS.variadic(2,(function __clojure_fn_3485_LT_EQ_3490(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.lte(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.lte(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.lte(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn > "Returns non-nil if nums are in monotonically decreasing order,\n  otherwise false." {:inline-arities #{2}, :inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/gt)) (clojure/list x) (clojure/list y)))))} ([x] true) ([x y] (. clojure.lang.Numbers (gt x y))) ([x y & more] (if (> x y) (if (rest more) (recur y (first more) (rest more)) (> y (first more))) false)))
//---
(function __clojure_fn_3507(){
return (clojure.JS.def(clojure,"_GT_",clojure.JS.variadic(2,(function __clojure_fn_3507_GT_3512(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.gt(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.gt(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.gt(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn >= "Returns non-nil if nums are in monotonically non-increasing order,\n  otherwise false." {:inline-arities #{2}, :inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/gte)) (clojure/list x) (clojure/list y)))))} ([x] true) ([x y] (. clojure.lang.Numbers (gte x y))) ([x y & more] (if (>= x y) (if (rest more) (recur y (first more) (rest more)) (>= y (first more))) false)))
//---
(function __clojure_fn_3529(){
return (clojure.JS.def(clojure,"_GT__EQ_",clojure.JS.variadic(2,(function __clojure_fn_3529_GT_EQ_3534(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.gte(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.gte(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.gte(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn == "Returns non-nil if nums all have the same value, otherwise false" {:inline-arities #{2}, :inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/equiv)) (clojure/list x) (clojure/list y)))))} ([x] true) ([x y] (. clojure.lang.Numbers (equiv x y))) ([x y & more] (if (== x y) (if (rest more) (recur y (first more) (rest more)) (== y (first more))) false)))
//---
(function __clojure_fn_3551(){
return (clojure.JS.def(clojure,"_EQ__EQ_",clojure.JS.variadic(2,(function __clojure_fn_3551_EQ_EQ_3556(x_1,y_2){switch(arguments.length){
case 2:return (clojure.lang.Numbers.equiv(x_1,y_2))
case 1:return (true)}
var _cnt,_rtn,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((clojure.lang.Numbers.equiv(x_1,y_2))?(((clojure.rest.apply(null,[more_3]))?((_cnt=1,_rtn=[y_2,clojure.first.apply(null,[more_3]),clojure.rest.apply(null,[more_3])],x_1=_rtn[0],y_2=_rtn[1],more_3=_rtn[2])):(clojure.lang.Numbers.equiv(y_2,clojure.first.apply(null,[more_3]))))):(false))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn max "Returns the greatest of the nums." ([x] x) ([x y] (if (> x y) x y)) ([x y & more] (reduce max (max x y) more)))
//---
(function __clojure_fn_3570(){
return (clojure.JS.def(clojure,"max",clojure.JS.variadic(2,(function __clojure_fn_3570_max_3572(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.lang.Numbers.gt(x_1,y_2))?(x_1):(y_2)))
case 1:return (x_1)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure.max,clojure.max.apply(null,[x_1,y_2]),more_3]))}))))}).apply(null,[]);

//======
//(defn min "Returns the least of the nums." ([x] x) ([x y] (if (< x y) x y)) ([x y & more] (reduce min (min x y) more)))
//---
(function __clojure_fn_3586(){
return (clojure.JS.def(clojure,"min",clojure.JS.variadic(2,(function __clojure_fn_3586_min_3588(x_1,y_2){switch(arguments.length){
case 2:return (((clojure.lang.Numbers.lt(x_1,y_2))?(x_1):(y_2)))
case 1:return (x_1)}
var more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.reduce.apply(null,[clojure.min,clojure.min.apply(null,[x_1,y_2]),more_3]))}))))}).apply(null,[]);

//======
//(defn inc "Returns a number one greater than num." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/inc)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (inc x)))
//---
(function __clojure_fn_3603(){
return (clojure.JS.def(clojure,"inc",(function __clojure_fn_3603_inc_3608(x_1){
return (clojure.lang.Numbers.inc(x_1))})))}).apply(null,[]);

//======
//(defn dec "Returns a number one less than num." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/dec)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (dec x)))
//---
(function __clojure_fn_3621(){
return (clojure.JS.def(clojure,"dec",(function __clojure_fn_3621_dec_3626(x_1){
return (clojure.lang.Numbers.dec(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-inc "Returns a number one greater than x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_inc)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (unchecked_inc x)))
//---
(function __clojure_fn_3639(){
return (clojure.JS.def(clojure,"unchecked_inc",(function __clojure_fn_3639_unchecked_inc_3644(x_1){
return (clojure.lang.Numbers.unchecked_inc(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-dec "Returns a number one less than x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_dec)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (unchecked_dec x)))
//---
(function __clojure_fn_3657(){
return (clojure.JS.def(clojure,"unchecked_dec",(function __clojure_fn_3657_unchecked_dec_3662(x_1){
return (clojure.lang.Numbers.unchecked_dec(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-negate "Returns the negation of x, an int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_negate)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (unchecked_negate x)))
//---
(function __clojure_fn_3675(){
return (clojure.JS.def(clojure,"unchecked_negate",(function __clojure_fn_3675_unchecked_negate_3680(x_1){
return (clojure.lang.Numbers.unchecked_negate(x_1))})))}).apply(null,[]);

//======
//(defn unchecked-add "Returns the sum of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_add)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_add x y)))
//---
(function __clojure_fn_3693(){
return (clojure.JS.def(clojure,"unchecked_add",(function __clojure_fn_3693_unchecked_add_3698(x_1,y_2){
return (clojure.lang.Numbers.unchecked_add(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-subtract "Returns the difference of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_subtract)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_subtract x y)))
//---
(function __clojure_fn_3711(){
return (clojure.JS.def(clojure,"unchecked_subtract",(function __clojure_fn_3711_unchecked_subtract_3716(x_1,y_2){
return (clojure.lang.Numbers.unchecked_subtract(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-multiply "Returns the product of x and y, both int or long. \n  Note - uses a primitive operator subject to overflow." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_multiply)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_multiply x y)))
//---
(function __clojure_fn_3729(){
return (clojure.JS.def(clojure,"unchecked_multiply",(function __clojure_fn_3729_unchecked_multiply_3734(x_1,y_2){
return (clojure.lang.Numbers.unchecked_multiply(x_1,y_2))})))}).apply(null,[]);

//======
//(defn unchecked-divide "Returns the division of x by y, both int or long. \n  Note - uses a primitive operator subject to truncation." {:inline (fn [x y] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/unchecked_divide)) (clojure/list x) (clojure/list y)))))} [x y] (. clojure.lang.Numbers (unchecked_divide x y)))
//---
(function __clojure_fn_3747(){
return (clojure.JS.def(clojure,"unchecked_divide",(function __clojure_fn_3747_unchecked_divide_3752(x_1,y_2){
return (clojure.lang.Numbers.unchecked_divide(x_1,y_2))})))}).apply(null,[]);

//======
//(defn pos? "Returns true if num is greater than zero, else false" {:tag Boolean, :inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/isPos)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (isPos x)))
//---
(function __clojure_fn_3765(){
return (clojure.JS.def(clojure,"pos_QMARK_",(function __clojure_fn_3765_pos_QMARK_3770(x_1){
return (clojure.lang.Numbers.isPos(x_1))})))}).apply(null,[]);

//======
//(defn neg? "Returns true if num is less than zero, else false" {:tag Boolean, :inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/isNeg)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (isNeg x)))
//---
(function __clojure_fn_3783(){
return (clojure.JS.def(clojure,"neg_QMARK_",(function __clojure_fn_3783_neg_QMARK_3788(x_1){
return (clojure.lang.Numbers.isNeg(x_1))})))}).apply(null,[]);

//======
//(defn zero? "Returns true if num is zero, else false" {:tag Boolean, :inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/isZero)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (isZero x)))
//---
(function __clojure_fn_3801(){
return (clojure.JS.def(clojure,"zero_QMARK_",(function __clojure_fn_3801_zero_QMARK_3806(x_1){
return (clojure.lang.Numbers.isZero(x_1))})))}).apply(null,[]);

//======
//(defn quot "quot[ient] of dividing numerator by denominator." [num div] (. clojure.lang.Numbers (quotient num div)))
//---
(function __clojure_fn_3816(){
return (clojure.JS.def(clojure,"quot",(function __clojure_fn_3816_quot_3818(num_1,div_2){
return (clojure.lang.Numbers.quotient(num_1,div_2))})))}).apply(null,[]);

//======
//(defn rem "rem[ainder] of dividing numerator by denominator." [num div] (. clojure.lang.Numbers (remainder num div)))
//---
(function __clojure_fn_3828(){
return (clojure.JS.def(clojure,"rem",(function __clojure_fn_3828_rem_3830(num_1,div_2){
return (clojure.lang.Numbers.remainder(num_1,div_2))})))}).apply(null,[]);

//======
//(defn rationalize "returns the rational value of num" [num] (. clojure.lang.Numbers (rationalize num)))
//---
(function __clojure_fn_3840(){
return (clojure.JS.def(clojure,"rationalize",(function __clojure_fn_3840_rationalize_3842(num_1){
return (clojure.lang.Numbers.rationalize(num_1))})))}).apply(null,[]);

//======
//(defn bit-not "Bitwise complement" [x] (. clojure.lang.Numbers not x))
//---
(function __clojure_fn_3852(){
return (clojure.JS.def(clojure,"bit_not",(function __clojure_fn_3852_bit_not_3854(x_1){
return (clojure.lang.Numbers.not(x_1))})))}).apply(null,[]);

//======
//(defn bit-and "Bitwise and" [x y] (. clojure.lang.Numbers and x y))
//---
(function __clojure_fn_3864(){
return (clojure.JS.def(clojure,"bit_and",(function __clojure_fn_3864_bit_and_3866(x_1,y_2){
return (clojure.lang.Numbers.and(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-or "Bitwise or" [x y] (. clojure.lang.Numbers or x y))
//---
(function __clojure_fn_3876(){
return (clojure.JS.def(clojure,"bit_or",(function __clojure_fn_3876_bit_or_3878(x_1,y_2){
return (clojure.lang.Numbers.or(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-xor "Bitwise exclusive or" [x y] (. clojure.lang.Numbers xor x y))
//---
(function __clojure_fn_3888(){
return (clojure.JS.def(clojure,"bit_xor",(function __clojure_fn_3888_bit_xor_3890(x_1,y_2){
return (clojure.lang.Numbers.xor(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-and-not "Bitwise and with complement" [x y] (. clojure.lang.Numbers andNot x y))
//---
(function __clojure_fn_3900(){
return (clojure.JS.def(clojure,"bit_and_not",(function __clojure_fn_3900_bit_and_not_3902(x_1,y_2){
return (clojure.lang.Numbers.andNot(x_1,y_2))})))}).apply(null,[]);

//======
//(defn bit-clear "Clear bit at index n" [x n] (. clojure.lang.Numbers clearBit x n))
//---
(function __clojure_fn_3912(){
return (clojure.JS.def(clojure,"bit_clear",(function __clojure_fn_3912_bit_clear_3914(x_1,n_2){
return (clojure.lang.Numbers.clearBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-set "Set bit at index n" [x n] (. clojure.lang.Numbers setBit x n))
//---
(function __clojure_fn_3924(){
return (clojure.JS.def(clojure,"bit_set",(function __clojure_fn_3924_bit_set_3926(x_1,n_2){
return (clojure.lang.Numbers.setBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-flip "Flip bit at index n" [x n] (. clojure.lang.Numbers flipBit x n))
//---
(function __clojure_fn_3936(){
return (clojure.JS.def(clojure,"bit_flip",(function __clojure_fn_3936_bit_flip_3938(x_1,n_2){
return (clojure.lang.Numbers.flipBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-test "Test bit at index n" [x n] (. clojure.lang.Numbers testBit x n))
//---
(function __clojure_fn_3948(){
return (clojure.JS.def(clojure,"bit_test",(function __clojure_fn_3948_bit_test_3950(x_1,n_2){
return (clojure.lang.Numbers.testBit(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-shift-left "Bitwise shift left" [x n] (. clojure.lang.Numbers shiftLeft x n))
//---
(function __clojure_fn_3960(){
return (clojure.JS.def(clojure,"bit_shift_left",(function __clojure_fn_3960_bit_shift_left_3962(x_1,n_2){
return (clojure.lang.Numbers.shiftLeft(x_1,n_2))})))}).apply(null,[]);

//======
//(defn bit-shift-right "Bitwise shift right" [x n] (. clojure.lang.Numbers shiftRight x n))
//---
(function __clojure_fn_3972(){
return (clojure.JS.def(clojure,"bit_shift_right",(function __clojure_fn_3972_bit_shift_right_3974(x_1,n_2){
return (clojure.lang.Numbers.shiftRight(x_1,n_2))})))}).apply(null,[]);

//======
//(defn complement "Takes a fn f and returns a fn that takes the same arguments as f,\n  has the same effects, if any, and returns the opposite truth value." [f] (fn [& args] (not (apply f args))))
//---
(function __clojure_fn_3987(){
return (clojure.JS.def(clojure,"complement",(function __clojure_fn_3987_complement_3989(f_1){
return (clojure.JS.variadic(0,(function __clojure_fn_3987_complement_3989_fn_3991(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.not.apply(null,[clojure.apply.apply(null,[f_1,args_1])]))})))})))}).apply(null,[]);

//======
//(defn constantly "Returns a function that takes any number of arguments and returns x." [x] (fn [& args] x))
//---
(function __clojure_fn_4005(){
return (clojure.JS.def(clojure,"constantly",(function __clojure_fn_4005_constantly_4007(x_1){
return (clojure.JS.variadic(0,(function __clojure_fn_4005_constantly_4007_fn_4009(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (x_1)})))})))}).apply(null,[]);

//======
//(defn identity "Returns its argument." [x] x)
//---
(function __clojure_fn_4020(){
return (clojure.JS.def(clojure,"identity",(function __clojure_fn_4020_identity_4022(x_1){
return (x_1)})))}).apply(null,[]);

//======
//(defn peek "For a list or queue, same as first, for a vector, same as, but much\n  more efficient than, last. If the collection is empty, returns nil." [coll] (. clojure.lang.RT (peek coll)))
//---
(function __clojure_fn_4038(){
return (clojure.JS.def(clojure,"peek",(function __clojure_fn_4038_peek_4040(coll_1){
return (clojure.lang.RT.peek(coll_1))})))}).apply(null,[]);

//======
//(defn pop "For a list or queue, returns a new list/queue without the first\n  item, for a vector, returns a new vector without the last item. If\n  the collection is empty, throws an exception.  Note - not the same\n  as rest/butlast." [coll] (. clojure.lang.RT (pop coll)))
//---
(function __clojure_fn_4050(){
return (clojure.JS.def(clojure,"pop",(function __clojure_fn_4050_pop_4052(coll_1){
return (clojure.lang.RT.pop(coll_1))})))}).apply(null,[]);

//======
//(defn dissoc "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,\n  that does not contain a mapping for key(s)." ([map] map) ([map key] (. clojure.lang.RT (dissoc map key))) ([map key & ks] (let [ret (dissoc map key)] (if ks (recur ret (first ks) (rest ks)) ret))))
//---
(function __clojure_fn_4084(){
return (clojure.JS.def(clojure,"dissoc",clojure.JS.variadic(2,(function __clojure_fn_4084_dissoc_4086(map_1,key_2){switch(arguments.length){
case 2:return (clojure.lang.RT.dissoc(map_1,key_2))
case 1:return (map_1)}
var _cnt,_rtn,ret_4,ks_3,ks_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((ret_4=clojure.dissoc.apply(null,[map_1,key_2])),
((ks_3)?((_cnt=1,_rtn=[ret_4,clojure.first.apply(null,[ks_3]),clojure.rest.apply(null,[ks_3])],map_1=_rtn[0],key_2=_rtn[1],ks_3=_rtn[2])):(ret_4)))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn disj "disj[oin]. Returns a new set of the same (hashed/sorted) type, that\n  does not contain key(s)." ([set] set) ([set key] (. set (disjoin key))) ([set key & ks] (let [ret (disj set key)] (if ks (recur ret (first ks) (rest ks)) ret))))
//---
(function __clojure_fn_4100(){
return (clojure.JS.def(clojure,"disj",clojure.JS.variadic(2,(function __clojure_fn_4100_disj_4102(set_1,key_2){switch(arguments.length){
case 2:return ((set_1).disjoin(key_2))
case 1:return (set_1)}
var _cnt,_rtn,ret_4,ks_3,ks_3=clojure.JS.rest_args(this,arguments,2);
do{_cnt=0;_rtn=((ret_4=clojure.disj.apply(null,[set_1,key_2])),
((ks_3)?((_cnt=1,_rtn=[ret_4,clojure.first.apply(null,[ks_3]),clojure.rest.apply(null,[ks_3])],set_1=_rtn[0],key_2=_rtn[1],ks_3=_rtn[2])):(ret_4)))
}while(_cnt);return _rtn;}))))}).apply(null,[]);

//======
//(defn select-keys "Returns a map containing only those entries in map whose key is in keys" [map keyseq] (loop [ret {} keys (seq keyseq)] (if keys (let [entry (. clojure.lang.RT (find map (first keys)))] (recur (if entry (conj ret entry) ret) (rest keys))) ret)))
//---
(function __clojure_fn_4120(){
return (clojure.JS.def(clojure,"select_keys",(function __clojure_fn_4120_select_keys_4122(map_1,keyseq_2){
var entry_5,keys_4,ret_3;
return (((function __loop(){var _rtn,_cnt;(ret_3=clojure.lang.PersistentHashMap.EMPTY),
(keys_4=clojure.seq.apply(null,[keyseq_2]));do{_cnt=0;
_rtn=((keys_4)?(((entry_5=clojure.lang.RT.find(map_1,clojure.first.apply(null,[keys_4]))),
(_cnt=1,_rtn=[((entry_5)?(clojure.conj.apply(null,[ret_3,entry_5])):(ret_3)),clojure.rest.apply(null,[keys_4])],ret_3=_rtn[0],keys_4=_rtn[1]))):(ret_3))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn key "Returns the key of the map entry." [e] (. e (getKey)))
//---
(function __clojure_fn_4144(){
return (clojure.JS.def(clojure,"key",(function __clojure_fn_4144_key_4146(e_1){
return ((e_1).getKey())})))}).apply(null,[]);

//======
//(defn val "Returns the value in the map entry." [e] (. e (getValue)))
//---
(function __clojure_fn_4156(){
return (clojure.JS.def(clojure,"val",(function __clojure_fn_4156_val_4158(e_1){
return ((e_1).getValue())})))}).apply(null,[]);

//======
//(defn rseq "Returns, in constant time, a sequence of the items in rev (which\n  can be a vector or sorted-map), in reverse order." [rev] (. rev (rseq)))
//---
(function __clojure_fn_4168(){
return (clojure.JS.def(clojure,"rseq",(function __clojure_fn_4168_rseq_4170(rev_1){
return ((rev_1).rseq())})))}).apply(null,[]);

//======
//(defn name "Returns the name String of a symbol or keyword." [x] (. x (getName)))
//---
(function __clojure_fn_4180(){
return (clojure.JS.def(clojure,"name",(function __clojure_fn_4180_name_4182(x_1){
return ((x_1).getName())})))}).apply(null,[]);

//======
//(defn namespace "Returns the namespace String of a symbol or keyword, or nil if not present." [x] (. x (getNamespace)))
//---
(function __clojure_fn_4192(){
return (clojure.JS.def(clojure,"namespace",(function __clojure_fn_4192_namespace_4194(x_1){
return ((x_1).getNamespace())})))}).apply(null,[]);

//======
//(defn find-var "Returns the global var named by the namespace-qualified symbol, or\n  nil if no var with that name." [sym] (. clojure.lang.Var (find sym)))
//---
(function __clojure_fn_4259(){
return (clojure.JS.def(clojure,"find_var",(function __clojure_fn_4259_find_var_4261(sym_1){
return (clojure.lang.Var.find(sym_1))})))}).apply(null,[]);

//======
//(defn agent "Creates and returns an agent with an initial value of state and an\n  optional validate fn. validate-fn must be nil or a side-effect-free fn of\n  one argument, which will be passed the intended new state on any state\n  change. If the new state is unacceptable, the validate-fn should\n  throw an exception." ([state] (new clojure.lang.Agent state)) ([state validate-fn] (new clojure.lang.Agent state validate-fn)))
//---
(function __clojure_fn_4272(){
return (clojure.JS.def(clojure,"agent",(function __clojure_fn_4272_agent_4274(state_1,validate_fn_2){switch(arguments.length){
case 1:return ((new clojure.lang.Agent(state_1)))}
return ((new clojure.lang.Agent(state_1,validate_fn_2)))})))}).apply(null,[]);

//======
//(defn ! [& args] (throw (clojure.lang.RT/makeException "! is now send. See also send-off")))
//---
(function __clojure_fn_4285(){
return (clojure.JS.def(clojure,"_BANG_",clojure.JS.variadic(0,(function __clojure_fn_4285_BANG_4287(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw clojure.lang.RT.makeException("! is now send. See also send-off")})())}))))}).apply(null,[]);

//======
//(defn send "Dispatch an action to an agent. Returns the agent immediately.\n  Subsequently, in a thread from a thread pool, the state of the agent\n  will be set to the value of:\n\n  (apply action-fn state-of-agent args)" [a f & args] (. a (dispatch f args false)))
//---
(function __clojure_fn_4297(){
return (clojure.JS.def(clojure,"send",clojure.JS.variadic(2,(function __clojure_fn_4297_send_4299(a_1,f_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((a_1).dispatch(f_2,args_3,false))}))))}).apply(null,[]);

//======
//(defn send-off "Dispatch a potentially blocking action to an agent. Returns the\n  agent immediately. Subsequently, in a separate thread, the state of\n  the agent will be set to the value of:\n\n  (apply action-fn state-of-agent args)" [a f & args] (. a (dispatch f args true)))
//---
(function __clojure_fn_4309(){
return (clojure.JS.def(clojure,"send_off",clojure.JS.variadic(2,(function __clojure_fn_4309_send_off_4311(a_1,f_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((a_1).dispatch(f_2,args_3,true))}))))}).apply(null,[]);

//======
//(defn add-watch "Experimental.\n  Adds a watcher to an agent. Whenever the agent runs an action, any\n  registered watchers will have their callback function called.  The\n  callback fn will be passed 3 args, the watcher, the agent and a boolean\n  which will be true if the agent's state was (potentially) changed by\n  the action. The callback fn is run synchronously with the action,\n  and thus derefs of the agent in the callback will see the value set\n  during that action. Because it is run on the action thread, the\n  callback should not block, but can send messages." [a watcher callback] (.addWatch a watcher callback))
//---
(function __clojure_fn_4321(){
return (clojure.JS.def(clojure,"add_watch",(function __clojure_fn_4321_add_watch_4323(a_1,watcher_2,callback_3){
return ((a_1).addWatch(watcher_2,callback_3))})))}).apply(null,[]);

//======
//(defn remove-watch "Experimental.\n  Removes a watcher (set by add-watch) from an agent" [a watcher] (.removeWatch a watcher))
//---
(function __clojure_fn_4333(){
return (clojure.JS.def(clojure,"remove_watch",(function __clojure_fn_4333_remove_watch_4335(a_1,watcher_2){
return ((a_1).removeWatch(watcher_2))})))}).apply(null,[]);

//======
//(defn agent-errors "Returns a sequence of the exceptions thrown during asynchronous\n  actions of the agent." [a] (. a (getErrors)))
//---
(function __clojure_fn_4345(){
return (clojure.JS.def(clojure,"agent_errors",(function __clojure_fn_4345_agent_errors_4347(a_1){
return ((a_1).getErrors())})))}).apply(null,[]);

//======
//(defn clear-agent-errors "Clears any exceptions thrown during asynchronous actions of the\n  agent, allowing subsequent actions to occur." [a] (. a (clearErrors)))
//---
(function __clojure_fn_4357(){
return (clojure.JS.def(clojure,"clear_agent_errors",(function __clojure_fn_4357_clear_agent_errors_4359(a_1){
return ((a_1).clearErrors())})))}).apply(null,[]);

//======
//(defn shutdown-agents "Initiates a shutdown of the thread pools that back the agent\n  system. Running actions will complete, but no new actions will be\n  accepted" [] (. clojure.lang.Agent shutdown))
//---
(function __clojure_fn_4369(){
return (clojure.JS.def(clojure,"shutdown_agents",(function __clojure_fn_4369_shutdown_agents_4371(){
return (clojure.lang.Agent.shutdown())})))}).apply(null,[]);

//======
//(defn ref "Creates and returns a Ref with an initial value of x and an optional validate fn.\n  validate-fn must be nil or a side-effect-free fn of one argument, which will\n  be passed the intended new state on any state change. If the new\n  state is unacceptable, the validate-fn should throw an\n  exception. validate-fn will be called on transaction commit, when\n  all refs have their final values." ([x] (new clojure.lang.Ref x)) ([x validate-fn] (new clojure.lang.Ref x validate-fn)))
//---
(function __clojure_fn_4382(){
return (clojure.JS.def(clojure,"ref",(function __clojure_fn_4382_ref_4384(x_1,validate_fn_2){switch(arguments.length){
case 1:return ((new clojure.lang.Ref(x_1)))}
return ((new clojure.lang.Ref(x_1,validate_fn_2)))})))}).apply(null,[]);

//======
//(defn deref "Also reader macro: @ref/@agent Within a transaction, returns the\n  in-transaction-value of ref, else returns the\n  most-recently-committed value of ref. When applied to an agent,\n  returns its current state." [ref] (. ref (get)))
//---
(function __clojure_fn_4395(){
return (clojure.JS.def(clojure,"deref",(function __clojure_fn_4395_deref_4397(ref_1){
return ((ref_1).get())})))}).apply(null,[]);

//======
//(defn set-validator "Sets the validator-fn for a var/ref/agent. validator-fn must be nil or a\n  side-effect-free fn of one argument, which will be passed the intended\n  new state on any state change. If the new state is unacceptable, the\n  validator-fn should throw an exception. If the current state (root\n  value if var) is not acceptable to the new validator, an exception\n  will be thrown and the validator will not be changed." [iref validator-fn] (. iref (setValidator validator-fn)))
//---
(function __clojure_fn_4407(){
return (clojure.JS.def(clojure,"set_validator",(function __clojure_fn_4407_set_validator_4409(iref_1,validator_fn_2){
return ((iref_1).setValidator(validator_fn_2))})))}).apply(null,[]);

//======
//(defn get-validator "Gets the validator-fn for a var/ref/agent." [iref] (. iref (getValidator)))
//---
(function __clojure_fn_4419(){
return (clojure.JS.def(clojure,"get_validator",(function __clojure_fn_4419_get_validator_4421(iref_1){
return ((iref_1).getValidator())})))}).apply(null,[]);

//======
//(defn commute "Must be called in a transaction. Sets the in-transaction-value of\n  ref to:\n\n  (apply fun in-transaction-value-of-ref args)\n\n  and returns the in-transaction-value of ref.\n\n  At the commit point of the transaction, sets the value of ref to be:\n\n  (apply fun most-recently-committed-value-of-ref args)\n\n  Thus fun should be commutative, or, failing that, you must accept\n  last-one-in-wins behavior.  commute allows for more concurrency than\n  ref-set." [ref fun & args] (. ref (commute fun args)))
//---
(function __clojure_fn_4431(){
return (clojure.JS.def(clojure,"commute",clojure.JS.variadic(2,(function __clojure_fn_4431_commute_4433(ref_1,fun_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((ref_1).commute(fun_2,args_3))}))))}).apply(null,[]);

//======
//(defn alter "Must be called in a transaction. Sets the in-transaction-value of\n  ref to:\n\n  (apply fun in-transaction-value-of-ref args)\n\n  and returns the in-transaction-value of ref." [ref fun & args] (. ref (alter fun args)))
//---
(function __clojure_fn_4443(){
return (clojure.JS.def(clojure,"alter",clojure.JS.variadic(2,(function __clojure_fn_4443_alter_4445(ref_1,fun_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((ref_1).alter(fun_2,args_3))}))))}).apply(null,[]);

//======
//(defn ref-set "Must be called in a transaction. Sets the value of ref.\n  Returns val." [ref val] (. ref (set val)))
//---
(function __clojure_fn_4455(){
return (clojure.JS.def(clojure,"ref_set",(function __clojure_fn_4455_ref_set_4457(ref_1,val_2){
return ((ref_1).set(val_2))})))}).apply(null,[]);

//======
//(defn ensure "Must be called in a transaction. Protects the ref from modification\n  by other transactions.  Returns the in-transaction-value of\n  ref. Allows for more concurrency than (ref-set ref @ref)" [ref] (. ref (touch)) (. ref (get)))
//---
(function __clojure_fn_4467(){
return (clojure.JS.def(clojure,"ensure",(function __clojure_fn_4467_ensure_4469(ref_1){
return ((ref_1).touch(),
(ref_1).get())})))}).apply(null,[]);

//======
//(defn comp "Takes a set of functions and returns a fn that is the composition\n  of those fns.  The returned fn takes a variable number of args,\n  applies the rightmost of fns to the args, the next\n  fn (right-to-left) to the result, etc." [& fs] (let [fs (reverse fs)] (fn [& args] (loop [ret (apply (first fs) args) fs (rest fs)] (if fs (recur ((first fs) ret) (rest fs)) ret)))))
//---
(function __clojure_fn_4488(){
return (clojure.JS.def(clojure,"comp",clojure.JS.variadic(0,(function __clojure_fn_4488_comp_4490(){
var fs_1,fs_2,fs_1=clojure.JS.rest_args(this,arguments,0);
return (((fs_2=clojure.reverse.apply(null,[fs_1])),
clojure.JS.variadic(0,(function __clojure_fn_4488_comp_4490_fn_4492(){
var args_1,ret_2,fs_3,args_1=clojure.JS.rest_args(this,arguments,0);
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.apply.apply(null,[clojure.first.apply(null,[fs_2]),args_1])),
(fs_3=clojure.rest.apply(null,[fs_2]));do{_cnt=0;
_rtn=((fs_3)?((_cnt=1,_rtn=[clojure.first.apply(null,[fs_3]).apply(null,[ret_2]),clojure.rest.apply(null,[fs_3])],ret_2=_rtn[0],fs_3=_rtn[1])):(ret_2))}while(_cnt);return _rtn;})()))}))))}))))}).apply(null,[]);

//======
//(defn partial "Takes a function f and fewer than the normal arguments to f, and\n  returns a fn that takes a variable number of additional args. When\n  called, the returned function calls f with args + additional args." ([f arg1] (fn [& args] (apply f arg1 args))) ([f arg1 arg2] (fn [& args] (apply f arg1 arg2 args))) ([f arg1 arg2 arg3] (fn [& args] (apply f arg1 arg2 arg3 args))) ([f arg1 arg2 arg3 & more] (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))
//---
(function __clojure_fn_4518(){
return (clojure.JS.def(clojure,"partial",clojure.JS.variadic(4,(function __clojure_fn_4518_partial_4520(f_1,arg1_2,arg2_3,arg3_4){switch(arguments.length){
case 4:return (clojure.JS.variadic(0,(function __clojure_fn_4518_partial_4520_fn_4530(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[f_1,arg1_2,arg2_3,arg3_4,args_1]))})))
case 3:return (clojure.JS.variadic(0,(function __clojure_fn_4518_partial_4520_fn_4526(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[f_1,arg1_2,arg2_3,args_1]))})))
case 2:return (clojure.JS.variadic(0,(function __clojure_fn_4518_partial_4520_fn_4522(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[f_1,arg1_2,args_1]))})))}
var more_5,more_5=clojure.JS.rest_args(this,arguments,4);
return (clojure.JS.variadic(0,(function __clojure_fn_4518_partial_4520_fn_4534(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[f_1,arg1_2,arg2_3,arg3_4,clojure.concat.apply(null,[more_5,args_1])]))})))}))))}).apply(null,[]);

//======
//(defn every? "Returns true if (pred x) is logical true for every x in coll, else\n  false." {:tag Boolean} [pred coll] (if (seq coll) (and (pred (first coll)) (recur pred (rest coll))) true))
//---
(function __clojure_fn_4545(){
return (clojure.JS.def(clojure,"every_QMARK_",(function __clojure_fn_4545_every_QMARK_4547(pred_1,coll_2){
var _cnt,_rtn,and__196_3;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[coll_2]))?(((and__196_3=pred_1.apply(null,[clojure.first.apply(null,[coll_2])])),
((and__196_3)?((_cnt=1,_rtn=[pred_1,clojure.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])):(and__196_3)))):(true))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def not-every? (comp not every?))
//---
(function __clojure_fn_4554(){
return (clojure.JS.def(clojure,"not_every_QMARK_",clojure.comp.apply(null,[clojure.not,clojure.every_QMARK_])))}).apply(null,[]);

//======
//(defn some "Returns the first logical true value of (pred x) for any x in coll,\n  else nil." [pred coll] (when (seq coll) (or (pred (first coll)) (recur pred (rest coll)))))
//---
(function __clojure_fn_4563(){
return (clojure.JS.def(clojure,"some",(function __clojure_fn_4563_some_4565(pred_1,coll_2){
var _cnt,_rtn,or__202_3;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[coll_2]))?(((or__202_3=pred_1.apply(null,[clojure.first.apply(null,[coll_2])])),
((or__202_3)?(or__202_3):((_cnt=1,_rtn=[pred_1,clojure.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1]))))):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(def not-any? (comp not some))
//---
(function __clojure_fn_4572(){
return (clojure.JS.def(clojure,"not_any_QMARK_",clojure.comp.apply(null,[clojure.not,clojure.some])))}).apply(null,[]);

//======
//(defn map "Returns a lazy seq consisting of the result of applying f to the\n  set of first items of each coll, followed by applying f to the set\n  of second items in each coll, until any one of the colls is\n  exhausted.  Any remaining items in other colls are ignored. Function\n  f should accept number-of-colls arguments." ([f coll] (when (seq coll) (lazy-cons (f (first coll)) (map f (rest coll))))) ([f c1 c2] (when (and (seq c1) (seq c2)) (lazy-cons (f (first c1) (first c2)) (map f (rest c1) (rest c2))))) ([f c1 c2 c3] (when (and (seq c1) (seq c2) (seq c3)) (lazy-cons (f (first c1) (first c2) (first c3)) (map f (rest c1) (rest c2) (rest c3))))) ([f c1 c2 c3 & colls] (let [step (fn step [cs] (when (every? seq cs) (lazy-cons (map first cs) (step (map rest cs)))))] (map (fn* [p1__4575] (apply f p1__4575)) (step (conj colls c3 c2 c1))))))
//---
(function __clojure_fn_4610(){
return (clojure.JS.def(clojure,"map",clojure.JS.variadic(4,(function __clojure_fn_4610_map_4612(f_1,c1_2,c2_3,c3_4){switch(arguments.length){
case 4:var and__196_5,and__196_6;
return (((((and__196_5=clojure.seq.apply(null,[c1_2])),
((and__196_5)?(((and__196_6=clojure.seq.apply(null,[c2_3])),
((and__196_6)?(clojure.seq.apply(null,[c3_4])):(and__196_6)))):(and__196_5))))?((new clojure.lang.LazyCons((function __clojure_fn_4610_map_4612_fn_4627(G__4626_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.first.apply(null,[c1_2]),clojure.first.apply(null,[c2_3]),clojure.first.apply(null,[c3_4])]))}
return (clojure.map.apply(null,[f_1,clojure.rest.apply(null,[c1_2]),clojure.rest.apply(null,[c2_3]),clojure.rest.apply(null,[c3_4])]))})))):(null)))
case 3:var and__196_4;
return (((((and__196_4=clojure.seq.apply(null,[c1_2])),
((and__196_4)?(clojure.seq.apply(null,[c2_3])):(and__196_4))))?((new clojure.lang.LazyCons((function __clojure_fn_4610_map_4612_fn_4621(G__4620_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.first.apply(null,[c1_2]),clojure.first.apply(null,[c2_3])]))}
return (clojure.map.apply(null,[f_1,clojure.rest.apply(null,[c1_2]),clojure.rest.apply(null,[c2_3])]))})))):(null)))
case 2:var coll_2=arguments[1];
return (((clojure.seq.apply(null,[coll_2]))?((new clojure.lang.LazyCons((function __clojure_fn_4610_map_4612_fn_4615(G__4614_1){switch(arguments.length){
case 0:return (f_1.apply(null,[clojure.first.apply(null,[coll_2])]))}
return (clojure.map.apply(null,[f_1,clojure.rest.apply(null,[coll_2])]))})))):(null)))}
var colls_5,step_6,colls_5=clojure.JS.rest_args(this,arguments,4);
return (((step_6=(function __clojure_fn_4610_map_4612_step_4632(cs_1){
var step_0=arguments.callee;
return (((clojure.every_QMARK_.apply(null,[clojure.seq,cs_1]))?((new clojure.lang.LazyCons((function __clojure_fn_4610_map_4612_step_4632_fn_4634(G__4633_1){switch(arguments.length){
case 0:return (clojure.map.apply(null,[clojure.first,cs_1]))}
return (step_0.apply(null,[clojure.map.apply(null,[clojure.rest,cs_1])]))})))):(null)))})),
clojure.map.apply(null,[(function __clojure_fn_4610_map_4612_fn_4639(p1__4575_1){
return (clojure.apply.apply(null,[f_1,p1__4575_1]))}),step_6.apply(null,[clojure.conj.apply(null,[colls_5,c3_4,c2_3,c1_2])])])))}))))}).apply(null,[]);

//======
//(defn mapcat "Returns the result of applying concat to the result of applying map\n  to f and colls.  Thus function f should return a collection." [f & colls] (apply concat (apply map f colls)))
//---
(function __clojure_fn_4650(){
return (clojure.JS.def(clojure,"mapcat",clojure.JS.variadic(1,(function __clojure_fn_4650_mapcat_4652(f_1){
var colls_2,colls_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.apply.apply(null,[clojure.concat,clojure.apply.apply(null,[clojure.map,f_1,colls_2])]))}))))}).apply(null,[]);

//======
//(defn filter "Returns a lazy seq of the items in coll for which\n  (pred item) returns true. pred must be free of side-effects." [pred coll] (when (seq coll) (if (pred (first coll)) (lazy-cons (first coll) (filter pred (rest coll))) (recur pred (rest coll)))))
//---
(function __clojure_fn_4667(){
return (clojure.JS.def(clojure,"filter",(function __clojure_fn_4667_filter_4669(pred_1,coll_2){
var _cnt,_rtn;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[coll_2]))?(((pred_1.apply(null,[clojure.first.apply(null,[coll_2])]))?((new clojure.lang.LazyCons((function __clojure_fn_4667_filter_4669_fn_4672(G__4671_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll_2]))}
return (clojure.filter.apply(null,[pred_1,clojure.rest.apply(null,[coll_2])]))})))):((_cnt=1,_rtn=[pred_1,clojure.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])))):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn take "Returns a lazy seq of the first n items in coll, or all items if\n  there are fewer than n." [n coll] (when (and (pos? n) (seq coll)) (lazy-cons (first coll) (take (dec n) (rest coll)))))
//---
(function __clojure_fn_4689(){
return (clojure.JS.def(clojure,"take",(function __clojure_fn_4689_take_4691(n_1,coll_2){
var and__196_3;
return (((((and__196_3=clojure.lang.Numbers.isPos(n_1)),
((and__196_3)?(clojure.seq.apply(null,[coll_2])):(and__196_3))))?((new clojure.lang.LazyCons((function __clojure_fn_4689_take_4691_fn_4694(G__4693_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll_2]))}
return (clojure.take.apply(null,[clojure.lang.Numbers.dec(n_1),clojure.rest.apply(null,[coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn take-while "Returns a lazy seq of successive items from coll while\n  (pred item) returns true. pred must be free of side-effects." [pred coll] (when (and (seq coll) (pred (first coll))) (lazy-cons (first coll) (take-while pred (rest coll)))))
//---
(function __clojure_fn_4711(){
return (clojure.JS.def(clojure,"take_while",(function __clojure_fn_4711_take_while_4713(pred_1,coll_2){
var and__196_3;
return (((((and__196_3=clojure.seq.apply(null,[coll_2])),
((and__196_3)?(pred_1.apply(null,[clojure.first.apply(null,[coll_2])])):(and__196_3))))?((new clojure.lang.LazyCons((function __clojure_fn_4711_take_while_4713_fn_4716(G__4715_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll_2]))}
return (clojure.take_while.apply(null,[pred_1,clojure.rest.apply(null,[coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn drop "Returns a lazy seq of all but the first n items in coll." [n coll] (if (and (pos? n) (seq coll)) (recur (dec n) (rest coll)) (seq coll)))
//---
(function __clojure_fn_4728(){
return (clojure.JS.def(clojure,"drop",(function __clojure_fn_4728_drop_4730(n_1,coll_2){
var _cnt,_rtn,and__196_3;
do{_cnt=0;_rtn=((((and__196_3=clojure.lang.Numbers.isPos(n_1)),
((and__196_3)?(clojure.seq.apply(null,[coll_2])):(and__196_3))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_1),clojure.rest.apply(null,[coll_2])],n_1=_rtn[0],coll_2=_rtn[1])):(clojure.seq.apply(null,[coll_2])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn drop-last "Return a lazy seq of all but the last n (default 1) items in coll" ([s] (drop-last 1 s)) ([n s] (map (fn [x _] x) (seq s) (drop n s))))
//---
(function __clojure_fn_4744(){
return (clojure.JS.def(clojure,"drop_last",(function __clojure_fn_4744_drop_last_4746(n_1,s_2){switch(arguments.length){
case 1:var s_1=arguments[0];
return (clojure.drop_last.apply(null,[1,s_1]))}
return (clojure.map.apply(null,[(function __clojure_fn_4744_drop_last_4746_fn_4749(x_1,__2){
return (x_1)}),clojure.seq.apply(null,[s_2]),clojure.drop.apply(null,[n_1,s_2])]))})))}).apply(null,[]);

//======
//(defn drop-while "Returns a lazy seq of the items in coll starting from the first\n  item for which (pred item) returns nil." [pred coll] (if (and (seq coll) (pred (first coll))) (recur pred (rest coll)) (seq coll)))
//---
(function __clojure_fn_4760(){
return (clojure.JS.def(clojure,"drop_while",(function __clojure_fn_4760_drop_while_4762(pred_1,coll_2){
var _cnt,_rtn,and__196_3;
do{_cnt=0;_rtn=((((and__196_3=clojure.seq.apply(null,[coll_2])),
((and__196_3)?(pred_1.apply(null,[clojure.first.apply(null,[coll_2])])):(and__196_3))))?((_cnt=1,_rtn=[pred_1,clojure.rest.apply(null,[coll_2])],pred_1=_rtn[0],coll_2=_rtn[1])):(clojure.seq.apply(null,[coll_2])))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn cycle "Returns a lazy (infinite!) seq of repetitions of the items in\n  coll." [coll] (when (seq coll) (let [rep (fn thisfn [xs] (if xs (lazy-cons (first xs) (thisfn (rest xs))) (recur (seq coll))))] (rep (seq coll)))))
//---
(function __clojure_fn_4779(){
return (clojure.JS.def(clojure,"cycle",(function __clojure_fn_4779_cycle_4781(coll_1){
var rep_2;
return (((clojure.seq.apply(null,[coll_1]))?(((rep_2=(function __clojure_fn_4779_cycle_4781_thisfn_4783(xs_1){
var _cnt,_rtn,thisfn_0=arguments.callee;
do{_cnt=0;_rtn=((xs_1)?((new clojure.lang.LazyCons((function __clojure_fn_4779_cycle_4781_thisfn_4783_fn_4785(G__4784_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[xs_1]))}
return (thisfn_0.apply(null,[clojure.rest.apply(null,[xs_1])]))})))):((_cnt=1,_rtn=[clojure.seq.apply(null,[coll_1])],xs_1=_rtn[0])))
}while(_cnt);return _rtn;})),
rep_2.apply(null,[clojure.seq.apply(null,[coll_1])]))):(null)))})))}).apply(null,[]);

//======
//(defn split-at "Returns a vector of [(take n coll) (drop n coll)]" [n coll] [(take n coll) (drop n coll)])
//---
(function __clojure_fn_4798(){
return (clojure.JS.def(clojure,"split_at",(function __clojure_fn_4798_split_at_4800(n_1,coll_2){
return (clojure.JS.lit_vector([clojure.take.apply(null,[n_1,coll_2]),clojure.drop.apply(null,[n_1,coll_2])]))})))}).apply(null,[]);

//======
//(defn split-with "Returns a vector of [(take-while pred coll) (drop-while pred coll)]" [pred coll] [(take-while pred coll) (drop-while pred coll)])
//---
(function __clojure_fn_4810(){
return (clojure.JS.def(clojure,"split_with",(function __clojure_fn_4810_split_with_4812(pred_1,coll_2){
return (clojure.JS.lit_vector([clojure.take_while.apply(null,[pred_1,coll_2]),clojure.drop_while.apply(null,[pred_1,coll_2])]))})))}).apply(null,[]);

//======
//(defn repeat "Returns a lazy (infinite!) seq of xs." [x] (lazy-cons x (repeat x)))
//---
(function __clojure_fn_4827(){
return (clojure.JS.def(clojure,"repeat",(function __clojure_fn_4827_repeat_4829(x_1){
return ((new clojure.lang.LazyCons((function __clojure_fn_4827_repeat_4829_fn_4832(G__4831_1){switch(arguments.length){
case 0:return (x_1)}
return (clojure.repeat.apply(null,[x_1]))}))))})))}).apply(null,[]);

//======
//(defn replicate "Returns a lazy seq of n xs." [n x] (take n (repeat x)))
//---
(function __clojure_fn_4844(){
return (clojure.JS.def(clojure,"replicate",(function __clojure_fn_4844_replicate_4846(n_1,x_2){
return (clojure.take.apply(null,[n_1,clojure.repeat.apply(null,[x_2])]))})))}).apply(null,[]);

//======
//(defn iterate "Returns a lazy seq of x, (f x), (f (f x)) etc. f must be free of side-effects" [f x] (lazy-cons x (iterate f (f x))))
//---
(function __clojure_fn_4861(){
return (clojure.JS.def(clojure,"iterate",(function __clojure_fn_4861_iterate_4863(f_1,x_2){
return ((new clojure.lang.LazyCons((function __clojure_fn_4861_iterate_4863_fn_4866(G__4865_1){switch(arguments.length){
case 0:return (x_2)}
return (clojure.iterate.apply(null,[f_1,f_1.apply(null,[x_2])]))}))))})))}).apply(null,[]);

//======
//(defn range "Returns a lazy seq of nums from start (inclusive) to end\n  (exclusive), by step, where start defaults to 0 and step to 1." ([end] (if (and (> end 0) (< end clojure.lang.RT/IntegerMaxValue)) (new clojure.lang.Range 0 end) (take end (iterate inc 0)))) ([start end] (if (and (< start end) (< end clojure.lang.RT/IntegerMaxValue)) (new clojure.lang.Range start end) (take (- end start) (iterate inc start)))) ([start end step] (take-while (partial (if (pos? step) > <) end) (iterate (partial + step) start))))
//---
(function __clojure_fn_4880(){
return (clojure.JS.def(clojure,"range",(function __clojure_fn_4880_range_4882(start_1,end_2,step_3){switch(arguments.length){
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
(function __clojure_fn_4894(){
return (clojure.JS.def(clojure,"merge",clojure.JS.variadic(0,(function __clojure_fn_4894_merge_4896(){
var maps_1,maps_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.reduce.apply(null,[clojure.conj,maps_1]))}))))}).apply(null,[]);

//======
//(defn merge-with "Returns a map that consists of the rest of the maps conj-ed onto\n  the first.  If a key occurs in more than one map, the mapping(s)\n  from the latter (left-to-right) will be combined with the mapping in\n  the result by calling (f val-in-result val-in-latter)." [f & maps] (let [merge-entry (fn [m e] (let [k (key e) v (val e)] (if (contains? m k) (assoc m k (f (m k) v)) (assoc m k v)))) merge2 (fn [m1 m2] (reduce merge-entry m1 (seq m2)))] (reduce merge2 maps)))
//---
(function __clojure_fn_4912(){
return (clojure.JS.def(clojure,"merge_with",clojure.JS.variadic(1,(function __clojure_fn_4912_merge_with_4914(f_1){
var merge_entry_3,maps_2,merge2_4,maps_2=clojure.JS.rest_args(this,arguments,1);
return (((merge_entry_3=(function __clojure_fn_4912_merge_with_4914_merge_entry_4916(m_1,e_2){
var v_4,k_3;
return (((k_3=clojure.key.apply(null,[e_2])),
(v_4=clojure.val.apply(null,[e_2])),
((clojure.contains_QMARK_.apply(null,[m_1,k_3]))?(clojure.assoc.apply(null,[m_1,k_3,f_1.apply(null,[m_1.apply(null,[k_3]),v_4])])):(clojure.assoc.apply(null,[m_1,k_3,v_4])))))})),
(merge2_4=(function __clojure_fn_4912_merge_with_4914_merge2_4919(m1_1,m2_2){
return (clojure.reduce.apply(null,[merge_entry_3,m1_1,clojure.seq.apply(null,[m2_2])]))})),
clojure.reduce.apply(null,[merge2_4,maps_2])))}))))}).apply(null,[]);

//======
//(defn zipmap "Returns a map with the keys mapped to the corresponding vals." [keys vals] (loop [map {} ks (seq keys) vs (seq vals)] (if (and ks vs) (recur (assoc map (first ks) (first vs)) (rest ks) (rest vs)) map)))
//---
(function __clojure_fn_4930(){
return (clojure.JS.def(clojure,"zipmap",(function __clojure_fn_4930_zipmap_4932(keys_1,vals_2){
var and__196_6,ks_4,map_3,vs_5;
return (((function __loop(){var _rtn,_cnt;(map_3=clojure.lang.PersistentHashMap.EMPTY),
(ks_4=clojure.seq.apply(null,[keys_1])),
(vs_5=clojure.seq.apply(null,[vals_2]));do{_cnt=0;
_rtn=((((and__196_6=ks_4),
((and__196_6)?(vs_5):(and__196_6))))?((_cnt=1,_rtn=[clojure.assoc.apply(null,[map_3,clojure.first.apply(null,[ks_4]),clojure.first.apply(null,[vs_5])]),clojure.rest.apply(null,[ks_4]),clojure.rest.apply(null,[vs_5])],map_3=_rtn[0],ks_4=_rtn[1],vs_5=_rtn[2])):(map_3))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn line-seq "Returns the lines of text from rdr as a lazy sequence of strings.\n  rdr must implement java.io.BufferedReader." [rdr] (let [line (. rdr (readLine))] (when line (lazy-cons line (line-seq rdr)))))
//---
(function __clojure_fn_4947(){
return (clojure.JS.def(clojure,"line_seq",(function __clojure_fn_4947_line_seq_4949(rdr_1){
var line_2;
return (((line_2=(rdr_1).readLine()),
((line_2)?((new clojure.lang.LazyCons((function __clojure_fn_4947_line_seq_4949_fn_4952(G__4951_1){switch(arguments.length){
case 0:return (line_2)}
return (clojure.line_seq.apply(null,[rdr_1]))})))):(null))))})))}).apply(null,[]);

//======
//(defn comparator "Returns an implementation of java.util.Comparator based upon pred." [pred] (fn [x y] (cond (pred x y) -1 (pred y x) 1 :else 0)))
//---
(function __clojure_fn_4967(){
return (clojure.JS.def(clojure,"comparator",(function __clojure_fn_4967_comparator_4969(pred_1){
return ((function __clojure_fn_4967_comparator_4969_fn_4971(x_1,y_2){
return (((pred_1.apply(null,[x_1,y_2]))?(-1):(((pred_1.apply(null,[y_2,x_1]))?(1):(((clojure.keyword("","else"))?(0):(null)))))))}))})))}).apply(null,[]);

//======
//(defn sort "Returns a sorted sequence of the items in coll. If no comparator is\n  supplied, uses compare. comparator must\n  implement java.util.Comparator." ([coll] (sort compare coll)) ([comp coll] (when (and coll (not (. coll (isEmpty)))) (let [a (. coll (toArray))] (clojure.lang.RT/sortArray a comp) (seq a)))))
//---
(function __clojure_fn_4983(){
return (clojure.JS.def(clojure,"sort",(function __clojure_fn_4983_sort_4985(comp_1,coll_2){switch(arguments.length){
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
(function __clojure_fn_5000(){
return (clojure.JS.def(clojure,"sort_by",(function __clojure_fn_5000_sort_by_5002(keyfn_1,comp_2,coll_3){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (clojure.sort_by.apply(null,[keyfn_1,clojure.compare,coll_2]))}
return (clojure.sort.apply(null,[(function __clojure_fn_5000_sort_by_5002_fn_5005(x_1,y_2){
return ((comp_2).compare(keyfn_1.apply(null,[x_1]),keyfn_1.apply(null,[y_2])))}),coll_3]))})))}).apply(null,[]);

//======
//(defn scan [& args] (throw (clojure.lang.RT/makeException "scan is now called dorun")))
//---
(function __clojure_fn_5029(){
return (clojure.JS.def(clojure,"scan",clojure.JS.variadic(0,(function __clojure_fn_5029_scan_5031(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw clojure.lang.RT.makeException("scan is now called dorun")})())}))))}).apply(null,[]);

//======
//(defn touch [& args] (throw (clojure.lang.RT/makeException "touch is now called doall")))
//---
(function __clojure_fn_5041(){
return (clojure.JS.def(clojure,"touch",clojure.JS.variadic(0,(function __clojure_fn_5041_touch_5043(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return ((function __throw(){throw clojure.lang.RT.makeException("touch is now called doall")})())}))))}).apply(null,[]);

//======
//(defn dorun "When lazy sequences are produced via functions that have side\n  effects, any effects other than those needed to produce the first\n  element in the seq do not occur until the seq is consumed. dorun can\n  be used to force any effects. Walks through the successive rests of\n  the seq, does not retain the head and returns nil." ([coll] (when (and (seq coll) (or (first coll) true)) (recur (rest coll)))) ([n coll] (when (and (seq coll) (pos? n) (or (first coll) true)) (recur (dec n) (rest coll)))))
//---
(function __clojure_fn_5054(){
return (clojure.JS.def(clojure,"dorun",(function __clojure_fn_5054_dorun_5056(n_1,coll_2){switch(arguments.length){
case 1:var _cnt,_rtn,or__202_3,and__196_2,coll_1=arguments[0];
do{_cnt=0;_rtn=((((and__196_2=clojure.seq.apply(null,[coll_1])),
((and__196_2)?(((or__202_3=clojure.first.apply(null,[coll_1])),
((or__202_3)?(or__202_3):(true)))):(and__196_2))))?((_cnt=1,_rtn=[clojure.rest.apply(null,[coll_1])],coll_1=_rtn[0])):(null))
}while(_cnt);return _rtn;}
var _cnt,_rtn,and__196_3,and__196_4,or__202_5;
do{_cnt=0;_rtn=((((and__196_3=clojure.seq.apply(null,[coll_2])),
((and__196_3)?(((and__196_4=clojure.lang.Numbers.isPos(n_1)),
((and__196_4)?(((or__202_5=clojure.first.apply(null,[coll_2])),
((or__202_5)?(or__202_5):(true)))):(and__196_4)))):(and__196_3))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_1),clojure.rest.apply(null,[coll_2])],n_1=_rtn[0],coll_2=_rtn[1])):(null))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn doall "When lazy sequences are produced via functions that have side\n  effects, any effects other than those needed to produce the first\n  element in the seq do not occur until the seq is consumed. doall can\n  be used to force any effects. Walks through the successive rests of\n  the seq, retains the head and returns it, thus causing the entire\n  seq to reside in memory at one time." ([coll] (dorun coll) coll) ([n coll] (dorun n coll) coll))
//---
(function __clojure_fn_5068(){
return (clojure.JS.def(clojure,"doall",(function __clojure_fn_5068_doall_5070(n_1,coll_2){switch(arguments.length){
case 1:var coll_1=arguments[0];
return (clojure.dorun.apply(null,[coll_1]),
coll_1)}
return (clojure.dorun.apply(null,[n_1,coll_2]),
coll_2)})))}).apply(null,[]);

//======
//(defn await1 [a] (when (pos? (.getQueueCount a)) (await a)) a)
//---
(function __clojure_fn_5090(){
return (clojure.JS.def(clojure,"await1",(function __clojure_fn_5090_await1_5092(a_1){
return (((clojure.lang.Numbers.isPos((a_1).getQueueCount()))?(clojure.await.apply(null,[a_1])):(null)),
a_1)})))}).apply(null,[]);

//======
//(import (quote (clojure.lang RT)))
//---
(function __clojure_fn_5121(){
return (clojure.import_.apply(null,[clojure.JS.lit_list(["'clojure.lang","'RT"])]))}).apply(null,[]);

//======
//(defn into-array "Returns an array of the type of the first element in coll,\n  containing the contents of coll, which must be of a compatible\n  type." [aseq] (. clojure.lang.RT (seqToTypedArray (seq aseq))))
//---
(function __clojure_fn_5130(){
return (clojure.JS.def(clojure,"into_array",(function __clojure_fn_5130_into_array_5132(aseq_1){
return (clojure.lang.RT.seqToTypedArray(clojure.seq.apply(null,[aseq_1])))})))}).apply(null,[]);

//======
//(defn into "Returns a new coll consisting of to-coll with all of the items of\n  from-coll conjoined." [to from] (let [ret to items (seq from)] (if items (recur (conj ret (first items)) (rest items)) ret)))
//---
(function __clojure_fn_5142(){
return (clojure.JS.def(clojure,"into",(function __clojure_fn_5142_into_5144(to_1,from_2){
var _cnt,_rtn,ret_3,items_4;
do{_cnt=0;_rtn=((ret_3=to_1),
(items_4=clojure.seq.apply(null,[from_2])),
((items_4)?((_cnt=1,_rtn=[clojure.conj.apply(null,[ret_3,clojure.first.apply(null,[items_4])]),clojure.rest.apply(null,[items_4])],to_1=_rtn[0],from_2=_rtn[1])):(ret_3)))
}while(_cnt);return _rtn;})))}).apply(null,[]);

//======
//(defn array [& items] (into-array items))
//---
(function __clojure_fn_5154(){
return (clojure.JS.def(clojure,"array",clojure.JS.variadic(0,(function __clojure_fn_5154_array_5156(){
var items_1,items_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.into_array.apply(null,[items_1]))}))))}).apply(null,[]);

//======
//(defn pr "Prints the object(s) to the output stream that is the current value\n  of *out*.  Prints the object(s), separated by spaces if there is\n  more than one.  By default, pr and prn print in a way that objects\n  can be read by the reader" ([] nil) ([x] (print-method x *out*) nil) ([x & more] (pr x) (. *out* (append \space)) (apply pr more)))
//---
(function __clojure_fn_5180(){
return (clojure.JS.def(clojure,"pr",clojure.JS.variadic(1,(function __clojure_fn_5180_pr_5182(x_1){switch(arguments.length){
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
(function __clojure_fn_5194(){
return (clojure.JS.def(clojure,"newline",(function __clojure_fn_5194_newline_5196(){
return ((clojure._STAR_out_STAR_).append("\n"),
null)})))}).apply(null,[]);

//======
//(defn flush "Flushes the output stream that is the current value of\n  *out*" [] (. *out* (flush)) nil)
//---
(function __clojure_fn_5206(){
return (clojure.JS.def(clojure,"flush",(function __clojure_fn_5206_flush_5208(){
return ((clojure._STAR_out_STAR_).flush(),
null)})))}).apply(null,[]);

//======
//(defn prn "Same as pr followed by (newline). Observes *flush-on-newline*" [& more] (apply pr more) (newline) (when *flush-on-newline* (flush)))
//---
(function __clojure_fn_5218(){
return (clojure.JS.def(clojure,"prn",clojure.JS.variadic(0,(function __clojure_fn_5218_prn_5220(){
var more_1,more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[clojure.pr,more_1]),
clojure.newline.apply(null,[]),
((clojure._STAR_flush_on_newline_STAR_)?(clojure.flush.apply(null,[])):(null)))}))))}).apply(null,[]);

//======
//(defn print "Prints the object(s) to the output stream that is the current value\n  of *out*.  print and println produce output for human consumption." [& more] (binding [*print-readably* nil] (apply pr more)))
//---
(function __clojure_fn_5230(){
return (clojure.JS.def(clojure,"print",clojure.JS.variadic(0,(function __clojure_fn_5230_print_5232(){
var more_1,more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_print_readably_STAR_,null])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.pr,more_1]))}
finally{clojure.lang.Var.popThreadBindings()}})())}))))}).apply(null,[]);

//======
//(defn println "Same as print followed by (newline)" [& more] (binding [*print-readably* nil] (apply prn more)))
//---
(function __clojure_fn_5242(){
return (clojure.JS.def(clojure,"println",clojure.JS.variadic(0,(function __clojure_fn_5242_println_5244(){
var more_1,more_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_print_readably_STAR_,null])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.prn,more_1]))}
finally{clojure.lang.Var.popThreadBindings()}})())}))))}).apply(null,[]);

//======
//(defn read "Reads the next object from stream, which must be an instance of\n  java.io.PushbackReader or some derivee.  stream defaults to the\n  current value of *in* ." ([] (read *in*)) ([stream] (read stream true nil)) ([stream eof-error? eof-value] (read stream eof-error? eof-value false)) ([stream eof-error? eof-value recursive?] (. clojure.lang.LispReader (read stream eof-error? eof-value recursive?))))
//---
(function __clojure_fn_5257(){
return (clojure.JS.def(clojure,"read",(function __clojure_fn_5257_read_5259(stream_1,eof_error_QMARK__2,eof_value_3,recursive_QMARK__4){switch(arguments.length){
case 3:return (clojure.read.apply(null,[stream_1,eof_error_QMARK__2,eof_value_3,false]))
case 1:return (clojure.read.apply(null,[stream_1,true,null]))
case 0:return (clojure.read.apply(null,[clojure._STAR_in_STAR_]))}
return (clojure.lang.LispReader.read(stream_1,eof_error_QMARK__2,eof_value_3,recursive_QMARK__4))})))}).apply(null,[]);

//======
//(defn read-line "Reads the next line from stream that is the current value of *in* ." [] (. *in* (readLine)))
//---
(function __clojure_fn_5272(){
return (clojure.JS.def(clojure,"read_line",(function __clojure_fn_5272_read_line_5274(){
return ((clojure._STAR_in_STAR_).readLine())})))}).apply(null,[]);

//======
//(defn num "Coerce to Number" {:tag Number, :inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (clojure/concat (clojure/list (quote clojure/num)) (clojure/list x)))))} [x] (. clojure.lang.Numbers (num x)))
//---
(function __clojure_fn_5317(){
return (clojure.JS.def(clojure,"num",(function __clojure_fn_5317_num_5322(x_1){
return (clojure.lang.Numbers.num(x_1))})))}).apply(null,[]);

//======
//(defn int "Coerce to int" {:tag Integer, :inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/intCast)) (clojure/list x)))))} [x] (. clojure.lang.RT (intCast x)))
//---
(function __clojure_fn_5335(){
return (clojure.JS.def(clojure,"int",(function __clojure_fn_5335_int_5340(x_1){
return (clojure.lang.RT.intCast(x_1))})))}).apply(null,[]);

//======
//(defn long "Coerce to long" {:tag Long, :inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/longCast)) (clojure/list x)))))} [x] (. x (longValue)))
//---
(function __clojure_fn_5353(){
return (clojure.JS.def(clojure,"long",(function __clojure_fn_5353_long_5358(x_1){
return ((x_1).longValue())})))}).apply(null,[]);

//======
//(defn float "Coerce to float" {:tag Float, :inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/floatCast)) (clojure/list x)))))} [x] (. x (floatValue)))
//---
(function __clojure_fn_5371(){
return (clojure.JS.def(clojure,"float",(function __clojure_fn_5371_float_5376(x_1){
return ((x_1).floatValue())})))}).apply(null,[]);

//======
//(defn double "Coerce to double" {:tag Double, :inline (fn [x] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/doubleCast)) (clojure/list x)))))} [x] (. x (doubleValue)))
//---
(function __clojure_fn_5389(){
return (clojure.JS.def(clojure,"double",(function __clojure_fn_5389_double_5394(x_1){
return ((x_1).doubleValue())})))}).apply(null,[]);

//======
//(defn short "Coerce to short" {:tag Short} [x] (. x (shortValue)))
//---
(function __clojure_fn_5404(){
return (clojure.JS.def(clojure,"short_",(function __clojure_fn_5404_short_5406(x_1){
return ((x_1).shortValue())})))}).apply(null,[]);

//======
//(defn byte "Coerce to byte" {:tag Byte} [x] (. x (byteValue)))
//---
(function __clojure_fn_5416(){
return (clojure.JS.def(clojure,"byte_",(function __clojure_fn_5416_byte_5418(x_1){
return ((x_1).byteValue())})))}).apply(null,[]);

//======
//(defn char "Coerce to char" {:tag Character} [x] (. clojure.lang.RT (charCast x)))
//---
(function __clojure_fn_5428(){
return (clojure.JS.def(clojure,"char_",(function __clojure_fn_5428_char_5430(x_1){
return (clojure.lang.RT.charCast(x_1))})))}).apply(null,[]);

//======
//(defn boolean "Coerce to boolean" {:tag Boolean} [x] (if x true false))
//---
(function __clojure_fn_5440(){
return (clojure.JS.def(clojure,"boolean_",(function __clojure_fn_5440_boolean_5442(x_1){
return (((x_1)?(true):(false)))})))}).apply(null,[]);

//======
//(import (quote (java.lang.reflect Array)))
//---
(function __clojure_fn_5461(){
return (clojure.import_.apply(null,[clojure.JS.lit_list(["'java.lang.reflect","'Array"])]))}).apply(null,[]);

//======
//(defn alength "Returns the length of the Java array. Works on arrays of all\n  types." {:inline (fn [a] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/alength)) (clojure/list a)))))} [array] (. clojure.lang.RT (alength array)))
//---
(function __clojure_fn_5473(){
return (clojure.JS.def(clojure,"alength",(function __clojure_fn_5473_alength_5478(array_1){
return (clojure.lang.RT.alength(array_1))})))}).apply(null,[]);

//======
//(defn aclone "Returns a clone of the Java array. Works on arrays of known\n  types." {:inline (fn [a] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/aclone)) (clojure/list a)))))} [array] (. clojure.lang.RT (aclone array)))
//---
(function __clojure_fn_5491(){
return (clojure.JS.def(clojure,"aclone",(function __clojure_fn_5491_aclone_5496(array_1){
return (clojure.lang.RT.aclone(array_1))})))}).apply(null,[]);

//======
//(defn aget "Returns the value at the index/indices. Works on Java arrays of all\n  types." {:inline-arities #{2}, :inline (fn [a i] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/aget)) (clojure/list a) (clojure/list i)))))} ([array idx] (RT/aget array idx)) ([array idx & idxs] (apply aget (aget array idx) idxs)))
//---
(function __clojure_fn_5510(){
return (clojure.JS.def(clojure,"aget",clojure.JS.variadic(2,(function __clojure_fn_5510_aget_5515(array_1,idx_2){switch(arguments.length){
case 2:return (clojure.lang.RT.aget(array_1,idx_2))}
var idxs_3,idxs_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.apply.apply(null,[clojure.aget,clojure.lang.RT.aget(array_1,idx_2),idxs_3]))}))))}).apply(null,[]);

//======
//(defn aset "Sets the value at the index/indices. Works on Java arrays of\n  reference types. Returns val." {:inline-arities #{3}, :inline (fn [a i v] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.RT)) (clojure/list (clojure/concat (clojure/list (quote clojure/aset)) (clojure/list a) (clojure/list i) (clojure/list v)))))} ([array idx val] (RT/aset array idx val) val) ([array idx idx2 & idxv] (apply aset (aget array idx) idx2 idxv)))
//---
(function __clojure_fn_5530(){
return (clojure.JS.def(clojure,"aset",clojure.JS.variadic(3,(function __clojure_fn_5530_aset_5535(array_1,idx_2,idx2_3){switch(arguments.length){
case 3:var val_3=arguments[2];
return (clojure.lang.RT.aset(array_1,idx_2,val_3),
val_3)}
var idxv_4,idxv_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.apply.apply(null,[clojure.aset,clojure.lang.RT.aget(array_1,idx_2),idx2_3,idxv_4]))}))))}).apply(null,[]);

//======
//(defn create-struct "Returns a structure basis object." [& keys] (. clojure.lang.PersistentStructMap (createSlotMap keys)))
//---
(function __clojure_fn_5638(){
return (clojure.JS.def(clojure,"create_struct",clojure.JS.variadic(0,(function __clojure_fn_5638_create_struct_5640(){
var keys_1,keys_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.lang.PersistentStructMap.createSlotMap(keys_1))}))))}).apply(null,[]);

//======
//(defn struct-map "Returns a new structmap instance with the keys of the\n  structure-basis. keyvals may contain all, some or none of the basis\n  keys - where values are not supplied they will default to nil.\n  keyvals can also contain keys not in the basis." [s & inits] (. clojure.lang.PersistentStructMap (create s inits)))
//---
(function __clojure_fn_5656(){
return (clojure.JS.def(clojure,"struct_map",clojure.JS.variadic(1,(function __clojure_fn_5656_struct_map_5658(s_1){
var inits_2,inits_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentStructMap.create(s_1,inits_2))}))))}).apply(null,[]);

//======
//(defn struct "Returns a new structmap instance with the keys of the\n  structure-basis. vals must be supplied for basis keys in order -\n  where values are not supplied they will default to nil." [s & vals] (. clojure.lang.PersistentStructMap (construct s vals)))
//---
(function __clojure_fn_5668(){
return (clojure.JS.def(clojure,"struct",clojure.JS.variadic(1,(function __clojure_fn_5668_struct_5670(s_1){
var vals_2,vals_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.lang.PersistentStructMap.construct(s_1,vals_2))}))))}).apply(null,[]);

//======
//(defn accessor "Returns a fn that, given an instance of a structmap with the basis,\n  returns the value at the key.  The key must be in the basis. The\n  returned function should be (slightly) more efficient than using\n  get, but such use of accessors should be limited to known\n  performance-critical areas." [s key] (. clojure.lang.PersistentStructMap (getAccessor s key)))
//---
(function __clojure_fn_5680(){
return (clojure.JS.def(clojure,"accessor",(function __clojure_fn_5680_accessor_5682(s_1,key_2){
return (clojure.lang.PersistentStructMap.getAccessor(s_1,key_2))})))}).apply(null,[]);

//======
//(defn subvec "Returns a persistent vector of the items in vector from\n  start (inclusive) to end (exclusive).  If end is not supplied,\n  defaults to (count vector). This operation is O(1) and very fast, as\n  the resulting vector shares structure with the original and no\n  trimming is done." ([v start] (subvec v start (count v))) ([v start end] (. clojure.lang.RT (subvec v start end))))
//---
(function __clojure_fn_5693(){
return (clojure.JS.def(clojure,"subvec",(function __clojure_fn_5693_subvec_5695(v_1,start_2,end_3){switch(arguments.length){
case 2:return (clojure.subvec.apply(null,[v_1,start_2,clojure.count.apply(null,[v_1])]))}
return (clojure.lang.RT.subvec(v_1,start_2,end_3))})))}).apply(null,[]);

//======
//(defn resultset-seq "Creates and returns a lazy sequence of structmaps corresponding to\n  the rows in the java.sql.ResultSet rs" [rs] (let [rsmeta (. rs (getMetaData)) idxs (range 1 (inc (. rsmeta (getColumnCount)))) keys (map (comp keyword (memfn toLowerCase)) (map (fn [i] (. rsmeta (getColumnName i))) idxs)) row-struct (apply create-struct keys) row-values (fn [] (map (fn [i] (. rs (getObject i))) idxs)) rows (fn thisfn [] (when (. rs (next)) (lazy-cons (apply struct row-struct (row-values)) (thisfn))))] (rows)))
//---
(function __clojure_fn_5737(){
return (clojure.JS.def(clojure,"resultset_seq",(function __clojure_fn_5737_resultset_seq_5739(rs_1){
var idxs_3,keys_4,rsmeta_2,rows_7,row_values_6,row_struct_5;
return (((rsmeta_2=(rs_1).getMetaData()),
(idxs_3=clojure.range.apply(null,[1,clojure.lang.Numbers.inc((rsmeta_2).getColumnCount())])),
(keys_4=clojure.map.apply(null,[clojure.comp.apply(null,[clojure.keyword,(function __clojure_fn_5737_resultset_seq_5739_fn_5741(target__859_1){
return ((target__859_1).toLowerCase())})]),clojure.map.apply(null,[(function __clojure_fn_5737_resultset_seq_5739_fn_5744(i_1){
return ((rsmeta_2).getColumnName(i_1))}),idxs_3])])),
(row_struct_5=clojure.apply.apply(null,[clojure.create_struct,keys_4])),
(row_values_6=(function __clojure_fn_5737_resultset_seq_5739_row_values_5747(){
return (clojure.map.apply(null,[(function __clojure_fn_5737_resultset_seq_5739_row_values_5747_fn_5749(i_1){
return ((rs_1).getObject(i_1))}),idxs_3]))})),
(rows_7=(function __clojure_fn_5737_resultset_seq_5739_thisfn_5753(){
var thisfn_0=arguments.callee;
return ((((rs_1).next())?((new clojure.lang.LazyCons((function __clojure_fn_5737_resultset_seq_5739_thisfn_5753_fn_5755(G__5754_1){switch(arguments.length){
case 0:return (clojure.apply.apply(null,[clojure.struct,row_struct_5,row_values_6.apply(null,[])]))}
return (thisfn_0.apply(null,[]))})))):(null)))})),
rows_7.apply(null,[])))})))}).apply(null,[]);

//======
//(defn set "Returns a set of the distinct elements of coll." [coll] (apply hash-set coll))
//---
(function __clojure_fn_5768(){
return (clojure.JS.def(clojure,"set",(function __clojure_fn_5768_set_5770(coll_1){
return (clojure.apply.apply(null,[clojure.hash_set,coll_1]))})))}).apply(null,[]);

//======
//(defn filter-key [keyfn pred amap] (loop [ret {} es (seq amap)] (if es (if (pred (keyfn (first es))) (recur (assoc ret (key (first es)) (val (first es))) (rest es)) (recur ret (rest es))) ret)))
//---
(function __clojure_fn_5786(){
return (clojure.JS.def(clojure,"filter_key",(function __clojure_fn_5786_filter_key_5788(keyfn_1,pred_2,amap_3){
var ret_4,es_5;
return (((function __loop(){var _rtn,_cnt;(ret_4=clojure.lang.PersistentHashMap.EMPTY),
(es_5=clojure.seq.apply(null,[amap_3]));do{_cnt=0;
_rtn=((es_5)?(((pred_2.apply(null,[keyfn_1.apply(null,[clojure.first.apply(null,[es_5])])]))?((_cnt=1,_rtn=[clojure.assoc.apply(null,[ret_4,clojure.key.apply(null,[clojure.first.apply(null,[es_5])]),clojure.val.apply(null,[clojure.first.apply(null,[es_5])])]),clojure.rest.apply(null,[es_5])],ret_4=_rtn[0],es_5=_rtn[1])):((_cnt=1,_rtn=[ret_4,clojure.rest.apply(null,[es_5])],ret_4=_rtn[0],es_5=_rtn[1])))):(ret_4))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn find-ns "Returns the namespace named by the symbol or nil if it doesn't exist." [sym] (clojure.lang.Namespace/find sym))
//---
(function __clojure_fn_5798(){
return (clojure.JS.def(clojure,"find_ns",(function __clojure_fn_5798_find_ns_5800(sym_1){
return (clojure.lang.Namespace.find(sym_1))})))}).apply(null,[]);

//======
//(defn create-ns "Create a new namespace named by the symbol if one doesn't already\n  exist, returns it or the already-existing namespace of the same\n  name." [sym] (clojure.lang.Namespace/findOrCreate sym))
//---
(function __clojure_fn_5810(){
return (clojure.JS.def(clojure,"create_ns",(function __clojure_fn_5810_create_ns_5812(sym_1){
return (clojure.lang.Namespace.findOrCreate(sym_1))})))}).apply(null,[]);

//======
//(defn remove-ns "Removes the namespace named by the symbol. Use with caution.\n  Cannot be used to remove the clojure namespace." [sym] (clojure.lang.Namespace/remove sym))
//---
(function __clojure_fn_5822(){
return (clojure.JS.def(clojure,"remove_ns",(function __clojure_fn_5822_remove_ns_5824(sym_1){
return (clojure.lang.Namespace.remove(sym_1))})))}).apply(null,[]);

//======
//(defn all-ns "Returns a sequence of all namespaces." [] (clojure.lang.Namespace/all))
//---
(function __clojure_fn_5834(){
return (clojure.JS.def(clojure,"all_ns",(function __clojure_fn_5834_all_ns_5836(){
return (clojure.lang.Namespace.all())})))}).apply(null,[]);

//======
//(defn the-ns [x] (if (instance? clojure.lang.Namespace x) x (or (find-ns x) (throw (RT/makeException (str "No namespace: " x " found"))))))
//---
(function __clojure_fn_5846(){
return (clojure.JS.def(clojure,"the_ns",(function __clojure_fn_5846_the_ns_5848(x_1){
var or__202_2;
return (((clojure.instance_QMARK_.apply(null,[clojure.lang.Namespace,x_1]))?(x_1):(((or__202_2=clojure.find_ns.apply(null,[x_1])),
((or__202_2)?(or__202_2):((function __throw(){throw clojure.lang.RT.makeException(clojure.str.apply(null,["No namespace: ",x_1," found"]))})()))))))})))}).apply(null,[]);

//======
//(defn ns-name "Returns the name of the namespace, a symbol." [ns] (.getName (the-ns ns)))
//---
(function __clojure_fn_5858(){
return (clojure.JS.def(clojure,"ns_name",(function __clojure_fn_5858_ns_name_5860(ns_1){
return ((clojure.the_ns.apply(null,[ns_1])).getName())})))}).apply(null,[]);

//======
//(defn ns-map "Returns a map of all the mappings for the namespace." [ns] (.getMappings (the-ns ns)))
//---
(function __clojure_fn_5870(){
return (clojure.JS.def(clojure,"ns_map",(function __clojure_fn_5870_ns_map_5872(ns_1){
return ((clojure.the_ns.apply(null,[ns_1])).getMappings())})))}).apply(null,[]);

//======
//(defn ns-unmap "Removes the mappings for the symbol from the namespace." [ns sym] (.unmap (the-ns ns) sym))
//---
(function __clojure_fn_5882(){
return (clojure.JS.def(clojure,"ns_unmap",(function __clojure_fn_5882_ns_unmap_5884(ns_1,sym_2){
return ((clojure.the_ns.apply(null,[ns_1])).unmap(sym_2))})))}).apply(null,[]);

//======
//(defn ns-publics "Returns a map of the public intern mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (= ns (.ns v)) (.isPublic v))) (ns-map ns))))
//---
(function __clojure_fn_5897(){
return (clojure.JS.def(clojure,"ns_publics",(function __clojure_fn_5897_ns_publics_5899(ns_1){
var ns_2;
return (((ns_2=clojure.the_ns.apply(null,[ns_1])),
clojure.filter_key.apply(null,[clojure.val,(function __clojure_fn_5897_ns_publics_5899_fn_5901(v_1){
var and__196_2,and__196_3;
return (((and__196_2=clojure.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__196_2)?(((and__196_3=clojure.lang.Util.equal(ns_2,(v_1).ns)),
((and__196_3)?((v_1).isPublic()):(and__196_3)))):(and__196_2))))}),clojure.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn ns-imports "Returns a map of the import mappings for the namespace." [ns] (filter-key val class? (ns-map ns)))
//---
(function __clojure_fn_5912(){
return (clojure.JS.def(clojure,"ns_imports",(function __clojure_fn_5912_ns_imports_5914(ns_1){
return (clojure.filter_key.apply(null,[clojure.val,clojure.class_QMARK_,clojure.ns_map.apply(null,[ns_1])]))})))}).apply(null,[]);

//======
//(defn ns-refers "Returns a map of the refer mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (not= ns (.ns v)))) (ns-map ns))))
//---
(function __clojure_fn_5933(){
return (clojure.JS.def(clojure,"ns_refers",(function __clojure_fn_5933_ns_refers_5935(ns_1){
var ns_2;
return (((ns_2=clojure.the_ns.apply(null,[ns_1])),
clojure.filter_key.apply(null,[clojure.val,(function __clojure_fn_5933_ns_refers_5935_fn_5937(v_1){
var and__196_2;
return (((and__196_2=clojure.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__196_2)?(clojure.not_EQ_.apply(null,[ns_2,(v_1).ns])):(and__196_2))))}),clojure.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn ns-interns "Returns a map of the intern mappings for the namespace." [ns] (let [ns (the-ns ns)] (filter-key val (fn [v] (and (instance? clojure.lang.Var v) (= ns (.ns v)))) (ns-map ns))))
//---
(function __clojure_fn_5951(){
return (clojure.JS.def(clojure,"ns_interns",(function __clojure_fn_5951_ns_interns_5953(ns_1){
var ns_2;
return (((ns_2=clojure.the_ns.apply(null,[ns_1])),
clojure.filter_key.apply(null,[clojure.val,(function __clojure_fn_5951_ns_interns_5953_fn_5955(v_1){
var and__196_2;
return (((and__196_2=clojure.instance_QMARK_.apply(null,[clojure.lang.Var,v_1])),
((and__196_2)?(clojure.lang.Util.equal(ns_2,(v_1).ns)):(and__196_2))))}),clojure.ns_map.apply(null,[ns_2])])))})))}).apply(null,[]);

//======
//(defn alias "Add an alias in the current namespace to another\n  namespace. Arguments are two symbols: the alias to be used, and\n  the symbolic name of the target namespace. Use :as in the ns macro in preference \n  to calling this directly." [alias namespace-sym] (.addAlias *ns* alias (find-ns namespace-sym)))
//---
(function __clojure_fn_5966(){
return (clojure.JS.def(clojure,"alias",(function __clojure_fn_5966_alias_5968(alias_1,namespace_sym_2){
return ((clojure._STAR_ns_STAR_).addAlias(alias_1,clojure.find_ns.apply(null,[namespace_sym_2])))})))}).apply(null,[]);

//======
//(defn ns-aliases "Returns a map of the aliases for the namespace." [ns] (.getAliases (the-ns ns)))
//---
(function __clojure_fn_5978(){
return (clojure.JS.def(clojure,"ns_aliases",(function __clojure_fn_5978_ns_aliases_5980(ns_1){
return ((clojure.the_ns.apply(null,[ns_1])).getAliases())})))}).apply(null,[]);

//======
//(defn ns-unalias "Removes the alias for the symbol from the namespace." [ns sym] (.removeAlias (the-ns ns) sym))
//---
(function __clojure_fn_5990(){
return (clojure.JS.def(clojure,"ns_unalias",(function __clojure_fn_5990_ns_unalias_5992(ns_1,sym_2){
return ((clojure.the_ns.apply(null,[ns_1])).removeAlias(sym_2))})))}).apply(null,[]);

//======
//(defn take-nth "Returns a lazy seq of every nth item in coll." [n coll] (when (seq coll) (lazy-cons (first coll) (take-nth n (drop n coll)))))
//---
(function __clojure_fn_6007(){
return (clojure.JS.def(clojure,"take_nth",(function __clojure_fn_6007_take_nth_6009(n_1,coll_2){
return (((clojure.seq.apply(null,[coll_2]))?((new clojure.lang.LazyCons((function __clojure_fn_6007_take_nth_6009_fn_6012(G__6011_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll_2]))}
return (clojure.take_nth.apply(null,[n_1,clojure.drop.apply(null,[n_1,coll_2])]))})))):(null)))})))}).apply(null,[]);

//======
//(defn interleave "Returns a lazy seq of the first item in each coll, then the second\n  etc." [& colls] (apply concat (apply map list colls)))
//---
(function __clojure_fn_6024(){
return (clojure.JS.def(clojure,"interleave",clojure.JS.variadic(0,(function __clojure_fn_6024_interleave_6026(){
var colls_1,colls_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[clojure.concat,clojure.apply.apply(null,[clojure.map,clojure.list,colls_1])]))}))))}).apply(null,[]);

//======
//(defn var-get "Gets the value in the var object" [x] (. x (get)))
//---
(function __clojure_fn_6036(){
return (clojure.JS.def(clojure,"var_get",(function __clojure_fn_6036_var_get_6038(x_1){
return ((x_1).get())})))}).apply(null,[]);

//======
//(defn var-set "Sets the value in the var object to val. The var must be\n thread-locally bound." [x val] (. x (set val)))
//---
(function __clojure_fn_6048(){
return (clojure.JS.def(clojure,"var_set",(function __clojure_fn_6048_var_set_6050(x_1,val_2){
return ((x_1).set(val_2))})))}).apply(null,[]);

//======
//(defn array-map "Constructs an array-map." ([] (. clojure.lang.PersistentArrayMap EMPTY)) ([& keyvals] (new clojure.lang.PersistentArrayMap (to-array keyvals))))
//---
(function __clojure_fn_6079(){
return (clojure.JS.def(clojure,"array_map",clojure.JS.variadic(0,(function __clojure_fn_6079_array_map_6081(){switch(arguments.length){
case 0:return (clojure.lang.PersistentArrayMap.EMPTY)}
var keyvals_1,keyvals_1=clojure.JS.rest_args(this,arguments,0);
return ((new clojure.lang.PersistentArrayMap(clojure.to_array.apply(null,[keyvals_1]))))}))))}).apply(null,[]);

//======
//(defn nthrest "Returns the nth rest of coll, (seq coll) when n is 0." [coll n] (loop [n n xs (seq coll)] (if (and xs (pos? n)) (recur (dec n) (rest xs)) xs)))
//---
(function __clojure_fn_6092(){
return (clojure.JS.def(clojure,"nthrest",(function __clojure_fn_6092_nthrest_6094(coll_1,n_2){
var n_3,and__196_5,xs_4;
return (((function __loop(){var _rtn,_cnt;(n_3=n_2),
(xs_4=clojure.seq.apply(null,[coll_1]));do{_cnt=0;
_rtn=((((and__196_5=xs_4),
((and__196_5)?(clojure.lang.Numbers.isPos(n_3)):(and__196_5))))?((_cnt=1,_rtn=[clojure.lang.Numbers.dec(n_3),clojure.rest.apply(null,[xs_4])],n_3=_rtn[0],xs_4=_rtn[1])):(xs_4))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn symbol? "Return true if x is a Symbol" [x] (instance? clojure.lang.Symbol x))
//---
(function __clojure_fn_6104(){
return (clojure.JS.def(clojure,"symbol_QMARK_",(function __clojure_fn_6104_symbol_QMARK_6106(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Symbol,x_1]))})))}).apply(null,[]);

//======
//(defn keyword? "Return true if x is a Keyword" [x] (instance? clojure.lang.Keyword x))
//---
(function __clojure_fn_6116(){
return (clojure.JS.def(clojure,"keyword_QMARK_",(function __clojure_fn_6116_keyword_QMARK_6118(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Keyword,x_1]))})))}).apply(null,[]);

//======
//(defn destructure [bindings] (let [bmap (apply array-map bindings) pb (fn pb [bvec b v] (let [pvec (fn [bvec b val] (let [gvec (gensym "vec__")] (loop [ret (-> bvec (conj gvec) (conj val)) n 0 bs b seen-rest? false] (if bs (let [firstb (first bs)] (cond (= firstb (quote &)) (recur (pb ret (second bs) (list (quote clojure/nthrest) gvec n)) n (rrest bs) true) (= firstb :as) (pb ret (second bs) gvec) :else (if seen-rest? (throw (RT/makeException "Unsupported binding form, only :as can follow & parameter")) (recur (pb ret firstb (list (quote clojure/nth) gvec n nil)) (inc n) (rest bs) seen-rest?)))) ret)))) pmap (fn [bvec b v] (let [gmap (or (:as b) (gensym "map__")) defaults (:or b)] (loop [ret (-> bvec (conj gmap) (conj v)) bes (reduce (fn [bes entry] (reduce (fn* [p1__6122 p2__6123] (assoc p1__6122 p2__6123 ((val entry) p2__6123))) (dissoc bes (key entry)) ((key entry) bes))) (dissoc b :as :or) {:keys (fn* [p1__6124] (keyword (str p1__6124))), :strs str, :syms (fn* [p1__6125] (list (quote quote) p1__6125))})] (if bes (let [bb (key (first bes)) bk (val (first bes)) has-default (contains? defaults bb)] (recur (pb ret bb (if has-default (list (quote clojure/get) gmap bk (defaults bb)) (list (quote clojure/get) gmap bk))) (rest bes))) ret))))] (cond (symbol? b) (-> bvec (conj b) (conj v)) (vector? b) (pvec bvec b v) (map? b) (pmap bvec b v) :else (throw (RT/makeException (str "Unsupported binding form: " b)))))) process-entry (fn [bvec b] (pb bvec (key b) (val b)))] (if (every? symbol? (keys bmap)) bindings (reduce process-entry [] bmap))))
//---
(function __clojure_fn_6155(){
return (clojure.JS.def(clojure,"destructure",(function __clojure_fn_6155_destructure_6157(bindings_1){
var process_entry_4,bmap_2,pb_3;
return (((bmap_2=clojure.apply.apply(null,[clojure.array_map,bindings_1])),
(pb_3=(function __clojure_fn_6155_destructure_6157_pb_6159(bvec_1,b_2,v_3){
var pvec_4,pmap_5,pb_0=arguments.callee;
return (((pvec_4=(function __clojure_fn_6155_destructure_6157_pb_6159_pvec_6160(bvec_1,b_2,val_3){
var gvec_4,bs_7,seen_rest_QMARK__8,ret_5,firstb_9,n_6;
return (((gvec_4=clojure.gensym.apply(null,["vec__"])),
((function __loop(){var _rtn,_cnt;(ret_5=clojure.conj.apply(null,[clojure.conj.apply(null,[bvec_1,gvec_4]),val_3])),
(n_6=0),
(bs_7=b_2),
(seen_rest_QMARK__8=false);do{_cnt=0;
_rtn=((bs_7)?(((firstb_9=clojure.first.apply(null,[bs_7])),
((clojure.lang.Util.equal(firstb_9,"'&"))?((_cnt=1,_rtn=[pb_0.apply(null,[ret_5,clojure.second.apply(null,[bs_7]),clojure.list.apply(null,["'clojure/nthrest",gvec_4,n_6])]),n_6,clojure.rrest.apply(null,[bs_7]),true],ret_5=_rtn[0],n_6=_rtn[1],bs_7=_rtn[2],seen_rest_QMARK__8=_rtn[3])):(((clojure.lang.Util.equal(firstb_9,clojure.keyword("","as")))?(pb_0.apply(null,[ret_5,clojure.second.apply(null,[bs_7]),gvec_4])):(((clojure.keyword("","else"))?(((seen_rest_QMARK__8)?((function __throw(){throw clojure.lang.RT.makeException("Unsupported binding form, only :as can follow & parameter")})()):((_cnt=1,_rtn=[pb_0.apply(null,[ret_5,firstb_9,clojure.list.apply(null,["'clojure/nth",gvec_4,n_6,null])]),clojure.lang.Numbers.inc(n_6),clojure.rest.apply(null,[bs_7]),seen_rest_QMARK__8],ret_5=_rtn[0],n_6=_rtn[1],bs_7=_rtn[2],seen_rest_QMARK__8=_rtn[3])))):(null)))))))):(ret_5))}while(_cnt);return _rtn;})())))})),
(pmap_5=(function __clojure_fn_6155_destructure_6157_pb_6159_pmap_6163(bvec_1,b_2,v_3){
var gmap_4,bk_9,has_default_10,ret_6,or__202_4,bes_7,bb_8,defaults_5;
return (((gmap_4=((or__202_4=clojure.keyword("","as").apply(null,[b_2])),
((or__202_4)?(or__202_4):(clojure.gensym.apply(null,["map__"]))))),
(defaults_5=clojure.keyword("","or").apply(null,[b_2])),
((function __loop(){var _rtn,_cnt;(ret_6=clojure.conj.apply(null,[clojure.conj.apply(null,[bvec_1,gmap_4]),v_3])),
(bes_7=clojure.reduce.apply(null,[(function __clojure_fn_6155_destructure_6157_pb_6159_pmap_6163_fn_6165(bes_1,entry_2){
return (clojure.reduce.apply(null,[(function __clojure_fn_6155_destructure_6157_pb_6159_pmap_6163_fn_6165_fn_6167(p1__6122_1,p2__6123_2){
return (clojure.assoc.apply(null,[p1__6122_1,p2__6123_2,clojure.val.apply(null,[entry_2]).apply(null,[p2__6123_2])]))}),clojure.dissoc.apply(null,[bes_1,clojure.key.apply(null,[entry_2])]),clojure.key.apply(null,[entry_2]).apply(null,[bes_1])]))}),clojure.dissoc.apply(null,[b_2,clojure.keyword("","as"),clojure.keyword("","or")]),clojure.hash_map(clojure.keyword("","keys"),(function __clojure_fn_6155_destructure_6157_pb_6159_pmap_6163_fn_6171(p1__6124_1){
return (clojure.keyword.apply(null,[clojure.str.apply(null,[p1__6124_1])]))}),clojure.keyword("","strs"),clojure.str,clojure.keyword("","syms"),(function __clojure_fn_6155_destructure_6157_pb_6159_pmap_6163_fn_6174(p1__6125_1){
return (clojure.list.apply(null,["'quote",p1__6125_1]))}))]));do{_cnt=0;
_rtn=((bes_7)?(((bb_8=clojure.key.apply(null,[clojure.first.apply(null,[bes_7])])),
(bk_9=clojure.val.apply(null,[clojure.first.apply(null,[bes_7])])),
(has_default_10=clojure.contains_QMARK_.apply(null,[defaults_5,bb_8])),
(_cnt=1,_rtn=[pb_0.apply(null,[ret_6,bb_8,((has_default_10)?(clojure.list.apply(null,["'clojure/get",gmap_4,bk_9,defaults_5.apply(null,[bb_8])])):(clojure.list.apply(null,["'clojure/get",gmap_4,bk_9])))]),clojure.rest.apply(null,[bes_7])],ret_6=_rtn[0],bes_7=_rtn[1]))):(ret_6))}while(_cnt);return _rtn;})())))})),
((clojure.symbol_QMARK_.apply(null,[b_2]))?(clojure.conj.apply(null,[clojure.conj.apply(null,[bvec_1,b_2]),v_3])):(((clojure.vector_QMARK_.apply(null,[b_2]))?(pvec_4.apply(null,[bvec_1,b_2,v_3])):(((clojure.map_QMARK_.apply(null,[b_2]))?(pmap_5.apply(null,[bvec_1,b_2,v_3])):(((clojure.keyword("","else"))?((function __throw(){throw clojure.lang.RT.makeException(clojure.str.apply(null,["Unsupported binding form: ",b_2]))})()):(null))))))))))})),
(process_entry_4=(function __clojure_fn_6155_destructure_6157_process_entry_6179(bvec_1,b_2){
return (pb_3.apply(null,[bvec_1,clojure.key.apply(null,[b_2]),clojure.val.apply(null,[b_2])]))})),
((clojure.every_QMARK_.apply(null,[clojure.symbol_QMARK_,clojure.keys.apply(null,[bmap_2])]))?(bindings_1):(clojure.reduce.apply(null,[process_entry_4,clojure.lang.PersistentVector.EMPTY,bmap_2])))))})))}).apply(null,[]);

//======
//(defn pr-str "pr to a string, returning it" {:tag String} [& xs] (with-out-str (apply pr xs)))
//---
(function __clojure_fn_6280(){
return (clojure.JS.def(clojure,"pr_str",clojure.JS.variadic(0,(function __clojure_fn_6280_pr_str_6282(){
var xs_1,s__1235_2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1235_2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_out_STAR_,s__1235_2])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.pr,xs_1]),
clojure.str.apply(null,[s__1235_2]))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn prn-str "prn to a string, returning it" {:tag String} [& xs] (with-out-str (apply prn xs)))
//---
(function __clojure_fn_6292(){
return (clojure.JS.def(clojure,"prn_str",clojure.JS.variadic(0,(function __clojure_fn_6292_prn_str_6294(){
var s__1235_2,xs_1,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1235_2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_out_STAR_,s__1235_2])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.prn,xs_1]),
clojure.str.apply(null,[s__1235_2]))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn print-str "print to a string, returning it" {:tag String} [& xs] (with-out-str (apply print xs)))
//---
(function __clojure_fn_6304(){
return (clojure.JS.def(clojure,"print_str",clojure.JS.variadic(0,(function __clojure_fn_6304_print_str_6306(){
var xs_1,s__1235_2,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1235_2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_out_STAR_,s__1235_2])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.print,xs_1]),
clojure.str.apply(null,[s__1235_2]))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn println-str "println to a string, returning it" {:tag String} [& xs] (with-out-str (apply println xs)))
//---
(function __clojure_fn_6316(){
return (clojure.JS.def(clojure,"println_str",clojure.JS.variadic(0,(function __clojure_fn_6316_println_str_6318(){
var s__1235_2,xs_1,xs_1=clojure.JS.rest_args(this,arguments,0);
return (((s__1235_2=clojure.lang.RT.makeStringWriter()),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_out_STAR_,s__1235_2])),
(function __try(){try{var _rtn=(clojure.apply.apply(null,[clojure.println,xs_1]),
clojure.str.apply(null,[s__1235_2]))}
finally{clojure.lang.Var.popThreadBindings()}})()))}))))}).apply(null,[]);

//======
//(defn test "test [v] finds fn at key :test in var metadata and calls it,\n  presuming failure will throw exception" [v] (let [f (:test (clojure/meta v))] (if f (do (f) :ok) :no-test)))
//---
(function __clojure_fn_6334(){
return (clojure.JS.def(clojure,"test",(function __clojure_fn_6334_test_6336(v_1){
var f_2;
return (((f_2=clojure.keyword("","test").apply(null,[clojure.meta.apply(null,[v_1])])),
((f_2)?(f_2.apply(null,[]),
clojure.keyword("","ok")):(clojure.keyword("","no-test")))))})))}).apply(null,[]);

//======
//(defn rand "Returns a random floating point number between 0 (inclusive) and\n  1 (exclusive)." ([] (RT/random)) ([n] (* n (rand))))
//---
(function __clojure_fn_6391(){
return (clojure.JS.def(clojure,"rand",(function __clojure_fn_6391_rand_6393(n_1){switch(arguments.length){
case 0:return (clojure.lang.RT.random())}
return (clojure.lang.Numbers.multiply(n_1,clojure.rand.apply(null,[])))})))}).apply(null,[]);

//======
//(defn rand-int "Returns a random integer between 0 (inclusive) and n (exclusive)." [n] (int (rand n)))
//---
(function __clojure_fn_6404(){
return (clojure.JS.def(clojure,"rand_int",(function __clojure_fn_6404_rand_int_6406(n_1){
return (clojure.lang.RT.intCast(clojure.rand.apply(null,[n_1])))})))}).apply(null,[]);

//======
//(defn print-doc [v] (println "-------------------------") (println (str (ns-name (:ns (clojure/meta v))) "/" (:name (clojure/meta v)))) (prn (:arglists (clojure/meta v))) (when (:macro (clojure/meta v)) (println "Macro")) (println " " (:doc (clojure/meta v))))
//---
(function __clojure_fn_6422(){
return (clojure.JS.def(clojure,"print_doc",(function __clojure_fn_6422_print_doc_6424(v_1){
return (clojure.println.apply(null,["-------------------------"]),
clojure.println.apply(null,[clojure.str.apply(null,[clojure.ns_name.apply(null,[clojure.keyword("","ns").apply(null,[clojure.meta.apply(null,[v_1])])]),"/",clojure.keyword("","name").apply(null,[clojure.meta.apply(null,[v_1])])])]),
clojure.prn.apply(null,[clojure.keyword("","arglists").apply(null,[clojure.meta.apply(null,[v_1])])]),
((clojure.keyword("","macro").apply(null,[clojure.meta.apply(null,[v_1])]))?(clojure.println.apply(null,["Macro"])):(null)),
clojure.println.apply(null,[" ",clojure.keyword("","doc").apply(null,[clojure.meta.apply(null,[v_1])])]))})))}).apply(null,[]);

//======
//(defn find-doc "Prints documentation for any var whose documentation or name\n contains a match for re-string" [re-string] (let [re (re-pattern re-string)] (dorun (for [ns (all-ns) v (sort-by (comp :name meta) (vals (ns-interns ns))) :when (and (:doc (clojure/meta v)) (or (re-find (re-matcher re (:doc (clojure/meta v)))) (re-find (re-matcher re (str (:name (clojure/meta v)))))))] (print-doc v)))))
//---
(function __clojure_fn_6454(){
return (clojure.JS.def(clojure,"find_doc",(function __clojure_fn_6454_find_doc_6456(re_string_1){
var iter__1211_3,re_2;
return (((re_2=clojure.re_pattern.apply(null,[re_string_1])),
clojure.dorun.apply(null,[((iter__1211_3=(function __clojure_fn_6454_find_doc_6456_iter_6458_6462(s__6459_1){
var iter__1204_4,ns_2,iterys__1210_3,iter__6458_0=arguments.callee;
return (((clojure.seq.apply(null,[s__6459_1]))?(((ns_2=clojure.first.apply(null,[s__6459_1])),
((true)?(((iterys__1210_3=(function __clojure_fn_6454_find_doc_6456_iter_6458_6462_iter_6460_6463(s__6461_1){
var _cnt,_rtn,v_2,and__196_3,or__202_4,iter__6460_0=arguments.callee;
do{_cnt=0;_rtn=((clojure.seq.apply(null,[s__6461_1]))?(((v_2=clojure.first.apply(null,[s__6461_1])),
((((and__196_3=clojure.keyword("","doc").apply(null,[clojure.meta.apply(null,[v_2])])),
((and__196_3)?(((or__202_4=clojure.re_find.apply(null,[clojure.re_matcher.apply(null,[re_2,clojure.keyword("","doc").apply(null,[clojure.meta.apply(null,[v_2])])])])),
((or__202_4)?(or__202_4):(clojure.re_find.apply(null,[clojure.re_matcher.apply(null,[re_2,clojure.str.apply(null,[clojure.keyword("","name").apply(null,[clojure.meta.apply(null,[v_2])])])])]))))):(and__196_3))))?((new clojure.lang.LazyCons((function __clojure_fn_6454_find_doc_6456_iter_6458_6462_iter_6460_6463_fn_6465(G__6464_1){switch(arguments.length){
case 0:return (clojure.print_doc.apply(null,[v_2]))}
return (iter__6460_0.apply(null,[clojure.rest.apply(null,[s__6461_1])]))})))):((_cnt=1,_rtn=[clojure.rest.apply(null,[s__6461_1])],s__6461_1=_rtn[0]))))):(null))
}while(_cnt);return _rtn;})),
((iter__1204_4=(function __clojure_fn_6454_find_doc_6456_iter_6458_6462_iter_1204_6470(coll__1205_1){
var iter__1204_0=arguments.callee;
return (((clojure.seq.apply(null,[coll__1205_1]))?((new clojure.lang.LazyCons((function __clojure_fn_6454_find_doc_6456_iter_6458_6462_iter_1204_6470_fn_6472(G__6471_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll__1205_1]))}
return (iter__1204_0.apply(null,[clojure.rest.apply(null,[coll__1205_1])]))})))):(clojure.seq.apply(null,[iter__6458_0.apply(null,[clojure.rest.apply(null,[s__6459_1])])]))))})),
iter__1204_4.apply(null,[iterys__1210_3.apply(null,[clojure.sort_by.apply(null,[clojure.comp.apply(null,[clojure.keyword("","name"),clojure.meta]),clojure.vals.apply(null,[clojure.ns_interns.apply(null,[ns_2])])])])])))):(null)))):(null)))})),
iter__1211_3.apply(null,[clojure.all_ns.apply(null,[])]))])))})))}).apply(null,[]);

//======
//(defn special-form-anchor "Returns the anchor tag on http://clojure.org/special_forms for the\n  special form x, or nil" [x] (#{(quote recur) (quote .) (quote var) (quote let) (quote quote) (quote set!) (quote monitor-enter) (quote loop) (quote new) (quote fn) (quote if) (quote try) (quote def) (quote monitor-exit) (quote throw) (quote do)} x))
//---
(function __clojure_fn_6486(){
return (clojure.JS.def(clojure,"special_form_anchor",(function __clojure_fn_6486_special_form_anchor_6488(x_1){
return (clojure.hash_set("'recur","'.","'var","'let","'quote","'set!","'monitor-enter","'loop","'new","'fn","'if","'try","'def","'monitor-exit","'throw","'do").apply(null,[x_1]))})))}).apply(null,[]);

//======
//(defn syntax-symbol-anchor "Returns the anchor tag on http://clojure.org/special_forms for the\n  special form that uses syntax symbol x, or nil" [x] ({(quote &) (quote fn), (quote catch) (quote try), (quote finally) (quote try)} x))
//---
(function __clojure_fn_6498(){
return (clojure.JS.def(clojure,"syntax_symbol_anchor",(function __clojure_fn_6498_syntax_symbol_anchor_6500(x_1){
return (clojure.hash_map("'&","'fn","'catch","'try","'finally","'try").apply(null,[x_1]))})))}).apply(null,[]);

//======
//(defn print-special-doc [name type anchor] (println "-------------------------") (println name) (println type) (println (str "  Please see http://clojure.org/special_forms#" anchor)))
//---
(function __clojure_fn_6510(){
return (clojure.JS.def(clojure,"print_special_doc",(function __clojure_fn_6510_print_special_doc_6512(name_1,type_2,anchor_3){
return (clojure.println.apply(null,["-------------------------"]),
clojure.println.apply(null,[name_1]),
clojure.println.apply(null,[type_2]),
clojure.println.apply(null,[clojure.str.apply(null,["  Please see http://clojure.org/special_forms#",anchor_3])]))})))}).apply(null,[]);

//======
//(defn tree-seq "returns a lazy sequence of the nodes in a tree, via a depth-first walk.\n  branch? must be a fn of one arg that returns true if passed a node\n  that can have children (but may not).  children must be a fn of one\n  arg that returns a sequence of the children. Will only be called on\n  nodes for which branch? returns true. Root is the root node of the\n  tree, must be a branch." [branch? children root] (let [walk (fn walk [nodes] (when-first node nodes (lazy-cons node (if (branch? node) (lazy-cat (walk (children node)) (walk (rest nodes))) (walk (rest nodes))))))] (lazy-cons root (walk (children root)))))
//---
(function __clojure_fn_6547(){
return (clojure.JS.def(clojure,"tree_seq",(function __clojure_fn_6547_tree_seq_6549(branch_QMARK__1,children_2,root_3){
var walk_4;
return (((walk_4=(function __clojure_fn_6547_tree_seq_6549_walk_6551(nodes_1){
var node_2,walk_0=arguments.callee;
return (((clojure.seq.apply(null,[nodes_1]))?(((node_2=clojure.first.apply(null,[nodes_1])),
(new clojure.lang.LazyCons((function __clojure_fn_6547_tree_seq_6549_walk_6551_fn_6553(G__6552_1){switch(arguments.length){
case 0:return (node_2)}
var iter__1204_2;
return (((branch_QMARK__1.apply(null,[node_2]))?(((iter__1204_2=(function __clojure_fn_6547_tree_seq_6549_walk_6551_fn_6553_iter_1204_6556(coll__1205_1){
var iter__1204_0=arguments.callee;
return (((clojure.seq.apply(null,[coll__1205_1]))?((new clojure.lang.LazyCons((function __clojure_fn_6547_tree_seq_6549_walk_6551_fn_6553_iter_1204_6556_fn_6558(G__6557_1){switch(arguments.length){
case 0:return (clojure.first.apply(null,[coll__1205_1]))}
return (iter__1204_0.apply(null,[clojure.rest.apply(null,[coll__1205_1])]))})))):(clojure.seq.apply(null,[walk_0.apply(null,[clojure.rest.apply(null,[nodes_1])])]))))})),
iter__1204_2.apply(null,[walk_0.apply(null,[children_2.apply(null,[node_2])])]))):(walk_0.apply(null,[clojure.rest.apply(null,[nodes_1])]))))}))))):(null)))})),
(new clojure.lang.LazyCons((function __clojure_fn_6547_tree_seq_6549_fn_6566(G__6565_1){switch(arguments.length){
case 0:return (root_3)}
return (walk_4.apply(null,[children_2.apply(null,[root_3])]))})))))})))}).apply(null,[]);

//======
//(defn file-seq "A tree seq on java.io.Files" [dir] (tree-seq (fn [f] (. f (isDirectory))) (fn [d] (seq (. d (listFiles)))) dir))
//---
(function __clojure_fn_6584(){
return (clojure.JS.def(clojure,"file_seq",(function __clojure_fn_6584_file_seq_6586(dir_1){
return (clojure.tree_seq.apply(null,[(function __clojure_fn_6584_file_seq_6586_fn_6588(f_1){
return ((f_1).isDirectory())}),(function __clojure_fn_6584_file_seq_6586_fn_6591(d_1){
return (clojure.seq.apply(null,[(d_1).listFiles()]))}),dir_1]))})))}).apply(null,[]);

//======
//(defn xml-seq "A tree seq on the xml elements as per xml/parse" [root] (tree-seq (complement string?) (comp seq :content) root))
//---
(function __clojure_fn_6602(){
return (clojure.JS.def(clojure,"xml_seq",(function __clojure_fn_6602_xml_seq_6604(root_1){
return (clojure.tree_seq.apply(null,[clojure.complement.apply(null,[clojure.string_QMARK_]),clojure.comp.apply(null,[clojure.seq,clojure.keyword("","content")]),root_1]))})))}).apply(null,[]);

//======
//(defn var? "Returns true if v is of type clojure.lang.Var" [v] (instance? clojure.lang.Var v))
//---
(function __clojure_fn_6620(){
return (clojure.JS.def(clojure,"var_QMARK_",(function __clojure_fn_6620_var_QMARK_6622(v_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Var,v_1]))})))}).apply(null,[]);

//======
//(defn subs "Returns the substring of s beginning at start inclusive, and ending\n  at end (defaults to length of string), exclusive." ([s start] (. s (substring start))) ([s start end] (. s (substring start end))))
//---
(function __clojure_fn_6639(){
return (clojure.JS.def(clojure,"subs",(function __clojure_fn_6639_subs_6641(s_1,start_2,end_3){switch(arguments.length){
case 2:return ((s_1).substring(start_2))}
return ((s_1).substring(start_2,end_3))})))}).apply(null,[]);

//======
//(defn max-key "Returns the x for which (k x), a number, is greatest." ([k x] x) ([k x y] (if (> (k x) (k y)) x y)) ([k x y & more] (reduce (fn* [p1__6646 p2__6647] (max-key k p1__6646 p2__6647)) (max-key k x y) more)))
//---
(function __clojure_fn_6659(){
return (clojure.JS.def(clojure,"max_key",clojure.JS.variadic(3,(function __clojure_fn_6659_max_key_6661(k_1,x_2,y_3){switch(arguments.length){
case 3:return (((clojure.lang.Numbers.gt(k_1.apply(null,[x_2]),k_1.apply(null,[y_3])))?(x_2):(y_3)))
case 2:return (x_2)}
var more_4,more_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.reduce.apply(null,[(function __clojure_fn_6659_max_key_6661_fn_6665(p1__6646_1,p2__6647_2){
return (clojure.max_key.apply(null,[k_1,p1__6646_1,p2__6647_2]))}),clojure.max_key.apply(null,[k_1,x_2,y_3]),more_4]))}))))}).apply(null,[]);

//======
//(defn min-key "Returns the x for which (k x), a number, is least." ([k x] x) ([k x y] (if (< (k x) (k y)) x y)) ([k x y & more] (reduce (fn* [p1__6670 p2__6671] (min-key k p1__6670 p2__6671)) (min-key k x y) more)))
//---
(function __clojure_fn_6683(){
return (clojure.JS.def(clojure,"min_key",clojure.JS.variadic(3,(function __clojure_fn_6683_min_key_6685(k_1,x_2,y_3){switch(arguments.length){
case 3:return (((clojure.lang.Numbers.lt(k_1.apply(null,[x_2]),k_1.apply(null,[y_3])))?(x_2):(y_3)))
case 2:return (x_2)}
var more_4,more_4=clojure.JS.rest_args(this,arguments,3);
return (clojure.reduce.apply(null,[(function __clojure_fn_6683_min_key_6685_fn_6689(p1__6670_1,p2__6671_2){
return (clojure.min_key.apply(null,[k_1,p1__6670_1,p2__6671_2]))}),clojure.min_key.apply(null,[k_1,x_2,y_3]),more_4]))}))))}).apply(null,[]);

//======
//(defn distinct "Returns a lazy sequence of the elements of coll with duplicates removed" [coll] (let [step (fn step [[f & r :as xs] seen] (when xs (if (seen f) (recur r seen) (lazy-cons f (step r (conj seen f))))))] (step (seq coll) #{})))
//---
(function __clojure_fn_6709(){
return (clojure.JS.def(clojure,"distinct",(function __clojure_fn_6709_distinct_6711(coll_1){
var step_2;
return (((step_2=(function __clojure_fn_6709_distinct_6711_step_6714(p__6713_1,seen_2){
var _cnt,_rtn,vec__6715_3,f_4,r_5,xs_6,step_0=arguments.callee;
do{_cnt=0;_rtn=((vec__6715_3=p__6713_1),
(f_4=clojure.nth.apply(null,[vec__6715_3,0,null])),
(r_5=clojure.nthrest.apply(null,[vec__6715_3,1])),
(xs_6=vec__6715_3),
((xs_6)?(((seen_2.apply(null,[f_4]))?((_cnt=1,_rtn=[r_5,seen_2],p__6713_1=_rtn[0],seen_2=_rtn[1])):((new clojure.lang.LazyCons((function __clojure_fn_6709_distinct_6711_step_6714_fn_6717(G__6716_1){switch(arguments.length){
case 0:return (f_4)}
return (step_0.apply(null,[r_5,clojure.conj.apply(null,[seen_2,f_4])]))})))))):(null)))
}while(_cnt);return _rtn;})),
step_2.apply(null,[clojure.seq.apply(null,[coll_1]),clojure.lang.PersistentHashSet.EMPTY])))})))}).apply(null,[]);

//======
//(defn replace "Given a map of replacement pairs and a vector/collection, returns a\n  vector/seq with any elements = a key in smap replaced with the\n  corresponding val in smap" [smap coll] (if (vector? coll) (reduce (fn [v i] (if-let e (find smap (nth v i)) (assoc v i (val e)) v)) coll (range (count coll))) (map (fn* [p1__6739] (if-let e (find smap p1__6739) (val e) p1__6739)) coll)))
//---
(function __clojure_fn_6752(){
return (clojure.JS.def(clojure,"replace",(function __clojure_fn_6752_replace_6754(smap_1,coll_2){
return (((clojure.vector_QMARK_.apply(null,[coll_2]))?(clojure.reduce.apply(null,[(function __clojure_fn_6752_replace_6754_fn_6756(v_1,i_2){
var e_4,temp__1414_3;
return (((temp__1414_3=clojure.find.apply(null,[smap_1,clojure.nth.apply(null,[v_1,i_2])])),
((temp__1414_3)?(((e_4=temp__1414_3),
clojure.assoc.apply(null,[v_1,i_2,clojure.val.apply(null,[e_4])]))):(v_1))))}),coll_2,clojure.range.apply(null,[clojure.count.apply(null,[coll_2])])])):(clojure.map.apply(null,[(function __clojure_fn_6752_replace_6754_fn_6759(p1__6739_1){
var temp__1414_2,e_3;
return (((temp__1414_2=clojure.find.apply(null,[smap_1,p1__6739_1])),
((temp__1414_2)?(((e_3=temp__1414_2),
clojure.val.apply(null,[e_3]))):(p1__6739_1))))}),coll_2]))))})))}).apply(null,[]);

//======
//(defn bound-fn {:private true} [sc test key] (fn [e] (test (.. sc comparator (compare (. sc entryKey e) key)) 0)))
//---
(function __clojure_fn_6786(){
return (clojure.JS.def(clojure,"bound_fn",(function __clojure_fn_6786_bound_fn_6788(sc_1,test_2,key_3){
return ((function __clojure_fn_6786_bound_fn_6788_fn_6790(e_1){
return (test_2.apply(null,[((sc_1).comparator()).compare((sc_1).entryKey(e_1),key_3),0]))}))})))}).apply(null,[]);

//======
//(defn subseq "sc must be a sorted collection, test(s) one of <, <=, > or\n  >=. Returns a seq of those entries with keys ek for\n  which (test (.. sc comparator (compare ek key)) 0) is true" ([sc test key] (let [include (bound-fn sc test key)] (if (#{> >=} test) (when-let [e :as s] (. sc seqFrom key true) (if (include e) s (rest s))) (take-while include (. sc seq true))))) ([sc start-test start-key end-test end-key] (when-let [e :as s] (. sc seqFrom start-key true) (take-while (bound-fn sc end-test end-key) (if ((bound-fn sc start-test start-key) e) s (rest s))))))
//---
(function __clojure_fn_6804(){
return (clojure.JS.def(clojure,"subseq",(function __clojure_fn_6804_subseq_6806(sc_1,start_test_2,start_key_3,end_test_4,end_key_5){switch(arguments.length){
case 3:var e_7,temp__1419_5,include_4,vec__6808_6,s_8,test_2=arguments[1],key_3=arguments[2];
return (((include_4=clojure.bound_fn.apply(null,[sc_1,test_2,key_3])),
((clojure.hash_set(clojure._GT_,clojure._GT__EQ_).apply(null,[test_2]))?(((temp__1419_5=(sc_1).seqFrom(key_3,true)),
((temp__1419_5)?(((vec__6808_6=temp__1419_5),
(e_7=clojure.nth.apply(null,[vec__6808_6,0,null])),
(s_8=vec__6808_6),
((include_4.apply(null,[e_7]))?(s_8):(clojure.rest.apply(null,[s_8]))))):(null)))):(clojure.take_while.apply(null,[include_4,(sc_1).seq(true)])))))}
var temp__1419_6,vec__6810_7,s_9,e_8;
return (((temp__1419_6=(sc_1).seqFrom(start_key_3,true)),
((temp__1419_6)?(((vec__6810_7=temp__1419_6),
(e_8=clojure.nth.apply(null,[vec__6810_7,0,null])),
(s_9=vec__6810_7),
clojure.take_while.apply(null,[clojure.bound_fn.apply(null,[sc_1,end_test_4,end_key_5]),((clojure.bound_fn.apply(null,[sc_1,start_test_2,start_key_3]).apply(null,[e_8]))?(s_9):(clojure.rest.apply(null,[s_9])))]))):(null))))})))}).apply(null,[]);

//======
//(defn rsubseq "sc must be a sorted collection, test(s) one of <, <=, > or\n  >=. Returns a reverse seq of those entries with keys ek for\n  which (test (.. sc comparator (compare ek key)) 0) is true" ([sc test key] (let [include (bound-fn sc test key)] (if (#{< <=} test) (when-let [e :as s] (. sc seqFrom key false) (if (include e) s (rest s))) (take-while include (. sc seq false))))) ([sc start-test start-key end-test end-key] (when-let [e :as s] (. sc seqFrom end-key false) (take-while (bound-fn sc start-test start-key) (if ((bound-fn sc end-test end-key) e) s (rest s))))))
//---
(function __clojure_fn_6822(){
return (clojure.JS.def(clojure,"rsubseq",(function __clojure_fn_6822_rsubseq_6824(sc_1,start_test_2,start_key_3,end_test_4,end_key_5){switch(arguments.length){
case 3:var e_7,temp__1419_5,s_8,vec__6826_6,include_4,test_2=arguments[1],key_3=arguments[2];
return (((include_4=clojure.bound_fn.apply(null,[sc_1,test_2,key_3])),
((clojure.hash_set(clojure._LT_,clojure._LT__EQ_).apply(null,[test_2]))?(((temp__1419_5=(sc_1).seqFrom(key_3,false)),
((temp__1419_5)?(((vec__6826_6=temp__1419_5),
(e_7=clojure.nth.apply(null,[vec__6826_6,0,null])),
(s_8=vec__6826_6),
((include_4.apply(null,[e_7]))?(s_8):(clojure.rest.apply(null,[s_8]))))):(null)))):(clojure.take_while.apply(null,[include_4,(sc_1).seq(false)])))))}
var s_9,temp__1419_6,e_8,vec__6828_7;
return (((temp__1419_6=(sc_1).seqFrom(end_key_5,false)),
((temp__1419_6)?(((vec__6828_7=temp__1419_6),
(e_8=clojure.nth.apply(null,[vec__6828_7,0,null])),
(s_9=vec__6828_7),
clojure.take_while.apply(null,[clojure.bound_fn.apply(null,[sc_1,start_test_2,start_key_3]),((clojure.bound_fn.apply(null,[sc_1,end_test_4,end_key_5]).apply(null,[e_8]))?(s_9):(clojure.rest.apply(null,[s_9])))]))):(null))))})))}).apply(null,[]);

//======
//(defn repeatedly "Takes a function of no args, presumably with side effects, and returns an infinite\n  lazy sequence of calls to it" [f] (lazy-cons (f) (repeatedly f)))
//---
(function __clojure_fn_6842(){
return (clojure.JS.def(clojure,"repeatedly",(function __clojure_fn_6842_repeatedly_6844(f_1){
return ((new clojure.lang.LazyCons((function __clojure_fn_6842_repeatedly_6844_fn_6847(G__6846_1){switch(arguments.length){
case 0:return (f_1.apply(null,[]))}
return (clojure.repeatedly.apply(null,[f_1]))}))))})))}).apply(null,[]);

//======
//(defn add-classpath "Adds the url (String or URL object) to the classpath per URLClassLoader.addURL" [url] (. clojure.lang.RT addURL url))
//---
(function __clojure_fn_6859(){
return (clojure.JS.def(clojure,"add_classpath",(function __clojure_fn_6859_add_classpath_6861(url_1){
return (clojure.lang.RT.addURL(url_1))})))}).apply(null,[]);

//======
//(defn hash "Returns the hash code of its argument" [x] (. clojure.lang.Util (hash x)))
//---
(function __clojure_fn_6871(){
return (clojure.JS.def(clojure,"hash",(function __clojure_fn_6871_hash_6873(x_1){
return (clojure.lang.Util.hash(x_1))})))}).apply(null,[]);

//======
//(defn interpose "Returns a lazy seq of the elements of coll separated by sep" [sep coll] (drop 1 (interleave (repeat sep) coll)))
//---
(function __clojure_fn_6883(){
return (clojure.JS.def(clojure,"interpose",(function __clojure_fn_6883_interpose_6885(sep_1,coll_2){
return (clojure.drop.apply(null,[1,clojure.interleave.apply(null,[clojure.repeat.apply(null,[sep_1]),coll_2])]))})))}).apply(null,[]);

//======
//(defn partition "Returns a lazy sequence of lists of n items each, at offsets step\n  apart. If step is not supplied, defaults to n, i.e. the partitions\n  do not overlap." ([n coll] (partition n n coll)) ([n step coll] (when (seq coll) (let [p (take n coll)] (when (= n (count p)) (lazy-cons p (partition n step (drop step coll))))))))
//---
(function __clojure_fn_6901(){
return (clojure.JS.def(clojure,"partition",(function __clojure_fn_6901_partition_6903(n_1,step_2,coll_3){switch(arguments.length){
case 2:var coll_2=arguments[1];
return (clojure.partition.apply(null,[n_1,n_1,coll_2]))}
var p_4;
return (((clojure.seq.apply(null,[coll_3]))?(((p_4=clojure.take.apply(null,[n_1,coll_3])),
((clojure.lang.Util.equal(n_1,clojure.count.apply(null,[p_4])))?((new clojure.lang.LazyCons((function __clojure_fn_6901_partition_6903_fn_6907(G__6906_1){switch(arguments.length){
case 0:return (p_4)}
return (clojure.partition.apply(null,[n_1,step_2,clojure.drop.apply(null,[step_2,coll_3])]))})))):(null)))):(null)))})))}).apply(null,[]);

//======
//(defn empty "Returns an empty collection of the same category as coll, or nil" [coll] (.empty coll))
//---
(function __clojure_fn_6927(){
return (clojure.JS.def(clojure,"empty",(function __clojure_fn_6927_empty_6929(coll_1){
return ((coll_1).empty())})))}).apply(null,[]);

//======
//(defn float-array "Creates an array of floats" {:inline-arities #{1 2}, :inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/float_array)) args))} ([size-or-seq] (. clojure.lang.Numbers float_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers float_array size init-val-or-seq)))
//---
(function __clojure_fn_6957(){
return (clojure.JS.def(clojure,"float_array",(function __clojure_fn_6957_float_array_6962(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.float_array(size_or_seq_1))}
return (clojure.lang.Numbers.float_array(size_1,init_val_or_seq_2))})))}).apply(null,[]);

//======
//(defn double-array "Creates an array of doubles" {:inline-arities #{1 2}, :inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/double_array)) args))} ([size-or-seq] (. clojure.lang.Numbers double_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers double_array size init-val-or-seq)))
//---
(function __clojure_fn_6977(){
return (clojure.JS.def(clojure,"double_array",(function __clojure_fn_6977_double_array_6982(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.double_array(size_or_seq_1))}
return (clojure.lang.Numbers.double_array(size_1,init_val_or_seq_2))})))}).apply(null,[]);

//======
//(defn int-array "Creates an array of ints" {:inline-arities #{1 2}, :inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/int_array)) args))} ([size-or-seq] (. clojure.lang.Numbers int_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers int_array size init-val-or-seq)))
//---
(function __clojure_fn_6997(){
return (clojure.JS.def(clojure,"int_array",(function __clojure_fn_6997_int_array_7002(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.int_array(size_or_seq_1))}
return (clojure.lang.Numbers.int_array(size_1,init_val_or_seq_2))})))}).apply(null,[]);

//======
//(defn long-array "Creates an array of ints" {:inline-arities #{1 2}, :inline (fn [& args] (clojure/concat (clojure/list (quote .)) (clojure/list (quote clojure.lang.Numbers)) (clojure/list (quote clojure/long_array)) args))} ([size-or-seq] (. clojure.lang.Numbers long_array size-or-seq)) ([size init-val-or-seq] (. clojure.lang.Numbers long_array size init-val-or-seq)))
//---
(function __clojure_fn_7017(){
return (clojure.JS.def(clojure,"long_array",(function __clojure_fn_7017_long_array_7022(size_1,init_val_or_seq_2){switch(arguments.length){
case 1:var size_or_seq_1=arguments[0];
return (clojure.lang.Numbers.long_array(size_or_seq_1))}
return (clojure.lang.Numbers.long_array(size_1,init_val_or_seq_2))})))}).apply(null,[]);

//======
//(import (quote (java.util.concurrent BlockingQueue LinkedBlockingQueue)))
//---
(function __clojure_fn_7078(){
return (clojure.import_.apply(null,[clojure.JS.lit_list(["'java.util.concurrent","'BlockingQueue","'LinkedBlockingQueue"])]))}).apply(null,[]);

//======
//(defn alter-var-root "Atomically alters the root binding of var v by applying f to its\n  current value plus any args" [v f & args] (.alterRoot v f args))
//---
(function __clojure_fn_7108(){
return (clojure.JS.def(clojure,"alter_var_root",clojure.JS.variadic(2,(function __clojure_fn_7108_alter_var_root_7110(v_1,f_2){
var args_3,args_3=clojure.JS.rest_args(this,arguments,2);
return ((v_1).alterRoot(f_2,args_3))}))))}).apply(null,[]);

//======
//(defn make-hierarchy "Creates a hierarchy object for use with derive, isa? etc." [] {:descendants {}, :ancestors {}, :parents {}})
//---
(function __clojure_fn_7120(){
return (clojure.JS.def(clojure,"make_hierarchy",(function __clojure_fn_7120_make_hierarchy_7122(){
return (clojure.hash_map(clojure.keyword("","descendants"),clojure.lang.PersistentHashMap.EMPTY,clojure.keyword("","ancestors"),clojure.lang.PersistentHashMap.EMPTY,clojure.keyword("","parents"),clojure.lang.PersistentHashMap.EMPTY))})))}).apply(null,[]);

//======
//(def global-hierarchy (make-hierarchy))
//---
(function __clojure_fn_7129(){
return (clojure.JS.def(clojure,"global_hierarchy",clojure.make_hierarchy.apply(null,[])))}).apply(null,[]);

//======
//(defn not-empty "If coll is empty, returns nil, else coll" [coll] (when (seq coll) coll))
//---
(function __clojure_fn_7138(){
return (clojure.JS.def(clojure,"not_empty",(function __clojure_fn_7138_not_empty_7140(coll_1){
return (((clojure.seq.apply(null,[coll_1]))?(coll_1):(null)))})))}).apply(null,[]);

//======
//(defn bases "Returns the immediate superclass and direct interfaces of c, if any" [c] (let [i (.getInterfaces c) s (.getSuperclass c)] (not-empty (if s (cons s i) i))))
//---
(function __clojure_fn_7150(){
return (clojure.JS.def(clojure,"bases",(function __clojure_fn_7150_bases_7152(c_1){
var s_3,i_2;
return (((i_2=(c_1).getInterfaces()),
(s_3=(c_1).getSuperclass()),
clojure.not_empty.apply(null,[((s_3)?(clojure.cons.apply(null,[s_3,i_2])):(i_2))])))})))}).apply(null,[]);

//======
//(defn supers "Returns the immediate and indirect superclasses and interfaces of c, if any" [class] (loop [ret #{} c class] (if c (recur (into ret (bases c)) (.getSuperclass c)) (not-empty ret))))
//---
(function __clojure_fn_7162(){
return (clojure.JS.def(clojure,"supers",(function __clojure_fn_7162_supers_7164(class_1){
var c_3,ret_2;
return (((function __loop(){var _rtn,_cnt;(ret_2=clojure.lang.PersistentHashSet.EMPTY),
(c_3=class_1);do{_cnt=0;
_rtn=((c_3)?((_cnt=1,_rtn=[clojure.into.apply(null,[ret_2,clojure.bases.apply(null,[c_3])]),(c_3).getSuperclass()],ret_2=_rtn[0],c_3=_rtn[1])):(clojure.not_empty.apply(null,[ret_2])))}while(_cnt);return _rtn;})()))})))}).apply(null,[]);

//======
//(defn isa? "Returns true if (= child parent), or child is directly or indirectly derived from\n  parent, either via a Java type inheritance relationship or a\n  relationship established via derive. h must be a hierarchy obtained\n  from make-hierarchy, if not supplied defaults to the global\n  hierarchy" ([child parent] (isa? global-hierarchy child parent)) ([h child parent] (or (= child parent) (and (class? parent) (class? child) (. parent isAssignableFrom child)) (contains? ((:ancestors h) child) parent) (and (class? child) (some (fn* [p1__7168] (contains? ((:ancestors h) p1__7168) parent)) (supers child))) (and (vector? parent) (vector? child) (= (count parent) (count child)) (loop [ret true i 0] (if (= i (count parent)) ret (recur (and (isa? (child i) (parent i)) ret) (inc i))))))))
//---
(function __clojure_fn_7179(){
return (clojure.JS.def(clojure,"isa_QMARK_",(function __clojure_fn_7179_isa_QMARK_7181(h_1,child_2,parent_3){switch(arguments.length){
case 2:var child_1=arguments[0],parent_2=arguments[1];
return (clojure.isa_QMARK_.apply(null,[clojure.global_hierarchy,child_1,parent_2]))}
var ret_11,and__196_9,i_12,or__202_5,or__202_4,or__202_6,and__196_10,and__196_7,and__196_13,and__196_5,or__202_7,and__196_8,and__196_6;
return (((or__202_4=clojure.lang.Util.equal(child_2,parent_3)),
((or__202_4)?(or__202_4):(((or__202_5=((and__196_5=clojure.class_QMARK_.apply(null,[parent_3])),
((and__196_5)?(((and__196_6=clojure.class_QMARK_.apply(null,[child_2])),
((and__196_6)?((parent_3).isAssignableFrom(child_2)):(and__196_6)))):(and__196_5)))),
((or__202_5)?(or__202_5):(((or__202_6=clojure.contains_QMARK_.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]).apply(null,[child_2]),parent_3])),
((or__202_6)?(or__202_6):(((or__202_7=((and__196_7=clojure.class_QMARK_.apply(null,[child_2])),
((and__196_7)?(clojure.some.apply(null,[(function __clojure_fn_7179_isa_QMARK_7181_fn_7184(p1__7168_1){
return (clojure.contains_QMARK_.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]).apply(null,[p1__7168_1]),parent_3]))}),clojure.supers.apply(null,[child_2])])):(and__196_7)))),
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
(function __clojure_fn_7196(){
return (clojure.JS.def(clojure,"parents",(function __clojure_fn_7196_parents_7198(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.parents.apply(null,[clojure.global_hierarchy,tag_1]))}
var tp_3;
return (clojure.not_empty.apply(null,[((tp_3=clojure.get.apply(null,[clojure.keyword("","parents").apply(null,[h_1]),tag_2])),
((clojure.class_QMARK_.apply(null,[tag_2]))?(clojure.into.apply(null,[clojure.set.apply(null,[clojure.bases.apply(null,[tag_2])]),tp_3])):(tp_3)))]))})))}).apply(null,[]);

//======
//(defn ancestors "Returns the immediate and indirect parents of tag, either via a Java type\n  inheritance relationship or a relationship established via derive. h\n  must be a hierarchy obtained from make-hierarchy, if not supplied\n  defaults to the global hierarchy" ([tag] (ancestors global-hierarchy tag)) ([h tag] (not-empty (let [ta (get (:ancestors h) tag)] (if (class? tag) (into (set (supers tag)) ta) ta)))))
//---
(function __clojure_fn_7210(){
return (clojure.JS.def(clojure,"ancestors",(function __clojure_fn_7210_ancestors_7212(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.ancestors.apply(null,[clojure.global_hierarchy,tag_1]))}
var ta_3;
return (clojure.not_empty.apply(null,[((ta_3=clojure.get.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]),tag_2])),
((clojure.class_QMARK_.apply(null,[tag_2]))?(clojure.into.apply(null,[clojure.set.apply(null,[clojure.supers.apply(null,[tag_2])]),ta_3])):(ta_3)))]))})))}).apply(null,[]);

//======
//(defn descendants "Returns the immediate and indirect children of tag, through a\n  relationship established via derive. h must be a hierarchy obtained\n  from make-hierarchy, if not supplied defaults to the global\n  hierarchy. Note: does not work on Java type inheritance\n  relationships." ([tag] (descendants global-hierarchy tag)) ([h tag] (if (class? tag) (throw (RT/makeUnsupportedException "Can't get descendants of classes")) (not-empty (get (:descendants h) tag)))))
//---
(function __clojure_fn_7224(){
return (clojure.JS.def(clojure,"descendants",(function __clojure_fn_7224_descendants_7226(h_1,tag_2){switch(arguments.length){
case 1:var tag_1=arguments[0];
return (clojure.descendants.apply(null,[clojure.global_hierarchy,tag_1]))}
return (((clojure.class_QMARK_.apply(null,[tag_2]))?((function __throw(){throw clojure.lang.RT.makeUnsupportedException("Can't get descendants of classes")})()):(clojure.not_empty.apply(null,[clojure.get.apply(null,[clojure.keyword("","descendants").apply(null,[h_1]),tag_2])]))))})))}).apply(null,[]);

//======
//(defn derive "Establishes a parent/child relationship between parent and\n  tag. Parent must be a namespace-qualified symbol or keyword and\n  child can be either a namespace-qualified symbol or keyword or a\n  class. h must be a hierarchy obtained from make-hierarchy, if not\n  supplied defaults to, and modifies, the global hierarchy." ([tag parent] (alter-var-root (var global-hierarchy) derive tag parent) nil) ([h tag parent] (assert (not= tag parent)) (assert (or (class? tag) (and (instance? clojure.lang.Named tag) (namespace tag)))) (assert (instance? clojure.lang.Named parent)) (assert (namespace parent)) (let [tp (:parents h) td (:descendants h) ta (:ancestors h) tf (fn [m source sources target targets] (reduce (fn [ret k] (assoc ret k (reduce conj (get targets k #{}) (cons target (targets target))))) m (cons source (sources source))))] (or (when-not (contains? (tp tag) parent) (when (contains? (ta tag) parent) (throw (RT/makeException (print-str tag "already has" parent "as ancestor")))) (when (contains? (ta parent) tag) (throw (RT/makeException (print-str "Cyclic derivation:" parent "has" tag "as ancestor")))) {:descendants (tf (:descendants h) parent ta tag td), :ancestors (tf (:ancestors h) tag td parent ta), :parents (assoc (:parents h) tag (conj (get tp tag #{}) parent))}) h))))
//---
(function __clojure_fn_7244(){
return (clojure.JS.def(clojure,"derive",(function __clojure_fn_7244_derive_7246(h_1,tag_2,parent_3){switch(arguments.length){
case 2:var tag_1=arguments[0],parent_2=arguments[1];
return (clojure.alter_var_root.apply(null,[clojure._var_global_hierarchy,clojure.derive,tag_1,parent_2]),
null)}
var and__196_5,td_5,ta_6,or__202_4,tp_4,tf_7,or__202_8;
return (((clojure.not_EQ_.apply(null,[tag_2,parent_3]))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.str.apply(null,["Assert failed: ",clojure.pr_str.apply(null,[clojure.JS.lit_list(["'not=","'tag","'parent"])])]))})())),
((((or__202_4=clojure.class_QMARK_.apply(null,[tag_2])),
((or__202_4)?(or__202_4):(((and__196_5=clojure.instance_QMARK_.apply(null,[clojure.lang.Named,tag_2])),
((and__196_5)?(clojure.namespace.apply(null,[tag_2])):(and__196_5)))))))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.str.apply(null,["Assert failed: ",clojure.pr_str.apply(null,[clojure.JS.lit_list(["'or",clojure.JS.lit_list(["'class?","'tag"]),clojure.JS.lit_list(["'and",clojure.JS.lit_list(["'instance?","'clojure.lang.Named","'tag"]),clojure.JS.lit_list(["'namespace","'tag"])])])])]))})())),
((clojure.instance_QMARK_.apply(null,[clojure.lang.Named,parent_3]))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.str.apply(null,["Assert failed: ",clojure.pr_str.apply(null,[clojure.JS.lit_list(["'instance?","'clojure.lang.Named","'parent"])])]))})())),
((clojure.namespace.apply(null,[parent_3]))?(null):((function __throw(){throw clojure.lang.RT.makeException(clojure.str.apply(null,["Assert failed: ",clojure.pr_str.apply(null,[clojure.JS.lit_list(["'namespace","'parent"])])]))})())),
((tp_4=clojure.keyword("","parents").apply(null,[h_1])),
(td_5=clojure.keyword("","descendants").apply(null,[h_1])),
(ta_6=clojure.keyword("","ancestors").apply(null,[h_1])),
(tf_7=(function __clojure_fn_7244_derive_7246_tf_7249(m_1,source_2,sources_3,target_4,targets_5){
return (clojure.reduce.apply(null,[(function __clojure_fn_7244_derive_7246_tf_7249_fn_7251(ret_1,k_2){
return (clojure.assoc.apply(null,[ret_1,k_2,clojure.reduce.apply(null,[clojure.conj,clojure.get.apply(null,[targets_5,k_2,clojure.lang.PersistentHashSet.EMPTY]),clojure.cons.apply(null,[target_4,targets_5.apply(null,[target_4])])])]))}),m_1,clojure.cons.apply(null,[source_2,sources_3.apply(null,[source_2])])]))})),
((or__202_8=((clojure.contains_QMARK_.apply(null,[tp_4.apply(null,[tag_2]),parent_3]))?(null):(((clojure.contains_QMARK_.apply(null,[ta_6.apply(null,[tag_2]),parent_3]))?((function __throw(){throw clojure.lang.RT.makeException(clojure.print_str.apply(null,[tag_2,"already has",parent_3,"as ancestor"]))})()):(null)),
((clojure.contains_QMARK_.apply(null,[ta_6.apply(null,[parent_3]),tag_2]))?((function __throw(){throw clojure.lang.RT.makeException(clojure.print_str.apply(null,["Cyclic derivation:",parent_3,"has",tag_2,"as ancestor"]))})()):(null)),
clojure.hash_map(clojure.keyword("","descendants"),tf_7.apply(null,[clojure.keyword("","descendants").apply(null,[h_1]),parent_3,ta_6,tag_2,td_5]),clojure.keyword("","ancestors"),tf_7.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]),tag_2,td_5,parent_3,ta_6]),clojure.keyword("","parents"),clojure.assoc.apply(null,[clojure.keyword("","parents").apply(null,[h_1]),tag_2,clojure.conj.apply(null,[clojure.get.apply(null,[tp_4,tag_2,clojure.lang.PersistentHashSet.EMPTY]),parent_3])]))))),
((or__202_8)?(or__202_8):(h_1)))))})))}).apply(null,[]);

//======
//(defn underive "Removes a parent/child relationship between parent and\n  tag. h must be a hierarchy obtained from make-hierarchy, if not\n  supplied defaults to, and modifies, the global hierarchy." ([tag parent] (alter-var-root (var global-hierarchy) underive tag parent) nil) ([h tag parent] (let [tp (:parents h) td (:descendants h) ta (:ancestors h) tf (fn [m source sources target targets] (reduce (fn [ret k] (assoc ret k (reduce disj (get targets k) (cons target (targets target))))) m (cons source (sources source))))] (if (contains? (tp tag) parent) {:descendants (tf (:descendants h) parent ta tag td), :ancestors (tf (:ancestors h) tag td parent ta), :parent (assoc (:parents h) tag (disj (get tp tag) parent))} h))))
//---
(function __clojure_fn_7270(){
return (clojure.JS.def(clojure,"underive",(function __clojure_fn_7270_underive_7272(h_1,tag_2,parent_3){switch(arguments.length){
case 2:var tag_1=arguments[0],parent_2=arguments[1];
return (clojure.alter_var_root.apply(null,[clojure._var_global_hierarchy,clojure.underive,tag_1,parent_2]),
null)}
var tp_4,ta_6,tf_7,td_5;
return (((tp_4=clojure.keyword("","parents").apply(null,[h_1])),
(td_5=clojure.keyword("","descendants").apply(null,[h_1])),
(ta_6=clojure.keyword("","ancestors").apply(null,[h_1])),
(tf_7=(function __clojure_fn_7270_underive_7272_tf_7275(m_1,source_2,sources_3,target_4,targets_5){
return (clojure.reduce.apply(null,[(function __clojure_fn_7270_underive_7272_tf_7275_fn_7277(ret_1,k_2){
return (clojure.assoc.apply(null,[ret_1,k_2,clojure.reduce.apply(null,[clojure.disj,clojure.get.apply(null,[targets_5,k_2]),clojure.cons.apply(null,[target_4,targets_5.apply(null,[target_4])])])]))}),m_1,clojure.cons.apply(null,[source_2,sources_3.apply(null,[source_2])])]))})),
((clojure.contains_QMARK_.apply(null,[tp_4.apply(null,[tag_2]),parent_3]))?(clojure.hash_map(clojure.keyword("","descendants"),tf_7.apply(null,[clojure.keyword("","descendants").apply(null,[h_1]),parent_3,ta_6,tag_2,td_5]),clojure.keyword("","ancestors"),tf_7.apply(null,[clojure.keyword("","ancestors").apply(null,[h_1]),tag_2,td_5,parent_3,ta_6]),clojure.keyword("","parent"),clojure.assoc.apply(null,[clojure.keyword("","parents").apply(null,[h_1]),tag_2,clojure.disj.apply(null,[clojure.get.apply(null,[tp_4,tag_2]),parent_3])]))):(h_1))))})))}).apply(null,[]);

//======
//(defn distinct? "Returns true if no two of the arguments are equal" {:tag Boolean} ([x] true) ([x y] (not (= x y))) ([x y & more] (if (not= x y) (loop [s #{y x} [x & etc :as xs] more] (if xs (if (contains? s x) false (recur (conj s x) etc)) true)) false)))
//---
(function __clojure_fn_7295(){
return (clojure.JS.def(clojure,"distinct_QMARK_",clojure.JS.variadic(2,(function __clojure_fn_7295_distinct_QMARK_7297(x_1,y_2){switch(arguments.length){
case 2:return (clojure.not.apply(null,[clojure.lang.Util.equal(x_1,y_2)]))
case 1:return (true)}
var xs_16,etc_15,etc_8,s_10,xs_9,x_7,G__7302_11,G__7302_5,x_14,vec__7304_13,s_12,s_4,vec__7303_6,more_3,more_3=clojure.JS.rest_args(this,arguments,2);
return (((clojure.not_EQ_.apply(null,[x_1,y_2]))?(((s_4=clojure.hash_set(y_2,x_1)),
(G__7302_5=more_3),
(vec__7303_6=G__7302_5),
(x_7=clojure.nth.apply(null,[vec__7303_6,0,null])),
(etc_8=clojure.nthrest.apply(null,[vec__7303_6,1])),
(xs_9=vec__7303_6),
((function __loop(){var _rtn,_cnt;(s_10=s_4),
(G__7302_11=G__7302_5);do{_cnt=0;
_rtn=((s_12=s_10),
(vec__7304_13=G__7302_11),
(x_14=clojure.nth.apply(null,[vec__7304_13,0,null])),
(etc_15=clojure.nthrest.apply(null,[vec__7304_13,1])),
(xs_16=vec__7304_13),
((xs_16)?(((clojure.contains_QMARK_.apply(null,[s_12,x_14]))?(false):((_cnt=1,_rtn=[clojure.conj.apply(null,[s_12,x_14]),etc_15],s_10=_rtn[0],G__7302_11=_rtn[1])))):(true)))}while(_cnt);return _rtn;})()))):(false)))}))))}).apply(null,[]);

//======
//(defn iterator-seq "Returns a seq on a java.util.Iterator. Note that most collections\n  providing iterators implement Iterable and thus support seq directly." [iter] (clojure.lang.IteratorSeq/create iter))
//---
(function __clojure_fn_7313(){
return (clojure.JS.def(clojure,"iterator_seq",(function __clojure_fn_7313_iterator_seq_7315(iter_1){
return (clojure.lang.IteratorSeq.create(iter_1))})))}).apply(null,[]);

//======
//(defn enumeration-seq "Returns a seq on a java.lang.Enumeration" [e] (clojure.lang.EnumerationSeq/create e))
//---
(function __clojure_fn_7325(){
return (clojure.JS.def(clojure,"enumeration_seq",(function __clojure_fn_7325_enumeration_seq_7327(e_1){
return (clojure.lang.EnumerationSeq.create(e_1))})))}).apply(null,[]);

//======
//(defn printf "Prints formatted output, as per format" [fmt & args] (print (apply format fmt args)))
//---
(function __clojure_fn_7343(){
return (clojure.JS.def(clojure,"printf",clojure.JS.variadic(1,(function __clojure_fn_7343_printf_7345(fmt_1){
var args_2,args_2=clojure.JS.rest_args(this,arguments,1);
return (clojure.print.apply(null,[clojure.apply.apply(null,[clojure.format,fmt_1,args_2])]))}))))}).apply(null,[]);

//======
//(defonce *loaded-libs* (ref (sorted-set)))
//---
(function __clojure_fn_7384(){
var v__1692_1;
return (((v__1692_1=clojure.JS.def(clojure,"_STAR_loaded_libs_STAR_",null)),
(((v__1692_1).hasRoot())?(null):(clojure.JS.def(clojure,"_STAR_loaded_libs_STAR_",clojure.ref.apply(null,[clojure.sorted_set.apply(null,[])]))))))}).apply(null,[]);

//======
//(defonce *pending-paths* #{})
//---
(function __clojure_fn_7390(){
var v__1692_1;
return (((v__1692_1=clojure.JS.def(clojure,"_STAR_pending_paths_STAR_",null)),
(((v__1692_1).hasRoot())?(null):(clojure.JS.def(clojure,"_STAR_pending_paths_STAR_",clojure.lang.PersistentHashSet.EMPTY)))))}).apply(null,[]);

//======
//(defonce *loading-verbosely* false)
//---
(function __clojure_fn_7396(){
var v__1692_1;
return (((v__1692_1=clojure.JS.def(clojure,"_STAR_loading_verbosely_STAR_",null)),
(((v__1692_1).hasRoot())?(null):(clojure.JS.def(clojure,"_STAR_loading_verbosely_STAR_",false)))))}).apply(null,[]);

//======
//(defn- throw-if "Throws an exception with a message if pred is true" [pred fmt & args] (when pred (let [message (apply format fmt args) exception (RT/makeException message) raw-trace (.getStackTrace exception) boring? (fn* [p1__7399] (not= (.getMethodName p1__7399) "doInvoke")) trace (into-array (drop 2 (drop-while boring? raw-trace)))] (.setStackTrace exception trace) (throw exception))))
//---
(function __clojure_fn_7409(){
return (clojure.JS.def(clojure,"throw_if",clojure.JS.variadic(2,(function __clojure_fn_7409_throw_if_7411(pred_1,fmt_2){
var raw_trace_6,message_4,args_3,trace_8,boring_QMARK__7,exception_5,args_3=clojure.JS.rest_args(this,arguments,2);
return (((pred_1)?(((message_4=clojure.apply.apply(null,[clojure.format,fmt_2,args_3])),
(exception_5=clojure.lang.RT.makeException(message_4)),
(raw_trace_6=(exception_5).getStackTrace()),
(boring_QMARK__7=(function __clojure_fn_7409_throw_if_7411_boring_QMARK_7413(p1__7399_1){
return (clojure.not_EQ_.apply(null,[(p1__7399_1).getMethodName,"doInvoke"]))})),
(trace_8=clojure.into_array.apply(null,[clojure.drop.apply(null,[2,clojure.drop_while.apply(null,[boring_QMARK__7,raw_trace_6])])])),
(exception_5).setStackTrace(trace_8),
(function __throw(){throw exception_5})())):(null)))}))))}).apply(null,[]);

//======
//(defn- libspec? "Returns true if x is a libspec" [x] (or (symbol? x) (and (vector? x) (or (nil? (second x)) (keyword? (second x))))))
//---
(function __clojure_fn_7424(){
return (clojure.JS.def(clojure,"libspec_QMARK_",(function __clojure_fn_7424_libspec_QMARK_7426(x_1){
var or__202_2,or__202_4,and__196_3;
return (((or__202_2=clojure.symbol_QMARK_.apply(null,[x_1])),
((or__202_2)?(or__202_2):(((and__196_3=clojure.vector_QMARK_.apply(null,[x_1])),
((and__196_3)?(((or__202_4=clojure.nil_QMARK_.apply(null,[clojure.second.apply(null,[x_1])])),
((or__202_4)?(or__202_4):(clojure.keyword_QMARK_.apply(null,[clojure.second.apply(null,[x_1])]))))):(and__196_3)))))))})))}).apply(null,[]);

//======
//(defn- prependss "Prepends a symbol or a seq to coll" [x coll] (if (symbol? x) (cons x coll) (concat x coll)))
//---
(function __clojure_fn_7436(){
return (clojure.JS.def(clojure,"prependss",(function __clojure_fn_7436_prependss_7438(x_1,coll_2){
return (((clojure.symbol_QMARK_.apply(null,[x_1]))?(clojure.cons.apply(null,[x_1,coll_2])):(clojure.concat.apply(null,[x_1,coll_2]))))})))}).apply(null,[]);

//======
//(defn- root-directory "Returns the root directory path for a lib" [lib] (str \/ (.. (name lib) (replace \- \_) (replace \. \/))))
//---
(function __clojure_fn_7448(){
return (clojure.JS.def(clojure,"root_directory",(function __clojure_fn_7448_root_directory_7450(lib_1){
return (clojure.str.apply(null,["/",((clojure.name.apply(null,[lib_1])).replace("-","_")).replace(".","/")]))})))}).apply(null,[]);

//======
//(defn- root-resource "Returns the root resource path for a lib" [lib] (let [d (root-directory lib) i (inc (.lastIndexOf d (int \/))) leaf (.substring d i)] (str d \/ leaf ".clj")))
//---
(function __clojure_fn_7460(){
return (clojure.JS.def(clojure,"root_resource",(function __clojure_fn_7460_root_resource_7462(lib_1){
var leaf_4,i_3,d_2;
return (((d_2=clojure.root_directory.apply(null,[lib_1])),
(i_3=clojure.lang.Numbers.inc((d_2).lastIndexOf(clojure.lang.RT.intCast("/")))),
(leaf_4=(d_2).substring(i_3)),
clojure.str.apply(null,[d_2,"/",leaf_4,".clj"])))})))}).apply(null,[]);

//======
//(def load)
//---
(function __clojure_fn_7469(){
return (clojure.JS.def(clojure,"load",null))}).apply(null,[]);

//======
//(defn- load-one "Loads a lib given its name. If need-ns, ensures that the associated\n  namespace exists after loading. If require, records the load so any\n  duplicate loads can be skipped." [lib need-ns require] (load (root-resource lib)) (throw-if (and need-ns (not (find-ns lib))) "namespace '%s' not found after loading '%s'" lib (root-resource lib)) (when require (dosync (commute *loaded-libs* conj lib))))
//---
(function __clojure_fn_7481(){
return (clojure.JS.def(clojure,"load_one",(function __clojure_fn_7481_load_one_7483(lib_1,need_ns_2,require_3){
var and__196_4;
return (clojure.load.apply(null,[clojure.root_resource.apply(null,[lib_1])]),
clojure.throw_if.apply(null,[((and__196_4=need_ns_2),
((and__196_4)?(clojure.not.apply(null,[clojure.find_ns.apply(null,[lib_1])])):(and__196_4))),"namespace '%s' not found after loading '%s'",lib_1,clojure.root_resource.apply(null,[lib_1])]),
((require_3)?(clojure.lang.LockingTransaction.runInTransaction((function __clojure_fn_7481_load_one_7483_fn_7485(){
return (clojure.commute.apply(null,[clojure._STAR_loaded_libs_STAR_,clojure.conj,lib_1]))}))):(null)))})))}).apply(null,[]);

//======
//(defn- load-all "Loads a lib given its name and forces a load of any libs it directly or\n  indirectly loads. If need-ns, ensures that the associated namespace\n  exists after loading. If require, records the load so any duplicate loads\n  can be skipped." [lib need-ns require] (dosync (commute *loaded-libs* (fn* [p1__7490 p2__7491] (reduce conj p1__7490 p2__7491)) (binding [*loaded-libs* (ref (sorted-set))] (load-one lib need-ns require) (clojure/deref *loaded-libs*)))))
//---
(function __clojure_fn_7507(){
return (clojure.JS.def(clojure,"load_all",(function __clojure_fn_7507_load_all_7509(lib_1,need_ns_2,require_3){
return (clojure.lang.LockingTransaction.runInTransaction((function __clojure_fn_7507_load_all_7509_fn_7511(){
return (clojure.commute.apply(null,[clojure._STAR_loaded_libs_STAR_,(function __clojure_fn_7507_load_all_7509_fn_7511_fn_7513(p1__7490_1,p2__7491_2){
return (clojure.reduce.apply(null,[clojure.conj,p1__7490_1,p2__7491_2]))}),clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_loaded_libs_STAR_,clojure.ref.apply(null,[clojure.sorted_set.apply(null,[])])])),
(function __clojure_fn_7507_load_all_7509_fn_7511_fn_7516(){
return ((function __try(){try{var _rtn=(clojure.load_one.apply(null,[lib_1,need_ns_2,require_3]),
clojure.deref.apply(null,[clojure._STAR_loaded_libs_STAR_]))}
finally{clojure.lang.Var.popThreadBindings()}})())}).apply(null,[])]))})))})))}).apply(null,[]);

//======
//(defn- load-lib "Loads a lib with options" [prefix lib & options] (throw-if (and prefix (pos? (.indexOf (name lib) (int \.)))) "lib names inside prefix lists must not contain periods") (let [lib (if prefix (symbol (str prefix \. lib)) lib) opts (apply hash-map options) {:keys [as reload reload-all require use verbose]} opts loaded (contains? (clojure/deref *loaded-libs*) lib) load (cond reload-all load-all (or reload (not require) (not loaded)) load-one) need-ns (or as use) filter-opts (select-keys opts (quote (:exclude :only :rename)))] (binding [*loading-verbosely* (or *loading-verbosely* verbose)] (if load (load lib need-ns require) (throw-if (and need-ns (not (find-ns lib))) "namespace '%s' not found" lib)) (when (and need-ns *loading-verbosely*) (printf "(clojure/in-ns '%s)\n" (ns-name *ns*))) (when as (when *loading-verbosely* (printf "(clojure/alias '%s '%s)\n" as lib)) (alias as lib)) (when use (when *loading-verbosely* (printf "(clojure/refer '%s" lib) (doseq opt filter-opts (printf " %s '%s" (key opt) (print-str (val opt)))) (printf ")\n")) (apply refer lib (mapcat seq filter-opts))))))
//---
(function __clojure_fn_7529(){
return (clojure.JS.def(clojure,"load_lib",clojure.JS.variadic(2,(function __clojure_fn_7529_load_lib_7531(prefix_1,lib_2){
var and__196_19,need_ns_15,lib_4,or__202_17,map__7533_6,opts_5,list__763_19,load_14,filter_opts_16,reload_7,and__196_4,options_3,or__202_14,require_9,loaded_13,opt_20,use_12,or__202_15,reload_all_10,verbose_11,or__202_15,as_8,and__196_19,options_3=clojure.JS.rest_args(this,arguments,2);
return (clojure.throw_if.apply(null,[((and__196_4=prefix_1),
((and__196_4)?(clojure.lang.Numbers.isPos((clojure.name.apply(null,[lib_2])).indexOf(clojure.lang.RT.intCast(".")))):(and__196_4))),"lib names inside prefix lists must not contain periods"]),
((lib_4=((prefix_1)?(clojure.symbol.apply(null,[clojure.str.apply(null,[prefix_1,".",lib_2])])):(lib_2))),
(opts_5=clojure.apply.apply(null,[clojure.hash_map,options_3])),
(map__7533_6=opts_5),
(reload_7=clojure.get.apply(null,[map__7533_6,clojure.keyword("","reload")])),
(as_8=clojure.get.apply(null,[map__7533_6,clojure.keyword("","as")])),
(require_9=clojure.get.apply(null,[map__7533_6,clojure.keyword("","require")])),
(reload_all_10=clojure.get.apply(null,[map__7533_6,clojure.keyword("","reload-all")])),
(verbose_11=clojure.get.apply(null,[map__7533_6,clojure.keyword("","verbose")])),
(use_12=clojure.get.apply(null,[map__7533_6,clojure.keyword("","use")])),
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
(function __clojure_fn_7543(){
return (clojure.JS.def(clojure,"load_libs",clojure.JS.variadic(0,(function __clojure_fn_7543_load_libs_7545(){
var arg_6,args_1,opts_3,vec__7547_7,args_9,list__763_10,flags_2,args_4,arg_11,prefix_8,list__763_5,args_1=clojure.JS.rest_args(this,arguments,0);
return (((flags_2=clojure.filter.apply(null,[clojure.keyword_QMARK_,args_1])),
(opts_3=clojure.interleave.apply(null,[flags_2,clojure.repeat.apply(null,[true])])),
(args_4=clojure.filter.apply(null,[clojure.complement.apply(null,[clojure.keyword_QMARK_]),args_1])),
((function __loop(){var _rtn,_cnt;(list__763_5=clojure.seq.apply(null,[args_4]));do{_cnt=0;
_rtn=((list__763_5)?(((arg_6=clojure.first.apply(null,[list__763_5])),
((clojure.libspec_QMARK_.apply(null,[arg_6]))?(clojure.apply.apply(null,[clojure.load_lib,null,clojure.prependss.apply(null,[arg_6,opts_3])])):(((vec__7547_7=arg_6),
(prefix_8=clojure.nth.apply(null,[vec__7547_7,0,null])),
(args_9=clojure.nthrest.apply(null,[vec__7547_7,1])),
clojure.throw_if.apply(null,[clojure.nil_QMARK_.apply(null,[prefix_8]),"prefix cannot be nil"]),
((function __loop(){var _rtn,_cnt;(list__763_10=clojure.seq.apply(null,[args_9]));do{_cnt=0;
_rtn=((list__763_10)?(((arg_11=clojure.first.apply(null,[list__763_10])),
clojure.apply.apply(null,[clojure.load_lib,prefix_8,clojure.prependss.apply(null,[arg_11,opts_3])])),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__763_10])],list__763_10=_rtn[0])):(null))}while(_cnt);return _rtn;})()))))),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__763_5])],list__763_5=_rtn[0])):(null))}while(_cnt);return _rtn;})())))}))))}).apply(null,[]);

//======
//(defn require "Loads libs, skipping any that are already loaded. Each argument is\n  either a libspec that identifies a lib, a prefix list that identifies\n  multiple libs whose names share a common prefix, or a flag that modifies\n  how all the identified libs are loaded. Use :require in the ns macro \n  in preference to calling this directly.\n\n  Libs\n\n  A 'lib' is a named set of resources in classpath whose contents define a\n  library of Clojure code. Lib names are symbols and each lib is associated\n  with a Clojure namespace and a Java package that share its name. A lib's\n  name also locates its root directory within classpath using Java's\n  package name to classpath-relative path mapping. All resources in a lib\n  should be contained in the directory structure under its root directory.\n  All definitions a lib makes should be in its associated namespace.\n\n  'require loads a lib by loading its root resource. The root resource path\n  is derived from the root directory path by repeating its last component\n  and appending '.clj'. For example, the lib 'x.y.z has root directory\n  <classpath>/x/y/z; root resource <classpath>/x/y/z/z.clj. The root\n  resource should contain code to create the lib's namespace and load any\n  additional lib resources.\n\n  Libspecs\n\n  A libspec is a lib name or a vector containing a lib name followed by\n  options expressed as sequential keywords and arguments.\n\n  Recognized options: :as\n  :as takes a symbol as its argument and makes that symbol an alias to the\n    lib's namespace in the current namespace.\n\n  Prefix Lists\n\n  It's common for Clojure code to depend on several libs whose names have\n  the same prefix. When specifying libs, prefix lists can be used to reduce\n  repetition. A prefix list contains the shared prefix followed by libspecs\n  with the shared prefix removed from the lib names. After removing the\n  prefix, the names that remain must not contain any periods.\n\n  Flags\n\n  A flag is a keyword.\n  Recognized flags: :reload, :reload-all, :verbose\n  :reload forces loading of all the identified libs even if they are\n    already loaded\n  :reload-all implies :reload and also forces loading of all libs that the\n    identified libs directly or indirectly load via require or use\n  :verbose triggers printing information about each load, alias, and refer" [& args] (apply load-libs :require args))
//---
(function __clojure_fn_7556(){
return (clojure.JS.def(clojure,"require",clojure.JS.variadic(0,(function __clojure_fn_7556_require_7558(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[clojure.load_libs,clojure.keyword("","require"),args_1]))}))))}).apply(null,[]);

//======
//(defn use "Like 'require, but also refers to each lib's namespace using\n  clojure/refer. Use :use in the ns macro in preference to calling\n  this directly.\n\n  'use accepts additional options in libspecs: :exclude, :only, :rename.\n  The arguments and semantics for :exclude, :only, and :rename are the same\n  as those documented for clojure/refer." [& args] (apply load-libs :require :use args))
//---
(function __clojure_fn_7568(){
return (clojure.JS.def(clojure,"use",clojure.JS.variadic(0,(function __clojure_fn_7568_use_7570(){
var args_1,args_1=clojure.JS.rest_args(this,arguments,0);
return (clojure.apply.apply(null,[clojure.load_libs,clojure.keyword("","require"),clojure.keyword("","use"),args_1]))}))))}).apply(null,[]);

//======
//(defn loaded-libs "Returns a sorted set of symbols naming the currently loaded libs" [] (clojure/deref *loaded-libs*))
//---
(function __clojure_fn_7580(){
return (clojure.JS.def(clojure,"loaded_libs",(function __clojure_fn_7580_loaded_libs_7582(){
return (clojure.deref.apply(null,[clojure._STAR_loaded_libs_STAR_]))})))}).apply(null,[]);

//======
//(defn load "Loads Clojure code from resources in classpath. A path is interpreted as\n  classpath-relative if it begins with a slash or relative to the root\n  directory for the current namespace otherwise." [& paths] (doseq path paths (let [path (if (.startsWith path "/") path (str (root-directory (ns-name *ns*)) \/ path))] (when *loading-verbosely* (printf "(clojure/load \"%s\")\n" path) (flush)) (throw-if (*pending-paths* path) "cannot load '%s' again while it is loading" path) (binding [*pending-paths* (conj *pending-paths* path)] (.loadResourceScript clojure.lang.RT (.substring path 1))))))
//---
(function __clojure_fn_7595(){
return (clojure.JS.def(clojure,"load",clojure.JS.variadic(0,(function __clojure_fn_7595_load_7597(){
var list__763_2,paths_1,path_3,path_4,paths_1=clojure.JS.rest_args(this,arguments,0);
return (((function __loop(){var _rtn,_cnt;(list__763_2=clojure.seq.apply(null,[paths_1]));do{_cnt=0;
_rtn=((list__763_2)?(((path_3=clojure.first.apply(null,[list__763_2])),
((path_4=(((path_3).startsWith("/"))?(path_3):(clojure.str.apply(null,[clojure.root_directory.apply(null,[clojure.ns_name.apply(null,[clojure._STAR_ns_STAR_])]),"/",path_3])))),
((clojure._STAR_loading_verbosely_STAR_)?(clojure.printf.apply(null,["(clojure/load \"%s\")\n",path_4]),
clojure.flush.apply(null,[])):(null)),
clojure.throw_if.apply(null,[clojure._STAR_pending_paths_STAR_.apply(null,[path_4]),"cannot load '%s' again while it is loading",path_4]),
clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[clojure._var__STAR_pending_paths_STAR_,clojure.conj.apply(null,[clojure._STAR_pending_paths_STAR_,path_4])])),
(function __clojure_fn_7595_load_7597_fn_7599(){
return ((function __try(){try{var _rtn=(clojure.lang.RT.loadResourceScript((path_4).substring(1)))}
finally{clojure.lang.Var.popThreadBindings()}})())}).apply(null,[]))),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__763_2])],list__763_2=_rtn[0])):(null))}while(_cnt);return _rtn;})()))}))))}).apply(null,[]);

//======
//(defn get-in "returns the value in a nested associative structure, where ks is a sequence of keys" [m ks] (reduce get m ks))
//---
(function __clojure_fn_7610(){
return (clojure.JS.def(clojure,"get_in",(function __clojure_fn_7610_get_in_7612(m_1,ks_2){
return (clojure.reduce.apply(null,[clojure.get,m_1,ks_2]))})))}).apply(null,[]);

//======
//(defn assoc-in "Associates a value in a nested associative structure, where ks is a\n  sequence of keys and v is the new value and returns a new nested structure.  \n  If any levels do not exist, hash-maps will be created." [m [k & ks] v] (if ks (assoc m k (assoc-in (get m k) ks v)) (assoc m k v)))
//---
(function __clojure_fn_7624(){
return (clojure.JS.def(clojure,"assoc_in",(function __clojure_fn_7624_assoc_in_7627(m_1,p__7626_2,v_3){
var ks_6,vec__7629_4,k_5;
return (((vec__7629_4=p__7626_2),
(k_5=clojure.nth.apply(null,[vec__7629_4,0,null])),
(ks_6=clojure.nthrest.apply(null,[vec__7629_4,1])),
((ks_6)?(clojure.assoc.apply(null,[m_1,k_5,clojure.assoc_in.apply(null,[clojure.get.apply(null,[m_1,k_5]),ks_6,v_3])])):(clojure.assoc.apply(null,[m_1,k_5,v_3])))))})))}).apply(null,[]);

//======
//(defn update-in "'Updates' a value in a nested associative structure, where ks is a\n  sequence of keys and f is a function that will take the old value\n  and return the new value, and returns a new nested structure.  \n  If any levels do not exist, hash-maps will be created." ([m [k & ks] f] (if ks (assoc m k (update-in (get m k) ks f)) (assoc m k (f (get m k))))))
//---
(function __clojure_fn_7640(){
return (clojure.JS.def(clojure,"update_in",(function __clojure_fn_7640_update_in_7643(m_1,p__7642_2,f_3){
var k_5,vec__7645_4,ks_6;
return (((vec__7645_4=p__7642_2),
(k_5=clojure.nth.apply(null,[vec__7645_4,0,null])),
(ks_6=clojure.nthrest.apply(null,[vec__7645_4,1])),
((ks_6)?(clojure.assoc.apply(null,[m_1,k_5,clojure.update_in.apply(null,[clojure.get.apply(null,[m_1,k_5]),ks_6,f_3])])):(clojure.assoc.apply(null,[m_1,k_5,f_3.apply(null,[clojure.get.apply(null,[m_1,k_5])])])))))})))}).apply(null,[]);

//======
//(defn empty? "Returns true if coll has no items - same as (not (seq coll)). \n  Please use the idiom (seq x) rather than (not (empty? x))" [coll] (not (seq coll)))
//---
(function __clojure_fn_7654(){
return (clojure.JS.def(clojure,"empty_QMARK_",(function __clojure_fn_7654_empty_QMARK_7656(coll_1){
return (clojure.not.apply(null,[clojure.seq.apply(null,[coll_1])]))})))}).apply(null,[]);

//======
//(defn coll? "Returns true if x implements IPersistentCollection" [x] (instance? clojure.lang.IPersistentCollection x))
//---
(function __clojure_fn_7666(){
return (clojure.JS.def(clojure,"coll_QMARK_",(function __clojure_fn_7666_coll_QMARK_7668(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentCollection,x_1]))})))}).apply(null,[]);

//======
//(defn list? "Returns true if x implements IPersistentList" [x] (instance? clojure.lang.IPersistentList x))
//---
(function __clojure_fn_7678(){
return (clojure.JS.def(clojure,"list_QMARK_",(function __clojure_fn_7678_list_QMARK_7680(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentList,x_1]))})))}).apply(null,[]);

//======
//(defn set? "Returns true if x implements IPersistentSet" [x] (instance? clojure.lang.IPersistentSet x))
//---
(function __clojure_fn_7690(){
return (clojure.JS.def(clojure,"set_QMARK_",(function __clojure_fn_7690_set_QMARK_7692(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IPersistentSet,x_1]))})))}).apply(null,[]);

//======
//(defn fn? "Returns true if x implements IFn. Note that many data structures \n  (e.g. sets and maps) implement IFn" [x] (instance? clojure.lang.IFn x))
//---
(function __clojure_fn_7708(){
return (clojure.JS.def(clojure,"fn_QMARK_",(function __clojure_fn_7708_fn_QMARK_7710(x_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.IFn,x_1]))})))}).apply(null,[]);

//======
//(defn ratio? "Returns true if n is a Ratio" [n] (instance? clojure.lang.Ratio n))
//---
(function __clojure_fn_7726(){
return (clojure.JS.def(clojure,"ratio_QMARK_",(function __clojure_fn_7726_ratio_QMARK_7728(n_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Ratio,n_1]))})))}).apply(null,[]);

//======
//(defn rational? [n] "Returns true if n is a rational number" (or (integer? n) (ratio? n) (decimal? n)))
//---
(function __clojure_fn_7750(){
return (clojure.JS.def(clojure,"rational_QMARK_",(function __clojure_fn_7750_rational_QMARK_7752(n_1){
var or__202_2,or__202_3;
return ("Returns true if n is a rational number",
((or__202_2=clojure.integer_QMARK_.apply(null,[n_1])),
((or__202_2)?(or__202_2):(((or__202_3=clojure.ratio_QMARK_.apply(null,[n_1])),
((or__202_3)?(or__202_3):(clojure.decimal_QMARK_.apply(null,[n_1]))))))))})))}).apply(null,[]);

//======
//(defn associative? "Returns true if coll implements Associative" [coll] (instance? clojure.lang.Associative coll))
//---
(function __clojure_fn_7762(){
return (clojure.JS.def(clojure,"associative_QMARK_",(function __clojure_fn_7762_associative_QMARK_7764(coll_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Associative,coll_1]))})))}).apply(null,[]);

//======
//(defn sequential? "Returns true if coll implements Sequential" [coll] (instance? clojure.lang.Sequential coll))
//---
(function __clojure_fn_7774(){
return (clojure.JS.def(clojure,"sequential_QMARK_",(function __clojure_fn_7774_sequential_QMARK_7776(coll_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Sequential,coll_1]))})))}).apply(null,[]);

//======
//(defn sorted? "Returns true if coll implements Sorted" [coll] (instance? clojure.lang.Sorted coll))
//---
(function __clojure_fn_7786(){
return (clojure.JS.def(clojure,"sorted_QMARK_",(function __clojure_fn_7786_sorted_QMARK_7788(coll_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Sorted,coll_1]))})))}).apply(null,[]);

//======
//(defn reversible? "Returns true if coll implements Reversible" [coll] (instance? clojure.lang.Reversible coll))
//---
(function __clojure_fn_7798(){
return (clojure.JS.def(clojure,"reversible_QMARK_",(function __clojure_fn_7798_reversible_QMARK_7800(coll_1){
return (clojure.instance_QMARK_.apply(null,[clojure.lang.Reversible,coll_1]))})))}).apply(null,[]);

//======
//(defn even? "Returns true if n is even, throws an exception if n is not an integer" [n] (zero? (bit-and n 1)))
//---
(function __clojure_fn_7810(){
return (clojure.JS.def(clojure,"even_QMARK_",(function __clojure_fn_7810_even_QMARK_7812(n_1){
return (clojure.lang.Numbers.isZero(clojure.bit_and.apply(null,[n_1,1])))})))}).apply(null,[]);

//======
//(defn odd? "Returns true if n is odd, throws an exception if n is not an integer" [n] (not (even? n)))
//---
(function __clojure_fn_7822(){
return (clojure.JS.def(clojure,"odd_QMARK_",(function __clojure_fn_7822_odd_QMARK_7824(n_1){
return (clojure.not.apply(null,[clojure.even_QMARK_.apply(null,[n_1])]))})))}).apply(null,[]);

//======
//(def *1)
//---
(function __clojure_fn_7870(){
return (clojure.JS.def(clojure,"_STAR_1",null))}).apply(null,[]);

//======
//(def *2)
//---
(function __clojure_fn_7876(){
return (clojure.JS.def(clojure,"_STAR_2",null))}).apply(null,[]);

//======
//(def *3)
//---
(function __clojure_fn_7882(){
return (clojure.JS.def(clojure,"_STAR_3",null))}).apply(null,[]);

//======
//(def *e)
//---
(function __clojure_fn_7888(){
return (clojure.JS.def(clojure,"_STAR_e",null))}).apply(null,[]);

//======
//(import (quote (java.io Writer)))
//---
(function __clojure_fn_7894(){
return (clojure.import_.apply(null,[clojure.JS.lit_list(["'java.io","'Writer"])]))}).apply(null,[]);

//======
//(defn- print-sequential [begin print-one sep end sequence w] (.write w begin) (loop [s (seq sequence)] (if (rest s) (do (print-one (first s) w) (.write w sep) (recur (rest s))) (when s (print-one (first s) w)))) (.write w end))
//---
(function __clojure_fn_7903(){
return (clojure.JS.def(clojure,"print_sequential",(function __clojure_fn_7903_print_sequential_7905(begin_1,print_one_2,sep_3,end_4,sequence_5,w_6){
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
(function __clojure_fn_7915(){
return (clojure.JS.def(clojure,"print_meta",(function __clojure_fn_7915_print_meta_7917(o_1,w_2){
var m_4,and__196_5,and__196_6,and__196_5,temp__1419_3;
return (((temp__1419_3=clojure.meta.apply(null,[o_1])),
((temp__1419_3)?(((m_4=temp__1419_3),
((((and__196_5=clojure._STAR_print_meta_STAR_),
((and__196_5)?(((and__196_6=clojure._STAR_print_readably_STAR_),
((and__196_6)?(clojure.lang.Numbers.isPos(clojure.count.apply(null,[m_4]))):(and__196_6)))):(and__196_5))))?((w_2).write("#^"),
((((and__196_5=clojure.lang.Util.equal(clojure.count.apply(null,[m_4]),1)),
((and__196_5)?(clojure.keyword("","tag").apply(null,[m_4])):(and__196_5))))?(clojure.print_method.apply(null,[clojure.keyword("","tag").apply(null,[m_4]),w_2])):(clojure.print_method.apply(null,[m_4,w_2]))),
(w_2).write(" ")):(null)))):(null))))})))}).apply(null,[]);

//======
//(defmethod print-method nil [o w] (.write w "nil"))
//---
(function __clojure_fn_7927(){
return ((clojure.print_method).addMethod(null,(function __clojure_fn_7927_fn_7929(o_1,w_2){
return ((w_2).write("nil"))})))}).apply(null,[]);

//======
//(defmethod print-method :default [o w] (print-meta o w) (.write w (str o)))
//---
(function __clojure_fn_7939(){
return ((clojure.print_method).addMethod(clojure.keyword("","default"),(function __clojure_fn_7939_fn_7941(o_1,w_2){
return (clojure.print_meta.apply(null,[o_1,w_2]),
(w_2).write(clojure.str.apply(null,[o_1])))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.ISeq [o w] (print-meta o w) (print-sequential "(" print-method " " ")" o w))
//---
(function __clojure_fn_7951(){
return ((clojure.print_method).addMethod(clojure.lang.ISeq,(function __clojure_fn_7951_fn_7953(o_1,w_2){
return (clojure.print_meta.apply(null,[o_1,w_2]),
clojure.print_sequential.apply(null,["(",clojure.print_method," ",")",o_1,w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentList [o w] (print-meta o w) (print-sequential "(" print-method " " ")" o w))
//---
(function __clojure_fn_7963(){
return ((clojure.print_method).addMethod(clojure.lang.IPersistentList,(function __clojure_fn_7963_fn_7965(o_1,w_2){
return (clojure.print_meta.apply(null,[o_1,w_2]),
clojure.print_sequential.apply(null,["(",clojure.print_method," ",")",o_1,w_2]))})))}).apply(null,[]);

//======
//(prefer-method print-method clojure.lang.IPersistentList clojure.lang.ISeq)
//---
(function __clojure_fn_7972(){
return ((clojure.print_method).preferMethod(clojure.lang.IPersistentList,clojure.lang.ISeq))}).apply(null,[]);

//======
//(def char-escape-string {\" "\\\"", \backspace "\\b", \tab "\\t", \newline "\\n", \formfeed "\\f", \return "\\r", \\ "\\\\"})
//---
(function __clojure_fn_7978(){
return (clojure.JS.def(clojure,"char_escape_string",clojure.hash_map("\"","\\\"","\b","\\b","\t","\\t","\n","\\n","\f","\\f","\r","\\r","\\","\\\\")))}).apply(null,[]);

//======
//(defmethod print-method String [s w] (if *print-readably* (do (.append w \") (dotimes n (count s) (let [c (.charAt s n) e (char-escape-string c)] (if e (.write w e) (.append w c)))) (.append w \")) (.write w s)) nil)
//---
(function __clojure_fn_7987(){
return ((clojure.print_method).addMethod(java.lang.String,(function __clojure_fn_7987_fn_7989(s_1,w_2){
var n__796_3,c_5,n_4,e_6;
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
(function __clojure_fn_7999(){
return ((clojure.print_method).addMethod(clojure.lang.IPersistentVector,(function __clojure_fn_7999_fn_8001(v_1,w_2){
var n_4,n__796_3;
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
(function __clojure_fn_8014(){
return ((clojure.print_method).addMethod(clojure.lang.IPersistentMap,(function __clojure_fn_8014_fn_8016(m_1,w_2){
return (clojure.print_meta.apply(null,[m_1,w_2]),
clojure.print_sequential.apply(null,["{",(function __clojure_fn_8014_fn_8016_fn_8018(e_1,w_2){
return (clojure.print_method.apply(null,[clojure.key.apply(null,[e_1]),w_2]),
(w_2).append(" "),
clojure.print_method.apply(null,[clojure.val.apply(null,[e_1]),w_2]))}),", ","}",clojure.seq.apply(null,[m_1]),w_2]))})))}).apply(null,[]);

//======
//(defmethod print-method clojure.lang.IPersistentSet [s w] (print-meta s w) (print-sequential "#{" print-method " " "}" (seq s) w))
//---
(function __clojure_fn_8029(){
return ((clojure.print_method).addMethod(clojure.lang.IPersistentSet,(function __clojure_fn_8029_fn_8031(s_1,w_2){
return (clojure.print_meta.apply(null,[s_1,w_2]),
clojure.print_sequential.apply(null,["#{",clojure.print_method," ","}",clojure.seq.apply(null,[s_1]),w_2]))})))}).apply(null,[]);

//======
//(def char-name-string {\space "space", \backspace "backspace", \tab "tab", \newline "newline", \formfeed "formfeed", \return "return"})
//---
(function __clojure_fn_8038(){
return (clojure.JS.def(clojure,"char_name_string",clojure.hash_map(" ","space","\b","backspace","\t","tab","\n","newline","\f","formfeed","\r","return")))}).apply(null,[]);

//======
//(defmethod print-method java.lang.Character [c w] (if *print-readably* (do (.append w \\) (let [n (char-name-string c)] (if n (.write w n) (.append w c)))) (.append w c)) nil)
//---
(function __clojure_fn_8047(){
return ((clojure.print_method).addMethod(java.lang.Character,(function __clojure_fn_8047_fn_8049(c_1,w_2){
var n_3;
return (((clojure._STAR_print_readably_STAR_)?((w_2).append("\\"),
((n_3=clojure.char_name_string.apply(null,[c_1])),
((n_3)?((w_2).write(n_3)):((w_2).append(c_1))))):((w_2).append(c_1))),
null)})))}).apply(null,[]);

//======
//(defmethod print-method Class [c w] (.write w (.getName c)))
//---
(function __clojure_fn_8059(){
return ((clojure.print_method).addMethod(java.lang.Class,(function __clojure_fn_8059_fn_8061(c_1,w_2){
return ((w_2).write((c_1).getName()))})))}).apply(null,[]);

//======
//(defmethod print-method java.math.BigDecimal [b w] (.write w (str b)) (.write w "M"))
//---
(function __clojure_fn_8071(){
return ((clojure.print_method).addMethod(java.math.BigDecimal,(function __clojure_fn_8071_fn_8073(b_1,w_2){
return ((w_2).write(clojure.str.apply(null,[b_1])),
(w_2).write("M"))})))}).apply(null,[]);

//======
//(defmethod print-method java.util.regex.Pattern [p w] (.append w \#) (print-method (str p) w))
//---
(function __clojure_fn_8083(){
return ((clojure.print_method).addMethod(java.util.regex.Pattern,(function __clojure_fn_8083_fn_8085(p_1,w_2){
return ((w_2).append("#"),
clojure.print_method.apply(null,[clojure.str.apply(null,[p_1]),w_2]))})))}).apply(null,[]);
