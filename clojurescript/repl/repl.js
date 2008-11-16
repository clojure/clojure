
//======
//(ns jsrepl)
//---
(function __user_fn_398(){
return (clojure.core.in_ns.apply(null,[clojure.core.symbol("jsrepl")]),
clojure.core.refer.apply(null,[clojure.core.symbol("clojure.core")]))}).apply(null,[]);

//======
//(def append-dom)
//---
(function __jsrepl_fn_404(){
return (clojure.JS.def(jsrepl,"append_dom",null))}).apply(null,[]);

//======
//(defn dom [o] (if (coll? o) (let [[tag attrs & body] o] (if (keyword? tag) (let [elem (.createElement document (name tag))] (when (map? attrs) (doseq [[k v] attrs] (when v (.setAttribute elem (name k) v)))) [(append-dom elem (if (map? attrs) body (cons attrs body)))]) (mapcat dom o))) (when o [(.createTextNode document (str o))])))
//---
(function __jsrepl_fn_407(){
return (clojure.JS.def(jsrepl,"dom",(function __jsrepl_fn_407_dom_409(o_1){
var elem_6,v_10,body_5,attrs_4,tag_3,sq__2972_7,k_9,vec__412_8,vec__411_2;
return (((clojure.core.coll_QMARK_.apply(null,[o_1]))?(((vec__411_2=o_1),
(tag_3=clojure.core.nth.apply(null,[vec__411_2,(0),null])),
(attrs_4=clojure.core.nth.apply(null,[vec__411_2,(1),null])),
(body_5=clojure.core.nthrest.apply(null,[vec__411_2,(2)])),
((clojure.core.keyword_QMARK_.apply(null,[tag_3]))?(((elem_6=(clojure.JS.resolveVar("document",jsrepl)).createElement(clojure.core.name.apply(null,[tag_3]))),
((clojure.core.map_QMARK_.apply(null,[attrs_4]))?(((function __loop(){var _rtn,_cnt;(sq__2972_7=clojure.core.seq.apply(null,[attrs_4]));do{_cnt=0;
_rtn=((sq__2972_7)?(((vec__412_8=clojure.core.first.apply(null,[sq__2972_7])),
(k_9=clojure.core.nth.apply(null,[vec__412_8,(0),null])),
(v_10=clojure.core.nth.apply(null,[vec__412_8,(1),null])),
((true)?(((true)?(((v_10)?((elem_6).setAttribute(clojure.core.name.apply(null,[k_9]),v_10)):(null))):(null)),
(_cnt=1,_rtn=[clojure.core.rest.apply(null,[sq__2972_7])],sq__2972_7=_rtn[0])):(null)))):(null))}while(_cnt);return _rtn;})())):(null)),
clojure.JS.lit_vector([jsrepl.append_dom.apply(null,[elem_6,((clojure.core.map_QMARK_.apply(null,[attrs_4]))?(body_5):(clojure.core.cons.apply(null,[attrs_4,body_5])))])]))):(clojure.core.mapcat.apply(null,[jsrepl.dom,o_1]))))):(((o_1)?(clojure.JS.lit_vector([(clojure.JS.resolveVar("document",jsrepl)).createTextNode(clojure.core.str.apply(null,[o_1]))])):(null)))))})))}).apply(null,[]);

//======
//(defn append-dom [parent v] (doseq [i (dom v)] (.appendChild parent i)) parent)
//---
(function __jsrepl_fn_415(){
return (clojure.JS.def(jsrepl,"append_dom",(function __jsrepl_fn_415_append_dom_417(parent_1,v_2){
var sq__2972_3,i_4;
return (((function __loop(){var _rtn,_cnt;(sq__2972_3=clojure.core.seq.apply(null,[jsrepl.dom.apply(null,[v_2])]));do{_cnt=0;
_rtn=((sq__2972_3)?(((i_4=clojure.core.first.apply(null,[sq__2972_3])),
((true)?(((true)?((parent_1).appendChild(i_4)):(null)),
(_cnt=1,_rtn=[clojure.core.rest.apply(null,[sq__2972_3])],sq__2972_3=_rtn[0])):(null)))):(null))}while(_cnt);return _rtn;})()),
parent_1)})))}).apply(null,[]);

//======
//(def elems)
//---
(function __jsrepl_fn_421(){
return (clojure.JS.def(jsrepl,"elems",null))}).apply(null,[]);

//======
//(def lastval)
//---
(function __jsrepl_fn_424(){
return (clojure.JS.def(jsrepl,"lastval",null))}).apply(null,[]);

//======
//(def *print-class* nil)
//---
(function __jsrepl_fn_427(){
return (clojure.JS.def(jsrepl,"_STAR_print_class_STAR_",null))}).apply(null,[]);

//======
//(defn repl-print [text] (let [log (:log elems)] (doseq [line (.split text #"\n")] (append-dom log [:div {:class (str "cg " (when *print-class* (str " " *print-class*)))} line])) (set! (.scrollTop log) (.scrollHeight log))))
//---
(function __jsrepl_fn_430(){
return (clojure.JS.def(jsrepl,"repl_print",(function __jsrepl_fn_430_repl_print_432(text_1){
var sq__2972_3,log_2,line_4;
return (((log_2=clojure.core.keyword("","log").apply(null,[jsrepl.elems])),
((function __loop(){var _rtn,_cnt;(sq__2972_3=clojure.core.seq.apply(null,[(text_1).split((/\n/))]));do{_cnt=0;
_rtn=((sq__2972_3)?(((line_4=clojure.core.first.apply(null,[sq__2972_3])),
((true)?(((true)?(jsrepl.append_dom.apply(null,[log_2,clojure.JS.lit_vector([clojure.core.keyword("","div"),clojure.core.hash_map(clojure.core.keyword("","class"),clojure.core.str.apply(null,["cg ",((jsrepl._STAR_print_class_STAR_)?(clojure.core.str.apply(null,[" ",jsrepl._STAR_print_class_STAR_])):(null))])),line_4])])):(null)),
(_cnt=1,_rtn=[clojure.core.rest.apply(null,[sq__2972_3])],sq__2972_3=_rtn[0])):(null)))):(null))}while(_cnt);return _rtn;})()),
(log_2.scrollTop=clojure.JS.getOrRun(log_2,"scrollHeight"))))})))}).apply(null,[]);

//======
//(defn postexpr [] (append-dom (:log elems) [:table [:tbody [:tr [:td {:class "cg"} "user=> "] [:td (-> :input elems .value (.replace #"\n$" ""))]]]]) (set! (-> :scripts elems .innerHTML) "") (set! (-> :input elems .value) ""))
//---
(function __jsrepl_fn_436(){
return (clojure.JS.def(jsrepl,"postexpr",(function __jsrepl_fn_436_postexpr_438(){
return (jsrepl.append_dom.apply(null,[clojure.core.keyword("","log").apply(null,[jsrepl.elems]),clojure.JS.lit_vector([clojure.core.keyword("","table"),clojure.JS.lit_vector([clojure.core.keyword("","tbody"),clojure.JS.lit_vector([clojure.core.keyword("","tr"),clojure.JS.lit_vector([clojure.core.keyword("","td"),clojure.core.hash_map(clojure.core.keyword("","class"),"cg"),"user=> "]),clojure.JS.lit_vector([clojure.core.keyword("","td"),(clojure.JS.getOrRun(jsrepl.elems.apply(null,[clojure.core.keyword("","input")]),"value")).replace((/\n$/),"")])])])])]),
(jsrepl.elems.apply(null,[clojure.core.keyword("","scripts")]).innerHTML=""),
(jsrepl.elems.apply(null,[clojure.core.keyword("","input")]).value=""))})))}).apply(null,[]);
// Skipping: (defmacro print-with-class [c m] (clojure.core/concat (clojure.core/list (quote clojure.core/binding)) (clojure.core/list (clojure.core/apply clojure.core/vector (clojure.core/concat (clojure.core/list (quote jsrepl/*print-class*)) (clojure.core/list c)))) (clojure.core/list (clojure.core/concat (clojure.core/list (quote clojure.core/println)) (clojure.core/list m)))))

//======
//(defn show-state [url] (set! (-> :status elems .src) url))
//---
(function __jsrepl_fn_451(){
return (clojure.JS.def(jsrepl,"show_state",(function __jsrepl_fn_451_show_state_453(url_1){
return ((jsrepl.elems.apply(null,[clojure.core.keyword("","status")]).src=url_1))})))}).apply(null,[]);

//======
//(defn state [status msg] (cond (= status "incomplete") (show-state "dots.png") (= status "done") (prn lastval) (= status "error") (do (postexpr) (show-state "blank.gif") (print-with-class "err" msg)) (= status "compiled") (do (postexpr) (setTimeout (fn* [] (show-state "blank.gif")) 0))))
//---
(function __jsrepl_fn_457(){
return (clojure.JS.def(jsrepl,"state",(function __jsrepl_fn_457_state_459(status_1,msg_2){
return (((clojure.lang.Util.equal(status_1,"incomplete"))?(jsrepl.show_state.apply(null,["dots.png"])):(((clojure.lang.Util.equal(status_1,"done"))?(clojure.core.prn.apply(null,[jsrepl.lastval])):(((clojure.lang.Util.equal(status_1,"error"))?(jsrepl.postexpr.apply(null,[]),
jsrepl.show_state.apply(null,["blank.gif"]),
clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[jsrepl._var__STAR_print_class_STAR_,"err"])),
(function __try(){try{var _rtn=(clojure.core.println.apply(null,[msg_2]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})()):(((clojure.lang.Util.equal(status_1,"compiled"))?(jsrepl.postexpr.apply(null,[]),
clojure.JS.resolveVar("setTimeout",jsrepl).apply(null,[(function __jsrepl_fn_457_state_459_fn_461(){
return (jsrepl.show_state.apply(null,["blank.gif"]))}),(0)])):(null)))))))))})))}).apply(null,[]);

//======
//(defn err [e] (print-with-class "err" e) (set! *e e))
//---
(function __jsrepl_fn_466(){
return (clojure.JS.def(jsrepl,"err",(function __jsrepl_fn_466_err_468(e_1){
return (clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[jsrepl._var__STAR_print_class_STAR_,"err"])),
(function __jsrepl_fn_466_err_468_fn_470(){
return ((function __try(){try{var _rtn=(clojure.core.println.apply(null,[e_1]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())}).apply(null,[]),
clojure.core._var__STAR_e.set(e_1))})))}).apply(null,[]);

//======
//(set! *print-length* 103)
//---
(function __jsrepl_fn_475(){
return (clojure.core._var__STAR_print_length_STAR_.set((103)))}).apply(null,[]);

//======
//(set! (.onload window) (fn [] (set! elems (into {} (for [n (quote (log input status scripts))] [(keyword n) (.getElementById document (str n))]))) (set! (.print window) repl-print) (set! (.onkeypress (:input elems)) (fn [e] (let [e (or e event)] (when (== (.keyCode e) 13) (set! (-> :status elems .src) "clojure-logo-anim-03.gif") (append-dom (:scripts elems) [:script {:src (str "http://localhost:8081/" (-> :input elems .value escape (.replace #"\+" "%2b")))}]))))) (println "ClojureScript") (.focus (:input elems))))
//---
(function __jsrepl_fn_478(){
return ((clojure.JS.resolveVar("window",jsrepl).onload=(function __jsrepl_fn_478_fn_480(){
var iter__3425_1;
return (jsrepl._var_elems.set(clojure.core.into.apply(null,[clojure.lang.PersistentHashMap.EMPTY,((iter__3425_1=(function __jsrepl_fn_478_fn_480_iter_482_484(s__483_1){
var n_2,iter__482_0=arguments.callee;
return (((clojure.core.seq.apply(null,[s__483_1]))?(((n_2=clojure.core.first.apply(null,[s__483_1])),
((true)?((new clojure.lang.LazyCons((function __jsrepl_fn_478_fn_480_iter_482_484_fn_486(G__485_1){switch(arguments.length){
case 0:return (clojure.JS.lit_vector([clojure.core.keyword.apply(null,[n_2]),(clojure.JS.resolveVar("document",jsrepl)).getElementById(clojure.core.str.apply(null,[n_2]))]))}
return (iter__482_0.apply(null,[clojure.core.rest.apply(null,[s__483_1])]))})))):(null)))):(null)))})),
iter__3425_1.apply(null,[clojure.JS.lit_list([clojure.core.symbol("log"),clojure.core.symbol("input"),clojure.core.symbol("status"),clojure.core.symbol("scripts")])]))])),
(clojure.JS.resolveVar("window",jsrepl).print=jsrepl.repl_print),
(clojure.core.keyword("","input").apply(null,[jsrepl.elems]).onkeypress=(function __jsrepl_fn_478_fn_480_fn_491(e_1){
var or__2371_2,e_2;
return (((e_2=((or__2371_2=e_1),
((or__2371_2)?(or__2371_2):(clojure.JS.resolveVar("event",jsrepl))))),
((clojure.lang.Numbers.equiv(clojure.JS.getOrRun(e_2,"keyCode"),(13)))?((jsrepl.elems.apply(null,[clojure.core.keyword("","status")]).src="clojure-logo-anim-03.gif"),
jsrepl.append_dom.apply(null,[clojure.core.keyword("","scripts").apply(null,[jsrepl.elems]),clojure.JS.lit_vector([clojure.core.keyword("","script"),clojure.core.hash_map(clojure.core.keyword("","src"),clojure.core.str.apply(null,["http://localhost:8081/",(clojure.JS.resolveVar("escape",jsrepl).apply(null,[clojure.JS.getOrRun(jsrepl.elems.apply(null,[clojure.core.keyword("","input")]),"value")])).replace((/\+/),"%2b")]))])])):(null))))})),
clojure.core.println.apply(null,["ClojureScript"]),
clojure.JS.getOrRun(clojure.core.keyword("","input").apply(null,[jsrepl.elems]),"focus"))})))}).apply(null,[]);
