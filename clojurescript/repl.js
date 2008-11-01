
//======
//(ns jsrepl)
//---
(function __user_fn_2877(){
return (clojure.in_ns.apply(null,[clojure.symbol("jsrepl")]),
clojure.refer.apply(null,[clojure.symbol("clojure")]))}).apply(null,[]);

//======
//(def append-dom)
//---
(function __jsrepl_fn_2883(){
return (clojure.JS.def(jsrepl,"append_dom",null))}).apply(null,[]);

//======
//(defn dom [o] (if (coll? o) (let [[tag attrs & body] o] (cond (coll? tag) (mapcat dom o) (keyword? tag) (let [elem (.createElement document (name tag))] (when (map? attrs) (doseq [k v] attrs (.setAttribute elem (name k) v))) [(append-dom elem (if (map? attrs) body (cons attrs body)))]) :else (lazy-cons (.createTextNode document (str (first o))) (dom (rest o))))) [(.createTextNode document (str o))]))
//---
(function __jsrepl_fn_2886(){
return (clojure.JS.def(jsrepl,"dom",(function __jsrepl_fn_2886_dom_2888(o_1){
var k_9,attrs_4,vec__2891_8,vec__2890_2,body_5,list__794_7,tag_3,elem_6,v_10;
return (((clojure.coll_QMARK_.apply(null,[o_1]))?(((vec__2890_2=o_1),
(tag_3=clojure.nth.apply(null,[vec__2890_2,(0),null])),
(attrs_4=clojure.nth.apply(null,[vec__2890_2,(1),null])),
(body_5=clojure.nthrest.apply(null,[vec__2890_2,(2)])),
((clojure.coll_QMARK_.apply(null,[tag_3]))?(clojure.mapcat.apply(null,[jsrepl.dom,o_1])):(((clojure.keyword_QMARK_.apply(null,[tag_3]))?(((elem_6=(clojure.JS.resolveVar("document",jsrepl)).createElement(clojure.name.apply(null,[tag_3]))),
((clojure.map_QMARK_.apply(null,[attrs_4]))?(((function __loop(){var _rtn,_cnt;(list__794_7=clojure.seq.apply(null,[attrs_4]));do{_cnt=0;
_rtn=((list__794_7)?(((vec__2891_8=clojure.first.apply(null,[list__794_7])),
(k_9=clojure.nth.apply(null,[vec__2891_8,(0),null])),
(v_10=clojure.nth.apply(null,[vec__2891_8,(1),null])),
(elem_6).setAttribute(clojure.name.apply(null,[k_9]),v_10)),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__794_7])],list__794_7=_rtn[0])):(null))}while(_cnt);return _rtn;})())):(null)),
clojure.JS.lit_vector([jsrepl.append_dom.apply(null,[elem_6,((clojure.map_QMARK_.apply(null,[attrs_4]))?(body_5):(clojure.cons.apply(null,[attrs_4,body_5])))])]))):(((clojure.keyword("","else"))?((new clojure.lang.LazyCons((function __jsrepl_fn_2886_dom_2888_fn_2893(G__2892_1){switch(arguments.length){
case 0:return ((clojure.JS.resolveVar("document",jsrepl)).createTextNode(clojure.str.apply(null,[clojure.first.apply(null,[o_1])])))}
return (jsrepl.dom.apply(null,[clojure.rest.apply(null,[o_1])]))})))):(null)))))))):(clojure.JS.lit_vector([(clojure.JS.resolveVar("document",jsrepl)).createTextNode(clojure.str.apply(null,[o_1]))]))))})))}).apply(null,[]);

//======
//(defn append-dom [parent v] (doseq i (dom v) (.appendChild parent i)) parent)
//---
(function __jsrepl_fn_2899(){
return (clojure.JS.def(jsrepl,"append_dom",(function __jsrepl_fn_2899_append_dom_2901(parent_1,v_2){
var i_4,list__794_3;
return (((function __loop(){var _rtn,_cnt;(list__794_3=clojure.seq.apply(null,[jsrepl.dom.apply(null,[v_2])]));do{_cnt=0;
_rtn=((list__794_3)?(((i_4=clojure.first.apply(null,[list__794_3])),
(parent_1).appendChild(i_4)),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__794_3])],list__794_3=_rtn[0])):(null))}while(_cnt);return _rtn;})()),
parent_1)})))}).apply(null,[]);

//======
//(def elems)
//---
(function __jsrepl_fn_2905(){
return (clojure.JS.def(jsrepl,"elems",null))}).apply(null,[]);

//======
//(defn repl-print [text] (let [log (:log elems)] (doseq line (.split text #"\n") (append-dom log [:div {:class "cg"} line])) (set! (.scrollTop log) (.scrollHeight log))))
//---
(function __jsrepl_fn_2908(){
return (clojure.JS.def(jsrepl,"repl_print",(function __jsrepl_fn_2908_repl_print_2910(text_1){
var list__794_3,log_2,line_4;
return (((log_2=clojure.keyword("","log").apply(null,[jsrepl.elems])),
((function __loop(){var _rtn,_cnt;(list__794_3=clojure.seq.apply(null,[(text_1).split((/\n/))]));do{_cnt=0;
_rtn=((list__794_3)?(((line_4=clojure.first.apply(null,[list__794_3])),
jsrepl.append_dom.apply(null,[log_2,clojure.JS.lit_vector([clojure.keyword("","div"),clojure.hash_map(clojure.keyword("","class"),"cg"),line_4])])),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__794_3])],list__794_3=_rtn[0])):(null))}while(_cnt);return _rtn;})()),
(log_2.scrollTop=clojure.JS.getOrRun(log_2,"scrollHeight"))))})))}).apply(null,[]);

//======
//(defn postexpr [] (append-dom (:log elems) [:table [:tbody [:tr [:td {:class "cg"} "user=> "] [:td (-> :input elems .value (.replace #"\n$" ""))]]]]) (set! (-> :scripts elems .innerHTML) "") (set! (-> :input elems .value) ""))
//---
(function __jsrepl_fn_2914(){
return (clojure.JS.def(jsrepl,"postexpr",(function __jsrepl_fn_2914_postexpr_2916(){
return (jsrepl.append_dom.apply(null,[clojure.keyword("","log").apply(null,[jsrepl.elems]),clojure.JS.lit_vector([clojure.keyword("","table"),clojure.JS.lit_vector([clojure.keyword("","tbody"),clojure.JS.lit_vector([clojure.keyword("","tr"),clojure.JS.lit_vector([clojure.keyword("","td"),clojure.hash_map(clojure.keyword("","class"),"cg"),"user=> "]),clojure.JS.lit_vector([clojure.keyword("","td"),(clojure.JS.getOrRun(jsrepl.elems.apply(null,[clojure.keyword("","input")]),"value")).replace((/\n$/),"")])])])])]),
(jsrepl.elems.apply(null,[clojure.keyword("","scripts")]).innerHTML=""),
(jsrepl.elems.apply(null,[clojure.keyword("","input")]).value=""))})))}).apply(null,[]);

//======
//(defn state [status msg] (set! (.innerHTML (:status elems)) status) (cond (= status "error") (do (postexpr) (print msg)) (= status "compiled") (postexpr)))
//---
(function __jsrepl_fn_2920(){
return (clojure.JS.def(jsrepl,"state",(function __jsrepl_fn_2920_state_2922(status_1,msg_2){
return ((clojure.keyword("","status").apply(null,[jsrepl.elems]).innerHTML=status_1),
((clojure.lang.Util.equal(status_1,"error"))?(jsrepl.postexpr.apply(null,[]),
clojure.print.apply(null,[msg_2])):(((clojure.lang.Util.equal(status_1,"compiled"))?(jsrepl.postexpr.apply(null,[])):(null)))))})))}).apply(null,[]);

//======
//(defn err [e] (println e) (set! *e e))
//---
(function __jsrepl_fn_2926(){
return (clojure.JS.def(jsrepl,"err",(function __jsrepl_fn_2926_err_2928(e_1){
return (clojure.println.apply(null,[e_1]),
clojure._var__STAR_e.set(e_1))})))}).apply(null,[]);

//======
//(set! *print-length* 103)
//---
(function __jsrepl_fn_2932(){
return (clojure._var__STAR_print_length_STAR_.set((103)))}).apply(null,[]);

//======
//(set! (.onload window) (fn [] (set! elems (into {} (for [n (quote (log input status scripts))] [(keyword n) (.getElementById document (str n))]))) (set! (.print window) repl-print) (set! (.onkeypress (:input elems)) (fn [e] (let [e (or e event)] (when (== (.keyCode e) 13) (let [s (.createElement document "script")] (set! (.src s) (str "http://localhost:8081/" (-> :input elems .value escape (.replace #"\+" "%2b")))) (.appendChild (:scripts elems) s)))))) (println "ClojureScript") (.focus (:input elems))))
//---
(function __jsrepl_fn_2935(){
return ((clojure.JS.resolveVar("window",jsrepl).onload=(function __jsrepl_fn_2935_fn_2937(){
var iter__1247_1;
return (jsrepl._var_elems.set(clojure.into.apply(null,[clojure.lang.PersistentHashMap.EMPTY,((iter__1247_1=(function __jsrepl_fn_2935_fn_2937_iter_2939_2941(s__2940_1){
var n_2,iter__2939_0=arguments.callee;
return (((clojure.seq.apply(null,[s__2940_1]))?(((n_2=clojure.first.apply(null,[s__2940_1])),
((true)?((new clojure.lang.LazyCons((function __jsrepl_fn_2935_fn_2937_iter_2939_2941_fn_2943(G__2942_1){switch(arguments.length){
case 0:return (clojure.JS.lit_vector([clojure.keyword.apply(null,[n_2]),(clojure.JS.resolveVar("document",jsrepl)).getElementById(clojure.str.apply(null,[n_2]))]))}
return (iter__2939_0.apply(null,[clojure.rest.apply(null,[s__2940_1])]))})))):(null)))):(null)))})),
iter__1247_1.apply(null,[clojure.JS.lit_list([clojure.symbol("log"),clojure.symbol("input"),clojure.symbol("status"),clojure.symbol("scripts")])]))])),
(clojure.JS.resolveVar("window",jsrepl).print=jsrepl.repl_print),
(clojure.keyword("","input").apply(null,[jsrepl.elems]).onkeypress=(function __jsrepl_fn_2935_fn_2937_fn_2948(e_1){
var or__202_2,e_2,s_3;
return (((e_2=((or__202_2=e_1),
((or__202_2)?(or__202_2):(clojure.JS.resolveVar("event",jsrepl))))),
((clojure.lang.Numbers.equiv(clojure.JS.getOrRun(e_2,"keyCode"),(13)))?(((s_3=(clojure.JS.resolveVar("document",jsrepl)).createElement("script")),
(s_3.src=clojure.str.apply(null,["http://localhost:8081/",(clojure.JS.resolveVar("escape",jsrepl).apply(null,[clojure.JS.getOrRun(jsrepl.elems.apply(null,[clojure.keyword("","input")]),"value")])).replace((/\+/),"%2b")])),
(clojure.keyword("","scripts").apply(null,[jsrepl.elems])).appendChild(s_3))):(null))))})),
clojure.println.apply(null,["ClojureScript"]),
clojure.JS.getOrRun(clojure.keyword("","input").apply(null,[jsrepl.elems]),"focus"))})))}).apply(null,[]);
