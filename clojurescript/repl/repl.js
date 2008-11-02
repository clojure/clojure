
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
//(defn dom [o] (if (coll? o) (let [[tag attrs & body] o] (if (keyword? tag) (let [elem (.createElement document (name tag))] (when (map? attrs) (doseq [k v] attrs (when v (.setAttribute elem (name k) v)))) [(append-dom elem (if (map? attrs) body (cons attrs body)))]) (mapcat dom o))) (when o [(.createTextNode document (str o))])))
//---
(function __jsrepl_fn_2886(){
return (clojure.JS.def(jsrepl,"dom",(function __jsrepl_fn_2886_dom_2888(o_1){
var body_5,vec__2891_8,tag_3,v_10,attrs_4,k_9,vec__2890_2,list__794_7,elem_6;
return (((clojure.coll_QMARK_.apply(null,[o_1]))?(((vec__2890_2=o_1),
(tag_3=clojure.nth.apply(null,[vec__2890_2,(0),null])),
(attrs_4=clojure.nth.apply(null,[vec__2890_2,(1),null])),
(body_5=clojure.nthrest.apply(null,[vec__2890_2,(2)])),
((clojure.keyword_QMARK_.apply(null,[tag_3]))?(((elem_6=(clojure.JS.resolveVar("document",jsrepl)).createElement(clojure.name.apply(null,[tag_3]))),
((clojure.map_QMARK_.apply(null,[attrs_4]))?(((function __loop(){var _rtn,_cnt;(list__794_7=clojure.seq.apply(null,[attrs_4]));do{_cnt=0;
_rtn=((list__794_7)?(((vec__2891_8=clojure.first.apply(null,[list__794_7])),
(k_9=clojure.nth.apply(null,[vec__2891_8,(0),null])),
(v_10=clojure.nth.apply(null,[vec__2891_8,(1),null])),
((v_10)?((elem_6).setAttribute(clojure.name.apply(null,[k_9]),v_10)):(null))),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__794_7])],list__794_7=_rtn[0])):(null))}while(_cnt);return _rtn;})())):(null)),
clojure.JS.lit_vector([jsrepl.append_dom.apply(null,[elem_6,((clojure.map_QMARK_.apply(null,[attrs_4]))?(body_5):(clojure.cons.apply(null,[attrs_4,body_5])))])]))):(clojure.mapcat.apply(null,[jsrepl.dom,o_1]))))):(((o_1)?(clojure.JS.lit_vector([(clojure.JS.resolveVar("document",jsrepl)).createTextNode(clojure.str.apply(null,[o_1]))])):(null)))))})))}).apply(null,[]);

//======
//(defn append-dom [parent v] (doseq i (dom v) (.appendChild parent i)) parent)
//---
(function __jsrepl_fn_2894(){
return (clojure.JS.def(jsrepl,"append_dom",(function __jsrepl_fn_2894_append_dom_2896(parent_1,v_2){
var list__794_3,i_4;
return (((function __loop(){var _rtn,_cnt;(list__794_3=clojure.seq.apply(null,[jsrepl.dom.apply(null,[v_2])]));do{_cnt=0;
_rtn=((list__794_3)?(((i_4=clojure.first.apply(null,[list__794_3])),
(parent_1).appendChild(i_4)),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__794_3])],list__794_3=_rtn[0])):(null))}while(_cnt);return _rtn;})()),
parent_1)})))}).apply(null,[]);

//======
//(def elems)
//---
(function __jsrepl_fn_2900(){
return (clojure.JS.def(jsrepl,"elems",null))}).apply(null,[]);

//======
//(def lastval)
//---
(function __jsrepl_fn_2903(){
return (clojure.JS.def(jsrepl,"lastval",null))}).apply(null,[]);

//======
//(def *print-color* nil)
//---
(function __jsrepl_fn_2906(){
return (clojure.JS.def(jsrepl,"_STAR_print_color_STAR_",null))}).apply(null,[]);

//======
//(defn repl-print [text] (let [log (:log elems)] (doseq line (.split text #"\n") (append-dom log [:div {:class "cg", :style (when *print-color* (str "color:" *print-color*))} line])) (set! (.scrollTop log) (.scrollHeight log))))
//---
(function __jsrepl_fn_2909(){
return (clojure.JS.def(jsrepl,"repl_print",(function __jsrepl_fn_2909_repl_print_2911(text_1){
var list__794_3,log_2,line_4;
return (((log_2=clojure.keyword("","log").apply(null,[jsrepl.elems])),
((function __loop(){var _rtn,_cnt;(list__794_3=clojure.seq.apply(null,[(text_1).split((/\n/))]));do{_cnt=0;
_rtn=((list__794_3)?(((line_4=clojure.first.apply(null,[list__794_3])),
jsrepl.append_dom.apply(null,[log_2,clojure.JS.lit_vector([clojure.keyword("","div"),clojure.hash_map(clojure.keyword("","class"),"cg",clojure.keyword("","style"),((jsrepl._STAR_print_color_STAR_)?(clojure.str.apply(null,["color:",jsrepl._STAR_print_color_STAR_])):(null))),line_4])])),
(_cnt=1,_rtn=[clojure.rest.apply(null,[list__794_3])],list__794_3=_rtn[0])):(null))}while(_cnt);return _rtn;})()),
(log_2.scrollTop=clojure.JS.getOrRun(log_2,"scrollHeight"))))})))}).apply(null,[]);

//======
//(defn postexpr [] (append-dom (:log elems) [:table [:tbody [:tr [:td {:class "cg"} "user=> "] [:td (-> :input elems .value (.replace #"\n$" ""))]]]]) (set! (-> :scripts elems .innerHTML) "") (set! (-> :input elems .value) ""))
//---
(function __jsrepl_fn_2915(){
return (clojure.JS.def(jsrepl,"postexpr",(function __jsrepl_fn_2915_postexpr_2917(){
return (jsrepl.append_dom.apply(null,[clojure.keyword("","log").apply(null,[jsrepl.elems]),clojure.JS.lit_vector([clojure.keyword("","table"),clojure.JS.lit_vector([clojure.keyword("","tbody"),clojure.JS.lit_vector([clojure.keyword("","tr"),clojure.JS.lit_vector([clojure.keyword("","td"),clojure.hash_map(clojure.keyword("","class"),"cg"),"user=> "]),clojure.JS.lit_vector([clojure.keyword("","td"),(clojure.JS.getOrRun(jsrepl.elems.apply(null,[clojure.keyword("","input")]),"value")).replace((/\n$/),"")])])])])]),
(jsrepl.elems.apply(null,[clojure.keyword("","scripts")]).innerHTML=""),
(jsrepl.elems.apply(null,[clojure.keyword("","input")]).value=""))})))}).apply(null,[]);

//======
//(defn print-err [m] (binding [*print-color* "#a00"] (println m)))
//---
(function __jsrepl_fn_2921(){
return (clojure.JS.def(jsrepl,"print_err",(function __jsrepl_fn_2921_print_err_2923(m_1){
return (clojure.lang.Var.pushThreadBindings(clojure.hash_map.apply(null,[jsrepl._var__STAR_print_color_STAR_,"#a00"])),
(function __try(){try{var _rtn=(clojure.println.apply(null,[m_1]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())})))}).apply(null,[]);

//======
//(defn show-state [url] (set! (-> :status elems .src) url))
//---
(function __jsrepl_fn_2927(){
return (clojure.JS.def(jsrepl,"show_state",(function __jsrepl_fn_2927_show_state_2929(url_1){
return ((jsrepl.elems.apply(null,[clojure.keyword("","status")]).src=url_1))})))}).apply(null,[]);

//======
//(defn state [status msg] (cond (= status "incomplete") (show-state "dots.png") (= status "done") (prn lastval) (= status "error") (do (postexpr) (show-state "blank.gif") (print-err msg)) (= status "compiled") (do (postexpr) (setTimeout (fn* [] (show-state "blank.gif")) 0))))
//---
(function __jsrepl_fn_2933(){
return (clojure.JS.def(jsrepl,"state",(function __jsrepl_fn_2933_state_2935(status_1,msg_2){
return (((clojure.lang.Util.equal(status_1,"incomplete"))?(jsrepl.show_state.apply(null,["dots.png"])):(((clojure.lang.Util.equal(status_1,"done"))?(clojure.prn.apply(null,[jsrepl.lastval])):(((clojure.lang.Util.equal(status_1,"error"))?(jsrepl.postexpr.apply(null,[]),
jsrepl.show_state.apply(null,["blank.gif"]),
jsrepl.print_err.apply(null,[msg_2])):(((clojure.lang.Util.equal(status_1,"compiled"))?(jsrepl.postexpr.apply(null,[]),
clojure.JS.resolveVar("setTimeout",jsrepl).apply(null,[(function __jsrepl_fn_2933_state_2935_fn_2937(){
return (jsrepl.show_state.apply(null,["blank.gif"]))}),(0)])):(null)))))))))})))}).apply(null,[]);

//======
//(defn err [e] (print-err e) (set! *e e))
//---
(function __jsrepl_fn_2942(){
return (clojure.JS.def(jsrepl,"err",(function __jsrepl_fn_2942_err_2944(e_1){
return (jsrepl.print_err.apply(null,[e_1]),
clojure._var__STAR_e.set(e_1))})))}).apply(null,[]);

//======
//(set! *print-length* 103)
//---
(function __jsrepl_fn_2948(){
return (clojure._var__STAR_print_length_STAR_.set((103)))}).apply(null,[]);

//======
//(set! (.onload window) (fn [] (set! elems (into {} (for [n (quote (log input status scripts))] [(keyword n) (.getElementById document (str n))]))) (set! (.print window) repl-print) (set! (.onkeypress (:input elems)) (fn [e] (let [e (or e event)] (when (== (.keyCode e) 13) (set! (-> :status elems .src) "clojure-logo-anim-03.gif") (append-dom (:scripts elems) [:script {:src (str "http://localhost:8081/" (-> :input elems .value escape (.replace #"\+" "%2b")))}]))))) (println "ClojureScript") (.focus (:input elems))))
//---
(function __jsrepl_fn_2951(){
return ((clojure.JS.resolveVar("window",jsrepl).onload=(function __jsrepl_fn_2951_fn_2953(){
var iter__1247_1;
return (jsrepl._var_elems.set(clojure.into.apply(null,[clojure.lang.PersistentHashMap.EMPTY,((iter__1247_1=(function __jsrepl_fn_2951_fn_2953_iter_2955_2957(s__2956_1){
var n_2,iter__2955_0=arguments.callee;
return (((clojure.seq.apply(null,[s__2956_1]))?(((n_2=clojure.first.apply(null,[s__2956_1])),
((true)?((new clojure.lang.LazyCons((function __jsrepl_fn_2951_fn_2953_iter_2955_2957_fn_2959(G__2958_1){switch(arguments.length){
case 0:return (clojure.JS.lit_vector([clojure.keyword.apply(null,[n_2]),(clojure.JS.resolveVar("document",jsrepl)).getElementById(clojure.str.apply(null,[n_2]))]))}
return (iter__2955_0.apply(null,[clojure.rest.apply(null,[s__2956_1])]))})))):(null)))):(null)))})),
iter__1247_1.apply(null,[clojure.JS.lit_list([clojure.symbol("log"),clojure.symbol("input"),clojure.symbol("status"),clojure.symbol("scripts")])]))])),
(clojure.JS.resolveVar("window",jsrepl).print=jsrepl.repl_print),
(clojure.keyword("","input").apply(null,[jsrepl.elems]).onkeypress=(function __jsrepl_fn_2951_fn_2953_fn_2964(e_1){
var or__202_2,e_2;
return (((e_2=((or__202_2=e_1),
((or__202_2)?(or__202_2):(clojure.JS.resolveVar("event",jsrepl))))),
((clojure.lang.Numbers.equiv(clojure.JS.getOrRun(e_2,"keyCode"),(13)))?((jsrepl.elems.apply(null,[clojure.keyword("","status")]).src="clojure-logo-anim-03.gif"),
jsrepl.append_dom.apply(null,[clojure.keyword("","scripts").apply(null,[jsrepl.elems]),clojure.JS.lit_vector([clojure.keyword("","script"),clojure.hash_map(clojure.keyword("","src"),clojure.str.apply(null,["http://localhost:8081/",(clojure.JS.resolveVar("escape",jsrepl).apply(null,[clojure.JS.getOrRun(jsrepl.elems.apply(null,[clojure.keyword("","input")]),"value")])).replace((/\+/),"%2b")]))])])):(null))))})),
clojure.println.apply(null,["ClojureScript"]),
clojure.JS.getOrRun(clojure.keyword("","input").apply(null,[jsrepl.elems]),"focus"))})))}).apply(null,[]);
