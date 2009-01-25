(function(){
return (clojure.core.in_ns.apply(null,[clojure.core.symbol("jsrepl")]),
clojure.core.refer.apply(null,[clojure.core.symbol("clojure.core")]))}).apply(null,[]);
(function(){
return (clojure.JS.def(jsrepl,"append_dom",null))}).apply(null,[]);
(function(){
return (clojure.JS.def(jsrepl,"dom",(function(o_1){
var elem_6,v_10,sq__3775__auto___7,k_9,vec__534_8,attrs_4,vec__533_2,body_5,tag_3;
return (((clojure.core.coll_QMARK_.apply(null,[o_1]))?(((vec__533_2=o_1),
(tag_3=clojure.core.nth.apply(null,[vec__533_2,(0),null])),
(attrs_4=clojure.core.nth.apply(null,[vec__533_2,(1),null])),
(body_5=clojure.core.nthrest.apply(null,[vec__533_2,(2)])),
((clojure.core.keyword_QMARK_.apply(null,[tag_3]))?(((elem_6=(clojure.JS.resolveVar("document",jsrepl)).createElement(clojure.core.name.apply(null,[tag_3]))),
((clojure.core.map_QMARK_.apply(null,[attrs_4]))?(((function(){var _rtn,_cnt;(sq__3775__auto___7=clojure.core.seq.apply(null,[attrs_4]));do{_cnt=0;
_rtn=((sq__3775__auto___7)?(((vec__534_8=clojure.core.first.apply(null,[sq__3775__auto___7])),
(k_9=clojure.core.nth.apply(null,[vec__534_8,(0),null])),
(v_10=clojure.core.nth.apply(null,[vec__534_8,(1),null])),
((true)?(((true)?(((v_10)?((elem_6).setAttribute(clojure.core.name.apply(null,[k_9]),v_10)):(null))):(null)),
(_cnt=1,_rtn=[clojure.core.rest.apply(null,[sq__3775__auto___7])],sq__3775__auto___7=_rtn[0])):(null)))):(null))}while(_cnt);return _rtn;})())):(null)),
clojure.JS.lit_vector([jsrepl.append_dom.apply(null,[elem_6,((clojure.core.map_QMARK_.apply(null,[attrs_4]))?(body_5):(clojure.core.cons.apply(null,[attrs_4,body_5])))])]))):(clojure.core.mapcat.apply(null,[jsrepl.dom,o_1]))))):(((o_1)?(clojure.JS.lit_vector([(clojure.JS.resolveVar("document",jsrepl)).createTextNode(clojure.core.str.apply(null,[o_1]))])):(null)))))})))}).apply(null,[]);
(function(){
return (clojure.JS.def(jsrepl,"append_dom",(function(parent_1,v_2){
var sq__3775__auto___3,i_4;
return (((function(){var _rtn,_cnt;(sq__3775__auto___3=clojure.core.seq.apply(null,[jsrepl.dom.apply(null,[v_2])]));do{_cnt=0;
_rtn=((sq__3775__auto___3)?(((i_4=clojure.core.first.apply(null,[sq__3775__auto___3])),
((true)?(((true)?((parent_1).appendChild(i_4)):(null)),
(_cnt=1,_rtn=[clojure.core.rest.apply(null,[sq__3775__auto___3])],sq__3775__auto___3=_rtn[0])):(null)))):(null))}while(_cnt);return _rtn;})()),
parent_1)})))}).apply(null,[]);
(function(){
return (clojure.JS.def(jsrepl,"_STAR_print_class_STAR_",null))}).apply(null,[]);
(function(){
return (clojure.JS.def(jsrepl,"repl_print",(function(log_1,text_2){
var sq__3775__auto___3,line_4;
return (((function(){var _rtn,_cnt;(sq__3775__auto___3=clojure.core.seq.apply(null,[(text_2).split((/\n/))]));do{_cnt=0;
_rtn=((sq__3775__auto___3)?(((line_4=clojure.core.first.apply(null,[sq__3775__auto___3])),
((true)?(((true)?(jsrepl.append_dom.apply(null,[log_1,clojure.JS.lit_vector([clojure.core.keyword("","div"),clojure.core.hash_map(clojure.core.keyword("","class"),clojure.core.str.apply(null,["cg ",((jsrepl._STAR_print_class_STAR_)?(clojure.core.str.apply(null,[" ",jsrepl._STAR_print_class_STAR_])):(null))])),line_4])])):(null)),
(_cnt=1,_rtn=[clojure.core.rest.apply(null,[sq__3775__auto___3])],sq__3775__auto___3=_rtn[0])):(null)))):(null))}while(_cnt);return _rtn;})()),
(log_1.scrollTop=clojure.JS.getOrRun(log_1,"scrollHeight")))})))}).apply(null,[]);
(function(){
return (clojure.JS.def(jsrepl,"postexpr",(function(log_1,input_2){
return (jsrepl.append_dom.apply(null,[log_1,clojure.JS.lit_vector([clojure.core.keyword("","table"),clojure.JS.lit_vector([clojure.core.keyword("","tbody"),clojure.JS.lit_vector([clojure.core.keyword("","tr"),clojure.JS.lit_vector([clojure.core.keyword("","td"),clojure.core.hash_map(clojure.core.keyword("","class"),"cg"),"user=> "]),clojure.JS.lit_vector([clojure.core.keyword("","td"),(clojure.JS.getOrRun(input_2,"value")).replace((/\n$/),"")])])])])]))})))}).apply(null,[]);
(function(){
return (clojure.core._var__STAR_print_length_STAR_.set((103)))}).apply(null,[]);
(function(){
return ((clojure.JS.resolveVar("window",jsrepl).onload=(function(){
var log_1,input_2,status_3,applet_4;
return (((log_1=(clojure.JS.resolveVar("document",jsrepl)).getElementById("log")),
(input_2=(clojure.JS.resolveVar("document",jsrepl)).getElementById("input")),
(status_3=(clojure.JS.resolveVar("document",jsrepl)).getElementById("status")),
(applet_4=(clojure.JS.resolveVar("document",jsrepl)).getElementById("applet")),
(clojure.JS.resolveVar("window",jsrepl).print=(function(p1__586_1){
return (jsrepl.repl_print.apply(null,[log_1,p1__586_1]))})),
(input_2.onkeypress=(function(ev_1){
var or__3114__auto___2,vec__596_2,status_name_3,text_4;
return (((clojure.lang.Numbers.equiv(clojure.JS.getOrRun(((or__3114__auto___2=ev_1),
((or__3114__auto___2)?(or__3114__auto___2):(clojure.JS.resolveVar("event",jsrepl)))),"keyCode"),(13)))?(((vec__596_2=(applet_4).tojs(clojure.JS.getOrRun(input_2,"value"))),
(status_name_3=clojure.core.nth.apply(null,[vec__596_2,(0),null])),
(text_4=clojure.core.nth.apply(null,[vec__596_2,(1),null])),
((clojure.lang.Util.equiv(status_name_3,"incomplete"))?((status_3.src="dots.png")):(jsrepl.postexpr.apply(null,[log_1,input_2]),
((clojure.lang.Util.equiv(status_name_3,"js"))?((function(){
var e_3;
return ((function(){try{var _rtn=(clojure.core.prn.apply(null,[(clojure.JS.resolveVar("window",jsrepl)).eval(text_4)]))}
catch(e_3){_rtn=clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[jsrepl._var__STAR_print_class_STAR_,"err"])),
(function(){
return ((function(){try{var _rtn=(clojure.core.println.apply(null,[e_3]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())}).apply(null,[]),
clojure.core._var__STAR_e.set(e_3)}return _rtn})())}).apply(null,[])):(clojure.lang.Var.pushThreadBindings(clojure.core.hash_map.apply(null,[jsrepl._var__STAR_print_class_STAR_,"err"])),
(function(){
return ((function(){try{var _rtn=(clojure.core.println.apply(null,[text_4]))}
finally{clojure.lang.Var.popThreadBindings()}return _rtn})())}).apply(null,[]))),
clojure.JS.resolveVar("setTimeout",jsrepl).apply(null,[(function(){
return ((input_2.value=""))}),(0)]),
(status_3.src="blank.gif"))))):(null)))})),
clojure.core.println.apply(null,["ClojureScript"]),
clojure.JS.getOrRun(input_2,"focus")))})))}).apply(null,[]);
