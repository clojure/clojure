package clojure.lang;

import java.util.IdentityHashMap;

public class CompilerClean {

  static final Symbol DEF = Symbol.intern("def");
  static final Symbol LOOP = Symbol.intern("loop*");
  static final Symbol RECUR = Symbol.intern("recur");
  static final Symbol IF = Symbol.intern("if");
  static final Symbol LET = Symbol.intern("let*");
  static final Symbol LETFN = Symbol.intern("letfn*");
  static final Symbol DO = Symbol.intern("do");
  static final Symbol FN = Symbol.intern("fn*");
  static final Symbol FNONCE = (Symbol) Symbol.intern("fn*").withMeta(
      RT.map(Keyword.intern(null, "once"), RT.T));
  static final Symbol QUOTE = Symbol.intern("quote");
  static final Symbol THE_VAR = Symbol.intern("var");
  static final Symbol DOT = Symbol.intern(".");
  static final Symbol ASSIGN = Symbol.intern("set!");
  // static final Symbol TRY_FINALLY = Symbol.intern("try-finally");
  static final Symbol TRY = Symbol.intern("try");
  static final Symbol CATCH = Symbol.intern("catch");
  static final Symbol FINALLY = Symbol.intern("finally");
  static final Symbol THROW = Symbol.intern("throw");
  static final Symbol MONITOR_ENTER = Symbol.intern("monitor-enter");
  static final Symbol MONITOR_EXIT = Symbol.intern("monitor-exit");
  static final Symbol IMPORT = Symbol.intern("clojure.core", "import*");
  // static final Symbol INSTANCE = Symbol.intern("instance?");
  static final Symbol DEFTYPE = Symbol.intern("deftype*");
  static final Symbol CASE = Symbol.intern("case*");

  // static final Symbol THISFN = Symbol.intern("thisfn");
  static final Symbol CLASS = Symbol.intern("Class");
  static final Symbol NEW = Symbol.intern("new");
  static final Symbol THIS = Symbol.intern("this");
  static final Symbol REIFY = Symbol.intern("reify*");
  // static final Symbol UNQUOTE = Symbol.intern("unquote");
  // static final Symbol UNQUOTE_SPLICING = Symbol.intern("unquote-splicing");
  // static final Symbol SYNTAX_QUOTE = Symbol.intern("clojure.core",
  // "syntax-quote");
  static final Symbol LIST = Symbol.intern("clojure.core", "list");
  static final Symbol HASHMAP = Symbol.intern("clojure.core", "hash-map");
  static final Symbol VECTOR = Symbol.intern("clojure.core", "vector");
  static final Symbol IDENTITY = Symbol.intern("clojure.core", "identity");

  static final Symbol _AMP_ = Symbol.intern("&");
  static final Symbol ISEQ = Symbol.intern("clojure.lang.ISeq");

  static final Keyword inlineKey = Keyword.intern(null, "inline");
  static final Keyword inlineAritiesKey = Keyword
      .intern(null, "inline-arities");
  static final Keyword staticKey = Keyword.intern(null, "static");
  static final Keyword arglistsKey = Keyword.intern(null, "arglists");
  static final Symbol INVOKE_STATIC = Symbol.intern("invokeStatic");

  static final Keyword volatileKey = Keyword.intern(null, "volatile");
  static final Keyword implementsKey = Keyword.intern(null, "implements");
  static final String COMPILE_STUB_PREFIX = "compile__stub";

  static final Keyword protocolKey = Keyword.intern(null, "protocol");
  static final Keyword onKey = Keyword.intern(null, "on");
  static Keyword dynamicKey = Keyword.intern("dynamic");

  static final Symbol NS = Symbol.intern("ns");
  static final Symbol IN_NS = Symbol.intern("in-ns");

  // static final Symbol IMPORT = Symbol.intern("import");
  // static final Symbol USE = Symbol.intern("use");

  // static final Symbol IFN = Symbol.intern("clojure.lang", "IFn");

  // Integer
  static final public Var LINE_BEFORE = Var.create(0).setDynamic();
  static final public Var COLUMN_BEFORE = Var.create(0).setDynamic();
  static final public Var LINE_AFTER = Var.create(0).setDynamic();
  static final public Var COLUMN_AFTER = Var.create(0).setDynamic();

  // Integer
  static final public Var NEXT_LOCAL_NUM = Var.create(0).setDynamic();

  // Integer
  static final public Var RET_LOCAL_NUM = Var.create().setDynamic();

  static final public Var COMPILE_STUB_SYM = Var.create(null).setDynamic();
  static final public Var COMPILE_STUB_CLASS = Var.create(null).setDynamic();

  //symbol->localbinding
  static final public Var LOCAL_ENV = Var.create(null).setDynamic();

  //vector<localbinding>
  static final public Var LOOP_LOCALS = Var.create().setDynamic();

  //Label
  static final public Var LOOP_LABEL = Var.create().setDynamic();

  //vector<object>
  static final public Var CONSTANTS = Var.create().setDynamic();

  //IdentityHashMap
  static final public Var CONSTANT_IDS = Var.create().setDynamic();

  //vector<keyword>
  static final public Var KEYWORD_CALLSITES = Var.create().setDynamic();

  //vector<var>
  static final public Var PROTOCOL_CALLSITES = Var.create().setDynamic();

  //set<var>
  static final public Var VAR_CALLSITES = Var.create().setDynamic();

  //keyword->constid
  static final public Var KEYWORDS = Var.create().setDynamic();

  //var->constid
  static final public Var VARS = Var.create().setDynamic();

  //FnFrame
  static final public Var METHOD = Var.create(null).setDynamic();

  //null or not
  static final public Var IN_CATCH_FINALLY = Var.create(null).setDynamic();

  static final public Var NO_RECUR = Var.create(null).setDynamic();

  //DynamicClassLoader
  static final public Var LOADER = Var.create().setDynamic();

  //String
  static final public Var SOURCE = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                              Symbol.intern("*source-path*"), "NO_SOURCE_FILE").setDynamic();

  //String
  static final public Var SOURCE_PATH = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                                   Symbol.intern("*file*"), "NO_SOURCE_PATH").setDynamic();

  //String
  static final public Var COMPILE_PATH = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                                    Symbol.intern("*compile-path*"), null).setDynamic();
  //boolean
  static final public Var COMPILE_FILES = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                                     Symbol.intern("*compile-files*"), Boolean.FALSE).setDynamic();

  static final public Var INSTANCE = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                              Symbol.intern("instance?"));

  static final public Var ADD_ANNOTATIONS = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                              Symbol.intern("add-annotations"));

  static final public Keyword disableLocalsClearingKey = Keyword.intern("disable-locals-clearing");
  static final public Keyword elideMetaKey = Keyword.intern("elide-meta");

  static final public Var COMPILER_OPTIONS = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                                        Symbol.intern("*compiler-options*"), null).setDynamic();

  static public Object getCompilerOption(Keyword k){
    return RT.get(COMPILER_OPTIONS.deref(),k);
  }

  
  static Object resolve(Symbol sym, boolean allowPrivate) {
    return resolveIn(currentNS(), sym, allowPrivate);
  }

  static Object resolve(Symbol sym) {
    return resolveIn(currentNS(), sym, false);
  }

  static Namespace namespaceFor(Symbol sym) {
    return namespaceFor(currentNS(), sym);
  }

  static Namespace namespaceFor(Namespace inns, Symbol sym) {
    // note, presumes non-nil sym.ns
    // first check against currentNS' aliases...
    Symbol nsSym = Symbol.intern(sym.ns);
    Namespace ns = inns.lookupAlias(nsSym);
    if (ns == null) {
      // ...otherwise check the Namespaces map.
      ns = Namespace.find(nsSym);
    }
    return ns;
  }

  static public Object resolveIn(Namespace n, Symbol sym, boolean allowPrivate) {
    // note - ns-qualified vars must already exist
    if (sym.ns != null) {
      Namespace ns = namespaceFor(n, sym);
      if (ns == null)
        throw Util.runtimeException("No such namespace: " + sym.ns);

      Var v = ns.findInternedVar(Symbol.intern(sym.name));
      if (v == null)
        throw Util.runtimeException("No such var: " + sym);
      else if (v.ns != currentNS() && !v.isPublic() && !allowPrivate)
        throw new IllegalStateException("var: " + sym + " is not public");
      return v;
    } else if (sym.name.indexOf('.') > 0 || sym.name.charAt(0) == '[') {
      return RT.classForName(sym.name);
    } else if (sym.equals(NS))
      return RT.NS_VAR;
    else if (sym.equals(IN_NS))
      return RT.IN_NS_VAR;
    else {
      if (Util.equals(sym, COMPILE_STUB_SYM.get()))
        return COMPILE_STUB_CLASS.get();
      Object o = n.getMapping(sym);
      if (o == null) {
        if (RT.booleanCast(RT.ALLOW_UNRESOLVED_VARS.deref())) {
          return sym;
        } else {
          throw Util.runtimeException("Unable to resolve symbol: " + sym
              + " in this context");
        }
      }
      return o;
    }
  }

  static public Object maybeResolveIn(Namespace n, Symbol sym) {
    // note - ns-qualified vars must already exist
    if (sym.ns != null) {
      Namespace ns = namespaceFor(n, sym);
      if (ns == null)
        return null;
      Var v = ns.findInternedVar(Symbol.intern(sym.name));
      if (v == null)
        return null;
      return v;
    } else if (sym.name.indexOf('.') > 0 && !sym.name.endsWith(".")
        || sym.name.charAt(0) == '[') {
      return RT.classForName(sym.name);
    } else if (sym.equals(NS))
      return RT.NS_VAR;
    else if (sym.equals(IN_NS))
      return RT.IN_NS_VAR;
    else {
      Object o = n.getMapping(sym);
      return o;
    }
  }

  static Var lookupVar(Symbol sym, boolean internNew, boolean registerMacro) {
    Var var = null;

    // note - ns-qualified vars in other namespaces must already exist
    if (sym.ns != null) {
      Namespace ns = namespaceFor(sym);
      if (ns == null)
        return null;
      // throw Util.runtimeException("No such namespace: " + sym.ns);
      Symbol name = Symbol.intern(sym.name);
      if (internNew && ns == currentNS())
        var = currentNS().intern(name);
      else
        var = ns.findInternedVar(name);
    } else if (sym.equals(NS))
      var = RT.NS_VAR;
    else if (sym.equals(IN_NS))
      var = RT.IN_NS_VAR;
    else {
      // is it mapped?
      Object o = currentNS().getMapping(sym);
      if (o == null) {
        // introduce a new var in the current ns
        if (internNew)
          var = currentNS().intern(Symbol.intern(sym.name));
      } else if (o instanceof Var) {
        var = (Var) o;
      } else {
        throw Util.runtimeException("Expecting var, but " + sym
            + " is mapped to " + o);
      }
    }
    if (var != null && (!var.isMacro() || registerMacro))
      registerVar(var);
    return var;
  }

  static Var lookupVar(Symbol sym, boolean internNew) {
    return lookupVar(sym, internNew, true);
  }

  private static int registerConstant(Object o){
    if(!CONSTANTS.isBound())
      return -1;
    PersistentVector v = (PersistentVector) CONSTANTS.deref();
    IdentityHashMap<Object,Integer> ids = (IdentityHashMap<Object,Integer>) CONSTANT_IDS.deref();
    Integer i = ids.get(o);
    if(i != null)
      return i;
    CONSTANTS.set(RT.conj(v, o));
    ids.put(o, v.count());
    return v.count();
  }
  
  private static void registerVar(Var var) {
    if (!VARS.isBound())
      return;
    IPersistentMap varsMap = (IPersistentMap) VARS.deref();
    Object id = RT.get(varsMap, var);
    if (id == null) {
      VARS.set(RT.assoc(varsMap, var, registerConstant(var)));
    }
    // if(varsMap != null && RT.get(varsMap, var) == null)
    // VARS.set(RT.assoc(varsMap, var, var));
  }

  static Namespace currentNS() {
    return (Namespace) RT.CURRENT_NS.deref();
  }

  static final public IPersistentSet specials = PersistentHashSet.create(DEF,
      LOOP, RECUR, IF, CASE, LET, LETFN, DO, FN, QUOTE, THE_VAR, IMPORT, DOT,
      ASSIGN, DEFTYPE, REIFY, TRY, THROW, MONITOR_ENTER, MONITOR_EXIT, CATCH,
      FINALLY, NEW, _AMP_);
  
  static boolean isSpecial(Object sym){
    return specials.contains(sym);
  }
  
  static Symbol resolveSymbol(Symbol sym) {
    // already qualified or classname?
    if (sym.name.indexOf('.') > 0)
      return sym;
    if (sym.ns != null) {
      Namespace ns = namespaceFor(sym);
      if (ns == null || ns.name.name == sym.ns)
        return sym;
      return Symbol.intern(ns.name.name, sym.name);
    }
    Object o = currentNS().getMapping(sym);
    if (o == null)
      return Symbol.intern(currentNS().name.name, sym.name);
    else if (o instanceof Class)
      return Symbol.intern(null, ((Class) o).getName());
    else if (o instanceof Var) {
      Var v = (Var) o;
      return Symbol.intern(v.ns.name.name, v.sym.name);
    }
    return null;

  }
  
  public static boolean namesStaticMember(Symbol sym) {
    return sym.ns != null && namespaceFor(sym) == null;
  }
  
  public static void pushNSandLoader(ClassLoader loader) {
    Var.pushThreadBindings(RT.map(
        Var.intern(Symbol.intern("clojure.core"), Symbol.intern("*ns*"))
            .setDynamic(), null, RT.FN_LOADER_VAR, loader, RT.READEVAL, RT.T));
  }
}
