clojure = {
  JS: {
    global: this,
    merge: function( t, s ) {
      for( var i in s ) {
        t[ i ] = s[ i ];
      }
      return t;
    }
  },
  lang: {
    Namespace: function( m ) { clojure.JS.merge( this, m || {} ); }
  }
};

clojure = new clojure.lang.Namespace({
  in_ns: function(s) {
    var nsparts = s.substring(1).split('.');
    var base = clojure.JS.global;
    for( var i = 0; i < nsparts.length; ++i ) {
      if( ! base[nsparts[i]] ) {
        base[nsparts[i]] = new clojure.lang.Namespace();
      }
      base = base[nsparts[i]];
    }
  },
  refer: function(s) {},
  seq: function(coll){
    if( coll === null ) return null;
    else if( coll.seq ) return coll.seq();
    //else if( coll.constructor === String )
    //  return clojure.lang.StringSeq.create(coll);
    else if( typeof coll.length == typeof 0 )
      return clojure.lang.ArraySeq.create(coll);
    else if( typeof coll === typeof {} )
      return clojure.JS.ObjSeq.create(coll);
    throw ("Don't know how to create ISeq from: " +
        (typeof coll) + " " + coll.constructor.name);
  },
  apply: function( f ) {
    if( f.isVariatic ) {
      // lazy
      var i, args = [];
      var eagercount = Math.min( f.arity, arguments.length - 2 );
      for( i = 0; i < eagercount; ++i ) {
        args.push( arguments[ i + 1 ] );
      }
      if( eagercount == f.arity ) {
        if( arguments.length - eagercount < 3 ) {
          args.push( clojure.seq( arguments[ arguments.length - 1 ] ) );
        }
        else {
          args.push( clojure.concat(
                  new clojure.lang.ArraySeq(
                      null, arguments, eagercount + 1, arguments.length - 1 ),
                  arguments[ arguments.length - 1 ] ) );
        }
      }
      else {
        var s = clojure.seq( arguments[ arguments.length - 1 ] );
        for( ; s && args.length < f.arity; s = s.rest() ) {
          args.push( s.first() );
        }
        args.push( s );
      }
      return f.apply( clojure.JS.variatic_sentinel, args );
    }
    else {
      // non-lazy
      var args = [];
      for( var i = 1; i < arguments.length - 1; ++i ) {
        args.push( arguments[ i ] );
      }
      for( var s = arguments[ arguments.length - 1]; s; s = s.rest()) {
        args.push( s.first() );
      }
      return f.apply( null, args );
    }
  },
  first: function(x) {
    if( x.first ) return x.first();
    var seq = clojure.seq( x );
    if( seq === null ) return null;
    return seq.first();
  },
  rest: function(x) {
    if( x.rest ) return x.rest();
    var seq = clojure.seq( x );
    if( seq === null ) return null;
    return seq.rest();
  },
  second: function(x) { return clojure.first(clojure.rest(x)); },
  instance_QMARK_: function( c, o ) {
    return clojure.JS.instanceq( c, o );
  },
  find: function(coll, key) {
    if( coll == null )
      return null;
    else if( clojure.JS.instanceq( java.util.Map, coll ) ) {
      if( coll.containsKey( key ) )
        return new clojure.lang.MapEntry( key, coll.get( key ) );
      return null;
    }
    return coll.entryAt( key );
  },
  hash_map: function() {
    // This just makes a seq for now
    var pairs = [];
    for( var i = 0; i < arguments.length; i += 2 ) {
      pairs.push( [ arguments[i], arguments[i + 1] ] )
    }
    return clojure.lang.ArraySeq.create( pairs );
  },
  assoc: function( coll, key, val ) {
    if( coll === null )
      return new clojure.lang.PersistentArrayMap([key, val]);
    return coll.assoc( key, val );
  },
  count: function(x) {
    if( x === null ) return 0;
    if( x.count ) return x.count();
    if( x.length != undefined ) return x.length;
    throw ("count not supported on: " + (typeof x) + " " + x.constructor);
  },
  import_: function() {
    // do nothing
  },
  identical_QMARK_: function( a, b ) {
    return a === b;
  },
  JS: {
    merge: clojure.JS.merge,
    global: clojure.JS.global,
    variatic: function( arity, f ) {
      f.arity = arity;
      f.isVariatic = true;
      return f;
    },
    resolveVar: function( sym, ctxns ) {
      return ctxns[ sym ] || clojure[ sym ] || clojure.JS.global[ sym ];
    },
    def: function( ns, name, init ) {
      var v = new clojure.lang.Var( ns, name );
      ns["_var_" + name] = v;
      v.push( init );
      return v;
    },
    variatic_sentinel: {},
    rest_args: function( varflag, args, i ) {
      if( varflag === clojure.JS.variatic_sentinel )
        return args[ args.length - 1 ];
      return new clojure.lang.ArraySeq( null, args, i );
    },
    lit_list: function( a ) {
      return new clojure.lang.ArraySeq( null, a, 0 );
    },
    implement: function( cls, name, extend, implement ) {
      cls.classname = name;
      cls.classset = {};
      cls.classset[ name ] = true;
      if( implement ) {
        for( var i = 0; i < implement.length; ++i ) {
          if( ! implement[ i ] )
            throw "Can't implement null";
          clojure.JS.merge( cls.classset, implement[ i ].classset );
        }
      }
    },
    definterface: function( pkg, name, implement ) {
      var cls = pkg[ name ] = {};
      clojure.JS.implement( cls, name, implement );
      return cls;
    },
    defclass: function( pkg, name, opts ) {
      var cls = pkg[ name ] = opts.init || function() {};
      clojure.JS.implement( cls, name, opts.extend, opts.implement );
      if( 'extend' in opts ) {
        cls.prototype = new opts.extend;
        cls.prototype.constructor = cls;
        clojure.JS.merge( cls.classset, opts.extend.classset );
      }
      if( opts.statics ) { clojure.JS.merge( cls, opts.statics ); }
      if( opts.methods ) { clojure.JS.merge( cls.prototype, opts.methods ); }
      return cls;
    },
    instanceq: function( c, o ){
      if( o === null || o.getClass === null )
        return false;
      if( o.constructor === c )
        return true;
      return o.constructor.classset[ c ];
    },
    ObjSeq: {
      create: function( obj ) {
        var pairs = [];
        for( var i in obj ) {
          pairs.push( [i, obj[i]] );
        }
        return clojure.lang.ArraySeq.create( pairs );
      }
    }
  },
  lang: {
    Namespace: clojure.lang.Namespace,
    Numbers: {
      isPos: function(x) { return x > 0; },
      inc: function(x) { return x + 1; },
      dec: function(x) { return x - 1; }
    },
    Util: {
      equal: function(x,y) { return x == y; },
      isInteger: function(x) { return typeof x == typeof 0; }
    },
    RT: {
      EMPTY_ARRAY: [],
      conj: function( coll, x ) {
        if( coll === null )
          return new clojure.lang.PersistentList( null, x );
        return coll.cons( x );
      },
      cons: function( x, coll ) {
        var y = clojure.seq( coll );
        if( y === null )
          return new clojure.lang.PersistentList( null, x );
        return y.cons( x );
      },
      seqToArray: function(s) {
        var ret = new Array( clojure.count( s ) );
        for( var i = 0; s !== null; ++i, s = s.rest() )
          ret[ i ] = s.first();
        return ret;
      },
      print: function( o, w ) {
        if( o !== null )
          w.write( "" + o );
      }
    },
    IReduce: {}
  }
});

java = { util: {}, lang: {} };
clojure.JS.definterface( java.util, "Map" );
clojure.JS.definterface( java.util, "Collection" );
clojure.JS.defclass( java.lang, "StringBuilder", {
  init: function( x ) { this.a = [ x ]; },
  methods: {
    append: function( x ) { this.a.push( x ); return this; },
    toString: function() { return this.a.join(''); }
  }
});

clojure.JS.definterface( clojure.lang, "IObj" );

clojure.JS.defclass( clojure.lang, "Obj", {
  implement: [ clojure.lang.IObj ],
  init: function(_meta) { this._meta = _meta; },
  methods: {
    meta: function() { return this._meta; }
  }
});

clojure.JS.defclass( clojure.lang, "ASeq", {
  methods: {
    equals: function( obj ) {
      var ms = obj.seq();
      for( var s = this.seq(); s !== null; s = s.rest(), ms = ms.rest() ) {
        if( ms === null || !clojure.lang.Util.equal( s.first(), ms.first() ))
          return false;
      }
      if( ms !== null )
        return false;
      return true;
    },
    hashCode: function() { throw "not yet implemented"; },
    count: function() {
      var i = 1;
      for( var s = this.rest(); s; s = s.rest() )
        i += 1;
      return i;
    },
    seq: function(){ return this; },
    cons: function(o){ return new clojure.lang.Cons( null, o, this ); },
    toArray: function(){ return clojure.lang.RT.seqToArray( this.seq() ); },
    containsAll: function(c){ throw "not yet implemented"; },
    size: function(){ return this.count(); },
    isEmpty: function(){ return this.count() == 0; },
    contains: function(c){ throw "not yet implemented"; }
  }
});

clojure.JS.defclass( clojure.lang, "Cons", {
  extend: clojure.lang.ASeq,
  init: function( _meta, _first, _rest ) {
    this._meta = _meta;
    this._first = _first;
    this._rest = _rest;
  },
  methods: {
    first: function(){ return this._first; },
    rest: function(){ return this._rest; },
    count: function(){ return 1 + clojure.count( this._rest ); },
    seq: function(){ return this; },
    withMeta: function(_meta){
      return new clojure.lang.Cons( _meta, this._first, this._rest );
    }
  }
});

clojure.JS.defclass( clojure.lang, "ArraySeq", {
  extend: clojure.lang.ASeq,
  init: function( _meta, a, i, len ) {
    this._meta = _meta;
    this.a = a;
    this.i = i;
    this.len = (len === undefined) ? a.length : len;
  },
  statics: {
    create: function( a ) {
      if( a && a.length ) {
        return new clojure.lang.ArraySeq( null, a, 0 );
      }
      else {
        return null;
      }
    }
  },
  methods: {
    first: function() { return this.a[this.i]; },
    rest: function() {
      if( this.i + 1 < this.len )
        return new clojure.lang.ArraySeq(
            this._meta, this.a, this.i + 1, this.len);
      return null;
    },
    count: function() { return this.len - this.i; },
    index: function() { return this.i; },
    withMeta: function( _meta ) {
      return new clojure.lang.ArraySeq( _meta, this.array, this.i, this.len );
    },
    reduce: function( fn, start ) {
      var ret = (start === undefined) ? this.a[0] : fn(start, this.a[0]);
      for( var x = this.i + 1; x < this.len; ++x ) {
        ret = fn( ret, this.a[x] );
      }
      return ret;
    },
    seq: function() { return this; }
  }
});


clojure.JS.defclass( clojure.lang, "LazyCons", {
  init: function(f,_first,_rest) {
    this.f = f;
    this._first = _first === undefined ? clojure.lang.LazyCons.sentinel :_first;
    this._rest  = _rest  === undefined ? clojure.lang.LazyCons.sentinel :_rest;
  },
  statics: {
    sentinel: {}
  },
  methods: {
    first: function() {
      if( this._first === clojure.lang.LazyCons.sentinel )
        this._first = this.f();
      return this._first;
    },
    rest: function() {
      if( this._rest === clojure.lang.LazyCons.sentinel ) {
        if( this._first === clojure.lang.LazyCons.sentinel ) {
          this.first();
        }
        this._rest = clojure.seq( this.f(null) );
        this.f = null;
      }
      return this._rest;
    },
    withMeta: function(_meta) {
      if( _meta == this.meta() )
        return this;
      //force before copying
      this.rest();
      return new clojure.lang.LazyCons( _meta, this._first, this._rest );
    },
    seq: function() { return this; }
  }
});


clojure.JS.defclass( clojure.lang, "Var", {
  init: function( ns, name ) {
    this.ns = ns;
    this.name = name;
    this.stack = [];
  },
  statics: {
    stack: [],
    pushThreadBindings: function( m ) {
      var vars=[], b;
      for( var bs = m.seq(); bs; bs = bs.rest()) {
        b = bs.first();
        vars.push( b[0] );
        b[0].push( b[1] );
      }
      clojure.lang.Var.stack.push( vars );
    },
    popThreadBindings: function() {
      var vars = clojure.lang.Var.stack.pop();
      for( var i = 0; i < vars.length; ++i ) {
        vars[i].pop();
      }
    }
  },
  methods: {
    push: function( val ) {
      this.stack.push( val );
      this.ns[ this.name ] = val;
    },
    pop: function() {
      this.stack.pop();
      this.ns[ this.name ] = this.stack[ this.stack.length - 1 ];
    },
    set: function( val ) {
      this.stack.pop();
      this.push( val );
    },
    hasRoot: function() { return this.stack.length > 0; },
    setMacro: function() { this.macro = true; }
  }
});

clojure.JS.definterface( clojure.lang, "IPersistentCollection" );

clojure.JS.definterface( clojure.lang, "IPersistentStack",
    [clojure.lang.IPersistentCollection] );

clojure.JS.definterface( clojure.lang, "Sequential" );

clojure.JS.definterface( clojure.lang, "Reversible" );

clojure.JS.definterface( clojure.lang, "IPersistentList",
    [clojure.lang.Sequential, clojure.lang.IPersistentStack] );

clojure.JS.defclass( clojure.lang, "EmptyList", {
  extend: clojure.lang.Obj,
  implement: [clojure.lang.IPersistentList, java.util.Collection],
  init: function( _meta ) { this._meta = _meta; },
  methods: {
    cons: function(o) {
      return new clojure.lang.PersistentList( this.meta(), o );
    },
    empty: function() { return this; },
    withMeta: function(m) {
      if( m != this.meta() )
        return new clojure.lang.EmptyList( m );
      return this;
    },
    peek: function() { return null; },
    pop: function() { throw "Can't pop empty list"; },
    count: function() { return 0; },
    seq: function() { return null; },
    size: function() { return 0; },
    isEmpty: function() { return true; },
    contains: function() { return false; },
    toArray: function() { return clojure.lang.RT.EMPTY_ARRAY; },
    containsAll: function( coll ) { return coll.isEmpty(); }
  }
});

clojure.JS.definterface( clojure.lang, "IMapEntry" );

clojure.JS.definterface( clojure.lang, "Associative",
    [ clojure.lang.IPersistentCollection ] );

clojure.JS.definterface( clojure.lang, "IPersistentVector",
    [ clojure.lang.Associative, clojure.lang.Sequential,
      clojure.lang.IPersistentStack, clojure.lang.Reversible ]);

clojure.JS.defclass( clojure.lang, "AMapEntry", {
  implement: [ clojure.lang.IMapEntry, clojure.lang.IPersistentVector ],
  methods: {
    empty: function(){ return null; },
    equals: function(o){
      return clojure.lang.APersistentVector.doEquals(this,o);
    },
    hashCode: function(){ throw "not implemented yet"; },
    toString: function(){
      var sw = new java.io.StringWriter();
      clojure.lang.RT.print( this, sw );
      return sw.toString();
    },
    length: function(){ return 2; },
    nth: function(i){
      switch(i){
        case 0: return this.key();
        case 1: return this.val();
        default: throw "Index out of bounds";
      }
    },
    asVector: function(){
      return clojure.lang.LazilyPersistentVector.createOwning(
          this.key(), this.val() );
    },
    assocN: function(i,v){ return this.asVector().assocN(i,v); },
    count: function(){ return 2; },
    seq: function(){ return this.asVector().seq(); },
    cons: function(o){ return this.asVector().cons(o); },
    containsKey: function(k){ return this.asVector().containsKey(k); },
    entryAt: function(k){ return this.asVector().entryAt(k); },
    assoc: function(k,v){ return this.asVector().assoc(k,v); },
    valAt: function(k,notFound){ return this.asVector().valAt(k,notFound); },
    peek: function(){ return this.val(); },
    pop: function(){
      return clojure.lang.LazilyPersistentVector.createOwning( this.key() );
    },
    rseq: function(){ return this.asVector().rseq(); }
  }
});

clojure.JS.defclass( clojure.lang, "MapEntry", {
  extend: clojure.lang.AMapEntry,
  init: function(k,v){
    this._key = k;
    this._val = v;
  },
  methods: {
    key: function(){ return this._key; },
    val: function(){ return this._val; },
    getKey: function(){ return this._key; },
    getValue: function(){ return this._val; }
  }
});

clojure.JS.defclass( clojure.lang, "PersistentList", {
  init: function( _meta, _first, _rest, _count ) {
    this._meta = _meta || null;
    this._first = _first;
    this._rest = _rest || null;
    this._count = _count || 1;
  },
  statics: {
    creator: function() {
      var real = clojure.lang.PersistentList.creator;
      if( real == arguments.callee ) {
        throw "Not yet implemented: clojure.lang.PersistentList.creator";
      }
      return real.apply( arguments );
    },
    EMPTY: new clojure.lang.EmptyList(null)
  },
  methods: {
    first: function(){ return this._first; },
    rest: function(){
      if( this._count == 1 )
        return null;
      return this._rest;
    },
    peek: function(){ return this.first; },
    pop: function(){
      if( this._rest === null )
        return this.empty();
      return this._rest;
    },
    count: function(){ return this._count; },
    cons: function(o){
      return new clojure.lang.PersistentList(
          this._meta, o, this, this._count + 1 );
    },
    empty: function(){
      return clojure.lang.PersistentList.EMPTY.withMeta( this._meta );
    },
    withMeta: function( _meta ){
      if( _meta != this._meta )
        return new clojure.lang.PersistentList(
            this._meta, this._first, this._rest, this._count );
      return this;
    },
    reduce: function( f, start ){
      var ret = (start === undefined) ? this.first() : f( start, this.first() );
      for( var s = this.rest(); s !== null; s = s.rest() )
        ret = f( ret, s.first() );
      return ret;
    }
  }
});

clojure.JS.defclass( clojure.lang, "APersistentVector", {
  init: function( _meta ) { this._meta = _meta; },
  methods: {
    meta: function() { return this._meta; },
    peek: function() {
      if( this.count() > 0 )
        return this.nth( this.count() - 1 );
      return null;
    },
    seq: function() {
      if( this.count() > 0 )
        return new clojure.lang.APersistentVector.Seq( null, this, 0 );
      return null;
    },
    rseq: function() {
      if( this.count() > 0 )
        return new clojure.lang.APersistentVector.RSeq( this, this.count() - 1);
      return null;
    },
    equals: function() { throw "not implemented yet"; },
    hashCode: function() { throw "not implemented yet"; },
    get: function(i) { return this.nth(i); },
    indexOf: function( o ){
      var len = this.count();
      for( var i = 0; i < len; ++i )
        if( clojure.lang.Util.equal( this.nth( i ), o ) )
          return i;
      return -1;
    },
    lastIndexOf: function( o ){
      for( var i = this.count() - 1; i >= 0; --i )
        if( clojure.lang.Util.equal( this.nth( i ), o ) )
          return i;
      return -1;
    },
    subList: function( fromi, toi ) {
      return clojure.lang.RT.subvec( this, fromi, toi );
    },
    invoke: function( i ) {
      if( clojure.lang.Util.isInteger(i) )
        return this.nth( parseInt( i ) );
      throw "Key must be integer";
    },
    peek: function() {
      if( this.count() > 0 )
        return this.nth( this.count() - 1 );
      return null
    },
    constainsKey: function(k){
      if( ! clojure.lang.Util.isInteger( k ) )
        return false;
      var i = parseInt(k);
      return i >= 0 && i < this.count();
    },
    entryAt: function(k){
      if( clojure.lang.Util.isInteger( k ) ) {
        var i = parseInt(k);
        if( i >= 0 && i < this.count() )
          return new clojure.lang.MapEntry( k, this.nth(i) );
      }
      return null;
    },
    assoc: function(k,v){
      if( clojure.lang.Util.isInteger( k ) ) {
        var i = parseInt(k);
        return this.assocN(i,v);
      }
      throw "Key must be integer";
    },
    valAt: function(k, notFound){
      if( clojure.lang.Util.isInteger( k ) ) {
        var i = parseInt(k);
        if( i >= 0 && i < this.count() )
          return this.nth(i);
      }
      return notFound;
    },
    toArray: function(){ return clojure.lang.RT.setToArray( this.seq() ); },
    containsAll: function(){ throw "not implemented yet"; },
    size: function(){ return this.count(); },
    isEmpty: function(){ return this.count() === 0; },
    contains: function(o){
      for( var s = this.seq(); s !== null; s = s.rest() ) {
        if( clojure.lang.Util.equal( s.first(), o ) )
          return true;
      }
      return false;
    },
    length: function(){ return this.count(); },
    compareTo: function(v){
      var c, len = this.count();
      if( len < v.count() )
        return -1;
      else if( len > v.count() )
        return 1;
      for( var i = 0; i < len; ++i ) {
        c = this.nth(i).compareTo( v.nth(i) );
        if( c != 0 )
          return c;
      }
      return 0;
    }
  }
});

clojure.JS.defclass( clojure.lang.APersistentVector, "Seq", {
  init: function( _meta, v, i){
    this._meta = _meta;
    this.v = v;
    this.i = i;
  },
  methods: {
    first: function(){ return this.v.nth(this.i); },
    rest: function(){
      if( this.i + 1 < this.v.count() )
        return new clojure.lang.APersistentVector.Seq(
            this._meta, this.v, this.i + 1 );
      return null;
    },
    index: function(){ return this.i; },
    count: function(){ return this.v.count() - this.i; },
    withMeta: function(_meta){
      return new clojure.lang.APersistentVector.Seq( _meta, this.v, this.i );
    },
    reduce: function( fn, start ) {
      var ret = (start === undefined) ?
                this.v.nth(this.i) : fn(start,this.v.nth(this.i));
      for( var x = this.i + 1; x < this.count(); ++x ) {
        ret = fn( ret, this.v.nth(x) );
      }
      return ret;
    }
  }
});

clojure.JS.defclass( clojure.lang.APersistentVector, "RSeq", {
  init: function( _meta, v, i){
    this._meta = _meta;
    this.v = v;
    this.i = i;
  },
  methods: {
    first: function(){ return this.v.nth(this.i); },
    rest: function(){
      if( this.i > 0 )
        return new clojure.lang.APersistentVector.RSeq( this.v, this.i - 1 );
      return null;
    },
    index: function(){ return this.i; },
    count: function(){ return this.i + 1; },
    withMeta: function(_meta){
      return new clojure.lang.APersistentVector.RSeq( _meta, this.v, this.i );
    }
  }
});

clojure.JS.defclass( clojure.lang, "PersistentVector", {
  extend: clojure.lang.APersistentVector,
  init: function( _meta, cnt, shift, root, tail ) {
    clojure.lang.APersistentVector.call( this, _meta );
    this.cnt = cnt;
    this.shift = shift;
    this.root = root;
    this.tail = tail;
  },
  statics: {
    create: function( items ) {
      var ret = clojure.lang.PersistentVector.EMPTY;
      for( var i = 0; i < items.length; ++i ) {
        ret = ret.cons( items[ i ] );
      }
      return ret;
    }
  },
  methods: {
    tailoff: function() { return this.cnt - this.tail.length; },
    nth: function( i ) {
      if( i >= 0 && i < this.cnt ) {
        if( i >= this.tailoff() ) {
          return this.tail[ i & 0x01f ];
        }
        var arr = this.root;
        for( var level = this.shift; level > 0; level -= 5 ) {
          arr = arr[ (i >>> level) & 0x01f ];
        }
        return arr[ i & 0x01f ];
      }
      throw "IndexOutOfBoundsException";
    },
    assocN: function( i, val ) {
      if( i >= 0 && i < this.cnt ) {
        if( i >= this.tailoff() ) {
          var newTail = this.tail.slice( 0 );
          newTail[ i & 0x01f ] = val;
          return new clojure.lang.PersistentVector(
              this.meta(), this.cnt, this.shift, this.root, newTail );
        }
        return new clojure.lang.PersistentVector(
            this.meta(), this.cnt, this.shift,
            this.doAssoc( this.shift, this.root, i, val), this.tail );
      }
      if( i == this.cnt ) {
        return this.cons( val );
      }
      throw "IndexOutOfBoundsException";
    },
    doAssoc: function( level, arr, i, val ) {
      var ret = arr.slice( 0 );
      if( level == 0 ) {
        ret[ i & 0x01f ] = val;
      }
      else {
        var subidx = (i >>> level) & 0x01f;
        ret[ subidx ] = this.doAssoc( level - 5, arr[ subidx ], i, val );
      }
      return ret;
    },
    count: function() { return this.cnt; },
    withMeta: function( _meta ) {
      return new clojure.lang.PersistentVector(
          _meta, this.cnt, this.shift, this.root, this.tail );
    },
    cons: function( val ) {
      if( this.tail.length < 32 ) {
        var newTail = this.tail.concat( [val] );
        return new clojure.lang.PersistentVector(
            this.meta(), this.cnt + 1, this.shift, this.root, newTail );
      }
      var expansion = [null];
      var newroot = this.pushTail(
          this.shift - 5, this.root, this.tail, expansion);
      var newshift = this.shift;
      if( expansion[0] != null ) {
        newroot = [newroot, expansion[0]];
        newshift += 5;
      }
      return new clojure.lang.PersistentVector(
          this.meta(), this.cnt+1, newshift, newroot, [val] );
    },
    empty: function() {
      return clojure.lang.PersistentVector.EMPTY.withMeta( this.meta() );
    },
    pushTail: function( level, arr, tailNode, expansion)
    {
      var newchild;
      if( level == 0 ) {
        newchild = tailNode;
      }
      else {
        newchild = this.pushTail(
            level - 5, arr[arr.length - 1], tailNode, expansion);
        if( expansion[0] == null ) {
          var ret = arr.slice( 0 );
          ret[ arr.length - 1 ] = newchild;
          return ret;
        }
        else {
          newchild = expansion[0];
        }
      }
      //expansion
      if( arr.length == 32 ) {
        expansion[0] = [newchild];
        return arr;
      }
      expansion[0] = null;
      return arr.concat([newchild]);
    },
    pop: function() {
      if( this.cnt == 0 ) {
        throw "IllegalStateException: Can't pop empty vector";
      }
      if( this.cnt == 1 ) {
        return clojure.lang.PersistentVector.EMPTY.withMeta( this.meta() );
      }
      if( this.tail.length > 1 ) {
        var newTail = this.tail.slice( 0, this.tail.length - 1 );
        return new clojure.lang.PersistentVector(
            this.meta(), this.cnt - 1, this.shift, this.root, newTail );
      }
      var ptail = [null];
      var newroot = this.popTail( this.shift - 5, this.root, ptail );
      var newshift = this.shift;
      if( newroot == null ) {
        newroot = clojure.lang.RT.EMPTY_ARRAY;
      }
      if( this.shift > 5 && newroot.length == 1 ) {
        newroot = newroot[0];
        newshift -= 5;
      }
      return new clojure.lang.PersistentVector(
          this.meta(), this.cnt - 1, newshift, newroot, ptail[0] );
    },
    popTail: function( shift, arr, ptail ) {
      if( shift > 0 ) {
        var newchild = this.popTail( shift - 5, arr[ arr.length - 1 ], ptail );
        if( newchild != null ) {
          var ret = arr.slice( 0 );
          ret[ arr.length - 1 ] = newchild;
          return ret;
        }
      }
      if( shift == 0 ) {
        ptail[0] = arr[ arr.length - 1 ];
      }
      //contraction
      if( arr.length == 1 ) {
        return null;
      }
      return arr.slice( 0, arr.length - 1 );
    }
  }
});

clojure.lang.PersistentVector.EMPTY =
  new clojure.lang.PersistentVector(
      {}, 0, 5, clojure.lang.RT.EMPTY_ARRAY, clojure.lang.RT.EMPTY_ARRAY );

clojure.lang.Namespace.find = function( s ) {
  return clojure.JS.global[ s.substring(1) ];
};

clojure.lang.Namespace.prototype.getMappings = function() {
  return this;
};

(function() {
  var buf = [];
  function write(s) {
    var parts = s.split(/\n/);
    if( parts.length == 1 ) {
      buf.push(s);
    }
    else {
      print( buf.join('') + parts.splice(0, parts.length - 1).join('\n') );
      buf = [ parts[parts.length - 1] ];
    }
  }
  clojure._STAR_out_STAR_ = { append: write, write: write };
})();
