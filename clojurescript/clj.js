function clojure_merge( t, s ) {
  for( var i in s ) {
    t[ i ] = s[ i ];
  }
  return t;
};

function clojure_Namespace( m ) {
  clojure_merge( this, m || {} );
};

clojure_global = this;

clojure = new clojure_Namespace({
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
    return o !== null && o.constructor == c;
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
    merge: clojure_merge,
    global: clojure_global,
    variatic: function( f ) {
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
    defclass: function( pkg, classname, opts ) {
      var cls = pkg[ classname ] = opts.init || function() {};
      cls.classname = classname;
      if( opts.extend ) { cls.prototype = new opts.extend[0]; }
      cls.constructor = cls;
      if( opts.statics ) { clojure.JS.merge( cls, opts.statics ); }
      if( opts.methods ) { clojure.JS.merge( cls.prototype, opts.methods ); }
      return cls;
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
    Numbers: {
      isPos: function(x) { return x > 0; },
      inc: function(x) { return x + 1; },
      dec: function(x) { return x - 1; }
    },
    Util: {
      equal: function(x,y) { return x == y; }
    },
    RT: {
      conj: function( coll, x ) {
        var y = clojure.seq( coll );
        if( y === null )
          return new clojure.lang.PersistentList( null, x );
        return y.cons( x );
      },
      print: function( o, w ) {
        if( o !== null )
          w.write( "" + o );
      }
    },
    IReduce: {}
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
  extend: [clojure.lang.ASeq],
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
  extend: [clojure.lang.ASeq],
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


clojure.lang.LazyCons = function(f,_first,_rest) {
  this.f = f;
  this._first = _first === undefined ? clojure.lang.LazyCons.sentinel : _first;
  this._rest = _rest === undefined ? clojure.lang.LazyCons.sentinel : _rest;
};

clojure.lang.LazyCons.sentinel = {};

clojure.lang.LazyCons.prototype.first = function() {
  if( this._first === clojure.lang.LazyCons.sentinel )
    this._first = this.f();
  return this._first;
};

clojure.lang.LazyCons.prototype.rest = function() {
  if( this._rest === clojure.lang.LazyCons.sentinel ) {
    if( this._first === clojure.lang.LazyCons.sentinel ) {
      this.first();
    }
    this._rest = clojure.seq( this.f(null) );
    this.f = null;
  }
  return this._rest;
};

clojure.lang.LazyCons.prototype.withMeta = function(_meta) {
  if( _meta == this.meta() )
    return this;
  //force before copying
  this.rest();
  return new clojure.lang.LazyCons( _meta, this._first, this._rest );
};

clojure.lang.LazyCons.prototype.seq = function() {
  return this;
};

clojure.lang.Var = function( ns, name ) {
  this.ns = ns;
  this.name = name;
  this.stack = [];
};

clojure.lang.Var.prototype.push = function( val ) {
  this.stack.push( val );
  this.ns[ this.name ] = val;
};

clojure.lang.Var.prototype.pop = function() {
  this.stack.pop();
  this.ns[ this.name ] = this.stack[ this.stack.length - 1 ];
};

clojure.lang.Var.prototype.set = function( val ) {
  this.stack.pop();
  this.push( val );
};

clojure.lang.Var.prototype.hasRoot = function() {
  return this.stack.length > 0;
};

clojure.lang.Var.prototype.setMacro = function() {
  this.macro = true;
};

clojure.lang.Var.stack = [];

clojure.lang.Var.pushThreadBindings = function( m ) {
  var vars=[], b;
  for( var bs = m.seq(); bs; bs = bs.rest()) {
    b = bs.first();
    vars.push( b[0] );
    b[0].push( b[1] );
  }
  clojure.lang.Var.stack.push( vars );
};

clojure.lang.Var.popThreadBindings = function() {
  var vars = clojure.lang.Var.stack.pop();
  for( var i = 0; i < vars.length; ++i ) {
    vars[i].pop();
  }
};



clojure.JS.defclass( clojure.lang, "EmptyList", {
  //extend: [clojure.lang.IPersistentList],
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


clojure.lang.Namespace = clojure_Namespace;

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

java = { lang: {} };
java.lang.StringBuilder = function( x ) {
  this.a = [ x ];
};
clojure.JS.merge( java.lang.StringBuilder.prototype, {
  append: function( x ) { this.a.push( x ); return this; },
  toString: function() { return this.a.join(''); }
});

delete clojure_merge;
delete clojure_Namespace;
delete clojure_global;
