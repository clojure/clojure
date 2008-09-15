clojure = {
  in_ns: function(s) {
    var ns = s.substring(1);
    if( ! window[ns] ) {
      window[ns] = {};
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
  prn: print,
  count: function(x) {
    if( x === null ) return 0;
    if( x.count ) return x.count();
    if( x.length != undefined ) return x.length;
    throw ("count not supported on: " + (typeof x) + " " + x.constructor);
  },
  JS: {
    resolveVar: function( symns, symnm, ctxns ) {
      if( symns ) {
        return symns[ symnm ];
      }
      else {
        return ctxns[ symnm ] || clojure[ symnm ] || window[ symnm ];
      }
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
  lang: {}
};

clojure.lang.ArraySeq = function( _meta, a, i ) {
  this._meta = _meta;
  this.a = a;
  this.i = i;
};

clojure.lang.ArraySeq.create = function( a ) {
  if( a && a.length ) {
    return new clojure.lang.ArraySeq( null, a, 0 );
  }
  else {
    return nil;
  }
};

clojure.lang.ArraySeq.prototype.first = function() {
  return this.a[this.i];
};

clojure.lang.ArraySeq.prototype.rest = function() {
  if( this.i + 1 < this.a.length )
    return new clojure.lang.ArraySeq( this._meta, this.a, this.i + 1 );
  return null;
};

clojure.lang.ArraySeq.prototype.count = function() {
  return this.a.length - this.i;
};

clojure.lang.ArraySeq.prototype.index = function() {
  return this.i;
};

clojure.lang.ArraySeq.prototype.withMeta = function( _meta ) {
  return new clojure.lang.ArraySeq( _meta, this.array, this.i );
};

clojure.lang.ArraySeq.prototype.reduce = function( fn, start ) {
  var ret = (start === undefined) ? this.a[0] : fn(start, this.a[0]);
  for( var x = this.i + 1; x < this.a.length; ++x ) {
    ret = fn( ret, this.a[x] );
  }
  return ret;
};

clojure.lang.ArraySeq.prototype.seq = function() {
  return this;
};


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
