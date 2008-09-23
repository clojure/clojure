function vToString( v ) {
  var a = new Array( v.count() );
  for( var i = 0; i < v.count(); ++i ) {
    a[ i ] = v.nth( i );
  }
  return ['[', a.join(' '), ']'].join('');
}

var v = clojure.lang.PersistentVector.EMPTY;
for( var i = 0; i < 100; ++i ) {
  v = v.cons( i * 10 );
}
print( vToString( v ) );
print( vToString( v.assocN( 20, 999 ) ) );

var a = [];
for( v2 = v; v2.count() > 0; v2 = v2.pop() ) {
  a.push( v2.peek() );
}
print( a );

v = clojure.lang.PersistentVector.EMPTY;
for( var i = 0; i < 100000; ++i ) { v = v.cons( i ); }
for(; v.count() > 0; v = v.pop() ) { v.peek() };


print( vToString( clojure.lang.PersistentVector.create(
            [ 'a', 'b', 'c', 'd', 'e' ] ) ) );

function time( msg, fn, reps ) {
  reps = reps || 1;
  var start = new Date();
  var last;
  for( var i = 0; i < reps; ++i ) {
    last = fn();
  }
  var end = new Date();
  print( msg + ': ' + (end - start) + ' msecs' );
  return last;
}

var Rand = (function(){
  var cycle = 1000000;
  var rnd = new Array( cycle );
  var idx = -1;
  for( var i = 0; i < cycle; ++i ) {
    rnd[i] = Math.random();
  }
  return {
    reset: function() { idx = -1; },
    next: function( r ) {
      idx = (idx + 1) % cycle;
      return Math.floor( rnd[ idx ] * r );
    }
  };
})();

function suite( size, writes, reads, reps ) {
  print( "Suite size: " + size + ", writes: " + writes + ", reads: " + reads );

  var a = [];
  var p = clojure.lang.PersistentVector.EMPTY;

  time( "  Array push", function() {
    for( var i = 0; i < size; i++ ) {
      a.push( i );
    }
  }, reps );

  time( "  PV cons   ", function() {
    for( var i = 0; i < size; i++ ) {
      p = p.cons( i );
    }
  }, reps );

  var ta = 0;
  time( "  Array set ", function() {
    Rand.reset();
    for( var i = 0; i < writes; ++i ) {
      a[ Rand.next( size ) ] = i;
    }
    for( var j = 0; j < reads; ++j ) {
      ta += a[ Rand.next( size ) ];
    }
  }, reps);

  var tp = 0;
  time( "  PV set    ", function() {
    Rand.reset();
    for( var i = 0; i < writes; ++i ) {
      p = p.assocN( Rand.next( size ), i );
    }
    for( var j = 0; j < reads; ++j ) {
      tp += p.nth( Rand.next( size ) );
    }
  }, reps);

  print( "Done: " + ta + ", " + tp + "\n" );
}

suite( 100000, 10000, 20000 );
suite( 30, 10000, 20000, 50 );
suite( 100000, 10000, 0 );
suite( 30, 10000, 0, 50 );
suite( 100000, 0, 20000 );
suite( 30, 0, 20000, 100 );

/*
var p = clojure.lang.PersistentVector.EMPTY;
for( var i = 0; i < 1088; i++ ) {
//for( var i = 0; i < 1056; i++ ) {
  p = p.cons( i );
}
print( p.nth( p.count() - 33 ) )
print( p.cons("oops").nth( p.count() - 33 ) )
*/

//print( clojure.lang.PersistentVector.EMPTY.constructor );
print('done');
