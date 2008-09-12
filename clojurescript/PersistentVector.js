RT = { EMPTY_ARRAY: [] };

function APersistentVector( meta_arg ) {

  this.meta = function() {
    return meta_arg;
  };

  this.peek = function() {
    if( this.count() > 0 ) {
      return this.nth( this.count() - 1 );
    }
    return null;
  };
};

function PersistentVector( meta_arg, cnt, shift, root, tail ) {
  APersistentVector.call( this, meta_arg );

  function tailoff() {
    return cnt - tail.length;
  };

  this.nth = function( i ) {
    if( i >= 0 && i < cnt ) {
      if( i >= tailoff() ) {
        return tail[ i & 0x01f ];
      }
      var arr = root;
      for( var level = shift; level > 0; level -= 5 ) {
        arr = arr[ (i >>> level) & 0x01f ];
      }
      return arr[ i & 0x01f ];
    }
    throw "IndexOutOfBoundsException";
  };

  this.assocN = function( i, val ) {
    if( i >= 0 && i < cnt ) {
      if( i >= tailoff() ) {
        var newTail = tail.slice( 0 );
        newTail[ i & 0x01f ] = val;
        return new PersistentVector( this.meta(), cnt, shift, root, newTail );
      }
      return new PersistentVector(
          this.meta(), cnt, shift, doAssoc( shift, root, i, val), tail );
    }
    if( i == cnt ) {
      return this.cons( val );
    }
    throw "IndexOutOfBoundsException";
  };

  function doAssoc( level, arr, i, val ) {
    var ret = arr.slice( 0 );
    if( level == 0 ) {
      ret[ i & 0x01f ] = val;
    }
    else {
      var subidx = (i >>> level) & 0x01f;
      ret[ subidx ] = doAssoc( level - 5, arr[ subidx ], i, val );
    }
    return ret;
  };

  this.count = function() {
    return cnt;
  };

  this.withMeta = function( meta_arg ) {
    return new PersistentVector( meta_arg, cnt, shift, root, tail );
  };

  this.cons = function( val ) {
    if( tail.length < 32 ) {
      var newTail = tail.concat( [val] );
      return new PersistentVector( this.meta(), cnt + 1, shift, root, newTail );
    }
    var expansion = [null];
    var newroot = pushTail( shift - 5, root, tail, expansion );
    var newshift = shift;
    if( expansion[0] != null ) {
      newroot = [newroot, expansion[0]];
      newshift += 5;
    }
    return new PersistentVector( this.meta(), cnt+1, newshift, newroot, [val] );
  };

  this.empty = function() {
    return PersistentVector.EMPTY.withMeta( this.meta() );
  };

  function pushTail( level, arr, tailNode, expansion ) {
    var newchild;
    if( level == 0 ) {
      newchild = tailNode;
    }
    else {
      newchild = pushTail( level - 5, arr[arr.length - 1], tailNode, expansion);
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
  };

  this.pop = function() {
    if( cnt == 0 ) {
      throw "IllegalStateException: Can't pop empty vector";
    }
    if( cnt == 1 ) {
      return PersistentVector.EMPTY.withMeta( this.meta() );
    }
    if( tail.length > 1 ) {
      var newTail = tail.slice( 0, tail.length - 1 );
      return new PersistentVector( this.meta(), cnt - 1, shift, root, newTail );
    }
    var ptail = [null];
    var newroot = popTail( shift - 5, root, ptail );
    var newshift = shift;
    if( newroot == null ) {
      newroot = RT.EMPTY_ARRAY;
    }
    if( shift > 5 && newroot.length == 1 ) {
      newroot = newroot[0];
      newshift -= 5;
    }
    return new PersistentVector(
        this.meta(), cnt - 1, newshift, newroot, ptail[0] );
  };

  function popTail( shift, arr, ptail ) {
    if( shift > 0 ) {
      var newchild = popTail( shift - 5, arr[ arr.length - 1 ], ptail );
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
  };
}

PersistentVector.EMPTY = new PersistentVector(
    {}, 0, 5, RT.EMPTY_ARRAY, RT.EMPTY_ARRAY );

PersistentVector.create = function( items ) {
  var ret = PersistentVector.EMPTY;
  for( var i = 0; i < items.length; ++i ) {
    ret = ret.cons( items[ i ] );
  }
  return ret;
}
