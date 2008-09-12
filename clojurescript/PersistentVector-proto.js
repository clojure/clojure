RT = { EMPTY_ARRAY: [] };

function APersistentVector( _meta ) {
  this._meta = _meta;
}

APersistentVector.prototype.meta = function() {
  return this._meta;
};

APersistentVector.prototype.peek = function() {
  if( this.count() > 0 ) {
    return this.nth( this.count() - 1 );
  }
  return null;
};

function PersistentVector( _meta, cnt, shift, root, tail ) {
  APersistentVector.call( this, _meta );
  this._meta = _meta;
  this.cnt = cnt;
  this.shift = shift;
  this.root = root;
  this.tail = tail;
}

PersistentVector.prototype = new APersistentVector( null );
PersistentVector.contraction = PersistentVector;

PersistentVector.prototype.tailoff = function() {
  return this.cnt - this.tail.length;
};

PersistentVector.prototype.nth = function( i ) {
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
};

PersistentVector.prototype.assocN = function( i, val ) {
  if( i >= 0 && i < this.cnt ) {
    if( i >= this.tailoff() ) {
      var newTail = this.tail.slice( 0 );
      newTail[ i & 0x01f ] = val;
      return new PersistentVector(
          this.meta(), this.cnt, this.shift, this.root, newTail );
    }
    return new PersistentVector(
        this.meta(), this.cnt, this.shift,
        this.doAssoc( this.shift, this.root, i, val), this.tail );
  }
  if( i == this.cnt ) {
    return this.cons( val );
  }
  throw "IndexOutOfBoundsException";
};

PersistentVector.prototype.doAssoc = function( level, arr, i, val ) {
  var ret = arr.slice( 0 );
  if( level == 0 ) {
    ret[ i & 0x01f ] = val;
  }
  else {
    var subidx = (i >>> level) & 0x01f;
    ret[ subidx ] = this.doAssoc( level - 5, arr[ subidx ], i, val );
  }
  return ret;
};

PersistentVector.prototype.count = function() {
  return this.cnt;
};

PersistentVector.prototype.withMeta = function( _meta ) {
  return new PersistentVector(
      _meta, this.cnt, this.shift, this.root, this.tail );
};

PersistentVector.prototype.cons = function( val ) {
  if( this.tail.length < 32 ) {
    var newTail = this.tail.concat( [val] );
    return new PersistentVector(
        this.meta(), this.cnt + 1, this.shift, this.root, newTail );
  }
  var expansion = [null];
  var newroot = this.pushTail( this.shift - 5, this.root, this.tail, expansion);
  var newshift = this.shift;
  if( expansion[0] != null ) {
    newroot = [newroot, expansion[0]];
    newshift += 5;
  }
  return new PersistentVector(
      this.meta(), this.cnt+1, newshift, newroot, [val] );
};

PersistentVector.prototype.empty = function() {
  return PersistentVector.EMPTY.withMeta( this.meta() );
};

PersistentVector.prototype.pushTail = function( level, arr, tailNode, expansion)
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
};

PersistentVector.prototype.pop = function() {
  if( this.cnt == 0 ) {
    throw "IllegalStateException: Can't pop empty vector";
  }
  if( this.cnt == 1 ) {
    return PersistentVector.EMPTY.withMeta( this.meta() );
  }
  if( this.tail.length > 1 ) {
    var newTail = this.tail.slice( 0, this.tail.length - 1 );
    return new PersistentVector(
        this.meta(), this.cnt - 1, this.shift, this.root, newTail );
  }
  var ptail = [null];
  var newroot = this.popTail( this.shift - 5, this.root, ptail );
  var newshift = this.shift;
  if( newroot == null ) {
    newroot = RT.EMPTY_ARRAY;
  }
  if( this.shift > 5 && newroot.length == 1 ) {
    newroot = newroot[0];
    newshift -= 5;
  }
  return new PersistentVector(
      this.meta(), this.cnt - 1, newshift, newroot, ptail[0] );
};

PersistentVector.prototype.popTail = function( shift, arr, ptail ) {
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
};

PersistentVector.EMPTY = new PersistentVector(
  {}, 0, 5, RT.EMPTY_ARRAY, RT.EMPTY_ARRAY );

PersistentVector.create = function( items ) {
  var ret = PersistentVector.EMPTY;
  for( var i = 0; i < items.length; ++i ) {
    ret = ret.cons( items[ i ] );
  }
  return ret;
}
