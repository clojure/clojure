// display topology of hashmaps, for debugging
function maptop(x,d) {
  d = d || "";
  var d2 = d + "  ";
  var c = x.constructor.classname;
  print(d+c);
  switch(c) {
    case "PersistentHashMap": maptop(x._root,d2); break;
    case "BitmapIndexedNode":
    case "FullNode":
      for( var i = 0; i < x.nodes.length; ++i ) {
        maptop(x.nodes[i],d2);
      }
      break;
    case "HashCollisionNode":
      for( var i = 0; i < x.leaves.length; ++i ) {
        maptop(x.leaves[i],d2);
      }
      break;
    case "LeafNode": print( d2 + x.key() + " : " + x.val() ); break;
  }
}

y = clojure.lang.PersistentHashMap.EMPTY;
for( var i = 0; i < 10; ++i ) {
  y = y.assoc( "a" + String.fromCharCode( 48 + i ), i );
  maptop( y );
}
