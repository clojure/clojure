/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

import java.util.Iterator;

public class MapEntry extends AMapEntry{
final Object _key;
final Object _val;

public MapEntry(Object key, Object val){
	this._key = key;
	this._val = val;
}

public Object key(){
	return _key;
}

public Object val(){
	return _val;
}

public Object getKey(){
	return key();
}

public Object getValue(){
	return val();
}

}
