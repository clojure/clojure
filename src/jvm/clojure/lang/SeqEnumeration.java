/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 3, 2008 */

package clojure.lang;

import java.util.Enumeration;

public class SeqEnumeration implements Enumeration{
ISeq seq;

public SeqEnumeration(ISeq seq){
	this.seq = seq;
}

public boolean hasMoreElements(){
	return seq != null;
}

public Object nextElement(){
	Object ret = RT.first(seq);
	seq = RT.next(seq);
	return ret;
}
}
