/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich May 14, 2008 */

package clojure.lang;

import java.util.Collection;

public class LazilyPersistentVector{


static public IPersistentVector createOwning(Object... items){
	if(items.length == 0)
		return PersistentVector.EMPTY;
	else if(items.length <= 32)
		return new PersistentVector(items.length, 5, PersistentVector.EMPTY_NODE,items);
	return PersistentVector.create(items);
}

static public IPersistentVector create(Collection coll){
	if(!(coll instanceof ISeq) && coll.size() <= 32)
		return createOwning(coll.toArray());
	return PersistentVector.create(RT.seq(coll));
}

}
