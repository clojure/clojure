// Copyright (c) Metadata Partners, LLC.
// All rights reserved.

/* rich 4/30/12 */

package clojure.lang;

public class Reduced implements IDeref{
Object val;

public Reduced(Object val){
	this.val = val;
}

public Object deref(){
	return val;
}
}
