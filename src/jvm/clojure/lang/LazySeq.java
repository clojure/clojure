/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Apr 20, 2008 */

package clojure.lang;

final public class LazySeq extends ASeq{
final IFn f;

public LazySeq(IFn f){
	this.f = f;
}

final public Object first(){
	try
		{
		return f.invoke();
		}
	catch(Exception e)
		{
		throw new RuntimeException(e);
		}
}

final public ISeq rest(){
	try
		{
		return RT.seq(f.invoke(null));
		}
	catch(Exception e)
		{
		throw new RuntimeException(e);
		}
}

LazySeq(IPersistentMap meta, IFn f){
	super(meta);
	this.f = f;
}

public Obj withMeta(IPersistentMap meta){
	if(meta == meta())
		return this;
	return new LazySeq(meta, f);
}
}
