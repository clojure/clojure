/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Aug 21, 2007 */

package clojure.lang;

import java.util.HashMap;

//todo: possibly extend URLClassLoader?

public class DynamicClassLoader extends ClassLoader{

HashMap<String, byte[]> map = new HashMap<String, byte[]>();

public DynamicClassLoader(){
	super();
}

public DynamicClassLoader(ClassLoader parent){
	super(parent);
}

public void addBytecode(String className, byte[] bytes){
	if(map.containsKey(className))
		throw new IllegalStateException(String.format("Class %s already present", className));
	map.put(className, bytes);
}

protected Class<?> findClass(String name) throws ClassNotFoundException{
	byte[] bytes = map.get(name);
	if(bytes != null)
		return defineClass(name, bytes, 0, bytes.length);
	throw new ClassNotFoundException(name);
}
}
