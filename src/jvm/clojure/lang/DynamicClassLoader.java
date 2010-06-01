/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Aug 21, 2007 */

package clojure.lang;

import java.util.HashMap;
import java.util.Map;
import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import java.net.URLClassLoader;
import java.net.URL;
import java.lang.ref.WeakReference;

public class DynamicClassLoader extends URLClassLoader{
HashMap<Integer, Object[]> constantVals = new HashMap<Integer, Object[]>();
static ConcurrentHashMap<String, Map.Entry<WeakReference<Class>,Object> >classCache =
        new ConcurrentHashMap<String, Map.Entry<WeakReference<Class>,Object> >();

static final URL[] EMPTY_URLS = new URL[]{};

public DynamicClassLoader(){
    //pseudo test in lieu of hasContextClassLoader()
	super(EMPTY_URLS,(Thread.currentThread().getContextClassLoader() == null ||
                Thread.currentThread().getContextClassLoader() == ClassLoader.getSystemClassLoader())?
                Compiler.class.getClassLoader():Thread.currentThread().getContextClassLoader());
}

public DynamicClassLoader(ClassLoader parent){
	super(EMPTY_URLS,parent);
}

public Class defineClass(String name, byte[] bytes, Object srcForm){
//    Map.Entry<WeakReference<Class>,Object> ce = classCache.get(name);
//    if(ce != null)
//        {
//        WeakReference<Class> cr = ce.getKey();
//        Class c = cr.get();
//        if((c != null) && srcForm.equals(ce.getValue()))
//            return c;
//        }
	Class c = defineClass(name, bytes, 0, bytes.length);
    classCache.put(name, new MapEntry(new WeakReference(c), null));
    return c;
}

protected Class<?> findClass(String name) throws ClassNotFoundException{
    Map.Entry<WeakReference<Class>,Object> ce = classCache.get(name);
    if(ce != null)
        {
        WeakReference<Class> cr = ce.getKey();
        Class c = cr.get();
        if(c != null)
            return c;
        classCache.remove(name);
        }
	return super.findClass(name);
}

public void registerConstants(int id, Object[] val){
	constantVals.put(id, val);
}

public Object[] getConstants(int id){
	return constantVals.get(id);
}

public void addURL(URL url){
	super.addURL(url);
}

}
