/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 4:28:27 PM */

package clojure.lang;

import java.util.Iterator;
import java.util.concurrent.atomic.AtomicInteger;
import java.io.*;

public class RT{

static public Symbol T = Symbol.create(null, "t");
final static public Var OUT =
		Var.intern(Symbol.create("clojure", "out"), new OutputStreamWriter(System.out));
final static Keyword TAG_KEY = Keyword.intern("clojure", "tag");

//final static public Var CURRENT_MODULE = Var.intern(Symbol.create("clojure", "current-module"),
//                                                    Module.findOrCreateModule("clojure/user"));

final static Symbol LOAD_FILE = Symbol.create("clojure", "load-file");
final static Symbol EQL_REF = Symbol.create("clojure", "eql-ref?");

//string
final static Var CURRENT_NS = Var.intern(Symbol.create("clojure", "*current-namespace*"), "clojure");
//simple-symbol->var
final static Var REFERS =
		Var.intern(Symbol.create("clojure", "*refers*"),
		           map(
				           LOAD_FILE, Var.intern(LOAD_FILE,
		                                         new AFn(){
			                                         public Object invoke(Object arg1) throws Exception{
				                                         return Compiler.loadFile((String) arg1);
			                                         }
		                                         }),
				           EQL_REF, Var.intern(EQL_REF,
		                                       new AFn(){
			                                       public Object invoke(Object arg1, Object arg2)
					                                       throws Exception{
				                                       return arg1 == arg2 ? RT.T : null;
			                                       }
		                                       })
		           ));
//simple-symbol->fully-qualified-class-name-string
final static Var IMPORTS = Var.intern(Symbol.create("clojure", "*imports*"),
                                      map(Symbol.create("RT"), "clojure.lang.RT",
                                          Symbol.create("Num"), "clojure.lang.Num",
                                          Symbol.create("Symbol"), "clojure.lang.Symbol",
                                          Symbol.create("Keyword"), "clojure.lang.Keyword",
                                          Symbol.create("Var"), "clojure.lang.Var",
                                          Symbol.create("Ref"), "clojure.lang.Ref",
                                          Symbol.create("IFn"), "clojure.lang.IFn",
                                          Symbol.create("IObj"), "clojure.lang.IObj",
                                          Symbol.create("ISeq"), "clojure.lang.ISeq",
                                          Symbol.create("IPersistentCollection"), "clojure.lang.IPersistentCollection",
                                          Symbol.create("IPersistentMap"), "clojure.lang.IPersistentMap",
                                          Symbol.create("IPersistentList"), "clojure.lang.IPersistentList",
                                          Symbol.create("IPersistentVector"), "clojure.lang.IPersistentVector",
                                          Symbol.create("Boolean"), "java.lang.Boolean",
                                          Symbol.create("Byte"), "java.lang.Byte",
                                          Symbol.create("Character"), "java.lang.Character",
                                          Symbol.create("Class"), "java.lang.Class",
                                          Symbol.create("ClassLoader"), "java.lang.ClassLoader",
                                          Symbol.create("Compiler"), "java.lang.Compiler",
                                          Symbol.create("Double"), "java.lang.Double",
                                          Symbol.create("Enum"), "java.lang.Enum",
                                          Symbol.create("Float"), "java.lang.Float",
                                          Symbol.create("InheritableThreadLocal"), "java.lang.InheritableThreadLocal",
                                          Symbol.create("Integer"), "java.lang.Integer",
                                          Symbol.create("Long"), "java.lang.Long",
                                          Symbol.create("Math"), "java.lang.Math",
                                          Symbol.create("Number"), "java.lang.Number",
                                          Symbol.create("Object"), "java.lang.Object",
                                          Symbol.create("Package"), "java.lang.Package",
                                          Symbol.create("Process"), "java.lang.Process",
                                          Symbol.create("ProcessBuilder"), "java.lang.ProcessBuilder",
                                          Symbol.create("Runtime"), "java.lang.Runtime",
                                          Symbol.create("RuntimePermission"), "java.lang.RuntimePermission",
                                          Symbol.create("SecurityManager"), "java.lang.SecurityManager",
                                          Symbol.create("Short"), "java.lang.Short",
                                          Symbol.create("StackTraceElement"), "java.lang.StackTraceElement",
                                          Symbol.create("StrictMath"), "java.lang.StrictMath",
                                          Symbol.create("String"), "java.lang.String",
                                          Symbol.create("StringBuffer"), "java.lang.StringBuffer",
                                          Symbol.create("StringBuilder"), "java.lang.StringBuilder",
                                          Symbol.create("System"), "java.lang.System",
                                          Symbol.create("Thread"), "java.lang.Thread",
                                          Symbol.create("ThreadGroup"), "java.lang.ThreadGroup",
                                          Symbol.create("ThreadLocal"), "java.lang.ThreadLocal",
                                          Symbol.create("Throwable"), "java.lang.Throwable",
                                          Symbol.create("Void"), "java.lang.Void",
                                          Symbol.create("Appendable"), "java.lang.Appendable",
                                          Symbol.create("CharSequence"), "java.lang.CharSequence",
                                          Symbol.create("Cloneable"), "java.lang.Cloneable",
                                          Symbol.create("Iterable"), "java.lang.Iterable",
                                          Symbol.create("Readable"), "java.lang.Readable",
                                          Symbol.create("Runnable"), "java.lang.Runnable",
                                          Symbol.create("Exception"), "java.lang.Exception",
                                          Symbol.create("Collection"), "java.util.Collection",
                                          Symbol.create("Comparator"), "java.util.Comparator",
                                          Symbol.create("Enumeration"), "java.util.Enumeration",
                                          Symbol.create("EventListener"), "java.util.EventListener",
                                          Symbol.create("Formattable"), "java.util.Formattable",
                                          Symbol.create("Iterator"), "java.util.Iterator",
                                          Symbol.create("List"), "java.util.List",
                                          Symbol.create("ListIterator"), "java.util.ListIterator",
                                          Symbol.create("Map"), "java.util.Map",
                                          Symbol.create("Map$Entry"), "java.util.Map$Entry",
                                          Symbol.create("Observer"), "java.util.Observer",
                                          Symbol.create("Queue"), "java.util.Queue",
                                          Symbol.create("RandomAccess"), "java.util.RandomAccess",
                                          Symbol.create("Set"), "java.util.Set",
                                          Symbol.create("SortedMap"), "java.util.SortedMap",
                                          Symbol.create("SortedSet"), "java.util.SortedSet"
                                      ));

static public final Object[] EMPTY_ARRAY = new Object[]{};
//static public final Character[] chars;
static AtomicInteger id = new AtomicInteger(1);

//static
//	{
//	chars = new Character[256];
//	for(int i = 0; i < chars.length; i++)
//		chars[i] = new Character((char) i);
//	}


static public int nextID(){
	return id.getAndIncrement();
}

static public boolean equal(Object k1, Object k2){
	return k1 == k2 ||
	       (k1 != null && k1.equals(k2));
}

//static public Object eq(Object arg1, Object arg2){
//	return (arg1 == arg2) ? Boolean.TRUE : null;
//}
//
//static public Object eql(Object arg1, Object arg2){
//	if(arg1 == arg2)
//		return Boolean.TRUE;
//	if(arg1 == null || arg2 == null)
//		return null;
//	if(arg1 instanceof Num
//	   && arg1.getClass() == arg2.getClass()
//	   && arg1.equals(arg2))
//		return Boolean.TRUE;
//	if(arg1.getClass() == Character.class
//	   && arg2.getClass() == Character.class
//	   && arg1.equals(arg2))
//		return Boolean.TRUE;
//	return null;
//}

//    static public Object equal(Object arg1, Object arg2) {
//        if(arg1 == null)
//            return arg2 == null ? Boolean.TRUE : null;
//        else if(arg2 == null)
//            return null;
//        return (eql(arg1,arg2) != null
//                || (arg1.getClass() == Cons.class
//                    && arg2.getClass() == Cons.class
//                    && equal(((Cons)arg1)._first,((Cons)arg2)._first)!=null
//                    && equal(((Cons)arg1)._rest,((Cons)arg2)._rest)!=null))
//               ?Boolean.TRUE:null;
//        }

////////////// Collections support /////////////////////////////////

static public int hash(Object o){
	if(o == null)
		return 0;
	return o.hashCode();
}

static public int hashCombine(int seed, int hash){
	//a la boost
	seed ^= hash + 0x9e3779b9 + (seed << 6) + (seed >> 2);
	return seed;
}

static public ISeq seq(Object coll){
	if(coll == null)
		return null;
	else if(coll instanceof IPersistentCollection)
		return ((IPersistentCollection) coll).seq();
	else if(coll instanceof Iterable)
		return IteratorSeq.create(((Iterable) coll).iterator());
	else if(coll instanceof Object[])
		return ArraySeq.create((Object[]) coll);
	else
		throw new IllegalAccessError("Don't know how to create ISeq from arg");
}

static public ISeq keys(Object coll){
	return APersistentMap.KeySeq.create(seq(coll));
}

static public ISeq vals(Object coll){
	return APersistentMap.ValSeq.create(seq(coll));
}

static public IPersistentMap meta(Object x){
	if(x == null)
		return null;
	return ((Obj) x).meta();
}

public static int count(Object o){
	if(o == null)
		return 0;
	return ((IPersistentCollection) o).count();
}

static public IPersistentCollection conj(IPersistentCollection coll, Object x){
	if(coll == null)
		return new PersistentList(x);
	return coll.cons(x);
}

static public ISeq cons(Object x, ISeq y){
	if(y == null)
		return new PersistentList(x);
	return y.cons(x);
}

static public Object first(Object x){
	ISeq seq = seq(x);
	if(seq == null)
		return null;
	return seq.first();
}

static public Object second(Object x){
	return first(rest(x));
}

static public Object third(Object x){
	return first(rest(rest(x)));
}

static public Object fourth(Object x){
	return first(rest(rest(rest(x))));
}

static public ISeq rest(Object x){
	ISeq seq = seq(x);
	if(seq == null)
		return null;
	return seq.rest();
}

static public ISeq rrest(Object x){
	return rest(rest(x));
}

static public Object peek(Object x){
	if(x == null)
		return null;
	return ((IPersistentStack) x).peek();
}

static public Object pop(Object x){
	if(x == null)
		return null;
	return ((IPersistentStack) x).pop();
}

static public Object get(Object coll, Object key){
	if(coll == null)
		return null;
	return ((Associative) coll).valAt(key);
}

static public Associative assoc(Object coll, Object key, Object val){
	if(coll == null)
		return new MapEntry(key, val);
	return ((Associative) coll).assoc(key, val);
}

static public Object contains(Object coll, Object key){
	if(coll == null)
		return false;
	return ((Associative) coll).contains(key) ? T : null;
}

static public Object find(Object coll, Object key){
	if(coll == null)
		return null;
	return ((Associative) coll).entryAt(key);
}

//takes a seq of key,val,key,val

//returns tail starting at val of matching key if found, else null
static public ISeq findKey(Keyword key, ISeq keyvals) throws Exception{
	while(keyvals != null)
		{
		ISeq r = keyvals.rest();
		if(r == null)
			throw new Exception("Malformed keyword argslist");
		if(keyvals.first() == key)
			return r;
		keyvals = r.rest();
		}
	return null;
}

static public Object dissoc(Object key, Object coll){
	if(coll == null)
		return null;
	return ((IPersistentMap) coll).without(key);
}

static public Object nth(Object coll, int n){
	if(coll == null)
		return null;
	else if(coll instanceof IPersistentVector)
		return ((IPersistentVector) coll).nth(n);
	else if(coll instanceof Object[])
		return ((Object[]) coll)[n];
	else if(coll instanceof Sequential)
		{
		ISeq seq = ((IPersistentCollection) coll).seq();
		for(int i = 0; i <= n && seq != null; ++i, seq = seq.rest())
			{
			if(i == n)
				return seq.first();
			}
		return null;
		}
	else
		return null;
}

static public Object assocN(int n, Object val, Object coll){
	if(coll == null)
		return null;
	else if(coll instanceof IPersistentVector)
		return ((IPersistentVector) coll).assocN(n, val);
	else if(coll instanceof Object[])
		{
		//hmm... this is not persistent
		Object[] array = ((Object[]) coll);
		array[n] = val;
		return array;
		}
	else
		return null;
}

/*
static public Iter iter(Object coll){
	if(coll == null || coll instanceof Iter)
		return (Iter) coll;
	else if(coll instanceof Iterator)
		{
		Iterator i = (Iterator) coll;
		if(i.hasNext())
			return new IteratorIter(i);
		return null;
		}
	else if(coll instanceof Iterable)
		return new IteratorIter(((Iterable) coll).iterator());

	else
		throw new IllegalArgumentException("Don't know how to create Iter from arg");
}
 */
static boolean hasTag(Object o, Object tag){
	if(!(o instanceof IObj))
		return false;
	IPersistentMap meta = ((IObj) o).meta();
	return RT.equal(tag, RT.get(meta, TAG_KEY));
}

/**
 * ********************* Boxing/casts ******************************
 */
static public Object box(Object x){
	return x;
}

static public Character box(char x){
	return Character.valueOf(x);
}

static public Object box(boolean x){
	return x ? T : null;
}

static public Object box(Boolean x){
	return x ? T : null;
}

static public Num box(byte x){
	return Num.from(x);
}

static public Num box(short x){
	return Num.from(x);
}

static public Num box(int x){
	return Num.from(x);
}

static public Num box(long x){
	return Num.from(x);
}

static public Num box(float x){
	return Num.from(x);
}

static public Num box(double x){
	return Num.from(x);
}

static public char charCast(Object x){
	if(x instanceof Character)
		return ((Character) x).charValue();
	return (char) ((Number) x).intValue();
}

static public boolean booleanCast(Object x){
	if(x instanceof Boolean)
		return ((Boolean) x).booleanValue();
	return x != null;
}

static public byte byteCast(Object x){
	return ((Number) x).byteValue();
}

static public short shortCast(Object x){
	return ((Number) x).shortValue();
}

static public int intCast(Object x){
	return ((Number) x).intValue();
}

static public long longCast(Object x){
	return ((Number) x).longValue();
}

static public float floatCast(Object x){
	return ((Number) x).floatValue();
}

static public double doubleCast(Object x){
	return ((Number) x).doubleValue();
}


static public IPersistentMap map(Object... init){
	return PersistentHashMap.create(init);
}

static public IPersistentVector vector(Object... init){
	return PersistentVector.create(init);
}

/**
 * **************************************** list support *******************************
 */


static public ISeq list(){
	return null;
}

static public ISeq list(Object arg1){
	return new PersistentList(arg1);
}

static public ISeq list(Object arg1, Object arg2){
	return listStar(arg1, arg2, null);
}

static public ISeq list(Object arg1, Object arg2, Object arg3){
	return listStar(arg1, arg2, arg3, null);
}

static public ISeq list(Object arg1, Object arg2, Object arg3, Object arg4){
	return listStar(arg1, arg2, arg3, arg4, null);
}

static public ISeq list(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5){
	return listStar(arg1, arg2, arg3, arg4, arg5, null);
}

static public ISeq listStar(Object arg1, ISeq rest){
	return (ISeq) cons(arg1, rest);
}

static public ISeq listStar(Object arg1, Object arg2, ISeq rest){
	return (ISeq) cons(arg1, cons(arg2, rest));
}

static public ISeq listStar(Object arg1, Object arg2, Object arg3, ISeq rest){
	return (ISeq) cons(arg1, cons(arg2, cons(arg3, rest)));
}

static public ISeq listStar(Object arg1, Object arg2, Object arg3, Object arg4, ISeq rest){
	return (ISeq) cons(arg1, cons(arg2, cons(arg3, cons(arg4, rest))));
}

static public ISeq listStar(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq rest){
	return (ISeq) cons(arg1, cons(arg2, cons(arg3, cons(arg4, cons(arg5, rest)))));
}

static public ISeq arrayToList(Object[] a) throws Exception{
	ISeq ret = null;
	for(int i = a.length - 1; i >= 0; --i)
		ret = (ISeq) cons(a[i], ret);
	return ret;
}

static public Object[] seqToArray(ISeq seq) throws Exception{
	int len = length(seq);
	Object[] ret = new Object[len];
	for(int i = 0; seq != null; ++i, seq = seq.rest())
		ret[i] = seq.first();
	return ret;
}

static public int length(ISeq list) throws Exception{
	int i = 0;
	for(ISeq c = list; c != null; c = c.rest())
		{
		i++;
		}
	return i;
}

static public int boundedLength(ISeq list, int limit) throws Exception{
	int i = 0;
	for(ISeq c = list; c != null && i <= limit; c = c.rest())
		{
		i++;
		}
	return i;
}

///////////////////////////////// reader support ////////////////////////////////

static Character readRet(int ret){
	if(ret == -1)
		return null;
	return box((char) ret);
}

static public Character readChar(Reader r) throws Exception{
	int ret = r.read();
	return readRet(ret);
}

static public Character peekChar(Reader r) throws Exception{
	int ret;
	if(r instanceof PushbackReader)
		{
		ret = r.read();
		((PushbackReader) r).unread(ret);
		}
	else
		{
		r.mark(1);
		ret = r.read();
		r.reset();
		}

	return readRet(ret);
}

static public int getLineNumber(Reader r){
	if(r instanceof LineNumberingPushbackReader)
		return ((LineNumberingPushbackReader) r).getLineNumber();
	return 0;
}

static public LineNumberingPushbackReader getLineNumberingReader(Reader r){
	if(isLineNumberingReader(r))
		return (LineNumberingPushbackReader) r;
	return new LineNumberingPushbackReader(r);
}

static public boolean isLineNumberingReader(Reader r){
	return r instanceof LineNumberingPushbackReader;
}

static public String resolveClassNameInContext(String className){
	//todo - look up in context var
	return className;
}

static public boolean suppressRead(){
	//todo - look up in suppress-read var
	return false;
}


static public void print(Object x, Writer w) throws Exception{
	//todo - make extensible
	if(x instanceof Obj)
		{
		Obj o = (Obj) x;
		if(RT.count(o.meta()) > 0)
			{
			IPersistentMap meta = o.meta();
			w.write("#^");
			if(meta.count() == 1 && meta.contains(TAG_KEY))
				print(meta.valAt(TAG_KEY), w);
			else
				print(meta, w);
			w.write(' ');
			}
		}
	if(x == null)
		w.write("nil");
	else if(x instanceof ISeq)
		{
		w.write('(');
		printInnerSeq(seq(x), w);
		w.write(')');
		}
	else if(x instanceof String)
		{
		w.write('"');
		w.write(x.toString());
		w.write('"');
		}
//	else if(x instanceof ArgVector)
//		{
//		w.write('|');
//		printInnerSeq(seq(x), w);
//		w.write('|');
//		}
	else if(x instanceof IPersistentMap)
		{
		w.write('{');
		for(ISeq s = seq(x); s != null; s = s.rest())
			{
			IMapEntry e = (IMapEntry) s.first();
			print(e.key(), w);
			w.write(' ');
			print(e.val(), w);
			if(s.rest() != null)
				w.write(", ");
			}
		w.write('}');
		}
	else if(x instanceof IPersistentVector)
		{
		IPersistentVector a = (IPersistentVector) x;
		w.write('[');
		for(int i = 0; i < a.count(); i++)
			{
			print(a.nth(i), w);
			if(i < a.count() - 1)
				w.write(' ');
			}
		w.write(']');
		}
	else if(x instanceof Character)
		{
		w.write('\\');
		char c = ((Character) x).charValue();
		switch(c)
			{
			case'\n':
				w.write("newline");
				break;
			case'\t':
				w.write("tab");
				break;
			case' ':
				w.write("space");
				break;
			default:
				w.write(c);
			}
		}
	else w.write(x.toString());
}

private static void printInnerSeq(ISeq x, Writer w) throws Exception{
	for(ISeq s = x; s != null; s = s.rest())
		{
		print(s.first(), w);
		if(s.rest() != null)
			w.write(' ');
		}
}

static public void formatAesthetic(Writer w, Object obj) throws IOException{
	if(obj == null)
		w.write("null");
	else
		w.write(obj.toString());
}

static public void formatStandard(Writer w, Object obj) throws IOException{
	if(obj == null)
		w.write("null");
	else if(obj instanceof String)
		{
		w.write('"');
		w.write((String) obj);
		w.write('"');
		}
	else if(obj instanceof Character)
		{
		w.write('\\');
		char c = ((Character) obj).charValue();
		switch(c)
			{
			case'\n':
				w.write("newline");
				break;
			case'\t':
				w.write("tab");
				break;
			case' ':
				w.write("space");
				break;
			default:
				w.write(c);
			}
		}
	else
		w.write(obj.toString());
}

static public Object format(Object o, String s, Object... args) throws Exception{
	Writer w;
	if(o == null)
		w = new StringWriter();
	else if(equal(o, T))
		w = (Writer) OUT.get();
	else
		w = (Writer) o;
	doFormat(w, s, ArraySeq.create(args));
	if(o == null)
		return w.toString();
	return null;
}

static public ISeq doFormat(Writer w, String s, ISeq args) throws Exception{
	for(int i = 0; i < s.length();)
		{
		char c = s.charAt(i++);
		switch(Character.toLowerCase(c))
			{
			case'~':
				char d = s.charAt(i++);
				switch(Character.toLowerCase(d))
					{
					case'%':
						w.write('\n');
						break;
					case't':
						w.write('\t');
						break;
					case'a':
						if(args == null)
							throw new IllegalArgumentException("Missing argument");
						RT.formatAesthetic(w, RT.first(args));
						args = RT.rest(args);
						break;
					case's':
						if(args == null)
							throw new IllegalArgumentException("Missing argument");
						RT.formatStandard(w, RT.first(args));
						args = RT.rest(args);
						break;
					case'{':
						int j = s.indexOf("~}", i);    //note - does not nest
						if(j == -1)
							throw new IllegalArgumentException("Missing ~}");
						String subs = s.substring(i, j);
						for(ISeq sargs = RT.seq(RT.first(args)); sargs != null;)
							sargs = doFormat(w, subs, sargs);
						args = RT.rest(args);
						i = j + 2; //skip ~}
						break;
					case'^':
						if(args == null)
							return null;
						break;
					case'~':
						w.write('~');
						break;
					default:
						throw new IllegalArgumentException("Unsupported ~ directive: " + d);
					}
				break;
			default:
				w.write(c);
			}
		}
	return args;
}
///////////////////////////////// values //////////////////////////

static public Object setValues(Object... vals){
	//ThreadLocalData.setValues(vals);
	if(vals.length > 0)
		return vals[0];
	return null;
}

}
