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

    static public Symbol T = Symbol.intern(":t");
	static public Var OUT = Module.intern("clojure","^out");
    static public Var _CT_MODULE = Module.intern("clojure", "^module");
    static public final Object[] EMPTY_ARRAY = new Object[]{};
    static public final Character[] chars;
    static AtomicInteger id = new AtomicInteger(1);

    static {
        chars = new Character[256];
        for(int i=0;i<chars.length;i++)
            chars[i] = new Character((char)i);

        OUT.bind(new OutputStreamWriter(System.out));
        _CT_MODULE.bind((Module.findOrCreate("clj-user")));
        }

    static public int nextID(){
        return id.getAndIncrement();
    }

static public boolean equal(Object k1,Object k2){
    return k1 == k2 ||
           (k1 != null && k1.equals(k2));
}

    static public Object eq(Object arg1, Object arg2) {
        return (arg1 == arg2)?Boolean.TRUE:null;
        }

    static public Object eql(Object arg1, Object arg2) {
        if(arg1 == arg2)
            return Boolean.TRUE;
        if(arg1 == null || arg2 == null)
            return null;
        if(arg1 instanceof Num
           && arg1.getClass() == arg2.getClass()
           && arg1.equals(arg2))
            return Boolean.TRUE;
        if(arg1.getClass() == Character.class
           && arg2.getClass() == Character.class
           && arg1.equals(arg2))
            return Boolean.TRUE;
        return null;
        }

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

static public ISeq seq(Object coll)  {
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

static public Object meta(Object x) {
    if(x == null)
        return null;
    return ((Obj)x).meta();
}

public static int count(Object o) {
    if(o == null)
        return 0;
    return ((IPersistentCollection)o).count();
}

static public IPersistentCollection cons(Object x, IPersistentCollection y) {
    if(y == null)
        return new PersistentList(x);
    return y.cons(x);
}

static public ISeq cons(Object x, ISeq y) {
    if(y == null)
        return new PersistentList(x);
    return y.cons(x);
}

static public Object first(Object x) {
    if(x == null)
        return null;
    return seq(x).first();
}

static public Object second(Object x) {
    return first(rest(x));
}

static public Object third(Object x) {
    return first(rest(rest(x)));
}

static public Object fourth(Object x) {
    return first(rest(rest(rest(x))));
}

static public ISeq rest(Object x) {
    if(x == null)
        return null;
    return seq(x).rest();
}

static public Object peek(Object x) {
    if(x == null)
        return null;
    return ((IPersistentList)x).peek();
}

static public Object pop(Object x) {
    if(x == null)
        return null;
    return ((IPersistentList)x).pop();
}

static public Object get(Object key, Object coll) {
    if(coll == null)
        return null;
    return ((Associative)coll).get(key);
}

static public Object assoc(Object key, Object val, Object coll) {
    if(coll == null)
        return new MapEntry(key,val);
    return ((Associative)coll).assoc(key,val);
}

static public Object contains(Object key, Object coll) {
    if(coll == null)
        return false;
    return ((Associative)coll).contains(key);
}

static public Object find(Object key, Object coll) {
    if(coll == null)
        return null;
    return ((Associative)coll).find(key);
}

//takes a seq of key,val,key,val
//returns tail starting at val of matching key if found, else null
static public ISeq findKey(Keyword key,ISeq keyvals) throws Exception {
    while(keyvals != null)
        {
        ISeq r = keyvals.rest();
        if (r == null)
            throw new Exception("Malformed keyword argslist");
        if (keyvals.first() == key)
            return r;
        keyvals = r.rest();
        }
    return null;
}

static public Object without(Object key, Object coll) {
    if(coll == null)
        return null;
    return ((IPersistentMap)coll).without(key);
}

static public Object nth(int n, Object coll) {
    if(coll == null)
        return null;
    else if(coll instanceof IPersistentArray)
        return ((IPersistentArray)coll).nth(n);
    else if(coll instanceof Object[])
        return ((Object[])coll)[n];
    else if(coll instanceof Sequential)
        {
        ISeq seq = ((IPersistentCollection) coll).seq();
        for(int i=0;i<=n && seq != null;++i, seq = seq.rest())
            {
            if(i == n)
                return seq.first();
            }
        return null;
        }
    else
        return null;
}

static public Object assocN(int n, Object val, Object coll) {
    if(coll == null)
        return null;
    else if(coll instanceof IPersistentArray)
        return ((IPersistentArray)coll).assocN(n,val);
    else if(coll instanceof Object[])
        {
        //hmm... this is not persistent
        Object[] array = ((Object[])coll);
        array[n] = val;
        return array;
        }
    else
        return null;
}

static public Iter iter(Object coll)
    {
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

/************************ Boxing/casts *******************************/
static public Object box(Object x)
    {
    return x;
    }

static public Character box(char x)
    {
    if(x < chars.length)
        return chars[x];
    return new Character(x);
    }

static public Boolean box(boolean x)
    {
    return Boolean.valueOf(x);
    }

static public Byte box(byte x)
    {
    return new Byte(x);
    }

static public Short box(short x)
    {
    return new Short(x);
    }

static public Integer box(int x)
    {
    return new Integer(x);
    }

static public Long box(long x)
    {
    return new Long(x);
    }

static public Float box(float x)
    {
    return new Float(x);
    }

static public Double box(double x)
    {
    return new Double(x);
    }

static public char charCast(Object x)
    {
    if(x instanceof Character)
        return ((Character)x).charValue();
    return (char) ((Number)x).intValue();
    }

static public boolean booleanCast(Object x)
    {
    if(x instanceof Boolean)
        return ((Boolean)x).booleanValue();
    return x != null;
    }

static public byte byteCast(Object x)
    {
    return ((Number)x).byteValue();
    }

static public short shortCast(Object x)
    {
    return ((Number)x).shortValue();
    }

static public int intCast(Object x)
    {
    return ((Number)x).intValue();
    }

static public long longCast(Object x)
    {
    return ((Number)x).longValue();
    }

static public float floatCast(Object x)
    {
    return ((Number)x).floatValue();
    }

static public double doubleCast(Object x)
    {
    return ((Number)x).doubleValue();
    }


/******************************************* list support ********************************/




static public ISeq list()
    {
    return null;
    }

static public ISeq list(Object arg1) {
    return new PersistentList(arg1);
}

static public ISeq list(Object arg1, Object arg2) throws Exception {
return listStar(arg1, arg2, null);
}

static public ISeq list(Object arg1, Object arg2, Object arg3) throws Exception {
return listStar(arg1, arg2, arg3, null);
}

static public ISeq list(Object arg1, Object arg2, Object arg3, Object arg4) throws Exception {
return listStar(arg1, arg2, arg3, arg4, null);
}

static public ISeq list(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) throws Exception {
return listStar(arg1, arg2, arg3, arg4, arg5, null);
}

static public ISeq listStar(Object arg1, ISeq rest) throws Exception {
return (ISeq) cons(arg1, rest);
}

static public ISeq listStar(Object arg1, Object arg2, ISeq rest) throws Exception {
return (ISeq) cons(arg1, cons(arg2, rest));
}

static public ISeq listStar(Object arg1, Object arg2, Object arg3, ISeq rest) throws Exception {
return (ISeq) cons(arg1, cons(arg2, cons(arg3, rest)));
}

static public ISeq listStar(Object arg1, Object arg2, Object arg3, Object arg4, ISeq rest) throws Exception {
return (ISeq) cons(arg1, cons(arg2, cons(arg3, cons(arg4, rest))));
}

static public ISeq listStar(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq rest) throws Exception {
return (ISeq) cons(arg1, cons(arg2, cons(arg3, cons(arg4, cons(arg5, rest)))));
}

static public ISeq arrayToList(Object[] a) throws Exception {
    ISeq ret = null;
    for(int i=a.length-1;i>=0;--i)
        ret = (ISeq) cons(a[i], ret);
    return ret;
}

static public Object[] seqToArray(ISeq seq) throws Exception {
    int len = length(seq);
    Object[] ret = new Object[len];
    for(int i=0;seq != null;++i, seq = seq.rest())
        ret[i] = seq.first();
    return ret;
}

static public int length(ISeq list) throws Exception {
int i = 0;
for(ISeq c = list; c != null; c = c.rest())
    {
    i++;
    }
return i;
}

static public int boundedLength(ISeq list, int limit) throws Exception {
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

static public Character readChar(Reader r) throws Exception {
    int ret = r.read();
    return readRet(ret);
}

static public Character peekChar(Reader r) throws Exception {
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
        return ((LineNumberingPushbackReader)r).getLineNumber();
    return 0;
}

static public LineNumberingPushbackReader getLineNumberingReader(Reader r) {
    if(isLineNumberingReader(r))
        return (LineNumberingPushbackReader) r;
    return new LineNumberingPushbackReader(r);
}

static public boolean isLineNumberingReader(Reader r) {
    return r instanceof LineNumberingPushbackReader;
}

static public String resolveClassNameInContext(String className) {
    //todo - look up in context var
    return className;
}

static public boolean suppressRead(){
    //todo - look up in suppress-read var
    return false;
}

static public void print(Object x, Writer w) throws Exception {
    //todo - make extensible
    if(x == null)
        w.write("null");
    else if(x instanceof ISeq)
        {
        w.write('(');
        for(ISeq s = (ISeq)x;s != null;s = s.rest())
            {
            print(s.first(), w);
            if(s.rest()!=null)
                w.write(' ');
            }
        w.write(')');
        }
    else if(x instanceof String)
        {
        w.write('"');
        w.write(x.toString());
        w.write('"');
        }
    else if(x instanceof Character)
        {
        w.write('\\');
        char c = ((Character)x).charValue();
        switch(c){
            case '\n':
                w.write("newline");
                break;
            case '\t':
                w.write("tab");
                break;
            case ' ':
                w.write("space");
                break;
            default:
                w.write(c);
            }
        }
    else w.write(x.toString());
}

static public void formatAesthetic(Writer w,Object obj) throws IOException {
    if(obj == null)
        w.write("null");
    else
        w.write(obj.toString());
}

static public void formatStandard(Writer w,Object obj) throws IOException {
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
        char c = ((Character)obj).charValue();
        switch(c){
            case '\n':
                w.write("newline");
                break;
            case '\t':
                w.write("tab");
                break;
            case ' ':
                w.write("space");
                break;
            default:
                w.write(c);
            }
        }
    else
        w.write(obj.toString());
}

static public Object format(Object o, String s, Object... args) throws Exception {
	Writer w;
	if(o == null)
		w = new StringWriter();
	else if(equal(o,T))
		w = (Writer)OUT.getValue();
	else
		w = (Writer)o;
	doFormat(w,s,ArraySeq.create(args));
	if(o == null)
		return w.toString();
	return null;
}

static public void doFormat(Writer w, String s, ISeq args) throws Exception {
    for (int i = 0; i < s.length();)
        {
        char c = s.charAt(i++);
        switch (Character.toLowerCase(c))
            {
            case '~':
                char d = s.charAt(i++);
                switch (Character.toLowerCase(d))
                    {
                    case '%':
                        w.write('\n');
                        break;
                    case 't':
                        w.write('\t');
                        break;
                    case 'a':
                        if(args == null)
                            throw new IllegalArgumentException("Missing argument");
                        RT.formatAesthetic(w, RT.first(args));
                        args = RT.rest(args);
                        break;
                    case 's':
                        if(args == null)
                            throw new IllegalArgumentException("Missing argument");
                        RT.formatStandard(w, RT.first(args));
                        args = RT.rest(args);
                        break;
                    case '{':
                        int j = s.indexOf("~}", i);    //note - does not nest
                        if(j == -1)
                            throw new IllegalArgumentException("Missing ~}");
                        String subs = s.substring(i, j);
                        format(w, subs, RT.seq(RT.first(args)));
                        args = RT.rest(args);
                        i = j+2; //skip ~}
                        break;
                    case '^':
                        if(args == null)
                            return;
                        break;
                    case '~':
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
}
///////////////////////////////// values //////////////////////////

static public Object setValues(Object... vals)
    {
    ThreadLocalData.setValues(vals);
    if(vals.length > 0)
        return vals[0];
    return null;
    }

}
