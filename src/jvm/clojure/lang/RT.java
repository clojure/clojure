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
import java.io.Reader;
import java.io.PushbackReader;
import java.io.Writer;

public class RT{

    static public Symbol T = Symbol.intern("t");
    static public final Object[] EMPTY_ARRAY = new Object[]{};
    static public final Character[] chars;

    static {
        chars = new Character[256];
        for(int i=0;i<chars.length;i++)
            chars[i] = new Character((char)i);
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

static public ISeq seq(Object coll) throws Exception {
    if(coll == null || coll instanceof ISeq)
        return (ISeq) coll;
    else if(coll instanceof IPersistentCollection)
        return ((IPersistentCollection) coll).seq();
    else if(coll instanceof Iterable)
        return IteratorSeq.create(((Iterable) coll).iterator());
    else if(coll instanceof Object[])
        return ArraySeq.create((Object[]) coll);
    else
        throw new IllegalArgumentException("Don't know how to create ISeq from arg");
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
static public Cons cons(Object x, Object y) throws Exception {
return new Cons(x, seq(y));
}

static public Object first(Object x) throws Exception {
    return seq(x).first();
}

static public ISeq rest(Object x) throws Exception {
    return seq(x).rest();
}

static public Cons list()
    {
    return null;
    }

static public Cons list(Object arg1) throws Exception {
return cons(arg1, null);
}

static public Cons list(Object arg1, Object arg2) throws Exception {
return listStar(arg1, arg2, null);
}

static public Cons list(Object arg1, Object arg2, Object arg3) throws Exception {
return listStar(arg1, arg2, arg3, null);
}

static public Cons list(Object arg1, Object arg2, Object arg3, Object arg4) throws Exception {
return listStar(arg1, arg2, arg3, arg4, null);
}

static public Cons list(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) throws Exception {
return listStar(arg1, arg2, arg3, arg4, arg5, null);
}

static public Cons listStar(Object arg1, ISeq rest) throws Exception {
return cons(arg1, rest);
}

static public Cons listStar(Object arg1, Object arg2, ISeq rest) throws Exception {
return cons(arg1, cons(arg2, rest));
}

static public Cons listStar(Object arg1, Object arg2, Object arg3, ISeq rest) throws Exception {
return cons(arg1, cons(arg2, cons(arg3, rest)));
}

static public Cons listStar(Object arg1, Object arg2, Object arg3, Object arg4, ISeq rest) throws Exception {
return cons(arg1, cons(arg2, cons(arg3, cons(arg4, rest))));
}

static public Cons listStar(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq rest) throws Exception {
return cons(arg1, cons(arg2, cons(arg3, cons(arg4, cons(arg5, rest)))));
}

static public Cons arrayToList(Object[] a) throws Exception {
    Cons ret = null;
    for(int i=a.length-1;i>=0;--i)
        ret = cons(a[i], ret);
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
///////////////////////////////// values //////////////////////////

static public Object setValues(Object... vals)
    {
    ThreadLocalData.setValues(vals);
    if(vals.length > 0)
        return vals[0];
    return null;
    }

}
