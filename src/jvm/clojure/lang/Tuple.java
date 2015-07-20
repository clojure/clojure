/**
 * Copyright (c) Rich Hickey. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 **/
 
/* rich 7/16/15 */
// proposed by Zach Tellman

package clojure.lang;

import java.util.Collection;
import java.util.RandomAccess;

public class Tuple{
    static final int MAX_SIZE = 6;
    static public IPersistentVector EMPTY = new T0();

    public static IPersistentVector create(){return EMPTY;}
    public static T1 create(Object v0){return new T1(v0);}
    public static T2 create(Object v0, Object v1){return new T2(v0, v1);}
    public static T3 create(Object v0, Object v1, Object v2){return new T3(v0, v1, v2);}
    public static T4 create(Object v0, Object v1, Object v2, Object v3){return new T4(v0, v1, v2, v3);}
    public static T5 create(Object v0, Object v1, Object v2, Object v3, Object v4){return new T5(v0, v1, v2, v3, v4);}
    public static T6 create(Object v0, Object v1, Object v2, Object v3, Object v4, Object v5)
        {return new T6(v0, v1, v2, v3, v4, v5);}

    public static IPersistentVector createFromArray(Object[] items){
        if(items.length <= Tuple.MAX_SIZE){
               switch(items.length){
               case 0:
                   return EMPTY;
               case 1:
                   return create(items[0]);
               case 2:
                   return create(items[0], items[1]);
               case 3:
                   return create(items[0], items[1], items[2]);
               case 4:
                   return create(items[0], items[1], items[2], items[3]);
               case 5:
                   return create(items[0], items[1], items[2], items[3], items[4]);
               case 6:
                   return create(items[0], items[1], items[2], items[3], items[4], items[5]);
               }
           }
        throw new IllegalAccessError("Too large an array for tuple");
    }

    public static IPersistentVector createFromColl(Object coll){
        if(coll instanceof RandomAccess) {
            switch(((Collection) coll).size()){
                case 0:
                    return EMPTY;
                case 1:
                    return create(RT.nth(coll, 0));
                case 2:
                    return create(RT.nth(coll,0), RT.nth(coll,1));
                case 3:
                    return create(RT.nth(coll,0), RT.nth(coll,1), RT.nth(coll,2));
                case 4:
                    return create(RT.nth(coll,0), RT.nth(coll,1), RT.nth(coll,2), RT.nth(coll,3));
                case 5:
                    return create(RT.nth(coll,0), RT.nth(coll,1), RT.nth(coll,2), RT.nth(coll,3), RT.nth(coll,4));
                case 6:
                    return create(RT.nth(coll,0), RT.nth(coll,1), RT.nth(coll,2), RT.nth(coll,3), RT.nth(coll,4), RT.nth(coll,5));
               }
            }
        return createFromArray(RT.toArray(coll));
    }

static public abstract class ATuple extends APersistentVector implements IObj, IEditableCollection{
    PersistentVector vec(){
        return PersistentVector.adopt(toArray());
    }

    public IObj withMeta(IPersistentMap meta){
        if(meta == null)
            return this;
        return vec().withMeta(meta);
    }

    public IPersistentMap meta(){
        return null;
    }

    public IPersistentVector assocN(int i, Object val){
        return vec().assocN(i, val);
    }

    public IPersistentCollection empty(){
        return EMPTY;
    }

    public IPersistentStack pop(){
        return vec().pop();
    }

    public ITransientCollection asTransient(){
        return vec().asTransient();
    }
}

static public class T0 extends ATuple{
    public int count(){
        return 0;
    }

    public Object nth(int i){
        throw new IndexOutOfBoundsException();
    }

    public IPersistentVector cons(Object o){
        return create(o);
    }

    public boolean equiv(Object obj){
        if(obj instanceof T0)
            return true;
        return super.equiv(obj);
    }
}

static public class T1 extends ATuple{
    public final Object v0;

    public T1(Object v0){
        this.v0 = v0;
    }

    public int count(){
        return 1;
    }

    public Object nth(int i){
        switch(i){
        case 0:
            return v0;
        default:
            throw new IndexOutOfBoundsException();
        }
    }

    public IPersistentVector cons(Object o){
        return create(v0, o);
    }

    public boolean equiv(Object obj){
        if(this == obj) return true;
        if(obj instanceof T1) {
            T1 o = (T1) obj;
            return Util.equiv(v0, o.v0);
            }
        return super.equiv(obj);
    }
}

static public class T2 extends ATuple implements IMapEntry{
    public final Object v0;
    public final Object v1;

    public T2(Object v0, Object v1){
        this.v0 = v0;
        this.v1 = v1;
    }

    public int count(){
        return 2;
    }

    public Object nth(int i){
        switch(i){
        case 0:
            return v0;
        case 1:
            return v1;
        default:
            throw new IndexOutOfBoundsException();
        }
    }

    public IPersistentVector cons(Object o){
        return create(v0, v1, o);
    }

    public boolean equiv(Object obj){
        if(this == obj) return true;
        if(obj instanceof T2) {
            T2 o = (T2) obj;
            return Util.equiv(v0, o.v0) &&
                   Util.equiv(v1, o.v1);
            }
        return super.equiv(obj);
    }

    @Override
    public Object key(){
        return v0;
    }

    public Object val(){
        return v1;
    }

    public Object getKey(){
        return v0;
    }

    public Object getValue(){
        return v1;
    }

    public Object setValue(Object value){
        throw new UnsupportedOperationException();
    }
}

static public class T3 extends ATuple{
    public final Object v0;
    public final Object v1;
    public final Object v2;

    public T3(Object v0, Object v1, Object v2){
        this.v0 = v0;
        this.v1 = v1;
        this.v2 = v2;
    }

    public int count(){
        return 3;
    }

    public Object nth(int i){
        switch(i){
        case 0:
            return v0;
        case 1:
            return v1;
        case 2:
            return v2;
        default:
            throw new IndexOutOfBoundsException();
        }
    }

    public IPersistentVector cons(Object o){
        return create(v0, v1, v2, o);
    }

    public boolean equiv(Object obj){
        if(this == obj) return true;
        if(obj instanceof T3) {
            T3 o = (T3) obj;
            return Util.equiv(v0, o.v0) &&
                   Util.equiv(v1, o.v1) &&
                   Util.equiv(v2, o.v2);
            }
        return super.equiv(obj);
    }
}

static public class T4 extends ATuple{
    public final Object v0;
    public final Object v1;
    public final Object v2;
    public final Object v3;

    public T4(Object v0, Object v1, Object v2, Object v3){
        this.v0 = v0;
        this.v1 = v1;
        this.v2 = v2;
        this.v3 = v3;
    }

    public int count(){
        return 4;
    }

    public Object nth(int i){
        switch(i){
        case 0:
            return v0;
        case 1:
            return v1;
        case 2:
            return v2;
        case 3:
            return v3;
        default:
            throw new IndexOutOfBoundsException();
        }
    }

    public IPersistentVector cons(Object o){
        return create(v0, v1, v2, v3, o);
    }

    public boolean equiv(Object obj){
        if(this == obj) return true;
        if(obj instanceof T4) {
            T4 o = (T4) obj;
            return Util.equiv(v0, o.v0) &&
                   Util.equiv(v1, o.v1) &&
                   Util.equiv(v2, o.v2) &&
                   Util.equiv(v3, o.v3);
            }
        return super.equiv(obj);
    }

}

static public class T5 extends ATuple{
    public final Object v0;
    public final Object v1;
    public final Object v2;
    public final Object v3;
    public final Object v4;

    public T5(Object v0, Object v1, Object v2, Object v3, Object v4){
        this.v0 = v0;
        this.v1 = v1;
        this.v2 = v2;
        this.v3 = v3;
        this.v4 = v4;
    }

    public int count(){
        return 5;
    }

    public Object nth(int i){
        switch(i){
        case 0:
            return v0;
        case 1:
            return v1;
        case 2:
            return v2;
        case 3:
            return v3;
        case 4:
            return v4;
        default:
            throw new IndexOutOfBoundsException();
        }
    }

    public IPersistentVector cons(Object o){
        return create(v0, v1, v2, v3, v4, o);
    }

    public boolean equiv(Object obj){
        if(this == obj) return true;
        if(obj instanceof T5) {
            T5 o = (T5) obj;
            return Util.equiv(v0, o.v0) &&
                   Util.equiv(v1, o.v1) &&
                   Util.equiv(v2, o.v2) &&
                   Util.equiv(v3, o.v3) &&
                   Util.equiv(v4, o.v4);
            }
        return super.equiv(obj);
    }

}

static public class T6 extends ATuple{
    public final Object v0;
    public final Object v1;
    public final Object v2;
    public final Object v3;
    public final Object v4;
    public final Object v5;

    public T6(Object v0, Object v1, Object v2, Object v3, Object v4, Object v5){
        this.v0 = v0;
        this.v1 = v1;
        this.v2 = v2;
        this.v3 = v3;
        this.v4 = v4;
        this.v5 = v5;
    }

    public int count(){
        return 6;
    }

    public Object nth(int i){
        switch(i){
        case 0:
            return v0;
        case 1:
            return v1;
        case 2:
            return v2;
        case 3:
            return v3;
        case 4:
            return v4;
        case 5:
            return v5;
        default:
            throw new IndexOutOfBoundsException();
        }
    }

    public IPersistentVector cons(Object o){
        return vec().cons(o);
    }

    public boolean equiv(Object obj){
        if(this == obj) return true;
        if(obj instanceof T6) {
            T6 o = (T6) obj;
            return Util.equiv(v0, o.v0) &&
                   Util.equiv(v1, o.v1) &&
                   Util.equiv(v2, o.v2) &&
                   Util.equiv(v3, o.v3) &&
                   Util.equiv(v4, o.v4) &&
                   Util.equiv(v5, o.v5);
            }
        return super.equiv(obj);
    }

}
}
