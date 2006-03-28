/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 28, 2006 10:09:27 AM */

package org.clojure.runtime;

import java.math.BigInteger;
 
public class FixNum extends IntegerNum {
	public int val;

	public boolean equals(Object arg0) {
		return arg0 != null
		       && arg0 instanceof FixNum
		       && ((FixNum)arg0).val == val;
	}
	public int hashCode() {
		return val;
	}
	public String toString() {
		return Integer.toString(val);
	}
	public FixNum(int val){
		this.val = val;
	}

	public double doubleValue() {
		return (double)val;
	}
	public float floatValue() {
		return (float)val;
	}
	public int intValue() {
		return val;
	}
	public long longValue() {
		return (long)val;
	}

	public boolean equiv(Num rhs) {
		return rhs.equivTo(val);
	}
	public boolean equivTo(BigInteger x) {
		//wouldn't still be a BigInteger if it fit in int
		return false;
	}
	public boolean equivTo(int x) {
		return x == val;
	}
	public boolean equivTo(RatioNum x) {
		//wouldn't still be a RatioNum if it was an integer
		return false;
	}

	public boolean lt(Num rhs)
		{
		return rhs.gt(val);
	}
	public boolean gt(BigInteger x)
		{
		return x.compareTo(BigInteger.valueOf(val)) < 0;
	}
	public boolean gt(int x)
		{
		return x < val;
	}
	public boolean gt(RatioNum x)
		{
		return x.numerator.lt(x.denominator.multiply(val));
	}

	public Num add(Num rhs) {
		return rhs.addTo(val);
	}
	public Num addTo(BigInteger x) {
		return Num.from(x.add(BigInteger.valueOf(val)));
	}
	public Num addTo(int x) {
		return Num.from((long)x + val);
	}
	public Num addTo(RatioNum x) {
		return x.addTo(val);
	}

	public Num subtractFrom(Num x) {
		return x.addTo(-val);
	}

	public Num multiplyBy(Num rhs) {
		return rhs.multiply(val);
	}
	public Num multiply(BigInteger x) {
		return Num.from(x.multiply(BigInteger.valueOf(val)));
	}
	public Num multiply(int x) {
		return Num.from((long)x * val);
	}
	public Num multiply(RatioNum x) {
		return x.multiply(val);
	}

	public Object truncateDivide(ThreadLocalData tld, Num num) {
		return num.truncateBy(tld, val);
	}

	public Object truncateBy(ThreadLocalData tld, int div) {
	    return RT.setValues(tld,Num.from(val/div),Num.from(val%div));
	}

	public Object truncateBy(ThreadLocalData tld, BigInteger div) {
	    return Num.truncateBigints(tld, BigInteger.valueOf(val),div);
	}

	public Object truncateBy(ThreadLocalData tld, RatioNum div) {
	    Num q =  (Num)Num.truncate(tld, div.denominator.multiply(val),div.numerator);
	    return RT.setValues(tld,q,q.multiplyBy(div).subtractFrom(this));
	}

	public Num divideBy(Num rhs) {
		return rhs.divide(val);
	}
	public Num divide(BigInteger n) {
		return Num.divide(n,BigInteger.valueOf(val));
	}

    static int gcd(int u, int v) {
        while (v != 0) {
            int r = u % v;
            u = v;
            v = r;
        }
        return u;
    }

	public Num divide(int n) {
		int gcd = gcd(n,val);
		if(gcd == 0)
			return Num.ZERO;

		n = n/gcd;
		int d = val/gcd;
		if(d == 1)
			return Num.from(n);
		if(d < 0){
			n = -n;
			d = -d;
		}
		return new RatioNum((IntegerNum)Num.from(n),(IntegerNum)Num.from(d));
	}
	public Num divide(RatioNum x) {
		return Num.divide(x.numerator,x.denominator.multiply(val));
	}

	public Num negate(){
		return Num.from(-val);
	}
    public boolean minusp() {
        return val < 0;
    }
    public boolean plusp() {
        return val > 0;
    }

    public Num oneMinus() {
        return Num.from(val - 1);
    }
    public Num onePlus() {
        return Num.from(val + 1);
    }

}
