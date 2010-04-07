/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 31, 2008 */

package clojure.lang;

import java.math.BigInteger;
import java.math.BigDecimal;
import java.math.MathContext;

public class Ratio extends Number implements Comparable{
final public BigInteger numerator;
final public BigInteger denominator;

public Ratio(BigInteger numerator, BigInteger denominator){
	this.numerator = numerator;
	this.denominator = denominator;
}

public boolean equals(Object arg0){
	return arg0 != null
	       && arg0 instanceof Ratio
	       && ((Ratio) arg0).numerator.equals(numerator)
	       && ((Ratio) arg0).denominator.equals(denominator);
}

public int hashCode(){
	return numerator.hashCode() ^ denominator.hashCode();
}

public String toString(){
	return numerator.toString() + "/" + denominator.toString();
}

public int intValue(){
	return (int) doubleValue();
}

public long longValue(){
	return bigIntegerValue().longValue();
}

public float floatValue(){
	return (float)doubleValue();
}

public double doubleValue(){
	return decimalValue(MathContext.DECIMAL64).doubleValue();
}

public BigDecimal decimalValue(){
	return decimalValue(MathContext.UNLIMITED);
}

public BigDecimal decimalValue(MathContext mc){
	BigDecimal numerator = new BigDecimal(this.numerator);
	BigDecimal denominator = new BigDecimal(this.denominator);

	return numerator.divide(denominator, mc);
}

public BigInteger bigIntegerValue(){
	return numerator.divide(denominator);
}

public int compareTo(Object o){
	Number other = (Number)o;
	return Numbers.compare(this, other);
}
}
