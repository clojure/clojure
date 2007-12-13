/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 28, 2006 10:11:55 AM */

package clojure.lang;

import java.math.BigInteger;

public abstract class IntegerNum extends RationalNum{
static public Num bitXor(Object x, Object y){
	return ((IntegerNum) Num.from(y)).bitXorBy((IntegerNum) Num.from(x));
}

abstract public Num bitXorBy(IntegerNum y);

abstract public Num bitXor(int y);

abstract public Num bitXor(BigInteger y);

static public Num bitAnd(Object x, Object y){
	return ((IntegerNum) Num.from(y)).bitAndBy((IntegerNum) Num.from(x));
}

abstract public Num bitAndBy(IntegerNum y);

abstract public Num bitAnd(int y);

abstract public Num bitAnd(BigInteger y);

static public Num bitOr(Object x, Object y){
	return ((IntegerNum) Num.from(y)).bitOrBy((IntegerNum) Num.from(x));
}

abstract public Num bitOrBy(IntegerNum y);

abstract public Num bitOr(int y);

abstract public Num bitOr(BigInteger y);

static public Num bitNot(Object x){
	return ((IntegerNum) Num.from(x)).bitNot();
}

abstract public Num bitNot();

static public Num shiftLeft(Object x, Object y){
	return ((IntegerNum) Num.from(y)).shiftLeftBy((IntegerNum) Num.from(x));
}

abstract public Num shiftLeftBy(IntegerNum y);

abstract public Num shiftLeft(BigInteger y);

abstract public Num shiftLeft(int y);

static public Num shiftRight(Object x, Object y){
	return ((IntegerNum) Num.from(y)).shiftRightBy((IntegerNum) Num.from(x));
}

abstract public Num shiftRightBy(IntegerNum y);

abstract public Num shiftRight(BigInteger y);

abstract public Num shiftRight(int y);
}