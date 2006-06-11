/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 11:45:22 AM */

package clojure.lang;

public class ThreadLocalData{

private static ThreadLocal<Transaction> transaction = new ThreadLocal<Transaction>();
private static ThreadLocal<Object[]> values = new ThreadLocal<Object[]>();

static public Object[] getValues(){
     return values.get();
}

static public void setValues(Object[] vals) {
    values.set(vals);
}

static public Transaction getTransaction() {
    return transaction.get();

}

static public void setTransaction(Transaction t){
    transaction.set(t);
}

}
